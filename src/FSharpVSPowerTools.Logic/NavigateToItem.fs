namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Threading

open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Language.NavigateTo.Interfaces
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open EnvDTE
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

module Constants = 
    let LogicalViewTextGuid = Guid(LogicalViewID.TextView)
    let EmptyReadOnlyCollection = System.Collections.ObjectModel.ReadOnlyCollection([||]); 
    let FSharpProjectKind = "{F2A71F9B-5D33-465A-A702-920D77279786}"

type NavigateToItemExtraData = 
    {
        FileName: string
        Span: Microsoft.FSharp.Compiler.Range.Range01
        Description: string
    }

[<Package("f152487e-9a22-4cf9-bee6-a8f7c77f828d")>]
[<Export(typeof<INavigateToItemProviderFactory>)>]
type NavigateToItemProviderFactory 
    [<ImportingConstructor>]
    (
        openDocumentsTracker: OpenDocumentsTracker,
        [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
        fsharpLanguageService: VSLanguageService,
        [<ImportMany>] itemDisplayFactories: seq<Lazy<INavigateToItemDisplayFactory, IMinimalVisualStudioVersionMetadata>>,
        vsCompositionService: ICompositionService
    ) =
    
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let currentVersion = VisualStudioVersion.FromDTEVersion dte.Version
    let itemDisplayFactory = 
        let candidate =
            itemDisplayFactories
            |> Seq.tryFind (fun f -> VisualStudioVersion.Matches currentVersion f.Metadata.Version)

        match candidate with
        | Some l -> l.Value
        | None -> 
            let instance = VS2012NavigateToItemDisplayFactory()
            vsCompositionService.SatisfyImportsOnce(instance)
            |> ignore
            instance :> _


    interface INavigateToItemProviderFactory with
        member x.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            provider <- new NavigateToItemProvider(openDocumentsTracker, serviceProvider, fsharpLanguageService, itemDisplayFactory)
            true
and
    NavigateToItemProvider
        (
            openDocumentsTracker: OpenDocumentsTracker,
            serviceProvider: IServiceProvider,
            fsharpLanguageService: VSLanguageService,
            itemDisplayFactory: INavigateToItemDisplayFactory
        ) = 
    let mutable cts = new System.Threading.CancellationTokenSource()
    let dte = serviceProvider.GetService<DTE, SDTE>()
    
    let listFSharpProjectsInSolution() = 
        let rec handleProject (p: Project) =
            if LanguagePrimitives.PhysicalEquality p null then []
            elif p.Kind.ToUpperInvariant() = Constants.FSharpProjectKind then 
                match ProjectCache.putProject p with
                | Some provider -> [provider]
                | _ -> []
            elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then handleProjectItems p.ProjectItems
            else []
        and handleProjectItems(items: ProjectItems) = 
            [
                for pi in items do
                    yield! handleProject pi.SubProject
            ]
        [
            for p in dte.Solution.Projects do yield! handleProject p
        ]

    let runSearch(projects: IProjectProvider list, openDocuments: Map<string, string>, searchValue: string, callback: INavigateToCallback, ct: CancellationToken) = 
        let processNavigableItems (navigationItems: seq<Navigation.NavigableItem>) = 
            for item in navigationItems do
                let name = item.Name
                let index = name.IndexOf(searchValue, StringComparison.CurrentCultureIgnoreCase)
                let matchKind = 
                    if index = -1 then MatchKind.None
                    elif index = 0 then
                        if searchValue.Length = name.Length then MatchKind.Exact
                        else MatchKind.Prefix
                    else MatchKind.Substring
                if matchKind <> MatchKind.None then
                    let kindOpt = 
                        match item.Kind with
                        | Navigation.NavigableItemKind.Exception -> Some(NavigateToItemKind.Class, "exception")
                        | Navigation.NavigableItemKind.Field -> Some (NavigateToItemKind.Field, "field")
                        | Navigation.NavigableItemKind.Constructor -> Some (NavigateToItemKind.Class, "constructor")
                        | Navigation.NavigableItemKind.Member -> Some (NavigateToItemKind.Method, "member")
                        | Navigation.NavigableItemKind.Module -> Some (NavigateToItemKind.Module, "module")
                        | Navigation.NavigableItemKind.ModuleAbbreviation -> Some (NavigateToItemKind.Module, "module abbreviation")
                        | Navigation.NavigableItemKind.ModuleValue -> Some (NavigateToItemKind.Field, "module value")
                        | Navigation.NavigableItemKind.Property -> Some (NavigateToItemKind.Property, "property")
                        | Navigation.NavigableItemKind.Type -> Some (NavigateToItemKind.Class, "type")
                        | Navigation.NavigableItemKind.EnumCase -> Some(NavigateToItemKind.EnumItem, "enum")
                        | Navigation.NavigableItemKind.UnionCase -> Some(NavigateToItemKind.Class, "union case")
                    match kindOpt with
                    | Some(kind, textKind) ->
                        let textKind = textKind + (if item.IsSignature then "(signature)" else "(implementation)")
                        let fileName, range01 = Microsoft.FSharp.Compiler.Range.Range.toFileZ item.Range
                        let extraData = { FileName = fileName; Span = range01; Description = textKind; }
                        let navigateToItem = NavigateToItem(item.Name, kind, "F#", searchValue, extraData, matchKind, itemDisplayFactory)
                        callback.AddItem navigateToItem
                    | _ -> ()

        Tasks.Task.Run(fun() ->
            for p in projects do
                if not ct.IsCancellationRequested then
                    do fsharpLanguageService.ProcessNavigableItemsInProject(openDocuments, p, processNavigableItems, ct)
            callback.Done()
        )
        |> ignore

    interface INavigateToItemProvider with
        member x.StartSearch(callback, searchValue) = 
            let openedDocuments = 
                openDocumentsTracker.MapOpenDocuments(fun (KeyValue (path,snapshot)) -> path, snapshot.GetText())
                |> Map.ofSeq

            let projects = 
                match listFSharpProjectsInSolution() with
                | [] -> maybe {
                          let dte = serviceProvider.GetService<EnvDTE.DTE, Microsoft.VisualStudio.Shell.Interop.SDTE>()
                          let! doc = dte.GetActiveDocument()
                          return! ProjectCache.getProject doc }
                          |> Option.toList
                | xs -> xs
            runSearch(projects, openedDocuments, searchValue, callback, cts.Token)
        member x.StopSearch() = 
            cts.Cancel()
            cts <- new CancellationTokenSource()

    interface IDisposable with
        member x.Dispose() = (x :> INavigateToItemProvider).StopSearch()
and
    [<Export>]
    NavigationItemIconCache() =

        [<Import; DefaultValue>] 
        val mutable glyphService: IGlyphService
        let iconCache = Dictionary()

        let glyphGroupForNavigateToItemKind (kind: string) =
            match kind with
            | NavigateToItemKind.Class    -> StandardGlyphGroup.GlyphGroupClass
            | NavigateToItemKind.Constant -> StandardGlyphGroup.GlyphGroupConstant
            | NavigateToItemKind.Delegate -> StandardGlyphGroup.GlyphGroupDelegate
            | NavigateToItemKind.Enum     -> StandardGlyphGroup.GlyphGroupEnum
            | NavigateToItemKind.EnumItem -> StandardGlyphGroup.GlyphGroupEnumMember
            | NavigateToItemKind.Event    -> StandardGlyphGroup.GlyphGroupEvent
            | NavigateToItemKind.Field    -> StandardGlyphGroup.GlyphGroupField
            | NavigateToItemKind.Interface -> StandardGlyphGroup.GlyphGroupInterface
            | NavigateToItemKind.Method   -> StandardGlyphGroup.GlyphGroupMethod
            | NavigateToItemKind.Module   -> StandardGlyphGroup.GlyphGroupModule
            | NavigateToItemKind.Property -> StandardGlyphGroup.GlyphGroupProperty
            | NavigateToItemKind.Structure -> StandardGlyphGroup.GlyphGroupStruct
            | other -> failwithf "Unrecognized NavigateToItemKind:%s" other
                    
        member private x.GetOrCreateIcon(glyphGroup, glyphItem): System.Drawing.Icon = 
            let key = (glyphGroup, glyphItem)
            match iconCache.TryGetValue key with
            | true, (icon, _) -> icon
            | false, _ ->
                let (icon, _) as pair =
                    match x.glyphService.GetGlyph(glyphGroup, glyphItem) with
                    | :? Windows.Media.Imaging.BitmapSource as bs ->
                        let bmpEncoder = Windows.Media.Imaging.PngBitmapEncoder()
                        bmpEncoder.Frames.Add(Windows.Media.Imaging.BitmapFrame.Create(bs))
                        let s = new System.IO.MemoryStream()
                        bmpEncoder.Save(s)
                        s.Position <- 0L
                        let bitmap =  new System.Drawing.Bitmap(s)
                        let icon = System.Drawing.Icon.FromHandle(bitmap.GetHicon())
                        icon, bitmap
                    | _ -> null, null
                iconCache.[key] <- pair
                icon
        member x.GetIconForNavigationItemKind(kind: string) = 
            let glyphGroup = glyphGroupForNavigateToItemKind kind
            // TODO
            let glyphItem = StandardGlyphItem.GlyphItemPublic
            x.GetOrCreateIcon((glyphGroup, glyphItem))

        interface IDisposable with
            member x.Dispose() = 
                for (KeyValue(_, (icon, bitmap))) in iconCache do
                    if not (LanguagePrimitives.PhysicalEquality icon null) then
                        icon.Dispose()
                        bitmap.Dispose()
                iconCache.Clear()
and
    [<ExportWithMinimalVisualStudioVersion(typeof<INavigateToItemDisplayFactory>, Version = VisualStudioVersion.VS2012)>]
    VS2012NavigateToItemDisplayFactory() =

        [<Import(typeof<SVsServiceProvider>); DefaultValue>]
        val mutable serviceProvider: IServiceProvider
        [<Import; DefaultValue>]
        val mutable iconCache: NavigationItemIconCache
        
        interface INavigateToItemDisplayFactory with
            member x.CreateItemDisplay(item) = 
                let icon = x.iconCache.GetIconForNavigationItemKind(item.Kind)
                NavigateToItemDisplay(item, icon, x.serviceProvider) :> _
and
    NavigateToItemDisplay(item: NavigateToItem, icon, serviceProvider: IServiceProvider) =
        let extraData: NavigateToItemExtraData = unbox item.Tag
        interface INavigateToItemDisplay with
            member x.Name = item.Name
            member x.Glyph = icon
            member x.AdditionalInformation = extraData.FileName
            member x.Description = extraData.Description
            member x.DescriptionItems = Constants.EmptyReadOnlyCollection
            member x.NavigateTo() = 
                let mutable hierarchy = Unchecked.defaultof<_>
                let mutable itemId = Unchecked.defaultof<_>
                let mutable windowFrame = Unchecked.defaultof<_>
                let isOpened = 
                    VsShellUtilities.IsDocumentOpen(
                        serviceProvider, 
                        extraData.FileName, 
                        Constants.LogicalViewTextGuid,
                        &hierarchy,
                        &itemId,
                        &windowFrame)
                let canShow = 
                    if isOpened then true
                    else
                        // TODO: track the project that contains document and open document in project context
                        try
                            VsShellUtilities.OpenDocument(
                                serviceProvider, 
                                extraData.FileName, 
                                Constants.LogicalViewTextGuid, 
                                &hierarchy,
                                &itemId,
                                &windowFrame)
                            true
                        with _ -> false
                if canShow then
                    windowFrame.Show()
                    |> ensureSucceded

                    let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                    let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
                    let mutable vsTextBuffer = Unchecked.defaultof<_>
                    vsTextView.GetBuffer(&vsTextBuffer)
                    |> ensureSucceded

                    let (startRow, startCol), (endRow, endCol) = extraData.Span
                    vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, startRow, startCol, endRow, endCol)
                    |> ensureSucceded

