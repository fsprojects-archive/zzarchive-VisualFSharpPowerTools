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

module Constants = 
    let LogicalViewTextGuid = Guid(LogicalViewID.TextView)
    let EmptyReadOnlyCollection = System.Collections.ObjectModel.ReadOnlyCollection([||]); 
    let FSharpProjectKind = "{F2A71F9B-5D33-465A-A702-920D77279786}"

type internal OpenedDocument = 
    {
        SourceText: string
        FilePath: string
    }

type internal NavigateToItemExtraData = 
    {
        FileName: string
        Span: Microsoft.FSharp.Compiler.Range.Range01
        Description: string
    }

[<Package("f152487e-9a22-4cf9-bee6-a8f7c77f828d")>]
[<Export(typeof<INavigateToItemProviderFactory>)>]
type NavigateToItemProviderFactory() =
    [<Import; DefaultValue>]
    val mutable activeViewsContainer: ActiveViewsContainer
    [<Import; DefaultValue>]
    val mutable textDocumentFactoryService: ITextDocumentFactoryService
    [<Import; DefaultValue>]
    val mutable fsharpLanguageService: VSLanguageService
    [<Import; DefaultValue>]
    val mutable itemDisplayFactory: INavigateToItemDisplayFactory
    
    interface INavigateToItemProviderFactory with
        member x.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            provider <- new NavigateToItemProvider(x.activeViewsContainer, x.textDocumentFactoryService, serviceProvider, x.fsharpLanguageService, x.itemDisplayFactory)
            true
and 
    NavigateToItemProvider
        (
            activeViewsContainer: ActiveViewsContainer,
            textDocumentFactoryService: ITextDocumentFactoryService,
            serviceProvider: IServiceProvider,
            fsharpLanguageService: VSLanguageService,
            itemDisplayFactory: INavigateToItemDisplayFactory
        ) = 
    let mutable cts = new System.Threading.CancellationTokenSource()
    let dte = serviceProvider.GetService(typeof<SDTE>) :?> DTE
    
    let listFSharpProjectsInSolution() = 
        let rec handleProject (p: Project) =
            if LanguagePrimitives.PhysicalEquality p null then []
            elif p.Kind.ToUpperInvariant() = Constants.FSharpProjectKind then [ ProjectProvider(p.Object :?> _)]
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

    let runSearch(projects: ProjectProvider list, openedDocuments: OpenedDocument list, searchValue: string, callback: INavigateToCallback, ct: CancellationToken) = 
        let openDocuments = openedDocuments |> List.map (fun { FilePath = f; SourceText = s } -> f, s) |> Map.ofList
        let searchPattern = Text.RegularExpressions.Regex(searchValue, Text.RegularExpressions.RegexOptions.IgnoreCase)
        let processNavigableItems (navigationItems: seq<Navigation.NavigableItem>) = 
            for item in navigationItems do
                let name = item.Name
                let m = searchPattern.Match name
                let matchKind = 
                    if not m.Success then MatchKind.None
                    elif m.Index = 0 then
                        if m.Length = name.Length then MatchKind.Exact
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
                activeViewsContainer.MapOpenViews (fun view -> 
                    match textDocumentFactoryService.TryGetTextDocument view.TextBuffer with
                    | false, _ -> None
                    | true, document ->
                        // TODO: save project for opened files 
                        let text = view.TextSnapshot.GetText()
                        Some ({ SourceText = text; FilePath = document.FilePath })
                )
            // TODO enable for loose files
            let projects = listFSharpProjectsInSolution();
            runSearch(projects, openedDocuments, searchValue, callback, cts.Token)
        member x.StopSearch() = 
            cts.Cancel()
            cts <- new CancellationTokenSource()

    interface IDisposable with
        member x.Dispose() = (x :> INavigateToItemProvider).StopSearch()
and
    [<Export>]
    IconCache() =
        [<Import; DefaultValue>] 
        val mutable glyphService: IGlyphService
        let iconCache = Dictionary()
        
        member x.GetIcon(glyphGroup, glyphItem): System.Drawing.Icon = 
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
        interface IDisposable with
            member x.Dispose() = 
                for (KeyValue(_, (icon, bitmap))) in iconCache do
                    if not (LanguagePrimitives.PhysicalEquality icon null) then
                        icon.Dispose()
                        bitmap.Dispose()
                iconCache.Clear()
and
    [<Export(typeof<INavigateToItemDisplayFactory>)>]
    NavigateToItemDisplayFactory() =
        
        [<Import; DefaultValue>]
        val mutable navigator: DocumentNavigator
        [<Import; DefaultValue>]
        val mutable iconCache: IconCache

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

        interface INavigateToItemDisplayFactory with
            member x.CreateItemDisplay(item) = 
                let glyphGroup = glyphGroupForNavigateToItemKind item.Kind
                // TODO 
                let glyphItem = StandardGlyphItem.GlyphItemPublic 
                let icon = x.iconCache.GetIcon(glyphGroup, glyphItem)
                NavigateToItemDisplay(item, icon, x.navigator) :> _
and 
    NavigateToItemDisplay(item: NavigateToItem, icon, navigator: DocumentNavigator) =
        let extraData: NavigateToItemExtraData = unbox item.Tag
        interface INavigateToItemDisplay2 with
            member x.Name = item.Name
            member x.Glyph = icon
            member x.AdditionalInformation = extraData.FileName
            member x.Description = extraData.Description
            member x.DescriptionItems = Constants.EmptyReadOnlyCollection
            member x.NavigateTo() = navigator.NavigateTo(extraData)
            member x.GetProvisionalViewingStatus() = navigator.GetProvisionalViewingStatus(extraData)
            member x.PreviewItem() = navigator.PreviewItem(extraData)
and
    [<Export>]
    DocumentNavigator() =

        [<Import(typeof<Microsoft.VisualStudio.Shell.SVsServiceProvider>); DefaultValue>]
        val mutable serviceProvider: IServiceProvider

        member internal x.NavigateTo(position: NavigateToItemExtraData) =
            let mutable hierarchy = Unchecked.defaultof<_>
            let mutable itemId = Unchecked.defaultof<_>
            let mutable windowFrame = Unchecked.defaultof<_>

            let isOpened = 
                VsShellUtilities.IsDocumentOpen(
                    x.serviceProvider, 
                    position.FileName, 
                    Constants.LogicalViewTextGuid,
                    &hierarchy,
                    &itemId,
                    &windowFrame)
            let canShow = 
                if isOpened then true
                else
                    // TODO: track the project that contains document and open document in project context
                    let opened = 
                        VsShellUtilities.TryOpenDocument(
                            x.serviceProvider, 
                            position.FileName, 
                            Constants.LogicalViewTextGuid, 
                            &hierarchy,
                            &itemId,
                            &windowFrame)
                    ErrorHandler.Succeeded(opened)
            if canShow then
                windowFrame.Show()
                |> ensureSucceded

                let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                let vsTextManager = x.serviceProvider.GetService(typeof<SVsTextManager>) :?> IVsTextManager
                let mutable vsTextBuffer = Unchecked.defaultof<_>
                vsTextView.GetBuffer(&vsTextBuffer)
                |> ensureSucceded

                let (startRow, startCol), (endRow, endCol) = position.Span
                vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, startRow, startCol, endRow, endCol)
                |> ensureSucceded
        member internal x.GetProvisionalViewingStatus(position: NavigateToItemExtraData) = 
            int (VsShellUtilities.GetProvisionalViewingStatus(position.FileName)) 
        member internal x.PreviewItem(position: NavigateToItemExtraData) = 
            x.NavigateTo(position)
