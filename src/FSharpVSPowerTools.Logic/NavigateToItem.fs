namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Threading
open Microsoft.VisualStudio.Language.NavigateTo.Interfaces
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open EnvDTE
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools

module Constants = 
    let LogicalViewTextGuid = Guid(LogicalViewID.TextView)
    let EmptyReadOnlyCollection = System.Collections.ObjectModel.ReadOnlyCollection([||]); 

type NavigateToItemExtraData = 
    {
        FileName: string
        Span: Range01
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
        vsCompositionService: ICompositionService,
        projectFactory: ProjectFactory
    ) =
    
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let currentVersion = VisualStudioVersion.fromDTEVersion dte.Version
    let itemDisplayFactory = 
        let candidate =
            itemDisplayFactories
            |> Seq.tryFind (fun f -> VisualStudioVersion.matches currentVersion f.Metadata.Version)

        match candidate with
        | Some l -> l.Value
        | None -> 
            let instance = VS2012NavigateToItemDisplayFactory()
            vsCompositionService.SatisfyImportsOnce(instance)
            |> ignore
            instance :> _

    interface INavigateToItemProviderFactory with
        member __.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            let navigateToEnabled = 
                let generalOptions = Setting.getGeneralOptions(serviceProvider)
                generalOptions.NavigateToEnabled
            if not navigateToEnabled then
                provider <- null
                false
            else
                provider <- new NavigateToItemProvider(openDocumentsTracker, serviceProvider, fsharpLanguageService, itemDisplayFactory,
                                                       projectFactory)
                true
and
    NavigateToItemProvider
        (
            openDocumentsTracker: OpenDocumentsTracker,
            serviceProvider: IServiceProvider,
            fsharpLanguageService: VSLanguageService,
            itemDisplayFactory: INavigateToItemDisplayFactory,
            projectFactory: ProjectFactory
        ) = 
    let processProjectsCTS = new CancellationTokenSource()
    let mutable searchCTS = CancellationTokenSource.CreateLinkedTokenSource(processProjectsCTS.Token)
    
    let projectIndexes = 
        lazy
            let listFSharpProjectsInSolution() = 
                projectFactory.ListFSharpProjectsInSolution(serviceProvider.GetService<DTE, SDTE>()) 
                |> List.map projectFactory.CreateForProject

            let openedDocuments = 
                openDocumentsTracker.MapOpenDocuments(fun (KeyValue (path, doc)) -> path, doc.Snapshot.GetText())
                |> Map.ofSeq

            let projects = 
                match listFSharpProjectsInSolution() with
                | [] -> maybe {
                            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                            let! doc = dte.GetActiveDocument()
                            let! openDoc = openDocumentsTracker.TryFindOpenDocument doc.FullName
                            let buffer = openDoc.Document.TextBuffer
                            return! projectFactory.CreateForDocument buffer doc }
                        |> Option.toArray
                | xs -> List.toArray xs
            
            // TODO: consider making index more coarse grained (i.e. 1 TCS per project instead of file)
            let length = projects |> Array.sumBy (fun p -> p.SourceFiles.Length)
            let indexPromises = Array.init length (fun _ -> new Tasks.TaskCompletionSource<_>())
            let fetchIndexes = async {
                let i = ref 0
                let counter = ref 0
                let processNavigableItemsInFile items = 
                    // TODO: consider using linear scan implementation of IIndexedNavigableItems if number of items is small
                    let indexBuilder = Navigation.Index.Builder()
                    indexBuilder.Add items
                    indexPromises.[!counter].SetResult(indexBuilder.BuildIndex())
                    incr counter

                while !i < projects.Length && not processProjectsCTS.IsCancellationRequested do
                    do! fsharpLanguageService.ProcessNavigableItemsInProject(openedDocuments, projects.[!i], processNavigableItemsInFile, processProjectsCTS.Token)
                    incr i }
            Async.StartInThreadPoolSafe fetchIndexes
            indexPromises |> Array.map (fun tcs -> tcs.Task)

    let runSearch(indexTasks: Tasks.Task<Navigation.Index.IIndexedNavigableItems>[], searchValue: string, callback: INavigateToCallback, ct: CancellationToken) = 
        let processItem (seen: HashSet<_>) (item: Navigation.NavigableItem, name: string, isOperator: bool, matchKind: Navigation.Index.MatchKind) = 
            let fileName, range01 = Microsoft.FSharp.Compiler.Range.Range.toFileZ item.Range
            let itemName = if isOperator then "(" + name + ")" else name
            if (seen.Add(itemName, fileName, range01)) then
                let kind, textKind = 
                    match item.Kind with
                    | Navigation.NavigableItemKind.Exception -> NavigateToItemKind.Class, "exception"
                    | Navigation.NavigableItemKind.Field -> NavigateToItemKind.Field, "field"
                    | Navigation.NavigableItemKind.Constructor -> NavigateToItemKind.Class, "constructor"
                    | Navigation.NavigableItemKind.Member -> NavigateToItemKind.Method, "member"
                    | Navigation.NavigableItemKind.Module -> NavigateToItemKind.Module, "module"
                    | Navigation.NavigableItemKind.ModuleAbbreviation -> NavigateToItemKind.Module, "module abbreviation"
                    | Navigation.NavigableItemKind.ModuleValue -> NavigateToItemKind.Field, "module value"
                    | Navigation.NavigableItemKind.Property -> NavigateToItemKind.Property, "property"
                    | Navigation.NavigableItemKind.Type -> NavigateToItemKind.Class, "type"
                    | Navigation.NavigableItemKind.EnumCase -> NavigateToItemKind.EnumItem, "enum"
                    | Navigation.NavigableItemKind.UnionCase -> NavigateToItemKind.Class, "union case"
                let textKind = textKind + (if item.IsSignature then "(signature)" else "(implementation)")
                let extraData = { FileName = fileName; Span = range01; Description = textKind; }
                let navigateToItem = NavigateToItem(itemName, kind, "F#", searchValue, extraData, enum (int matchKind), itemDisplayFactory)
                callback.AddItem navigateToItem

        let searchValueComputations = async {
            let seen = HashSet()
            try
                for i = 0 to indexTasks.Length - 1 do
                    let! index = Async.AwaitTask indexTasks.[i]
                    index.Find(searchValue, processItem seen)
                    callback.ReportProgress(i + 1, indexTasks.Length)
            finally
                callback.Done()
        }
        
        Async.StartInThreadPoolSafe(searchValueComputations, cancellationToken = ct)

    interface INavigateToItemProvider with
        member __.StartSearch(callback, searchValue) = 
            let token = searchCTS.Token
            let indexes = projectIndexes.Force()
            runSearch(indexes, searchValue.Trim '`', callback, token)
        member __.StopSearch() = 
            searchCTS.Cancel()
            searchCTS <- CancellationTokenSource.CreateLinkedTokenSource(processProjectsCTS.Token)

    interface IDisposable with
        member __.Dispose() = 
            processProjectsCTS.Cancel()
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
            member __.Dispose() = 
                for (KeyValue(_, (icon, bitmap))) in iconCache do
                    if not (icon === null) then
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
            member __.Name = item.Name
            member __.Glyph = icon
            member __.AdditionalInformation = extraData.FileName
            member __.Description = extraData.Description
            member __.DescriptionItems = Constants.EmptyReadOnlyCollection
            member __.NavigateTo() = 
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
                    |> ensureSucceeded

                    let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                    let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
                    let mutable vsTextBuffer = Unchecked.defaultof<_>
                    vsTextView.GetBuffer(&vsTextBuffer)
                    |> ensureSucceeded

                    let (startRow, startCol), (endRow, endCol) = extraData.Span
                    vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, startRow, startCol, endRow, endCol)
                    |> ensureSucceeded

