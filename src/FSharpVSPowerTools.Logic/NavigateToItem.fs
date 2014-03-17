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
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
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

module Index =
    open System.Globalization
    
    [<System.Diagnostics.DebuggerDisplay("{DebugString()}")>]
    type private IndexEntry(str: string, offset: int, item: Navigation.NavigableItem, isOperator: bool) =
        member x.String = str
        member x.Offset = offset
        member x.Length = str.Length - offset
        member x.Item = item
        member x.IsOperator = isOperator
        member x.StartsWith (s: string) = 
            if s.Length > x.Length then 
                false
            else
                CultureInfo.CurrentCulture.CompareInfo.IndexOf(str, s, offset, s.Length, CompareOptions.IgnoreCase) = offset
        member private x.DebugString() = sprintf "%s (offset %d) (%s)" (str.Substring offset) offset str

    let private IndexEntryComparer =
        {
            new IComparer<IndexEntry> with
                member x.Compare(a, b) = 
                    CultureInfo.CurrentCulture.CompareInfo.Compare(a.String, a.Offset, b.String, b.Offset, CompareOptions.IgnoreCase)
        }
        
    type IIndexedNavigableItems =
        abstract Find: searchValue: string * itemProcessor: (Navigation.NavigableItem * string * bool * MatchKind-> unit) -> unit

    type Builder() =
        let entries = ResizeArray()

        member x.Add(items: seq<Navigation.NavigableItem>) =
            for item in items do
                let isOperator, name = 
                    if PrettyNaming.IsMangledOpName item.Name then 
                        true, PrettyNaming.DecompileOpName item.Name 
                    else 
                        false, item.Name
                for i = 0 to name.Length - 1 do
                    entries.Add(IndexEntry(name, i, item, isOperator))

        member x.BuildIndex() =
            entries.Sort(IndexEntryComparer)
            {
                new IIndexedNavigableItems with
                    member x.Find(searchValue, processor) = 
                        let entryToFind = IndexEntry(searchValue, 0, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
                        let mutable pos = entries.BinarySearch(entryToFind, IndexEntryComparer)
                        if pos < 0 then pos <- ~~~pos
                        while pos < entries.Count && entries.[pos].StartsWith searchValue do
                            let entry = entries.[pos]
                            let matchKind = 
                                if entry.Offset = 0 then
                                    if entry.Length = searchValue.Length then MatchKind.Exact
                                    else MatchKind.Prefix
                                else MatchKind.Substring
                            processor(entry.Item, entry.String, entry.IsOperator, matchKind)
                            pos <- pos + 1
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
        member x.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            let navigateToEnabled = 
                try
                    // If this class is in the main project, we will use a more type-safe way to get options
                    let props = dte.Properties(Resource.vsPackageTitle, "General")
                    props.["NavigateToEnabled"].Value :?> bool
                with _ -> false
            if not navigateToEnabled then
                debug "[NavigateTo] The feature is disabled in General option page."
                provider <- null
                false
            else
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
    let processProjectsCTS = new CancellationTokenSource()
    let mutable searchCTS = CancellationTokenSource.CreateLinkedTokenSource(processProjectsCTS.Token)
    
    let projectIndexes = 
        lazy
            let dte = serviceProvider.GetService<DTE, SDTE>()
            let listFSharpProjectsInSolution() = 
                let rec handleProject (p: Project) =
                    if p === null then []
                    elif isFSharpProject p then [ProjectProvider.createForProject p]
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
            let openedDocuments = 
                openDocumentsTracker.MapOpenDocuments(fun (KeyValue (path,snapshot)) -> path, snapshot.GetText())
                |> Map.ofSeq

            let projects = 
                match listFSharpProjectsInSolution() with
                | [] -> maybe {
                            let dte = serviceProvider.GetService<EnvDTE.DTE, Microsoft.VisualStudio.Shell.Interop.SDTE>()
                            let! doc = dte.GetActiveDocument()
                            return! ProjectProvider.createForDocument doc }
                            |> Option.toArray
                | xs -> List.toArray xs
            
            // TODO: consider making index more coarse grained (i.e. 1 TCS per project instead of file)
            let length = projects |> Array.sumBy(fun p -> p.SourceFiles.Length)
            let indexPromises = Array.init length (fun _ -> new Tasks.TaskCompletionSource<_>())
            Tasks.Task.Run(fun() ->
                let mutable i = 0
                let counter = ref 0
                let processNavigableItemsInFile items = 
                    // TODO: consider using linear scan implementation of IIndexedNavigableItems if number of items is small
                    let indexBuilder = Index.Builder()
                    indexBuilder.Add items
                    indexPromises.[!counter].SetResult(indexBuilder.BuildIndex())
                    incr counter

                while i < projects.Length && not processProjectsCTS.IsCancellationRequested do
                    do fsharpLanguageService.ProcessNavigableItemsInProject(openedDocuments, projects.[i], processNavigableItemsInFile, processProjectsCTS.Token)
                    i <- i + 1
            )
            |> ignore
            indexPromises |> Array.map (fun tcs -> tcs.Task)

    let runSearch(indexTasks: Tasks.Task<Index.IIndexedNavigableItems>[], searchValue: string, callback: INavigateToCallback, ct: CancellationToken) = 
        let processItem (item: Navigation.NavigableItem, name: string, isOperator: bool, matchKind: MatchKind) = 
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
            let fileName, range01 = Microsoft.FSharp.Compiler.Range.Range.toFileZ item.Range
            let extraData = { FileName = fileName; Span = range01; Description = textKind; }
            let itemName = if isOperator then "(" + name + ")" else name
            let navigateToItem = NavigateToItem(itemName, kind, "F#", searchValue, extraData, matchKind, itemDisplayFactory)
            callback.AddItem navigateToItem

        let searchValueComputations = async {
            try
                for i = 0 to indexTasks.Length - 1 do
                    let! index = Async.AwaitTask indexTasks.[i]
                    index.Find(searchValue, processItem)
                    callback.ReportProgress(i + 1, indexTasks.Length)
            finally
                callback.Done()
        }
        
        Async.Start(searchValueComputations, cancellationToken = ct)

    interface INavigateToItemProvider with
        member x.StartSearch(callback, searchValue) = 
            let token = searchCTS.Token
            let indexes = projectIndexes.Force()
            runSearch(indexes, searchValue, callback, token)
        member x.StopSearch() = 
            searchCTS.Cancel()
            searchCTS <- CancellationTokenSource.CreateLinkedTokenSource(processProjectsCTS.Token)

    interface IDisposable with
        member x.Dispose() = 
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
            member x.Dispose() = 
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

