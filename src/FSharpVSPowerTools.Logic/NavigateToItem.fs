namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Drawing
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Threading
open Microsoft.VisualStudio.Language.NavigateTo.Interfaces
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open EnvDTE
open FSharpVSPowerTools
open FSharpVSPowerTools.Navigation
open System.Collections.Concurrent
open System.IO
open FSharpVSPowerTools.AsyncMaybe

module Constants = 
    let EmptyReadOnlyCollection = System.Collections.ObjectModel.ReadOnlyCollection([||])

type NavigateToItemExtraData = 
    { FileName: string
      Span: NavigableItemRange
      Description: string }

module private ItemKind =
    let toKinds = function
        | NavigableItemKind.Exception -> NavigateToItemKind.Class, "exception"
        | NavigableItemKind.Field -> NavigateToItemKind.Field, "field"
        | NavigableItemKind.Constructor -> NavigateToItemKind.Class, "constructor"
        | NavigableItemKind.Member -> NavigateToItemKind.Method, "member"
        | NavigableItemKind.Module -> NavigateToItemKind.Module, "module"
        | NavigableItemKind.ModuleAbbreviation -> NavigateToItemKind.Module, "module abbreviation"
        | NavigableItemKind.ModuleValue -> NavigateToItemKind.Field, "module value"
        | NavigableItemKind.Property -> NavigateToItemKind.Property, "property"
        | NavigableItemKind.Type -> NavigateToItemKind.Class, "type"
        | NavigableItemKind.EnumCase -> NavigateToItemKind.EnumItem, "enum"
        | NavigableItemKind.UnionCase -> NavigateToItemKind.Class, "union case"

    let fromGlyphGroup = function
        | NavigateToItemKind.Class -> StandardGlyphGroup.GlyphGroupClass
        | NavigateToItemKind.Constant -> StandardGlyphGroup.GlyphGroupConstant
        | NavigateToItemKind.Delegate -> StandardGlyphGroup.GlyphGroupDelegate
        | NavigateToItemKind.Enum -> StandardGlyphGroup.GlyphGroupEnum
        | NavigateToItemKind.EnumItem -> StandardGlyphGroup.GlyphGroupEnumMember
        | NavigateToItemKind.Event -> StandardGlyphGroup.GlyphGroupEvent
        | NavigateToItemKind.Field -> StandardGlyphGroup.GlyphGroupField
        | NavigateToItemKind.Interface -> StandardGlyphGroup.GlyphGroupInterface
        | NavigateToItemKind.Method -> StandardGlyphGroup.GlyphGroupMethod
        | NavigateToItemKind.Module -> StandardGlyphGroup.GlyphGroupModule
        | NavigateToItemKind.Property -> StandardGlyphGroup.GlyphGroupProperty
        | NavigateToItemKind.Structure -> StandardGlyphGroup.GlyphGroupStruct
        | other -> failwithf "Unrecognized NavigateToItemKind:%s" other

[<Export>]
type NavigationItemIconCache [<ImportingConstructor>] ([<Import>] glyphService: IGlyphService) =
    let iconCache = Dictionary<StandardGlyphGroup * StandardGlyphItem, Icon * Bitmap>()
                
    member private __.GetOrCreateIcon(glyphGroup, glyphItem): System.Drawing.Icon = 
        let key = glyphGroup, glyphItem
        iconCache 
        |> Dict.tryFind key
        |> Option.map fst
        |> Option.getOrTry (fun _ ->
            let icon, bitmap =
                match glyphService.GetGlyph(glyphGroup, glyphItem) with
                | :? Windows.Media.Imaging.BitmapSource as bs ->
                    let bmpEncoder = Windows.Media.Imaging.PngBitmapEncoder()
                    bmpEncoder.Frames.Add(Windows.Media.Imaging.BitmapFrame.Create bs)
                    let s = new System.IO.MemoryStream()
                    bmpEncoder.Save s 
                    s.Position <- 0L
                    let bitmap = new Bitmap(s)
                    Icon.FromHandle(bitmap.GetHicon()), bitmap
                | _ -> null, null
            iconCache.[key] <- (icon, bitmap)
            icon)

    member x.GetIconForNavigationItemKind kind = 
        x.GetOrCreateIcon (ItemKind.fromGlyphGroup kind, StandardGlyphItem.GlyphItemPublic)

    interface IDisposable with
        member __.Dispose() = 
            for KeyValue (_, (icon, bitmap)) in iconCache do
                if not (isNull icon) then
                    icon.Dispose()
                    bitmap.Dispose()
            iconCache.Clear()

type NavigateToItemProvider
    (
        openDocumentsTracker: IOpenDocumentsTracker,
        serviceProvider: IServiceProvider,
        languageService: VSLanguageService,
        itemDisplayFactory: INavigateToItemDisplayFactory,
        projectFactory: ProjectFactory,
        navigableItemCache: NavigableItemCache
    ) = 
    let processProjectsCTS = new CancellationTokenSource()
    let mutable searchCts = CancellationTokenSource.CreateLinkedTokenSource processProjectsCTS.Token
    
    let processParseTrees (project: IProjectProvider, files: (FileDescriptor * (unit -> Source option))[], parseTreeHandler, 
                           ct: CancellationToken) =
        let rec loop i = 
            asyncMaybe {
                if not ct.IsCancellationRequested && i < files.Length then
                    let file, getSource = files.[i]
                    let! source = getSource()
                    let! parseResults = languageService.ParseFileInProject(file.Path, source, project) |> liftAsync
                    let! ast = parseResults.ParseTree
                    parseTreeHandler file ast
                    return! loop (i + 1)
            }
        loop 0 |> Async.Ignore

    let processNavigableItemsInProject (openDocuments, project: IProjectProvider, processNavigableItems, ct: CancellationToken) =
        async {
            let (cachedItems, newItems): NavigableItem[][] * ((FileDescriptor * (unit -> Source option)) option []) =
                project.SourceFiles 
                |> Array.mapPartition (fun file ->
                    let res =
                        match openDocuments |> Map.tryFind file with
                        | Some (source: Source, descriptor) -> Some (descriptor, fun _ -> Some source)
                        | None ->
                            maybe {
                                let! lastWriteTime = File.tryGetLastWriteTime file
                                return 
                                    { Path = file; LastWriteTime = lastWriteTime },
                                    fun _ -> Option.attempt (fun _ -> File.ReadAllText file)
                            }
                        
                    match res with
                    | None -> Choice2Of2 None
                    | Some (descriptor, _) as res ->
                        match navigableItemCache.TryGet descriptor with
                        | Some items -> Choice1Of2 items
                        | None -> Choice2Of2 res)

            let newItems = newItems |> Array.choose id
            cachedItems |> Array.iter processNavigableItems

            let processAst (file: FileDescriptor) ast =
                let items = Navigation.NavigableItemsCollector.collect file.Path ast |> Seq.toArray
                navigableItemCache.Add (file, items)
                processNavigableItems items

            return! processParseTrees(project, newItems, processAst, ct)
        }

    let projectIndexes = 
        lazy
            let listFSharpProjectsInSolution() = 
                projectFactory.ListFSharpProjectsInSolution(serviceProvider.GetService<DTE, SDTE>()) 
                |> List.map projectFactory.CreateForProject

            let openedDocuments = 
                openDocumentsTracker.MapOpenDocuments (fun (KeyValue (path, doc)) -> 
                    path, (doc.Text.Value, { Path = path; LastWriteTime = doc.LastChangeTime }))
                |> Map.ofSeq

            let projects = 
                match listFSharpProjectsInSolution() with
                | [] -> 
                    maybe {
                        let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                        let! doc = dte.GetActiveDocument()
                        let! openDoc = openDocumentsTracker.TryFindOpenDocument doc.FullName
                        let buffer = openDoc.Document.TextBuffer
                        return! projectFactory.CreateForDocument buffer doc 
                    } |> Option.toArray
                | xs -> List.toArray xs
            
            // TODO: consider making index more coarse grained (i.e. 1 TCS per project instead of file)
            let length = projects |> Array.sumBy (fun p -> p.SourceFiles.Length)
            let indexPromises = Array.init length (fun _ -> Tasks.TaskCompletionSource())
            let fetchIndexes = 
                async {
                    let i = ref 0
                    let counter = ref 0
                    let processNavigableItemsInFile items = 
                        // TODO: consider using linear scan implementation of IIndexedNavigableItems if number of items is small
                        let indexBuilder = Index.Builder()
                        indexBuilder.Add items
                        indexPromises.[!counter].SetResult(indexBuilder.BuildIndex())
                        incr counter
                    
                    while !i < projects.Length && not processProjectsCTS.IsCancellationRequested do
                        do! processNavigableItemsInProject(openedDocuments, projects.[!i], processNavigableItemsInFile, processProjectsCTS.Token)
                        incr i 
                }
            Async.StartInThreadPoolSafe fetchIndexes
            indexPromises |> Array.map (fun tcs -> tcs.Task)

    let runSearch(indexTasks: Tasks.Task<Index.IIndexedNavigableItems>[], searchValue: string, callback: INavigateToCallback, ct) = 
        let processItem (seen: ConcurrentDictionary<_, unit>) (item: NavigableItem, name, isOperator, matchKind: Index.MatchKind) = 
            let itemName = if isOperator then "(" + name + ")" else name
            if seen.TryAdd ((itemName, item.FilePath, item.Range), ()) then
                let kind, textKind = ItemKind.toKinds item.Kind
                let textKind = textKind + (if item.IsSignature then "(signature)" else "(implementation)")
                let extraData = { FileName = item.FilePath; Span = item.Range; Description = textKind; }
                let navigateToItem = NavigateToItem(itemName, kind, "F#", searchValue, extraData, enum (int matchKind), itemDisplayFactory)
                callback.AddItem navigateToItem

        let searchValueComputations = 
            async {
                try
                    let seen = ConcurrentDictionary()
                    let completedCount = ref 0
                    do! indexTasks
                        |> Array.map (fun task ->
                            async {
                                let! index = Async.AwaitTask task
                                index.Find(searchValue, processItem seen)
                                callback.ReportProgress(Interlocked.Increment completedCount, indexTasks.Length)
                            })
                        |> Async.Parallel
                        |> Async.Ignore
                finally 
                    callback.Done()
            }
        
        Async.StartInThreadPoolSafe(searchValueComputations, cancellationToken = ct)

    member __.ProcessNavigableItemsInProject (openDocs, project, ct): Async<NavigableItem list> =
        async {
            let result = ResizeArray()
            do! processNavigableItemsInProject (openDocs, project, (fun items -> result.AddRange items), ct)
            return List.ofSeq result
        }  

    interface INavigateToItemProvider with
        member __.StartSearch(callback, searchValue) = 
            let token = searchCts.Token
            let indexes = projectIndexes.Force()
            runSearch(indexes, searchValue.Trim '`', callback, token)
        member __.StopSearch() = 
            searchCts.Cancel()
            searchCts <- CancellationTokenSource.CreateLinkedTokenSource processProjectsCTS.Token

    interface IDisposable with
        member __.Dispose() = processProjectsCTS.Cancel()

type NavigateToItemDisplay(item: NavigateToItem, icon, serviceProvider: IServiceProvider) =
    let extraData: NavigateToItemExtraData = unbox item.Tag
    interface INavigateToItemDisplay with
        member __.Name = item.Name
        member __.Glyph = icon
        member __.AdditionalInformation = extraData.FileName
        member __.Description = extraData.Description
        member __.DescriptionItems = Constants.EmptyReadOnlyCollection
        member __.NavigateTo() = 
            serviceProvider.NavigateTo(extraData.FileName, extraData.Span.Start.Row, 
                extraData.Span.Start.Col, extraData.Span.End.Row, extraData.Span.End.Col)

[<ExportWithMinimalVisualStudioVersion(typeof<INavigateToItemDisplayFactory>, Version = VisualStudioVersion.VS2012)>]
type VS2012NavigateToItemDisplayFactory() =
    [<Import(typeof<SVsServiceProvider>); DefaultValue>]
    val mutable ServiceProvider: IServiceProvider
    [<Import; DefaultValue>]
    val mutable IconCache: NavigationItemIconCache
    
    interface INavigateToItemDisplayFactory with
        member x.CreateItemDisplay(item) = 
            let icon = x.IconCache.GetIconForNavigationItemKind(item.Kind)
            NavigateToItemDisplay(item, icon, x.ServiceProvider) :> _

[<Package("f152487e-9a22-4cf9-bee6-a8f7c77f828d")>]
[<Export(typeof<INavigateToItemProviderFactory>)>]
type NavigateToItemProviderFactory 
    [<ImportingConstructor>]
    (
        openDocumentsTracker: IOpenDocumentsTracker,
        [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
        languageService: VSLanguageService,
        [<ImportMany>] itemDisplayFactories: seq<Lazy<INavigateToItemDisplayFactory, IMinimalVisualStudioVersionMetadata>>,
        vsCompositionService: ICompositionService,
        projectFactory: ProjectFactory,
        [<Import(typeof<NavigableItemCache>)>] navigableItemCache: NavigableItemCache
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
            vsCompositionService.SatisfyImportsOnce instance |> ignore
            upcast instance

    interface INavigateToItemProviderFactory with
        member __.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            let navigateToEnabled = 
                let generalOptions = Setting.getGeneralOptions(serviceProvider)
                generalOptions.NavigateToEnabled
            if not navigateToEnabled then
                provider <- null
                false
            else
                provider <- new NavigateToItemProvider(openDocumentsTracker, serviceProvider, 
                                    languageService, itemDisplayFactory, projectFactory, navigableItemCache)
                true
