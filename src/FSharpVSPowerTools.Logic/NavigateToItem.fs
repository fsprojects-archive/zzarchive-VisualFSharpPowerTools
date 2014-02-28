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
        StringRepresentation: string
        Kind: string
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
        member this.TryCreateNavigateToItemProvider(serviceProvider, provider) = 
            provider <- new NavigateToItemProvider(this.activeViewsContainer, this.textDocumentFactoryService, serviceProvider, this.fsharpLanguageService, this.itemDisplayFactory)
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
        let rec handleProject (p : Project) =
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
        let searchPattern = System.Text.RegularExpressions.Regex(searchValue)
        let seen = HashSet()
        let reportSymbols (symbols : FSharpSymbolUse[]) = 
            for s in symbols do
                if not (seen.Add(s.Range,s.Symbol.DisplayName)) then 
                    // skip duplicates
                    ()
                else
                let name = s.Symbol.DisplayName
                let m = searchPattern.Match name
                let matchKind = 
                    if not m.Success then MatchKind.None
                    elif m.Index = 0 then
                        if m.Length = name.Length then MatchKind.Exact
                        else MatchKind.Prefix
                    else MatchKind.Substring
                if matchKind <> MatchKind.None then
                    let kind, textKind = 
                        match s.Symbol with
                        | :? FSharpField-> NavigateToItemKind.Field, "field"
                        | :? FSharpEntity as fse -> 
                            if fse.IsClass then 
                                if fse.IsMeasure then NavigateToItemKind.Class, "class"
                                else NavigateToItemKind.Class, "measure"
                            elif fse.IsFSharpExceptionDeclaration then NavigateToItemKind.Class, "exception"
                            elif fse.IsFSharpAbbreviation then NavigateToItemKind.Class, "type abbreviation"
                            elif fse.IsFSharpRecord then NavigateToItemKind.Class, "record"
                            elif fse.IsFSharpUnion then NavigateToItemKind.Class, "union"
                            elif fse.IsDelegate then NavigateToItemKind.Delegate, "delegate"
                            elif fse.IsEnum then NavigateToItemKind.Enum, "enum"
                            elif fse.IsFSharpModule then NavigateToItemKind.Module, "module"
                            elif fse.IsInterface then NavigateToItemKind.Interface, "interface" 
                            elif fse.IsValueType then NavigateToItemKind.Structure, "struct"
                            else NavigateToItemKind.Class, "???"
                        | :? FSharpMemberFunctionOrValue as fsm -> 
                            if fsm.IsGetterMethod then NavigateToItemKind.Property, "getter"
                            elif fsm.IsSetterMethod then NavigateToItemKind.Property, "setter"
                            elif fsm.IsActivePattern then NavigateToItemKind.Method, "active pattern"
                            elif fsm.IsExtensionMember then NavigateToItemKind.Method, "extension"
                            elif fsm.IsImplicitConstructor then NavigateToItemKind.Method, "constructor"
                            elif fsm.IsTypeFunction then NavigateToItemKind.Method, "type function"
                            elif fsm.IsModuleValueOrMember then NavigateToItemKind.Method, "module value or member"
                            else NavigateToItemKind.Method, "???" // TODO: recognize parameters, locals etc...
                        | _ -> NavigateToItemKind.Constant, "???" // TODO: reasonable default value
                    let extraData = { FileName = s.FileName; Span = s.Range; StringRepresentation = s.Symbol.ToString(); Kind = textKind }
                    let navigateToItem = NavigateToItem(s.Symbol.DisplayName, kind, "F#", searchValue, extraData, matchKind, itemDisplayFactory)
                    callback.AddItem navigateToItem

        let computation = async {
            for p in projects do
                if not ct.IsCancellationRequested then
                    do! fsharpLanguageService.FindSymbols(openDocuments, p, reportSymbols, ct)
            callback.Done()
        }
        Async.Start(computation)

    interface INavigateToItemProvider with
        member this.StartSearch(callback, searchValue) = 
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
        member this.StopSearch() = 
            cts.Cancel()
            cts <- new CancellationTokenSource()

    interface IDisposable with
        member this.Dispose() = (this :> INavigateToItemProvider).StopSearch()
and
    [<Export(typeof<INavigateToItemDisplayFactory>)>]
    NavigateToItemDisplayFactory() as this =
        
        [<Import; DefaultValue>] 
        val mutable glyphService: IGlyphService
        [<Import; DefaultValue>]
        val mutable navigator: DocumentNavigator

        let iconCache = Dictionary()
        let getIcon glyphGroup glyphItem: System.Drawing.Icon = 
            let key = (glyphGroup, glyphItem)
            match iconCache.TryGetValue key with
            | true, (icon, _) -> icon
            | false, _ ->
                let (icon, _) as pair =
                    match this.glyphService.GetGlyph(glyphGroup, glyphItem) with
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

        let glyphForNavigateToItemKind (kind : string) =
            let glyphGroup = 
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
            // TODO 
            let glyphItem = StandardGlyphItem.GlyphItemPublic 
            getIcon glyphGroup glyphItem

        interface INavigateToItemDisplayFactory with
            member this.CreateItemDisplay(item) = NavigateToItemDisplay(item, glyphForNavigateToItemKind item.Kind, this.navigator) :> _
        interface IDisposable with
            member this.Dispose() = 
                for (KeyValue(_, (icon, bitmap))) in iconCache do
                    if not (LanguagePrimitives.PhysicalEquality icon null) then
                        icon.Dispose()
                        bitmap.Dispose()
                iconCache.Clear()
and 
    NavigateToItemDisplay(item : NavigateToItem, icon, navigator: DocumentNavigator) =
        let extraData : NavigateToItemExtraData = unbox item.Tag
        interface INavigateToItemDisplay with
            member this.Name = item.Name
            member this.Glyph = icon
            member this.AdditionalInformation = extraData.Kind
            member this.Description = extraData.StringRepresentation
            member this.DescriptionItems = Constants.EmptyReadOnlyCollection
            member this.NavigateTo() = navigator.NavigateTo(extraData)
and
    [<Export(typeof<DocumentNavigator>)>]
    DocumentNavigator() =

        [<Import(typeof<Microsoft.VisualStudio.Shell.SVsServiceProvider>); DefaultValue>]
        val mutable serviceProvider : IServiceProvider

        member internal this.NavigateTo(position: NavigateToItemExtraData) =
            let mutable hierarchy = Unchecked.defaultof<_>
            let mutable itemId = Unchecked.defaultof<_>
            let mutable windowFrame = Unchecked.defaultof<_>

            let isOpened = 
                VsShellUtilities.IsDocumentOpen(
                    this.serviceProvider, 
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
                            this.serviceProvider, 
                            position.FileName, 
                            Constants.LogicalViewTextGuid, 
                            &hierarchy,
                            &itemId,
                            &windowFrame)
                    ErrorHandler.Succeeded(opened)
            if (canShow) then
                windowFrame.Show()
                |> ensureSucceded

                let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                let vsTextManager = this.serviceProvider.GetService(typeof<SVsTextManager>) :?> IVsTextManager
                let mutable vsTextBuffer = Unchecked.defaultof<_>
                vsTextView.GetBuffer(&vsTextBuffer)
                |> ensureSucceded

                let (startRow, startCol), (endRow, endCol) = position.Span
                vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, startRow, startCol, endRow, endCol)
                |> ensureSucceded