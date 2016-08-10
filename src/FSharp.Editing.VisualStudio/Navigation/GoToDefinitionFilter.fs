namespace FSharp.Editing.VisualStudio.Navigation

open System
open System.IO
open System.Threading
open System.Text.RegularExpressions
open Microsoft.Win32
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.VisualStudio.ProjectSystem
open SourceLink.SymbolStore
open FSharp.Editing.AsyncMaybe
open FSharp.Editing.Infrastructure
open FSharp.Editing.VisualStudio

[<RequireQualifiedAccess>]
type NavigationPreference =
    | SymbolSourceOrMetadata
    | SymbolSource
    | Metadata

type internal UrlChangeEventArgs(url: string) =
    inherit EventArgs()
    member __.Url = url

[<NoComparison; NoEquality>]
type GoToDefinitionResult =
    | FoundInternal
    | FoundExternal of (IProjectProvider * ParsedInput option * Symbol * FSharpSymbolUse)
    | FoundLoadDirective of string

type GoToDefinitionFilter
    (
        doc: ITextDocument,
        view: IWpfTextView, 
        vsLanguageService: VSLanguageService, 
        serviceProvider: System.IServiceProvider,                          
        projectFactory: ProjectFactory,
        referenceSourceProvider: ReferenceSourceProvider,
        metadataService: NavigateToMetadataService,
        navigationPreference: NavigationPreference,
        fireNavigationEvent: bool,
        processStart: System.Action<string>
    ) =
    
    let urlChanged = if fireNavigationEvent then Some (Event<UrlChangeEventArgs>()) else None
    let mutable currentUrl = None

    let dte = serviceProvider.GetDte()
    let textBuffer = view.TextBuffer
    let project() = projectFactory.CreateForDocument view.TextBuffer doc.FilePath

    let getDocumentState (point: PointInDocument<FCS>) getLexerState =
        asyncMaybe {
            let! project = project()
            let symbol = vsLanguageService.GetSymbol(point, project, getLexerState)
            match symbol with
            | Some symbol ->
                let! fsSymbolUse, fileScopedCheckResults = vsLanguageService.GetFSharpSymbolUse(point.CurrentLine.Value, symbol, project, AllowStaleResults.MatchingSource)
                let lineText = point.Line
                let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol, lineText, preferSignature = false) |> liftAsync
                return 
                    match findDeclResult with
                    | FSharpFindDeclResult.DeclFound _ -> FoundInternal
                    | FSharpFindDeclResult.DeclNotFound _ -> FoundExternal (project, fileScopedCheckResults.ParseTree, symbol, fsSymbolUse)
            | None ->
                let! loadDirectiveFile = vsLanguageService.GetLoadDirectiveFileNameAtCursor(point.File, view, project)
                return FoundLoadDirective loadDirectiveFile
        }

    let shouldGenerateDefinition (fsSymbol: FSharpSymbol) =
        match fsSymbol with
        | FSharpEntity(TypedAstPatterns.Namespace, _, _)
        | FSharpEntity(ProvidedAndErasedType, _, _)
        | FSharpEntity(ByRef, _, _) -> false
        | FSharpEntity(Enum as e, _, _) when not e.IsFSharp -> false
        | _ -> true

    let replace (oldValue: string) newValue (str: string) = str.Replace(oldValue, newValue)

    let getSymbolCacheDir() = 
        let keyName = String.Format(@"Software\Microsoft\VisualStudio\{0}.0\Debugger",
                                    dte.Version |> VisualStudioVersion.fromDTEVersion |> VisualStudioVersion.toString)
        use key = Registry.CurrentUser.OpenSubKey(keyName)
        key.GetValue("SymbolCacheDir", null)
        |> Option.ofNull
        |> Option.bind (fun o ->
            let s = string o
            if String.IsNullOrEmpty s then None else Some s)

    let navigateToSource (fsSymbol: FSharpSymbol) (url: string) = 
        fsSymbol.ImplementationLocation
        |> Option.iter (fun r ->
            // NOTE: other source code hostings might have different url templates
            let m = Regex.Matches(url, "[0-9a-fA-F]{32}") |> Seq.cast<Match> |> Seq.tryHead
            let replaceBlob (m: Match option) url =
                match m with
                | Some m when m.Success ->
                    replace m.Value (sprintf "blob/%s" m.Value) url
                | _ -> url
            let browserUrl =
                let formattedGithubUrl =  
                    sprintf "%s#L%d" 
                        (url 
                        |> replace "raw.githubusercontent" "github" 
                        |> replace "raw.github" "github"
                        |> replaceBlob m)
                        r.StartLine

                match url with
                | String.StartsWith "https://raw.githubusercontent.com" -> formattedGithubUrl
                | String.StartsWith "https://raw.github.com" -> formattedGithubUrl
                | String.StartsWith "https://github.com" -> formattedGithubUrl
                | String.StartsWith "https://bitbucket.org" -> 
                    let fileName = Path.GetFileName r.FileName
                    sprintf "%s?fileviewer=file-view-default#%s-%d" (url |> replace "/raw/" "/src/") fileName r.StartLine
                | String.Contains ".codebasehq.com" -> sprintf "%s#L%d" (url |> replace "/raw/" "/blob/") r.StartLine
                | String.StartsWith "https://gitlab.com" -> sprintf "%s#L%d" (url |> replace "/raw/" "/blob/") r.StartLine
                | other -> other

            if fireNavigationEvent then
                currentUrl <- Some url
                urlChanged |> Option.iter (fun event -> event.Trigger(UrlChangeEventArgs(url)))
            processStart.Invoke(browserUrl) |> ignore)

    let tryFindSourceUrl (fsSymbol: FSharpSymbol) = 
        let changeExt ext path = Path.ChangeExtension(path, ext)
        let combine prefix suffix = Path.Combine(prefix, suffix)
        let fileNameOf path = Path.GetFileNameSafe(path)
        let fullPathOf path = Path.GetFullPathSafe(path)
        maybe {
            let! assemblyPath = fsSymbol.Assembly.FileName
            // First try to find pdb files in the current folder
            // If not found, try to find pdb files in the symbol cache folder
            let! pdbPath = 
                maybe {
                    let localPdbPath = changeExt ".pdb" assemblyPath
                    if File.Exists(localPdbPath) then 
                        return localPdbPath
                    else
                        let! cacheDir = getSymbolCacheDir()
                        let cachePdbPath = fileNameOf assemblyPath |> combine cacheDir |> changeExt ".pdb"
                        if File.Exists(cachePdbPath) then 
                            return cachePdbPath
                        else
                            return! None
                }

            let cacheDir = fileNameOf assemblyPath |> hash |> string |> combine (Path.GetTempPath())
            Directory.CreateDirectory(cacheDir) |> ignore
            use pdbStream = File.OpenRead(pdbPath)
            let symbolCache = SymbolCache(cacheDir)
            let pdbReader = symbolCache.ReadPdb pdbStream pdbPath
            if pdbReader.IsSourceIndexed then                    
                let! range = fsSymbol.ImplementationLocation
                // Fsi files don't appear on symbol servers, so we try to get urls via its associated fs files
                let path, isFsiFile = 
                    let path = fullPathOf range.FileName
                    let isFsiFile = isSignatureFile path
                    let newPath = if isFsiFile then changeExt ".fs" path else path
                    newPath, isFsiFile
                let! url = pdbReader.GetDownloadUrl(path)
                if isFsiFile then 
                    return changeExt ".fsi" url
                else 
                    return url
            else 
                return! None
        }

    let cancellationToken = Atom None

    let handleFoundExternal project parseTree (symbol: Symbol) (fsSymbolUse: FSharpSymbolUse) (snapshotPoint: SnapshotPoint) = async {        
        let span = SnapshotSpan.MakeFromRange snapshotPoint.Snapshot symbol.Range
        match navigationPreference with
        | NavigationPreference.Metadata ->
            if shouldGenerateDefinition fsSymbolUse.Symbol then
                return! metadataService.NavigateToMetadata(project, textBuffer, parseTree, span, fsSymbolUse)
        | NavigationPreference.SymbolSourceOrMetadata
        | NavigationPreference.SymbolSource as pref ->   
            let symbol = fsSymbolUse.Symbol
            Logging.logInfo (fun _ -> sprintf "Checking symbol source of %s..." symbol.FullName)
            if symbol.Assembly.FileName
                |> Option.map (Path.GetFileNameWithoutExtension >> referenceSourceProvider.AvailableAssemblies.Contains)
                |> Option.getOrElse false then
                match referenceSourceProvider.TryGetNavigatedUrl symbol with
                | Some url ->
                    if fireNavigationEvent then
                        currentUrl <- Some url
                        urlChanged |> Option.iter (fun event -> event.Trigger(UrlChangeEventArgs(url)))                                    
                    processStart.Invoke(url) |> ignore
                | None ->
                    Logging.logWarning (fun _ -> sprintf "Can't find navigation information for %s." symbol.FullName)
            else
                match tryFindSourceUrl symbol with
                | Some url ->
                    return navigateToSource symbol url
                | None ->
                    match pref with
                    | NavigationPreference.SymbolSourceOrMetadata ->
                        if shouldGenerateDefinition symbol then
                            return! metadataService.NavigateToMetadata(project, textBuffer, parseTree, span, fsSymbolUse)
                    | _ ->
                        let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                        statusBar.SetText(Resource.goToDefinitionNoSourceSymbolMessage) |> ignore
                        return ()
    }

    let gotoDefinition continueCommandChain =
        let uiContext = SynchronizationContext.Current
        let doFallbackContinuation () = async {
              // Run the operation on UI thread since continueCommandChain may access UI components.
              do! Async.SwitchToContext uiContext
              // Declaration location might exist so let Visual F# Tools handle it. 
              return continueCommandChain()
          }

        let cancelToken = new CancellationTokenSource() 
        cancellationToken.Swap (fun _ -> Some (cancelToken))
        |> Option.iter (fun oldToken -> 
            oldToken.Cancel()
            oldToken.Dispose())

        let worker =
            async {
                let getLexerState = vsLanguageService.GetQueryLexState textBuffer
                let snapshotPoint = view.SnapshotPointAtCaret
                match snapshotPoint with
                | Some snapshotPoint ->
                    match vsLanguageService.MakePointInDocument(doc, snapshotPoint) with
                    | Some pointInDocument ->
                        let! symbolResult = getDocumentState pointInDocument getLexerState
                        match symbolResult with
                        | Some FoundInternal
                        | None -> return! doFallbackContinuation ()
                        | Some (FoundLoadDirective fileToOpen) -> 
                            // directive found, navigate to the file at first line
                            serviceProvider.NavigateTo(fileToOpen, startRow=0, startCol=0, endRow=0, endCol=0)
                        | Some (FoundExternal (project, parseTree, symbol, fsSymbolUse)) ->
                            return! handleFoundExternal project parseTree symbol fsSymbolUse snapshotPoint
                    | None -> return! doFallbackContinuation ()
                | None -> return! doFallbackContinuation ()
            }
        Async.StartInThreadPoolSafe (worker, cancelToken.Token)

    member internal __.UrlChanged = urlChanged |> Option.map (fun event -> event.Publish)
    member internal __.CurrentUrl = currentUrl

    interface IMenuCommand with
        member val IsAdded = false with get, set
        member val NextTarget = null with get, set

        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            let x = x :> IMenuCommand
            if pguidCmdGroup = Constants.guidOldStandardCmdSet && nCmdId = Constants.cmdidGoToDefinition then
                let nextTarget = x.NextTarget
                let cmdGroup = ref pguidCmdGroup
                gotoDefinition (fun _ -> nextTarget.Exec(cmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) |> ignore)
                // We assume that the operation is going to succeed.
                VSConstants.S_OK
            else
                x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = Constants.guidOldStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidGoToDefinition) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                let x = x :> IMenuCommand
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)
                
    interface IDisposable with
        member __.Dispose() = 
            cancellationToken.Value
            |> Option.iter (fun token -> 
                token.Cancel()
                token.Dispose())
           