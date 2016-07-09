﻿namespace FSharpVSPowerTools.Navigation

open System
open System.IO
open System.Threading
open System.Security
open System.Collections.Generic
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.CodeGeneration
open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.Range
open SourceLink.SymbolStore
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.Win32
open System.Text
open FSharpVSPowerTools.AsyncMaybe

[<RequireQualifiedAccess>]
type NavigationPreference =
    | SymbolSourceOrMetadata
    | SymbolSource
    | Metadata

type internal UrlChangeEventArgs(url: string) =
    inherit EventArgs()
    member __.Url = url

type GoToDefinitionFilter
    (
        textDocument: ITextDocument,
        view: IWpfTextView, 
        vsLanguageService: VSLanguageService, 
        serviceProvider: System.IServiceProvider,                          
        projectFactory: ProjectFactory,
        referenceSourceProvider: ReferenceSourceProvider,
        metadataService: NavigateToMetadataService,
        navigationPreference: NavigationPreference,
        fireNavigationEvent: bool
    ) =
    
    let urlChanged = if fireNavigationEvent then Some (Event<UrlChangeEventArgs>()) else None
    let mutable currentUrl = None

    let dte = serviceProvider.GetDte()
    let textBuffer = view.TextBuffer
    let project = lazy (projectFactory.CreateForDocument textBuffer textDocument.FilePath)

    let getDocumentState () =
        asyncMaybe {
            let fileName = textDocument.FilePath
            let! project = project.Value
            let! caretPos = textBuffer.GetSnapshotPoint view.Caret.Position
            let! span, symbol = vsLanguageService.GetSymbol(caretPos, fileName, project)

            let! result = vsLanguageService.GetFSharpSymbolUse(span, symbol, fileName, project, AllowStaleResults.MatchingSource) |> liftAsync
            match result with
            | Some (fsSymbolUse, fileScopedCheckResults) ->
              let lineText = span.Start.GetContainingLine().GetText()
              let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol, lineText, preferSignature = false) |> liftAsync
              return! Some (project, fileScopedCheckResults.ParseTree, span, fsSymbolUse, findDeclResult)
            | None -> return! None
        }

    let shouldGenerateDefinition (fsSymbol: FSharpSymbol) =
        match fsSymbol with
        | Entity(TypedAstPatterns.Namespace, _, _)
        | Entity(ProvidedAndErasedType, _, _)
        | Entity(ByRef, _, _) -> false
        | Entity(Enum as e, _, _) when not e.IsFSharp -> false
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
            Process.Start(browserUrl) |> ignore)

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

    let gotoDefinition continueCommandChain =
        let cancelToken = new CancellationTokenSource() 
        cancellationToken.Swap (fun _ -> Some (cancelToken))
        |> Option.iter (fun oldToken -> 
            oldToken.Cancel()
            oldToken.Dispose())
        let uiContext = SynchronizationContext.Current
        let worker =
            async {
                let! symbolResult = getDocumentState()
                match symbolResult with
                | Some (_, _, _, _, FSharpFindDeclResult.DeclFound _) 
                | None ->
                    // no FSharpSymbol found, here we look at #load directive
                    let! directive = 
                        asyncMaybe {
                            let! project = project.Value
                            return! vsLanguageService.GetLoadDirectiveFileNameAtCursor(textDocument.FilePath, view, project)
                        }
                    match directive with
                    | Some fileToOpen ->
                        // directive found, navigate to the file at first line
                        serviceProvider.NavigateTo(fileToOpen, startRow=0, startCol=0, endRow=0, endCol=0)
                    | None ->
                        // Run the operation on UI thread since continueCommandChain may access UI components.
                        do! Async.SwitchToContext uiContext
                        // Declaration location might exist so let Visual F# Tools handle it. 
                        return continueCommandChain()
                | Some (project, parseTree, span, fsSymbolUse, FSharpFindDeclResult.DeclNotFound _) ->
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
                                Process.Start url |> ignore
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
           