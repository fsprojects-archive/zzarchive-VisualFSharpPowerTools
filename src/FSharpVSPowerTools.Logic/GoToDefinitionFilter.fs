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
open System.Net.Http
open System.Text.RegularExpressions
open Microsoft.Win32
open System.Text

[<RequireQualifiedAccess>]
type NavigationPreference =
    | SymbolSourceOrMetadata
    | SymbolSource
    | Metadata

type internal UrlChangeEventArgs(url: string) =
    inherit EventArgs()
    member __.Url = url

type GoToDefinitionFilter(textDocument: ITextDocument,
                          view: IWpfTextView, 
                          editorOptionsFactory: IEditorOptionsFactoryService, 
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: System.IServiceProvider,                          
                          projectFactory: ProjectFactory,
                          referenceSourceProvider: ReferenceSourceProvider,
                          navigationPreference: NavigationPreference,
                          fireNavigationEvent: bool) =
    let urlChanged = if fireNavigationEvent then Some (Event<UrlChangeEventArgs>()) else None
    let mutable currentUrl = None

    let getCurrentFilePathProjectAndDoc () =
        maybe {
            let filepath = textDocument.FilePath
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(filepath)
            let! project = projectFactory.CreateForDocument view.TextBuffer doc
            return (filepath, project, doc)
        }

    let getDocumentState () =
        async {
            
            let projectItems = maybe {
                let! _, project, doc = getCurrentFilePathProjectAndDoc()
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! span, symbol = vsLanguageService.GetSymbol(caretPos, project)
                return doc.FullName, project, span, symbol }

            match projectItems with
            | Some (file, project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, file, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let lineStr = span.Start.GetContainingLine().GetText()
                    let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol.Line, symbol.RightColumn, lineStr, symbol.Text, preferSignature=false)
                    return Some (project, fileScopedCheckResults.GetUntypedAst(), span, fsSymbolUse, findDeclResult) 
                | _ -> return None
            | _ -> return None
        }

    // Use a single cache across text views
    static let xmlDocCache = Dictionary<string, IVsXMLMemberIndex>()
    let xmlIndexService = serviceProvider.GetService<IVsXMLMemberIndexService, SVsXMLMemberIndexService>()

    /// If the XML comment starts with '<' not counting whitespace then treat it as a literal XML comment.
    /// Otherwise, escape it and surround it with <summary></summary>
    let processXml (xml: string) =
        if String.IsNullOrEmpty(xml) then xml
        else
            let trimmedXml = xml.TrimStart([|' ';'\r';'\n'|])
            if String.IsNullOrEmpty(trimmedXml) then xml
            else
                match trimmedXml.[0] with
                | '<' ->
                    String.Join("", "<root>", xml, "</root>")
                | _ ->
                    let escapedXml = SecurityElement.Escape(xml)
                    String.Join("", "<summary>", escapedXml, "</summary>")
            
    let getXmlDocBySignature (fsSymbol: FSharpSymbol) signature =
        match fsSymbol.Assembly.FileName with
        | Some assemblyName ->
            match xmlDocCache.TryGetValue(assemblyName) with
            | true, xmlIndex -> 
                match xmlIndex.ParseMemberSignature(signature) with
                | _, 0u -> []
                | _, index ->
                    match xmlIndex.GetMemberXML(index) with
                    | VSConstants.S_OK, xml ->
                        let processedXml = processXml xml
                        let xmlDocs = ResizeArray()
                        match xmlIndexService.GetMemberDataFromXML(processedXml) with
                        | VSConstants.S_OK, memberData ->
                            match memberData.GetSummaryText() with
                            | VSConstants.S_OK, xmlSummary ->
                                xmlDocs.Add(xmlSummary)
                            | _ -> ()
                            match memberData.GetParamCount() with
                            | VSConstants.S_OK, count when count > 0 ->
                                for i in 0..count-1 do
                                    match memberData.GetParamTextAt(i) with
                                    | VSConstants.S_OK, name, text ->
                                        let xmlDoc = sprintf "%s: %s" name text
                                        xmlDocs.Add(xmlDoc)
                                    | _ -> ()
                            | _ -> ()
                            xmlDocs |> Seq.toList
                        | _ -> []                            
                    | _ -> []
            | false, _ -> 
                match xmlIndexService.CreateXMLMemberIndex(assemblyName) with
                | VSConstants.S_OK, xmlIndex ->
                    match xmlIndex.BuildMemberIndex() with
                    | VSConstants.S_OK ->
                        xmlDocCache.Add(assemblyName, xmlIndex)
                        []
                    | _ -> []
                | _ -> []
        | None -> []

    // Keep a single window frame for all text views
    static let mutable currentWindow: (IVsWindowFrame * string) option = None 

    let gotoExactLocation signature filePath signatureProject currentSymbol vsTextBuffer =
        async {
            let! symbolUses = 
                vsLanguageService.GetAllUsesOfAllSymbolsInSourceString(
                    signature, filePath, signatureProject, 
                    AllowStaleResults.No, checkForUnusedOpens=false, profiler=Profiler())

            /// Try to reconstruct fully qualified name for the purpose of matching symbols
            let rec tryGetFullyQualifiedName (symbol: FSharpSymbol) = 
                Option.attempt (fun _ -> 
                    match symbol with
                    | TypedAstPatterns.Entity (entity, _, _) ->
                        Some (sprintf "%s.%s" entity.AccessPath entity.DisplayName)
                    | MemberFunctionOrValue mem ->
                        tryGetFullyQualifiedName mem.EnclosingEntity
                        |> Option.map (fun parent -> sprintf "%s.%s" parent mem.DisplayName)
                    | Field(field, _) ->
                        tryGetFullyQualifiedName field.DeclaringEntity
                        |> Option.map (fun parent -> sprintf "%s.%s" parent field.DisplayName)
                    | UnionCase uc ->
                        match uc.ReturnType with
                        | TypeWithDefinition entity ->
                            tryGetFullyQualifiedName entity
                            |> Option.map (fun parent -> sprintf "%s.%s" parent uc.DisplayName)
                        | _ -> 
                            None
                    | ActivePatternCase case ->
                        let group = case.Group
                        group.EnclosingEntity
                        |> Option.bind tryGetFullyQualifiedName
                        |> Option.map (fun parent -> 
                            let sb = StringBuilder()
                            sb.Append("|") |> ignore
                            for name in group.Names do
                                sb.AppendFormat("{0}|", name) |> ignore
                            if not group.IsTotal then
                                sb.Append("_|") |> ignore
                            sprintf "%s.( %O )" parent sb)
                    | _ ->
                        None)
                |> Option.flatten
                |> Option.orTry (fun _ -> Option.attempt (fun _ -> symbol.FullName))

            let isLocalSymbol filePath (symbol: FSharpSymbol) =
                symbol.DeclarationLocation 
                |> Option.map (fun r -> String.Equals(r.FileName, filePath, StringComparison.OrdinalIgnoreCase)) 
                |> Option.getOrElse false

            let currentSymbolFullName = tryGetFullyQualifiedName currentSymbol

            let matchedSymbol = 
                symbolUses 
                |> Seq.groupBy (fun { SymbolUse = symbolUse } -> symbolUse.Symbol)
                |> Seq.collect (fun (_, uses) -> Seq.truncate 1 uses)
                |> Seq.choose (fun { SymbolUse = symbolUse } -> 
                    match symbolUse.Symbol with
                    | TypedAstPatterns.Entity _
                    | MemberFunctionOrValue _
                    | ActivePatternCase _                   
                    | UnionCase _
                    | Field _ as symbol -> 
                        let symbolFullName = tryGetFullyQualifiedName symbol
                        if symbolFullName = currentSymbolFullName && isLocalSymbol filePath symbol then
                            Some symbol
                        else None
                    | _ -> None)
                |> Seq.sortBy (fun symbol -> 
                    symbol.DeclarationLocation |> Option.map (fun r -> r.StartLine, r.StartColumn))
                |> Seq.tryHead

            match matchedSymbol with
            | Some symbol ->
                let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
                symbol.DeclarationLocation
                |> Option.iter (fun r -> 
                    let (startRow, startCol) = (r.StartLine-1, r.StartColumn)
                    vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.guidLogicalTextView, startRow, startCol, startRow, startCol) 
                    |> ensureSucceeded)
            | None ->
                Logging.logInfo "Can't find a matching symbol for '%A'" currentSymbol
        }

    // Now the input is an entity or a member/value.
    // We always generate the full enclosing entity signature if the symbol is a member/value
    let navigateToMetadata project (span: SnapshotSpan) ast (fsSymbolUse: FSharpSymbolUse) = 
        async {
            let fsSymbol = fsSymbolUse.Symbol
            let displayContext = fsSymbolUse.DisplayContext
            let fileName = SignatureGenerator.getFileNameFromSymbol fsSymbol

            // The file system is case-insensitive so list.fsi and List.fsi can clash
            // Thus, we generate a tmp subfolder based on the hash of the filename
            let subFolder = string (uint32 (hash fileName))

            let filePath = Path.Combine(Path.GetTempPath(), subFolder, fileName)
            let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
            let editorOptions = editorOptionsFactory.GetOptions(view.TextBuffer)
            let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)  
            match VsShellUtilities.IsDocumentOpen(serviceProvider, filePath, Constants.guidLogicalTextView) with
            | true, _hierarchy, _itemId, windowFrame ->
                let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                let mutable vsTextLines = Unchecked.defaultof<_>
                vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                let vsTextBuffer = vsTextLines :> IVsTextBuffer                
                match currentWindow, projectFactory.TryGetSignatureProjectProvider(filePath) with
                | Some (_, signature), Some signatureProject ->
                    do! gotoExactLocation signature filePath signatureProject fsSymbol vsTextBuffer
                | _ -> 
                    Logging.logInfo "Can't find a signature or signature project for '%s'" filePath
                // If the buffer has been opened, we will not re-generate signatures
                windowFrame.Show() |> ensureSucceeded
            | _ ->
                let (startLine, startCol, _, _) = span.ToRange()
                let pos = mkPos (startLine+1) startCol
                let openDeclarations = 
                    ast 
                    |> Option.map (OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation pos) 
                    |> Option.getOrElse []
                match SignatureGenerator.formatSymbol 
                        (getXmlDocBySignature fsSymbol) indentSize displayContext openDeclarations fsSymbol 
                        SignatureGenerator.Filterer.NoFilters SignatureGenerator.BlankLines.Default with
                | Some signature ->
                    let directoryPath = Path.GetDirectoryName(filePath)
                    Directory.CreateDirectory(directoryPath) |> ignore
                    File.WriteAllText(filePath, signature)
                    let mutable hierarchy = Unchecked.defaultof<_>
                    let mutable itemId = Unchecked.defaultof<_>
                    let windowFrame = ref null
                    let canShow = 
                        try
                            VsShellUtilities.OpenDocument(
                                serviceProvider, 
                                filePath, 
                                Constants.guidLogicalTextView, 
                                &hierarchy,
                                &itemId,
                                windowFrame)
                            true
                        with _ -> false
                    if canShow then
                        // Ensure that only one signature is opened at a time
                        currentWindow 
                        |> Option.bind (fun (window, _) -> Option.ofNull window)
                        |> Option.iter (fun window -> window.CloseFrame(uint32 __FRAMECLOSE.FRAMECLOSE_NoSave) |> ignore)
                        // Prevent the window being reopened as a part of a solution
                        (!windowFrame).SetProperty(int __VSFPROPID5.VSFPROPID_DontAutoOpen, true) |> ignore
                        currentWindow <- Some (!windowFrame, signature)
                        let vsTextView = VsShellUtilities.GetTextView(!windowFrame)
                        let mutable vsTextLines = Unchecked.defaultof<_>
                        vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                        let vsTextBuffer = vsTextLines :> IVsTextBuffer
                        match vsTextBuffer.GetStateFlags() with
                        | VSConstants.S_OK, currentFlags ->
                            // Try to set buffer to read-only mode
                            vsTextBuffer.SetStateFlags(currentFlags ||| uint32 BUFFERSTATEFLAGS.BSF_USER_READONLY) |> ignore
                        | _ -> ()
                        let signatureProject = projectFactory.RegisterSignatureProjectProvider(filePath, project)
                        do! gotoExactLocation signature filePath signatureProject fsSymbol vsTextBuffer
                        // We display the window after putting the project into the project system.
                        // Hopefully syntax coloring on generated signatures will catch up on time.
                        (!windowFrame).Show() |> ensureSucceeded
                        statusBar.SetText(Resource.goToDefinitionStatusMessage) |> ignore
                | None ->
                    statusBar.SetText(Resource.goToDefinitionInvalidSymbolMessage) |> ignore  
        }

    let shouldGenerateDefinition (fsSymbol: FSharpSymbol) =
        match fsSymbol with
        | Entity(TypedAstPatterns.Namespace, _, _)
        | Entity(ProvidedAndErasedType, _, _)
        | Entity(ByRef, _, _) -> false
        | Entity(Enum as e, _, _) when not e.IsFSharp -> false
        | _ -> true

    let replace (b:string) c (a:string) = a.Replace(b, c)

    let getSymbolCacheDir() = 
        let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
        let keyName = String.Format(@"Software\Microsoft\VisualStudio\{0}.0\Debugger",
                                    dte.Version |> VisualStudioVersion.fromDTEVersion |> VisualStudioVersion.toString)
        use key = Registry.CurrentUser.OpenSubKey(keyName)
        key.GetValue("SymbolCacheDir", null)
        |> Option.ofNull
        |> Option.bind (fun o ->
            let s = string o
            if String.IsNullOrEmpty s then None else Some s)

    let navigateToSource (fsSymbol: FSharpSymbol) (url: string) = 
        async {
            use handler = new HttpClientHandler(UseDefaultCredentials = true)
            use http = new HttpClient(handler)
            let! response = http.GetAsync(url) |> Async.AwaitTask
            if response.IsSuccessStatusCode then
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
                        | String.StartsWith "https://raw.githubusercontent.com" _-> formattedGithubUrl
                        | String.StartsWith "https://raw.github.com" _-> formattedGithubUrl
                        | String.StartsWith "https://github.com" _-> formattedGithubUrl
                        | String.StartsWith "https://bitbucket.org" _-> 
                            sprintf "%s#cl-%d" (url |> replace "/raw/" "/src/") r.StartLine
                        | String.Contains ".codebasehq.com" _-> sprintf "%s#L%d" (url |> replace "/raw/" "/blob/") r.StartLine
                        | String.StartsWith "https://gitlab.com" _-> sprintf "%s#L%d" (url |> replace "/raw/" "/blob/") r.StartLine
                        | other -> other

                    if fireNavigationEvent then
                        currentUrl <- Some url
                        urlChanged |> Option.iter (fun event -> event.Trigger(UrlChangeEventArgs(url)))                        
                    Process.Start(browserUrl) |> ignore)
            else
                let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                statusBar.SetText(sprintf "The url '%s' is invalid with error code %A." url response.StatusCode) |> ignore
        }

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
                    let isFsiFile = path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)
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
                            let! filepath, project, _ = getCurrentFilePathProjectAndDoc()
                            return! vsLanguageService.GetLoadDirectiveFileNameAtCursor(filepath, view, project)
                        }
                    match directive with
                    | Some fileToOpen ->
                        // directive found, navigate to the file at first line
                        serviceProvider.NavigateTo(fileToOpen, 0, 0, 0, 0)
                    | None ->
                        // Run the operation on UI thread since continueCommandChain may access UI components.
                        do! Async.SwitchToContext uiContext
                        // Declaration location might exist so let Visual F# Tools handle it. 
                        return continueCommandChain()
                | Some (project, parseTree, span, fsSymbolUse, FSharpFindDeclResult.DeclNotFound _) ->
                    match navigationPreference with
                    | NavigationPreference.Metadata ->
                        if shouldGenerateDefinition fsSymbolUse.Symbol then
                            return! navigateToMetadata project span parseTree fsSymbolUse
                    | NavigationPreference.SymbolSourceOrMetadata
                    | NavigationPreference.SymbolSource as pref ->   
                        let symbol = fsSymbolUse.Symbol
                        Logging.logInfo "Checking symbol source of %s..." symbol.FullName
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
                                Logging.logWarning "Can't find navigation information for %s." symbol.FullName
                        else
                            match tryFindSourceUrl symbol with
                            | Some url ->
                                return! navigateToSource symbol url
                            | None ->
                                match pref with
                                | NavigationPreference.SymbolSourceOrMetadata ->
                                    if shouldGenerateDefinition symbol then
                                        return! navigateToMetadata project span parseTree fsSymbolUse
                                | _ ->
                                    let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                                    statusBar.SetText(Resource.goToDefinitionNoSourceSymbolMessage) |> ignore
                                    return ()
            }
        Async.StartInThreadPoolSafe (worker, cancelToken.Token)

    static member ClearXmlDocCache() =
        xmlDocCache.Clear()

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    member internal __.UrlChanged = urlChanged |> Option.map (fun event -> event.Publish)
    member internal __.CurrentUrl = currentUrl

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
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
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)
                
    interface IDisposable with
        member __.Dispose() = 
            cancellationToken.Value
            |> Option.iter (fun token -> 
                token.Cancel()
                token.Dispose())
           