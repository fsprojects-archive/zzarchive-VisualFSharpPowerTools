namespace FSharpVSPowerTools.Navigation

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

type GoToDefinitionFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider,
                          editorOptionsFactory: IEditorOptionsFactoryService, projectFactory: ProjectFactory) =

    let document = serviceProvider.GetService<EnvDTE.DTE, SDTE>().GetActiveDocument()

    let getDocumentState () =
        async {
            let projectItems = maybe {
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! doc = document
                let! project = projectFactory.CreateForDocument view.TextBuffer doc
                let! span, symbol = vsLanguageService.GetSymbol(caretPos, project)
                return doc.FullName, project, span, symbol }

            match projectItems with
            | Some (file, project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, file, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let lineStr = span.Start.GetContainingLine().GetText()
                    let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol.Line, symbol.RightColumn, lineStr, symbol.Text, false)
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
    static let mutable currentWindowFrame: IVsWindowFrame option = None 

    // Now the input is an entity or a member/value.
    // We always generate the full enclosing entity signature if the symbol is a member/value
    let navigateToMetadata project (span: SnapshotSpan) parseTree (fsSymbolUse: FSharpSymbolUse) = 
        let fsSymbol = fsSymbolUse.Symbol
        let displayContext = fsSymbolUse.DisplayContext
        let fileName = SignatureGenerator.getFileNameFromSymbol fsSymbol
        let filePath = Path.Combine(Path.GetTempPath(), fileName)
        let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
        let editorOptions = editorOptionsFactory.GetOptions(view.TextBuffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)  
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        let isOpened = 
            VsShellUtilities.IsDocumentOpen(
                serviceProvider, 
                filePath, 
                Constants.LogicalViewTextGuid,
                &hierarchy,
                &itemId,
                &windowFrame)      
        if isOpened then
            // If the buffer has been opened, we will not re-generate signatures
            windowFrame.Show() |> ensureSucceeded
        else
            let (startLine, startCol, _, _) = span.ToRange()
            let pos = mkPos (startLine+1) startCol
            let openDeclarations = 
                parseTree 
                |> Option.map (OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation pos) 
                |> Option.getOrElse []
            match SignatureGenerator.formatSymbol (getXmlDocBySignature fsSymbol) indentSize displayContext openDeclarations fsSymbol with
            | Some signature ->
                File.WriteAllText(filePath, signature)
                let canShow = 
                    try
                        VsShellUtilities.OpenDocument(
                            serviceProvider, 
                            filePath, 
                            Constants.LogicalViewTextGuid, 
                            &hierarchy,
                            &itemId,
                            &windowFrame)
                        true
                    with _ -> false
                if canShow then
                    // Ensure that only one signature is opened at a time
                    currentWindowFrame 
                    |> Option.iter (fun window -> 
                          if window <> null then
                              window.CloseFrame(uint32 __FRAMECLOSE.FRAMECLOSE_NoSave) |> ignore)
                    currentWindowFrame <- Some windowFrame
                    let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                    let mutable vsTextLines = Unchecked.defaultof<_>
                    vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                    let vsTextBuffer = vsTextLines :> IVsTextBuffer
                    match vsTextBuffer.GetStateFlags() with
                    | VSConstants.S_OK, currentFlags ->
                        // Try to set buffer to read-only mode
                        vsTextBuffer.SetStateFlags(currentFlags ||| uint32 BUFFERSTATEFLAGS.BSF_USER_READONLY) |> ignore
                    | _ -> ()
                    projectFactory.AddSignatureProjectProvider(filePath, project)
                    // We display the window after putting the project into the project system.
                    // Hopefully syntax coloring on generated signatures will catch up on time.
                    windowFrame.Show() |> ensureSucceeded
                    statusBar.SetText(Resource.goToDefinitionStatusMessage) |> ignore
            | None ->
                statusBar.SetText(Resource.goToDefinitionInvalidSymbolMessage) |> ignore  

    let shouldGenerateDefinition (fsSymbol: FSharpSymbol) =
        match fsSymbol with
        | Entity(TypedAstPatterns.Namespace, _, _)
        | Entity(ProvidedAndErasedType, _, _) -> false
        | Entity(Enum as e, _, _) when not e.IsFSharp -> false
        | _ -> true

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
                | Some (_, _, _, _, FindDeclResult.DeclFound _) 
                | None ->
                    // Run the operation on UI thread since continueCommandChain may access UI components.
                    do! Async.SwitchToContext uiContext
                    // Declaration location might exist so let Visual F# Tools handle it. 
                    return continueCommandChain()
                | Some (project, parseTree, span, fsSymbolUse, FindDeclResult.DeclNotFound _) ->
                    if shouldGenerateDefinition fsSymbolUse.Symbol then
                        return navigateToMetadata project span parseTree fsSymbolUse  
            }
        Async.StartInThreadPoolSafe (worker, cancelToken.Token)

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

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