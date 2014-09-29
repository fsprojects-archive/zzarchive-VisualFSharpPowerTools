namespace FSharpVSPowerTools.Navigation

open System
open System.IO
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
open System.Collections.Generic
open System.Security

type GoToDefinitionFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider,
                          editorOptionsFactory: IEditorOptionsFactoryService, projectFactory: ProjectFactory) =
    let getDocumentState () =
        async {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let projectItems = maybe {
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! doc = dte.GetActiveDocument()
                let! project = projectFactory.CreateForDocument view.TextBuffer doc
                let! span, symbol = vsLanguageService.GetSymbol(caretPos, project)
                return doc.FullName, project, span, symbol }

            match projectItems with
            | Some (file, project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, file, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let fsSymbol = fsSymbolUse.Symbol
                    let lineStr = span.Start.GetContainingLine().GetText()
                    let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol.Line, symbol.RightColumn, lineStr, symbol.Text, false)
                    return Some (fsSymbol, fsSymbolUse.DisplayContext, span, fileScopedCheckResults.GetUntypedAst(), findDeclResult) 
                | _ -> return None
            | _ -> return None
        }

    let xmlDocCache = Dictionary<string, IVsXMLMemberIndex>()
    let xmlIndexService = serviceProvider.GetService<IVsXMLMemberIndexService, SVsXMLMemberIndexService>()
    /// If the XML comment starts with '<' not counting whitespace then treat it as a literal XML comment.
    /// Otherwise, escape it and surround it with <summary></summary>
    let processXml (xml:string) =
        if String.IsNullOrEmpty(xml) then xml
        else
            let trimmedXml = xml.TrimStart([|' ';'\r';'\n'|])
            if trimmedXml.Length>0 then
                if trimmedXml.[0] <> '<' then 
                    let escapedXml = SecurityElement.Escape(xml)
                    String.Join("", "<summary>", escapedXml, "</summary>")
                else 
                    String.Join("", "<root>", xml, "</root>")
            else xml

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

    // Now the input is an entity or a member/value.
    // We always generate the full enclosing entity signature if the symbol is a member/value
    let navigateToMetadata displayContext (span: SnapshotSpan) parseTree (fsSymbol: FSharpSymbol) = 
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
            // TODO: navigate to exaction location
            windowFrame.Show() |> ensureSucceeded
        else
            let (startLine, startCol, _, _) = span.ToRange()
            let pos = mkPos (startLine+1) startCol
            let openDeclarations = parseTree |> Option.map (OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation pos) |> Option.getOrElse []
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
                    windowFrame.Show() |> ensureSucceeded
                    let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                    let mutable vsTextLines = Unchecked.defaultof<_>
                    vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                    let vsTextBuffer = vsTextLines :> IVsTextBuffer
                    match vsTextBuffer.GetStateFlags() with
                    | VSConstants.S_OK, currentFlags ->
                        // Try to set buffer to read-only mode
                        vsTextBuffer.SetStateFlags(currentFlags ||| uint32 BUFFERSTATEFLAGS.BSF_USER_READONLY) |> ignore
                    | _ -> ()
                    statusBar.SetText("Generated symbol metadata") |> ignore  
            | None ->
                statusBar.SetText("Can't generate metadata for this symbol.") |> ignore  

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if pguidCmdGroup = Constants.guidOldStandardCmdSet && nCmdId = Constants.cmdidGoToDefinition then
                let symbolResult = getDocumentState () |> Async.RunSynchronously
                let shouldGenerateDefinition (fsSymbol: FSharpSymbol) =
                    match fsSymbol with
                    | :? FSharpEntity as e when e.IsNamespace -> false
                    | :? FSharpEntity as e when e.IsProvidedAndErased -> false
                    | :? FSharpEntity as e when e.IsEnum && not e.IsFSharp -> false
                    | _ -> true

                match symbolResult with
                | Some (_, _, _, _, FindDeclResult.DeclFound _) 
                | None ->
                    // Declaration location might exist so let's Visual F# Tools handle it  
                    x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)
                | Some (fsSymbol, displayContext, span, parseTree, FindDeclResult.DeclNotFound _) ->
                    if shouldGenerateDefinition fsSymbol then
                        navigateToMetadata displayContext span parseTree fsSymbol
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