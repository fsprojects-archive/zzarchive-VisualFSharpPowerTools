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

    // Now the input is an entity or a member/value.
    // We always generate the full enclosing entity signature if the symbol is a member/value
    let navigateToMetadata displayContext (span: SnapshotSpan) parseTree (fsSymbol: FSharpSymbol) = 
        let fileName =
            match fsSymbol with
            | :? FSharpMemberFunctionOrValue as mem ->
                match mem.LogicalEnclosingEntity.TryGetFullName() with
                | Some fullName -> fullName + ".fsi"
                | _ -> "tmp.fsi"
            | _ ->
                try fsSymbol.FullName + ".fsi"
                with _ ->
                    try fsSymbol.DisplayName + ".fsi"
                    with _ -> "tmp.fsi"

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
            match SignatureGenerator.formatSymbol indentSize displayContext openDeclarations fsSymbol with
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