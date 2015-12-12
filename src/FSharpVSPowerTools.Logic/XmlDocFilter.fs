namespace FSharpVSPowerTools.XmlDoc

// Somewhat based on these:
// https://github.com/NoahRic/EditorItemTemplates/raw/master/CommandFilterTemplate.cs
// http://msdn.microsoft.com/en-us/library/dd885474.aspx

// A command filter for the editor. 
// Command filters get an opportunity to observe and handle commands before and after the editor acts on them.

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell.Interop

type XmlDocFilter 
     (
        textView: IVsTextView, 
        wpfTextView: IWpfTextView, 
        fileName: string, 
        projectFactory: ProjectFactory,
        languageService: VSLanguageService,
        openDocumentsTracker: IOpenDocumentsTracker,
        serviceProvider: System.IServiceProvider
     ) as self =
    
    let mutable passThruToEditor: IOleCommandTarget = null
    do if not (ErrorHandler.Succeeded(textView.AddCommandFilter(self, &passThruToEditor))) && Debugger.IsAttached then 
           Debugger.Break()

    /// Get the char for a <see cref="VSConstants.VSStd2KCmdID.TYPECHAR"/> command.
    let getTypedChar(pvaIn: IntPtr) = 
        char (Marshal.GetObjectForNativeVariant(pvaIn) :?> uint16)

    let dte = serviceProvider.GetDte()

    interface IOleCommandTarget with
        member __.Exec(pguidCmdGroup: byref<Guid>, nCmdID: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if pguidCmdGroup = VSConstants.VSStd2K && nCmdID = uint32 VSConstants.VSStd2KCmdID.TYPECHAR then
                match getTypedChar pvaIn with
                | ('/' | '<') as lastChar ->
                    let indexOfCaret = wpfTextView.Caret.Position.BufferPosition.Position 
                                        - wpfTextView.Caret.Position.BufferPosition.GetContainingLine().Start.Position 
                        
                    let curLine = wpfTextView.Caret.Position.BufferPosition.GetContainingLine().GetText()
                    let lineWithLastCharInserted = curLine.Insert (indexOfCaret, string lastChar)

                    match XmlDocComment.isBlank lineWithLastCharInserted with
                    | Some i when i = indexOfCaret ->
                        asyncMaybe {
                            // XmlDocable line #1 are 1-based, editor is 0-based
                            let curLineNum = wpfTextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber + 1 
                            let! document = dte.GetCurrentDocument fileName
                            let! project = projectFactory.CreateForDocument wpfTextView.TextBuffer document
                            let! parseResults = languageService.ParseFileInProject (fileName, project)
                            let! source = openDocumentsTracker.TryGetDocumentText document.FullName
                            let! xmlDocables = XmlDocParser.getXmlDocables (source, parseResults.ParseTree) |> liftAsync
                            let xmlDocablesBelowThisLine = 
                                // +1 because looking below current line for e.g. a 'member'
                                xmlDocables |> List.filter (fun (XmlDocable(line,_indent,_paramNames)) -> line = curLineNum+1) 
                            match xmlDocablesBelowThisLine with
                            | [] -> ()
                            | XmlDocable(_line,indent,paramNames)::_t ->
                                // delete the slashes the user typed (they may be indented wrong)
                                wpfTextView.TextBuffer.Delete(wpfTextView.Caret.Position.BufferPosition.GetContainingLine().Extent.Span) |> ignore
                                // add the new xmldoc comment
                                let toInsert = new System.Text.StringBuilder()
                                toInsert.Append(' ', indent).AppendLine("/// <summary>")
                                        .Append(' ', indent).AppendLine("/// ")
                                        .Append(' ', indent).Append("/// </summary>") |> ignore
                                paramNames
                                |> List.iter (fun p ->
                                    toInsert.AppendLine().Append(' ', indent).Append(sprintf "/// <param name=\"%s\"></param>" p) |> ignore)
                                let _newSS = wpfTextView.TextBuffer.Insert(wpfTextView.Caret.Position.BufferPosition.Position, toInsert.ToString())
                                // move the caret to between the summary tags
                                let lastLine = wpfTextView.Caret.Position.BufferPosition.GetContainingLine()
                                let middleSummaryLine = wpfTextView.TextSnapshot.GetLineFromLineNumber(lastLine.LineNumber - 1 - paramNames.Length)
                                wpfTextView.Caret.MoveTo(wpfTextView.GetTextViewLineContainingBufferPosition(middleSummaryLine.Start)) |> ignore
                        } 
                        |> Async.Ignore 
                        |> Async.StartImmediateSafe
                    | Some _ | None -> ()
                | _ -> ()
            passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)

        member __.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD [], pCmdText: IntPtr) =
            passThruToEditor.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText)

