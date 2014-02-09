namespace FSharpXmlDoc

// Somewhat based on these:
// https://github.com/NoahRic/EditorItemTemplates/raw/master/CommandFilterTemplate.cs
// http://msdn.microsoft.com/en-us/library/dd885474.aspx

// A command filter for the editor.  Command filters get an opportunity to observe and handle commands before and after the editor acts on them.

open System
open System.Diagnostics
open System.ComponentModel.Composition
open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities

module XmlDocComment =
    let private ws (s: string, pos) = 
        let res = s.TrimStart()
        Some (res, pos + (s.Length - res.Length))

    let private str (prefix: string) (s: string, pos) =
        match s.StartsWith prefix with
        | true -> 
            let res = s.Substring prefix.Length
            Some (res, pos + (s.Length - res.Length))
        | _ -> None

    let inline private (>=>) f g = fun x -> f x |> Option.bind g

    // if it's a blank XML comment with trailing "<", returns Some (index of the "<"), otherwise returns None
    let isBlank (s: string) =
        let parser = ws >=> str "///" >=> ws >=> str "<"
        let res = parser (s, 0) |> Option.map snd |> Option.map (fun x -> x - 1)
        res

type XmlDocFilter(textView:IVsTextView, wpfTextView:IWpfTextView, filename:string) as this =
    let mutable passThruToEditor : IOleCommandTarget = null
    do
        if ErrorHandler.Succeeded(textView.AddCommandFilter(this, &passThruToEditor)) then
            () // ok
        else
            if System.Diagnostics.Debugger.IsAttached then
                System.Diagnostics.Debugger.Break()

    /// Get the char for a <see cref="VSConstants.VSStd2KCmdID.TYPECHAR"/> command.
    let getTypedChar(pvaIn:IntPtr) =
        char (Marshal.GetObjectForNativeVariant(pvaIn) :?> uint16)

    interface IOleCommandTarget with
        member this.Exec(pguidCmdGroup:byref<Guid>, nCmdID:uint32, nCmdexecopt:uint32, pvaIn:IntPtr, pvaOut:IntPtr) =
            let hresult =
                if pguidCmdGroup = VSConstants.VSStd2K && nCmdID = uint32 VSConstants.VSStd2KCmdID.TYPECHAR then
                    match getTypedChar pvaIn with
                    | ('/' | '<') as lastChar ->
                        let curLine = wpfTextView.Caret.Position.BufferPosition.GetContainingLine().GetText()
                        let indexOfCaret = wpfTextView.Caret.Position.BufferPosition.Position 
                                           - wpfTextView.Caret.Position.BufferPosition.GetContainingLine().Start.Position 
                        match XmlDocComment.isBlank (curLine + (string lastChar)) with
                        | Some i when i = indexOfCaret ->
                            let curLineNum = wpfTextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber + 1 // XmlDocable line #1 are 1-based, editor is 0-based
                            let xmldocables = XmlDocHelpers.GetXmlDocables(wpfTextView.TextSnapshot.GetText(), filename)
                            let xmlDocablesBelowThisLine = 
                                xmldocables 
                                |> List.filter (fun (XmlDocHelpers.XmlDocable(line,_indent,_paramNames)) -> line = curLineNum+1) // +1 because looking below current line for e.g. a 'member'
                            let hr = passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut) // parse it before we pass thru to editor, as we want to notice if no XmlDoc yet
                            match xmlDocablesBelowThisLine with
                            | [] -> ()
                            | XmlDocHelpers.XmlDocable(_line,indent,paramNames)::t ->
                                // delete the slashes the user typed (they may be indented wrong)
                                wpfTextView.TextBuffer.Delete(wpfTextView.Caret.Position.BufferPosition.GetContainingLine().Extent.Span) |> ignore
                                // add the new xmldoc comment
                                let toInsert = new System.Text.StringBuilder()
                                toInsert.Append(' ', indent).AppendLine("/// <summary>")
                                        .Append(' ', indent).AppendLine("/// ")
                                        .Append(' ', indent).Append("/// </summary>") |> ignore
                                for p in paramNames do
                                    toInsert.AppendLine().Append(' ', indent).Append(sprintf "/// <param name=\"%s\"></param>" p) |> ignore
                                let newSS = wpfTextView.TextBuffer.Insert(wpfTextView.Caret.Position.BufferPosition.Position, toInsert.ToString())
                                // move the caret to between the summary tags
                                let lastLine = wpfTextView.Caret.Position.BufferPosition.GetContainingLine()
                                let middleSummaryLine = wpfTextView.TextSnapshot.GetLineFromLineNumber(lastLine.LineNumber - 1 - paramNames.Length)
                                let _newCaret = wpfTextView.Caret.MoveTo(wpfTextView.GetTextViewLineContainingBufferPosition(middleSummaryLine.Start))
                                ()
                            hr
                        | _ -> passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)
                    | _ -> passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)
                else
                    passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)

            hresult

        member this.QueryStatus(pguidCmdGroup:byref<Guid>, cCmds:uint32, prgCmds:OLECMD[], pCmdText:IntPtr) =
            passThruToEditor.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText)

