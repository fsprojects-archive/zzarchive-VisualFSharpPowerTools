namespace FSharpVSPowerTools.CSharpToFSharp

// This will make life easier for polyglot / C# / ES6 -developer to code F#
// While you type it will first to replace:
// "var "    to  "let "            (the whole line only)
// "using "  to  "open "           (the whole line only and "using(" is not replaced)
// "(... => "  to  "(fun ... -> "  (this is for lambdas like ".Select(x => ...)" when there is no "->"-arrow already)

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

type CSharpToFSharpFilter(textView: IVsTextView, wpfTextView: IWpfTextView, filename: string, languageService: VSLanguageService) as self =
    let mutable passThruToEditor: IOleCommandTarget = null
    do if not (ErrorHandler.Succeeded(textView.AddCommandFilter(self, &passThruToEditor))) && Debugger.IsAttached then 
           Debugger.Break()

    /// Get the char for a <see cref="VSConstants.VSStd2KCmdID.TYPECHAR"/> command.
    let getTypedChar(pvaIn: IntPtr) =
        char (Marshal.GetObjectForNativeVariant(pvaIn) :?> uint16)

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdID: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if pguidCmdGroup = VSConstants.VSStd2K && nCmdID = uint32 VSConstants.VSStd2KCmdID.TYPECHAR then
                match getTypedChar pvaIn with
                | ' ' as lastChar ->
                    let pos = wpfTextView.Caret.Position.BufferPosition
                    let curLine = pos.GetContainingLine()
                    let startPos = curLine.Start.Position
                    let newLine = curLine.GetText().Insert(pos.Position - startPos, string lastChar)
                    let buffer = wpfTextView.TextBuffer

                    match newLine.TrimStart() with
                    | "var " | "using " as cmd -> 
                        let startIdx = newLine.IndexOf(cmd) + startPos
                        let toFsharp = match cmd with "var " -> "let" | "using " -> "open" | _ -> ""
                        let toReplace = Text.Span.FromBounds(startIdx, startIdx+cmd.Length-1)
                        buffer.Replace(toReplace, toFsharp) |> ignore
                        ()
                    | _ ->
                        let indexBegin = newLine.LastIndexOf('(') + startPos
                        let indexArrow = newLine.LastIndexOf("=> ") + startPos
                        let ``typing after arrow`` = indexArrow > indexBegin && pos.Position > indexArrow+1 
                        let quotes = newLine.Contains("\"") || newLine.Contains("'") || newLine.Contains("//") || newLine.Contains("(*") || newLine.Contains("``")
                        match indexBegin > startPos && ``typing after arrow`` && newLine.IndexOf("->")<0 && not(quotes) with
                        | true -> 
                            let ``kleisli arrow`` = "->"
                            let lambdaFun = "(fun "
                            let lambdaPart = Text.Span.FromBounds(indexArrow, indexArrow+2)
                            let beginPart = Text.Span.FromBounds(indexBegin, indexBegin+1)
                            buffer.Replace(lambdaPart, ``kleisli arrow``) |> ignore
                            buffer.Replace(beginPart, lambdaFun) |> ignore
                            ()
                        | false -> ()
                | _ -> ()
            passThruToEditor.Exec(&pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD [], pCmdText: IntPtr) =
            passThruToEditor.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText)