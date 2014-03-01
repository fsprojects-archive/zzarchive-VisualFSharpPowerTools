namespace FSharpVSPowerTools.CodeFormatting.Commands

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Fantomas.FormatConfig
open FSharpVSPowerTools.CodeFormatting.Utils

type FormatDocumentCommand(getConfig: Func<FormatConfig>) =
    inherit FormatCommand(getConfig)

    override this.Execute() =
        use disposable = Cursor.Wait()
        this.ExecuteFormat()

    override this.GetFormatted(isSignatureFile: bool, source: string, config: Fantomas.FormatConfig.FormatConfig): string =
        Fantomas.CodeFormatter.formatSourceString isSignatureFile source config

    override this.GetNewCaretPositionSetter() =
        let currentSnapshot = this.TextView.TextBuffer.CurrentSnapshot

        let caretPos = this.TextView.Caret.Position.BufferPosition
        let caretLine = currentSnapshot.GetLineFromPosition(caretPos.Position)
        let line = currentSnapshot.GetLineNumberFromPosition(int caretPos)
        let column = caretPos - caretLine.Start
        
        // Get start line of scroll bar
        let scrollBarLine = this.TextView.TextViewLines.FirstOrDefault(fun l -> l.VisibilityState <> VisibilityState.Hidden)
        let scrollBarPos =
            match scrollBarLine with
            | null -> 0
            | _ -> currentSnapshot.GetLineNumberFromPosition(int scrollBarLine.Start)
        let maxLine = currentSnapshot.LineCount

        let setNewCaretPosition() =
            let newCurrentSnapshot = this.TextView.TextBuffer.CurrentSnapshot
            let newMaxLine = newCurrentSnapshot.LineCount

            // Scale caret positions in a linear way
            let newLine = int (float line * (float newMaxLine) / (float maxLine))
            let newCaretPos = newCurrentSnapshot.GetLineFromLineNumber(newLine).Start.Add(column)
            let caretPolet = VirtualSnapshotPoint(newCurrentSnapshot, int newCaretPos)
            this.TextView.Caret.MoveTo(caretPolet) |> ignore

            // Assume that the document scales in a linear way
            let newScrollBarPos = int (float scrollBarPos * (float newMaxLine) / (float maxLine))
            this.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, newScrollBarPos)

        setNewCaretPosition