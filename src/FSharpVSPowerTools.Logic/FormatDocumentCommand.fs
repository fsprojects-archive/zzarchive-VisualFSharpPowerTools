namespace FSharpVSPowerTools.CodeFormatting

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Fantomas.FormatConfig
open Fantomas
open FSharpVSPowerTools.ProjectSystem

type FormatDocumentCommand(getConfig: Func<FormatConfig>) =
    inherit FormatCommand(getConfig, hasSelection = false)

    override x.Execute() =
        use _disposable = Cursor.wait()
        x.ExecuteFormat()

    override __.GetFormatted(filePath, source, config, projectOptions, checker) =
        async {
            let! formattedText = CodeFormatter.FormatDocumentAsync(filePath, source, config, projectOptions, checker)

            return { OldTextStartIndex = 0
                     OldTextLength = source.Length
                     NewText = formattedText }
        }

    override x.SetNewCaretPosition(caretPos, scrollBarPos, originalSnapshot) =
        let caretLine = originalSnapshot.GetLineFromPosition(caretPos.Position)
        let line = originalSnapshot.GetLineNumberFromPosition(int caretPos)
        let column = caretPos.Position - caretLine.Start.Position
        let maxLine = originalSnapshot.LineCount

        let currentSnapshot = x.TextBuffer.CurrentSnapshot
        let newMaxLine = currentSnapshot.LineCount

        // Scale caret positions in a linear way
        let newLineNo = int (float line * (float newMaxLine) / (float maxLine))
        let newLine = currentSnapshot.GetLineFromLineNumber(newLineNo)
        let newColumn = min column newLine.Length
        let newCaretPos = newLine.Start.Add(newColumn)
        let caretPolet = VirtualSnapshotPoint(currentSnapshot, int newCaretPos)
        x.TextView.Caret.MoveTo(caretPolet) |> ignore

        // Assume that the document scales in a linear way
        let newScrollBarPos = int (float scrollBarPos * (float newMaxLine) / (float maxLine))
        x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, newScrollBarPos)