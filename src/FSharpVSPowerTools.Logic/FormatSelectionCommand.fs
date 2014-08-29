﻿namespace FSharpVSPowerTools.CodeFormatting

open System
open Microsoft.FSharp.Compiler.Range
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Fantomas.FormatConfig
open Fantomas.CodeFormatter
open FSharpVSPowerTools.ProjectSystem

type FormatSelectionCommand(getConfig: Func<FormatConfig>) =
    inherit FormatCommand(getConfig)

    let mutable isFormattingCursorPosition = false
    let mutable selStartPos = 0
    let mutable selOffsetFromEnd = 0
    let mutable isReversedSelection = false
    
    override x.Execute() =
        isFormattingCursorPosition <- x.TextView.Selection.IsEmpty
        selStartPos <- x.TextView.Selection.Start.Position.Position
        selOffsetFromEnd <- x.TextBuffer.CurrentSnapshot.Length - x.TextView.Selection.End.Position.Position
        isReversedSelection <- x.TextView.Selection.IsReversed

        use _disposable = Cursor.wait()
        x.ExecuteFormat()

    override x.GetFormatted(isSignatureFile: bool, source: string, config: FormatConfig) =
        if isFormattingCursorPosition then
            let caretPos = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, int x.TextView.Caret.Position.BufferPosition)
            let pos = TextUtils.getFSharpPos(caretPos)
            let range = inferSelectionFromCursorPos pos source
            let formattedSelection = formatSelectionOnly isSignatureFile range source config

            let snapshot = x.TextBuffer.CurrentSnapshot
            let startIndex = snapshot.GetLineFromLineNumber(range.StartLine-1).Start.Position + range.StartColumn
            let endIndex = snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn + 1

            { OldTextStartIndex = startIndex
              OldTextLength = endIndex - startIndex
              NewText = formattedSelection }
        else
            let startPos = TextUtils.getFSharpPos(x.TextView.Selection.Start)
            let startIndex = x.TextView.Selection.Start.Position.Position
            let endIndex = x.TextView.Selection.End.Position.Position
            let endPos = TextUtils.getFSharpPos(VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, endIndex-1))
            let range = mkRange "/tmp.fsx" startPos endPos
            let formattedSelection = formatSelectionOnly isSignatureFile range source config

            { OldTextStartIndex = startIndex
              OldTextLength = endIndex - startIndex
              NewText = formattedSelection }

    override x.SetNewCaretPosition(caretPos, scrollBarPos, _originalSnapshot) =
        let currentSnapshot = x.TextBuffer.CurrentSnapshot
        if isFormattingCursorPosition || caretPos = x.TextView.Selection.Start.Position then
            // The caret is at the start of selection, its position is unchanged
            let newSelStartPos = selStartPos
            let newActivePoint = new VirtualSnapshotPoint(currentSnapshot, newSelStartPos)
            x.TextView.Caret.MoveTo(newActivePoint) |> ignore
        else
            // The caret is at the end of selection, its offset from the end of text is unchanged
            let newSelEndPos = currentSnapshot.Length - selOffsetFromEnd
            let newAnchorPoint = VirtualSnapshotPoint(currentSnapshot, newSelEndPos)
            x.TextView.Caret.MoveTo(newAnchorPoint) |> ignore
        x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)

        if not isFormattingCursorPosition then
            // We're going to take advantage of the fact that nothing before or after the selection
            // should change, so the post-formatting range will start at the same point, and end at
            // the same offset from the end of the file.
            let newSelStartPos = selStartPos
            let newSelEndPos = currentSnapshot.Length - selOffsetFromEnd

            let newActivePointPos = if isReversedSelection then newSelStartPos else newSelEndPos
            let newAnchorPointPos = if isReversedSelection then newSelEndPos else newSelStartPos
            let newActivePoint = VirtualSnapshotPoint(currentSnapshot, newActivePointPos) 
            let newAnchorPoint = VirtualSnapshotPoint(currentSnapshot, newAnchorPointPos)
            x.TextView.Selection.Select(newAnchorPoint, newActivePoint)