namespace FSharpVSPowerTools.CodeFormatting.Commands

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open FSharpVSPowerTools.CodeFormatting.Utils
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting

type FormatSelectionCommand() as this =
    inherit FormatCommand()

    let mutable isFormattingCursor = false

    override this.Execute(): unit =
        isFormattingCursor <- this.TextView.Selection.IsEmpty

        use disposable = Cursor.Wait()
        let resetSelection = this.GetSelectionResetter()
        this.ExecuteFormat()

        resetSelection()

    override this.GetFormatted(isSignatureFile: bool, source: string, config: Fantomas.FormatConfig.FormatConfig): string =
        if isFormattingCursor then
            let caretPos = new VirtualSnapshotPoint(this.TextView.TextBuffer.CurrentSnapshot, int this.TextView.Caret.Position.BufferPosition)
            let pos = TextUtils.GetFSharpPos(caretPos)
            Fantomas.CodeFormatter.formatAroundCursor isSignatureFile pos source config
        else
            let startPos = TextUtils.GetFSharpPos(this.TextView.Selection.Start)
            let endPos = TextUtils.GetFSharpPos(this.TextView.Selection.End)
            let range = Range.mkRange "fsfile" startPos endPos

            Fantomas.CodeFormatter.formatSelectionFromString isSignatureFile range source config

    override this.GetNewCaretPositionSetter() =
        let caretPos = this.TextView.Caret.Position.BufferPosition

        if (isFormattingCursor || caretPos = this.TextView.Selection.Start.Position) then
            let selStartPos = this.TextView.Selection.Start.Position.Position

            // Get start line of scroll bar
            let scrollBarLine = this.TextView.TextViewLines.FirstOrDefault(fun l -> l.VisibilityState <> VisibilityState.Hidden)
            let scrollBarPos =
                if (scrollBarLine = null) then 0 else scrollBarLine.Snapshot.GetLineNumberFromPosition(int scrollBarLine.Start)

            let setNewCaretPosition () =
                // The caret is at the start of selection, its position is unchanged
                let newSelStartPos = selStartPos
                let newActivePoint = new VirtualSnapshotPoint(this.TextView.TextBuffer.CurrentSnapshot, newSelStartPos)
                this.TextView.Caret.MoveTo(newActivePoint) |> ignore
                this.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)

            setNewCaretPosition
        else
            let selOffsetFromEnd = this.TextView.TextBuffer.CurrentSnapshot.Length - this.TextView.Selection.End.Position.Position

            // Get start line of scroll bar
            let scrollBarLine = this.TextView.TextViewLines.FirstOrDefault(fun l -> l.VisibilityState <> VisibilityState.Hidden);
            let scrollBarPos =
                if (scrollBarLine = null)
                then 0
                else scrollBarLine.Snapshot.GetLineNumberFromPosition(int scrollBarLine.Start)

            let setNewCaretPosition () =
                // The caret is at the end of selection, its offset from the end of text is unchanged
                let newSelEndPos = this.TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
                let newAnchorPoint = new VirtualSnapshotPoint(this.TextView.TextBuffer.CurrentSnapshot, newSelEndPos)

                this.TextView.Caret.MoveTo(newAnchorPoint) |> ignore
                this.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)

            setNewCaretPosition

    member this.GetSelectionResetter(): unit -> unit =
        if isFormattingCursor then
            fun () -> ()
        else
            // We're going to take advantage of the fact that nothing before or after the selection
            // should change, so the post-formatting range will start at the same point, and end at
            // the same offset from the end of the file.
            let activePointPos = this.TextView.Selection.ActivePoint.Position.Position
            let anchorPointPos = this.TextView.Selection.AnchorPoint.Position.Position
            let activePointIsAtStart = activePointPos <= anchorPointPos  // they should always be different but just in case

            let selOffsetFromStart = this.TextView.Selection.Start.Position.Position
            let selOffsetFromEnd = this.TextView.TextBuffer.CurrentSnapshot.Length - this.TextView.Selection.End.Position.Position

            let resetSelection () =
                let newSelStartPos = selOffsetFromStart
                let newSelEndPos = this.TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
                let newActivePointPos = if activePointIsAtStart then newSelStartPos else newSelEndPos
                let newAnchorPointPos = if activePointIsAtStart then newSelEndPos else newSelStartPos
                let newActivePoint = new VirtualSnapshotPoint(this.TextView.TextBuffer.CurrentSnapshot, newActivePointPos) 
                let newAnchorPoint = new VirtualSnapshotPoint(this.TextView.TextBuffer.CurrentSnapshot, newAnchorPointPos)
                this.TextView.Selection.Select(newAnchorPoint, newActivePoint)

            resetSelection