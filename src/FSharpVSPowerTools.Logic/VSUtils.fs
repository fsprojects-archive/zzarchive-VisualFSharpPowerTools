[<AutoOpen>]
module FSharpVSPowerTools.VSUtils

open System
open System.Text.RegularExpressions
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities

/// Retrieve snapshot from VS zero-based positions
let fromVSPos(snapshot : ITextSnapshot, startLine, startCol, endLine, endCol) =
    let startPos = snapshot.GetLineFromLineNumber(startLine).Start.Position + startCol
    let endPos = snapshot.GetLineFromLineNumber(endLine).Start.Position + endCol
    SnapshotSpan(snapshot, startPos, endPos - startPos)

open Microsoft.FSharp.Compiler.PrettyNaming

let isIdentifier (s : string) =
    s |> Seq.mapi (fun i c -> i, c)
      |> Seq.forall (fun (i, c) -> (i = 0 && IsIdentifierFirstCharacter c) || IsIdentifierPartCharacter c) 

let isIdentifierOrOperator (s : string) =
    isIdentifier s || IsOpName s

type SnapshotPoint with
    member this.WordExtentIsValid(word : TextExtent) =
        word.IsSignificant && isIdentifierOrOperator(this.Snapshot.GetText(word.Span.Span))
        

type SnapshotSpan with
    member this.GetWordIncludingQuotes() =
        let endWordPos = ref this.End.Position
        let mutable word = this.GetText().Trim()
        let mutable currentWord = this

        while !endWordPos < currentWord.Snapshot.Length && currentWord.Snapshot.GetText(!endWordPos, 1) = "\'" do
            word <- word + "\'"
            incr endWordPos

        if word.EndsWith("\'") then
            currentWord <- SnapshotSpan(currentWord.Snapshot, currentWord.Start.Position, word.Length)

        while !endWordPos < currentWord.Snapshot.Length && currentWord.Snapshot.GetText(!endWordPos, 1) = "`" do
            word <- word + "`"
            incr endWordPos

        if word.EndsWith("``") then
            let startWordPos = ref currentWord.Start.Position
            let mutable newWord = currentWord.Snapshot.GetText(!startWordPos, !endWordPos - !startWordPos)
            decr startWordPos
            
            while not (newWord.StartsWith("``")) || !startWordPos <= 0 || currentWord.Snapshot.GetText(!startWordPos, 1) = "\n" do
                newWord <- currentWord.Snapshot.GetText(!startWordPos, !endWordPos - !startWordPos)
                decr startWordPos
            word <- newWord
            currentWord <- SnapshotSpan(currentWord.Snapshot, !startWordPos + 1, word.Length)

        (currentWord, word)

    /// Return corresponding zero-based range
    member this.GetRange() =
        let (_, w2) = this.GetWordIncludingQuotes()
        let extraLength = w2.Length - this.Length
        let lineStart = this.Snapshot.GetLineNumberFromPosition(this.Start.Position)
        let lineEnd = this.Snapshot.GetLineNumberFromPosition(this.End.Position)
        let startLine = this.Snapshot.GetLineFromPosition(this.Start.Position)
        let endLine = this.Snapshot.GetLineFromPosition(this.End.Position)
        let colStart = this.Start.Position - startLine.Start.Position
        let colEnd = this.End.Position - endLine.Start.Position
        (lineStart, colStart, lineEnd, colEnd + extraLength - 1)

    member this.FindNewSpans() =
        let txt = this.Snapshot.GetText()
        let (span, content) = this.GetWordIncludingQuotes()
        try
            let matches = Regex.Matches(txt, content)
            let spans = 
                matches
                |> Seq.cast<Match>
                |> Seq.map (fun m -> SnapshotSpan(this.Snapshot, m.Index, m.Length))
            (span, spans)
        with _ ->
            (span, Seq.empty)

type ITextStructureNavigator with
    member this.FindAllWords(currentRequest : SnapshotPoint, operatorBounds: (int * int) option) =
        let mutable word = 
            match operatorBounds with
            | Some (left, right) -> TextExtent(SnapshotSpan(currentRequest.Snapshot, left, right - left), true)
            | None -> this.GetExtentOfWord(currentRequest)

        let mutable foundWord = true

        // If we've selected something not worth highlighting, we might have
        // missed a "word" by a little bit
        if not <| currentRequest.WordExtentIsValid(word) then
            // Before we retry, make sure it is worthwhile
            if word.Span.Start <> currentRequest ||
               currentRequest = currentRequest.GetContainingLine().Start ||
               Char.IsWhiteSpace((currentRequest - 1).GetChar()) then
                foundWord <- false
            else
                // Try again, one character previous.  If the caret is at the end of a word, then
                // this will pick up the word we are at the end of.
                word <- this.GetExtentOfWord(currentRequest - 1)
                // If we still aren't valid the second time around, we're done
                if not <| currentRequest.WordExtentIsValid(word) then
                    foundWord <- false
        if foundWord then Some word.Span else None