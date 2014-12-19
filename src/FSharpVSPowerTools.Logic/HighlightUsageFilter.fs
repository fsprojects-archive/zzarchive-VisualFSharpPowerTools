namespace FSharpVSPowerTools.HighlightUsage

open System.IO
open System.Windows
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.ViewModule.Progress
open FSharpVSPowerTools.AsyncMaybe
open Microsoft.VisualStudio.Text.Tagging

[<NoEquality; NoComparison>]
type private DocumentState =
    { Word: (SnapshotSpan * Symbol) option
      File: string
      Project: IProjectProvider }

[<RequireQualifiedAccess>]
type private NeighbourKind =
    | Previous
    | Next

type HighlightUsageFilter(textDocument: ITextDocument,
                          view: IWpfTextView, 
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: System.IServiceProvider,
                          projectFactory: ProjectFactory,
                          referenceTagger: ITagAggregator<TextMarkerTag>) =

    let gotoReference kind =
        let buffer = view.TextBuffer
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        let span =
            referenceTagger.GetTags(span)
            |> Seq.collect (fun mappedSpan -> mappedSpan.Span.GetSpans(buffer))
            |> Seq.sortBy (fun span -> span.Start.Position)
            |> match kind with 
               | NeighbourKind.Previous -> Seq.head
               | NeighbourKind.Next -> Seq.last
        view.Caret.MoveTo(span.Start) |> ignore

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            if (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidNextHighlightedReference) then
                gotoReference NeighbourKind.Next
            elif (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidPreviousHighlightedReference) then
                gotoReference NeighbourKind.Previous
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidNextHighlightedReference 
                                                  || x.cmdID = Constants.cmdidPreviousHighlightedReference) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            