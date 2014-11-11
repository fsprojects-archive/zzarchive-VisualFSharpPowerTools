namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools
open System.Threading

type IAsyncTaggerSource<'Data,'Tag when 'Tag :> ITag> =
    abstract Delay              : int option        with get
    abstract TextSnapshot       : ITextSnapshot     with get
    abstract TextViewOptional   : ITextView
    abstract GetDataForSnapshot : snapshot:ITextSnapshot -> 'Data
    abstract GetTagsInBackground: data:'Data -> span:SnapshotSpan -> cancellationToken:CancellationToken -> IReadOnlyCollection<ITagSpan<'Tag>>
    abstract TryGetTagsPrompt   : span:SnapshotSpan -> seq<ITagSpan<'Tag>>
    [<CLIEvent>]
    abstract Changed            : IEvent<'Data>


[<Struct; NoComparison>]
type OutliningRegion =
    val Tag     : OutliningRegionTag
    val Span    : SnapshotSpan
    val Cookie  : int

    new ( tag, span, cookie ) =
        {   Tag     = tag
            Span    = span
            Cookie  = cookie    }


type IAdhocOutliner=
    abstract TextBuffer             : ITextBuffer with get
    abstract GetOutliningRegions    : SnapshotSpan -> IReadOnlyCollection<OutliningRegion>
    abstract CreateOutliningRegion  : span:SnapshotSpan -> spanTrackingMode:SpanTrackingMode 
                                        -> text:string -> hint:string -> OutliningRegion
    abstract DeleteOutliningRegion  : cookie:int -> bool
    [<CLIEvent>]
    abstract Changed            : IEvent<unit>