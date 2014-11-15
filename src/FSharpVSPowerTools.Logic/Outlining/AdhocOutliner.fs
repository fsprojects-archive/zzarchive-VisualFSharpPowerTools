namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools
open System.Data
open Extensions

[<Struct>][<CustomEquality>][<NoComparison>]
type OutliningRegion =
    val Tag     : OutliningRegionTag
    val Span    : SnapshotSpan
    val Cookie  : int

    new ( tag, span, cookie ) =
        {   Tag     = tag
            Span    = span
            Cookie  = cookie    }

    override x.Equals other =
        match other with
        | :? OutliningRegion as o -> 
            o.Tag.GetHashCode() = x.Tag.GetHashCode()   &&
            o.Span              = x.Span                &&
            o.Cookie            = x.Cookie

        | _ -> false

    override x.GetHashCode() = 
       (pown (hash x.Tag) x.Cookie ) + (pown (hash x.Span) x.Cookie)


[<Struct>][<NoComparison>]
type OutliningData =
    val TrackingSpan: ITrackingSpan
    val Tag         : OutliningRegionTag
    val Cookie      : int

    new ( trackingSpan, tag, cookie ) =
        {   TrackingSpan = trackingSpan
            Tag          = tag
            Cookie       = cookie   }
                             


type IAdhocOutliner=
    abstract TextBuffer             : ITextBuffer with get
    abstract GetOutliningRegions    : SnapshotSpan -> IReadOnlyCollection<OutliningRegion>
    abstract CreateOutliningRegion  : span:SnapshotSpan -> spanTrackingMode:SpanTrackingMode 
                                        -> text:string -> hint:string -> OutliningRegion
    abstract DeleteOutliningRegion  : cookie:int -> bool
    [<CLIEvent>]
    abstract Changed                : IEvent<EventArgs>



type AdhocOutliner (textBuffer:ITextBuffer) as self =
    static let emptyCollection   = ReadOnlyCollection<OutliningRegion>(List<OutliningRegion>())
    static let outlinerKey       = obj()
    static let outlinerTaggerKey = obj()

    let map     = Dictionary<int,OutliningData>()
    let mutable counter = 0
    let changed = Event<_>()

    static member OutlinerKey       = outlinerKey
    static member OutlinerTaggerKey = outlinerTaggerKey

    /// <summary>
    /// The outlining implementation is worthless unless it is also registered as an ITagger 
    /// component.  If this hasn't happened by the time the APIs are being queried then it is
    /// a bug and we need to notify the developer
    /// </summary>
    member __.EnsureTagger() =
        if textBuffer.Properties.ContainsProperty(outlinerTaggerKey) then
            let msg = "In order to use IAdhocOutliner you must also export an ITagger implementation for the buffer which return CreateOutliningTagger"
            raise (Exception msg)

    // TODO Unsure if this will raise
    member __.RaiseChanged() =
        changed.Trigger(EventArgs.Empty)
    

    /// <summary>
    /// Get all of the values which map to the given ITextSnapshot
    /// </summary>
    member __.GetOutliningRegions (span:SnapshotSpan) =
        if map.Count = 0 then emptyCollection else

        let snapshot = span.Snapshot
        let list = List<OutliningRegion>()
        map.Values |> Seq.iter ( fun cur -> 
            let currentSpan = cur.TrackingSpan.GetSpanSafe(snapshot)
            if currentSpan <> None && currentSpan.Value.IntersectsWith span then
                list.Add( OutliningRegion(cur.Tag, currentSpan.Value, cur.Cookie)) )
               
        list.ToReadOnlyCollectionShallow()


    member __.CreateOutlingRegion   (span:SnapshotSpan) (spanTrackingMode:SpanTrackingMode) 
                                    (text:string)       (hint:string) =
        let snapshot = span.Snapshot
        let trackingSpan = snapshot.CreateTrackingSpan(span.Span, spanTrackingMode)
        let tag     = OutliningRegionTag(text, hint)
        let data    = OutliningData(trackingSpan, tag, counter)
        map.Add(counter, data)
        counter <- counter+1
        self.RaiseChanged()
        OutliningRegion(tag, span, data.Cookie)
        


    interface IAdhocOutliner with
        
        [<CLIEvent>]
        member __.Changed: IEvent<EventArgs> = 
            changed.Publish
        

        member __.CreateOutliningRegion(span: SnapshotSpan) (spanTrackingMode: SpanTrackingMode) (text: string) (hint: string): OutliningRegion = 
            self.EnsureTagger()
            self.CreateOutlingRegion span spanTrackingMode text hint 
        

        member x.DeleteOutliningRegion(cookie: int): bool = 
            self.EnsureTagger()
            let success = map.Remove cookie
            if success = true then self.RaiseChanged()
            success
        

        member __.GetOutliningRegions(span: SnapshotSpan): IReadOnlyCollection<OutliningRegion> = 
            self.EnsureTagger()
            self.GetOutliningRegions(span) :> IReadOnlyCollection<OutliningRegion>
        
        member x.TextBuffer: ITextBuffer = 
            textBuffer
         