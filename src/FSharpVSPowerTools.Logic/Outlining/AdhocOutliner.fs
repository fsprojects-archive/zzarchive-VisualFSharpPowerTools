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

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Interface)>]
    type UsedInBackgroundThreadAttribute () = 
        inherit Attribute()

    


[<Struct; CustomEquality; NoComparison>]
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
                             


type IAdhocOutliner=
    abstract TextBuffer             : ITextBuffer with get
    abstract GetOutliningRegions    : SnapshotSpan -> IReadOnlyCollection<OutliningRegion>
    abstract CreateOutliningRegion  : span:SnapshotSpan -> spanTrackingMode:SpanTrackingMode 
                                        -> text:string -> hint:string -> OutliningRegion
    abstract DeleteOutliningRegion  : cookie:int -> bool
    [<CLIEvent>]
    abstract Changed            : IEvent<unit>