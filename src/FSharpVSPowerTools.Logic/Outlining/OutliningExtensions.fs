namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Projection
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools
//
//type ContractException =
//    inherit Exception
//
// 


module Extensions =

    type Span with

        static member CreateOverarching (left:Span) (right:Span) =
            let start   = Math.Min( left.Start, right.Start )
            let finish  = Math.Max( left.End  , right.End   )
            Span.FromBounds ( start, finish )



    type  ITextSnapshotLine with

        static member  GetStartLine (span:SnapshotSpan) =
            span.Start.GetContainingLine()

        static member GetLastLine (span:SnapshotSpan) =
            if  span.Length > 0 
            then span.End.Subtract(1).GetContainingLine()
            else ITextSnapshotLine.GetStartLine(span)


    type SnapshotSpan with 
        
        member x.GetStartLine() =  x.Start.GetContainingLine()
        member x.GetLastLine()  =  x.End.GetContainingLine()

        
        static member CreateOverarching (left:SnapshotSpan) (right:SnapshotSpan) =
            //Contract.
            ()

