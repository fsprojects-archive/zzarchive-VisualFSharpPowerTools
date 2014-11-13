namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
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



//        static member CreateOverarching (left:SnapshotSpan) (right:SnapshotSpan) =
//            //Contract.
//  
//    type ReadOnlyCollection<'T> with

    type NormalizedSnapshotSpanCollection with
        
        member self.GetOverarchingSpan() =
            let start  = self.[0]
            let finish = self.[self.Count - 1]
            SnapshotSpan(start.Start, finish.End)

 
    type ITextView with

        member self.GetVisibleSnapshotLineRange() =
            if self.InLayout = true then None else
            let lines       = self.TextViewLines
            let startLine   = lines.FirstVisibleLine.Start.GetContainingLine().LineNumber
            let lastLine    = lines.LastVisibleLine.End.GetContainingLine().LineNumber
            SnapshotLineRange.CreateForLineNumberRange self.TextSnapshot startLine lastLine



    type ITrackingSpan with
        // TODO in editorUtils this is nullable, so this might not work        
        member x.GetSpanSafe (snapshot:ITextSnapshot) =
            try x.GetSpan(snapshot) |> Some
            with
            | :? ArgumentException -> None


    type  ITextSnapshotLine with

        static member  GetStartLine (span:SnapshotSpan) =
            span.Start.GetContainingLine()

        static member GetLastLine (span:SnapshotSpan) =
            if  span.Length > 0 
            then span.End.Subtract(1).GetContainingLine()
            else ITextSnapshotLine.GetStartLine(span)


    type ITextSnapshot with

        member self.GetExtent () =
            SnapshotSpan( self, 0, self.Length )



    type IEnumerable<'T> with
        
        member x.ToReadOnlyCollection<'T>() =
            ReadOnlyCollection<'T>( x.ToList() )
        
        static member distinct<'source>  arg (ienum:IEnumerable<'source>) = ienum.Distinct  arg 

        static member filter<'source>    (func:'source -> bool) 
                                        (ienum:IEnumerable<'source>) = 
            ienum.Where  func


        static member concat<'source>  arg  (ienum:IEnumerable<'source>) = ienum.Concat<'source>    arg 

        static member map<'source,'result>   ( func:'source->'result)  
                                                (ienum:IEnumerable<'source>)= 
            ienum.Select    func
     
        static member fold<'state,'elem>    ( folder:'state->'elem->'state)
                                            ( state :'state )  
                                            ( ienum:IEnumerable<'elem>    ) = 
            ienum.Aggregate( state, Func<'state,'elem,'state>(folder))
     

        static member toHashSet<'source>() (ienum:IEnumerable<'source>) =
            HashSet<'source> ienum

        static member toReadOnlyCollection<'source> (ienum:IEnumerable<'source>) =
            ReadOnlyCollection<'source>( ienum.ToList() )



    type List<'T> with
        member x.ToReadOnlyCollectionShallow<'T>() =
            ReadOnlyCollection<'T>(x)
