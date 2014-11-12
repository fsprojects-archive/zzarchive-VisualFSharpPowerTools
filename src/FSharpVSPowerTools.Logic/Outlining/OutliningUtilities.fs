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




type IBasicTaggerSource<'Tag when 'Tag :> ITag> =
    abstract GetTags : span:SnapshotSpan -> ReadOnlyCollection<ITagSpan<'Tag>>


[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Interface)>]
type UsedInBackgroundThreadAttribute () = 
    inherit Attribute()

    
[<UsedInBackgroundThread>]
type ReadOnlyStack<'T>(lineRange:'T, next: ReadOnlyStack<'T> option) =
     
    new() = ReadOnlyStack<'T>(Unchecked.defaultof<'T>, None)

    static member Empty = ReadOnlyStack<'T>()

  //  member x.Next  = next

    member x.Value with get() = x.ThrowIfEmpty(); lineRange

    member x.Count with get() = if next <> None then next.Value.Count + 1 else 0

    member x.IsEmpty with get() = next = None
    
    member x.ThrowIfEmpty() =
        if x.IsEmpty then raise (Exception(sprintf "ReadOnlyStack< %A > is empty" typeof<'T> ))
    
    member x.Pop() = x.ThrowIfEmpty(); next.Value

    member x.Push (lineRange:'T) =  ReadOnlyStack<'T>(lineRange, Some x)

    member x.GetEnumerator<'T>() =
        // TODO - This probably doesn't work, remember to check here for issues 
        let rec gen (top:ReadOnlyStack<'T>) = 
            seq{   if top.IsEmpty = true then () else
                   yield top.Value 
                   yield! gen (top.Pop())
                }
        (gen x).GetEnumerator() 


    interface IEnumerable<'T> with

        member x.GetEnumerator(): IEnumerator<'T> = 
            x.GetEnumerator()    

        member x.GetEnumerator(): Collections.IEnumerator = 
            x.GetEnumerator()  :> Collections.IEnumerator

module Constants =
    [<Literal>]
    let DefaultAsyncDelay = 100       