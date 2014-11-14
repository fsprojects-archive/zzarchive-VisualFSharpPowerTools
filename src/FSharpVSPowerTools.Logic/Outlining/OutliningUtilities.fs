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
open FSharpVSPowerTools.Outlining.Extensions



type IBasicTaggerSource<'Tag when 'Tag :> ITag> =
    abstract GetTags : span:SnapshotSpan -> ReadOnlyCollection<ITagSpan<'Tag>>


[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Interface)>]
type UsedInBackgroundThreadAttribute () = 
    inherit Attribute()

    
[<UsedInBackgroundThread>]
type ReadOnlyStack<'T>(lineRange:'T, next: ReadOnlyStack<'T> option) =
     
    new() = ReadOnlyStack<'T>(Unchecked.defaultof<'T>, None)

    static member Empty = ReadOnlyStack<'T>()

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



 type EqualityUtility<'T> ( equalsFunc      : 'T -> 'T -> bool  ,
                            getHashCodeFunc : 'T -> int         ) as self =

    member __.Equals ( x, y )   = equalsFunc x y
    member __.GetHashCode x     = getHashCodeFunc x

    static member Create<'T> ( equalsFunc      : 'T -> 'T -> bool )
                             ( getHashCodeFunc : 'T -> int        ) =
        EqualityUtility<'T>  ( equalsFunc, getHashCodeFunc )

    interface IEqualityComparer<'T> with

        member __.Equals(x:'T, y:'T): bool  = self.Equals(x, y)
        member __.GetHashCode(x: 'T): int   = self.GetHashCode x     


type TaggerUtil =
        /// <summary>
        /// <para> The simple taggers when changed need to provide an initial SnapshotSpan           </para>
        /// <para> for the TagsChanged event.  It's important that this SnapshotSpan be kept as      </para>
        /// <para> small as possible.  If it's incorrectly large it can have a negative performance  </para>
        /// <para> impact on the editor.  In particular                                              </para>
        /// <para>&#160;</para>
        /// <para> 1. The value is directly provided in ITagAggregator::TagsChanged.  This value     </para>
        /// <para>    is acted on directly by many editor components.  Providing a large range       </para>
        /// <para>    unnecessarily increases their work load.                                       </para>
        /// <para>&#160;</para>
        /// <para> 2. It can cause a ripple effect in Visual Studio 2010 RTM.  The SnapshotSpan      </para>
        /// <para>    returned will be immediately be the vale passed to GetTags for every other     </para>
        /// <para>    ITagger in the system (TextMarkerVisualManager issue).                         </para>
        /// <para>&#160;</para>
        /// <para> In order to provide the minimum possible valid SnapshotSpan the simple taggers    </para>
        /// <para> cache the overarching SnapshotSpan for the latest ITextSnapshot of all requests   </para>
        /// <para> to which they are given.                                                          </para>
        /// </summary>
        static member AdjustRequestedSpan (cachedRequestSpan:SnapshotSpan option)( requestSpan:SnapshotSpan) =
            if cachedRequestSpan.IsNone then requestSpan else

            let cachedSnapshot  = cachedRequestSpan.Value.Snapshot
            let requestSnapshot = requestSpan.Snapshot

            if cachedSnapshot = requestSnapshot then
                // Same snapshot so we just need the overarching SnapshotSpan
                SnapshotSpan.CreateOverarching (cachedRequestSpan.Value) (requestSpan)

            else
                if cachedSnapshot.Version.VersionNumber < requestSnapshot.Version.VersionNumber then
                    // Request for a span on a new ITextSnapshot.  Translate the old SnapshotSpan
                    // to the new ITextSnapshot and get the overarching value 
                    let trackingSpan = cachedSnapshot.CreateTrackingSpan(cachedRequestSpan.Value.Span, SpanTrackingMode.EdgeInclusive);
                    let traslatedSpan = trackingSpan.GetSpanSafe(requestSnapshot)

                    if traslatedSpan.IsSome then
                        SnapshotSpan.CreateOverarching (traslatedSpan.Value) (requestSpan)
                    else
                        // If we can't translate the previous SnapshotSpan forward then simply use the 
                        // entire ITextSnapshot.  This is a correct value, it just has the potential for
                        // some inefficiencies
                        requestSnapshot.GetExtent()

                // It's a request for a value in the past.  This is a very rare scenario that is almost
                // always followed by a request for a value on the current snapshot.  Just return the 
                // entire ITextSnapshot.  This is a correct value, it just has the potential for
                // some inefficiencies 
                else
                    requestSpan.Snapshot.GetExtent();


module Constants =
    let private x = 22
    let inline xd s = 33 + s
    [<Literal>]
    let DefaultAsyncDelay = 100       