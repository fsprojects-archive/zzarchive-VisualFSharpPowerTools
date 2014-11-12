namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Collections.ObjectModel
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
open FSharpVSPowerTools.Outlining.Constants


type IAsyncTaggerSource<'Data,'Tag when 'Tag :> ITag> =
    abstract Delay              : int option        with get
    abstract TextSnapshot       : ITextSnapshot     with get
    abstract TextViewOptional   : ITextView option
    abstract GetDataForSnapshot : snapshot:ITextSnapshot -> 'Data
    [<UsedInBackgroundThread>]
    abstract GetTagsInBackground: data:'Data -> span:SnapshotSpan -> cancellationToken:CancellationToken -> IReadOnlyCollection<ITagSpan<'Tag>>
    abstract TryGetTagsPrompt   : span:SnapshotSpan -> seq<ITagSpan<'Tag>> ref -> bool * seq<ITagSpan<'Tag>> ref
    [<CLIEvent>]
    abstract Changed            : IEvent<EventArgs>


[<AbstractClass>]
type AsyncTaggerSource<'Data, 'Tag when 'Tag :> ITag>
            (textBuffer:ITextBuffer, ?textView:ITextView) as self =
    
    let changed = Event<EventArgs>()
    
    do 
        if textView.Value = null then
            failwithf "Cannot construct an AsyncTaggerSource from a null textView"
        if textBuffer = null then
            failwithf "Cannot construct an AsyncTaggerSource from a null textBuffer"
            

    member __.TextBuffer with get() = textBuffer
    member __.TextViewOptional with get() = textView

    abstract GetDataForSnapshot : snapshot:ITextSnapshot -> 'Data
    abstract GetTagsInBackground : data:'Data -> span:SnapshotSpan ->
                                        cancellationToken:CancellationToken ->
                                            ReadOnlyCollection<ITagSpan<'Tag>>

    abstract TryGetTagsPrompt : span:SnapshotSpan -> 
                                    tags:IEnumerable<ITagSpan<'Tag>> ref -> 
                                            bool *tags:IEnumerable<ITagSpan<'Tag>> ref
                                    
    default  __.TryGetTagsPrompt (_:SnapshotSpan) (tags:IEnumerable<ITagSpan<'Tag>> ref) =
        tags:=  null
        false, tags
    


    member x.RaiseChanged() =
        changed.Trigger(EventArgs.Empty)


    interface IAsyncTaggerSource<'Data,'Tag> with

        [<CLIEvent>]
        member x.Changed: IEvent<EventArgs> = 
           changed.Publish
    
        member x.Delay with get(): int option = Some Constants.DefaultAsyncDelay
    
        member x.GetDataForSnapshot(snapshot: ITextSnapshot): 'Data = 
            x.GetDataForSnapshot snapshot
    
        member x.GetTagsInBackground(data: 'Data) (span: SnapshotSpan) (cancellationToken: CancellationToken): IReadOnlyCollection<ITagSpan<'Tag>> = 
            x.GetTagsInBackground data span cancellationToken :> IReadOnlyCollection<ITagSpan<'Tag>> 
    
        member x.TextSnapshot: ITextSnapshot = 
            textBuffer.CurrentSnapshot
    
        member x.TextViewOptional: ITextView option = 
            textView
    
        member x.TryGetTagsPrompt(span: SnapshotSpan) (tags: seq<ITagSpan<'Tag>> ref) = 
            x.TryGetTagsPrompt span tags
     
        

