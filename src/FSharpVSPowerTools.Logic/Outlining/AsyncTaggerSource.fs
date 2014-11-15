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

/// <summary>
/// A tagger source for asynchronous taggers.  This interface is consumed from multiple threads
/// and each method which is called on the background thread is labelled as such
/// be called on any thread
/// </summary>
type IAsyncTaggerSource<'Data,'Tag when 'Tag :> ITag> =
    
    /// <summary>
    /// Delay in milliseconds which should occur between the call to GetTags and the kicking off
    /// of a background task
    /// </summary>
    abstract Delay              : int option        with get

    /// <summary>
    /// The current Snapshot.  
    /// Called from the main thread only
    /// </summary>
    abstract TextSnapshot       : ITextSnapshot     with get

    /// <summary>
    /// The current ITextView if this tagger is attached to a ITextView.  This is an optional
    /// value
    ///
    /// Called from the main thread only
    /// </summary>
    abstract TextViewOptional   : ITextView option
    /// <summary>
    /// This method is called to gather data on the UI thread which will then be passed
    /// down to the background thread for processing
    ///
    /// Called from the main thread only
    /// </summary>
    abstract GetDataForSnapshot : snapshot:ITextSnapshot -> 'Data


    [<UsedInBackgroundThread>]
    /// <summary>
    /// Return the applicable tags for the given SnapshotSpan instance.  This will be
    /// called on a background thread and should respect the provided CancellationToken
    ///
    /// Called from the background thread only
    /// </summary>
    abstract GetTagsInBackground: data:'Data -> span:SnapshotSpan -> cancellationToken:CancellationToken -> IReadOnlyCollection<ITagSpan<'Tag>>

    /// <summary>
    /// To prevent needless spawning of Task<T> values the async tagger has the option
    /// of providing prompt data.  This method should only be used when determination
    /// of the tokens requires no calculation.
    ///
    /// Called from the main thread only
    /// <summary>
    abstract TryGetTagsPrompt   : span:SnapshotSpan -> seq<ITagSpan<'Tag>> ref -> bool * seq<ITagSpan<'Tag>> ref
    
    [<CLIEvent>]
    /// <summary>
    /// Raised by the source when the underlying source has changed.  All previously
    /// provided data should be considered incorrect after this event
    /// </summary>
    abstract Changed            : IEvent<EventArgs>


[<AbstractClass>]
type AsyncTaggerSource<'Data, 'Tag when 'Tag :> ITag>
            (textBuffer:ITextBuffer, ?textView:ITextView)  =
    
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
     
        

