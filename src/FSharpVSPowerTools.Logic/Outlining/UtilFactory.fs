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
open System.Threading.Tasks
open System.Collections.ObjectModel
open FSharpVSPowerTools.Outlining.Extensions
open Microsoft.VisualStudio.Text.Classification





/// <summary>
/// It's possible, and very likely, for an ITagger<T> to be requested multiple times for the 
/// same scenario via the ITaggerProvider.  This happens when extensions spin up custom 
/// ITagAggregator instances or simple manually query for a new ITagger.  Having multiple taggers
/// for the same data is often very unnecessary.  Produces a lot of duplicate work.  For example
/// consider having multiple :hlsearch taggers for the same ITextView.  
///
/// CountedTagger helps solve this by using a ref counted solution over the raw ITagger.  It allows
/// for only one ITagger to be created for the same scenario
/// </summary>
type CountedTagger<'Tag when 'Tag :> ITag > 
                    (   propertyCollection  :   PropertyCollection      , 
                        key                 :   obj                     , 
                        createFunc          :   unit -> ITagger<'Tag>   ) as self = 

    let _countedValue = CountedValue<ITagger<'Tag>>.GetOrCreate(propertyCollection, key, createFunc)

    member __.Tagger with get() = _countedValue.Value

    member __.Dispose() = _countedValue.Release()

    interface ITagger<'Tag> with
        member x.GetTags(spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<'Tag>> = 
            self.Tagger.GetTags(spans)
            
        [<CLIEvent>]
        member x.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
            self.Tagger.TagsChanged
    
    interface IDisposable with
        member __.Dispose() = self.Dispose()
             




/// <summary>
/// Importable interface which produces ITagger implementations based on sources
/// </summary>
type UtilFactory =

    static member CreateTaggerRaw<'Data,'Tag when 'Tag :> ITag> (asyncTaggerSource:IAsyncTaggerSource<'Data,'Tag >) =
        new AsyncTagger<'Data,'Tag> (asyncTaggerSource)

    /// <summary>
    /// Create an ITagger implementation for the IAsyncTaggerSource.  This instance will be a counted 
    /// wrapper over the single IAsyncTaggerSource represented by the specified key
    /// </summary>
    static member CreateTagger<'Data, 'Tag when 'Tag :> ITag> 
            (   propertyCollection  :   PropertyCollection                      ,
                key                 :   obj                                     ,
                createFunc          :   unit -> IAsyncTaggerSource<'Data, 'Tag> ) : ITagger<'Tag> =
        let makeAsyncTagger = 
            ( fun()-> new AsyncTagger<'Data,'Tag>(createFunc()) :> ITagger<'Tag>)

        new CountedTagger<'Tag> (   propertyCollection  , 
                                    key                 , 
                                    makeAsyncTagger     ) :> ITagger<'Tag>



    static member CreateClassifierRaw<'Data>(asyncTaggerSource: IAsyncTaggerSource<'Data, IClassificationTag> ) =
        new Classifier ( UtilFactory.CreateTaggerRaw(asyncTaggerSource))


    static member CreateClassifier<'Data>
                    (   propertyCollection  : PropertyCollection                                    , 
                        key                 : obj                                                   , 
                        createFunc          : unit -> IAsyncTaggerSource<'Data, IClassificationTag> ) =
    
        let makeRawClassifier = 
            fun() -> UtilFactory.CreateClassifierRaw(createFunc())  :> IClassifier

        new CountedClassifier( propertyCollection, key, makeRawClassifier )


    static member GetOrCreateOutlinerCore(textBuffer:ITextBuffer ) =
        textBuffer.Properties.GetOrCreateSingletonProperty(AdhocOutliner.OutlinerKey, ( fun _ -> new AdhocOutliner(textBuffer)))

    /// <summary>
    /// Get or create the IAdhocOutliner instance for the given ITextBuffer.  This return will be useless 
    /// unless the code which calls this method exports an ITaggerProvider which proxies the return 
    /// of GetOrCreateOutlinerTagger
    /// </summary>
    static member GetOrCreateOutliner( textBuffer:ITextBuffer) =
        UtilFactory.GetOrCreateOutlinerCore(textBuffer)

    /// <summary>
    /// This is the ITagger implementation for IAdhocOutliner
    /// </summary>
    static member CreateOutlinerTagger( textBuffer:ITextBuffer ) : ITagger<OutliningRegionTag> =
        UtilFactory.CreateTagger    (textBuffer.Properties ,
                                    AdhocOutliner.OutlinerTaggerKey  ,
                                    (fun() -> UtilFactory.GetOrCreateOutlinerCore(textBuffer)))


    






        


