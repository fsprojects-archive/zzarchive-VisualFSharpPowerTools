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
             




/// <summary>
/// Importable interface which produces ITagger implementations based on sources
/// </summary>
type UtilFactory =

    static member CreateTaggerRaw<'Data,'Tag when 'Tag :> ITag> (asyncTaggerSource:IAsyncTaggerSource<'Data,'Tag >) =
        new AsyncTagger<'Data,'Tag> (asyncTaggerSource)

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
    /// Create an ITagger implementation for the IAsyncTaggerSource.  This instance will be a counted 
    /// wrapper over the single IAsyncTaggerSource represented by the specified key
    /// </summary>
    static member CreateTagger<'Data, 'Tag when 'Tag :> ITag> ( propertyCollection:PropertyCollection) (key:obj) (createFunc:unit -> IAsyncTaggerSource<'Data, 'Tag>) =
            return new CountedTagger<TTag>(
                propertyCollection,
                key,
//                () => new AsyncTagger<TData, TTag>(createFunc()));

    /// <summary>
    /// This is the ITagger implementation for IAdhocOutliner
    /// </summary>
    static member  CreateOutlinerTagger(ITextBuffer textBuffer) =
        eateTagger(
            textBuffer.Properties,
            AdhocOutliner.OutlinerTaggerKey,
            () => GetOrCreateOutlinerCore(textBuffer));



//
//        /// <summary>
//        /// Create an ITagger implementation for the IBasicTaggerSource.  This instance will be a counted
//        /// wrapper over the single IBasicTaggerSource represented by the specified key
//        /// </summary>
//        public static ITagger<TTag> CreateTagger<TTag>(PropertyCollection propertyCollection, object key, Func<IBasicTaggerSource<TTag>> createFunc)
//            where TTag : ITag
//        {
//            return new CountedTagger<TTag>(
//                propertyCollection,
//                key,
//                () => new BasicTagger<TTag>(createFunc()));
//        }
//
//        public static IClassifier CreateClassifierRaw(IBasicTaggerSource<IClassificationTag> basicTaggerSource)
//        {
//            return new Classifier(CreateTaggerRaw(basicTaggerSource));
//        }
//
//        public static IClassifier CreateClassifierRaw<TData>(IAsyncTaggerSource<TData, IClassificationTag> asyncTaggerSource)
//        {
//            return new Classifier(CreateTaggerRaw(asyncTaggerSource));
//        }
//
//        public static IClassifier CreateClassifier(PropertyCollection propertyCollection, object key, Func<IBasicTaggerSource<IClassificationTag>> createFunc)
//        {
//            return new CountedClassifier(
//                propertyCollection,
//                key,
//                () => CreateClassifierRaw(createFunc()));
//        }
//
//        public static IClassifier CreateClassifier<TData>(PropertyCollection propertyCollection, object key, Func<IAsyncTaggerSource<TData, IClassificationTag>> createFunc)
//        {
//            return new CountedClassifier(
//                propertyCollection,
//                key,
//                () => CreateClassifierRaw(createFunc()));
//        }
//
//        public static IBasicUndoHistoryRegistry CreateBasicUndoHistoryRegistry()
//        {
//            return new BasicTextUndoHistoryRegistry();
//        }
//
//        public static IProtectedOperations CreateProtectedOperations(IEnumerable<Lazy<IExtensionErrorHandler>> errorHandlers)
//        {
//            return new ProtectedOperations(errorHandlers);
//        }
//
//        public static IProtectedOperations CreateProtectedOperations(IEnumerable<IExtensionErrorHandler> errorHandlers)
//        {
//            var lazyList = errorHandlers.Select(x => new Lazy<IExtensionErrorHandler>(() => x)).ToList();
//            return new ProtectedOperations(lazyList);
//        }
//
//        /// <summary>
//        /// Get or create the IAdhocOutliner instance for the given ITextBuffer.  This return will be useless 
//        /// unless the code which calls this method exports an ITaggerProvider which proxies the return 
//        /// of GetOrCreateOutlinerTagger
//        /// </summary>
//        public static IAdhocOutliner GetOrCreateOutliner(ITextBuffer textBuffer)
//        {
//            return GetOrCreateOutlinerCore(textBuffer);
//        }
//
//        /// <summary>
//        /// This is the ITagger implementation for IAdhocOutliner
//        /// </summary>
//        public static ITagger<OutliningRegionTag> CreateOutlinerTagger(ITextBuffer textBuffer)
//        {
//            return CreateTagger(
//                textBuffer.Properties,
//                AdhocOutliner.OutlinerTaggerKey,
//                () => GetOrCreateOutlinerCore(textBuffer));
//        }

        


