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


type Classifier(tagger:ITagger<IClassificationTag> ) as self =
    
    let _subscriptions = List<IDisposable>()

    let _tagger = tagger

    let _classificationChanged = Event<EventHandler<ClassificationChangedEventArgs>,ClassificationChangedEventArgs>() 
    
    do _tagger.TagsChanged.Subscribe(self.OnTagsChanged) |> _subscriptions.Add
     
    member this.ClassificationChanged = _classificationChanged.Publish
            
    member __.OnTagsChanged( args ) =
        _classificationChanged.Trigger(self, new ClassificationChangedEventArgs( args.Span))

    member __.Dispose() =
        _subscriptions.ForEach ( fun x -> x.Dispose() ) 
        _subscriptions.Clear()
        

        if _tagger <> null then (_tagger :?> IDisposable).Dispose()

    interface IClassifier with
        [<CLIEvent>]
        member x.ClassificationChanged: IEvent<EventHandler<ClassificationChangedEventArgs>,ClassificationChangedEventArgs> = 
            self.ClassificationChanged
        
        member x.GetClassificationSpans(span: SnapshotSpan): IList<ClassificationSpan> = 
            _tagger.GetTags(
                NormalizedSnapshotSpanCollection span).Select( 
                    fun x -> ClassificationSpan(x.Span, x.Tag.ClassificationType)).ToList() :> IList<_>

    interface IDisposable with 
        member __.Dispose() =
            self.Dispose();



/// <summary>
/// Counts Values for Counted Classifier
/// </summary>
type CountedValue<'T> ( propertyCollection:PropertyCollection, key:obj, value:'T ) =
    let _value = value
    let _key = key
    let _propertyCollection = propertyCollection
    let mutable _count = 1

    member __.Value with get() = _value

    member __.Count with get() = _count and set v = _count <- v

    member __.Release() =
        _count <- _count - 1
    //    if _count = 0 then
      //      (_value :?> IDisposable).Dispose()
        _propertyCollection.RemoveProperty(_key) |> ignore


    static member GetOrCreate( propertyCollection:PropertyCollection , key:obj, createFunc:unit-> 'T) =
        let countedValueRef = ref Unchecked.defaultof<CountedValue<'T>>

        if propertyCollection.TryGetPropertySafe(key, countedValueRef) then
            let countedValue = !countedValueRef
            countedValue.Count <- countedValue.Count + 1
            countedValue
        else
            let countedValue = new CountedValue<'T>( propertyCollection, key, createFunc())
            propertyCollection.[key] <- countedValue
            countedValue
            


/// <summary>
/// This solves the same problem as CountedTagger but for IClassifier
/// </summary>
type CountedClassifier( propertyCollection:PropertyCollection , key:obj,createFunc:unit-> IClassifier) as self =
//    private readonly CountedValue<IClassifier> _countedValue;
    let _countedValue = CountedValue.GetOrCreate( propertyCollection, key, createFunc)

    member __.Classifier with get() = _countedValue.Value

    member __.Dispose() =   _countedValue.Release()

    interface  IClassifier with
        [<CLIEvent>]
        member x.ClassificationChanged: IEvent<EventHandler<ClassificationChangedEventArgs>,ClassificationChangedEventArgs> = 
            self.Classifier.ClassificationChanged
        
        member x.GetClassificationSpans(span: SnapshotSpan): IList<ClassificationSpan> = 
            self.Classifier.GetClassificationSpans(span)
         

    interface IDisposable with
        member __.Dispose() = self.Dispose()



        

//    IList<ClassificationSpan> IClassifier.GetClassificationSpans(SnapshotSpan span)
//    {
//        return Classifier.GetClassificationSpans(span);
//    }

