namespace FSharpVSPowerTools.Logic.VS2015

open System.ComponentModel.Composition
open Microsoft.VisualStudio.Language.Intellisense
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open FSharpVSPowerTools.ProjectSystem
open System.Threading.Tasks

//[<ExportWithMinimalVisualStudioVersion(typeof<ISuggestedActionsSourceProvider>, Version = VisualStudioVersion.VS2015)>]
[<Export(typeof<ISuggestedActionsSourceProvider>)>]
[<Name "Resolve Unopened Namespaces Suggested Actions">]
[<ContentType "F#">]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type ResolveUnopenedNamespaceSuggestedActionsSourceProvider() =
    [<Import(typeof<ITextStructureNavigatorSelectorService>); DefaultValue>]
    val mutable NavigatorService: ITextStructureNavigatorSelectorService

    interface ISuggestedActionsSourceProvider with
        member x.CreateSuggestedActionsSource(textView: ITextView, textBuffer: ITextBuffer): ISuggestedActionsSource = 
            if textBuffer = null && textView = null then null
            else new ResolveUnopenedNamespaceSuggestedActionsSource(x, textView, textBuffer) :> _

and ResolveUnopenedNamespaceSuggestedActionsSource
    (
        provider: ResolveUnopenedNamespaceSuggestedActionsSourceProvider,
        textView: ITextView, 
        textBuffer: ITextBuffer
    ) =
    let actionsChanged = Event<_,_>()
    interface ISuggestedActionsSource with
        member __.Dispose() = ()
        member __.GetSuggestedActions (requestedActionCategories, range, cancellationToken) = 
            let action = { new ISuggestedAction with
                               member __.DisplayText = "foo"
                               member __.Dispose() = ()
                               member __.GetActionSetsAsync ct = Task.FromResult <| seq []
                               member __.GetPreviewAsync ct = Task.FromResult null
                               member __.HasActionSets = false
                               member __.HasPreview = false
                               member __.IconAutomationText = "auto foo"
                               member __.IconMoniker = Unchecked.defaultof<_>
                               member __.InputGestureText = "gesture foo"
                               member __.Invoke ct = ()
                               member __.TryGetTelemetryId telemetryId = false }
            
            seq { yield SuggestedActionSet [action] }

        member __.HasSuggestedActionsAsync (requestedCategories, range, ct) = Task.FromResult true
//            TextExtent extent
//            if (TryGetWordUnderCaret(out extent) && extent.IsSignificant)
//    {
//        ITrackingSpan trackingSpan = range.Snapshot.CreateTrackingSpan(extent.Span, SpanTrackingMode.EdgeInclusive);
//        var upperAction = new UpperCaseSuggestedAction(trackingSpan);
//        var lowerAction = new LowerCaseSuggestedAction(trackingSpan);
//        return new SuggestedActionSet[] { new SuggestedActionSet(new ISuggestedAction[] { upperAction, lowerAction }) };
//    }
    //return Enumerable.Empty<SuggestedActionSet>();

        [<CLIEvent>]
        member __.SuggestedActionsChanged: IEvent<EventHandler<EventArgs>, EventArgs> = actionsChanged.Publish
        member __.TryGetTelemetryId telemetryId = telemetryId <- Guid.Empty; false