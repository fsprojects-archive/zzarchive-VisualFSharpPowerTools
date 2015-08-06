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
open FSharpVSPowerTools.Refactoring
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools

[<Export(typeof<ISuggestedActionsSourceProvider>)>]
[<Name "Resolve Unopened Namespaces Suggested Actions">]
[<ContentType "F#">]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type ResolveUnopenedNamespaceSuggestedActionsSourceProvider() =
    [<Import; DefaultValue>]
    val mutable fsharpVsLanguageService: VSLanguageService

    [<Import; DefaultValue>]
    val mutable textDocumentFactoryService: ITextDocumentFactoryService

    [<Import; DefaultValue>]
    val mutable serviceProvider: IServiceProvider

    [<Import; DefaultValue>]
    val mutable undoHistoryRegistry: ITextUndoHistoryRegistry

    [<Import; DefaultValue>]
    val mutable projectFactory: ProjectFactory

    interface ISuggestedActionsSourceProvider with
        member x.CreateSuggestedActionsSource(textView: ITextView, buffer: ITextBuffer): ISuggestedActionsSource = 
            if textView.TextBuffer <> buffer then null
            else 
                let generalOptions = Setting.getGeneralOptions x.serviceProvider
                if obj.ReferenceEquals (generalOptions, null) || not generalOptions.ResolveUnopenedNamespacesEnabled then null
                else
                    match x.textDocumentFactoryService.TryGetTextDocument(buffer) with
                    | true, doc -> 
                        let resolver = 
                            new UnopenedNamespaceResolver(doc, textView, x.undoHistoryRegistry.RegisterHistory(buffer),
                                                          x.fsharpVsLanguageService, x.serviceProvider, x.projectFactory)
                    
                        new ResolveUnopenedNamespaceSuggestedActionsSource(resolver) :> _
                    | _ -> null

and ResolveUnopenedNamespaceSuggestedActionsSource
    (
        resolver: UnopenedNamespaceResolver
    ) as self =
    let actionsChanged = Event<_,_>()
    do resolver.Updated.Add (fun _ -> actionsChanged.Trigger (self, EventArgs.Empty))
    interface ISuggestedActionsSource with
        member __.Dispose() = (resolver :> IDisposable).Dispose()
        member __.GetSuggestedActions (requestedActionCategories, range, cancellationToken) = 
            match resolver.CurrentWord, resolver.Suggestions with
            | Some word, Some suggestions ->
                suggestions
                |> List.map (fun s ->
                     { new ISuggestedAction with
                           member __.DisplayText = s.Text
                           member __.Dispose() = ()
                           member __.GetActionSetsAsync ct = Task.FromResult <| seq []
                           member __.GetPreviewAsync ct = Task.FromResult null
                           member __.HasActionSets = false
                           member __.HasPreview = false
                           member __.IconAutomationText = null
                           member __.IconMoniker = Unchecked.defaultof<_>
                           member __.InputGestureText = null
                           member __.Invoke ct = s.Invoke()
                           member __.TryGetTelemetryId telemetryId = false })
            | _ -> []
            |> fun xs -> [ SuggestedActionSet(xs) ] :> _

        member __.HasSuggestedActionsAsync (requestedCategories, range, ct) = 
            Task.FromResult (resolver.Suggestions |> Option.getOrElse [] |> (List.isEmpty >> not))
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