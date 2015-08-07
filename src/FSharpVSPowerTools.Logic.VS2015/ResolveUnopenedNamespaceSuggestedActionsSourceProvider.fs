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
    val mutable FsharpVsLanguageService: VSLanguageService

    [<Import; DefaultValue>]
    val mutable TextDocumentFactoryService: ITextDocumentFactoryService

    [<Import(typeof<SVsServiceProvider>); DefaultValue>]
    val mutable ServiceProvider: IServiceProvider

    [<Import; DefaultValue>]
    val mutable UndoHistoryRegistry: ITextUndoHistoryRegistry

    [<Import; DefaultValue>]
    val mutable ProjectFactory: ProjectFactory

    interface ISuggestedActionsSourceProvider with
        member x.CreateSuggestedActionsSource(textView: ITextView, buffer: ITextBuffer): ISuggestedActionsSource = 
            if textView.TextBuffer <> buffer then null
            else
                let generalOptions = Setting.getGeneralOptions x.ServiceProvider
                if obj.ReferenceEquals (generalOptions, null) || not generalOptions.ResolveUnopenedNamespacesEnabled then null
                else
                    match x.TextDocumentFactoryService.TryGetTextDocument(buffer) with
                    | true, doc -> 
                        let resolver = 
                            new UnopenedNamespaceResolver(doc, textView, x.UndoHistoryRegistry.RegisterHistory(buffer),
                                                          x.FsharpVsLanguageService, x.ServiceProvider, x.ProjectFactory)
                    
                        new ResolveUnopenedNamespaceSuggestedActionsSource(resolver) :> _
                    | _ -> null

and ResolveUnopenedNamespaceSuggestedActionsSource (resolver: UnopenedNamespaceResolver) as self =
    let actionsChanged = Event<_,_>()
    do resolver.Updated.Add (fun _ -> actionsChanged.Trigger (self, EventArgs.Empty))
    interface ISuggestedActionsSource with
        member __.Dispose() = (resolver :> IDisposable).Dispose()
        member __.GetSuggestedActions (_requestedActionCategories, _range, _ct) = 
            match resolver.CurrentWord, resolver.Suggestions with
            | Some _, Some suggestions ->
                suggestions
                |> List.map (fun xs ->
                    xs
                    |> List.map (fun s ->
                         { new ISuggestedAction with
                               member __.DisplayText = s.Text
                               member __.Dispose() = ()
                               member __.GetActionSetsAsync _ct = Task.FromResult <| seq []
                               member __.GetPreviewAsync _ct = Task.FromResult null
                               member __.HasActionSets = false
                               member __.HasPreview = false
                               member __.IconAutomationText = null
                               member __.IconMoniker = Unchecked.defaultof<_>
                               member __.InputGestureText = null
                               member __.Invoke _ct = s.Invoke()
                               member __.TryGetTelemetryId _telemetryId = false })
                     |> fun xs -> SuggestedActionSet xs) :> _
            | _ -> Seq.empty

        member __.HasSuggestedActionsAsync (_requestedCategories, _range, _ct) = 
            Task.FromResult (resolver.Suggestions |> Option.getOrElse [] |> (List.isEmpty >> not))

        [<CLIEvent>]
        member __.SuggestedActionsChanged: IEvent<EventHandler<EventArgs>, EventArgs> = actionsChanged.Publish
        member __.TryGetTelemetryId telemetryId = telemetryId <- Guid.Empty; false