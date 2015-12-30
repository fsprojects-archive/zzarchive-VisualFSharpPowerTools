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
[<Name "Implement Interface Suggested Actions">]
[<ContentType "F#">]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type ImplementInterfaceSuggestedActionsSourceProvider [<ImportingConstructor>]
   (fsharpVsLanguageService: VSLanguageService,
    textDocumentFactoryService: ITextDocumentFactoryService,
    [<Import(typeof<SVsServiceProvider>)>]
    serviceProvider: IServiceProvider,
    undoHistoryRegistry: ITextUndoHistoryRegistry,
    projectFactory: ProjectFactory,
    editorOptionsFactory: IEditorOptionsFactoryService) =

    interface ISuggestedActionsSourceProvider with
        member __.CreateSuggestedActionsSource(textView: ITextView, buffer: ITextBuffer): ISuggestedActionsSource =
            if textView.TextBuffer <> buffer then null
            elif not SettingsContext.GeneralOptions.ResolveUnopenedNamespacesEnabled then null else
            match textDocumentFactoryService.TryGetTextDocument(buffer) with
            | true, doc ->
                let implementInterface =
                    new ImplementInterface(
                            doc, textView,
                            editorOptionsFactory, undoHistoryRegistry.RegisterHistory buffer,
                            fsharpVsLanguageService, serviceProvider, projectFactory,
                            SettingsContext.getInterfaceMemberIdentifier(),
                            SettingsContext.getDefaultMemberBody())

                new ImplementInterfaceSuggestedActionsSource(implementInterface) :> _
            | _ -> null

and ImplementInterfaceSuggestedActionsSource (implementInterface: ImplementInterface) as self =
    let actionsChanged = Event<_,_>()
    do implementInterface.Changed.Add (fun _ -> actionsChanged.Trigger (self, EventArgs.Empty))
    interface ISuggestedActionsSource with
        member __.Dispose() = (implementInterface :> IDisposable).Dispose()
        member __.GetSuggestedActions (_requestedActionCategories, _range, _ct) =
            match implementInterface.CurrentWord, implementInterface.Suggestions with
            | Some _, (_ :: _ as suggestions)  ->
                suggestions
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
                |> fun xs -> [ SuggestedActionSet xs ] :> _
            | _ -> Seq.empty

        member __.HasSuggestedActionsAsync (_requestedCategories, _range, _ct) =
            Task.FromResult(
                Option.isSome implementInterface.CurrentWord &&
                not (List.isEmpty implementInterface.Suggestions))

        [<CLIEvent>]
        member __.SuggestedActionsChanged: IEvent<EventHandler<EventArgs>, EventArgs> = actionsChanged.Publish
        member __.TryGetTelemetryId telemetryId = telemetryId <- Guid.Empty; false
