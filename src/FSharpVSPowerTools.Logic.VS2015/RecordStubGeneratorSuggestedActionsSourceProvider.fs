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
[<Name "Record Stub Generator Suggested Actions">]
[<ContentType "F#">]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type RecordStubGeneratorSuggestedActionsSourceProvider() =
    [<Import; DefaultValue>]
    val mutable FSharpVsLanguageService: VSLanguageService

    [<Import; DefaultValue>]
    val mutable TextDocumentFactoryService: ITextDocumentFactoryService

    [<Import(typeof<SVsServiceProvider>); DefaultValue>]
    val mutable ServiceProvider: IServiceProvider

    [<Import; DefaultValue>]
    val mutable UndoHistoryRegistry: ITextUndoHistoryRegistry

    [<Import; DefaultValue>]
    val mutable ProjectFactory: ProjectFactory

    [<Import; DefaultValue>]
    val mutable EditorOptionsFactory: IEditorOptionsFactoryService

    interface ISuggestedActionsSourceProvider with
        member x.CreateSuggestedActionsSource(textView: ITextView, buffer: ITextBuffer): ISuggestedActionsSource = 
            if textView.TextBuffer <> buffer then null
            else
                let generalOptions = Setting.getGeneralOptions x.ServiceProvider
                let codeGenOptions = Setting.getCodeGenerationOptions x.ServiceProvider
                if generalOptions == null 
                   || codeGenOptions == null
                   || not generalOptions.GenerateRecordStubEnabled then null
                else 
                    match x.TextDocumentFactoryService.TryGetTextDocument(buffer) with
                    | true, doc -> 
                        let generator = 
                            new RecordStubGenerator(
                                  doc, textView,
                                  x.UndoHistoryRegistry.RegisterHistory(buffer),
                                  x.FSharpVsLanguageService, x.ServiceProvider,
                                  x.ProjectFactory, Setting.getDefaultMemberBody codeGenOptions)
                    
                        new RecordStubGeneratorSuggestedActionsSource(generator) :> _
                    | _ -> null

and RecordStubGeneratorSuggestedActionsSource (generator: RecordStubGenerator) as self =
    let actionsChanged = Event<_,_>()
    do generator.Changed.Add (fun _ -> actionsChanged.Trigger (self, EventArgs.Empty))
    interface ISuggestedActionsSource with
        member __.Dispose() = (generator :> IDisposable).Dispose()
        member __.GetSuggestedActions (_requestedActionCategories, _range, _ct) = 
            match generator.CurrentWord, generator.Suggestions with
            | None, _
            | _, [] ->
                Seq.empty
            | Some _, suggestions ->
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

        member __.HasSuggestedActionsAsync (_requestedCategories, _range, _ct) = 
            Task.FromResult(
                Option.isSome generator.CurrentWord && 
                generator.Suggestions 
                |> List.isEmpty 
                |> not)

        [<CLIEvent>]
        member __.SuggestedActionsChanged: IEvent<EventHandler<EventArgs>, EventArgs> = actionsChanged.Publish
        member __.TryGetTelemetryId telemetryId = telemetryId <- Guid.Empty; false