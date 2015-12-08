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
[<TextViewRole(PredefinedTextViewRoles.PrimaryDocument)>]
type ImplementInterfaceSuggestedActionsSourceProvider() =
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
                   || not generalOptions.ResolveUnopenedNamespacesEnabled then null
                else 
                    match x.TextDocumentFactoryService.TryGetTextDocument(buffer) with
                    | true, doc -> 
                        let implementInterface = 
                            new ImplementInterface(
                                  doc, textView,
                                  x.EditorOptionsFactory, x.UndoHistoryRegistry.RegisterHistory buffer,
                                  x.FSharpVsLanguageService, x.ServiceProvider, x.ProjectFactory,
                                  Setting.getInterfaceMemberIdentifier codeGenOptions,
                                  Setting.getDefaultMemberBody codeGenOptions)
                    
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