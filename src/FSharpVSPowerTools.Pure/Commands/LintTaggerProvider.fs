module FSharpVSPowerTools.LintTaggerProvider

open FSharpVSPowerTools.Linting
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Adornments
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System
open System.ComponentModel.Composition
open System.Windows.Media


[<Export (typeof<EditorFormatDefinition>)>]
[<Name (Constants.LintTagErrorType)>]
[<Order (After = Priority.High)>]
[<UserVisible true>]

type internal LintFormatDefinition () =
    inherit EditorFormatDefinition ()
    do  base.ForegroundColor <- Nullable Colors.Orange
        base.BackgroundCustomizable <- Nullable false
        base.DisplayName <- "F# Lint"

[<Export(typeof<ErrorTypeDefinition>)>]
[<Name(Constants.LintTagErrorType)>]
[<DisplayName(Constants.LintTagErrorType)>]
let internal  LintErrorTypeDefinition : ErrorTypeDefinition = null
    

[<Export (typeof<IViewTaggerProvider>)>]
[<ContentType "F#">]
[<TagType (typeof<LintTag>)>]
type LintTaggerProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider                 : IServiceProvider              ,
        textDocumentFactoryService      :   ITextDocumentFactoryService ,
        projectFactory                  :   ProjectFactory              ,
        vsLanguageService               :   VSLanguageService           ) =

    interface IViewTaggerProvider with
        member __.CreateTagger (textView, buffer) =
            maybe{
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                if not generalOptions.LinterEnabled then return! None else
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
                return buffer.Properties.GetOrCreateSingletonProperty (typeof<LintTagger>, fun () ->
                    new LintTagger 
                       (doc, vsLanguageService, serviceProvider, 
                        projectFactory, OpenDocumentsTracker textDocumentFactoryService)
                ) :> obj :?> _
            } |> Option.getOrElse null
