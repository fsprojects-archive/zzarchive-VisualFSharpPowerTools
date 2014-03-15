namespace FSharpVSPowerTools.SyntaxColoring

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.Core.SourceCodeClassifier
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding

type SyntaxConstructClassifier (buffer: ITextBuffer, classificationRegistry: IClassificationTypeRegistryService,
                                vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider) as self = 
    let classificationChanged = Event<_,_>()
    let lockObject = Object()
    let mutable lastSnapshot: ITextSnapshot = null
    let mutable wordSpans = [||]
    let mutable isWorking = false 

    
    let referenceType = classificationRegistry.GetClassificationType "FSharp.ReferenceType"
    let valueType = classificationRegistry.GetClassificationType "FSharp.ValueType"
    let patternType = classificationRegistry.GetClassificationType "FSharp.PatternCase"

    let getClassficationType cat =
        match cat with
        | ReferenceType -> Some referenceType
        | ValueType -> Some valueType
        | PatternCase -> Some patternType
        | TypeParameter -> None
        | Other -> None
    
    let synchronousUpdate (newSpans: (Category * SnapshotSpan) []) = 
        lock lockObject <| fun _ -> 
            wordSpans <- newSpans
            classificationChanged.Trigger
                (self, ClassificationChangedEventArgs(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)))
    
    let updateSyntaxConstructClassifiers callerName = 
        let snapshot = buffer.CurrentSnapshot
        if not isWorking && snapshot <> lastSnapshot then 
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc

                debug "[SyntaxConstructClassifier] %s - Effective update" callerName
                isWorking <- true
                lastSnapshot <- snapshot
                async {
                    try
                        let! allSymbolsUses =
                            vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FullName, project, AllowStaleResults.MatchingSource)
                
                        getTypeLocations allSymbolsUses
                        |> Array.map (fun (category, location) -> category, fromPos snapshot location)
                        |> synchronousUpdate
                    finally
                        isWorking <- false
                } |> Async.Start
            } |> ignore
    do 
        buffer.Changed.Add <| fun _ -> updateSyntaxConstructClassifiers "Buffer changed"
        // Execute it the first time
        updateSyntaxConstructClassifiers "First execution"
    
    interface IClassifier with
        member x.GetClassificationSpans(_snapshotSpan: SnapshotSpan) = 
            [| // And now return classification spans for everything
               for (category, span) in wordSpans do
                   match getClassficationType category with
                   | Some classificationType ->
                        yield ClassificationSpan(span, classificationType)
                   | None -> () |] :> _
        
        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish
