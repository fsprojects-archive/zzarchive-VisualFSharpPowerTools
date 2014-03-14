namespace FSharpVSPowerTools.SyntaxColoring

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding

type SyntaxConstructClassifier (buffer: ITextBuffer, classificationRegistry: IClassificationTypeRegistryService,
                                vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider) as self = 
    let classificationChanged = Event<_,_>()
    let lockObject = new Object()
    let mutable lastSnapshot: ITextSnapshot = null
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable isWorking = false
    
    let synchronousUpdate (newSpans: NormalizedSnapshotSpanCollection) = 
        lock lockObject <| fun _ -> 
            wordSpans <- newSpans
            classificationChanged.Trigger
                (self, ClassificationChangedEventArgs(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)))
    
    let updateSyntaxConstructClassifiers callerName = 
        let snapshot = buffer.CurrentSnapshot
        if isWorking = false && snapshot <> lastSnapshot then 
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, Microsoft.VisualStudio.Shell.Interop.SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc

                debug "[SyntaxConstructClassifier] %s - Effective update" callerName
                isWorking <- true
                lastSnapshot <- snapshot
                async {
                    try
                        let! allSymbolsUses =
                            vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FullName, project, AllowStaleResults.MatchingSource)
                
                        let typeLocations = SourceCodeClassifier.getTypeLocations allSymbolsUses
                        let wordSpans = NormalizedSnapshotSpanCollection (typeLocations |> Array.map (fromFSharpPos snapshot))
                        synchronousUpdate wordSpans
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
               for (span: SnapshotSpan) in wordSpans do
                   let classificationType = classificationRegistry.GetClassificationType "FSharp.TypeName"
                   yield ClassificationSpan(span, classificationType) |] :> _
        
        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish
