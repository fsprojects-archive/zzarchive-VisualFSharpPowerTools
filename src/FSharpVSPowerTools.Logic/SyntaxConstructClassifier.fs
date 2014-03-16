namespace FSharpVSPowerTools.SyntaxColoring

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
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
    let functionType = classificationRegistry.GetClassificationType "FSharp.Function"

    let getClassficationType cat =
        match cat with
        | ReferenceType -> Some referenceType
        | ValueType -> Some valueType
        | PatternCase -> Some patternType
        | TypeParameter -> None
        | Function -> Some functionType
        | PublicField | Other -> None
    
    let synchronousUpdate (newSpans: (Category * SnapshotSpan) []) = 
        lock lockObject <| fun _ -> 
            wordSpans <- newSpans
            let snapshot = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
            classificationChanged.Trigger (self, ClassificationChangedEventArgs(snapshot))
    
    let updateSyntaxConstructClassifiers() = 
        let snapshot = buffer.CurrentSnapshot
        if not isWorking && snapshot <> lastSnapshot then 
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc

                debug "[SyntaxConstructClassifier] - Effective update"
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
    
    let _ = DocumentEventsListener ([ViewChange.bufferChangedEvent buffer], 
                                    TimeSpan.FromMilliseconds 200.,
                                    fun _ -> updateSyntaxConstructClassifiers())
    
    interface IClassifier with
        member x.GetClassificationSpans(snapshotSpan: SnapshotSpan) = 
            // And now return classification spans for everything
            let spans = 
                wordSpans
                |> Array.filter (fun (_, span) -> span.IntersectsWith snapshotSpan)
                |> Array.choose (fun (category, span) -> 
                    getClassficationType category 
                    |> Option.map (fun classificationType -> ClassificationSpan(span, classificationType)))
            upcast spans
        
        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish
