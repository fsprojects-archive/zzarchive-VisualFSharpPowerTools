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
    let lastSnapshot = Atom null
    let locations = Atom [||]
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
    
    let synchronousUpdate (newLocations: (Category * (int * int * int * int)) []) = 
        locations.Swap(fun _ -> newLocations) |> ignore
        let snapshot = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        classificationChanged.Trigger (self, ClassificationChangedEventArgs(snapshot))
    
    let updateSyntaxConstructClassifiers() =
        let snapshot = buffer.CurrentSnapshot
        if not isWorking && snapshot <> lastSnapshot.Value then 
            isWorking <- true
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc

                debug "[SyntaxConstructClassifier] - Effective update"
                lastSnapshot.Swap (fun _ -> snapshot) |> ignore
                async {
                    try
                        let! allSymbolsUses =
                            vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FullName, project, AllowStaleResults.MatchingSource)
                
                        getCategoriesAndLocations allSymbolsUses
                        |> Array.sortBy (fun (_, (startLine, startCol, _, _)) -> startLine, startCol)
                        |> synchronousUpdate
                    finally
                        isWorking <- false
                } |> Async.Start
            } |> ignore
    
    let _ = DocumentEventsListener ([ViewChange.bufferChangedEvent buffer], 
                                    TimeSpan.FromMilliseconds 200.,
                                    fun _ -> updateSyntaxConstructClassifiers())

    do buffer.Changed.Add (fun _ -> locations.Swap (fun _ -> [||]) |> ignore)
    
    interface IClassifier with
        // it's called for each visible line of code
        member x.GetClassificationSpans(snapshotSpan: SnapshotSpan) = 
            let spanStartLine = snapshotSpan.Start.GetContainingLine().LineNumber + 1
            let spanEndLine = (snapshotSpan.End - 1).GetContainingLine().LineNumber + 1

            let spans = 
                locations.Value
                // locations are sorted, so we can safely filter them efficently
                |> Seq.skipWhile (fun (_, (startLine, _, _, _)) -> startLine < spanStartLine)
                |> Seq.takeWhile (fun (_, (startLine, _, _, _)) -> startLine <= spanEndLine)
                |> Seq.choose (fun (category, location) -> 
                    getClassficationType category 
                    |> Option.map (fun classificationType -> 
                         ClassificationSpan(fromPos snapshotSpan.Snapshot location, classificationType)))
                |> Seq.toArray
            upcast spans
        
        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish
