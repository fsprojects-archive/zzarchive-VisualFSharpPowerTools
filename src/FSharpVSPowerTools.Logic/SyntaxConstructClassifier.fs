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

type SyntaxConstructClassifier (doc: ITextDocument, classificationRegistry: IClassificationTypeRegistryService,
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
    
    let synchronousUpdate (newLocations: CategorizedColumnSpan []) = 
        locations.Swap(fun _ -> newLocations) |> ignore
        // TextBuffer is null if a solution is closed at this moment
        if doc.TextBuffer <> null then
            let currentSnapshot = doc.TextBuffer.CurrentSnapshot
            let snapshot = SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length)
            classificationChanged.Trigger(self, ClassificationChangedEventArgs(snapshot))
    
    let updateSyntaxConstructClassifiers() =
        let snapshot = doc.TextBuffer.CurrentSnapshot
        if not isWorking && snapshot <> lastSnapshot.Value then 
            isWorking <- true
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! projectItem = Option.attempt (fun _ -> dte.Solution.FindProjectItem doc.FilePath)
                let! project = ProjectProvider.createForFileInProject doc.FilePath projectItem.ContainingProject

                debug "[SyntaxConstructClassifier] - Effective update"
                lastSnapshot.Swap (fun _ -> snapshot) |> ignore
                async {
                    try
                        let! allSymbolsUses =
                            vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FilePath, project, AllowStaleResults.MatchingSource)
                
                        getCategoriesAndLocations allSymbolsUses
                        |> Array.sortBy (fun { Line = line } -> line)
                        |> synchronousUpdate
                    finally
                        isWorking <- false
                } |> Async.Start
            } |> ignore
    
    let _ = DocumentEventsListener ([ViewChange.bufferChangedEvent doc.TextBuffer], 
                                    TimeSpan.FromMilliseconds 200.,
                                    fun _ -> updateSyntaxConstructClassifiers())

    do doc.TextBuffer.Changed.Add (fun _ -> locations.Swap (fun _ -> [||]) |> ignore)
    
    interface IClassifier with
        // it's called for each visible line of code
        member x.GetClassificationSpans(snapshotSpan: SnapshotSpan) = 
            let spanStartLine = snapshotSpan.Start.GetContainingLine().LineNumber + 1
            let spanEndLine = (snapshotSpan.End - 1).GetContainingLine().LineNumber + 1

            let spans =
                locations.Value
                // locations are sorted, so we can safely filter them efficently
                |> Seq.skipWhile (fun { Line = line } -> line < spanStartLine)
                |> Seq.takeWhile (fun { Line = line } -> line <= spanEndLine)
                |> Seq.choose (fun loc -> 
                    getClassficationType loc.Category
                    |> Option.map (fun classificationType -> 
                        let range = loc.Line, loc.ColumnSpan.Start, loc.Line, loc.ColumnSpan.End
                        ClassificationSpan(fromPos snapshotSpan.Snapshot range, classificationType)))
                |> Seq.toArray
            upcast spans
        
        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish
