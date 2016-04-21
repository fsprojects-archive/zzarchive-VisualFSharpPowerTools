namespace FSharpVSPowerTools.Logic.VS2015

open System
open System.IO
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.Navigation
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open EnvDTE
open Microsoft.VisualStudio.Text.Editor

type internal DefinitionPeekableItem(span: SnapshotSpan, range: Range.range, peekResultFactory: IPeekResultFactory) =
    interface IPeekableItem with
        member __.DisplayName = null
        member __.Relationships = seq [PredefinedPeekRelationships.Definitions]
        member __.GetOrCreateResultSource _relationshipName = 
            { new IPeekResultSource with
                member __.FindResults(relationshipName, resultCollection, _ct, _callback) =
                    if relationshipName = PredefinedPeekRelationships.Definitions.Name then
                        let filePath = range.FileName
                        let fileName = Path.GetFileName filePath
                        let label = sprintf "%s - (%d, %d)" filePath range.StartLine range.StartColumn
                        let displayInfo = 
                            new PeekResultDisplayInfo(label = label, labelTooltip = box filePath, title = fileName, 
                                                        titleTooltip = filePath)
                        
                        let result =
                            peekResultFactory.Create(
                                                displayInfo, 
                                                filePath, 
                                                range.StartLine - 1,
                                                range.StartColumn,
                                                range.EndLine - 1,
                                                range.EndColumn,
                                                span.StartLine.LineNumber,
                                                span.StartColumn,
                                                false)

                        resultCollection.Add result
            }

type PeekableItemSource
    (
        textBuffer: ITextBuffer,
        doc: ITextDocument,
        peekResultFactory: IPeekResultFactory,
        metadataService: NavigateToMetadataService,
        projectFactory: ProjectFactory,
        vsLanguageService: VSLanguageService
    ) =

    let project = lazy (projectFactory.CreateForDocument textBuffer doc.FilePath)

    let getDefinitionRange (point: SnapshotPoint) =
        async {
            let projectItems = 
                maybe {
                    let! project = project.Value
                    let! span, symbol = vsLanguageService.GetSymbol(point, doc.FilePath, project)
                    return project, span, symbol 
                }
                 
            match projectItems with
            | Some (project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, doc.FilePath, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let start = span.Start
                    let lineStr = start.GetContainingLine().GetText()
                    let! findDeclResult = fileScopedCheckResults.GetDeclarationLocation(symbol.Line, symbol.RightColumn, lineStr, symbol.Text, preferSignature=false)
                    
                    match findDeclResult with
                    | FSharpFindDeclResult.DeclFound range -> 
                        return Some (span, range)
                    | FSharpFindDeclResult.DeclNotFound _ ->
                        let! range = metadataService.TryFindMetadataRange(project, textBuffer, fileScopedCheckResults.ParseTree, span, fsSymbolUse)
                        return range |> Option.map (fun r -> span, r)
                | _ -> return None
            | _ -> return None
        }

    interface IPeekableItemSource with
        member __.AugmentPeekSession (session, peekableItems) =
            asyncMaybe {
                do! if String.Equals(session.RelationshipName, PredefinedPeekRelationships.Definitions.Name, 
                                     StringComparison.OrdinalIgnoreCase) then Some() else None
                let! triggerPoint = session.GetTriggerPoint(textBuffer.CurrentSnapshot) |> Option.ofNullable
                let! symbolSpan, definitionRange = getDefinitionRange triggerPoint
                peekableItems.Add (DefinitionPeekableItem(symbolSpan, definitionRange, peekResultFactory))
            } |> Async.RunSynchronously |> ignore

        member __.Dispose() = ()

[<Export(typeof<IPeekableItemSourceProvider>)>]
[<ContentType "F#">]
[<Name "F# Peekable Item Provider">]
[<SupportsPeekRelationship "IsDefinedBy">]
[<TextViewRole(PredefinedTextViewRoles.PrimaryDocument)>]
type PeekableItemSourceProvider
    [<ImportingConstructor>]
    (peekResultFactory: IPeekResultFactory,
     textDocumentFactoryService: ITextDocumentFactoryService,
     [<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider,
     metadataService: NavigateToMetadataService,
     projectFactory: ProjectFactory,
     vsLanguageService: VSLanguageService) =

    let vsVersion = 
        lazy(let dte = serviceProvider.GetService<DTE, SDTE>()
             VisualStudioVersion.fromDTEVersion dte.Version)

    interface IPeekableItemSourceProvider with
        member __.TryCreatePeekableItemSource(buffer: ITextBuffer) =
            let generalOptions = Setting.getGeneralOptions serviceProvider
            if generalOptions == null || not generalOptions.PeekDefinitionEnabled then null
            else
                match vsVersion.Value with
                | VisualStudioVersion.Unknown
                | VisualStudioVersion.VS2012
                | VisualStudioVersion.VS2013 -> null
                | _ ->
                    match textDocumentFactoryService.TryGetTextDocument buffer with
                    | true, doc ->
                        buffer.Properties.GetOrCreateSingletonProperty(
                            fun () -> upcast new PeekableItemSource(buffer, doc, peekResultFactory, metadataService, projectFactory, vsLanguageService))
                    | _ -> null