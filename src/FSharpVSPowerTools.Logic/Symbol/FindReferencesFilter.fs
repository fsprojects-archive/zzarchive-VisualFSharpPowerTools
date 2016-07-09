namespace FSharpVSPowerTools.Navigation

open System
open System.IO
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.ViewModule.Progress
open Microsoft.VisualStudio.Text
open System.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library


type FindReferencesFilter
     (
        textDocument: ITextDocument, 
        view: IWpfTextView, 
        vsLanguageService: VSLanguageService, 
        serviceProvider: System.IServiceProvider,
        projectFactory: ProjectFactory,
        showProgress: bool,
        fileSystem: IFileSystem
     ) =    

    let dte = serviceProvider.GetDte()
    let project() = projectFactory.CreateForDocument view.TextBuffer textDocument.FilePath

    let getDocumentState (progress: ShowProgress) =
        async {
            let projectItem = maybe {
                progress(OperationState.Reporting(Resource.findAllReferencesInitializingMessage))
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! project = project()
                let! span, symbol = vsLanguageService.GetSymbol(caretPos, textDocument.FilePath, project)
                return project, span, symbol }

            match projectItem with
            | Some (project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, textDocument.FilePath, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let! results = 
                        match projectFactory.GetSymbolDeclarationLocation fsSymbolUse.Symbol textDocument.FilePath project with
                        | Some SymbolDeclarationLocation.File ->
                            progress(OperationState.Reporting(Resource.findAllReferencesFindInFileMessage))
                            vsLanguageService.FindUsagesInFile (span, symbol, fileScopedCheckResults)
                        | scope ->
                            let projectsToCheck =
                                match scope with
                                | Some (SymbolDeclarationLocation.Projects (declProjects, false)) ->
                                    projectFactory.GetDependentProjects dte declProjects
                                | Some (SymbolDeclarationLocation.Projects (declProjects, true)) ->
                                    declProjects
                                // The symbol is declared in .NET framework, an external assembly or in a C# project within the solution.
                                // In order to find all its usages we have to check all F# projects.
                                | _ -> 
                                    let allProjects = 
                                        projectFactory.ListFSharpProjectsInSolution dte  
                                        |> List.map projectFactory.CreateForProject
                                    
                                    Debug.Assert(allProjects |> List.exists (fun p -> p.ProjectFileName = project.ProjectFileName),
                                        sprintf "Project '%O' should appear in the list of checked projects '%A'." project.ProjectFileName 
                                            (allProjects |> List.map (fun p -> p.ProjectFileName)))
                                    allProjects
                            progress(OperationState.Reporting(Resource.findAllReferencesFindInProjectsMessage))
                            vsLanguageService.FindUsages (span, textDocument.FilePath, project, projectsToCheck, progress) 
                    return (results |> Option.map (fun (_, _, references) -> references), symbol) |> Choice1Of2
                | _ -> return Choice2Of2 Resource.findAllReferencesIllformedExpressionMessage
            | _ -> return Choice2Of2 Resource.findAllReferencesInvalidExpressionMessage
        }

    let findReferences () = 
        async {
            use status = new StatusHandler(serviceProvider, StatusIcon.Find, true)
            let progress = 
                if showProgress then status.Report
                else (fun _ -> ())
            let! references = getDocumentState progress
            match references with
            | Choice1Of2 (refs, symbol) ->
                let findResults =
                    match refs with
                    | Some references ->
                        let references = 
                            references
                            |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse))
                            |> Seq.groupBy (fst >> Path.GetFullPathSafe)
                            |> Seq.map (fun (_, symbolUses) -> 
                                // Sort symbols by positions
                                symbolUses 
                                |> Seq.map snd 
                                |> Seq.sortBy (fun s -> s.RangeAlternate.StartLine, s.RangeAlternate.StartColumn))
                            |> Seq.concat
            
                        let nodes =
                            // There are duplications from FCS, we remove duplications by checking text representation
                            references 
                            |> Seq.map (fun reference -> FSharpLibraryNode(symbol.Text, serviceProvider, fileSystem, reference))
                            |> Seq.distinctBy (fun node -> node.GetTextWithOwnership(VSTREETEXTOPTIONS.TTO_DEFAULT))

                        let findResults = FSharpLibraryNode("Find Symbol Results", serviceProvider, fileSystem)
                        for node in nodes do
                            findResults.AddNode(node)
                        findResults
                    | None ->
                        FSharpLibraryNode("Find Symbol Results", serviceProvider, fileSystem)

                let findService = serviceProvider.GetService<IVsFindSymbol, SVsObjectSearch>()
                let searchCriteria = 
                    VSOBSEARCHCRITERIA2(
                        dwCustom = Constants.findReferencesResults,
                        eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
                        pIVsNavInfo = (findResults :> IVsNavInfo),
                        grfOptions = uint32 _VSOBSEARCHOPTIONS2.VSOBSO_LISTREFERENCES,
                        szName = symbol.Text)

                let guid = ref Constants.guidSymbolLibrary
                ErrorHandler.ThrowOnFailure(findService.DoSearch(guid, [| searchCriteria |])) |> ignore
            | Choice2Of2 msg -> 
                // Clear cursor after finishing
                progress(OperationState.Idle)
                let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                statusBar.SetText(msg) |> ignore 
        } |> Async.StartImmediateSafe
 
    interface IMenuCommand with
        member val IsAdded = false with get, set
        member val NextTarget = null with get, set
   
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if (pguidCmdGroup = Constants.guidOldStandardCmdSet && nCmdId = Constants.cmdidFindReferences) then
                findReferences ()
            let x = x :> IMenuCommand
            let nextTarget = x.NextTarget
            if nextTarget <> null then
                nextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)
            else
                VSConstants.S_OK

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = Constants.guidOldStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidFindReferences) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                let x = x :> IMenuCommand
                let nextTarget = x.NextTarget
                if nextTarget <> null then
                    nextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            
                else
                    VSConstants.S_OK
                