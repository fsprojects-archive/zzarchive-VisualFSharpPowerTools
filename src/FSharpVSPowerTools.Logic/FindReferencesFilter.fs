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


type FindReferencesFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider,
                          projectFactory: ProjectFactory) =
    let getDocumentState (progress : OperationState -> unit) =
        async {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let projectItems = maybe {
                progress(OperationState.Reporting(Resource.findAllReferencesInitializingMessage))
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! doc = dte.GetActiveDocument()
                let! project = projectFactory.CreateForDocument view.TextBuffer doc
                let! span, symbol = vsLanguageService.GetSymbol(caretPos, project)
                return doc.FullName, project, span, symbol }

            match projectItems with
            | Some (file, project, span, symbol) ->
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, symbol, file, project, AllowStaleResults.MatchingSource)
                match symbolUse with
                | Some (fsSymbolUse, fileScopedCheckResults) ->
                    let! results = 
                        match projectFactory.GetSymbolDeclarationLocation fsSymbolUse.Symbol file project with
                        | Some SymbolDeclarationLocation.File ->
                            progress(OperationState.Reporting(Resource.findAllReferencesFindInFileMessage))
                            vsLanguageService.FindUsagesInFile (span, symbol, fileScopedCheckResults)
                        | scope ->
                            let projectsToCheck =
                                match scope with
                                | Some (SymbolDeclarationLocation.Projects declProjects) ->
                                    projectFactory.GetDependentProjects dte declProjects
                                // The symbol is declared in .NET framework, an external assembly or in a C# project within the solution.
                                // In order to find all its usages we have to check all F# projects.
                                | _ -> 
                                    let allProjects = 
                                        projectFactory.ListFSharpProjectsInSolution dte  
                                        |> List.map projectFactory.CreateForProject
                                    
                                    if allProjects |> List.exists (fun p -> p.ProjectFileName = project.ProjectFileName) 
                                    then allProjects 
                                    else project :: allProjects
                            progress(OperationState.Reporting(Resource.findAllReferencesFindInProjectsMessage))
                            vsLanguageService.FindUsages (span, file, project, projectsToCheck, progress) 
                    return results |> Option.map (fun (_, _, references) -> references, symbol)
                | _ -> return None
            | _ -> return None
        }

    let findReferences() = 
        async {
            use status = new StatusHandler(serviceProvider, StatusIcon.Find, true)

            let! references = getDocumentState status.Report
            match references with
            | Some (references, symbol) ->
                let references = 
                    references
                    |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse))
                    |> Seq.groupBy (fst >> Path.GetFullPath)
                    |> Seq.map (fun (_, symbolUses) -> 
                        // Sort symbols by positions
                        symbolUses 
                        |> Seq.map snd 
                        |> Seq.sortBy (fun s -> s.RangeAlternate.StartLine, s.RangeAlternate.StartColumn))
                    |> Seq.concat
            
                let findResults = FSharpLibraryNode("Find Symbol Results", serviceProvider)
                for reference in references do
                    findResults.AddNode(FSharpLibraryNode(symbol.Text, serviceProvider, reference))

                let findService = serviceProvider.GetService<IVsFindSymbol, SVsObjectSearch>()
                let searchCriteria = 
                    VSOBSEARCHCRITERIA2(
                        dwCustom = Constants.FindReferencesResults,
                        eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
                        pIVsNavInfo = (findResults :> IVsNavInfo),
                        grfOptions = uint32 _VSOBSEARCHOPTIONS2.VSOBSO_LISTREFERENCES,
                        szName = symbol.Text)

                let guid = ref Constants.guidSymbolLibrary
                ErrorHandler.ThrowOnFailure(findService.DoSearch(guid, [| searchCriteria |])) |> ignore
            | _ -> 
                status.Report(OperationState.Idle)
                let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                statusBar.SetText(Resource.findAllReferencesInvalidExpressionMessage) |> ignore 
        } |> Async.StartImmediateSafe

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if (pguidCmdGroup = Constants.guidOldStandardCmdSet && nCmdId = Constants.cmdidFindReferences) then
                findReferences()
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = Constants.guidOldStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidFindReferences) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            