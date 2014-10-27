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


type FindReferencesFilter(textDocument: ITextDocument, 
                          view: IWpfTextView, 
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: System.IServiceProvider,
                          projectFactory: ProjectFactory,
                          showProgress: bool) =    
    let getDocumentState (progress: ShowProgress) =
        async {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let projectItems = maybe {
                progress(OperationState.Reporting(Resource.findAllReferencesInitializingMessage))
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! doc = dte.GetCurrentDocument(textDocument.FilePath)
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

    let status = 
        if showProgress then Some (new StatusHandler(serviceProvider, StatusIcon.Find, true))
        else None

    let findReferences progress = 
        async {
            let! references = getDocumentState progress
            match references with
            | Some (references, symbol) ->
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
                    |> Seq.map (fun reference -> FSharpLibraryNode(symbol.Text, serviceProvider, reference))
                    |> Seq.distinctBy (fun node -> node.GetTextWithOwnership(VSTREETEXTOPTIONS.TTO_DEFAULT))

                let findResults = FSharpLibraryNode("Find Symbol Results", serviceProvider)
                for node in nodes do
                    findResults.AddNode(node)

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
            | _ -> 
                progress(OperationState.Idle)
                let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                statusBar.SetText(Resource.findAllReferencesInvalidExpressionMessage) |> ignore 
        } |> Async.StartImmediateSafe

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set
    
    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if (pguidCmdGroup = Constants.guidOldStandardCmdSet && nCmdId = Constants.cmdidFindReferences) then
                status
                |> Option.map (fun status -> status.Report)
                |> Option.getOrElse (fun _ -> ())
                |> findReferences
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
                let nextTarget = x.NextTarget
                if nextTarget <> null then
                    nextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            
                else
                    VSConstants.S_OK

    interface IDisposable with
        member __.Dispose() = 
            status |> Option.iter (fun status -> (status :> IDisposable).Dispose())
        
                