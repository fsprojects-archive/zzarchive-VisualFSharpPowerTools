namespace FSharpVSPowerTools.Navigation

open System
open System.Threading
open System.IO
open System.Windows
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding

[<RequireQualifiedAccess>]
module PkgCmdConst =
    let cmdidFindReferences = uint32 VSConstants.VSStd97CmdID.FindReferences
    let guidBuiltinCmdSet = VSConstants.GUID_VSStandardCommandSet97
    let guidSymbolLibrary = Guid("2ad4e2a2-b89f-48b6-98e8-363bd1a35450")

[<NoEquality; NoComparison>]
type DocumentState =
    { Word: SnapshotSpan * Symbol
      File: string
      Project: IProjectProvider
      TargetProjects: IProjectProvider list }

type FindReferencesFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider) =
    let getState() =
        async {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let projectItems = maybe {
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc
                let! span, sym = vsLanguageService.GetSymbol(caretPos, project)
                return doc.FullName, project, span, sym }

            match projectItems with
            | Some (file, project, span, sym) ->
                let syncContext = SynchronizationContext.Current
                do! Async.SwitchToThreadPool()
                let! symbolUse = vsLanguageService.GetFSharpSymbolUse(span, sym, file, project, AllowStaleResults.No)
                do! Async.SwitchToContext syncContext
                match symbolUse with
                | Some (symbolUse, _) ->
                    match ProjectProvider.getSymbolUsageScope symbolUse.Symbol dte file with
                    | Some symbolDeclLocation ->
                        let dependentProjects =
                            let declProject = 
                                match symbolDeclLocation with
                                | SymbolDeclarationLocation.File -> project
                                | SymbolDeclarationLocation.Project p -> p

                            let dependentProjects = ProjectProvider.getDependentProjects dte declProject
                            if dependentProjects |> List.exists (fun x -> x.ProjectFileName = declProject.ProjectFileName)
                            then dependentProjects
                            else declProject :: dependentProjects

                        return Some { Word = span, sym
                                      File = file
                                      Project = project
                                      TargetProjects = dependentProjects }
                    | _ -> return None
                | _ -> return None
            | _ -> return None
        }

    let findReferences() = 
        async {
            let! state = getState()
            match state with
            | Some { Word = cw, sym; File = file; Project = project; TargetProjects = targetProjects } ->
                let syncContext = SynchronizationContext.Current
                do! Async.SwitchToThreadPool()
                let! references = vsLanguageService.FindUsages (cw, file, project, targetProjects) 
                do! Async.SwitchToContext syncContext
                let references = 
                    references
                    |> Option.map (fun (_, _, refs) -> 
                        refs 
                        |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse))
                        |> Seq.groupBy (fst >> Path.GetFullPath)
                        |> Seq.map (fun (_, symbolUses) -> 
                            // Sort symbols by positions
                            symbolUses 
                            |> Seq.map snd 
                            |> Seq.sortBy (fun s -> s.RangeAlternate.StartLine, s.RangeAlternate.StartColumn) 
                            |> Seq.toList)
                        |> Seq.concat
                        |> Seq.toList)
                    |> fun opt -> defaultArg opt []
            
                let findResults = FSharpLibraryNode("Find results", serviceProvider)
                for r in references do
                    findResults.AddNode(FSharpLibraryNode(r.Symbol.DisplayName, serviceProvider, r))

                let findService = serviceProvider.GetService<IVsFindSymbol, SVsObjectSearch>()
                let searchCriteria = 
                    VSOBSEARCHCRITERIA2(
                        dwCustom = Constants.FindReferencesResults,
                        eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
                        pIVsNavInfo = (findResults :> IVsNavInfo),
                        grfOptions = uint32 _VSOBSEARCHOPTIONS2.VSOBSO_LISTREFERENCES,
                        szName = sym.Text)

                let guid = ref PkgCmdConst.guidSymbolLibrary
                ErrorHandler.ThrowOnFailure(findService.DoSearch(guid, [| searchCriteria |])) |> ignore
            | _ -> 
                let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                statusBar.SetText("The caret must be on valid expression to find all references.") |> ignore 
        } |> Async.StartImmediate

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if (pguidCmdGroup = PkgCmdConst.guidBuiltinCmdSet && nCmdId = PkgCmdConst.cmdidFindReferences) then
                findReferences()
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = PkgCmdConst.guidBuiltinCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdConst.cmdidFindReferences) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            