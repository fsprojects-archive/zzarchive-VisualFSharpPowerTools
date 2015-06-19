namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio
open System

/// Listen to events related to solution builds
type SolutionBuildEventListener(serviceProvider: IServiceProvider) as self =
    let solutionBuildManager = serviceProvider.GetService<IVsSolutionBuildManager2, SVsSolutionBuildManager>()
    let mutable updateSolutionEventsCookie = 0u
    do solutionBuildManager.AdviseUpdateSolutionEvents(self, &updateSolutionEventsCookie) |> ignore
    let activeConfigChanged = Event<_>()

    [<CLIEvent>]
    member __.ActiveConfigChanged = activeConfigChanged.Publish

    interface IVsUpdateSolutionEvents with
        member __.OnActiveProjectCfgChange(pIVsHierarchy: IVsHierarchy): int = 
            match getProject pIVsHierarchy with
            | Some project ->
                activeConfigChanged.Trigger(project)
            | None ->
                ()
            VSConstants.S_OK
        
        member __.UpdateSolution_Begin(_pfCancelUpdate: byref<int>): int = 
            VSConstants.E_NOTIMPL
        
        member __.UpdateSolution_Cancel(): int = 
            VSConstants.E_NOTIMPL
        
        member __.UpdateSolution_Done(_fSucceeded: int, _fModified: int, _fCancelCommand: int): int = 
            VSConstants.E_NOTIMPL
        
        member __.UpdateSolution_StartUpdate(_pfCancelUpdate: byref<int>): int = 
            VSConstants.E_NOTIMPL
    
    interface IDisposable with
        member __.Dispose() = 
            solutionBuildManager.UnadviseUpdateSolutionEvents(updateSolutionEventsCookie) |> ignore
