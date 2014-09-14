namespace FSharpVSPowerTools.ProjectSystem

open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio
open System

/// Listen to events related to solution/project open/close
type SolutionEventsListener(serviceProvider: IServiceProvider) as self =
    let slnCookie = ref 0u
    let solution = serviceProvider.GetService<IVsSolution, SVsSolution>()
    do solution.AdviseSolutionEvents(self, slnCookie) |> ignore

    let solutionOpened = Event<_>()
    let solutionClosed = Event<_>()

    [<CLIEvent>]
    member x.SolutionOpened = solutionOpened.Publish
    [<CLIEvent>]
    member x.SolutionClosed = solutionClosed.Publish

    interface IVsSolutionEvents with
        member x.OnAfterCloseSolution(_pUnkReserved: obj): int = 
            solutionClosed.Trigger(EventArgs.Empty)
            VSConstants.S_OK
        
        member x.OnAfterLoadProject(_pStubHierarchy: IVsHierarchy, _pRealHierarchy: IVsHierarchy): int = 
            VSConstants.E_NOTIMPL

        member x.OnAfterOpenProject(_pHierarchy: IVsHierarchy, _fAdded: int): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnAfterOpenSolution(_pUnkReserved: obj, _fNewSolution: int): int = 
            solutionOpened.Trigger(EventArgs.Empty)
            VSConstants.S_OK
        
        member x.OnBeforeCloseProject(_pHierarchy: IVsHierarchy, _fRemoved: int): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnBeforeCloseSolution(_pUnkReserved: obj): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnBeforeUnloadProject(_pRealHierarchy: IVsHierarchy, _pStubHierarchy: IVsHierarchy): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnQueryCloseProject(_pHierarchy: IVsHierarchy, _fRemoving: int, _pfCancel: byref<int>): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnQueryCloseSolution(_pUnkReserved: obj, _pfCancel: byref<int>): int = 
            VSConstants.E_NOTIMPL
        
        member x.OnQueryUnloadProject(_pRealHierarchy: IVsHierarchy, _pfCancel: byref<int>): int = 
            VSConstants.E_NOTIMPL
        
    interface IDisposable with
        member x.Dispose() = 
            solution.UnadviseSolutionEvents(!slnCookie) |> ignore