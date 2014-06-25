namespace FSharpVSPowerTools

open System
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.ViewModule.Progress

/// Union of the available Visual Studio icons used for animation in the status bar
type StatusIcon =
    | General
    | Print
    | Save
    | Deploy
    | Synch
    | Build
    | Find

/// Handler for reporting status information to the Visual Studio status bar area
type StatusHandler(serviceProvider: IServiceProvider, statusIcon: StatusIcon, overrideCursor) =
    // Get our status bar service up front
    let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()

    // This tracks the last known state, so we can handle tracking identifiers correctly
    let mutable lastState = OperationState.Idle
    let mutable id = 0u
    let mutable icon = 
        let tmp = 
            match statusIcon with
            | General -> Constants.SBAI_General
            | Print -> Constants.SBAI_Print
            | Save -> Constants.SBAI_Save
            | Deploy -> Constants.SBAI_Deploy
            | Synch -> Constants.SBAI_Synch             
            | Build -> Constants.SBAI_Build
            | Find -> Constants.SBAI_Find
        box tmp
    let mutable cursorOverride: IDisposable = null

    let startAnimation () =
        statusBar.Animation(1, &icon) |> ignore
    let stopAnimation () =
        statusBar.Animation(0, &icon) |> ignore
    let setProgress status current total =
        statusBar.Progress(&id, 1, status, (uint32 current), (uint32 total)) |> ignore
    let stopProgress () =
        statusBar.Progress(&id, 0, String.Empty, 0u, 0u) |> ignore
        id <- 0u
    let clearStatus() =
        statusBar.SetText(String.Empty) |> ignore
        statusBar.Clear() |> ignore
    let setStatus status =
        statusBar.SetText(status) |> ignore
    let setWaitCursor() =
        match overrideCursor, cursorOverride with
        | true, null ->
            cursorOverride <- Cursor.wait()
        | _, _ ->
            ()
    let clearWaitCursor () =
        match cursorOverride with
        | null -> ()
        | _ -> 
            cursorOverride.Dispose()
            cursorOverride <- null
    
    let mutable frozen = 0

    let updateFrozen () =
        if lastState = OperationState.Idle then
            // Verify whether we're frozen
            statusBar.IsFrozen(&frozen) |> ignore
        else
            frozen <- 0

    let report (operationState: OperationState) =        
        updateFrozen()

        if lastState = OperationState.Idle && frozen = 0 then
            match lastState, operationState with
            // Handle idle calls
            | OperationState.Idle, OperationState.Idle -> () // Do nothing if we're changing from idle to idle
            // Change from executing -> Other
            | OperationState.Executing(_,_,_), OperationState.Idle ->
                stopAnimation()
                stopProgress()
                clearStatus()
                clearWaitCursor()            
            | OperationState.Executing(_,_,_), OperationState.Executing(status, current, total) ->
                // Stop the progress
                setProgress status current total
                setStatus status
            | OperationState.Executing(_,_,_), OperationState.Reporting(status) ->
                // Stop the progress
                stopProgress()
                setStatus status
            // Change from Reporting -> other
            | OperationState.Reporting(_), OperationState.Reporting(status) ->
                setStatus status
            | OperationState.Reporting(_), OperationState.Idle ->
                stopAnimation()
                clearStatus()
                clearWaitCursor()
            | OperationState.Reporting(_), OperationState.Executing(status, current, total) ->            
                setStatus status
                setProgress status current total
            // Change from Idle -> other
            | OperationState.Idle, OperationState.Reporting(status) ->
                startAnimation()
                setStatus status
                setWaitCursor()
            | OperationState.Idle, OperationState.Executing(status, current, total) ->
                startAnimation()
                setStatus status
                setProgress status current total
                setWaitCursor()
            lastState <- operationState
    
    let cleanup() =
        if lastState <> OperationState.Idle then
            report(OperationState.Idle) // Set us to an idle state to cleanup

    /// Provides a function which matches the required syntax to use FSharp.ViewModule.Progress reporting
    member x.Report = report

    interface IDisposable with
        member x.Dispose() = cleanup()

[<RequireQualifiedAccess>]
module Status =
    /// Gets a reporter that works with FSharp.ViewModule reporting
    let getReporter (statusHandler: StatusHandler) =
        Some(statusHandler.Report)

    /// Gets a reporter that works with FSharp.ViewModule reporting and reports to the status bar and a progress manager
    let getCompositeReporter (statusHandler: StatusHandler) (progressManager: ProgressManager) =
        let statusReport = statusHandler.Report
        let managerReport = updateProgress(progressManager)
        let composed operationState =
            statusReport(operationState)
            managerReport(operationState)
        Some(composed)