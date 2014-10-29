namespace FSharpVSPowerTools

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
type LogType =
    | Information
    | Warning
    | Error
    override x.ToString() = 
        match x with
        | Information -> "Information"
        | Warning -> "Warning"
        | Error -> "Error"

[<Export>]
type Logger [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let getEntryTypeInt = function
        | LogType.Information -> __ACTIVITYLOG_ENTRYTYPE.ALE_INFORMATION
        | LogType.Warning -> __ACTIVITYLOG_ENTRYTYPE.ALE_WARNING
        | LogType.Error -> __ACTIVITYLOG_ENTRYTYPE.ALE_ERROR

    let getIcon = function
        | LogType.Information -> OLEMSGICON.OLEMSGICON_INFO
        | LogType.Warning -> OLEMSGICON.OLEMSGICON_WARNING
        | LogType.Error -> OLEMSGICON.OLEMSGICON_CRITICAL

    let getShellService() = 
        serviceProvider.GetService<IVsUIShell, SVsUIShell>()

    let getActivityLogService() =
        let service = serviceProvider.GetService<IVsActivityLog, SVsActivityLog>() 
        Option.ofNull service

    member __.Log logType message =
        getActivityLogService()
        |> Option.iter (fun s -> s.LogEntry(uint32 (getEntryTypeInt logType), Resource.vsPackageTitle, message) |> ignore)

    member x.LogExceptionWithMessage(e: Exception, message) =
        let message =
            sprintf "Message: %s\nException Message: %s\nStack Trace: %s" message e.Message e.StackTrace
        x.Log LogType.Error message

    member x.LogException (e: Exception) =
        let message = sprintf "Exception Message: %s\nStack Trace: %s" e.Message e.StackTrace
        x.Log LogType.Error message
                
    member x.MessageBox(logType, message) =
        let icon = getIcon logType
        let service = getShellService()
        let result = ref 0
        service.ShowMessageBox(0u, ref Guid.Empty, Resource.vsPackageTitle, message, "", 0u, 
            OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST, icon, 0, result)

module OutputWindowHelper =
    open Microsoft.VisualStudio

    let tryGetPowerToolsWindowPane(serviceProvider: IServiceProvider) =
        let outputWindow = serviceProvider.GetService<IVsOutputWindow, SVsOutputWindow>()
        outputWindow
        |> Option.ofNull
        |> Option.bind (fun window ->
            let outputPaneGuid = ref Constants.guidPowerToolsOutputPane
            window.CreatePane(outputPaneGuid, Resource.vsPackageTitle, 1, 1) |> ignore
            match window.GetPane(outputPaneGuid) with
            | VSConstants.S_OK, windowPane -> Some windowPane
            | _ -> None)

    let writeToOutputWindow (logType: LogType) (message: string) (window: IVsOutputWindowPane) =
        let outputMessage = 
            String.Format("[VFPT][{0} {1}] {2}{3}",
                logType.ToString(), DateTime.Now.ToString("hh:mm:ss tt"), message, Environment.NewLine)
        window.OutputString(outputMessage) |> ignore
    
    /// This global output window is initialized once for each Visual Studio session.
    let outputWindowPane = lazy(tryGetPowerToolsWindowPane(ServiceProvider.GlobalProvider))
    let globalOptions = lazy(Setting.getGlobalOptions(ServiceProvider.GlobalProvider))

    let diagnose logType msg =
        // Guard against exceptions since it's not entirely clear that GlobalProvider will be populated correctly.
        if (try globalOptions.Value.DiagnosticMode with _ -> false) then
            outputWindowPane.Value 
            |> Option.iter (writeToOutputWindow logType msg)       

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Logging =
    open OutputWindowHelper

    /// This is a global logger, please make sure that it is executed after the package is loaded.
    let logger = lazy (Logger(ServiceProvider.GlobalProvider))

    let log logType (msg: Printf.StringFormat<'T, unit>) = 
        let format msg = 
            diagnose logType msg
            logger.Value.Log logType msg
        Printf.kprintf format msg
        
    let logInfo msg = log LogType.Information msg
    let logWarning msg = log LogType.Warning msg
    let logError msg = log LogType.Error msg

    let logExceptionWithMessage (ex: Exception) message = 
        logError "Context: %s\nException Message: %s\nStack Trace: %s" message ex.Message ex.StackTrace

    let logException (ex: Exception) = 
        logError "Exception Message: %s\nStack Trace: %s" ex.Message ex.StackTrace
        
    let messageBox logType msg = logger.Value.MessageBox(logType, msg) |> ignore

    let messageBoxInfo msg = messageBox LogType.Information msg
    let messageBoxWarning msg = messageBox LogType.Warning msg
    let messageBoxError msg = messageBox LogType.Error msg