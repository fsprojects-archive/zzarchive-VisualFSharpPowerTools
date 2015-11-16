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
type internal Logger () = //,
   //  [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let shellService = ServiceProvider.GlobalProvider.GetService<SVsUIShell,IVsUIShell>()
    let activityLog  = ServiceProvider.GlobalProvider.GetService<SVsActivityLog,IVsActivityLog>()

    let getEntryTypeInt = function
        | LogType.Information -> __ACTIVITYLOG_ENTRYTYPE.ALE_INFORMATION
        | LogType.Warning -> __ACTIVITYLOG_ENTRYTYPE.ALE_WARNING
        | LogType.Error -> __ACTIVITYLOG_ENTRYTYPE.ALE_ERROR

    let getIcon = function
        | LogType.Information -> OLEMSGICON.OLEMSGICON_INFO
        | LogType.Warning -> OLEMSGICON.OLEMSGICON_WARNING
        | LogType.Error -> OLEMSGICON.OLEMSGICON_CRITICAL



//    static let mutable globalServiceProvider: IServiceProvider option = None
//
//    /// Quick and dirty global service provider for testing purpose.
//    static member internal GlobalServiceProvider 
//        with get () = globalServiceProvider |> Option.getOrElse (ServiceProvider.GlobalProvider :> _)
//        and set v = globalServiceProvider <- Some v

    member __.Log(logType, message) =
        activityLog.LogEntry (uint32 (getEntryTypeInt logType), Resource.vsPackageTitle, message) |> ignore
        
    member __.MessageBox(logType, message) =
        let icon = getIcon logType
        let result = ref 0
        shellService.ShowMessageBox(0u, ref Guid.Empty, Resource.vsPackageTitle, message, "", 0u, 
                OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST, icon, 0, result)



module OutputWindowHelper =
    open Microsoft.VisualStudio

    let getPowerToolsWindowPane(serviceProvider: IServiceProvider) =
        let outputWindow = serviceProvider.TryGetService<SVsOutputWindow,IVsOutputWindow>()
        outputWindow
        |> Option.bind (fun window ->
            let outputPaneGuid = ref Constants.guidPowerToolsOutputPane
            window.CreatePane(outputPaneGuid, Resource.vsPackageTitle, 1, 1) |> ignore
            match window.GetPane(outputPaneGuid) with
            | VSConstants.S_OK, windowPane -> Some windowPane
            | _ -> None)

    let writeToOutputWindow (logType: LogType) (message: string) (window: IVsOutputWindowPane) =
        let outputMessage = 
            String.Format("[PVFPT][{0} {1}] {2}{3}",
                logType.ToString(), DateTime.Now.ToString("hh:mm:ss tt"), message, Environment.NewLine)
        window.OutputString(outputMessage) |> ignore
    
    /// This global output window is initialized once for each Visual Studio session.
    let outputWindowPane = lazy(getPowerToolsWindowPane ServiceProvider.GlobalProvider )
    let globalOptions = lazy(Setting.getGlobalOptions ServiceProvider.GlobalProvider)

    let diagnose logType msg =
        outputWindowPane.Value 
        |> Option.iter (writeToOutputWindow logType msg)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Logging =
    open OutputWindowHelper

    /// This is a global logger, please make sure that it is executed after the package is loaded.
    //let internal logger = lazy (Logger(Logger.GlobalServiceProvider))
    let internal logger = lazy (Logger())

    let internal log logType (produceMessage: _ -> string) = 
        // Guard against exceptions since it's not entirely clear that GlobalProvider will be populated correctly.
        if (try globalOptions.Value.DiagnosticMode with _ -> false) then
            let msg = produceMessage()
            diagnose logType msg
            logger.Value.Log(logType, msg)
            
    let logInfo msg = log LogType.Information msg
    let logWarning msg = log LogType.Warning msg
    let logError msg = log LogType.Error msg

    let internal messageBox logType msg = logger.Value.MessageBox(logType, msg) |> ignore

    let messageBoxInfo msg = messageBox LogType.Information msg
    let messageBoxWarning msg = messageBox LogType.Warning msg
    let messageBoxError msg = messageBox LogType.Error msg

    // Specialized logging functions to be C#-friendly

    let logInfoMessage (msg: Func<string>) = logInfo msg.Invoke
    let logWarningMessage (msg: Func<string>) = logWarning msg.Invoke
    let logErrorMessage (msg: Func<string>) = logError msg.Invoke

    let logExceptionWithContext(ex: Exception, context) = 
        logError (fun _ -> sprintf "Context: %s\nException Message: %s\nStack Trace: %s" context ex.Message ex.StackTrace)

    let logException (ex: Exception) = 
        logError (fun _ -> sprintf "Exception Message: %s\nStack Trace: %s" ex.Message ex.StackTrace)