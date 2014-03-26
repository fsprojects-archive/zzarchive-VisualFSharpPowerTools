namespace FSharpVSPowerTools

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem.VSUtils

type LogType =
    | Information
    | Warning
    | Error

[<Export>]
type Logger
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let getEntryType logType =
        let value =
            match logType with
            | LogType.Information -> __ACTIVITYLOG_ENTRYTYPE.ALE_INFORMATION
            | LogType.Warning -> __ACTIVITYLOG_ENTRYTYPE.ALE_WARNING
            | LogType.Error -> __ACTIVITYLOG_ENTRYTYPE.ALE_ERROR
        Convert.ToUInt32(value)

    let getIcon logType =
        match logType with
        | LogType.Information -> OLEMSGICON.OLEMSGICON_INFO
        | LogType.Warning -> OLEMSGICON.OLEMSGICON_WARNING
        | LogType.Error -> OLEMSGICON.OLEMSGICON_CRITICAL

    let getShellService() = serviceProvider.GetService<IVsUIShell, SVsUIShell>()

    let getActivityLogService() =
        let service = serviceProvider.GetService<IVsActivityLog, SVsActivityLog>()
        match service with 
        | null -> None
        | x -> Some x

    member x.Log logType message =
        getActivityLogService()
        |> Option.iter (fun s -> s.LogEntry((getEntryType logType), Resource.vsPackageTitle, message) |> ignore)

    member x.LogExceptionWithMessage (e:Exception) message=
        let message =
            sprintf "Message: %s\nException Message: %s\nStack Trace: %s" message e.Message e.StackTrace
        x.Log LogType.Error message

    member x.LogException (e:Exception) =
        let message = sprintf "Exception Message: %s\mStart Trace: %s" e.Message e.StackTrace
        x.Log LogType.Error message
                
    member x.MessageBox logType message =
        let icon = getIcon logType
        let service = getShellService()
        let result = 0
        service.ShowMessageBox(0u, ref Guid.Empty, Resource.vsPackageTitle, message, "", 0u, OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST, icon, 0, ref result)

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Loggging =
    let inline log logType msg = 
        let logger = new Logger(ServiceProvider.GlobalProvider)
        let log = logger.Log logType
        Printf.kprintf log msg

    let inline logInf msg = log LogType.Information msg
    let inline logWar msg = log LogType.Warning msg
    let inline logErr msg = log LogType.Error msg

    let inline logException ex =
        let logger = new Logger(ServiceProvider.GlobalProvider)
        logger.LogException ex
        ex

    let inline msgbox logType msg =
        let logger = new Logger(ServiceProvider.GlobalProvider)
        logger.MessageBox logType msg |> ignore

    let inline msgboxInf msg = msgbox LogType.Information msg
    let inline msgboxWar msg = msgbox LogType.Warning msg
    let inline msgboxErr msg = msgbox LogType.Error msg