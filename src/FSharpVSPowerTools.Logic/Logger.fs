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

    [<Literal>]
    let source = "F# Power Tools"

    let getEntryType logType =
        let value =
            match logType with
            | LogType.Information -> __ACTIVITYLOG_ENTRYTYPE.ALE_INFORMATION
            | LogType.Warning -> __ACTIVITYLOG_ENTRYTYPE.ALE_WARNING
            | LogType.Error -> __ACTIVITYLOG_ENTRYTYPE.ALE_ERROR
        Convert.ToUInt32(value) :> UInt32

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
        |> Option.iter (fun s -> s.LogEntry((getEntryType logType), source, message) |> ignore)

    member x.LogExceptionWithMessage message (e:Exception) =
        let message =
            sprintf "Message: %s\nException Message: %s\nStack Trace: %s" message e.Message e.StackTrace
        x.Log LogType.Error message

    member x.LogException (e:Exception) =
        let message = sprintf "Exception Message: %s\mStart Trace: %s" e.Message e.StackTrace
        x.Log LogType.Error message
                
    member x.MessageBox title message logType =
        let icon = getIcon logType
        let service = getShellService()
        let result = 0
        service.ShowMessageBox(0u, ref Guid.Empty, title, message, "", 0u, OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST, icon, 0, ref result)