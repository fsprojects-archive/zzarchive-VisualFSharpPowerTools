namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio
open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell

/// Encapsulate listening for shell events.
[<Export>]
type ShellEventListener () as self =
    let shellService = Package.GetService<SVsShell, IVsShell>() |> Option.ofNull
    let broadcastEventCookie = ref 0u
    do shellService 
       |> Option.iter (fun shell ->
            ErrorHandler.ThrowOnFailure(shell.AdviseBroadcastMessages(self, broadcastEventCookie)) |> ignore)
    let themeChanged = DelegateEvent<EventHandler>()

    static let [<Literal>] WM_SYSCOLORCHANGE = 0x0015u

    [<CLIEvent>]
    member __.ThemeChanged = themeChanged.Publish

    interface IVsBroadcastMessageEvents with
        member __.OnBroadcastMessage(msg: uint32, _wParam: nativeint, _lParam: nativeint): int = 
            if msg = WM_SYSCOLORCHANGE then
                themeChanged.Trigger([|box self; box EventArgs.Empty|])
            VSConstants.S_OK
        
    interface IDisposable with
        member __.Dispose() = 
            shellService 
            |> Option.iter (fun shell -> 
                 shell.UnadviseBroadcastMessages(!broadcastEventCookie) |> ignore
                 broadcastEventCookie := 0u)
 