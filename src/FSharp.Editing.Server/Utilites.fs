[<AutoOpen>]
module FSharp.Editing.Server.Utilites

open System

module Logger =
    open Microsoft.FSharp.Core.Printf

    let debug msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [DEBUG] " + x)) msg
    let info msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [INFO] " + x)) msg
    let error msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [ERROR] " + x)) msg
