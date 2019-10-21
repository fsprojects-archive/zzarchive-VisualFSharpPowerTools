[<AutoOpen>]
module FSharp.Editing.Server.Utilites

open System

module Logger =
    open Microsoft.FSharp.Core.Printf

    let debug msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [DEBUG] " + x)) msg
    let info msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [INFO] " + x)) msg
    let error msg = kprintf (fun x -> Console.WriteLine ((string DateTime.Now) + " [ERROR] " + x)) msg

module Map =
    let addOrUpdate (key: 'k) (newValue: unit -> 'v) (update: 'v -> 'v) (m: Map<'k, 'v>) =
        match m |> Map.tryFind key with
        | Some oldV -> m |> Map.add key (update oldV)
        | None -> m |> Map.add key (newValue())
    