namespace FSharpVSPowerTools.Core

[<RequireQualifiedAccess>]
module Option =
    open System

    let inline ofNull value =
        if obj.ReferenceEquals(value, null) then None else Some value

    let inline ofNullable (value : Nullable<'a>) =
        if value.HasValue then Some value.Value else None

    let inline toNullable (value : 'a option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()
    
[<AutoOpen>]
module Debug =
    let inline debug msg = Printf.kprintf System.Diagnostics.Debug.WriteLine msg

