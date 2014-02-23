namespace FSharpVSPowerTools

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Debug =
    let inline debug msg = Printf.kprintf System.Diagnostics.Debug.WriteLine msg

