[<AutoOpen>]
module FSharp.Editing.Constants

/// F# project file extension - `.fsproj`
let [<Literal>] fsprojext = ".fsproj"

(* Compiler Flags *)

/// Compiler Flag `--noframework`
let [<Literal>] noframeworkFlag = "--noframework" 
/// Compiler Flag `--debug-`
let [<Literal>] debugFlag       = "--debug-" 
/// Compiler Flag `--optimize-`
let [<Literal>] optimizeFlag    = "--optimize-"
/// Compiler Flag `--tailcalls-`
let [<Literal>] tailcallsFlag   = "--tailcalls-"
