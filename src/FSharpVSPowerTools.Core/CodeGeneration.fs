[<AutoOpen>]
module FSharpVSPowerTools.CodeGeneration.Utils

open System
open System.IO
open System.CodeDom.Compiler

type internal ColumnIndentedTextWriter() =
    let stringWriter = new StringWriter()
    let indentWriter = new IndentedTextWriter(stringWriter, " ")

    member x.Write(s: string, [<ParamArray>] objs: obj []) =
        indentWriter.Write(s, objs)

    member x.WriteLine(s: string, [<ParamArray>] objs: obj []) =
        indentWriter.WriteLine(s, objs)

    member x.Indent i = 
        indentWriter.Indent <- indentWriter.Indent + i

    member x.Unindent i = 
        indentWriter.Indent <- max 0 (indentWriter.Indent - i)

    member x.Writer = 
        indentWriter :> TextWriter

    member x.Dump() =
        indentWriter.InnerWriter.ToString()

    interface IDisposable with
        member x.Dispose() =
            stringWriter.Dispose()
            indentWriter.Dispose()

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let (|IndexerArg|) = function
    | SynIndexerArg.Two(e1, e2) -> [e1; e2]
    | SynIndexerArg.One e -> [e]

let (|IndexerArgList|) xs =
    List.collect (|IndexerArg|) xs

let hasAttribute<'T> (attrs: seq<FSharpAttribute>) =
    attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'T>.Name)