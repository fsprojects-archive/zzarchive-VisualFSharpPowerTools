namespace FSharpVSPowerTools.Core

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