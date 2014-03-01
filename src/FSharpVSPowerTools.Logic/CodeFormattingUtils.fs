namespace FSharpVSPowerTools.CodeFormatting.Utils

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text

module internal Disposable =
    let Create (onDispose: unit -> unit) =
        { new IDisposable with
            member x.Dispose() =
                onDispose() }

module internal Cursor =
    let Wait (): IDisposable =
        let currentCursor = System.Windows.Forms.Cursor.Current
        System.Windows.Forms.Cursor.Current <- System.Windows.Forms.Cursors.WaitCursor
        Disposable.Create(fun () -> System.Windows.Forms.Cursor.Current <- currentCursor)

module internal TextUtils =
    let GetFSharpPos (point: VirtualSnapshotPoint): Range.pos =
        let containingLine = point.Position.GetContainingLine()
        // F# compiler line numbers start at 1
        let lineNumber = containingLine.LineNumber + 1
        let charIndex = point.Position.Position - containingLine.Start.Position
        Range.mkPos lineNumber charIndex