namespace FSharpVSPowerTools.CodeFormatting.Utils

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text

module internal Disposable =
    let create (onDispose: unit -> unit) =
        { new IDisposable with
            member x.Dispose() =
                onDispose() }

module internal Cursor =
    let wait() =
        let currentCursor = System.Windows.Forms.Cursor.Current
        System.Windows.Forms.Cursor.Current <- System.Windows.Forms.Cursors.WaitCursor
        Disposable.create(fun () -> System.Windows.Forms.Cursor.Current <- currentCursor)

module internal TextUtils =
    let getFSharpPos (point: VirtualSnapshotPoint) =
        let containingLine = point.Position.GetContainingLine()
        // F# compiler line numbers start at 1
        let lineNumber = containingLine.LineNumber + 1
        let charIndex = point.Position.Position - containingLine.Start.Position
        Range.mkPos lineNumber charIndex