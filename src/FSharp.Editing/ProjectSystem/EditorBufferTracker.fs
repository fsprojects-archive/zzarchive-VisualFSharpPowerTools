namespace FSharp.Editing.ProjectSystem

open System
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range


[<NoComparison>]
type EditorBuffer =
    {   Text           : string
        Range          : range 
        IsDirty        : bool
        Encoding       : Encoding
        LastChangeTime : DateTime
        ViewCount      : int 
    }
    static member Create text range isDirty encoding lastChangeTime = 
        {   Text           = text
            Range          = range
            IsDirty        = isDirty
            Encoding       = encoding
            LastChangeTime = lastChangeTime
            ViewCount      = 1 
        }


type IBufferTracker =
    abstract MapEditorBuffers    : (KeyValuePair<string, EditorBuffer> -> 'a) -> seq<'a>
    abstract TryFindEditorBuffer : string -> EditorBuffer option
    abstract TryGetBufferText    : string -> string option
    abstract BufferChanged       : string IEvent
    abstract BufferClosed        : string IEvent

