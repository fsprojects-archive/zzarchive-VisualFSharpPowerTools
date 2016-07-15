[<AutoOpen>]
module FSharp.Editing.VisualStudio.CodeGeneration.Utils

type ISuggestion =
    abstract Text: string
    abstract Invoke: unit -> unit
    abstract NeedsIcon: bool

type SuggestionGroup = ISuggestion list