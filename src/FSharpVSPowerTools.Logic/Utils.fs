[<AutoOpen>]
module FSharpVSPowerTools.Refactoring.Utils

type ISuggestion =
    abstract Text: string
    abstract Invoke: unit -> unit
    abstract NeedsIcon: bool

type SuggestionGroup = ISuggestion list