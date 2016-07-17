namespace FSharp.Editing.Server

open FSharp.Editing

[<NoComparison>]
type Context =
    { Solution: Solution
      LanguageService: LanguageService }

