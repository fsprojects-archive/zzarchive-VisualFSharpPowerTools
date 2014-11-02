module FSharpVSPowerTools.Refactoring.RenameSuggestions

open System.Text.RegularExpressions
open System
open FSharp.Control
open NHunspell

type Kind =
| Type
| Function
| Variable

type Suggestion = {
    Term: string
    Priority: int
    // TODO: Category
}

let veryLowPrio term = { Term = term; Priority = 10000 }
let lowPrio term = { Term = term; Priority = 1000 }
let mediumPrio term = { Term = term; Priority = 100 }
let highPrio term = { Term = term; Priority = 1 }

let suggestIndexNames name =
    let indexRegex = Regex("[i](\d*)")
    [let indexMatch = indexRegex.Match(name)
     if indexMatch.Success && indexMatch.Value = name then
         yield lowPrio("index" + indexMatch.Groups.[1].Value)
     if name = "index" then
         yield lowPrio "i"
     if name = "i2" || name = "index2" then
         yield lowPrio "j"
     if name = "i3" || name = "index3" then
         yield lowPrio "k" ]

let lower (s:string) = s.[0].ToString().ToLower() + if s.Length > 1 then s.Substring(1) else ""

let upper (s:string) = s.[0].ToString().ToUpper() + if s.Length > 1 then s.Substring(1) else ""

let splitInParts (name:string) =
    [let last = ref 0
     for i in 0..name.Length-1 do
        if Char.IsUpper(name.[i]) && i <> 0 then
            yield name.Substring(!last,i - !last)
            last := i
        if i = name.Length - 1 then
            yield name.Substring(!last)]

let internal thes = lazy(new MyThes("th_en_us_new.dat"))

let findSynonyms word =
    let thes = thes.Force()
    let tr = thes.Lookup(word)

    [if tr <> null && tr.Meanings <> null then
        for meaning in tr.Meanings do
            if meaning.Synonyms <> null then
                for s in meaning.Synonyms do
                    if s <> word && not (s.Contains " ") && not (s.Contains "-") then
                        if Char.IsLower(word.[0]) then yield lower s else yield upper s]
    |> Set.ofList

let private createSuggestions' parts =    
    let path = IO.FileInfo(typeof<Kind>.Assembly.Location).Directory.FullName
    if Hunspell.NativeDllPath <> path then
        Hunspell.NativeDllPath <- path
    use hunspell = new Hunspell("en_us.aff", "en_us.dic")

    let rec loop parts =
        match parts with
        | [] -> []
        | x::rest ->
            let laterParts = loop rest
            let prepend element =
                [if laterParts = [] then
                     yield [element]
                 for l in laterParts do
                     yield element :: l]

            if hunspell.Spell x then
                // word is correct - try to find synonyms, singluar and plural
                [yield! prepend x

                 if rest = [] then
                     yield [Pluralizer.toPlural x]
                     yield [Pluralizer.toSingular x]

                 if x.Length > 4 then
                     for w in findSynonyms x do
                         yield! prepend w]
            else
                // word is not written correctly try to correct
                [for s in hunspell.Suggest x do
                     yield! prepend s]

    loop parts
    |> List.map (fun s -> highPrio (String.Concat(s)))
                

let createSuggestions name =
    splitInParts name
    |> createSuggestions'

let getSubParts (name:string) parts =
    let rec getLaterParts parts =
        match parts with
        | [] -> []
        | x::rest ->
            let laterParts = getLaterParts rest
            [if laterParts = [] then
                 yield [x]
             for l in laterParts do
                 yield l
                 yield x :: l]

    getLaterParts parts 
    |> List.map String.Concat 
    |> List.filter (fun s -> name.Contains(s))

let suggest (kind:Kind) (name:string) =
    let parts = splitInParts name
    let suggestions =
        match parts with
        | [_] -> createSuggestions' [name]
        | _ ->  createSuggestions' [name] @ createSuggestions' parts

    match kind with
    | Variable -> highPrio name :: suggestIndexNames name
    | Type
    | Function -> [highPrio name]
    |> List.append (getSubParts name parts |> List.map veryLowPrio)
    |> List.append suggestions
    |> List.map (fun x -> 
                match kind with
                | Kind.Variable -> { x with Term = lower x.Term }
                | Kind.Type -> { x with Term = upper x.Term }
                | Kind.Function -> x)
    |> List.filter (fun s -> s.Term <> name)
    |> Seq.distinct