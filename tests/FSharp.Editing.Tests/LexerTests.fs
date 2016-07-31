#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharp.Editing.Tests.SymbolParserTests
#endif

open NUnit.Framework
open System.IO
open FSharp.Editing

[<Literal>]
let dataFolderName = __SOURCE_DIRECTORY__ + "/../data/"
type dataFolder = FSharp.Management.FileSystem<dataFolderName>
 
let fileName = dataFolder.``LanguageServiceSampleFile.fs``
let source = File.ReadAllText fileName

let args = LanguageServiceTestHelper.args

let checkGetSymbol line col lineStr expected =
    Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy args Lexer.queryLexState
    |> Option.map (fun { Line = line; LeftColumn = leftCol; RightColumn = rightCol; Text = text; Kind = kind } ->
        text, (line, leftCol), (line, rightCol), kind)
    |> assertEqual expected

[<Test; Parallelizable>]
let ``should find operators``() =
    checkGetSymbol 693 10 "    let (>>=) x y = ()" (Some (">>=", (693, 9), (693, 12), Operator))
    checkGetSymbol 695 12 "    let (>~>>) x y = ()" (Some (">~>>", (695, 9), (695, 13), Operator))
    checkGetSymbol 701 12 "    let ( >>. ) x y = x" (Some (">>.", (701, 10), (701, 13), Operator))
    checkGetSymbol 704 15 "    let x = 1 >>. ws >>. 2 >>. ws" (Some (">>.", (704, 14), (704, 17), Operator))
    checkGetSymbol 728 9  "    M.N.(+.) 1 2" (Some ("+.", (728, 9), (728, 11), Operator))

[<Test; Parallelizable>]
let ``should find identifiers``() =
    checkGetSymbol 703 8  "    let ws x = x" (Some ("ws", (703, 8), (703, 10), Ident))
    checkGetSymbol 702 24 "    1 >>. 2 >>. 3 |> ignore" (Some ("ignore", (702, 21), (702, 27), Ident))
    
    checkGetSymbol 722 14 "    Nested.``long name``()" (Some ("``long name``", (722, 11), (722, 24), Ident))

    checkGetSymbol 582 59 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("map", (582, 57), (582, 60), Ident))

    checkGetSymbol 582 48 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("Parallel", (582, 48), (582, 56), Ident))

    checkGetSymbol 582 56 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("Parallel", (582, 48), (582, 56), Ident))

    checkGetSymbol 773 17 "    refValue := !refValue + 1" (Some ("refValue", (773, 17), (773, 25), Ident))
    checkGetSymbol 773 25 "    refValue := !refValue + 1" (Some ("refValue", (773, 17), (773, 25), Ident))
    checkGetSymbol 897 13 "    let _ = Type1<_,_>.Member1()" (Some ("Type1", (897, 12), (897, 17), Ident))

[<Test; Parallelizable>]
let ``should find generic parameters``() =
    checkGetSymbol 707 12 "    type C<'a> = C of 'a" (Some ("'a", (707, 11), (707, 13), GenericTypeParameter))
    checkGetSymbol 707 22 "    type C<'a> = C of 'a" (Some ("'a", (707, 22), (707, 24), GenericTypeParameter))

[<Test; Parallelizable>]
let ``should find statically resolved type parameters``() =
    checkGetSymbol 730 22 "    let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        (Some ("^T", (730, 22), (730, 24), StaticallyResolvedTypeParameter))

[<Test; Parallelizable>]
let ``should find active patterns``() =
    checkGetSymbol 744 10 "    let (|A|Bb|Ccc|) (x: int) =" (Some ("A", (744, 10), (744, 11), Ident))
    checkGetSymbol 744 12 "    let (|A|Bb|Ccc|) (x: int) =" (Some ("Bb", (744, 12), (744, 14), Ident))

let checkGetLongSymbol line col lineStr expected =
    Lexer.getSymbol lineStr line col lineStr SymbolLookupKind.ByLongIdent args Lexer.queryLexState
    |> Option.map (fun { Line = line; LeftColumn = leftCol; RightColumn = rightCol; Text = text; Kind = kind } ->
        text, (line, leftCol), (line, rightCol), kind)
    |> assertEqual expected

[<Test; Parallelizable>]
let ``should find long identifiers``() =
    checkGetLongSymbol 0 11 "open A.B.C.D" (Some ("A.B.C.D", (0, 5), (0, 12), Ident))
    checkGetLongSymbol 0 38 "type t = Microsoft.FSharp.Quotations.Expr<bool>" (Some ("Microsoft.FSharp.Quotations.Expr", (0, 9), (0, 41), Ident))
    checkGetLongSymbol 0 20 "open Microsoft.FSharp.Quotations" (Some ("Microsoft.FSharp", (0, 5), (0, 21), Ident))
    
[<Test; Parallelizable>]
let ``should find long identifiers up until the dots``() =
    checkGetLongSymbol 0 6 "open A.B.C.D" (Some ("A", (0, 5), (0, 6), Ident))