#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.NavigateToIndexTests
#endif

open NUnit.Framework
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open FSharpVSPowerTools.ProjectSystem.Navigation

[<Test>]
let ``Index should return all matching entries``() =
    let b = Index.Builder()
    let items =
        [
            { Name = "foo"; Range = Range.rangeN "a.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "Symb"; Range = Range.rangeN "b.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "SymbolOf"; Range = Range.rangeN "c.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "Symbol"; Range = Range.rangeN "d.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "Symbol"; Range = Range.rangeN "e.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "Symbol"; Range = Range.rangeN "f.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "GetSymbol"; Range = Range.rangeN "g.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
            { Name = "bar"; Range = Range.rangeN "h.fs" 1; IsSignature = false; Kind = NavigableItemKind.Type }
        ]
    b.Add(items)
    let index = b.BuildIndex()
    let results = ResizeArray()
    index.Find("symbol", results.Add)
    assertTrue (results.Count = 5)
    results
    |> Seq.distinctBy (fun (r, _, _, _) -> r.Range.FileName)
    |> Seq.length
    |> (assertTrue << ((=)5))  
