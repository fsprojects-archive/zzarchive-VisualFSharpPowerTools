#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharp.Editing.Tests.NavigateToIndexTests
#endif

open NUnit.Framework
open FSharp.Editing
open FSharp.Editing.Navigation

[<Test>]
let ``Index should return all matching entries``() =
    let b = Index.Builder()
    let item filePath name =
        { FilePath = filePath
          Name = name
          Range = { Start = { Row = 1; Col = 1 }; End = { Row = 1; Col = 1 }}
          IsSignature = false
          Kind = NavigableItemKind.Type }
    let items =
        [ item "a.fs" "foo"
          item "b.fs" "Symb"
          item "c.fs" "SymbolOf"
          item "d.fs" "Symbol"
          item "e.fs" "Symbol"
          item "f.fs" "Symbol"
          item "g.fs" "GetSymbol"
          item "h.fs" "bar" ]

    b.Add(items)
    let index = b.BuildIndex()
    let results = ResizeArray()
    index.Find("symbol", results.Add)
    assertTrue (results.Count = 5)
    results
    |> Seq.distinctBy (fun (r, _, _, _) -> r.FilePath)
    |> Seq.length
    |> (assertTrue << ((=)5))  
