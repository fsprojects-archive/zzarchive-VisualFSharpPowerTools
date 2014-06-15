module FSharpVSPowerTools.Core.Tests.UnopenedNamespacesResolverTests

open NUnit.Framework
open FSharpVSPowerTools

let (==>) (ns, ident, fullName) res = 
    Entity.tryCreate ns ident fullName 
    |> assertEqual (res |> Option.map (fun (ns, name) -> { Namespace = ns; Name = name }))

[<Test>] 
let ``fully qualified external entities``() =
    (None, "Now", "System.DateTime.Now") ==> Some ("System.DateTime", "Now")
    (None, "Now", "System.Now") ==> Some ("System", "Now")
    (Some "Myns", "Now", "System.Now") ==> Some ("System", "Now")
    (Some "Myns.Nested", "Now", "System.Now") ==> Some ("System", "Now")

[<Test>]
let ``simple entities``() =
    (None, "Now", "Now") ==> None  
    (Some "Myns", "Now", "Now") ==> None

[<Test>]
let ``internal entities``() =
    (Some "Myns", "Now", "Myns.Nested.Now") ==> Some ("Nested", "Now")   
    (Some "Myns.Nested", "Now", "Myns.Nested.Nested2.Now") ==> Some ("Nested2", "Now")
    (Some "Myns.Nested", "Now", "Myns.Nested.Now") ==> None
