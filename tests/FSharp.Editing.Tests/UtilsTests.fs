#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#r "../../packages/FsCheck/lib/net45/FsCheck.dll"
#load   "TestHelpers.fs"
        "../../src/FSharpVSPowerTools.Core/Utils.fs"
#else
module FSharp.Editing.Tests.UtilsTests
#endif

open NUnit.Framework
open FsCheck
open FSharp.Editing
open System.Collections.Generic


  //==============//
 //  List Tests  //
//==============//

[<Test; Parallelizable>]
let ``|List| foldi indexes lists properly`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        (xs <> []) ==>
            ((List.foldi (fun _ i _ -> i)) 0 xs = xs.Length-1)


[<Test; Parallelizable>]
let  ``|List| takeWhile pred xs + skipWhile pred xs = xs`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        let takeList, skipList = List.takeWhile ((>)1000) xs, List.skipWhile ((>)1000) xs
        takeList @ skipList = xs
        

[<Test; Parallelizable>]
let  ``|List| recursing with tryhead returns the list`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        let rec loop acc xs = 
            match (List.tryHead xs) ,xs with
            | (Some hd),_::[]   ->  hd::acc
            | (Some hd),_::tl   ->  loop (hd::acc) tl
            | _     , _         ->  acc
        (loop [] xs |> List.rev) = xs


[<Test; Parallelizable>]
let  ``|List| GroupBy doesn't lose elements in the process`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        List.groupBy (string>>String.length) xs 
            |> List.collect snd |> List.length = xs.Length


[<Test; Parallelizable>]
let  ``|List| Groups map back onto their keys`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
    (xs <> []) ==> let keyfn = (string>>String.length) in
        (true, List.groupBy keyfn  xs)
        ||> List.fold (fun eval (key, elms) ->
            match eval with
            | false -> false
            | true  -> List.forall (fun x -> keyfn x = key) elms)


  //==============//
 // Array Tests  //
//==============//


[<Test; Parallelizable>]
let  ``|Array| areEqual``() =
    Check.QuickThrowOnFailure <| fun (x: int[]) (y: int[]) ->
        (x = y) = (Array.areEqual x y)
    
    Assert.AreEqual((null = [||]), (Array.areEqual null [||]))
    Assert.AreEqual(([||] = null), (Array.areEqual [||] null))


[<Test; Parallelizable>]
let  ``|Array| random slice is a sub array``() =
    let rng = System.Random ()
    Check.QuickThrowOnFailure <| 
    fun (array:int[]) ->
        (array <> [||]) ==>
           lazy(let start   = rng.Next(0,array.Length-1)
                let finish  = rng.Next(start,array.Length-1)
                Array.isSubArray (array.[start..finish]) array start = true)


[<Test; Parallelizable>]
let  ``|Array| startsWith respects bounds``() =
    Check.QuickThrowOnFailure <| 
    fun (subEnd: int) (array:int[]) ->
        (subEnd >= 0 && subEnd < array.Length) ==> 
           lazy (Array.startsWith (array.[0..subEnd]) array = true)


[<Test; Parallelizable>]
let  ``|Array| endsWith respects bounds``() =
    Check.QuickThrowOnFailure <| 
    fun (subStart: int) (array:int[]) ->
        (subStart >= 0 && subStart < array.Length) ==> 
           lazy (Array.endsWith (array.[subStart..array.Length-1]) array = true)


[<Test; Parallelizable>]
let  ``|Array| Distinct has no duplicates``() =
    Check.QuickThrowOnFailure <|
        fun (array:string []) ->
            (Array.distinct array).Length = (HashSet<_> array).Count


[<Test; Parallelizable>]
let  ``|Array| Distinct is the same size or smaller than its input`` () =
    Check.QuickThrowOnFailure <|
        fun (array:string []) ->
            (Array.distinct array).Length <= Array.length array


[<Test; Parallelizable>]
let  ``|Array| DistinctBy has no duplicates``() =
    Check.QuickThrowOnFailure <|
    fun (array:int []) ->
        (Array.distinctBy string array).Length = (HashSet<_> array).Count
        

[<Test; Parallelizable>]
let  ``|Array| DistinctBy is the same size or smaller than its input`` () =
    Check.QuickThrowOnFailure <|
    fun (array:int[]) ->
        (Array.distinctBy string array).Length <= Array.length array


[<Test; Parallelizable>]
let  ``|Array| foldi indexes arrays properly`` () =
    Check.QuickThrowOnFailure <|
    fun (array:int[]) ->
        (array <> [||]) ==>
            ((Array.foldi (fun _ i _ -> i)) 0 array = array.Length-1)


[<Test; Parallelizable>]
let  ``|Array| takeWhile pred xs + skipWhile pred xs = xs`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        let takeList, skipList = Array.takeWhile ((>)1000) xs, Array.skipWhile ((>)1000) xs
        Array.append takeList  skipList = xs
        

[<Test; Parallelizable>]
let  ``|Array| Groups map back onto their keys`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
    (xs <> [||]) ==> let keyfn = (string>>String.length) in
        (true, Array.groupBy keyfn  xs)
        ||> Array.fold (fun eval (key, elms) ->
            match eval with
            | false -> false
            | true  -> Array.forall (fun x -> keyfn x = key) elms)


[<Test; Parallelizable>]
let  ``|Array| Groupsby doesn't lose elements`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        Array.groupBy (string>>String.length) xs 
        |> Array.map (snd>>Array.length)|>Array.sum = xs.Length


[<Test; Parallelizable>]
let  ``|Array| filterMap = filter |> map`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        let filter x = x<>0 || x%4<>0
        let mapfn = float>>string
        Array.filterMap filter mapfn xs = (xs |> Array.filter filter |> Array.map mapfn)


