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


let [<Test>] ``|List| foldi indexes lists properly`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        (xs <> []) ==>
            ((List.foldi (fun _ i _ -> i)) 0 xs = xs.Length-1)


let [<Test>] ``|List| takeWhile pred xs + skipWhile pred xs = xs`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        let takeList, skipList = List.takeWhile ((>)1000) xs, List.skipWhile ((>)1000) xs
        takeList @ skipList = xs
        

let [<Test>] ``|List| recursing with tryhead returns the list`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        let rec loop acc xs = 
            match (List.tryHead xs) ,xs with
            | (Some hd),_::[]   ->  hd::acc
            | (Some hd),_::tl   ->  loop (hd::acc) tl
            | _     , _         ->  acc
        (loop [] xs |> List.rev) = xs


let [<Test>] ``|List| GroupBy doesn't lose elements in the process`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int list) ->
        List.groupBy (string>>String.length) xs 
            |> List.collect snd |> List.length = xs.Length


let [<Test>] ``|List| Groups map back onto their keys`` () =
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


let [<Test>] ``|Array| areEqual``() =
    Check.QuickThrowOnFailure <| fun (x: int[]) (y: int[]) ->
        (x = y) = (Array.areEqual x y)
    
    Assert.AreEqual((null = [||]), (Array.areEqual null [||]))
    Assert.AreEqual(([||] = null), (Array.areEqual [||] null))


let [<Test>] ``|Array| random slice is a sub array``() =
    let rng = System.Random ()
    Check.QuickThrowOnFailure <| 
    fun (array:int[]) ->
        (array <> [||]) ==>
           lazy(let start   = rng.Next(0,array.Length-1)
                let finish  = rng.Next(start,array.Length-1)
                Array.isSubArray (array.[start..finish]) array start = true)


let [<Test>] ``|Array| startsWith respects bounds``() =
    Check.QuickThrowOnFailure <| 
    fun (subEnd: int) (array:int[]) ->
        (subEnd >= 0 && subEnd < array.Length) ==> 
           lazy (Array.startsWith (array.[0..subEnd]) array = true)


let [<Test>] ``|Array| endsWith respects bounds``() =
    Check.QuickThrowOnFailure <| 
    fun (subStart: int) (array:int[]) ->
        (subStart >= 0 && subStart < array.Length) ==> 
           lazy (Array.endsWith (array.[subStart..array.Length-1]) array = true)


let [<Test>] ``|Array| Distinct has no duplicates``() =
    Check.QuickThrowOnFailure <|
        fun (array:string []) ->
            (Array.distinct array).Length = (HashSet<_> array).Count


let [<Test>] ``|Array| Distinct is the same size or smaller than its input`` () =
    Check.QuickThrowOnFailure <|
        fun (array:string []) ->
            (Array.distinct array).Length <= Array.length array


let [<Test>] ``|Array| DistinctBy has no duplicates``() =
    Check.QuickThrowOnFailure <|
    fun (array:int []) ->
        (Array.distinctBy string array).Length = (HashSet<_> array).Count
        

let [<Test>] ``|Array| DistinctBy is the same size or smaller than its input`` () =
    Check.QuickThrowOnFailure <|
    fun (array:int[]) ->
        (Array.distinctBy string array).Length <= Array.length array


let [<Test>] ``|Array| foldi indexes arrays properly`` () =
    Check.QuickThrowOnFailure <|
    fun (array:int[]) ->
        (array <> [||]) ==>
            ((Array.foldi (fun _ i _ -> i)) 0 array = array.Length-1)


let [<Test>] ``|Array| takeWhile pred xs + skipWhile pred xs = xs`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        let takeList, skipList = Array.takeWhile ((>)1000) xs, Array.skipWhile ((>)1000) xs
        Array.append takeList  skipList = xs
        

let [<Test>] ``|Array| Groups map back onto their keys`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
    (xs <> [||]) ==> let keyfn = (string>>String.length) in
        (true, Array.groupBy keyfn  xs)
        ||> Array.fold (fun eval (key, elms) ->
            match eval with
            | false -> false
            | true  -> Array.forall (fun x -> keyfn x = key) elms)


let [<Test>] ``|Array| Groupsby doesn't lose elements`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        Array.groupBy (string>>String.length) xs 
        |> Array.map (snd>>Array.length)|>Array.sum = xs.Length


let [<Test>] ``|Array| filterMap = filter |> map`` () =
    Check.QuickThrowOnFailure <|
    fun (xs:int []) ->
        let filter x = x<>0 || x%4<>0
        let mapfn = float>>string
        Array.filterMap filter mapfn xs = (xs |> Array.filter filter |> Array.map mapfn)


