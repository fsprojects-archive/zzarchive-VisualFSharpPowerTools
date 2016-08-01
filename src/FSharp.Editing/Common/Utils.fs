namespace FSharp.Editing

open System
open System.Diagnostics

type FileName = string
type FilePath = string

[<Measure>] type FCS

type Point<[<Measure>]'t> = { Line : int; Column : int }
type Range<[<Measure>]'t> = { Start : Point<'t>; End: Point<'t> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Point =
    let make line column : Point<'t> = { Line = line; Column = column }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    let make startLine startColumn endLine endColumn : Range<'t> =
        { Start = Point.make startLine startColumn
          End = Point.make endLine endColumn }
    
type CurrentLine<[<Measure>]'t> = 
    { Line: string
      File: FileName; Range: Range<'t> }
    member x.EndLine = x.Range.End.Line 

[<NoComparison>]
type PointInDocument<[<Measure>]'t> = 
    { Point: Point<'t>
      Line: string
      Document: string
      File: FileName }
    member x.LineIndex = x.Point.Line
    member x.ColumnIndex = x.Point.Column
    member x.CurrentLine : Lazy<CurrentLine<'t>> = 
        lazy
          { Line = x.Line
            File = x.File
            Range = Range.make x.LineIndex x.ColumnIndex x.LineIndex x.Line.Length }

[<AutoOpen>]
module Prelude =
    /// obj.ReferenceEquals
    let inline (==) a b = obj.ReferenceEquals(a, b)
    /// LanguagePrimitives.PhysicalEquality
    let inline (===) a b = LanguagePrimitives.PhysicalEquality a b
    let inline debug msg = Printf.kprintf Debug.WriteLine msg
    let inline fail msg = Printf.kprintf Debug.Fail msg
    let inline isNull v = match v with | null -> true | _ -> false
    let inline isNotNull v = not (isNull v)
    let inline dispose (disposable:#IDisposable) = disposable.Dispose ()

    let inline Ok a = Choice1Of2 a
    let inline Fail a = Choice2Of2 a
    let inline (|Ok|Fail|) a = a

      
    
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    let toReadOnlyCollection (xs: _ seq) = ResizeArray(xs).AsReadOnly()

[<RequireQualifiedAccess>]
module List =
    /// Fold over the list passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (xs: 'T list) =
        match xs with 
        | [] -> state
        | _ -> 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
            let rec loop idx s xs = 
                match xs with 
                | [] -> s
                | h::t -> loop (idx+1) (f.Invoke(s,idx,h)) t
            loop 0 state xs

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    let inline private checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
    /// ~2x faster for floats
    /// ~0.8x slower for ints
    let inline areEqual (xs: 'T []) (ys: 'T []) =
        match xs, ys with
        | null, null -> true
        | [||], [||] -> true
        | null, _ | _, null -> false
        | _ when xs.Length <> ys.Length -> false
        | _ ->
            let mutable break' = false
            let mutable i = 0
            let mutable result = true
            while i < xs.Length && not break' do
                if xs.[i] <> ys.[i] then 
                    break' <- true
                    result <- false
                i <- i + 1
            result

    /// check if subArray is found in the wholeArray starting 
    /// at the provided index
    let inline isSubArray (subArray: 'T []) (wholeArray:'T []) index = 
        if isNull subArray || isNull wholeArray then false
        elif subArray.Length = 0 then true
        elif subArray.Length > wholeArray.Length then false
        elif subArray.Length = wholeArray.Length then areEqual subArray wholeArray else
        let rec loop subidx idx =
            if subidx = subArray.Length then true 
            elif subArray.[subidx] = wholeArray.[idx] then loop (subidx+1) (idx+1) 
            else false
        loop 0 index

    /// Returns true if one array has another as its subset from index 0.
    let startsWith (prefix: _ []) (whole: _ []) =
        isSubArray prefix whole 0

    /// Returns true if one array has trailing elements equal to another's.
    let endsWith (suffix: _ []) (whole: _ []) =
        isSubArray suffix whole (whole.Length-suffix.Length)

    /// Returns a new array with an element replaced with a given value.
    let replace index value (array: _ []) =
        checkNonNull "array" array
        if index >= array.Length then raise (IndexOutOfRangeException "index")
        let res = Array.copy array
        res.[index] <- value
        res

    /// Returns all heads of a given array.
    /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
    let heads (array: 'T []) =
        checkNonNull "array" array
        let res = Array.zeroCreate<'T[]> array.Length
        for i = array.Length - 1 downto 0 do
            res.[i] <- array.[0..i]
        res

    /// Fold over the array passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then state else
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state:'State = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

    /// pass an array byref to reverse it in place
    let revInPlace (array: 'T []) =
        checkNonNull "array" array
        if areEqual array [||] then () else
        let arrlen, revlen = array.Length-1, array.Length/2 - 1
        for idx in 0 .. revlen do
            let t1 = array.[idx] 
            let t2 = array.[arrlen-idx]
            array.[idx] <- t2
            array.[arrlen-idx] <- t1

    /// Map all elements of the array that satisfy the predicate
    let filterMap predicate mapfn (array: 'T [])  =
        checkNonNull "array" array
        if array.Length = 0 then [||] else
        let result = Array.zeroCreate array.Length
        let mutable count = 0
        for elm in array do
            if predicate elm then 
               result.[count] <- mapfn elm
               count <- count + 1
        if count = 0 then [||] else
        result.[0..count-1]

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the given function returns
    /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "array" array
        
        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()
    
            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            array
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value)
    
            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray ()

    let splitByChunks (chunkSizes : int[]) (arr : 'T[]) =
        let rec loop (chunks : int[]) (arr : 'T[]) acc =
            match chunks, arr with
            | [||], _ 
            | _, [||] -> acc
            | _ ->
                let chunk = min chunks.[0] arr.Length
                loop chunks.[1 .. ] arr.[chunk .. ] (arr.[0..(chunk-1)] :: acc)

        loop chunkSizes arr []
        |> Array.ofList
        |> Array.rev

    let toShortHexString (bytes: byte[]) =
        let length = bytes.Length
        let chars = Array.zeroCreate length
        for i in 0..length/2-1 do
            let b1 = byte (bytes.[i] >>> 4)
            chars.[i * 2] <- if b1 > 9uy then char (b1 + 87uy) else char (b1 + 0x30uy)
            let b2 = byte (bytes.[i] &&& 0xFuy)
            chars.[i * 2 + 1] <- if b2 > 9uy then char (b2 + 87uy) else char (b2 + 0x30uy)
        String chars

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
    let inline ofNull value =
        if obj.ReferenceEquals(value, null) then None else Some value

    let inline ofNullable (value: Nullable<'T>) =
        if value.HasValue then Some value.Value else None

    let inline toNullable (value: 'T option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()

    let inline attempt (f: unit -> 'T) = try Some <| f() with _ -> None

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElse v =
        function
        | Some x -> x
        | None -> v

    /// Gets the option if Some x, otherwise the supplied default value.
    let inline orElse v =
        function
        | Some x -> Some x
        | None -> v

    /// Gets the value if Some x, otherwise try to get another value by calling a function
    let inline getOrTry f =
        function
        | Some x -> x
        | None -> f()

    /// Gets the option if Some x, otherwise try to get another value
    let inline orTry f =
        function
        | Some x -> Some x
        | None -> f()

    /// Some(Some x) -> Some x | None -> None
    let inline flatten x =
        match x with
        | Some x -> x
        | None -> None

    let inline toList x =
        match x with
        | Some x -> [x]
        | None -> []

    let inline iterElse someAction noneAction opt =
        match opt with
        | Some x -> someAction x
        | None   -> noneAction ()
    
// Async helper functions copied from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/ControlCollections.Async.fs
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    /// Transforms an Async value using the specified function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> =
        async {
            // Get the input value.
            let! x = value
            // Apply the mapping function and return the result.
            return mapping x
        }

    // Transforms an Async value using the specified Async function.
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Async<'U>) (value : Async<'T>) : Async<'U> =
        async {
            // Get the input value.
            let! x = value
            // Apply the binding function and return the result.
            return! binding x
        }

    [<RequireQualifiedAccess>]    
    module Array =
        /// Async implementation of Array.map.
        let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async { // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.mapi.
        let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async {
                // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping i array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.exists.
        let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
            let len = Array.length array
            let rec loop i =
                async {
                    if i >= len then
                        return false
                    else
                        let! found = predicate array.[i]
                        if found then
                            return true
                        else
                            return! loop (i + 1)
                }
            loop 0

    [<RequireQualifiedAccess>]
    module List =
        let rec private mapImpl (mapping, mapped : 'U list, pending : 'T list) =
            async {
                match pending with
                | [] ->
                    // Reverse the list of mapped values before returning it.
                    return List.rev mapped

                | el :: pending ->
                    // Apply the current list element to the mapping function.
                    let! mappedEl = mapping el

                    // Cons the result to the list of mapped values, then continue
                    // mapping the rest of the pending list elements.
                    return! mapImpl (mapping, mappedEl :: mapped, pending)
                }

        /// Async implementation of List.map.
        let map (mapping : 'T -> Async<'U>) (list : 'T list) : Async<'U list> =
            mapImpl (mapping, [], list)



/// Maybe computation expression builder, copied from ExtCore library
/// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
[<Sealed>]
type MaybeBuilder () =
    // 'T -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Return value: 'T option =
        Some value

    // M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.ReturnFrom value: 'T option =
        value

    // unit -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Zero (): unit option =
        Some ()     // TODO: Should this be None?

    // (unit -> M<'T>) -> M<'T>
    [<DebuggerStepThrough>]
    member __.Delay (f: unit -> 'T option): 'T option =
        f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Combine (r1, r2: 'T option): 'T option =
        match r1 with
        | None ->
            None
        | Some () ->
            r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    [<DebuggerStepThrough>]
    member inline __.Bind (value, f: 'T -> 'U option): 'U option =
        Option.bind f value

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    [<DebuggerStepThrough>]
    member __.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
        try body resource
        finally
            if not <| obj.ReferenceEquals (null, box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member x.While (guard, body: _ option): _ option =
        if guard () then
            // OPTIMIZE: This could be simplified so we don't need to make calls to Bind and While.
            x.Bind (body, (fun () -> x.While (guard, body)))
        else
            x.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    [<DebuggerStepThrough>]
    member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
        // OPTIMIZE: This could be simplified so we don't need to make calls to Using, While, Delay.
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (
                enum.MoveNext,
                x.Delay (fun () ->
                    body enum.Current)))

[<Sealed>]
type AsyncMaybeBuilder () =
    [<DebuggerStepThrough>]
    member __.Return value : Async<'T option> = Some value |> async.Return

    [<DebuggerStepThrough>]
    member __.ReturnFrom value : Async<'T option> = value

    [<DebuggerStepThrough>]
    member __.ReturnFrom (value: 'T option) : Async<'T option> = async.Return value

    [<DebuggerStepThrough>]
    member __.Zero () : Async<unit option> =
        Some () |> async.Return

    [<DebuggerStepThrough>]
    member __.Delay (f : unit -> Async<'T option>) : Async<'T option> = f ()

    [<DebuggerStepThrough>]
    member __.Combine (r1, r2 : Async<'T option>) : Async<'T option> =
        async {
            let! r1' = r1
            match r1' with
            | None -> return None
            | Some () -> return! r2
        }

    [<DebuggerStepThrough>]
    member __.Bind (value: Async<'T option>, f : 'T -> Async<'U option>) : Async<'U option> =
        async {
            let! value' = value
            match value' with
            | None -> return None
            | Some result -> return! f result
        }

    [<DebuggerStepThrough>]
    member __.Bind (value: 'T option, f : 'T -> Async<'U option>) : Async<'U option> =
        async {
            match value with
            | None -> return None
            | Some result -> return! f result
        }

    [<DebuggerStepThrough>]
    member __.Using (resource : ('T :> IDisposable), body : _ -> Async<_ option>) : Async<_ option> =
        try body resource
        finally 
            if isNotNull resource then resource.Dispose ()

    [<DebuggerStepThrough>]
    member x.While (guard, body : Async<_ option>) : Async<_ option> =
        if guard () then
            x.Bind (body, (fun () -> x.While (guard, body)))
        else
            x.Zero ()

    [<DebuggerStepThrough>]
    member x.For (sequence : seq<_>, body : 'T -> Async<unit option>) : Async<_ option> =
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (enum.MoveNext, x.Delay (fun () -> body enum.Current)))

    [<DebuggerStepThrough>]
    member inline __.TryWith (computation : Async<'T option>, catchHandler : exn -> Async<'T option>) : Async<'T option> =
            async.TryWith (computation, catchHandler)

    [<DebuggerStepThrough>]
    member inline __.TryFinally (computation : Async<'T option>, compensation : unit -> unit) : Async<'T option> =
            async.TryFinally (computation, compensation)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncMaybe =
    let inline liftAsync (async : Async<'T>) : Async<_ option> =
        async |> Async.map Some

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pervasive =
    open System.Threading

// Redirect debug output to F# Interactive for debugging purpose.
// It requires adding '-d:DEBUG' setting in F# Interactive Options.
#if INTERACTIVE
    Debug.Listeners.Add(new TextWriterTraceListener(System.Console.Out)) |> ignore
    Debug.AutoFlush <- true
#endif

    let maybe = MaybeBuilder()
    let asyncMaybe = AsyncMaybeBuilder()
    
    let tryCast<'T> (o: obj): 'T option = 
        match o with
        | null -> None
        | :? 'T as a -> Some a
        | _ -> 
            debug "Cannot cast %O to %O" (o.GetType()) typeof<'T>.Name
            None

    /// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
    /// Not yet sure if this works for scripts.
    let fakeDateTimeRepresentingTimeLoaded x = DateTime(abs (int64 (match x with null -> 0 | _ -> x.GetHashCode())) % 103231L)
    
    let synchronize f = 
        let ctx = SynchronizationContext.Current
        
        let thread = 
            match ctx with
            | null -> null // saving a thread-local access
            | _ -> Thread.CurrentThread
        f (fun g arg -> 
            let nctx = SynchronizationContext.Current
            match ctx, nctx with
            | null, _ -> g arg
            | _, _ when Object.Equals(ctx, nctx) && thread.Equals(Thread.CurrentThread) -> g arg
            | _ -> ctx.Post((fun _ -> g (arg)), null))

    let memoize f =
        let cache = System.Collections.Generic.Dictionary()
        fun x ->
            match cache.TryGetValue x with
            | true, x -> x
            | _ ->
                let res = f x
                cache.[x] <- res
                res

    type Microsoft.FSharp.Control.Async with
        static member EitherEvent(ev1: IObservable<'T>, ev2: IObservable<'U>) = 
            synchronize (fun f -> 
                Async.FromContinuations((fun (cont, _econt, _ccont) -> 
                    let rec callback1 = 
                        (fun value -> 
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice1Of2(value)))
                    
                    and callback2 = 
                        (fun value -> 
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice2Of2(value)))
                    
                    and remover1: IDisposable = ev1.Subscribe(callback1)
                    and remover2: IDisposable = ev2.Subscribe(callback2)
                    ())))

    type Atom<'T when 'T: not struct>(value: 'T) = 
        let refCell = ref value
        
        let rec swap f = 
            let currentValue = !refCell
            let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
            if obj.ReferenceEquals(result, currentValue) then result
            else 
                Thread.SpinWait 20
                swap f
        
        member __.Value = !refCell
        member __.Swap(f: 'T -> 'T) = swap f

    open System.IO

    type Path with
        static member GetFullPathSafe path =
            try Path.GetFullPath path
            with _ -> path

        static member GetFileNameSafe path =
            try Path.GetFileName path
            with _ -> path

    /// Path.Combine
    let (</>) path1 path2 = Path.Combine (path1, path2)

[<RequireQualifiedAccess>]
module Dict = 
    open System.Collections.Generic

    let add key value (dict: #IDictionary<_,_>) =
        dict.[key] <- value
        dict

    let remove (key: 'k) (dict: #IDictionary<'k,_>) =
        dict.Remove key |> ignore
        dict

    let tryFind key (dict: #IDictionary<'k, 'v>) = 
        let mutable value = Unchecked.defaultof<_>
        if dict.TryGetValue (key, &value) then Some value
        else None

    let ofSeq (xs: ('k * 'v) seq) = 
        let dict = Dictionary()
        for k, v in xs do dict.[k] <- v
        dict

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let inline toCharArray (str:string) = str.ToCharArray()

    let lowerCaseFirstChar (str: string) =
        if String.IsNullOrEmpty str 
         || Char.IsLower(str, 0) then str else 
        let strArr = toCharArray str
        match Array.tryHead strArr with
        | None -> str
        | Some c  -> 
            strArr.[0] <- Char.ToLower c
            String (strArr)


    let extractTrailingIndex (str: string) =
        match str with
        | null -> null, None
        | _ ->
            let charr = str.ToCharArray() 
            Array.revInPlace charr
            let digits = Array.takeWhile Char.IsDigit charr
            Array.revInPlace digits
            String digits
            |> function
               | "" -> str, None
               | index -> str.Substring (0, str.Length - index.Length), Some (int index)

    /// Remove all trailing and leading whitespace from the string
    /// return null if the string is null
    let trim (value: string) = if isNull value then null else value.Trim()
    
    /// Splits a string into substrings based on the strings in the array separators
    let split options (separator: string []) (value: string) = 
        if isNull value  then null else value.Split(separator, options)

    let (|StartsWith|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.StartsWith pattern then
            Some()
        else None

    let (|Contains|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.Contains pattern then
            Some()
        else None
    
    open System.IO

    let getLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            yield !line
            line := reader.ReadLine()
        if str.EndsWith("\n") then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
        |]

    let getNonEmptyLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            if (!line).Length > 0 then
                yield !line
            line := reader.ReadLine()
        |]

    /// Parse a string to find the first nonempty line
    /// Return null if the string was null or only contained empty lines
    let firstNonEmptyLine (str: string) =
        use reader = new StringReader (str)
        let rec loop (line:string) =
            if isNull line then None 
            elif  line.Length > 0 then Some line
            else loop (reader.ReadLine())
        loop (reader.ReadLine())

open System.Text
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringBuilder =
    /// Pipelining function for appending a string to a stringbuilder
    let inline append (str:string) (sb:StringBuilder) = sb.Append str

    /// Pipelining function for appending a string with a '\n' to a stringbuilder
    let inline appendLine (str:string) (sb:StringBuilder) = sb.AppendLine str
    
    /// SideEffecting function for appending a string to a stringbuilder
    let inline appendi (str:string) (sb:StringBuilder) = sb.Append str |> ignore

    /// SideEffecting function for appending a string with a '\n' to a stringbuilder
    let inline appendLinei (str:string) (sb:StringBuilder) = sb.AppendLine str |> ignore

module Reflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    type private Expr = System.Linq.Expressions.Expression
    let instanceNonPublic = BindingFlags.Instance ||| BindingFlags.NonPublic
    
    let precompileFieldGet<'R>(f : FieldInfo) =
        let p = Expr.Parameter(typeof<obj>)
        let lambda = Expr.Lambda<Func<obj, 'R>>(Expr.Field(Expr.Convert(p, f.DeclaringType) :> Expr, f) :> Expr, p)
        lambda.Compile().Invoke

    // Various flags configurations for Reflection
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let ctorFlags = instanceFlags
    let inline asMethodBase (a : #MethodBase) = a :> MethodBase
    
    let (?) (o : obj) name : 'R = 
        // The return type is a function, which means that we want to invoke a method
        if FSharpType.IsFunction(typeof<'R>) then 
            let argType, _resType = FSharpType.GetFunctionElements(typeof<'R>)
            FSharpValue.MakeFunction(typeof<'R>, 
                fun args -> 
                    // We treat elements of a tuple passed as argument as a list of arguments
                    // When the 'o' object is 'System.Type', we call static methods
                    let methods, instance, args = 
                        let typeInfo = o.GetType()
                        let args = 
                            if argType = typeof<unit> then [||]
                            elif not (FSharpType.IsTuple(argType)) then [| args |]
                            else FSharpValue.GetTupleFields(args)
                        if (typeof<System.Type>).IsAssignableFrom(typeInfo) then 
                            let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
                            let ctors = 
                                (unbox<Type> o).GetConstructors(ctorFlags) 
                                |> Array.map asMethodBase
                            Array.concat [ methods; ctors ], null, args
                        else 
                            typeInfo.GetMethods(instanceFlags) |> Array.map asMethodBase, o, 
                            args
                                         
                    // A simple overload resolution based on the name and number of parameters only
                    let methods = 
                        [ for m in methods do
                            if m.Name = name && m.GetParameters().Length = args.Length then 
                                yield m ]
                                         
                    match methods with
                    | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
                    | _ :: _ :: _ -> 
                        failwithf "Multiple methods '%s' with %d arguments found" name args.Length
                    | [ :? ConstructorInfo as c ] -> c.Invoke(args)
                    | [ m ] -> m.Invoke(instance, args))
            |> unbox<'R>
        else 
            // When the 'o' object is 'System.Type', we access static properties
            let typ, flags, instance = 
                if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then unbox o, staticFlags, null
                else o.GetType(), instanceFlags, o
            
            // Find a property that we can call and get the value
            let prop = typ.GetProperty(name, flags)
            if prop = null then failwithf "Property '%s' not found in '%s' using flags '%A'." name typ.Name flags
            let meth = prop.GetGetMethod(true)
            if prop = null then failwithf "Property '%s' found, but doesn't have 'get' method." name
            meth.Invoke(instance, [||]) |> unbox<'R>

    let (?<-) (o: obj) name value =
      o.GetType().GetProperty(name).SetValue(o, value, null)

module File =
    open System.IO

    let tryGetLastWriteTime file = Option.attempt (fun _ -> FileInfo(file).LastWriteTimeUtc)

open System.Text
open System.Diagnostics

type Profiler() =
    let measures = ResizeArray()
    let total = Stopwatch.StartNew()

    member __.Time msg f = 
        let sw = Stopwatch.StartNew()
        let res = f()
        measures.Add(msg, sw.Elapsed)
        res

    member __.TimeAsync msg f = async {
        let sw = Stopwatch.StartNew()
        let! res = f()
        measures.Add(msg, sw.Elapsed)
        return res }

    member __.Stop() = total.Stop()
    
    member __.Result =
        sprintf
            "\nTotal = %O\n%s" 
            total.Elapsed
            (measures 
             |> Seq.groupBy (fun (msg, _) -> msg)
             |> Seq.map (fun (msg, ts) -> 
                 msg, TimeSpan.FromTicks (ts |> Seq.sumBy (fun (_, t) -> t.Ticks)))
             |> Seq.sortBy (fun (_, t) -> -t)
             |> Seq.fold (fun (acc: StringBuilder) (msg, t) -> 
                 acc.AppendLine (sprintf "%s, %O" msg t)) (StringBuilder())
             |> string)

    member __.Elapsed = total.Elapsed        