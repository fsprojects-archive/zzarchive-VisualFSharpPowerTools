module SampleFile

let x = 42

module LocalModule =
    module Inner = 
        let x = 4
        let f() =  // spaces after end of expression cause color not to extend to width of viewport?
            x    
        let f2() =
            x   // comment here causes color to left not to extend to width of viewport, but that's hard to fix
        type Foo() =
            class end
        let f3() =
            x
        let f4() =
            x
            x
        let f5() =  // TODO untype and retype the x below, and watch colors of blank lines not get updated because no tags change
            x


let g x = 42

let g2 x = 
    if true then
                ignore ()  // test blank lines below 

                let n = 24

                [ [ 43
                    44 ] ]
    else [[]]

let g3 x = 
    if true then
                ignore ()  // test #command as unindented lines
#if DEBUG
                42
#else
                0
#endif
    else 0

let g4 x = 
    if true then
                ignore ()  // test comments as unindented lines
//blah
                42
    else 0

let rec r1 x =
    r2 x
and r2 x =
    r1 x

type Foo1() =
    member this.Bar() =
        ()
and Foo2() =
    member this.Bar() =
        ()

type Qux() = 
    inherit System.Object()
    let x = 42
    let y = 43
    let f() = ()
    let g() = ()
    member this.Foo() = ()
    member this.Baz() = ()
    member this.Bar() =
        let x = 42
        let y = 43
        let f() = ()
        let g() = ()
        this.Foo()
    interface System.IDisposable with
        member this.Dispose() =
            printfn "done"
        member this.Dispose2() =
            printfn "done"

let h x = [ 42
            43
            44 ]

type Blah() =              
    let bar() = ()
    member this.Bar() =
        let x = 4
        if true then
            bar()
            if true then
                bar()
                if true then
                    bar()
                    if true then
                        bar()
                        if true then
                            bar()
                            if true then
                                bar()
                                if true then
                                    bar()
                                    if true then
                                        bar()
                                        if true then
                                            bar()
                                            if true then
                                                bar()
                                                if true then
                                                    bar()
                                                    if true then
                                                        bar()
                                                        if true then
                                                            bar()
                                                            if true then
                                                                bar()
        34                      +                     22   |> ignore
    34                      +                     22   |> ignore

    member this.QUX() =
        bar()
        if true then
            bar()
        else
            bar()

        try
            bar()
        with e ->
            bar()

        try
            bar()
        finally
            bar()

        match 24 with
        | 0 -> bar()
        | 1 -> bar()
               bar()
        | n -> 
            bar()

        if true then
            bar()
        elif true then
            bar()
        elif true then
            bar()
        else
            bar()

        while true do
            bar()

        for i = 0 to 10 do
            bar()

        for i in 1..10 do
            bar()
        
        let x = 
            42

        do printfn "hi"

        do
            printfn "hi"
            printfn "hi"

        async {
            bar()
            let! x = async { return 1 }
            bar()
            let! x =
                async { return 1 }
            bar()
            do! async { return () }
            bar()
            do!
                async { return () }
            bar()
        } |> Async.Start 

let f x = 42


type Xyzzy() = 
    inherit System.Object()
    static let s = @"hello
                     world"
    let x = 42
    let y = 43
    let f() = ()
    let g() = ()
    member this.Foo() = ()
    member this.Bar() =
        let x = 42
        let y = 43
        let f() = ()
        let g() = ()
        this.Foo()
    member this.Baz() = ()
    interface System.IDisposable with
        member this.Dispose() =
            printfn "done"
            if true then
                f()
    member this.Qux() = ()

let k x = 42

type MyDU =
    | Foo of int
    | Bar of string * int

let g() =
    async {
        bar() 
        let! x = blah
        bar() // last thing in async is "if" and previous there is "let!" -> bad color
        if true then () else () 
    } |> Async.Start 

    let rec Loop() =
        async {
            let! msg = mbox.Receive()
            return! foo
            do! cheese
            match msg with
            | EnsureVisited url -> 
                if not(visited.Contains(url)) && not(started.Contains(url)) then
                    started.Add(url) |> ignore
                    visit url |> Async.Start 
            |  FinishedVisiting url ->
                started.Remove(url) |> ignore
                visited.Add(url) |> ignore
            incr numIters
            if !numIters % 10 = 0 then
                printf "%d " started.Count 
            if started.Count <> 0 then
                return! Loop()
            else
                allDone.Set() |> ignore
        }
    Loop()




