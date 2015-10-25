module SampleFile

let noIsAValue = 42

let YesIsACurriedMethod x y = ()

[<System.Obsolete>]
let YesIsATupledMethod(x, y) = ()

type MyType1() =
    member this.Whatever() = ()
and MyType2() =
    member this.Whatever() = ()
and 
    MyType3() =
    member this.Whatever() = ()

[<AbstractClass>]
type TestXmlDoc() =              
    let nope() = ()

    member this.YesNoArgs() = ()

    [<System.Obsolete>]
    member this.YesThreeArgs(z,x:int,y:string) = ()

    member this.NopeIsAProperty = 42 // does, no args

    member this.NopeIsAPropertyGetterSetter
        with get() = 42  // does
        and set(x) = ()  // does

    static member YesIsStatic(x,y) = ()

    [<System.Obsolete>]
    abstract member YesIsAbstract : x:int * y:string -> unit

    abstract member YesIsAbstractNoNames : int * string -> unit

    override this.ToString() = ""

type IMyInterface =
    abstract member YesIsAbstract : x:int * y:string -> unit

type MyRecord = { xx:int; yy:string } with
    member this.Blah(z:string) = ()
    member this.Prop = ()

type MyUnion =
    | This of int
    | That of string * int with
    member this.Blah(xx:int) = ()
    member this.Prop = ()

module LocalModule =

    let noIsAValue = 42

    let YesIsACurriedMethod x y = ()

    let YesIsATupledMethod(x, y) = ()

    type TestXmlDoc() =              
        let nope() = ()

        member this.YesNoArgs() = ()

        member this.YesThreeArgs(z,x:int,y:string) = ()

        member this.NopeIsAProperty = 42

        member this.NopeIsAPropertyGetterSetter
            with get() = 42
            and set(x) = ()

///
let functionWithEmptyXmlDocComment x = ()

/// <
let functionWithOpenSquareBracketOnlyXmlDocComment x = ()

/// function
let functionWithNormalXmlDocComment x = ()

/// 
type TypeWithEmptyXmlDocComment = class end

/// type
type TypeWithXmlDocComment() =
    ///
    member x.X = ()
    /// member
    member x.Y = ()

let f = 0

let func : int -> int -> int list =
    fun x y ->
        List.append [x] [y]