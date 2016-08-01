
type MyClass() =
    let mutable instanceValue = 0
    static let mutable staticValue = 0

    member val GetterAndSetter = 0 with get, set
    member __.SetterOnly with set(value) = instanceValue <- value

    static member val StaticGetterAndSetter = 0 with get, set
    static member StaticSetterOnly with set(value) = staticValue <- value
