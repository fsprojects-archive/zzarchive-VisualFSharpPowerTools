
type MyClass<'T when 'T : (static member ``A static member`` : unit -> 'T)> =
    class end
