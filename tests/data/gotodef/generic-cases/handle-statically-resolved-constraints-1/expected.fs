module File

type MyClass< ^T when ^T : (static member Create : unit ->  ^T) and ^T : (member Prop : int)> =
    class
    end
