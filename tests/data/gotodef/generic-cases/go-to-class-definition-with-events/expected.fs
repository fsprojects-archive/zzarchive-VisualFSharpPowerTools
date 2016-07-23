module File

type Class =
    new : unit -> Class
    member add_MyEvent : Handler<int> -> unit
    [<CLIEvent>]
    member MyEvent : IEvent<int>
    member remove_MyEvent : Handler<int> -> unit
