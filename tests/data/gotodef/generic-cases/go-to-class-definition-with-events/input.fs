
type Class() =
    let event = Event<int>()
    [<CLIEvent>]
    member this.MyEvent = event.Publish