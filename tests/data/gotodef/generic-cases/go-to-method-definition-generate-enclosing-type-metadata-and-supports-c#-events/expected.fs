namespace System

open System

/// Represents the standard input, output, and error streams for console applications. This class cannot be inherited.
[<Class>]
type Console =
    [<Security.SecuritySafeCritical>]
    static member add_CancelKeyPress : value:ConsoleCancelEventHandler -> unit
    /// Gets or sets the background color of the console.
    static member BackgroundColor : ConsoleColor with get, set
    /// Plays the sound of a beep through the console speaker.
    static member Beep : unit -> unit
