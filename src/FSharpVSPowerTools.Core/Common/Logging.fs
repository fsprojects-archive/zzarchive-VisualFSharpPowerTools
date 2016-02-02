module FSharpVSPowerTools.Logging 

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Reflection
open FSharpVSPowerTools


/// Defines Tracing information for TraceListeners
type TraceData = 
    | StartMessage
    | ImportantMessage of string
    | ErrorMessage of string
    | LogMessage of string * bool
    | TraceMessage of string * bool
    | FinishedMessage
    | OpenTag of string * string
    | CloseTag of string

/// Defines a TraceListener interface
type ITraceListener = 
    abstract Write : TraceData -> unit

/// A default color map which maps TracePriorities to ConsoleColors
let colorMap traceData = 
    match traceData with
    | ImportantMessage _ -> ConsoleColor.Yellow
    | ErrorMessage _ -> ConsoleColor.Red
    | LogMessage _ -> ConsoleColor.Gray
    | TraceMessage _ -> ConsoleColor.Green
    | FinishedMessage -> ConsoleColor.White
    | _ -> ConsoleColor.Gray

/// Implements a TraceListener for System.Console.
/// ## Parameters
///  - `importantMessagesToStdErr` - Defines whether to trace important messages to StdErr.
///  - `colorMap` - A function which maps TracePriorities to ConsoleColors.
type ConsoleTraceListener(importantMessagesToStdErr, colorMap) = 
    
    let writeText toStdErr color newLine text = 
        let curColor = Console.ForegroundColor
        if curColor <> color then Console.ForegroundColor <- color
        if toStdErr then 
            if newLine then eprintfn "%s" text
            else eprintf "%s" text
        else if newLine then printfn "%s" text
        else printf "%s" text
        if curColor <> color then Console.ForegroundColor <- curColor
    
    interface ITraceListener with
        /// Writes the given message to the Console.
        member this.Write msg = 
            let color = colorMap msg
            match msg with
            | StartMessage -> ()
            | OpenTag _ -> ()
            | CloseTag _ -> ()
            | ImportantMessage text | ErrorMessage text -> writeText importantMessagesToStdErr color true text
            | LogMessage(text, newLine) | TraceMessage(text, newLine) -> writeText false color newLine text
            | FinishedMessage -> ()

let defaultConsoleTraceListener =
    // If we write the stderr on those build servers the build will fail.
    ConsoleTraceListener(true, colorMap)

/// A List with all registered listeners
let listeners = new Collections.Generic.List<ITraceListener>()

/// Allows to register a new Xml listeners

// register listeners
listeners.Add defaultConsoleTraceListener

/// Allows to post messages to all trace listeners
let postMessage x = listeners.ForEach(fun listener -> listener.Write x)

/// Allows to post messages to all trace listeners

    /// [omit]
let mutable verbose = false

let private openTags = new ThreadLocal<list<string>>(fun _ -> [])

/// Logs the specified string        
let log message = LogMessage(message, true) |> postMessage

/// Logs the specified message
let logfn fmt = Printf.ksprintf log fmt

/// Logs the specified message (without line break)
let logf fmt = Printf.ksprintf (fun text -> postMessage (LogMessage(text, false))) fmt

/// Logs the specified string if the verbose mode is activated.
let logVerbosefn fmt = 
    Printf.ksprintf (if verbose then log
                        else ignore) fmt

/// Writes a trace to the command line (in green)
let trace message = postMessage (TraceMessage(message, true))

/// Writes a message to the command line (in green)
let tracefn fmt = Printf.ksprintf trace fmt

/// Writes a message to the command line (in green) and without a line break
let tracef fmt = Printf.ksprintf (fun text -> postMessage (TraceMessage(text, false))) fmt

/// Writes a trace to the command line (in green) if the verbose mode is activated.
let traceVerbose s = 
    if verbose then trace s

/// Writes a trace to stderr (in yellow)  
let traceImportant text = postMessage (ImportantMessage text)

/// Writes a trace to the command line (in yellow)
let traceFAKE fmt = Printf.ksprintf (fun text -> postMessage (ImportantMessage text)) fmt

/// Traces an error (in red)
let traceError error = postMessage (ErrorMessage error)

open Microsoft.FSharp.Core.Printf
/// Traces an exception details (in red)
let traceException (ex:Exception) =
    let sb = Text.StringBuilder()
    let delimeter = String.replicate 50 "*"
    let nl = Environment.NewLine
    let rec printException (e:Exception) count =
        if (e :? TargetException && e.InnerException <> null)
        then printException (e.InnerException) count
        else
            if (count = 1) then bprintf sb "Exception Message:%s%s%s" e.Message nl delimeter
            else bprintf sb "%s%s%d)Exception Message:%s%s%s" nl nl count e.Message nl delimeter
            bprintf sb "%sType: %s" nl (e.GetType().FullName)
            // Loop through the public properties of the exception object
            // and record their values.
            e.GetType().GetProperties()
            |> Array.iter (fun p ->
                // Do not log information for the InnerException or StackTrace.
                // This information is captured later in the process.
                if (p.Name <> "InnerException" && p.Name <> "StackTrace" &&
                    p.Name <> "Message" && p.Name <> "Data") then
                    try
                        let value = p.GetValue(e, null)
                        if (value <> null)
                        then bprintf sb "%s%s: %s" nl p.Name (value.ToString())
                    with
                    | e2 -> bprintf sb "%s%s: %s" nl p.Name e2.Message
            )
            if (e.StackTrace <> null) then
                bprintf sb "%s%sStackTrace%s%s%s" nl nl nl delimeter nl
                bprintf sb "%s%s" nl e.StackTrace
            if (e.InnerException <> null)
            then printException e.InnerException (count+1)
    printException ex 1
    sb.ToString() |> traceError

/// Retrieves all environment variables from the given target
let environVars target = 
    [ for e in Environment.GetEnvironmentVariables target -> 
          let e1 = e :?> Collections.DictionaryEntry
          e1.Key, e1.Value ]

/// Traces the EnvironmentVariables
let TraceEnvironmentVariables() = 
    [   EnvironmentVariableTarget.Machine 
        EnvironmentVariableTarget.Process  
        EnvironmentVariableTarget.User      ] 
    |> Seq.iter (fun mode -> 
            tracefn "Environment-Settings (%A):" mode
            environVars mode |> Seq.iter (tracefn "  %A"))

/// Traces a line
let traceLine() = trace "---------------------------------------------------------------------"

/// Traces a header
let traceHeader name = 
    trace ""
    traceLine()
    trace name
    traceLine()

/// Traces the begin of the build
let traceStartBuild() = postMessage StartMessage

/// Traces the end of the build
let traceEndBuild() = postMessage FinishedMessage

/// Puts an opening tag on the internal tag stack
let openTag tag = openTags.Value <- tag :: openTags.Value

/// Removes an opening tag from the internal tag stack
let closeTag tag = 
    match openTags.Value with
    | x :: rest when x = tag -> openTags.Value <- rest
    | _ -> failwithf "Invalid tag structure. Trying to close %s tag but stack is %A" tag openTags
    CloseTag tag |> postMessage

let closeAllOpenTags() = Seq.iter closeTag openTags.Value

/// Traces the begin of a target
let traceStartTarget name description dependencyString = 
    openTag "target"
    OpenTag("target", name) |> postMessage
    tracefn "Starting Target: %s %s" name dependencyString
    if description <> null then tracefn "  %s" description

/// Traces the end of a target   
let traceEndTarget name = 
    tracefn "Finished Target: %s" name
    closeTag "target"

/// Traces the begin of a task
let traceStartTask task  = 
    openTag "task"
    OpenTag("task", task) |> postMessage

/// Traces the end of a task
let traceEndTask () = 
    closeTag "task"

let console = new ConsoleTraceListener(false, colorMap) :> ITraceListener

open System.Diagnostics

/// Traces the message to the console
let logToConsole (msg, eventLogEntry : EventLogEntryType) = 
    match eventLogEntry with
    | EventLogEntryType.Error -> ErrorMessage msg
    | EventLogEntryType.Information -> TraceMessage(msg, true)
    | EventLogEntryType.Warning -> ImportantMessage msg
    | _ -> LogMessage(msg, true)
    |> console.Write

/// Logs the given files with the message.
let Log message files = files |> Seq.iter (log << sprintf "%s%s" message)



/// [omit]
type Trace = {
    Level: TraceLevel
    Text: string
    NewLine: bool }


/// [omit]
let event = Event<Trace>()


let tracen s = event.Trigger { Level = TraceLevel.Info; Text = s; NewLine = true }

let verbosefn fmt = Printf.ksprintf traceVerbose fmt

let traceWarn s = event.Trigger { Level = TraceLevel.Warning; Text = s; NewLine = true }

let traceErrorfn fmt = Printf.ksprintf traceError fmt

let traceWarnfn fmt = Printf.ksprintf traceWarn fmt


// Console Trace

/// [omit]
let traceColored color (s:string) = 
    let curColor = Console.ForegroundColor
    if curColor <> color then Console.ForegroundColor <- color
    use textWriter = 
        match color with
        | ConsoleColor.Red -> Console.Error
        | _ -> Console.Out
    textWriter.WriteLine s
    if curColor <> color then Console.ForegroundColor <- curColor

/// [omit]
let monitor = new Object()

/// [omit]
let traceToConsole (trace:Trace) =
    lock monitor
        (fun () ->
            match trace.Level with
            | TraceLevel.Warning -> traceColored ConsoleColor.Yellow trace.Text
            | TraceLevel.Error -> traceColored ConsoleColor.Red trace.Text
            | _ ->
                if trace.NewLine then Console.WriteLine trace.Text
                else Console.Write trace.Text )


// Log File Trace

/// [omit]
let mutable logFile : string option = None

/// [omit]
let traceToFile (trace:Trace) =
    match logFile with
    | Some fileName -> try File.AppendAllLines(fileName,[trace.Text]) with | _ -> ()
    | _ -> ()

/// [omit]
let setLogFile fileName =
    let fi = FileInfo fileName
    logFile <- Some fi.FullName
    if fi.Exists then
        fi.Delete()
    else
        if fi.Directory.Exists |> not then
            fi.Directory.Create()
    event.Publish |> Observable.subscribe traceToFile

let protectOrDefault f defaultVal =
    try
        f()
    with e ->
        traceException  e
        defaultVal

/// Try to run a given async computation, catch and log its exceptions
let protectAsync a =
    async {
        let! res = Async.Catch a
        return 
            match res with 
            | Choice1Of2 () -> ()
            | Choice2Of2 e ->
                traceException e
                ()
    }

/// Try to run a given function and catch its exceptions
let protect f = protectOrDefault f ()

/// Execute a function and record execution time
let time label f =
    let sw = Stopwatch.StartNew()
    let result = f()
    sw.Stop()
    debug "%s took: %i ms" label sw.ElapsedMilliseconds
    result

