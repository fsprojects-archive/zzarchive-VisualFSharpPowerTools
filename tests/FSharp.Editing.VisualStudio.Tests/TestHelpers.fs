﻿[<AutoOpen>]
module FSharp.Editing.VisualStudio.Tests.Helpers

open TestUtilities.Mocks
open NUnit.Framework
open System.IO
open System
open Microsoft.VisualStudio.Text
open FSharp.Editing
open FSharp.Editing.VisualStudio.ProjectSystem
open System.Threading
open FSharp.Editing.ProjectSystem
open FSharp.Editing.VisualStudio

let inline notimpl<'T> : 'T = failwith "Not implemented yet"

let getTempFileName =
    let counter = ref 0
    fun ext ->
        incr counter
        Path.Combine(__SOURCE_DIRECTORY__, sprintf "test%i%s" !counter ext)

let createMockTextBuffer content fileName = 
    MockTextBuffer(content, filename = fileName, contentType = "F#")

let createMockTextView buffer =
    MockTextView(buffer)

let createVirtualProject(buffer: ITextBuffer, fileName) =
    VirtualProjectProvider(buffer.CurrentSnapshot.GetText(), fileName, VisualStudioVersion.VS2013) :> IProjectProvider

/// Tests that the specified condition is true.
/// If not, calls Assert.Fail with a formatted string.
let inline assertf (condition : bool) format : 'T =
    Printf.ksprintf (fun str -> if not condition then Assert.Fail str) format

/// Asserts that two values are equal.
let inline assertEqual<'T when 'T : equality> (expected : 'T) (actual : 'T) =
    Assert.AreEqual (expected, actual, sprintf "Expected: %A\nActual: %A" expected actual)

/// Asserts that two values are NOT equal.
let inline assertNotEqual<'T when 'T : equality> (expected : 'T) (actual : 'T) =
    Assert.AreNotEqual (expected, actual)

/// Asserts that two objects are identical.
let inline assertSame<'T when 'T : not struct> (expected : 'T) (actual : 'T) =
    Assert.AreSame (expected, actual)

/// Asserts that two objects are NOT identical.
let inline assertNotSame<'T when 'T : not struct> (expected : 'T) (actual : 'T) =
    Assert.AreNotSame (expected, actual)

/// Asserts that a condition is true.
let inline assertTrue (condition: bool) = Assert.IsTrue condition

/// Asserts that a condition is false.
let inline assertFalse (condition: bool) = Assert.IsFalse condition

type [<Measure>] ms

#if APPVEYOR
let timeout = 60000<ms>
#else
let timeout = 10000<ms>
#endif

let (=>) (startLine, startCol) (endLine, endCol) =
        (startLine, startCol, endLine, endCol)
        
let snapshotPoint (snapshot: ITextSnapshot) line (column: int) = 
    let line = snapshot.GetLineFromLineNumber(line - 1)
    SnapshotPoint(snapshot, line.Start.Position + column)

let testEventTrigger event errorMessage (timeout: int<ms>) triggerEvent predicate =
    Thread.Sleep 1000
    use ct = new CancellationTokenSource()
    let task = Async.StartAsTask (Async.AwaitEvent event, cancellationToken = ct.Token)
    try
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        triggerEvent()
        
        match task.Wait (int timeout) with
        | true ->
            sw.Stop()
            Console.WriteLine(sprintf "Event took: %O s" (sw.ElapsedMilliseconds/1000L))
            predicate()
        | false ->
            sw.Stop()
            Console.WriteLine(sprintf "Event took: %O s" (sw.ElapsedMilliseconds/1000L))
            Assert.Fail errorMessage
    finally
        ct.Cancel()
        if not task.IsCanceled then
            task.Wait (int timeout) |> ignore

let testEvent event errorMessage (timeout: int<ms>) predicate =
    testEventTrigger event errorMessage timeout id predicate

/// Asserts that two strings are the same modulo new line format.
let inline assertEquivString (expected: string) (actual: string) =
    actual.Replace("\r\n", "\n")
    |> assertEqual (expected.Replace("\r\n", "\n"))

let inline prependNewLine str = Environment.NewLine + str
let inline appendNewLine str = str + Environment.NewLine

let nameOf<'T> = typeof<'T>.Name

let fullPathBasedOnSourceDir relativePath =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, relativePath))
