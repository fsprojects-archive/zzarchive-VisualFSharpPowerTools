﻿[<AutoOpen>]
module FSharpVSPowerTools.Tests.TestHelpers

open TestUtilities.Mocks
open NUnit.Framework
open System.IO
open System
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools.ProjectSystem

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

let internal createVirtualProject(buffer, fileName) =
    VirtualProjectProvider(buffer, fileName, VisualStudioVersion.VS2013)

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
let inline assertTrue condition =
    Assert.IsTrue (condition)

/// Asserts that a condition is false.
let inline assertFalse condition =
    Assert.IsFalse (condition)

type [<Measure>] ms

#if APPVEYOR
let timeout = 40000<ms>
#else
let timeout = 10000<ms>
#endif

let (=>) (startLine, startCol) (endLine, endCol) =
        (startLine, startCol, endLine, endCol)
        
let snapshotPoint (snapshot: ITextSnapshot) line (column: int) = 
    let line = snapshot.GetLineFromLineNumber(line - 1)
    SnapshotPoint(snapshot, line.Start.Position + column)

let testEventTrigger event errorMessage (timeout: int<_>) triggerEvent predicate =
    let task =
        event
        |> Async.AwaitEvent
        |> Async.StartAsTask
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    triggerEvent()
    match task.Wait(TimeSpan.FromMilliseconds(float timeout)) with
    | true ->         
        sw.Stop()
        Console.WriteLine(sprintf "Event took: %O s" (sw.ElapsedMilliseconds/1000L))
        predicate()
    | false ->
        sw.Stop()
        Console.WriteLine(sprintf "Event took: %O s" (sw.ElapsedMilliseconds/1000L))
        Assert.Inconclusive errorMessage

let testEvent event errorMessage (timeout: int<_>) predicate =
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
