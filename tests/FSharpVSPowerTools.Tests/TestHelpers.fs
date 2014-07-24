[<AutoOpen>]
module FSharpVSPowerTools.Tests.TestHelpers

open TestUtilities.Mocks

let inline notimpl<'T> : 'T = failwith "Not implemented yet"

let createMockTextBuffer content fileName = 
    MockTextBuffer(content, filename = fileName, contentType = "F#")

