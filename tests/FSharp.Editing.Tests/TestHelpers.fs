(*

Copyright 2005-2009 Microsoft Corporation
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

/// Helper functions for implementing unit tests.
[<AutoOpen>]
module FSharp.Editing.Tests.TestHelpers

open System
open NUnit.Framework

(* Fluent test helpers for use with NUnit and FsUnit. *)

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

/// Assertion functions for collections.
[<RequireQualifiedAccess>]
module Collection =
    /// Asserts that two collections are exactly equal.
    /// The collections must have the same count, and contain the exact same objects in the same order.
    let inline assertEqual<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreEqual (expected, actual)

    /// Asserts that two collections are not exactly equal.
    let inline assertNotEqual<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreNotEqual (expected, actual)

    /// Asserts that two collections are exactly equal.
    /// The collections must have the same count, and contain the exact same objects but the match may be in any order.
    let inline assertEquiv<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        try CollectionAssert.AreEquivalent (expected, actual) 
        with _ -> Diagnostics.Trace.WriteLine (sprintf "Expected: %A, actual: %A" expected actual); reraise()

    /// Asserts that two collections are not exactly equal.
    let inline assertNotEquiv<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreNotEquivalent (expected, actual)

module LanguageServiceTestHelper =
    open Microsoft.FSharp.Compiler.SourceCodeServices

    let references = 
        [ 
          yield typeof<System.Object>.Assembly.Location; // mscorlib
          yield typeof<System.Console>.Assembly.Location; // System.Console
          yield typeof<System.ComponentModel.DefaultValueAttribute>.Assembly.Location; // System.Runtime
          yield typeof<System.ComponentModel.PropertyChangedEventArgs>.Assembly.Location; // System.ObjectModel             
          yield typeof<System.IO.BufferedStream>.Assembly.Location; // System.IO
          yield typeof<System.Linq.Enumerable>.Assembly.Location; // System.Linq
          yield typeof<System.Xml.Linq.XDocument>.Assembly.Location; // System.Xml.Linq
          yield typeof<System.Net.WebRequest>.Assembly.Location; // System.Net.Requests
          yield typeof<System.Numerics.BigInteger>.Assembly.Location; // System.Runtime.Numerics
          yield typeof<System.Threading.Tasks.TaskExtensions>.Assembly.Location; // System.Threading.Tasks
          yield typeof<Microsoft.FSharp.Core.MeasureAttribute>.Assembly.Location; // FSharp.Core
        ]
        
    let args =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            [|
               "--noframework";
               "--debug-";
               "--optimize-";
               "--tailcalls-";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\mscorlib.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Core.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Drawing.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Numerics.dll";
               @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Windows.Forms.dll";
            |]
        else
            [|
               yield "--noframework"
               yield "--debug-"
               yield "--optimize-"
               yield "--tailcalls-"
               for r in references do
                   yield "-r:" + r
            |]

    let argsDotNET451 = args
    
    let projectOptions =
        let counter = ref 0
        fun fileName ->
            incr counter
            { ProjectFileName = sprintf "/Project%i.fsproj" !counter
              ProjectFileNames = [| fileName |]
              OtherOptions = args
              ReferencedProjects = Array.empty
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = true
              LoadTime = DateTime.UtcNow
              UnresolvedReferences = None }
