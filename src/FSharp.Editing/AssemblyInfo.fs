namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharp.Editing.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharp.Editing")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("2.5.2")>]
[<assembly: AssemblyFileVersionAttribute("2.5.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.5.2"
    let [<Literal>] InformationalVersion = "2.5.2"
