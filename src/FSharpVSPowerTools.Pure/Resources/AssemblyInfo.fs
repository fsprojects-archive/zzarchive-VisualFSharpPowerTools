namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Pure")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("The Purest Source of F# Power in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("2.2.0")>]
[<assembly: AssemblyFileVersionAttribute("2.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.2.0"
