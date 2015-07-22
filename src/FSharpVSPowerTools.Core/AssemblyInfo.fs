namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Core.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Core")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("2.0.0")>]
[<assembly: AssemblyFileVersionAttribute("2.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.0"
