namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Logic.VS2015")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("2.6.0")>]
[<assembly: AssemblyFileVersionAttribute("2.6.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.6.0"
    let [<Literal>] InformationalVersion = "2.6.0"
