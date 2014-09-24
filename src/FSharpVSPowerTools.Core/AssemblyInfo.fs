namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Core")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("1.5.0")>]
[<assembly: AssemblyFileVersionAttribute("1.5.0")>]
[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Core.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.5.0"
    