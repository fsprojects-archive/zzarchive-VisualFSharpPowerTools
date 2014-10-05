namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Core.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Core")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("1.6.0")>]
[<assembly: AssemblyFileVersionAttribute("1.6.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.6.0"
