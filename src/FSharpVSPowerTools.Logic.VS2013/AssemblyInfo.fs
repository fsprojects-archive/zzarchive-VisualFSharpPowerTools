namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Logic.VS2013")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("1.10.0")>]
[<assembly: AssemblyFileVersionAttribute("1.10.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.10.0"
