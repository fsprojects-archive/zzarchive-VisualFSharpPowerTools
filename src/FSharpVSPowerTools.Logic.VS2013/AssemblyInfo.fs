namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Logic.VS2013")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("A collection of additional commands for F# in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("1.3.0")>]
[<assembly: AssemblyFileVersionAttribute("1.3.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.3.0"
