namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Logic.VS2013")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("Visual F# Power Tools (by F# Community)")>]
[<assembly: AssemblyVersionAttribute("0.6.0")>]
[<assembly: AssemblyFileVersionAttribute("0.6.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.6.0"
