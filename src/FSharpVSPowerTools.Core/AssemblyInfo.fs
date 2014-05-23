namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Core")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools")>]
[<assembly: AssemblyDescriptionAttribute("Visual F# Power Tools (by F# Community)")>]
[<assembly: AssemblyVersionAttribute("1.1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.1.0"
