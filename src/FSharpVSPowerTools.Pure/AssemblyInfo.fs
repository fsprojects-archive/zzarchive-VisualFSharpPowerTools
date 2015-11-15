namespace System
open System.Reflection
open System.Runtime.CompilerServices
open Microsoft.VisualStudio.Shell

[<assembly: ProvideCodeBase
            (AssemblyName="FSharpVSPowerTools.Pure",
            Version="2.2.0",
            CodeBase = "$PackageFolder$\\FSharpVSPowerTools.Pure.dll")>]
[<assembly: ProvideCodeBase
            (AssemblyName="FSharpVSPowerTools.Core",
            Version="2.2.0",
            CodeBase = "$PackageFolder$\\FSharpVSPowerTools.Core.dll")>]
[<assembly: ProvideCodeBase
            (AssemblyName="FSharpVSPowerTools.Logic",
            Version="2.2.0",
            CodeBase = "$PackageFolder$\\FSharpVSPowerTools.Logic.dll")>]
[<assembly: InternalsVisibleToAttribute("FSharpVSPowerTools.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharpVSPowerTools.Pure")>]
[<assembly: AssemblyProductAttribute("FSharpVSPowerTools.Pure")>]
[<assembly: AssemblyDescriptionAttribute("The Purest Source of F# Power in Visual Studio")>]
[<assembly: AssemblyVersionAttribute("2.2.0")>]
[<assembly: AssemblyFileVersionAttribute("2.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.2.0"
