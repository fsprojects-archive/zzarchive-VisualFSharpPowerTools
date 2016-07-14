namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("FSharp.Editing.Tests")>]
[<assembly: AssemblyTitleAttribute("FSharp.Editing")>]
[<assembly: AssemblyProductAttribute("FSharp.Editing")>]
[<assembly: AssemblyDescriptionAttribute("Support for F# tooling")>]
[<assembly: AssemblyVersionAttribute("2.6.0")>]
[<assembly: AssemblyFileVersionAttribute("2.6.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.6.0"
    let [<Literal>] InformationalVersion = "2.6.0"
