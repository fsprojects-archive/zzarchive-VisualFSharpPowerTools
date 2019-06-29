[![Issue Stats](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools/badge/issue)](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools)
[![Issue Stats](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools/badge/pr)](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools)
[![NuGet](https://buildstats.info/nuget/FSharpVSPowerTools.Core)](https://www.nuget.org/packages/FSharpVSPowerTools.Core)

## This project is deprecated. Most of the functionality has been ported to [Visual F# Tools](https://github.com/dotnet/fsharp), Visual Studio 2017 and newer. As of year 2019, we recommend to use JetBrains Rider, which has the most robust F# support (however, not all features are implemented there yet) ##

[Visual F# Power Tools](https://visualstudiogallery.msdn.microsoft.com/136b942e-9f2c-4c0b-8bac-86d774189cff)
=====================

[![Join the chat at https://gitter.im/fsprojects/VisualFSharpPowerTools](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/fsprojects/VisualFSharpPowerTools?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Power commands for F# in Visual Studio

### Why Visual F# Power Tools?
Visual F# Power Tools is a community effort to bring useful F# VS extensions into a single home for the following purposes:
 - Easy maintenance and upgrade
 - Sharing common functionalities and reducing conflicts between F# VS extensions
 - Adding new features which complement Visual F# Tools

This project is made possible by excellent work in [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service)
and [FSharpBinding](https://github.com/fsharp/fsharpbinding).

> **NOTE:** Since v2.0.0, we no longer officially support Visual Studio 2012.
An archived v2.0.0 compatible with Visual Studio 2012 is kept at [AppVeyor deployment](https://ci.appveyor.com/project/dungpa/visualfsharppowertools/branch/vs2012/artifacts)
 in case someone needs it.
Alternatively, one can build [vs2012 branch](https://github.com/fsprojects/VisualFSharpPowerTools/tree/vs2012) for Visual Studio 2012 support.

### Currently supported features
 - Auto-generating XmlDoc (via [F# XmlDoc extension](http://lorgonblog.wordpress.com/2010/12/04/source-code-for-f-xmldoc-extension/))
 - Formatting document / Formatting selection (via [Fantomas extension](https://github.com/dungpa/fantomas))
 - Highlight references
 - Rename refactoring
 - Depth colorizer
 - NavigateTo
 - Syntax coloring
 - Folder organization
 - Find all references
 - Implement interface
 - Record stub generation
 - Union type pattern generation
 - Resolve unopened namespaces
 - Gray out unused declarations
 - Gray out unused open statements
 - Go to metadata
 - Task List comments
 - Generate references for F# Interactive (see [this article](http://apollo13cn.blogspot.dk/2012/08/f-add-reference-addon.html))
 - Navigate to source
 - Quick info panel
 - FSharpLint integration
 - Outlining
 - Peek Definition (VS 2015+)

You can suggest new features at our [user voice system](http://vfpt.uservoice.com/). 
Please vote for your favourite features so that they have higher chances to be implemented. 
For user guides, please take a look at [the home page](http://fsprojects.github.io/VisualFSharpPowerTools/).

### Build 

<table>
  <tr>
    <td colspan="2">Build status</td>
  </tr>
  <tr>
    <td>Windows (AppVeyor)</td>
    <td><a href="https://ci.appveyor.com/project/dungpa/visualfsharppowertools"><img src="https://ci.appveyor.com/api/projects/status/9ii93nkif8hc6cdv/branch/master"></a></td>
  </tr>
  <tr>
    <td>Linux (Travis)</td>
    <td><a href="https://travis-ci.org/fsprojects/VisualFSharpPowerTools"><img src="https://travis-ci.org/fsprojects/VisualFSharpPowerTools.svg?branch=master"></a></td>
  </tr>
</table>

Require Visual Studio 2013 and Visual Studio 2013 SDK. 
Run the `build.cmd` script or `FSharpVSPowerTools.sln` to build the solution.

You can download the latest installer [here](https://ci.appveyor.com/project/dungpa/visualfsharppowertools/branch/master/artifacts).

### Getting involved

You can ask questions regarding the project on GitHub issues or on Twitter (tweeting to [@FSPowerTools](https://twitter.com/FSPowerTools) and [#fsharp](https://twitter.com/search?f=realtime&q=%23fsharp&src=typd) hashtag).

Contributions are always welcome.
The maintainers don't have much experience with Visual Studio Extensibility; any help is much appreciated.

### License
The extension is available under Apache 2.0 license. For more information see the [License file](LICENSE.txt).

### Maintainers

- [@dungpa](https://github.com/dungpa)
- [@vasily-kirichenko](https://github.com/vasily-kirichenko)
- [@OkayX6](https://github.com/OkayX6)
- [@forki](https://github.com/forki)
- [@cloudRoutine](https://github.com/cloudRoutine)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
