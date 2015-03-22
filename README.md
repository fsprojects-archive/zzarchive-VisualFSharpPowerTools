[![Issue Stats](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools/badge/issue)](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools)
[![Issue Stats](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools/badge/pr)](http://issuestats.com/github/fsprojects/VisualFSharpPowerTools)
[![NuGet](https://img.shields.io/nuget/dt/FSharpVSPowerTools.Core.svg)]()

Visual F# Power Tools
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

### Currently supported features
 - Auto-generating XmlDoc (via [F# XmlDoc extension](http://lorgonblog.wordpress.com/2010/12/04/source-code-for-f-xmldoc-extension/))
 - Formatting document / Formatting selection (via [Fantomas extension](https://github.com/dungpa/fantomas))
 - Navigation bar (see [this article](http://tomasp.net/blog/regions-navigation.aspx/))
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

You can suggest new features at our [user voice system](http://vfpt.uservoice.com/). 
Please vote for your favourite features so that they have higher chances to be implemented. 
For user guides, please take a look at [the home page](http://fsprojects.github.io/VisualFSharpPowerTools/).

### Build [![Build status](https://ci.appveyor.com/api/projects/status/9ii93nkif8hc6cdv/branch/master)](https://ci.appveyor.com/project/dungpa/visualfsharppowertools)

Require Visual Studio 2013 and Visual Studio 2013 SDK. 
Run the `build.cmd` script or `FSharpVSPowerTools.sln` to build the solution.

You can download the latest installer [here](https://ci.appveyor.com/project/dungpa/visualfsharppowertools/branch/master/artifacts).

### Getting involved

You can ask questions regarding the project on GitHub issues or on Twitter (tweeting to [@FSPowerTools](https://twitter.com/FSPowerTools) and [#fsharp](https://twitter.com/search?f=realtime&q=%23fsharp&src=typd) hashtag).

Contributions are always welcome.
The maintainers don't have much experience with Visual Studio Extensibility; any help is much appreciated.

### License
The extension is available under Apache 2.0 license. For more information see the [License file](LICENSE.txt).

### Maintainer(s)

- [@dungpa](https://github.com/dungpa)
- [@vasily-kirichenko](https://github.com/vasily-kirichenko)
- [@OkayX6](https://github.com/OkayX6)
- [@forki](https://github.com/forki)

The default maintainer account for projects under "fsprojects" is [@fsgit](https://github.com/fsgit) - F# Community Project Incubation Space (repo management)
