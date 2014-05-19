Visual F# Power Tools
=====================

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

You can suggest new features at our [user voice system](http://vfpt.uservoice.com/). 
Please vote for your favourite features so that they have higher chances to be implemented. 
For user guides, please take a look at [the home page](http://fsprojects.github.io/VisualFSharpPowerTools/).

### Build [![Build status](https://ci.appveyor.com/api/projects/status/9ii93nkif8hc6cdv)](https://ci.appveyor.com/project/dungpa/visualfsharppowertools)

Require Visual Studio 2013 and Visual Studio SDK 2013. 
Run the `build.cmd` script or `FSharpVSPowerTools.sln` to build the solution.

You can download the latest installer [here](https://ci.appveyor.com/api/buildjobs/683c775376wlog6q/artifacts/bin/FSharpVSPowerTools.vsix).

### Getting involved
Contributions are always welcome.
The maintainers don't have much experience with Visual Studio Extensibility; any help is much appreciated.

### License
The extension is available under Apache 2.0 license. For more information see the [License file](LICENSE.txt).
