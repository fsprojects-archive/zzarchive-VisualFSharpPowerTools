F# Power Tools
============

F# Community Power Tools (for Visual Studio)

### Why F# Power Tools?
F# Power Tools is a community effort to bring useful F# VS extensions into a single home for the following purposes:
 - Easy maintenance and upgrade
 - Sharing common functionalities and reducing conflicts between F# VS extensions
 - Adding new features which complement Visual F# Tools for VS

This project is made possible by excellent work in [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service)
and [FSharpBinding](https://github.com/fsharp/fsharpbinding).

### Currently supported features
 - Auto-generating XmlDoc (via [F# XmlDoc extension](http://lorgonblog.wordpress.com/2010/12/04/source-code-for-f-xmldoc-extension/))
 - Formatting document / Formatting selection (via [Fantomas extension](https://github.com/dungpa/fantomas))
 - Navigation bar (see [this article](http://tomasp.net/blog/regions-navigation.aspx/))
 - Highlight references
 - Rename refactoring

For user guides, please take a look at [the home page](http://fsprojects.github.io/FSharpVSPowerTools/).

### Build [![Build Status](https://ci-beta.appveyor.com/api/projects/status/aab9drboxx8mxegt)](https://ci-beta.appveyor.com/project/dungpa/fsharpvspowertools)

Require Visual Studio 2013 and Visual Studio SDK 2013. 
Run the `build.cmd` script or `FSharpVSPowerTools.sln` to build the solution.

### Getting involved
Contributions are always welcome.
The maintainers don't have much experience with Visual Studio Extensibility; any help is much appreciated.

### License
The extension is available under Apache 2.0 license. For more information see the [License file](License.txt).
