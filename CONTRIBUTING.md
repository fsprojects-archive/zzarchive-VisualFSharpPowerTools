Contributing guideline
============

We have been receiving a lot of contributions from F# community and we love it. F# Power Tools welcomes your contributions. The purpose of this guideline is to make contribution to the project easier and more constructive.

### Using the issue tracker

 - *Bugs* - You welcome to fix them. We would love to receive pull requests for bug fixing.
 - *Features* - It is a good idea to discuss on the issue tracker first if you would like to implement new features.
 - `up-for-grabs` tag - This tag labels self-contained issues which can be solved without understanding too many details of the current code base. They are available for newcomers to take. When someone agrees to help with an `up-for-grab` issue, we will mark it as `taken` to avoid duplication of effort.

### Discussing features

Requests from F# community will be the source of our upcoming features. If you have a feature request or would like to help us implement a new feature, please open a new issue. If there is already a similar request on the issue tracker, please jump in and discuss; it will help us prioritize our work better.

### New to Visual Studio Extensibility (VSX)?
Don't be scared away. We all have been new to VSX.
Here are a few resources to get started with VSX:

 - [Inside VS Editor](http://msdn.microsoft.com/en-us/library/vstudio/dd885240.aspx) - This MSDN documentation contains many useful walkthroughs that you may find familiar in F# Power Tools code base.
 - [Resources for writing Visual Studio Extensions](https://github.com/jaredpar/VsVim/wiki/Resources-for-writing-Visual-Studio-Extensions) - Jared has compiled an excellent list of open source VS extensions where you can learn and find solutions for specific issues.

### How is the code base organized?

The code base is structured as follows:

 - [FSharpVSPowerTools.Core](src/FSharpVSPowerTools.Core) project consists of IDE-agnostic features that might be reused in other IDEs and editors.
 - [FSharpVSPowerTools.Logic](src/FSharpVSPowerTools.Logic) project contains most of VS-dependent operations that are required to implement specific features.
 - [FSharpVSPowerTools](src/FSharpVSPowerTools) is the entry-point project which registers the package, create MEF components and get user configurations via options dialogs.

When you hit F5 (Debug --> Start Debugging) on `FSharpVSPowerTools` project, an experimental VS instance will be opened with the extension being installed. You can set breakpoints in any part of the code to debug or check View --> Output window for debugging logs.

We use NUnit for unit testing. If you touch FSharpVSPowerTools.Core project, please consider to add unit tests to [FSharpVSPowerTools.Core.Tests](tests/FSharpVSPowerTools.Core.Tests) project.

We use a FAKE script to build the solution, run unit tests and create user guides. If you implement/modify features, please adjust documentation in [content](docs/content) folder.

### Code Styles

When you consider to contribute, please follow the following coding conventions:
 
 - Use four spaces for indentation
 - Use camelCase for module functions and PascalCase for type members.
 - When you refer to class instances, use `x` in each  member. If you reference to the instances in constructors, we generally recommend to use `as self` pattern.
 - We turn on unused-variable warnings by default, please remove all unused identifiers before sending pull requests. When suppressing unused variables, you could use `_` prefix to give them mnemonic names.

We recommend these two sources for F# formatting conventions and coding styles:
 
 - [F# Formatting Conventions](https://github.com/dungpa/fantomas/blob/master/docs/FormattingConventions.md)
 - [F# Component Design Guidelines](http://fsharp.org/about/files/guidelines.pdf)


That's it for now. We look forward to your contributions.


