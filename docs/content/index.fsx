(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**

You can install this extension in Visual Studio 14, Visual Studio 2013 and Visual Studio 2012 by searching for "FSharp Power Tools" in "Tools --> Extensions and Updates --> Online".
The extension is also available in the [Visual Studio Gallery](http://visualstudiogallery.msdn.microsoft.com/136b942e-9f2c-4c0b-8bac-86d774189cff).

User guides
-----------------------

The extension comes with independent commands which can be toggled on/off in "Tools --> Options --> F# Power Tools --> General" dialog:

![General options](img/general_options.png)

More information is available on [Frequently Asked Questions](faq.html).

Here are currently supported features:

 * [Auto-generating XmlDoc](xmldoc.html)
 * [Source code formatting](codeformatting.html)
 * [Navigation bar](navbar.html)
 * [Highlight references](highlightusage.html)
 * [Rename refactoring](rename.html)
 * [Depth colorizer](depthcolorizer.html)
 * [NavigateTo](navigateto.html)
 * [Syntax coloring](syntaxcoloring.html)
 * [Folder organization](folderorganization.html)
 * [Find all references](findallreferences.html)
 * [Implement interface](implementinterface.html)
 * [Record stub generation](recordstubgeneration.html)
 * [Union pattern match case generation](unionpatternmatchcasegeneration.html)
 * [Resolve unopened namespaces and modules](resolveunopenednamespaces.html)
 * [Go to metadata](gotometadata.html)
 * [Task List comments](tasklistcomments.html)
 * [Generate references in F# Interactive](generatereferences.html)
 * [Auto-correct common C#-/JavaScript-developer typos in F#-files](commoncstypos.html)
  
Default Keymap
--------------

| Command                    | Hot-key            |
|----------------------------|--------------------|
| Rename refactoring         | Ctrl + R, Ctrl + R |
| NavigateTo                 | Ctrl + ,           |
| Find All References        | Shift + F12        |
| Go to metadata             | F12                |
| Formatting Document        | Ctrl + K, Ctrl + D |
| Formatting Selection       | Ctrl + K, Ctrl + F |
| Formatting Cursor Position | Ctrl + K, Ctrl + F |

If you are using Visual F# Power Tools along side with ReSharper 8 or below, there is a [known issue](http://youtrack.jetbrains.com/issue/RSRP-409199) where some F# commands are hidden by ReSharper. You can workaround by disabling the ReSharper option "Hide overridden Visual Studio menu items" (ReSharper -> Options -> Keyboard & Menus).
  
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. You can suggest new features at our [user voice system](http://vfpt.uservoice.com/). 
Please vote for your favourite features so that they have higher chances to be implemented. 
If you're adding new features, please also consider adding [user guides][content] that can be turned into documentation.

The extension is available under Apache 2.0 license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/VisualFSharpPowerTools/tree/master/docs/content
  [gh]: https://github.com/fsprojects/VisualFSharpPowerTools
  [issues]: https://github.com/fsprojects/VisualFSharpPowerTools/issues
  [readme]: https://github.com/fsprojects/VisualFSharpPowerTools/tree/master/README.md
  [license]: https://github.com/fsprojects/VisualFSharpPowerTools/tree/master/LICENSE.txt
*)
