#### 1.9.0 - June 21 2015
* Implement QuickInfo margin ([#973](https://github.com/fsprojects/VisualFSharpPowerTools/issues/973))
* Fix UI freezes by moving computation to background threads ([#975](https://github.com/fsprojects/VisualFSharpPowerTools/issues/975))
* Improve folder organization feature ([#969](https://github.com/fsprojects/VisualFSharpPowerTools/issues/969), [#986](https://github.com/fsprojects/VisualFSharpPowerTools/issues/986))
* Fix source code formatting bugs ([fantomas#168](https://github.com/dungpa/fantomas/pull/168))
* Optimize for better responsiveness ([#1001](https://github.com/fsprojects/VisualFSharpPowerTools/pull/1001), [#1002](https://github.com/fsprojects/VisualFSharpPowerTools/pull/1002))
* Improve union pattern match case generation feature ([#1003](https://github.com/fsprojects/VisualFSharpPowerTools/pull/1003))
* Enhance unused declaration margin feature ([#1012](https://github.com/fsprojects/VisualFSharpPowerTools/pull/1012))
* Create a single HighlightUsageTagger for each document ([#1021](https://github.com/fsprojects/VisualFSharpPowerTools/pull/1021))

#### 1.8.0 - April 2 2015
* Support operator coloring ([#936](https://github.com/fsprojects/VisualFSharpPowerTools/issues/936))
* Implement Navigate to Source ([#945](https://github.com/fsprojects/VisualFSharpPowerTools/issues/945))
* Implement assembly resolution for FsiReferenceCommand ([#949](https://github.com/fsprojects/VisualFSharpPowerTools/pull/949))
* Fix missing colors in VS 2012 ([#952](https://github.com/fsprojects/VisualFSharpPowerTools/pull/952))
* No longer report synthetic range symbols ([#954](https://github.com/fsprojects/VisualFSharpPowerTools/pull/954))
* Generate signatures of enclosing modules of active patterns ([#955](https://github.com/fsprojects/VisualFSharpPowerTools/pull/955))

#### 1.7.3 - February 20 2015
* Find all references and Rename refactoring does not search for private/internal symbol usages in projects other than ones where it's declared 
([#932](https://github.com/fsprojects/VisualFSharpPowerTools/issues/932))
* Fix inconsistent code coloring ([#933](https://github.com/fsprojects/VisualFSharpPowerTools/pull/933))

#### 1.7.2 - February 5 2015
* Fix wrong recognition of dark themes on VS 2015 ([#917](https://github.com/fsprojects/VisualFSharpPowerTools/issues/917))
* Generate references for F# Interactive preserve references order if users override them ([#921](https://github.com/fsprojects/VisualFSharpPowerTools/issues/921))
* Remove dependencies on .NET 4.5, which cause installation errors on VS 2015 ([#924](https://github.com/fsprojects/VisualFSharpPowerTools/issues/924))

#### 1.7.0 - January 2 2015
* Fix various bugs on Go to Metadata ([#865](https://github.com/fsprojects/VisualFSharpPowerTools/issues/865), [#867](https://github.com/fsprojects/VisualFSharpPowerTools/issues/867), [#891](https://github.com/fsprojects/VisualFSharpPowerTools/issues/891))
* Check graying out unused declarations/opens in a separate coloring stage ([#852](https://github.com/fsprojects/VisualFSharpPowerTools/issues/852))
* Improve performance of Find All References when symbols appear on many documents ([#839](https://github.com/fsprojects/VisualFSharpPowerTools/issues/839))
* Report printf specifiers from user-defined printf-like functions ([#877](https://github.com/fsprojects/VisualFSharpPowerTools/issues/877))
* Implement Next and Previous Highlighted Reference commands ([#899](https://github.com/fsprojects/VisualFSharpPowerTools/issues/899))
* Automatically update depth colorizer when VS theme changes ([#901](https://github.com/fsprojects/VisualFSharpPowerTools/issues/901))
* Integrate AddReferenceInFSI extension as Generate References for F# Interactive feature ([#904](https://github.com/fsprojects/VisualFSharpPowerTools/pull/904))
* No longer reopen generated signatures and go to exact symbol locations on signature files ([#906](https://github.com/fsprojects/VisualFSharpPowerTools/issues/906))

#### 1.6.1 - November 6 2014
* Fix a concurrency bug ([issue](https://github.com/fsprojects/VisualFSharpPowerTools/issues/822) & [fix](https://github.com/fsharp/FSharp.Compiler.Service/pull/252))
* Fix displaying underscores in resolving unopened namespaces ([issue](https://github.com/fsprojects/VisualFSharpPowerTools/issues/823) & [fix](https://github.com/fsprojects/VisualFSharpPowerTools/pull/826))
* Fix issues with string highlighting in VS 2013 ([issue](https://github.com/fsprojects/VisualFSharpPowerTools/issues/825) & [fix](https://github.com/fsprojects/VisualFSharpPowerTools/pull/837))

#### 1.6.0 - November 4 2014
* Add syntax coloring for generated signatures
* Fix various bugs on syntax coloring for printf specifiers
* Support multiline strings in printf formatting
* Implement Task List Comments
* Fix issues with retrieving wrong active documents
* Add syntax coloring for escaped characters in strings
* Add Strict mode to control granularity of caching
* Add Diagnostic mode to dump logging information to VS Output window

#### 1.5.2 - October 8 2014
* Add syntax coloring for printf specifiers
* Fix project load times to point to last changed files
* Fix exceptions on FullOutputFilePath

#### 1.5.0 - October 2 2014
* Support more complex scenarios in VS unit tests
* Fix various issues with union case generation
* Implement Go to Metadata

#### 1.4.5 - August 28 2014
* Fix forcing saving projects when project items change
* Fix various issues on unused open declarations

#### 1.4.0 - August 10 2014
* Improve rename refactoring validation
* Add support for VS 14
* Implement Gray out unused open statements
* Handle included files while working on F# scripts
* Add VS unit tests for multiple features
* Implement unused declaration scrollbar markers

#### 1.3.0 - July 10 2014
* Add report progress and cancellation to Rename Refactoring
* Add report progress to Find All References
* Enhance Xaml support
* Implement Resolve unopened namespaces
* Implement Gray out unused declarations
* Add a NuGet package for the core project

#### 1.2.0 - June 17 2014
* Implement Union type pattern generation
* Fix various bugs on syntax coloring
* Improve responsiveness on big solutions
* Improve memory usage
* Protect against intermittent crashes

#### 1.1.0 - May 22 2014
* Add Implement Interface feature
* Fix duplicated symbols and missing private symbols in Find All References
* Enhance syntax coloring of type-provider-related source code
* Reduce the size of VSIX output
* Implement record stub generation
* Fix the issue where VFPT is active during CPU idles
* Return correct symbol information on dirty files and dirty projects

#### 1.0.0 - April 26 2014
* Implement Find all references
* Improve robustness of XmlDoc generation
* Improve performance on large solutions
* Add validation after reformatting

#### 0.6.1 - April 14 2014
* Fix the issue where rename refactoring is applied after cancelled by 'Esc'

#### 0.6.0 - April 14 2014
* Add folder organization support
* Enhance syntax coloring (now with coloring modules and quotations)
* Implement solution-wide rename refactoring
* Add logging facilities

#### 0.5.0 - March 24 2014
* Improve Rename dialog
* Improve performance of NavigateTo feature
* Implement semantic highlighting
* Migrate to XAML provider in FsXaml
* Improve lexing performance
* Fix intermittent navigation bar configuration 
* Setup integration tests

#### 0.4.1 - March 13 2014
* Fix the issue with standalone fs files
* Fix the issue with failing source code formatting feature

#### 0.4.0 - March 11 2014
* Import depth colorizer
* Implement NavigateTo command
* Fix performance issues with highlight references and rename refactoring
* Support highlight references, rename refactoring and NavigateTo on standalone files
* Listen to events due to project changes
* Improve F# project system

#### 0.3.2 - February 26 2014
* Fix the NRE when there is no project or solution

#### 0.3.1 - February 25 2014
* Fix the issue where multiple replacements are wrong if performed in a single line
* Use Ctrl + R, Ctrl + R key binding for rename refactoring

#### 0.3.0 - February 25 2014
* Implement major improvements on highlight references
* Support operator highlighting
* Reuse C#'s highlighted reference marker
* Add rename refactoring

#### 0.2.0 - February 12 2014
* Add navigation bar setting
* XML doc comments are generated as the first '<' char in a blank "///" comment is entered
* Implement highlight references

#### 0.1.1 - February 5 2014
* Add options to disable constituent parts

#### 0.1.0 - February 5 2014
* Import Fantomas extension
* Import FSharpXMLDoc extension
* Add basic documentation
