#### 0.1.0 - February 5 2014
* Import Fantomas extension
* Import FSharpXMLDoc extension
* Add basic documentation

#### 0.1.1 - February 5 2014
* Add options to disable constituent parts

#### 0.2.0 - February 12 2014
* Add navigation bar setting
* XML doc comments are generated as the first '<' char in a blank "///" comment is entered
* Implement highlight references

#### 0.3.0 - February 25 2014
* Implement major improvements on highlight references
* Support operator highlighting
* Reuse C#'s highlighted reference marker
* Add rename refactoring

#### 0.3.1 - February 25 2014
* Fix the issue where multiple replacements are wrong if performed in a single line
* Use Ctrl + R, Ctrl + R key binding for rename refactoring

#### 0.3.2 - February 26 2014
* Fix the NRE when there is no project or solution

#### 0.4.0 - March 11 2014
* Import depth colorizer
* Implement NavigateTo command
* Fix performance issues with highlight references and rename refactoring
* Support highlight references, rename refactoring and NavigateTo on standalone files
* Listen to events due to project changes
* Improve F# project system

#### 0.4.1 - March 13 2014
* Fix the issue with standalone fs files
* Fix the issue with failing source code formatting feature

#### 0.5.0 - March 24 2014
* Improve Rename dialog
* Improve performance of NavigateTo feature
* Implement semantic highlighting
* Migrate to XAML provider in FsXaml
* Improve lexing performance
* Fix intermittent navigation bar configuration 
* Setup integration tests

#### 0.6.0 - April 14 2014
* Add folder organization support
* Enhance syntax coloring (now with coloring modules and quotations)
* Implement solution-wide rename refactoring
* Add logging facilities

#### 0.6.1 - April 14 2014
* Fix the issue where rename refactoring is applied after cancelled by 'Esc'

#### 1.0.0 - April 26 2014
* Implement Find all references
* Improve robustness of XmlDoc generation
* Improve performance on large solutions
* Add validation after reformatting

#### 1.1.0 - May 22 2014
* Add Implement Interface feature
* Fix duplicated symbols and missing private symbols in Find All References
* Enhance syntax coloring of type-provider-related source code
* Reduce the size of VSIX output
* Implement record stub generation
* Fix the issue where VFPT is active during CPU idles
* Return correct symbol information on dirty files and dirty projects

#### 1.2.0 - Unreleased
* Implement Union type pattern generation
* Fix various bugs on syntax coloring
* Improve responsiveness on big solutions
* Improve memory usage
* Protect against intermittent crashes


