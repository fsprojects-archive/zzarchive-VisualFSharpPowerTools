(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Syntax coloring
-----------------------

We currently support coloring:

- types
- value types
- modules
- functions / methods (off by default)
- quotations
- patterns (off by default)
- mutable variables / reference cells (off by default)
- unused non public types, methods, functions and values declarations
- unused open statements

There is a special user setting for enabling graying out unused declarations and open statements.

If unused declarations and open statements are found in current file, orange markers will appear on top of vertical scroll bar.
You can navigate to lines consisting of unused items by clicking on these markers (see the screenshot below).
![Syntax coloring](img/syntax_coloring_unused_decl.png)

Here is how the editor looks like in blue & dark theme:

![Syntax coloring](img/syntax_coloring_demo_blue.png)

![Syntax coloring](img/syntax_coloring_demo_dark.png)


How to customize colors
-----------------------

Colors of syntax constructs can be customized in "Tools --> Options --> Environment --> Fonts and Colors --> Display items" via:

- F# Functions / Methods
- F# Modules
- F# Mutable Variables / Reference Cells
- F# Patterns
- F# Quotations
- F# Types
- F# Value Types
- F# Unused Items

You can press Ctrl + F in 'Display items' to quickly navigate to these colors.

![Syntax coloring](img/syntax_coloring_options.png)

*)