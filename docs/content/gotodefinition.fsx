(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Go to definition
================

Go to definition is now implemented for symbols where source code is not available (or C# code), using the usual F12 keyboard shortcut.

We currently support:

- classes, structs
- modules
- union types
- records
- enums
- delegates
- exceptions

![Go to definition window](img/gotodefinition.png)

Limitations
-----------

We do not support the following types due to upstream limitations:

 - provided erased types
 - C# enums
*)