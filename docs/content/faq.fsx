(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FAQ - Frequently Asked Questions
==============================

Why is unused declaration option divided into two separate options in v1.6.0?
-----------------------

There are some performance penalties on checking unused declarations. 
Until the performance issue is sorted out, we divide the option into two settings to provide more granularity to users.
The new options are disabled by default.

What is Strict mode? When should I use it?
-----------------------

`FSharp.Compiler.Service` (one of our main components) makes extensive use of caching.
In Strict mode, cache invalidation is done more aggressively. 
Consequently, type checking results are more up-to-date but there might be performance degradation.
This setting is disabled by default.
We recommend to use it if there are frequent intermittent errors on syntax coloring.

What is Diagnostic mode? When should I use it?
-----------------------

If Diagnostic mode is on, logging information will be dumped into Visual Studio Output window (particularly on F# Power Tools tab).
This information is valuable for bug fixing purpose.
If you encounter any bug, please take some time to record diagnostics information and include it for bug reports.

![Configuration dialog](img/configuration_dialog.png)

Why are object identifiers in members marked as unused?
-----------------------
If object identifiers (`x` in `x.Member`) aren't referred anywhere in the member bodies, they are understood as unused values.
In order to fix it, we recommend to use `__` or any identifier with `_` prefix (`_x` for example).
*)