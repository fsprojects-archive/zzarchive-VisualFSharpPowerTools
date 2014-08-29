(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Resolve unopened namespaces and modules
-----------------------

If a type / function / value is declared in one of the current project's referenced assemblies, 
but its corresponding namespace or module is not yet opened, a smart tag appears which allows to open 
the namespace / module or suggests to use the fully qualified name.

Here is a demo screencast.

![Resolve unopened namespaces](img/resolve_unopened_namespaces.gif)

*)