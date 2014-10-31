(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Task List comments
-----------------------

This feature displays the comments specified in Tools -> Options -> Task List in Visual Studio's Task List window.

For example, if you have the code:
*)

// TODO examine literature for more precise value
let mutable standardMagnitude = 20
standardMagnitude <- 23 // HACK this seems to fix the problem

(**
the Task List will contain 2 entries similar to

| Description                                     | Line, Column       |
|-------------------------------------------------|--------------------|
| TODO examine literature for more precise value  | 1, 4               |
| HACK this seems to fix the problem              | 3, 28              |

*)