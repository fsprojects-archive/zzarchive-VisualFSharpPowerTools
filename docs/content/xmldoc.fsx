(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Auto-generating XMLDoc
-----------------------

This feature auto creates xml documentation boilerplate when you type triple-slash.

For example, if you have the code:
*)

type SomeType1() =

    member this.Foo(x:int, s:string) = 
        printfn "%d: %s" x s

(**
and you type `///` anywhere on the blank line before `Foo`, then you get:
*)

type SomeType2() =
   /// <summary>
   /// 
   /// </summary>
   /// <param name="x"></param>
   /// <param name="s"></param>
   member this.Foo(x:int, s:string) = 
       printfn "%d: %s" x s

(**
Just type `///` on a blank line right before a member, type, or module-scoped let (before any attributes, if there are any), 
and you get a blank XMLDoc template (if there wasn't already an XMLDoc there).
*)