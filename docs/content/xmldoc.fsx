(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Auto-generating XmlDoc
-----------------------

This feature, originally written by Brian McNamara, auto creates Xml documentation boilerplate when you type triple-slash, following with `<` symbol.

For example, if you have the code:
*)

(*** hide ***)
module Version1 =
(**
*)
 type SomeType() =
  
     member this.Foo(x: int, s: string) = 
         printfn "%d: %s" x s

(**
and you type `///<` anywhere on the blank line before `Foo`, then you get:
*)

(*** hide ***)
module Version2 =
(**
*)
 type SomeType() =
    /// <summary>
    /// 
    /// </summary>
    /// <param name="x"></param>
    /// <param name="s"></param>
    member this.Foo(x:int, s:string) = 
        printfn "%d: %s" x s

(**
Just type `///<` (the slashes and `<` symbol may be delimited by any number of whitespace symbols) on a blank line right before a member, 
type, or module-scoped let (before any attributes, if there are any), 
and you get a blank XmlDoc template (if there wasn't already a non-empty XmlDoc there).
*)

(** This is how it looks like in action
![Auto generate Xml doc](img/auto_generate_xml_doc.gif)
*)
