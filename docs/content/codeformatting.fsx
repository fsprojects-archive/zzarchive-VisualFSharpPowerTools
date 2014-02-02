(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Code Formatting
-----------------------

Two main formatting commands are available:
 
 - *Formatting Document*, available under **Ctrl + K D** key combination.
 - *Formatting Selection / Formatting Cursor Position*, available under **Ctrl + K F** key combination.

Using **Ctrl + K F** combination without a selection, 
the smallest parseable block (inside `[` and `]`, `[|` and `|]`, `{` and `}` or `(` and `)`) will be formatted.

To illustrate, the following example
*)

(*** hide ***)
module Hidden1 =
(**
*)
    type Type
        = TyLam of Type * Type
        | TyVar of string
        | TyCon of string * Type list
        with override this.ToString () =
                match this with
                | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
                | TyVar a -> a
                | TyCon (s, ts) -> s

(** will be rewritten to *)
(*** hide ***)
module Hidden2 =
(**
*)
    type Type = 
        | TyLam of Type * Type
        | TyVar of string
        | TyCon of string * Type list
        override this.ToString() = 
            match this with
            | TyLam(t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon(s, ts) -> s

(**
The main formatting options are available under "Tools --> Options --> Fantomas" dialog. 

![Fantomas options](img/fantomas_options.png)

To be consistent with Visual Studio editors, the last option, *indent size*, 
can be adjusted under "Tools --> Options --> Text Editor --> F# --> Tabs" (looking for "Indent size" option).
*)