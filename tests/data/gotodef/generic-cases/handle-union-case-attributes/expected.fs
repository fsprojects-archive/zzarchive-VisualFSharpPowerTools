module File

type Union =
    | [<System.Diagnostics.Conditional ("MyConditional")>]
      [<System.Obsolete ("hello")>]
      Case1 of int
    | [<System.Obsolete ("cuir")>]
      Case2 of string
    | Case3
