(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Record stub generation
-----------------------

The feature is activated via a smart tagger on a record type or a record field identifier. 
When the cursor is placed on one of these identifiers, the smart tagger will hint users at generating record field definitions.

Note that if the current line is `let x = {` or `let x: Record = {`, the code might not be parseable (i.e. no semantic highlighting for the current line).
We recommend users to generate field stubs while the code is parseable e.g. after writing `let x: Record = { }` or `{ Field1 = defaultValue }`.

See the demo screencasts below.

![Record stub generation (typed binding)](img/record_stub_generation_typed_binding.gif)

![Record stub generation (qualified field)](img/record_stub_generation_qualified_field.gif)

![Record stub generation (non-qualified field)](img/record_stub_generation_non_qualified_field.gif)

The default value of record fields can be configured in 'F# Power Tools --> Code Generation' dialog.

![Code generation](img/code_generation_options.png)
*)