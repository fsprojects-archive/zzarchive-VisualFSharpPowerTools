(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Union pattern match case generation
-----------------------------------

The feature is activated via a smart tagger on a union type or a union case constructor, inside a `match x with` or `function Case1 | Case2` expression. 
When the cursor is placed on one of these identifiers, the smart tagger will suggest users to automatically generate pattern match clauses.

Note that if the current line is `match x` or `function`, the code might not be parseable (i.e. no semantic highlighting for the current line).
We recommend users to generate pattern match cases while the code is parseable e.g. after writing `match x with Case1 -> ()` or `function Case1 -> ()`.

See the demo screencasts below.

Pattern match case generation

![Union pattern match case generation (non-qualified access)](img/pattern_match_case_generation_non_qualified.gif)

Lambda pattern match case generation

![Lambda union pattern match case generation (non-qualified access)](img/lambda_pattern_match_case_generation_non_qualified.gif)

Handles union types with qualified access

![Union pattern match case generation (qualified access)](img/pattern_match_case_generation_qualified.gif)

The default value of patterns' right-hand sides can be configured in 'F# Power Tools --> Code Generation' dialog.

![Code generation](img/code_generation_options.png)
*)