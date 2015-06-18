(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Implement interface
-----------------------

The feature is activated via a smart tagger on an interface identifier. 
When the cursor is placed on an identifier of interface declaration or object expression, the smart tagger will hint users at generating relevant interfaces.

Note that if the current line is `interface ISomeInterface with` or `new ISomeInterface with`, the code might not be parseable (i.e. no semantic highlighting for the current line).
We recommend users to generate interfaces while the code is parseable e.g. after writing `interface ISomeInterface` or `new ISomeInterface`.

See the demo screencast below.

![Implement interface](img/implement_interface.gif)

The default body of interface methods can be configured in 'F# Power Tools --> Code Generation' dialog.

![Code generation](img/code_generation_options.png)

*)