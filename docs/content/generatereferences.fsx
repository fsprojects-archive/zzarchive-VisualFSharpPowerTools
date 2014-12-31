(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Generate references in F# Interactive
-----------------------

The feature is originally written by Tao Liu (see [the relevant article](http://apollo13cn.blogspot.dk/2012/08/f-add-reference-addon.html)).
When being integrated to VFPT, the feature has been modified as follows:

 - Script files are generated for the project's active build configuration only.
 - The script files consists of relative paths for references.
 - The `load-references.fsx` script generates `#r` directives for all references in a given project.
 - The `load-project.fsx` script generates `#load` directives for all source files in a given project.

The feature can be invoked from 'Generate References in F# Interactive' command on a References node or a Project node itself (see the screenshot below).
 
![Generate references](img/generate_references.png)

*)