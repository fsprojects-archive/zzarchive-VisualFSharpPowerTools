(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Navigate to Source
==============================

When the feature is enabled and users click Go to Definition (F12), Navigate to Source happens if:

 - current symbol originates from BCL libraries. The behaviour is to go to exact line in http://referencesource.microsoft.com/.
 - current symbol comes from an assembly with an associated source-indexed pdb file. The behaviour is to go to the source code provider that hosts associated source code.

Here is a screencast of the feature in action:

![Navigate to Source](img/navigate_to_source.gif)

I am a library author, what should I do to enable source indexing on my libraries?
-----------------------

You should run [SourceLink](http://ctaggart.github.io/SourceLink/) on pdb files and distribute these files with your NuGet packages. 
[ProjectScaffold](https://github.com/fsprojects/ProjectScaffold) also includes a SourceLink target to make the process easier to use.
Once the source-indexed pdb files are available, Navigate to Source should work as expected.

I am a normal user, what should I do to have Navigate to Source working?
-----------------------

Navigate to Source should work for any assembly if its source-indexed pdb file locates in the symbol cache directory.
You should enable source server support and set symbol cache directory -- see a simple guide at http://ctaggart.github.io/SourceLink/visualstudio.html. 
During the debugging process, pdb files will be downloaded and cached in this folder.
In the subsequent uses of F12 Navigate to Source, VFPT will pick up and use these pdb files.
 
*)