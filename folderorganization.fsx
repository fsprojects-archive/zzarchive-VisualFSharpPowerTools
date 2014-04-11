(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Folder Organization
-----------------------

We currently support:

- adding folders to projects
- adding subfolders to existing folders
- renaming folders
- moving files to folders
- moving folders up/down

Here is how Solution Explorer context menu commands looks like for Folder Organization:

![Folder Organization Commands](img/FolderOrganization_ContextMenu.png)

Particular commands are enabled/disabled according to item you invoke the context menu on.

When you add new folder or rename existing one folder name must be provided into a dialog:

![New Folder Dialog](img/FolderOrganization_NewFolderDialog.png)


**This feature is disabled by default**

Because there are known issues with F# Project System in Visual Studio F# Tools, Folder Organization may not work 100% correctly in all cases.

**We do not advice you to introduce complicated folder structure within F# projects.** You should keep number of folders within your project as low as possible.

One of known issues causes problems when project contains two folders with the same name. Our *New Folder* and *Rename Folder* dialogs validates folders names across entire project to prevent that problem from occuring.

*)