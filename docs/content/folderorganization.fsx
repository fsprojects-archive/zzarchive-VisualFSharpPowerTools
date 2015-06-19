(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Folder Organization
===================

> **NOTE:** Folder organization feature doesn't change the order of source files in your projects. You still have to follow the linear order of F# source files as shown in Solution Explorer.


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


**This feature is disabled by default**. 
Because there are known issues with F# Project System in Visual F# Tools, folder organization may not work 100% correctly in all cases.

 - **We do not advice you to introduce complicated folder structure within F# projects.
 - ** You should keep number of folders within your project as low as possible.

Limitations
-----------

One of known issues causes problems when project contains two folders with the same name. 
Our *New Folder* and *Rename Folder* dialogs validates folders names across entire project to prevent that problem from occurring.

Other issues:

 - An empty folder will disappear after saving if you don't add any file to it.
 - Renaming a file could make it jump out of the current folder. 
It's a UI bug, if you close the project and reopen it, Solution Explorer shows correct folder structure again. 
Alternatively you can move files out of the folder and move them into the folder again using menus.
 - Renaming a folder using standard F2 shortcut (in-place editing) is buggy due to issues inside F# project system. We advise you to use 'F# Power Tools --> Rename Folder' instead.
*)