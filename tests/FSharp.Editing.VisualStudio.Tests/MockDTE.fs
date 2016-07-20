namespace FSharp.Editing.VisualStudio.Tests

open System
open System.IO
open EnvDTE
open System.Collections
open System.Collections.Generic
open FSharp.Editing.ProjectSystem
open FSharp.Editing
open FSharp.Editing.VisualStudio


/// Create a simple mock DTE for an F#-only solution
type MockDTE() =
    let projects = Dictionary()
    let mutable filePath = Unchecked.defaultof<_>
    member __.AddProject(projectFileName: string, project: IProjectProvider) =
        match projects.TryGetValue(projectFileName) with
        | true, _ ->
            printfn "Project %s exists in DTE." projectFileName
            projects.[projectFileName] <- project
        | false, _ ->
            printfn "Adding %s to DTE." projectFileName
            projects.[projectFileName] <- project

    member __.GetProject(projectFileName) = 
            projects.[projectFileName]

    member __.SetActiveDocument(fileName) =
        filePath <- fileName

    interface DTE with
        member __.ActiveSolutionProjects = notimpl
        member __.ActiveWindow = notimpl
        member __.AddIns = notimpl
        member __.Application = notimpl
        member __.CommandBars = notimpl
        member __.CommandLineArguments = notimpl
        member __.Commands = notimpl
        member __.ContextAttributes = notimpl
        member __.DTE = notimpl
        member __.Debugger = notimpl
        member __.DisplayMode
            with get () = notimpl
            and set _v = notimpl
        member __.Documents = notimpl
        member __.Edition = notimpl
        member __.ExecuteCommand(_commandName, _commandArgs) = notimpl
        member __.FileName = notimpl
        member __.Find = notimpl
        member __.FullName = notimpl
        member __.GetObject(_name) = notimpl
        member __.Globals = notimpl
        member __.IsOpenFile with get (_viewKind, _fileName) = notimpl
        member __.ItemOperations = notimpl
        member __.LaunchWizard(_vSZFile, _contextParams) = notimpl
        member __.LocaleID = notimpl
        member __.Macros = notimpl
        member __.MacrosIDE = notimpl
        member __.MainWindow = notimpl
        member __.Mode = notimpl
        member __.Name = notimpl
        member __.ObjectExtenders = notimpl
        member __.OpenFile(_viewKind, _fileName) = notimpl
        member __.Properties with get (_category, _page) = notimpl
        member __.Quit() = notimpl
        member __.RegistryRoot = notimpl
        member __.SatelliteDllPath(_path, _name) = notimpl
        member __.SelectedItems = notimpl
        member __.SourceControl = notimpl
        member __.StatusBar = notimpl
        member __.SuppressUI
            with get () = notimpl
            and set _v = notimpl
        member __.UndoContext = notimpl
        member __.UserControl
            with get () = notimpl
            and set _v = notimpl
        member __.WindowConfigurations = notimpl
        member __.Windows = notimpl
        
        member x.ActiveDocument = 
            if String.IsNullOrEmpty(filePath) then
                invalidArg "filePath" "Invalid path for active document"
            else
                MockDocument(filePath, x) :> _

        member __.Events = MockEvents() :> _
        member x.Solution = MockSolution(projects, x) :> _
        member __.Version = "12.0"
 
and MockDocument(filePath, dte: DTE) =
    interface Document with
        member __.Activate() = notimpl
        member __.ActiveWindow = notimpl
        member __.ClearBookmarks() = notimpl
        member __.Close(_save) = notimpl
        member __.Collection = notimpl
        member __.Extender with get (_extenderName) = notimpl
        member __.ExtenderCATID = notimpl
        member __.ExtenderNames = notimpl
        member __.IndentSize = notimpl
        member __.Kind = notimpl
        member __.Language
            with get () = notimpl
            and set _v = notimpl
        member __.MarkText(_pattern, _flags) = notimpl
        member __.Name = notimpl
        member __.NewWindow() = notimpl
        member __.Object(_modelKind) = notimpl
        member __.Path = notimpl
        member __.PrintOut() = notimpl
        member __.ReadOnly
            with get () = notimpl
            and set _v = notimpl
        member __.Redo() = notimpl
        member __.ReplaceText(_findText, _replaceText, _flags) = notimpl
        member __.Save(_fileName) = notimpl
        member __.Saved
            with get () = notimpl
            and set _v = notimpl
        member __.Selection = notimpl
        member __.TabSize = notimpl
        member __.Type = notimpl
        member __.Undo() = notimpl
        member __.Windows = notimpl
        member __.DTE = dte
        member __.FullName = filePath
        member __.ProjectItem = dte.Solution.FindProjectItem filePath

and MockEvents() =
    interface Events with
        member __.BuildEvents = notimpl
        member __.CommandBarEvents with get (_commandBarControl) = notimpl
        member __.CommandEvents with get (_guid, _iD) = notimpl
        member __.DTEEvents = notimpl
        member __.DebuggerEvents = notimpl
        member __.DocumentEvents with get (_document) = notimpl
        member __.FindEvents = notimpl
        member __.GetObject(_name) = notimpl
        member __.MiscFilesEvents = notimpl
        member __.OutputWindowEvents with get (_pane) = notimpl
        member __.SelectionEvents = notimpl
        member __.SolutionEvents = notimpl
        member __.SolutionItemsEvents = notimpl
        member __.TaskListEvents with get (_filter) = notimpl
        member __.TextEditorEvents with get (_textDocumentFilter) = notimpl
        member __.WindowEvents with get (_windowFilter) = notimpl
        
and MockSolution(projects, dte: DTE) =
    interface IEnumerable with
        member __.GetEnumerator() = notimpl
        
    interface Solution with
        member __.AddFromFile(_fileName, _exclusive) = notimpl
        member __.AddFromTemplate(_fileName, _destination, _projectName, _exclusive) = notimpl        
        member __.AddIns = notimpl        
        member __.Close(_saveFirst) = notimpl        
        member __.Count = notimpl        
        member __.Create(_destination, _name) = notimpl        
        member __.DTE = notimpl        
        member __.Extender with get (_extenderName) = notimpl        
        member __.ExtenderCATID = notimpl        
        member __.ExtenderNames = notimpl        
        member __.FileName = notimpl        
        member __.FindProjectItem(fileName) = 
            let allProjects = projects |> Seq.map (|KeyValue|) |> Seq.map snd
            // Accept relative file path as an input
            let fileName = Path.GetFullPathSafe(fileName)
            match allProjects |> Seq.tryFind (fun project -> 
                    // Should compare and ignore cases since MsBuild can resolve differently.
                    Array.exists (fun path -> String.Equals(fileName, path, StringComparison.OrdinalIgnoreCase)) project.Project.SourceFiles) with
            | Some project ->
                MockProjectItem(fileName, project, dte) :> _
            | None -> null

        member __.FullName = notimpl        
        member __.GetEnumerator() = notimpl        
        member __.Globals = notimpl        
        member __.IsDirty with get () = notimpl and set (_v) = notimpl        
        member __.IsOpen = notimpl        
        member __.Item(_index) = notimpl        
        member __.Open(_fileName) = notimpl        
        member __.Parent = notimpl        
        member __.ProjectItemsTemplatePath(_projectKind) = notimpl        
        member __.Projects = 
            let projs =
                projects 
                |> Seq.map (|KeyValue|) 
                |> Seq.map snd
                |> Seq.map (fun p -> MockProject(p, dte) :> Project)
            { new Projects with
                  member __.Count = notimpl
                  member __.DTE = notimpl
                  member __.Item(_index) = notimpl
                  member __.Kind = notimpl
                  member __.Parent = notimpl
                  member __.Properties = notimpl

                  member __.GetEnumerator() = 
                      projs.GetEnumerator() :> _
                 
              interface IEnumerable with
                    member __.GetEnumerator() = 
                        projs.GetEnumerator() :> _ }
                
        member __.Properties = notimpl        
        member __.Remove(_proj) = notimpl        
        member __.SaveAs(_fileName) = notimpl        
        member __.Saved with get () = notimpl and set (_v) = notimpl        
        member __.SolutionBuild = notimpl        
        member __.TemplatePath with get (_projectType) = notimpl

and MockProject(project: IProjectProvider, _dte: DTE) = 
    interface Project with
        member __.CodeModel = notimpl
        member __.Collection = notimpl
        member __.ConfigurationManager = notimpl
        member __.DTE = notimpl
        member __.Delete() = notimpl
        member __.Extender with get (_extenderName) = notimpl
        member __.ExtenderCATID = notimpl
        member __.ExtenderNames = notimpl
        member __.FileName = notimpl
        member __.Globals = notimpl
        member __.IsDirty
            with get () = notimpl
            and set _v = notimpl
        member __.Name
            with get () = notimpl
            and set _v = notimpl
        member __.Object = notimpl
        member __.ParentProjectItem = notimpl
        member __.ProjectItems = notimpl
        member __.Properties = notimpl
        member __.Save(_fileName) = notimpl
        member __.SaveAs(_newFileName) = notimpl
        member __.Saved
            with get () = notimpl
            and set _v = notimpl
        member __.UniqueName = notimpl
        
        member __.FullName = 
            project.Project.ProjectFile

        member __.Kind = 
            FSharpProjectKind

and MockProjectItem(filePath, project: IProjectProvider, dte: DTE) =
    interface ProjectItem with
        member __.Collection = notimpl
        member __.ConfigurationManager = notimpl
        member __.DTE = dte
        member __.Delete() = notimpl
        member __.ExpandView() = notimpl
        member __.Extender with get (_extenderName) = notimpl
        member __.ExtenderCATID = notimpl
        member __.ExtenderNames = notimpl
        member __.FileCodeModel = notimpl
        member __.FileCount = notimpl
        member __.FileNames with get (_index) = notimpl
        member __.IsDirty
            with get () = notimpl
            and set _v = notimpl
        member __.IsOpen with get (_viewKind) = notimpl
        member __.Kind = notimpl
        member __.Name
            with get () = notimpl
            and set _v = notimpl
        member __.Object = notimpl
        member __.Open(_viewKind) = notimpl
        member __.ProjectItems = notimpl
        member __.Properties = notimpl
        member __.Remove() = notimpl
        member __.Save(_fileName) = notimpl
        member __.SaveAs(_newFileName) = notimpl
        member __.Saved
            with get () = notimpl
            and set _v = notimpl
        member __.SubProject = notimpl
        
        member __.ContainingProject = 
            if project.Project.IsForStandaloneScript then null
            else MockProject(project, dte) :> _

        member __.Document = 
            MockDocument(filePath, dte) :> _
