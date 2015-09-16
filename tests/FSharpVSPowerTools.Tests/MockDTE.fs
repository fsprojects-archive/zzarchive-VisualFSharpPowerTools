namespace FSharpVSPowerTools.Tests

open System
open System.IO
open EnvDTE
open System.Collections
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open System.Collections.Generic
open FSharpVSPowerTools.Tests.TestHelpers
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
        member x.ActiveDocument: Document = 
            if String.IsNullOrEmpty(filePath) then
                invalidArg "filePath" "Invalid path for active document"
            else
                MockDocument(filePath, x) :> _

        member __.ActiveSolutionProjects: obj = notimpl
        member __.ActiveWindow: Window = notimpl
        member __.AddIns: AddIns = notimpl
        member __.Application: DTE = notimpl
        member __.CommandBars: obj = notimpl
        member __.CommandLineArguments: string = notimpl
        member __.Commands: Commands = notimpl
        member __.ContextAttributes: ContextAttributes = notimpl
        member __.DTE: DTE = notimpl
        member __.Debugger: Debugger = notimpl
        member __.DisplayMode with get (): vsDisplay = notimpl and set (_v: vsDisplay): unit = notimpl
        member __.Documents: Documents = notimpl
        member __.Edition: string = notimpl
        member __.Events: Events = 
            MockEvents() :> _
        
        member __.ExecuteCommand(_commandName: string, _commandArgs: string): unit = notimpl
        member __.FileName: string = notimpl
        member __.Find: Find = notimpl
        member __.FullName: string = notimpl
        member __.GetObject(_name: string): obj = notimpl
        member __.Globals: Globals = notimpl
        member __.IsOpenFile with get (_viewKind: string, _fileName: string): bool = notimpl
        member __.ItemOperations: ItemOperations = notimpl
        member __.LaunchWizard(_vSZFile: string, _contextParams: byref<obj []>): wizardResult = notimpl
        member __.LocaleID: int = notimpl
        member __.Macros: Macros = notimpl
        member __.MacrosIDE: DTE = notimpl
        member __.MainWindow: Window = notimpl
        member __.Mode: vsIDEMode = notimpl
        member __.Name: string = notimpl
        member __.ObjectExtenders: ObjectExtenders = notimpl
        member __.OpenFile(_viewKind: string, _fileName: string): Window = notimpl
        member __.Properties with get (_category: string, _page: string): Properties = notimpl
        member __.Quit(): unit = notimpl
        member __.RegistryRoot: string = notimpl
        member __.SatelliteDllPath(_path: string, _name: string): string = notimpl
        member __.SelectedItems: SelectedItems = notimpl
        member x.Solution: Solution = 
            MockSolution(projects, x) :> _
        
        member __.SourceControl: SourceControl = notimpl
        member __.StatusBar: StatusBar = notimpl
        member __.SuppressUI with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.UndoContext: UndoContext = notimpl
        member __.UserControl with get (): bool = notimpl and set (_v: bool): unit = notimpl        
        member __.Version: string = 
            "12.0"
                    
        member __.WindowConfigurations: WindowConfigurations = notimpl        
        member __.Windows: Windows = notimpl

and MockDocument(filePath: string, dte: DTE) =
    interface Document with
        member __.Activate(): unit = notimpl
        member __.ActiveWindow: Window = notimpl
        member __.ClearBookmarks(): unit = notimpl
        member __.Close(_save: vsSaveChanges): unit = notimpl
        member __.Collection: Documents = notimpl
        member __.DTE: DTE = dte
        member __.Extender with get (_extenderName: string): obj = notimpl
        member __.ExtenderCATID: string = notimpl
        member __.ExtenderNames: obj = notimpl
        member __.FullName: string = 
            filePath

        member __.IndentSize: int = notimpl
        member __.Kind: string = notimpl
        member __.Language with get (): string = notimpl and set (_v: string): unit = notimpl
        member __.MarkText(_pattern: string, _flags: int): bool = notimpl
        member __.Name: string = notimpl
        member __.NewWindow(): Window = notimpl
        member __.Object(_modelKind: string): obj = notimpl
        member __.Path: string = notimpl
        member __.PrintOut(): unit = notimpl
        member __.ProjectItem: ProjectItem = 
            dte.Solution.FindProjectItem filePath

        member __.ReadOnly with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.Redo(): bool = notimpl
        member __.ReplaceText(_findText: string, _replaceText: string, _flags: int): bool = notimpl
        member __.Save(_fileName: string): vsSaveStatus = notimpl
        member __.Saved with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.Selection: obj = notimpl
        member __.TabSize: int = notimpl
        member __.Type: string = notimpl
        member __.Undo(): bool = notimpl
        member __.Windows: Windows = notimpl        

and MockEvents() =
    interface Events with
        member __.BuildEvents: BuildEvents = notimpl
        member __.CommandBarEvents with get (_commandBarControl: obj): obj = notimpl        
        member __.CommandEvents with get (_guid: string, _iD: int): CommandEvents = notimpl        
        member __.DTEEvents: DTEEvents = notimpl        
        member __.DebuggerEvents: DebuggerEvents = notimpl        
        member __.DocumentEvents with get (_document: Document): DocumentEvents = notimpl        
        member __.FindEvents: FindEvents = notimpl        
        member __.GetObject(_name: string): obj = notimpl        
        member __.MiscFilesEvents: ProjectItemsEvents = notimpl        
        member __.OutputWindowEvents with get (_pane: string): OutputWindowEvents = notimpl        
        member __.SelectionEvents: SelectionEvents = notimpl        
        member __.SolutionEvents: SolutionEvents = notimpl        
        member __.SolutionItemsEvents: ProjectItemsEvents = notimpl        
        member __.TaskListEvents with get (_filter: string): TaskListEvents = notimpl        
        member __.TextEditorEvents with get (_textDocumentFilter: TextDocument): TextEditorEvents = notimpl        
        member __.WindowEvents with get (_windowFilter: Window): WindowEvents = notimpl
        
and MockSolution(projects, dte: DTE) =
    interface IEnumerable with
        member __.GetEnumerator(): IEnumerator = notimpl
        
    interface Solution with
        member __.AddFromFile(_fileName: string, _exclusive: bool): Project = notimpl
        member __.AddFromTemplate(_fileName: string, _destination: string, _projectName: string, _exclusive: bool): Project = notimpl        
        member __.AddIns: AddIns = notimpl        
        member __.Close(_saveFirst: bool): unit = notimpl        
        member __.Count: int = notimpl        
        member __.Create(_destination: string, _name: string): unit = notimpl        
        member __.DTE: DTE = notimpl        
        member __.Extender with get (_extenderName: string): obj = notimpl        
        member __.ExtenderCATID: string = notimpl        
        member __.ExtenderNames: obj = notimpl        
        member __.FileName: string = notimpl        
        member __.FindProjectItem(fileName: string): ProjectItem = 
            let allProjects = projects |> Seq.map (|KeyValue|) |> Seq.map snd
            // Accept relative file path as an input
            let fileName = Path.GetFullPathSafe(fileName)
            match allProjects |> Seq.tryFind (fun project -> 
                    // Should compare and ignore cases since MsBuild can resolve differently.
                    Array.exists (fun path -> String.Equals(fileName, path, StringComparison.OrdinalIgnoreCase)) project.SourceFiles) with
            | Some project ->
                MockProjectItem(fileName, project, dte) :> _
            | None -> null

        member __.FullName: string = notimpl        
        member __.GetEnumerator(): Collections.IEnumerator = notimpl        
        member __.Globals: Globals = notimpl        
        member __.IsDirty with get (): bool = notimpl and set (_v: bool): unit = notimpl        
        member __.IsOpen: bool = notimpl        
        member __.Item(_index: obj): Project = notimpl        
        member __.Open(_fileName: string): unit = notimpl        
        member __.Parent: DTE = notimpl        
        member __.ProjectItemsTemplatePath(_projectKind: string): string = notimpl        
        member __.Projects: Projects = 
            let projs =
                projects 
                |> Seq.map (|KeyValue|) 
                |> Seq.map snd
                |> Seq.map (fun p -> MockProject(p, dte) :> Project)
            { new Projects with
                  member x.Count: int = 
                      notimpl
                  
                  member x.DTE: DTE = 
                      notimpl
                  
                  member x.GetEnumerator(): IEnumerator = 
                      projs.GetEnumerator() :> IEnumerator
                  
                  member x.Item(index: obj): Project = 
                      notimpl
                  
                  member x.Kind: string = 
                      notimpl
                  
                  member x.Parent: DTE = 
                      notimpl
                  
                  member x.Properties: Properties = 
                      notimpl
              interface IEnumerable with
                    member x.GetEnumerator(): IEnumerator = 
                        projs.GetEnumerator() :> IEnumerator }
                
        member __.Properties: Properties = notimpl        
        member __.Remove(_proj: Project): unit = notimpl        
        member __.SaveAs(_fileName: string): unit = notimpl        
        member __.Saved with get (): bool = notimpl and set (_v: bool): unit = notimpl        
        member __.SolutionBuild: SolutionBuild = notimpl        
        member __.TemplatePath with get (_projectType: string): string = notimpl

and MockProject(project: IProjectProvider, _dte: DTE) = 
    interface Project with
        member __.CodeModel: CodeModel = notimpl
        member __.Collection: Projects = notimpl
        member __.ConfigurationManager: ConfigurationManager = notimpl
        member __.DTE: DTE = notimpl
        member __.Delete(): unit = notimpl
        member __.Extender with get (_extenderName: string): obj = notimpl
        member __.ExtenderCATID: string = notimpl
        member __.ExtenderNames: obj = notimpl
        member __.FileName: string = notimpl
        member __.FullName: string = 
            project.ProjectFileName 

        member __.Globals: Globals = notimpl
        member __.IsDirty with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.Kind: string = 
            FSharpProjectKind

        member __.Name with get (): string = notimpl and set (_v: string): unit = notimpl
        member __.Object: obj = notimpl
        member __.ParentProjectItem: ProjectItem =  notimpl
        member __.ProjectItems: ProjectItems = notimpl
        member __.Properties: Properties = notimpl
        member __.Save(_fileName: string): unit = notimpl
        member __.SaveAs(_newFileName: string): unit = notimpl
        member __.Saved with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.UniqueName: string = notimpl
                  

and MockProjectItem(filePath: string, project: IProjectProvider, dte: DTE) =
    interface ProjectItem with
        member __.Collection: ProjectItems = notimpl
        member __.ConfigurationManager: ConfigurationManager = notimpl
        member __.ContainingProject: Project = 
            if project.IsForStandaloneScript then null
            else MockProject(project, dte) :> _

        member __.DTE: DTE = notimpl
        member __.Delete(): unit = notimpl
        member __.Document: Document = 
            MockDocument(filePath, dte) :> _

        member __.ExpandView(): unit = notimpl
        member __.Extender with get (_extenderName: string): obj = notimpl
        member __.ExtenderCATID: string = notimpl
        member __.ExtenderNames: obj = notimpl
        member __.FileCodeModel: FileCodeModel = notimpl
        member __.FileCount: int16 = notimpl
        member __.FileNames with get (_index: int16): string = notimpl
        member __.IsDirty with get (): bool = notimpl and set (_v: bool): unit = notimpl
        member __.IsOpen with get (_viewKind: string): bool = notimpl
        member __.Kind: string = notimpl
        member __.Name with get (): string = notimpl and set (_v: string): unit = notimpl
        member __.Object: obj = notimpl
        member __.Open(_viewKind: string): Window = notimpl
        member __.ProjectItems: ProjectItems = notimpl
        member __.Properties: Properties = notimpl        
        member __.Remove(): unit = notimpl        
        member __.Save(_fileName: string): unit = notimpl        
        member __.SaveAs(_newFileName: string): bool = notimpl        
        member __.Saved with get (): bool = notimpl and set (_v: bool): unit = notimpl        
        member __.SubProject: Project = notimpl
