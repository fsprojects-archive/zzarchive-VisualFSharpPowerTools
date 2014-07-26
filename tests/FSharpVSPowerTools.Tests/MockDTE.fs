namespace FSharpVSPowerTools.Tests

open System
open EnvDTE
open EnvDTE80
open System.Collections
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open System.Collections.Generic

/// Create a simple mock DTE for an F# project
type MockDTE() =
    let projects = Dictionary()
    member x.AddProject(projectName: string, project: IProjectProvider) =
        match projects.TryGetValue(projectName) with
        | true, project ->
            Console.WriteLine("WARNING: a project with the same name has been already added to DTE.")
            projects.[projectName] <- project
        | false, _ ->
            Console.WriteLine("Adding {0} to DTE.", projectName)
            projects.[projectName] <- project

    interface DTE with
        member x.ActiveDocument: Document = 
            notimpl
        member x.ActiveSolutionProjects: obj = 
            notimpl
        member x.ActiveWindow: Window = 
            notimpl
        member x.AddIns: AddIns = 
            notimpl
        member x.Application: DTE = 
            notimpl
        member x.CommandBars: obj = 
            notimpl
        member x.CommandLineArguments: string = 
            notimpl
        member x.Commands: Commands = 
            notimpl
        member x.ContextAttributes: ContextAttributes = 
            notimpl
        member x.DTE: DTE = 
            notimpl
        member x.Debugger: Debugger = 
            notimpl
        member x.DisplayMode
            with get (): vsDisplay = 
                notimpl
            and set (v: vsDisplay): unit = 
                notimpl
        member x.Documents: Documents = 
            notimpl
        member x.Edition: string = 
            notimpl
        member x.Events: Events = 
            MockEvents() :> Events
        
        member x.ExecuteCommand(commandName: string, commandArgs: string): unit = 
            notimpl
        member x.FileName: string = 
            notimpl
        member x.Find: Find = 
            notimpl
        member x.FullName: string = 
            notimpl
        member x.GetObject(name: string): obj = 
            notimpl
        member x.Globals: Globals = 
            notimpl
        member x.IsOpenFile
            with get (viewKind: string, fileName: string): bool = 
                notimpl
        member x.ItemOperations: ItemOperations = 
            notimpl
        member x.LaunchWizard(vSZFile: string, contextParams: byref<obj []>): wizardResult = 
            notimpl
        member x.LocaleID: int = 
            notimpl
        member x.Macros: Macros = 
            notimpl
        member x.MacrosIDE: DTE = 
            notimpl
        member x.MainWindow: Window = 
            notimpl
        member x.Mode: vsIDEMode = 
            notimpl
        member x.Name: string = 
            notimpl
        member x.ObjectExtenders: ObjectExtenders = 
            notimpl
        member x.OpenFile(viewKind: string, fileName: string): Window = 
            notimpl
        member x.Properties
            with get (category: string, page: string): Properties = 
                notimpl
        member x.Quit(): unit = 
            notimpl
        member x.RegistryRoot: string = 
            notimpl
        member x.SatelliteDllPath(path: string, name: string): string = 
            notimpl
        member x.SelectedItems: SelectedItems = 
            notimpl
        member x.Solution: Solution = 
            MockSolution(projects, x) :> Solution
        
        member x.SourceControl: SourceControl = 
            notimpl
        member x.StatusBar: StatusBar = 
            notimpl
        member x.SuppressUI
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl
        member x.UndoContext: UndoContext = 
            notimpl
        member x.UserControl
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl        
        member x.Version: string = 
            "12.0"
                    
        member x.WindowConfigurations: WindowConfigurations = 
            notimpl        
        member x.Windows: Windows = 
            notimpl

and MockEvents() =
    interface Events with
        member x.BuildEvents: BuildEvents = 
            notimpl
        member x.CommandBarEvents
            with get (commandBarControl: obj): obj = 
                notimpl        
        member x.CommandEvents
            with get (guid: string, iD: int): CommandEvents = 
                notimpl        
        member x.DTEEvents: DTEEvents = 
            notimpl        
        member x.DebuggerEvents: DebuggerEvents = 
            notimpl        
        member x.DocumentEvents
            with get (document: Document): DocumentEvents = 
                notimpl        
        member x.FindEvents: FindEvents = 
            notimpl        
        member x.GetObject(name: string): obj = 
            notimpl        
        member x.MiscFilesEvents: ProjectItemsEvents = 
            notimpl        
        member x.OutputWindowEvents
            with get (pane: string): OutputWindowEvents = 
                notimpl        
        member x.SelectionEvents: SelectionEvents = 
            notimpl        
        member x.SolutionEvents: SolutionEvents = 
            notimpl        
        member x.SolutionItemsEvents: ProjectItemsEvents = 
            notimpl        
        member x.TaskListEvents
            with get (filter: string): TaskListEvents = 
                notimpl        
        member x.TextEditorEvents
            with get (textDocumentFilter: TextDocument): TextEditorEvents = 
                notimpl        
        member x.WindowEvents
            with get (windowFilter: Window): WindowEvents = 
                notimpl
        
and MockSolution(projects, dte: DTE) =
    interface IEnumerable with
        member x.GetEnumerator(): IEnumerator = 
            notimpl
        
    interface Solution with
        member x.AddFromFile(fileName: string, exclusive: bool): Project = 
            notimpl
        member x.AddFromTemplate(fileName: string, destination: string, projectName: string, exclusive: bool): Project = 
            notimpl        
        member x.AddIns: AddIns = 
            notimpl        
        member x.Close(saveFirst: bool): unit = 
            notimpl        
        member x.Count: int = 
            notimpl        
        member x.Create(destination: string, name: string): unit = 
            notimpl        
        member x.DTE: DTE = 
            notimpl        
        member x.Extender
            with get (extenderName: string): obj = 
                notimpl        
        member x.ExtenderCATID: string = 
            notimpl        
        member x.ExtenderNames: obj = 
            notimpl        
        member x.FileName: string = 
            notimpl        
        member x.FindProjectItem(fileName: string): ProjectItem = 
            let allProjects = projects |> Seq.map (|KeyValue|) |> Seq.map snd
            match allProjects |> Seq.tryFind (fun project -> Array.exists ((=) fileName) project.SourceFiles) with
            | Some project ->
                MockProjectItem(fileName, project, dte) :> ProjectItem
            | None -> null

        member x.FullName: string = 
            notimpl        
        member x.GetEnumerator(): Collections.IEnumerator = 
            notimpl        
        member x.Globals: Globals = 
            notimpl        
        member x.IsDirty
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl        
        member x.IsOpen: bool = 
            notimpl        
        member x.Item(index: obj): Project = 
            notimpl        
        member x.Open(fileName: string): unit = 
            notimpl        
        member x.Parent: DTE = 
            notimpl        
        member x.ProjectItemsTemplatePath(projectKind: string): string = 
            notimpl        
        member x.Projects: Projects = 
            notimpl        
        member x.Properties: Properties = 
            notimpl        
        member x.Remove(proj: Project): unit = 
            notimpl        
        member x.SaveAs(fileName: string): unit = 
            notimpl        
        member x.Saved
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl        
        member x.SolutionBuild: SolutionBuild = 
            notimpl        
        member x.TemplatePath
            with get (projectType: string): string = 
                notimpl

and MockProject(project: IProjectProvider, dte: DTE) = 
    interface Project with
        member x.CodeModel: CodeModel = 
            notimpl
        member x.Collection: Projects = 
            notimpl
        member x.ConfigurationManager: ConfigurationManager = 
            notimpl
        member x.DTE: DTE = 
            notimpl
        member x.Delete(): unit = 
            notimpl
        member x.Extender
            with get (extenderName: string): obj = 
                notimpl
        member x.ExtenderCATID: string = 
            notimpl
        member x.ExtenderNames: obj = 
            notimpl
        member x.FileName: string = 
            notimpl
        member x.FullName: string = 
            notimpl
        member x.Globals: Globals = 
            notimpl
        member x.IsDirty
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl
        member x.Kind: string = 
            FSharpProjectKind
        member x.Name
            with get (): string = 
                notimpl
            and set (v: string): unit = 
                notimpl
        member x.Object: obj = 
            notimpl
        member x.ParentProjectItem: ProjectItem = 
            notimpl
        member x.ProjectItems: ProjectItems = 
            notimpl
        member x.Properties: Properties = 
            notimpl
        member x.Save(fileName: string): unit = 
            debug "[MockProject] Suppose to save file into disk but do nothing here"
        
        member x.SaveAs(newFileName: string): unit = 
            notimpl
        member x.Saved
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl
        member x.UniqueName: string = 
            project.ProjectFileName       

and MockProjectItem(fileName: string, project: IProjectProvider, dte: DTE) =
    interface ProjectItem with
        member x.Collection: ProjectItems = 
            notimpl
        member x.ConfigurationManager: ConfigurationManager = 
            notimpl
        member x.ContainingProject: Project = 
            if project.IsForStandaloneScript then null
            else MockProject(project, dte) :> Project

        member x.DTE: DTE = 
            notimpl
        member x.Delete(): unit = 
            notimpl
        member x.Document: Document = 
            notimpl
        member x.ExpandView(): unit = 
            notimpl
        member x.Extender
            with get (extenderName: string): obj = 
                notimpl
        member x.ExtenderCATID: string = 
            notimpl
        member x.ExtenderNames: obj = 
            notimpl
        member x.FileCodeModel: FileCodeModel = 
            notimpl
        member x.FileCount: int16 = 
            notimpl
        member x.FileNames
            with get (index: int16): string = 
                notimpl
        member x.IsDirty
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl
        member x.IsOpen
            with get (viewKind: string): bool = 
                notimpl
        member x.Kind: string = 
            notimpl
        member x.Name
            with get (): string = 
                notimpl
            and set (v: string): unit = 
                notimpl
        member x.Object: obj = 
            notimpl
        member x.Open(viewKind: string): Window = 
            notimpl
        member x.ProjectItems: ProjectItems = 
            notimpl
        member x.Properties: Properties = 
            notimpl        
        member x.Remove(): unit = 
            notimpl        
        member x.Save(fileName: string): unit = 
            notimpl        
        member x.SaveAs(newFileName: string): bool = 
            notimpl        
        member x.Saved
            with get (): bool = 
                notimpl
            and set (v: bool): unit = 
                notimpl        
        member x.SubProject: Project = 
            notimpl
        
        
        

