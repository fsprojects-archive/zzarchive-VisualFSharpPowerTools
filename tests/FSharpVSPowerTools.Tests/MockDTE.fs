namespace FSharpVSPowerTools.Tests

open System
open EnvDTE
open EnvDTE80

type MockDTE() =
    interface DTE with
        member x.ActiveDocument: Document = 
            failwith "Not implemented yet"
        
        member x.ActiveSolutionProjects: obj = 
            failwith "Not implemented yet"
        
        member x.ActiveWindow: Window = 
            failwith "Not implemented yet"
        
        member x.AddIns: AddIns = 
            failwith "Not implemented yet"
        
        member x.Application: DTE = 
            failwith "Not implemented yet"
        
        member x.CommandBars: obj = 
            failwith "Not implemented yet"
        
        member x.CommandLineArguments: string = 
            failwith "Not implemented yet"
        
        member x.Commands: Commands = 
            failwith "Not implemented yet"
        
        member x.ContextAttributes: ContextAttributes = 
            failwith "Not implemented yet"
        
        member x.DTE: DTE = 
            failwith "Not implemented yet"
        
        member x.Debugger: Debugger = 
            failwith "Not implemented yet"
        
        member x.DisplayMode
            with get (): vsDisplay = 
                failwith "Not implemented yet"
            and set (v: vsDisplay): unit = 
                failwith "Not implemented yet"
        
        member x.Documents: Documents = 
            failwith "Not implemented yet"
        
        member x.Edition: string = 
            failwith "Not implemented yet"
        
        member x.Events: Events = 
            MockEvents() :> Events
        
        member x.ExecuteCommand(commandName: string, commandArgs: string): unit = 
            failwith "Not implemented yet"
        
        member x.FileName: string = 
            failwith "Not implemented yet"
        
        member x.Find: Find = 
            failwith "Not implemented yet"
        
        member x.FullName: string = 
            failwith "Not implemented yet"
        
        member x.GetObject(name: string): obj = 
            failwith "Not implemented yet"
        
        member x.Globals: Globals = 
            failwith "Not implemented yet"
        
        member x.IsOpenFile
            with get (viewKind: string, fileName: string): bool = 
                failwith "Not implemented yet"
        
        member x.ItemOperations: ItemOperations = 
            failwith "Not implemented yet"
        
        member x.LaunchWizard(vSZFile: string, contextParams: byref<obj []>): wizardResult = 
            failwith "Not implemented yet"
        
        member x.LocaleID: int = 
            failwith "Not implemented yet"
        
        member x.Macros: Macros = 
            failwith "Not implemented yet"
        
        member x.MacrosIDE: DTE = 
            failwith "Not implemented yet"
        
        member x.MainWindow: Window = 
            failwith "Not implemented yet"
        
        member x.Mode: vsIDEMode = 
            failwith "Not implemented yet"
        
        member x.Name: string = 
            failwith "Not implemented yet"
        
        member x.ObjectExtenders: ObjectExtenders = 
            failwith "Not implemented yet"
        
        member x.OpenFile(viewKind: string, fileName: string): Window = 
            failwith "Not implemented yet"
        
        member x.Properties
            with get (category: string, page: string): Properties = 
                failwith "Not implemented yet"
        
        member x.Quit(): unit = 
            failwith "Not implemented yet"
        
        member x.RegistryRoot: string = 
            failwith "Not implemented yet"
        
        member x.SatelliteDllPath(path: string, name: string): string = 
            failwith "Not implemented yet"
        
        member x.SelectedItems: SelectedItems = 
            failwith "Not implemented yet"
        
        member x.Solution: Solution = 
            failwith "Not implemented yet"
        
        member x.SourceControl: SourceControl = 
            failwith "Not implemented yet"
        
        member x.StatusBar: StatusBar = 
            failwith "Not implemented yet"
        
        member x.SuppressUI
            with get (): bool = 
                failwith "Not implemented yet"
            and set (v: bool): unit = 
                failwith "Not implemented yet"
        
        member x.UndoContext: UndoContext = 
            failwith "Not implemented yet"
        
        member x.UserControl
            with get (): bool = 
                failwith "Not implemented yet"
            and set (v: bool): unit = 
                failwith "Not implemented yet"
        
        member x.Version: string = 
            failwith "Not implemented yet"
        
        member x.WindowConfigurations: WindowConfigurations = 
            failwith "Not implemented yet"
        
        member x.Windows: Windows = 
            failwith "Not implemented yet"

and MockEvents() =
    interface Events with
        member x.BuildEvents: BuildEvents = 
            failwith "Not implemented yet"
        
        member x.CommandBarEvents
            with get (commandBarControl: obj): obj = 
                failwith "Not implemented yet"
        
        member x.CommandEvents
            with get (guid: string, iD: int): CommandEvents = 
                failwith "Not implemented yet"
        
        member x.DTEEvents: DTEEvents = 
            failwith "Not implemented yet"
        
        member x.DebuggerEvents: DebuggerEvents = 
            failwith "Not implemented yet"
        
        member x.DocumentEvents
            with get (document: Document): DocumentEvents = 
                failwith "Not implemented yet"
        
        member x.FindEvents: FindEvents = 
            failwith "Not implemented yet"
        
        member x.GetObject(name: string): obj = 
            failwith "Not implemented yet"
        
        member x.MiscFilesEvents: ProjectItemsEvents = 
            failwith "Not implemented yet"
        
        member x.OutputWindowEvents
            with get (pane: string): OutputWindowEvents = 
                failwith "Not implemented yet"
        
        member x.SelectionEvents: SelectionEvents = 
            failwith "Not implemented yet"
        
        member x.SolutionEvents: SolutionEvents = 
            failwith "Not implemented yet"
        
        member x.SolutionItemsEvents: ProjectItemsEvents = 
            failwith "Not implemented yet"
        
        member x.TaskListEvents
            with get (filter: string): TaskListEvents = 
                failwith "Not implemented yet"
        
        member x.TextEditorEvents
            with get (textDocumentFilter: TextDocument): TextEditorEvents = 
                failwith "Not implemented yet"
        
        member x.WindowEvents
            with get (windowFilter: Window): WindowEvents = 
                failwith "Not implemented yet"
        
        

