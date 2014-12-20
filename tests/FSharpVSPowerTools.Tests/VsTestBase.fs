namespace FSharpVSPowerTools.Tests

open TestUtilities.Mocks
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Foq
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Shell.Interop

/// Replace internal project providers by external ones for testing
type MockProjectFactory(serviceProvider, openDocTracker, vsLanguageService, dte: MockDTE) =
    inherit ProjectFactory(serviceProvider, openDocTracker, vsLanguageService)
    override __.CreateForProject(p) = 
        dte.GetProject(p.FullName)

/// A base class for initializing necessary VS services
type VsTestBase() =
    let serviceProvider = MockServiceProvider()        
    
    do serviceProvider.Services.[nameOf<SVsActivityLog>] <- MockActivityLog()
    do serviceProvider.Services.[nameOf<SVsShell>] <- MockVsShell()
    do serviceProvider.Services.[nameOf<SVsStatusbar>] <- Mocks.createSVsStatusbar()
    do serviceProvider.Services.[nameOf<SVsSolutionBuildManager>] <- Mocks.createVsSolutionBuildManager2()
    do serviceProvider.Services.[nameOf<IGeneralOptions>] <- Mocks.createGeneralOptionsPage()
    do serviceProvider.Services.[nameOf<IFormattingOptions>] <- new FantomasOptionsPage()
    do serviceProvider.Services.[nameOf<ICodeGenerationOptions>] <- new CodeGenerationOptionsPage()
    do serviceProvider.Services.[nameOf<IGlobalOptions>] <- new GlobalOptionsPage()

    let dte = MockDTE()
    do serviceProvider.Services.[nameOf<EnvDTE.DTE>] <- dte
    do serviceProvider.Services.[nameOf<SDTE>] <- dte
    do serviceProvider.Services.[nameOf<SVsResourceManager>] <- Mocks.createSVsResourceManager()

    let vsEditorAdaptersFactoryService = Mocks.createVsEditorAdaptersFactoryService()
    let classificationRegistry = Mocks.createClassificationTypeRegistryService()
    let documentFactoryService = Mocks.createDocumentFactoryService()
    let undoHistoryRegistry = Mocks.createTextUndoHistoryRegistry()

    let editorOptionsFactoryService = Mocks.createEditorOptionsFactoryService()
    let editorOperationsFactoryService = Mocks.createEditorOperationsFactoryService()
    let textBufferUndoManagerProvider = Mocks.createTextBufferUndoManagerProvider()

    let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    let fileSystem = FileSystem(openDocumentsTracker)
    let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                              openDocumentsTracker, fileSystem, serviceProvider, SkipLexCache = true)
    let projectFactory = new MockProjectFactory(serviceProvider, openDocumentsTracker, vsLanguageService, dte)

    let shellEventListener = new ShellEventListener(serviceProvider)
    
    member __.ServiceProvider = serviceProvider
    member __.ShellEventListener = shellEventListener
    member __.FSharpLanguageService = fsharpLanguageService
    member __.VsEditorAdaptersFactoryService = vsEditorAdaptersFactoryService
    member __.DocumentFactoryService = documentFactoryService
    member __.OpenDocumentsTracker = openDocumentsTracker
    member __.FileSystem = fileSystem
    member __.VsLanguageService = vsLanguageService
    member __.ProjectFactory = projectFactory
    member __.ClassificationTypeRegistryService = classificationRegistry
    member __.UndoHistoryRegistry = undoHistoryRegistry
    member __.EditorOptionsFactoryService = editorOptionsFactoryService
    member __.EditorOperationsFactoryService = editorOperationsFactoryService
    member __.TextBufferUndoManagerProvider = textBufferUndoManagerProvider

    member __.AddProject(project: IProjectProvider) = 
        dte.AddProject(project.ProjectFileName, project)

    member __.SetActiveDocument(filePath: string) = 
        dte.SetActiveDocument(filePath)

    member x.SetUpProjectAndCurrentDocument(project: IProjectProvider, filePath: string) =
        match project with
        | :? ExternalProjectProvider as p ->
            x.AddProject(p)
            for p' in p.ReferencedProjects do x.AddProject(p')
        | _ ->
            // Assume that this kind of project provider doesn't have referenced projects
            x.AddProject(project)
        x.SetActiveDocument(filePath)
        
