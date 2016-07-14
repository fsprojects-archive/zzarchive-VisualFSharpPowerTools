namespace FSharpVSPowerTools.Tests

open TestUtilities.Mocks
open FSharpPowerTools.Core.Infrastructure
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell.Interop
open EnvDTE

/// Replace internal project providers by external ones for testing
type MockProjectFactory(serviceProvider, openDocTracker, vsLanguageService, dte: MockDTE) =
    inherit ProjectFactory(serviceProvider, openDocTracker, vsLanguageService)
    override __.CreateForProject p = dte.GetProject p.FullName

/// A base class for initializing necessary VS services
type VsTestBase() =
    static let serviceProvider = MockServiceProvider()
    
    static do serviceProvider.Services.[nameOf<SVsActivityLog>] <- MockActivityLog()
    static do serviceProvider.Services.[nameOf<SVsShell>] <- MockVsShell()
    static do serviceProvider.Services.[nameOf<SVsStatusbar>] <- Mocks.createSVsStatusbar()
    static do serviceProvider.Services.[nameOf<SVsSolutionBuildManager>] <- Mocks.createVsSolutionBuildManager2()
    
    static do serviceProvider.Services.[nameOf<IGeneralOptions>] <- Mocks.createGeneralOptionsPage()
    static do serviceProvider.Services.[nameOf<IFormattingOptions>] <- new FantomasOptionsPage()
    static do serviceProvider.Services.[nameOf<ICodeGenerationOptions>] <- new CodeGenerationOptionsPage()
    static do serviceProvider.Services.[nameOf<IGlobalOptions>] <- new GlobalOptionsPage()
    static do serviceProvider.Services.[nameOf<IOutliningOptions>] <- new OutliningOptionsPage()

    static let dte = MockDTE()
    static do serviceProvider.Services.[nameOf<EnvDTE.DTE>] <- dte
    static do serviceProvider.Services.[nameOf<SDTE>] <- dte
    static do serviceProvider.Services.[nameOf<SVsResourceManager>] <- Mocks.createSVsResourceManager()
    
    static do serviceProvider.Services.[nameOf<SVsRunningDocumentTable>] <- Mocks.createSVsRunningDocumentTable(dte)
    static do serviceProvider.Services.[nameOf<ILintOptions>] <- new Linting.LintOptionsPage(dte)

    let vsEditorAdaptersFactoryService = Mocks.createVsEditorAdaptersFactoryService()
    let classificationRegistry = Mocks.createClassificationTypeRegistryService()
    let documentFactoryService = Mocks.createDocumentFactoryService()
    let undoHistoryRegistry = Mocks.createTextUndoHistoryRegistry()

    let editorOptionsFactoryService = Mocks.createEditorOptionsFactoryService()
    let editorOperationsFactoryService = Mocks.createEditorOperationsFactoryService()
    let textBufferUndoManagerProvider = Mocks.createTextBufferUndoManagerProvider()

    let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    let openDocumentsTracker = Mocks.OpenDocumentTrackerStub()
    let fileSystem = FileSystem(openDocumentsTracker)
    let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                              openDocumentsTracker, fileSystem, serviceProvider, SkipLexCache = true)
    let projectFactory = new MockProjectFactory(serviceProvider, openDocumentsTracker, vsLanguageService, dte)

    let referenceSourceProvider = new DotNetReferenceSourceProvider()
    // Ensure that the timer is activated before running tests
    do if not referenceSourceProvider.IsActivated then referenceSourceProvider.Activate()

    static member GlobalServiceProvider = serviceProvider
    member __.ServiceProvider = serviceProvider

    member __.FSharpLanguageService = fsharpLanguageService
    member __.VsEditorAdaptersFactoryService = vsEditorAdaptersFactoryService
    member __.DocumentFactoryService = documentFactoryService
    member __.OpenDocumentsTracker = openDocumentsTracker :> IOpenDocumentsTracker
    member __.FileSystem = fileSystem
    member __.VsLanguageService = vsLanguageService
    member __.ProjectFactory = projectFactory
    member __.ClassificationTypeRegistryService = classificationRegistry
    member __.UndoHistoryRegistry = undoHistoryRegistry
    member __.EditorOptionsFactoryService = editorOptionsFactoryService
    member __.EditorOperationsFactoryService = editorOperationsFactoryService
    member __.TextBufferUndoManagerProvider = textBufferUndoManagerProvider
    member __.ReferenceSourceProvider = referenceSourceProvider

    member __.AddProject(project: IProjectProvider) = 
        dte.AddProject(project.ProjectFileName, project)

    member __.SetActiveDocument(filePath: string, content: string) = 
        dte.SetActiveDocument filePath
        openDocumentsTracker.SetActiveDocumentContent content

    member x.SetUpProjectAndCurrentDocument(project: IProjectProvider, filePath: string, content: string) =
        match project with
        | :? ExternalProjectProvider as p ->
            x.AddProject p
            for p' in p.ReferencedProjects do x.AddProject p'
        | _ ->
            // Assume that this kind of project provider doesn't have referenced projects
            x.AddProject project
        x.SetActiveDocument (filePath, content)

    member __.SetActiveDocumentContent content =
        openDocumentsTracker.SetActiveDocumentContent content
