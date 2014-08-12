namespace FSharpVSPowerTools.Tests

open TestUtilities.Mocks
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Foq
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Operations

/// A base class for initializing necessary VS services
type VsTestBase() =
    // We mark required services as static fields in order that only one language service is used for all tests.
    // The idea is to reduce overheads of creating language service for testing.
    static let serviceProvider = MockServiceProvider()        
    
    static do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    static do serviceProvider.Services.["SVsShell"] <- MockVsShell()
    static do serviceProvider.Services.["SVsSolutionBuildManager"] <- Mocks.createVsSolutionBuildManager2()
    static do serviceProvider.Services.["GeneralOptionsPage"] <- Mocks.createGeneralOptionsPage()

    static let dte = MockDTE()
    static do serviceProvider.Services.["DTE"] <- dte
    static do serviceProvider.Services.["SDTE"] <- dte
    static do serviceProvider.Services.["SVsResourceManager"] <- Mocks.createSVsResourceManager()

    static let vsEditorAdaptersFactoryService = Mocks.createVsEditorAdaptersFactoryService()
    static let classificationRegistry = Mocks.createClassificationTypeRegistryService()
    static let documentFactoryService = Mocks.createDocumentFactoryService()
    static let undoHistoryRegistry = Mocks.createTextUndoHistoryRegistry()

    static let editorOptionsFactoryService = Mocks.createEditorOptionsFactoryService()
    static let editorOperationsFactoryService = Mocks.createEditorOperationsFactoryService()
    static let textBufferUndoManagerProvider = Mocks.createTextBufferUndoManagerProvider()

    static let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    static let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    static let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                                     openDocumentsTracker, SkipLexCache = true)
    static let projectFactory = new ProjectFactory(serviceProvider, vsLanguageService)
    
    member __.ServiceProvider = serviceProvider
    member __.FSharpLanguageService = fsharpLanguageService
    member __.VsEditorAdaptersFactoryService = vsEditorAdaptersFactoryService
    member __.DocumentFactoryService = documentFactoryService
    member __.OpenDocumentsTracker = openDocumentsTracker
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
