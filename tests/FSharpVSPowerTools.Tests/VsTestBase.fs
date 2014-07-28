namespace FSharpVSPowerTools.Tests

open TestUtilities.Mocks
open FSharpVSPowerTools.ProjectSystem
open Foq
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text

/// A base class for initializing necessary VS services
type VsTestBase() =
    let serviceProvider = MockServiceProvider()        
    let dte = MockDTE()
    do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    do serviceProvider.Services.["SVsShell"] <- MockVsShell()
    do serviceProvider.Services.["GeneralOptionsPage"] <- MockGeneralOptionsPage()
    do serviceProvider.Services.["DTE"] <- dte
    do serviceProvider.Services.["SDTE"] <- dte
    do serviceProvider.Services.["SVsResourceManager"] <- Mocks.createSVsResourceManager()

    let vsEditorAdaptersFactoryService = Mock<IVsEditorAdaptersFactoryService>().Create()
    
    let classificationRegistry = 
        Mock<IClassificationTypeRegistryService>()
            .Setup(fun x -> <@ x.GetClassificationType (any()) @>)
            .Calls<string>(fun t -> Mock<IClassificationType>.With(fun x -> <@ x.Classification --> t @>))
            .Create()
    
    let documentFactoryService = Mocks.createDocumentFactoryService()
    let undoHistoryRegistry = Mocks.createTextUndoHistoryRegistry()
    let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                              openDocumentsTracker, SkipLexCache = true)
    let projectFactory = ProjectFactory(serviceProvider, vsLanguageService)
    
    member __.ServiceProvider = serviceProvider
    member __.FSharpLanguageService = fsharpLanguageService
    member __.VsEditorAdaptersFactoryService = vsEditorAdaptersFactoryService
    member __.DocumentFactoryService = documentFactoryService
    member __.OpenDocumentsTracker = openDocumentsTracker
    member __.VsLanguageService = vsLanguageService
    member __.ProjectFactory = projectFactory
    member __.ClassificationTypeRegistryService = classificationRegistry
    member __.UndoHistoryRegistry = undoHistoryRegistry

    member __.AddProject(project: IProjectProvider) = 
        dte.AddProject(project.ProjectFileName, project)

    member __.SetActiveDocument(filePath: string) = 
        dte.SetActiveDocument(filePath)
