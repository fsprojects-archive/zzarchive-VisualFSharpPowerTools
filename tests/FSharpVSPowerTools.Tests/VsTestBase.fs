namespace FSharpVSPowerTools.Tests

open TestUtilities.Mocks
open FSharpVSPowerTools.ProjectSystem

/// A base class for initializing necessary VS services
type VsTestBase() =
    let serviceProvider = MockServiceProvider()        
    let dte = MockDTE()
    do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    do serviceProvider.Services.["SVsShell"] <- MockVsShell()
    do serviceProvider.Services.["GeneralOptionsPage"] <- MockGeneralOptionsPage()
    do serviceProvider.Services.["DTE"] <- dte
    do serviceProvider.Services.["SDTE"] <- dte

    let vsEditorAdaptersFactoryService = Mocks.createVsEditorAdaptersFactoryService()
    let classificationRegistry = Mocks.createClassificationTypeRegistryService()
    let documentFactoryService = Mocks.createDocumentFactoryService()

    let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                                openDocumentsTracker, skipLexCache = true)
    let projectFactory = ProjectFactory(serviceProvider, vsLanguageService)
    
    member x.ServiceProvider = serviceProvider
    member x.FSharpLanguageService = fsharpLanguageService
    member x.VsEditorAdaptersFactoryService = vsEditorAdaptersFactoryService
    member x.DocumentFactoryService = documentFactoryService
    member x.OpenDocumentsTracker = openDocumentsTracker
    member x.VsLanguageService = vsLanguageService
    member x.ProjectFactory = projectFactory
    member x.ClassificationTypeRegistryService = classificationRegistry

    member x.AddProject(project: IProjectProvider) = dte.AddProject(project.ProjectFileName, project)

