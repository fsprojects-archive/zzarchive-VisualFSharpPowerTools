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
    // The idea is to reduce overheads of creating language service for testing/
    static let serviceProvider = MockServiceProvider()        
    static let dte = MockDTE()
    static do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    static do serviceProvider.Services.["SVsShell"] <- MockVsShell()
        
    static do serviceProvider.Services.["GeneralOptionsPage"] <- 
                Mock<IGeneralOptionsPage>.With(fun page ->
                    <@
                        page.DepthColorizerEnabled --> true
                        page.FindAllReferencesEnabled --> true
                        page.FolderOrganizationEnabled --> true
                        page.FormattingEnabled --> true
                        page.GenerateRecordStubEnabled --> true
                        page.HighlightUsageEnabled --> true
                        page.InterfaceImplementationEnabled --> true
                        page.NavBarEnabled --> true
                        page.NavigateToEnabled --> true
                        page.RenameRefactoringEnabled --> true
                        page.ResolveUnopenedNamespacesEnabled --> true
                        page.SyntaxColoringEnabled --> true
                        page.UnionPatternMatchCaseGenerationEnabled --> true
                        page.UnusedDeclarationsEnabled --> true
                        page.XmlDocEnabled --> true
                    @>)

    static do serviceProvider.Services.["DTE"] <- dte
    static do serviceProvider.Services.["SDTE"] <- dte
    static do serviceProvider.Services.["SVsResourceManager"] <- Mocks.createSVsResourceManager()

    static let vsEditorAdaptersFactoryService = 
        Mock<IVsEditorAdaptersFactoryService>().Create()
    
    static let classificationRegistry = 
        Mock<IClassificationTypeRegistryService>()
            .Setup(fun x -> <@ x.GetClassificationType (any()) @>)
            .Calls<string>(fun t -> Mock<IClassificationType>.With(fun x -> <@ x.Classification --> t @>))
            .Create()
    
    static let documentFactoryService = Mocks.createDocumentFactoryService()

    static let undoTransation =
        Mock<ITextUndoTransaction>.With(fun x ->
            <@
                x.Complete()
                x.Dispose()
            @>)

    static let undoHistory =
        Mock<ITextUndoHistory>()
            .Setup(fun x -> <@ x.CreateTransaction (any()) @>)
            .Returns(undoTransation)
            .Create()

    static let undoHistoryRegistry = 
        Mock<ITextUndoHistoryRegistry>()
            .Setup(fun x -> <@ x.RegisterHistory (any()) @>)
            .Returns(undoHistory)
            .Create()

    static let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    static let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    static let vsLanguageService = VSLanguageService(vsEditorAdaptersFactoryService, fsharpLanguageService, 
                                                     openDocumentsTracker, SkipLexCache = true)
    static let projectFactory = ProjectFactory(serviceProvider, vsLanguageService)
    
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
