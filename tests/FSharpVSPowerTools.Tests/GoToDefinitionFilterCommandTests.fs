namespace FSharpVSPowerTools.Tests

open System
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.Navigation
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop

type GoToDefinitionCommandHelper() =    
    inherit VsTestBase()
    let command = new GoToDefinitionFilterProvider(
                        editorFactory = base.VsEditorAdaptersFactoryService,
                        textDocumentFactoryService = base.DocumentFactoryService,                            
                        fsharpVsLanguageService = base.VsLanguageService,
                        serviceProvider = base.ServiceProvider,
                        projectFactory = base.ProjectFactory,
                        referenceSourceProvider = base.ReferenceSourceProvider)

    member __.GetCommandFilter(wpfTextView) =
        command.RegisterCommandFilter(wpfTextView, fireNavigationEvent = true)

module GoToDefinitionCommandTests =
    open System.IO

#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif

    let helper = GoToDefinitionCommandHelper()
   
    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true
        Logger.GlobalServiceProvider <- helper.ServiceProvider

    [<Test>]
    let ``should be able to go to definition to an external class``() = 
        let content = """
module GoToDefinitionTests
open System.IO
let f x = Path.GetFileName(x)
let g x = File.Exists(x)
"""
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/NavigateToSource/NavigateToSource.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/NavigateToSource/FAKETests.fs"))
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(ExternalProjectProvider(projectFileName), fileName)    
        let textView = createMockTextView buffer
        let command = helper.GetCommandFilter(textView)
        let urlChanged = command.UrlChanged.Value
        let filter = command :> IOleCommandTarget
        let prefix = Path.GetFullPath(fileName)
        
        testEventTrigger urlChanged "Timed out before being able to go to definition" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 11) |> ignore
                filter.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidGoToDefinition, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                command.CurrentUrl
                |> Option.get
                |> assertEqual "http://referencesource.microsoft.com/mscorlib/a.html#090eca8621a248ee")

    [<Test>]
    let ``should be able to go to definition to an external member``() = 
        let content = """
module GoToDefinitionTests
open System.IO
let f x = Path.GetFileName(x)
let g x = File.Exists(x)
"""
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/NavigateToSource/NavigateToSource.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/NavigateToSource/FAKETests.fs"))
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(ExternalProjectProvider(projectFileName), fileName)              
        let textView = createMockTextView buffer
        let command = helper.GetCommandFilter(textView)
        let urlChanged = command.UrlChanged.Value
        let filter = command :> IOleCommandTarget
        let prefix = Path.GetFullPath(fileName)
        
        testEventTrigger urlChanged "Timed out before being able to go to definition" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 18) |> ignore
                filter.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidGoToDefinition, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                command.CurrentUrl
                |> Option.get
                |> assertEqual "http://referencesource.microsoft.com/mscorlib/a.html#95facc58d06cadd0")


