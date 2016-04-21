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
                        editorOptionsFactory = base.EditorOptionsFactoryService,
                        textDocumentFactoryService = base.DocumentFactoryService,                            
                        languageService = base.VsLanguageService,
                        serviceProvider = base.ServiceProvider,
                        projectFactory = base.ProjectFactory,
                        referenceSourceProvider = base.ReferenceSourceProvider,
                        metadataService = Unchecked.defaultof<_>)

    member __.GetCommandFilter(wpfTextView) =
        command.RegisterCommandFilter(wpfTextView, fireNavigationEvent = true)

module GoToDefinitionCommandTests =

    let helper = GoToDefinitionCommandHelper()
   
    let internal getCommandFilter content filePath projectPath =
        let projectFileName = fullPathBasedOnSourceDir projectPath
        let fileName = fullPathBasedOnSourceDir filePath
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(ExternalProjectProvider(projectFileName), fileName, content) 
        let textView = createMockTextView buffer
        let command = helper.GetCommandFilter(textView)
        let urlChanged = command.UrlChanged.Value
        let filter = command :> IOleCommandTarget
        textView, command, filter, urlChanged

    [<Test>]
    let ``should be able to go to definition to an external class``() = 
        let content = """
module NavigateToSourceTests
open System.IO
let f x = Path.GetFileName(x)
let g x = File.Exists(x)
"""
        let (textView, command, filter, urlChanged) = 
            getCommandFilter content "../data/NavigateToSource/OctokitTests.fs"
                "../data/NavigateToSource/NavigateToSource.fsproj"
        
        testEventTrigger urlChanged "Timed out before being able to go to definition" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 11) |> ignore
                filter.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidGoToDefinition, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                command.CurrentUrl
                |> assertEqual (Some "http://referencesource.microsoft.com/mscorlib/a.html#090eca8621a248ee"))

    [<Test>]
    let ``should be able to go to definition to an external member using reference sources``() = 
        let content = """
module NavigateToSourceTests
open System.IO
let f x = Path.GetFileName(x)
let g x = File.Exists(x)
"""
        let (textView, command, filter, urlChanged) = 
            getCommandFilter content "../data/NavigateToSource/OctokitTests.fs"
                "../data/NavigateToSource/NavigateToSource.fsproj"
        
        testEventTrigger urlChanged "Timed out before being able to go to definition" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 18) |> ignore
                filter.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidGoToDefinition, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                command.CurrentUrl
                |> assertEqual (Some "http://referencesource.microsoft.com/mscorlib/a.html#95facc58d06cadd0"))

    [<Test; Ignore "Current FAKE is not source indexed. Reactivate this test when it's fixed.">]
    let ``should be able to go to definition to an external member using pdb files``() = 
        let content = """
module FAKETests
open Fake
Target "Main" DoNothing
RunTargetOrDefault "Main"
"""
        let (textView, command, filter, urlChanged) = 
            getCommandFilter content "../data/NavigateToSource/FAKETests.fs"
                "../data/NavigateToSource/NavigateToSource.fsproj"
        
        testEventTrigger urlChanged "Timed out before being able to go to definition" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 1) |> ignore
                filter.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidGoToDefinition, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                let url = Option.get command.CurrentUrl
                // We don't assert on hash values since it will be changed on next FAKE release
                url.Contains("https://raw.github.com/fsharp/FAKE/") |> assertTrue
                url.Contains("/src/app/FakeLib/TargetHelper.fs") |> assertTrue)


