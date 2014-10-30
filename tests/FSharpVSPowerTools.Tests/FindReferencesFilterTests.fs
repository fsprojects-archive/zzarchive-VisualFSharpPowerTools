namespace FSharpVSPowerTools.Tests

open System
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.Navigation
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop

type FindReferencesCommandHelper() =    
    inherit VsTestBase()
    let findReferencesBuffer = createMockTextBuffer "" (getTempFileName ".fs")

    /// Retrieve symbol information and output to the find reference buffer.
    /// It gives us a dirty way to assert on find reference results.
    let createVSFindSymbol() =
        { 
            new IVsFindSymbol with
                member __.DoSearch(_guidSymbolScope: byref<Guid>, pobSrch: VSOBSEARCHCRITERIA2 []): int = 
                    match box pobSrch.[0].pIVsNavInfo with
                    | :? FSharpLibraryNode as node -> 
                        let text =
                            seq {
                                for child in node.Children -> child.GetTextWithOwnership(VSTREETEXTOPTIONS.TTO_DEFAULT)
                            }
                            |> String.concat Environment.NewLine
                        let span = SnapshotSpan(findReferencesBuffer.CurrentSnapshot, 0, findReferencesBuffer.CurrentSnapshot.Length)
                        findReferencesBuffer.Replace(span.Span, text) |> ignore
                        VSConstants.S_OK
                    | _ -> 
                        VSConstants.S_OK

                member __.GetUserOptions(_pguidScope: byref<Guid>, _pobSrch: VSOBSEARCHCRITERIA2 []): int = 
                    notimpl
            
                member __.SetUserOptions(_guidScope: byref<Guid>, _pobSrch: VSOBSEARCHCRITERIA2 []): int = 
                    notimpl
        }

    do base.ServiceProvider.Services.[nameOf<SVsObjectSearch>] <- createVSFindSymbol()

    let command = FindReferencesFilterProvider(
                        editorFactory = base.VsEditorAdaptersFactoryService,
                        textDocumentFactoryService = base.DocumentFactoryService,                            
                        fsharpVsLanguageService = base.VsLanguageService,
                        serviceProvider = base.ServiceProvider,
                        projectFactory = base.ProjectFactory)

    member __.GetCommand(wpfTextView) =
        // ShowProgress requires to run on UI thread
        // Disable it otherwise test run deadlocks.
        let filter = command.RegisterCommandFilter(wpfTextView, showProgress = false)
        filter :> IOleCommandTarget

    [<CLIEvent>]
    member __.ReferencesChanged = findReferencesBuffer.Changed

    member __.ReferencesResults = 
        findReferencesBuffer.CurrentSnapshot.GetText().Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        

module FindReferencesCommandTests =
    open System.IO

#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif

    let helper = FindReferencesCommandHelper()
    let fileName = getTempFileName ".fs"

    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true

    [<Test>]
    let ``should be able to find all references in a single document``() =
        let content = """
let x = 0
x
"""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(VirtualProjectProvider(buffer, fileName), fileName)            
        let textView = createMockTextView buffer
        let command = helper.GetCommand(textView)

        let prefix = Path.GetFullPath(fileName)
        
        testEventTrigger helper.ReferencesChanged "Timed out before being able to find all references" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 3 1) |> ignore
                command.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidFindReferences, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                helper.ReferencesResults 
                |> assertEqual 
                    [| sprintf "%s - (%i, %i) : val x" prefix 2 4;
                       sprintf "%s - (%i, %i) : val x" prefix 3 0 |])

    [<Test>]
    let ``should be able to find all references in multiple documents``() = 
        let content = """
module Sample

val func : int -> int
"""
        // Use absolute path just to be sure
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/FSharpSignature/FSharpSignature.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/FSharpSignature/Sample.fsi"))
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(ExternalProjectProvider(projectFileName), fileName)         
        let textView = createMockTextView buffer
        let command = helper.GetCommand(textView)

        let prefix = Path.GetFullPath(fileName)
        
        testEventTrigger helper.ReferencesChanged "Timed out before being able to find all references" timeout
            (fun () -> 
                textView.Caret.MoveTo(snapshotPoint textView.TextSnapshot 4 6) |> ignore
                command.Exec(ref Constants.guidOldStandardCmdSet, Constants.cmdidFindReferences, 
                    0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () -> 
                helper.ReferencesResults 
                |> assertEqual 
                    [| sprintf "%s - (%i, %i) : val func" prefix 4 4;
                       sprintf "%s - (%i, %i) : val func" (prefix.Replace("Sample.fsi", "Sample.fs")) 3 4;
                       sprintf "%s - (%i, %i) : val func" (prefix.Replace("Sample.fsi", "Program.fs")) 5 4; |])
