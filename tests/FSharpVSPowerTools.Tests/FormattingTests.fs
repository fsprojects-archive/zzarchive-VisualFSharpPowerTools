namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Text.Editor

type FormattingCommandHelper() =    
    inherit VsTestBase()

    let command = CodeFormattingHookHelper(
                            adaptersFactory = base.VsEditorAdaptersFactoryService,
                            editorOptionsFactory = base.EditorOptionsFactoryService,
                            editorOperationsFactoryService = base.EditorOperationsFactoryService,
                            textBufferUndoManagerProvider = base.TextBufferUndoManagerProvider,
                            textDocumentFactoryService = base.DocumentFactoryService,
                            serviceProvider = base.ServiceProvider)

    member __.GetCommand(wpfTextView: IWpfTextView) =
        command.RegisterCommandDispatcher(wpfTextView)

module FormattingCommandTests =
#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif

    let helper = FormattingCommandHelper()
    let fileName = getTempFileName ".fsx"

    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true

    [<Test>]
    let ``formatting smoke test``() =
        let buffer = createMockTextBuffer "let x = 0" fileName
        let textView = createMockTextView buffer
        Assert.IsNotNull(helper.GetCommand(textView))