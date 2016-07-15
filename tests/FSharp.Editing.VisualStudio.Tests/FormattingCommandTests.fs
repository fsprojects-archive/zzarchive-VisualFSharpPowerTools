namespace FSharp.Editing.VisualStudio.Tests

open System
open NUnit.Framework
open FSharpVSPowerTools
open Microsoft.VisualStudio 
open Microsoft.VisualStudio.Text.Editor

type FormattingCommandHelper() =
    inherit VsTestBase()

    let command = 
        CodeFormattingHookHelper
            (
                adaptersFactory = base.VsEditorAdaptersFactoryService,
                editorOptionsFactory = base.EditorOptionsFactoryService,
                editorOperationsFactoryService = base.EditorOperationsFactoryService,
                textBufferUndoManagerProvider = base.TextBufferUndoManagerProvider,
                textDocumentFactoryService = base.DocumentFactoryService,
                serviceProvider = base.ServiceProvider,
                projectFactory = base.ProjectFactory,
                vsLanguageService = base.VsLanguageService,
                openDocumentTracker = base.OpenDocumentsTracker
            )

    member __.GetCommand(wpfTextView: IWpfTextView) =
        command.RegisterCommandDispatcher(wpfTextView)

module FormattingCommandTests =
    open System.Threading
    open FSharp.Editing.VisualStudio

    let helper = FormattingCommandHelper()
    let fileName = getTempFileName ".fs"

    [<Test; Apartment(ApartmentState.STA)>]
    let ``should be able to format document``() =
        let content = """
module M
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString () =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s
"""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let textView = createMockTextView buffer
        testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
            (fun () ->
                let command = helper.GetCommand(textView)
                command.Exec(ref Constants.guidStandardCmdSet, uint32 VSConstants.VSStd2KCmdID.FORMATDOCUMENT, 
                             0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () ->
                buffer.CurrentSnapshot.GetText()
                |> prependNewLine
                |> assertEquivString """
module M

type Type = 
    | TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    override this.ToString() = 
        match this with
        | TyLam(t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
        | TyVar a -> a
        | TyCon(s, ts) -> s
""" )

    [<Test; Apartment(ApartmentState.STA)>]
    let ``should be able to format selection``() =
        let content = """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."
"""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let textView = createMockTextView buffer
        let selection = fromRange buffer.CurrentSnapshot (3, 4, 5, 52) |> Option.get
        textView.Selection.Select(selection, isReversed=false)
        testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
            (fun () ->
                let command = helper.GetCommand(textView)
                command.Exec(ref Constants.guidStandardCmdSet, uint32 VSConstants.VSStd2KCmdID.FORMATSELECTION, 
                                0u, IntPtr.Zero, IntPtr.Zero) |> ignore)
            (fun () ->
                buffer.CurrentSnapshot.GetText()
                |> assertEquivString """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size / 2 && var1 <= mid + size / 2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."
""" )