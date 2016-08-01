#nowarn "44"
namespace FSharp.Editing.VisualStudio.Tests

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Language.Intellisense

type ImplementInterfaceSmartTaggerHelper() =    
    inherit VsTestBase()

    let taggerProvider = 
        ImplementInterfaceSmartTaggerProvider
            (
                fsharpVsLanguageService = base.VsLanguageService,
                editorOptionsFactory = base.EditorOptionsFactoryService,
                serviceProvider = base.ServiceProvider,
                undoHistoryRegistry = base.UndoHistoryRegistry,
                projectFactory = base.ProjectFactory,
                textDocumentFactoryService = base.DocumentFactoryService
            )

    member __.GetView(buffer) =
        createMockTextView buffer

    member __.GetTagger(buffer, view) = 
        taggerProvider.CreateTagger<_>(view, buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))
        |> Seq.choose(fun span ->
                match box span.Tag with
                | :? SmartTag as tag ->
                    tag.ActionSets |> Seq.map (fun ac -> seq ac.Actions) |> Some
                | _ -> 
                    None)
        |> Seq.concat

module ImplementInterfaceSmartTaggerTests =

    let helper = ImplementInterfaceSmartTaggerHelper()
    
    [<Test>]
    let ``return nothing if interfaces are empty``() = 
        let content = """
type IEmptyInterface = interface end
type Impl() =
    interface IEmptyInterface
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 4 15) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger) 
                       |> Seq.concat 
                       |> Seq.toList 
                       |> assertEqual [])

    [<Test>]
    let ``return nothing if all members have been implemented and there is no type error``() = 
        let content = """
let _ =
    { new System.IDisposable with
          member x.Dispose(): unit = 
              failwith "Not implemented yet"
           }
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 18) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger) 
                       |> Seq.concat 
                       |> Seq.toList 
                       |> assertEqual [])

    [<Test>]
    let ``should insert all members if no member has been implemented``() = 
        let content = """
let _ =
    { new System.IDisposable }
"""
        // Use separate file name since asynchronous action might fire a bit later
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () ->  view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 18) |> ignore)
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.head
                // Test tag insertion in an asynchronous manner
                testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
                    (fun () -> tagToInsert.Invoke())
                    (fun () -> 
                        buffer.CurrentSnapshot.GetText() |> assertEquivString """
let _ =
    { new System.IDisposable with
          member x.Dispose(): unit = 
              failwith "Not implemented yet"
           }
"""))

    [<Test>]
    let ``should insert selective members if some members have been implemented``() = 
        let content = """
type Interface =
    abstract member Method1 : int -> int
    abstract member Method2 : int -> int
    abstract member Method3 : int -> int
type Class() =
    interface Interface with 
        member __.Method1(n) = 2 * n
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () ->  view.Caret.MoveTo(snapshotPoint view.TextSnapshot 7 16) |> ignore)
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.head
                testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
                    (fun () -> tagToInsert.Invoke())
                    (fun () -> 
                        buffer.CurrentSnapshot.GetText() |> assertEquivString """
type Interface =
    abstract member Method1 : int -> int
    abstract member Method2 : int -> int
    abstract member Method3 : int -> int
type Class() =
    interface Interface with
        member x.Method2(arg1: int): int = 
            failwith "Not implemented yet"
        member x.Method3(arg1: int): int = 
            failwith "Not implemented yet"
         
        member __.Method1(n) = 2 * n
"""))

    [<Test>]
    let ``should insert duplicated members once``() = 
        let content = """
type Interface1 =
    abstract member Method : int -> int
type Interface2 =
    abstract member Method : int -> int
type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int
type Class() =
    interface Interface3
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () ->  view.Caret.MoveTo(snapshotPoint view.TextSnapshot 11 16) |> ignore)
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.head
                testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
                    (fun () -> tagToInsert.Invoke())
                    (fun () -> 
                        buffer.CurrentSnapshot.GetText() |> assertEquivString """
type Interface1 =
    abstract member Method : int -> int
type Interface2 =
    abstract member Method : int -> int
type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int
type Class() =
    interface Interface3 with
        member x.Method(arg1: int): int = 
            failwith "Not implemented yet"
        member x.Method3(arg1: int): int = 
            failwith "Not implemented yet"
        
"""))

    [<Test>]
    let ``should generate members correctly for indexer-like properties``() = 
        let content = """
type Interface =
    abstract member Method1 : int -> int with get
    abstract member Method2 : int -> unit with set
type Class() =
    interface Interface
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () ->  view.Caret.MoveTo(snapshotPoint view.TextSnapshot 6 16) |> ignore)
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.last
                testEventTrigger buffer.Changed "Timed out before buffer updated" timeout
                    (fun () -> tagToInsert.Invoke())
                    (fun () -> 
                        buffer.CurrentSnapshot.GetText() |> assertEquivString """
type Interface =
    abstract member Method1 : int -> int with get
    abstract member Method2 : int -> unit with set
type Class() =
    interface Interface with
        member x.Method1
            with get (arg1) = failwith "Not implemented yet"
        member x.Method2
            with set (arg1) (v) = failwith "Not implemented yet"
        
"""))