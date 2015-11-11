namespace FSharpVSPowerTools.CodeFormatting

open System
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations

[<AllowNullLiteral>]
type CodeFormattingServices(editorOptionsFactory: IEditorOptionsFactoryService, 
                            editorOperationsFactoryService: IEditorOperationsFactoryService,
                            textBufferUndoManagerProvider: ITextBufferUndoManagerProvider, 
                            textDocumentFactoryService: ITextDocumentFactoryService,
                            serviceProvider: IServiceProvider) = 
    member val EditorOptionsFactory = editorOptionsFactory
    member val TextBufferUndoManagerProvider = textBufferUndoManagerProvider
    member val EditorOperationsFactoryService = editorOperationsFactoryService
    member val TextDocumentFactoryService = textDocumentFactoryService
    member val ServiceProvider = serviceProvider

module internal TextUtils =
    let getFSharpPos (point: VirtualSnapshotPoint) =
        let containingLine = point.Position.GetContainingLine()
        // F# compiler line numbers start at 1
        let lineNumber = containingLine.LineNumber + 1
        let charIndex = point.Position.Position - containingLine.Start.Position
        Range.mkPos lineNumber charIndex