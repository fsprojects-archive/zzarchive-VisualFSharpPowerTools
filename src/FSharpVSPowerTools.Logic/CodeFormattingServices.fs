namespace FSharpVSPowerTools

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Text

[<AllowNullLiteral>]
type CodeFormattingServices (editorOptionsFactory : IEditorOptionsFactoryService, 
                             editorOperationsFactoryService : IEditorOperationsFactoryService,
                             textBufferUndoManagerProvider : ITextBufferUndoManagerProvider, 
                             textDocumentFactoryService : ITextDocumentFactoryService) = 
    member val EditorOptionsFactory = editorOptionsFactory
    member val TextBufferUndoManagerProvider = textBufferUndoManagerProvider
    member val EditorOperationsFactoryService = editorOperationsFactoryService
    member val TextDocumentFactoryService = textDocumentFactoryService
