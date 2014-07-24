[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Classification
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

let createSingleFileProject(fileName: string) =
    {
        new IProjectProvider with
            member x.CompilerOptions: string [] = 
                notimpl
            
            member x.FullOutputFilePath: string = 
                notimpl
            
            member x.GetAllReferencedProjectFileNames(): string list = 
                notimpl
            
            member x.GetProjectCheckerOptions(arg1: LanguageService): Async<ProjectOptions> = 
                notimpl
            
            member x.GetReferencedProjects(): IProjectProvider list = 
                notimpl
            
            member x.IsForStandaloneScript: bool = 
                notimpl
            
            member x.ProjectFileName: string = 
                notimpl
            
            member x.SourceFiles: string [] = 
                notimpl
            
            member x.TargetFramework: FSharpTargetFramework = 
                notimpl
            
    }

let createDocumentFactoryService() =
    { 
        new ITextDocumentFactoryService with
            member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType): ITextDocument = 
                notimpl
            member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType, encoding: Text.Encoding, characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
                notimpl
            member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType, attemptUtf8Detection: bool, characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
                notimpl
            member x.CreateTextDocument(textBuffer: ITextBuffer, filePath: string): ITextDocument = 
                notimpl
            [<CLIEvent>]
            member x.TextDocumentCreated: IEvent<EventHandler<TextDocumentEventArgs>, _> = 
                notimpl
            [<CLIEvent>]
            member x.TextDocumentDisposed: IEvent<EventHandler<TextDocumentEventArgs>, _> = 
                notimpl
        
            member x.TryGetTextDocument(textBuffer: ITextBuffer, textDocument: byref<ITextDocument>): bool = 
                textBuffer.Properties.TryGetProperty(typeof<ITextDocument>, &textDocument) 
    }

let createClassificationType classificationType =
    {
        new IClassificationType with
            member x.BaseTypes: IClassificationType seq = 
                notimpl
            member x.IsOfType(``type``: string): bool = 
                notimpl
                            
            member x.Classification: string = classificationType            
    }

let createClassificationTypeRegistryService() =
    { 
        new IClassificationTypeRegistryService with
            member x.CreateClassificationType(``type``: string, baseTypes: IClassificationType seq): IClassificationType = 
                notimpl
            member x.CreateTransientClassificationType(baseTypes: IClassificationType seq): IClassificationType = 
                notimpl
            member x.CreateTransientClassificationType(baseTypes: IClassificationType []): IClassificationType = 
                notimpl

            member x.GetClassificationType(``type``: string): IClassificationType = 
                createClassificationType ``type``
    }