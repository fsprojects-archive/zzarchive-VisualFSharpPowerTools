namespace FSharpVSPowerTools.Tests

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities

type MockTextDocument(filePath: string, textBuffer: ITextBuffer) =
    interface ITextDocument with
        [<CLIEvent>]
        member x.DirtyStateChanged: IEvent<EventHandler, _> = 
            failwith "Not implemented yet"
        
        member x.Dispose(): unit = 
            failwith "Not implemented yet"
        
        member x.Encoding
            with get (): Text.Encoding = 
                failwith "Not implemented yet"
            and set (v: Text.Encoding): unit = 
                failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.EncodingChanged: IEvent<EventHandler<EncodingChangedEventArgs>, _> = 
            failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.FileActionOccurred: IEvent<EventHandler<TextDocumentFileActionEventArgs>, _> = 
            failwith "Not implemented yet"
        
        member x.FilePath: string = 
            filePath
        
        member x.IsDirty: bool = 
            failwith "Not implemented yet"
        
        member x.IsReloading: bool = 
            failwith "Not implemented yet"
        
        member x.LastContentModifiedTime: DateTime = 
            failwith "Not implemented yet"
        
        member x.LastSavedTime: DateTime = 
            failwith "Not implemented yet"
        
        member x.Reload(): ReloadResult = 
            failwith "Not implemented yet"
        
        member x.Reload(options: EditOptions): ReloadResult = 
            failwith "Not implemented yet"
        
        member x.Rename(newFilePath: string): unit = 
            failwith "Not implemented yet"
        
        member x.Save(): unit = 
            failwith "Not implemented yet"
        
        member x.SaveAs(filePath: string, overwrite: bool): unit = 
            failwith "Not implemented yet"
        
        member x.SaveAs(filePath: string, overwrite: bool, createFolder: bool): unit = 
            failwith "Not implemented yet"
        
        member x.SaveAs(filePath: string, overwrite: bool, newContentType: IContentType): unit = 
            failwith "Not implemented yet"
        
        member x.SaveAs(filePath: string, overwrite: bool, createFolder: bool, newContentType: IContentType): unit = 
            failwith "Not implemented yet"
        
        member x.SaveCopy(filePath: string, overwrite: bool): unit = 
            failwith "Not implemented yet"
        
        member x.SaveCopy(filePath: string, overwrite: bool, createFolder: bool): unit = 
            failwith "Not implemented yet"
        
        member x.SetEncoderFallback(fallback: Text.EncoderFallback): unit = 
            failwith "Not implemented yet"
        
        member x.TextBuffer: ITextBuffer = 
            textBuffer
        
        member x.UpdateDirtyState(isDirty: bool, lastContentModifiedTime: DateTime): unit = 
            failwith "Not implemented yet"
        
