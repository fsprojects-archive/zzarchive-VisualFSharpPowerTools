namespace FSharpVSPowerTools.Tests

open System
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities

type MockTextSnapshot(buffer: ITextBuffer, text: string) =
    interface ITextSnapshot with
        member x.ContentType: IContentType = 
            failwith "Not implemented yet"
        
        member x.CopyTo(sourceIndex: int, destination: char [], destinationIndex: int, count: int): unit = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingPoint(position: int, trackingMode: PointTrackingMode): ITrackingPoint = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingPoint(position: int, trackingMode: PointTrackingMode, trackingFidelity: TrackingFidelityMode): ITrackingPoint = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingSpan(span: Span, trackingMode: SpanTrackingMode): ITrackingSpan = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingSpan(span: Span, trackingMode: SpanTrackingMode, trackingFidelity: TrackingFidelityMode): ITrackingSpan = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingSpan(start: int, length: int, trackingMode: SpanTrackingMode): ITrackingSpan = 
            failwith "Not implemented yet"
        
        member x.CreateTrackingSpan(start: int, length: int, trackingMode: SpanTrackingMode, trackingFidelity: TrackingFidelityMode): ITrackingSpan = 
            failwith "Not implemented yet"
        
        member x.GetLineFromLineNumber(lineNumber: int): ITextSnapshotLine = 
            failwith "Not implemented yet"
        
        member x.GetLineFromPosition(position: int): ITextSnapshotLine = 
            failwith "Not implemented yet"
        
        member x.GetLineNumberFromPosition(position: int): int = 
            failwith "Not implemented yet"
        
        member x.GetText(span: Span): string = 
            failwith "Not implemented yet"
        
        member x.GetText(startIndex: int, length: int): string = 
            failwith "Not implemented yet"
        
        member x.GetText(): string = 
            failwith "Not implemented yet"
        
        member x.Item
            with get (position: int): char = 
                failwith "Not implemented yet"
        
        member x.Length: int = 
            text.Length
        
        member x.LineCount: int = 
            failwith "Not implemented yet"
        
        member x.Lines: IEnumerable<ITextSnapshotLine> = 
            failwith "Not implemented yet"
        
        member x.TextBuffer: ITextBuffer = 
            failwith "Not implemented yet"
        
        member x.ToCharArray(startIndex: int, length: int): char [] = 
            failwith "Not implemented yet"
        
        member x.Version: ITextVersion = 
            failwith "Not implemented yet"
        
        member x.Write(writer: IO.TextWriter, span: Span): unit = 
            failwith "Not implemented yet"
        
        member x.Write(writer: IO.TextWriter): unit = 
            failwith "Not implemented yet"
         
