#r "Microsoft.VisualStudio"
#r "Microsoft.VisualStudio.Editor"
#r "Microsoft.VisualStudio.CoreUtility"
#r "Microsoft.VisualStudio.Text.Data"
#r "Microsoft.VisualStudio.Text.Logic"
#r "Microsoft.VisualStudio.Text.UI"
#r "System.ComponentModel.Composition"
#r @"..\..\..\packages\FSharp.Compiler.Service\lib\net45\FSharp.Compiler.Service.dll"
#r "EnvDTE"
#r @"..\..\FSharpVSPowerTools.Core\bin\Debug\FSharpVSPowerTools.Core.dll"

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools



type testSnapshotLine (str, num) =
    new(str) = testSnapshotLine (str,0)
    interface ITextSnapshotLine with
        member x.End: SnapshotPoint                     = Unchecked.defaultof<_>
        member x.EndIncludingLineBreak: SnapshotPoint   = Unchecked.defaultof<_>      
        member x.Extent: SnapshotSpan                   = Unchecked.defaultof<_>
        member x.ExtentIncludingLineBreak: SnapshotSpan = Unchecked.defaultof<_>
        member x.GetLineBreakText(): string             = Unchecked.defaultof<_>
        member x.GetText(): string                      = str
        member x.GetTextIncludingLineBreak(): string    = Unchecked.defaultof<_>
        member x.Length: int                            = Unchecked.defaultof<_>
        member x.LengthIncludingLineBreak: int          = Unchecked.defaultof<_>
        member x.LineBreakLength: int                   = Unchecked.defaultof<_>
        member x.LineNumber: int                        = num
        member x.Snapshot: ITextSnapshot                = Unchecked.defaultof<_>
        member x.Start: SnapshotPoint                   = Unchecked.defaultof<_>
    override __.ToString() = str

type testTextBuffer () =
    interface ITextBuffer with
        member x.ChangeContentType(newContentType: Microsoft.VisualStudio.Utilities.IContentType, editTag: obj): unit = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.Changed: IEvent<EventHandler<TextContentChangedEventArgs>,TextContentChangedEventArgs> = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.ChangedHighPriority: IEvent<EventHandler<TextContentChangedEventArgs>,TextContentChangedEventArgs> = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.ChangedLowPriority: IEvent<EventHandler<TextContentChangedEventArgs>,TextContentChangedEventArgs> = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.Changing: IEvent<EventHandler<TextContentChangingEventArgs>,TextContentChangingEventArgs> = 
            Unchecked.defaultof<_>
        
        member x.CheckEditAccess(): bool = 
            Unchecked.defaultof<_>
        
        member x.ContentType: Microsoft.VisualStudio.Utilities.IContentType = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.ContentTypeChanged: IEvent<EventHandler<ContentTypeChangedEventArgs>,ContentTypeChangedEventArgs> = 
            Unchecked.defaultof<_>
        
        member x.CreateEdit(options: EditOptions, reiteratedVersionNumber: Nullable<int>, editTag: obj): ITextEdit = 
            Unchecked.defaultof<_>
        
        member x.CreateEdit(): ITextEdit = 
            Unchecked.defaultof<_>
        
        member x.CreateReadOnlyRegionEdit(): IReadOnlyRegionEdit = 
            Unchecked.defaultof<_>
        
        member x.CurrentSnapshot: ITextSnapshot = 
            Unchecked.defaultof<_>
        
        member x.Delete(deleteSpan: Span): ITextSnapshot = 
            Unchecked.defaultof<_>
        
        member x.EditInProgress: bool = 
            Unchecked.defaultof<_>
        
        member x.GetReadOnlyExtents(span: Span): NormalizedSpanCollection = 
            Unchecked.defaultof<_>
        
        member x.Insert(position: int, text: string): ITextSnapshot = 
            Unchecked.defaultof<_>
        
        member x.IsReadOnly(position: int): bool = 
            Unchecked.defaultof<_>
        
        member x.IsReadOnly(position: int, isEdit: bool): bool = 
            Unchecked.defaultof<_>
        
        member x.IsReadOnly(span: Span): bool = 
            Unchecked.defaultof<_>
        
        member x.IsReadOnly(span: Span, isEdit: bool): bool = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.PostChanged: IEvent<EventHandler,EventArgs> = 
            Unchecked.defaultof<_>
        
        member x.Properties: Microsoft.VisualStudio.Utilities.PropertyCollection = 
            Unchecked.defaultof<_>
        
        [<CLIEvent>]
        member x.ReadOnlyRegionsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
            Unchecked.defaultof<_>
        
        member x.Replace(replaceSpan: Span, replaceWith: string): ITextSnapshot = 
            Unchecked.defaultof<_>
        
        member x.TakeThreadOwnership(): unit = 
            Unchecked.defaultof<_>
        


let printseq s = s |> Seq.iter ( fun elm -> printfn "%A, " elm )
                 printf "\n"


[<Struct>]
type Region =
    val StartLine   : int
    val EndLine     : int


    new ( startline, (*startoffset,*) endline ) =
        {   StartLine   = startline
    ///         StartOffset = startoffset
            EndLine     = endline        }
    override self.ToString() = 
        sprintf "[Region {ln %d - ln %d}]" self.StartLine self.EndLine



let rng = Random()


let options = [| "/// </summary> "; "??? //// "; "// Ouch" ; "////" ;"/// (* *)";"      ///"; " // /"  |]

let genSeq = 
    let testLines = options
    let randline num = 
        if num >= 0 && num < testLines.Length then
            testSnapshotLine testLines.[num] :> ITextSnapshotLine
        else 
            testSnapshotLine testLines.[0]  :> ITextSnapshotLine

    let rnd = new Random()
    let rec gen _ = 
        seq{    let textline = randline <| rnd.Next(0,testLines.Length-1) 
                yield  textline
                yield! gen ()   }
    gen()
               
let staticSeq len =  
    (Seq.take len genSeq) 
    |> Seq.toList 
    |> List.mapi ( fun num elm -> testSnapshotLine (elm.GetText(), num )
                                    :> ITextSnapshotLine)
    |> List.toSeq

;;

let startDoc = "///"
let validStarts = [ "///"; "//"; "*)"; "(*";]

let lineMatch (line:ITextSnapshotLine) start = 
        line.GetText().TrimStart().StartsWith(start)

let lineMatchAny (line:ITextSnapshotLine) ls =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

let rec takeBlock ( lines:seq<ITextSnapshotLine> ) = 
    seq {   printfn "The length of the seq is %A" (lines.Count())
            if lines.Count() = 0 then () else 
            let block = lines
                        |> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                        |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
//            printfn "\n====="
//            printfn   "Block"
//            printfn   "====="
            block |> Seq.iter ( printfn "%A")
            let rest = lines |> Seq.skip ( Seq.length block )
            yield block
            if lines.Count() = rest.Count() then () else 
            yield! takeBlock rest
        }


let takeBlocks ( lines:seq<ITextSnapshotLine> )= 
    let rec loop (lines:seq<ITextSnapshotLine>) =
        seq{    if lines.Count() = 0 then () else 
                let block = lines
                            |> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                            |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
                let rest = lines|> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc ) 
                                |> Seq.skip ( Seq.length block )
                yield block
                yield! loop rest
            }
    loop lines |> Seq.map(List.ofSeq) |> List.ofSeq



let getBlocks ( lines:seq<ITextSnapshotLine> ) = 
    lines |> Seq.unfold( fun lines -> 
        if Seq.isEmpty lines then None else 
        let block = lines|> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                         |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
        let rest = lines |> Seq.skip ( Seq.length block )
        Some(block,rest))

let buildRegions (regls:ITextSnapshotLine list list) =
    let build (ls:ITextSnapshotLine list) =
        match ls with 
        | [] -> None
        | _  -> let fstline = ls.[0].LineNumber
                let lstline = ls.[ls.Length-1].LineNumber
                if fstline = lstline then None else
                Some <| Region( fstline, lstline)
    match regls with
        | [] -> []
        | _  -> regls   |> List.map build 
                        |> List.filter ( fun x -> x <> None ) 
                        |> List.map Option.get 






#time "on"
;;
let testSeq =  staticSeq 20
;;
//printfn "testSeq"
//printseq testSeq
//printfn "\ngetBlocks"
//getBlocks testSeq |> Seq.iter( fun x -> printseq x )
//;;
printseq testSeq
printfn "testSeq"
printfn "\ntakeBlock"
testSeq |> (takeBlocks >> buildRegions )
//|> Seq.iter ( fun x ->  printf "%A " x )
;;

