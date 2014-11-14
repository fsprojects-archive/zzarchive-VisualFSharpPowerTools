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

open EnvDTE
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools



type testSnapshotLine (str) =
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
        member x.LineNumber: int                        = Unchecked.defaultof<_>
        member x.Snapshot: ITextSnapshot                = Unchecked.defaultof<_>
        member x.Start: SnapshotPoint                   = Unchecked.defaultof<_>
    override __.ToString() = str
         

let printseq s = s |> Seq.iter ( fun elm -> printf "%A, " elm )
                 printf "\n"

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
               
let staticSeq len =  (Seq.take len genSeq) |> Seq.toList |> List.toSeq

let testSeq =  staticSeq 20
;;
printseq testSeq;;
printseq testSeq;;

let startDoc = "///"
let validStarts = [ "///"; "//"; "*)"; "(*";]

let lineMatch (line:ITextSnapshotLine) start = 
        line.GetText().TrimStart().StartsWith(start)

let lineMatchAny (line:ITextSnapshotLine) ls =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

let rec takeBlock ( lines:seq<ITextSnapshotLine> ) = 
    seq {   if Seq.isEmpty lines then () else 
            let block = lines
                        |> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                        |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
            let rest = lines |> Seq.skip ( Seq.length block )
            yield block
            yield! takeBlock rest
        }

let getBlocks ( lines:seq<ITextSnapshotLine> ) = 
    lines |> Seq.unfold( fun lines -> 
        if Seq.isEmpty lines then None else 
        let block = lines|> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                         |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
        let rest = lines |> Seq.skip ( Seq.length block )
        Some(block,rest))

#time "on"
;;
printfn "\ngetBlocks"
getBlocks testSeq |> Seq.iter( fun x -> printseq x )
printfn "testSeq"
printseq testSeq
;;
//printfn "\ntakeBlock"
//takeBlock testSeq |> Seq.iter ( fun x ->  printf "%A " x )
//printfn "testSeq"
//printseq testSeq


