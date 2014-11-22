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
#load "CommentOutlining.fs"


open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.Outlining.Comments



type testSnapshotLine (str, num) =
    new(str) = testSnapshotLine (str,0)
    interface ITextSnapshotLine with
        member __.End: SnapshotPoint                     = Unchecked.defaultof<_>
        member __.EndIncludingLineBreak: SnapshotPoint   = Unchecked.defaultof<_>      
        member __.Extent: SnapshotSpan                   = Unchecked.defaultof<_>
        member __.ExtentIncludingLineBreak: SnapshotSpan = Unchecked.defaultof<_>
        member __.GetLineBreakText(): string             = Unchecked.defaultof<_>
        member __.GetText(): string                      = str
        member __.GetTextIncludingLineBreak(): string    = Unchecked.defaultof<_>
        member __.Length: int                            = Unchecked.defaultof<_>
        member __.LengthIncludingLineBreak: int          = Unchecked.defaultof<_>
        member __.LineBreakLength: int                   = Unchecked.defaultof<_>
        member __.LineNumber: int                        = num
        member __.Snapshot: ITextSnapshot                = Unchecked.defaultof<_>
        member __.Start: SnapshotPoint                   = Unchecked.defaultof<_>
    override __.ToString() = str



let printseq s = s |> Seq.iteri ( fun num elm -> printfn "%d - %A, " num elm )
                 printf "\n"




let rng = Random()


let options = [| "/// </summary> "; "??? //// "; "// Ouch" ; "////" ;"/// (* *)";"      ///"; " // /" ; 
                 " //<<><>><><"   ; "/ / / / / /"; "/slippery/" |]

let comOpt  = [| "// a comment "; "??? //// "; "// Ouch" ; "// //" ;"/// (* *)";"      //"; " // /" ; 
                 " //<<><>><><"   ; "/ / / / / /"; "/slippery/" |]

let genSeq (opts:string[]) = 
    let randline num = 
        if num >= 0 && num < opts.Length then
            testSnapshotLine opts.[num] :> ITextSnapshotLine
        else 
            testSnapshotLine opts.[0]  :> ITextSnapshotLine

    let rnd = new Random()
    let rec gen _ = 
        seq{    let textline = randline <| rnd.Next(0,opts.Length-1) 
                yield  textline
                yield! gen ()   }
    gen()
               
let staticSeq len opts =  
    (Seq.take len (genSeq opts) )
    |> Seq.toList 
    |> List.mapi ( fun num elm -> testSnapshotLine (elm.GetText(), num )
                                    :> ITextSnapshotLine)
    |> List.toSeq



#time "on"
;;
//let docSeq =  staticSeq 50 options
let comSeq =  staticSeq 50 comOpt
;;
//printfn "testSeq"
//printseq testSeq
//printfn "\ngetBlocks"
//getBlocks testSeq |> Seq.iter( fun x -> printseq x )
//;;
printseq comSeq
printfn "testSeq"
printfn "\nBUILD COMMENT REGIONS"
printfn   "=====================\n"
comSeq  |> buildCommentRegions
        |> Seq.iter ( fun x ->  printf "%A\n" x )
;;

