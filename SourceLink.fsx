#r @"packages\SourceLink.SymbolStore\lib\net45\SourceLink.SymbolStore.CorSym.dll"
#r @"packages\SourceLink.SymbolStore\lib\net45\SourceLink.SymbolStore.dll"

open System
open System.IO
open SourceLink
open SourceLink.SymbolStore
open System.Reflection
 
let pdbFcs = Path.Combine(__SOURCE_DIRECTORY__, @"packages\FSharp.Compiler.Service\lib\net45\FSharp.Compiler.Service.pdb")
let dllFcs = Path.Combine(__SOURCE_DIRECTORY__, @"packages\FSharp.Compiler.Service\lib\net45\FSharp.Compiler.Service.dll")
let cacheDir = Path.Combine(__SOURCE_DIRECTORY__, @"packages")
 
let printPdbDocuments() =
    use pdb = File.OpenRead pdbFcs
    let symbolCache = SymbolCache cacheDir
    let pdbReader = symbolCache.ReadPdb pdb pdbFcs
    printfn "Number of documents: %i" pdbReader.Documents.Length
    for d in pdbReader.Documents do
        printfn "\npdb original source file path: %s" d.SourceFilePath
        printfn "it had an md5 checksum of: %s" d.ChecksumHex
        let url = pdbReader.GetDownloadUrl d.SourceFilePath
        if url.IsSome then
            let url = url |> Option.get
            printfn "has download url if source indexed: %A" url
            let downloadedFile = symbolCache.DownloadFile url
            printfn "downloaded the file to the cache %s" downloadedFile
 
let printMethods() =
    use pdb = File.OpenRead pdbFcs
    let symbolCache = SymbolCache cacheDir
    let pdbReader = symbolCache.ReadPdb pdb pdbFcs
    let dll = Assembly.LoadFrom dllFcs
    dll.DefinedTypes
//    |> Seq.filter (fun dt -> dt.FullName.StartsWith "Microsoft.FSharp.Compiler.SourceCodeServices")
    |> Seq.filter (fun dt -> dt.FullName = "Microsoft.FSharp.Compiler.SourceCodeServices.FSharpCheckFileResults")
    |> Seq.iter (fun dt ->
        for mbr in dt.GetMembers() do
            printfn "%s, %s" dt.FullName mbr.Name
            match pdbReader.GetMethod mbr.MetadataToken with
            | None -> ()
            | Some mth ->
                for sp in mth.SequencePoints do
                    printfn "    %s %d %d" sp.Document.SourceFilePath sp.Line sp.Column
                    let url = pdbReader.GetDownloadUrl sp.Document.SourceFilePath
                    if url.IsSome then
                        let url = url.Value
                        printfn "    %s" url
                        let replace (b:string) c (a:string) = a.Replace(b, c)
                        let browserUrl =
                            sprintf "%s#L%d"
                                (url |> replace "raw.githubusercontent" "github" |> replace "Service/" "Service/blob/") 
                                sp.Line
                        printfn "    %s" browserUrl 
    )
 

printPdbDocuments();;
printMethods();;
