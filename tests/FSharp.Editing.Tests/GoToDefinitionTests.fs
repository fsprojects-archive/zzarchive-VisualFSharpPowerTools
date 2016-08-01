module FSharp.Editing.Tests.GoToDefinitionTests

open NUnit.Framework
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Data
open FSharp.Editing
open FSharp.Editing.Features
open CodeGenerationTestInfrastructure
open LanguageServiceTestHelper
open System
open System.Xml.Linq
open System.Collections.Generic
open System.IO
open FSharp.Editing.Features.SignatureGenerator

let languageService = LanguageService()
let xmlFileCache = Dictionary()

type GenericTestsFolder = FSharp.Management.FileSystem< const(__SOURCE_DIRECTORY__ + "/../data/gotodef")>
type GenericTestSettings = JsonProvider<GenericTestsFolder.``generic-sample``.``settings.list.json``, SampleIsList = true>

let genericCasesDirectory = DirectoryInfo(GenericTestsFolder.``generic-cases``.Path)

type CompareStringResult =
    | Equal
    | NotEqual of message: string

let compareString (expected: string) (actual: string) =
    let expected = expected.Replace("\r","")
    let actual = actual.Replace("\r","")
    use messageWriter = new Internal.TextMessageWriter(MaxLineLength = 30000)
    let equalConstraint = Constraints.EqualConstraint(expected)
    let result = equalConstraint.ApplyTo(actual)
    result.WriteMessageTo(messageWriter)
    if result.IsSuccess then
       Equal
    else
        NotEqual (messageWriter.ToString().Replace(@"\r",@"").Replace(@"\n", Environment.NewLine))

let tryGenerateDefinitionFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    // We use .NET 4.5.1 that comes with VS 2013 in order that Xml documentation files are retrieved correctly.
    let projectOptions = { projectOptions document.FullName with OtherOptions = argsDotNET451 }

    asyncMaybe {
        let! _range, symbolAtPos = codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)
        let! parseResults = codeGenService.ParseFileInProject(document, projectOptions)
        let! parseTree = parseResults.ParseTree
        let getXmlDocBySignature = 
            let xmlFile = 
                symbolUse.Symbol.Assembly.FileName 
                |> Option.bind (fun fileName -> 
                    let xmlFile = Path.ChangeExtension(fileName, ".xml")
                    if File.Exists xmlFile then Some xmlFile
                    else 
                        // In Linux, we need to check for upper case extension separately
                        let xmlFile = Path.ChangeExtension(xmlFile, Path.GetExtension(xmlFile).ToUpper())
                        if File.Exists xmlFile then Some xmlFile else None)
            let xmlMemberMap =
                match xmlFile with
                | Some xmlFile ->
                    match xmlFileCache.TryGetValue(xmlFile) with
                    | true, xmlMemberMap ->
                        xmlMemberMap
                    | false, _ ->
                        let xmlMemberMap = Dictionary()
                        let doc = XDocument.Load(xmlFile)
                        if doc <> null then                            
                            for key, value in 
                              [ for e in doc.Descendants(XName.Get "member") do
                                  let attr = e.Attribute(XName.Get "name") 
                                  if attr <> null && not (String.IsNullOrEmpty(attr.Value)) then 
                                    yield attr.Value, e ] do
                                xmlMemberMap.Add(key, value)
                            xmlFileCache.Add(xmlFile, xmlMemberMap)
                        xmlMemberMap
                | None -> 
                    Dictionary()
            fun signature ->
                match xmlMemberMap.TryGetValue(signature) with
                | true, element ->
                    try [ element.Element(XName.Get "summary").Value ] with _ -> []
                | false, _ ->
                    []
            
        let openDeclarations = OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation caretPos parseTree
        let! generatedCode = formatSymbol getXmlDocBySignature 4 symbolUse.DisplayContext openDeclarations symbolUse.Symbol Filterer.NoFilters BlankLines.Default
        return generatedCode
    }
    |> Async.RunSynchronously

let validateSignature source signature =
    let projFileName = @"/Project.fsproj"
    let sourceFile = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    File.WriteAllText(sourceFile, source)
    let signatureFile = @"/Temp.fsi"
    let opts =
        { ProjectFileName = projFileName
          ProjectFileNames = [| sourceFile; signatureFile|]
          OtherOptions = LanguageServiceTestHelper.args
          ReferencedProjects = Array.empty
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = true
          LoadTime = DateTime.UtcNow
          UnresolvedReferences = None }
    let results =
        languageService.ParseAndCheckFileInProject(opts, signatureFile, signature, AllowStaleResults.No)
        |> Async.RunSynchronously
    results.CheckErrors

let generateDefinitionFromPos caretPos src =

    let signature = Option.get (tryGenerateDefinitionFromPos caretPos src)
    match validateSignature src signature with
    | None | Some [||] -> ()
    | Some errors -> 
        printfn "Output signature: %s" signature
        failwithf "Type checking results in errors: %A" errors
    signature

let generateDefinitionFromPosNoValidation caretPos src = 
    Option.get (tryGenerateDefinitionFromPos caretPos src)

type SignatureValidation =
    | WithoutValidation
    | WithValidation

[<NoComparison>]
type GenericTest =
    | Skip of reason: string
    | AssertEqual of input: string * position: pos * expected: string * SignatureValidation
    | AssertEqualFirstLines of input: string * position: pos * expected: string * firstLines: int

[<NoComparison>]
type GenericTestResult =
    | Passed
    | Inconclusive
    | Failed of exn
    | IncorrectFolderSetup of message: string
    | IncorrectResult of message: string
    static member HasFailed = function | Passed | Inconclusive -> false | _ -> true

let getNLines n = 
    String.getLines 
    >> Array.truncate n 
    >> String.concat Environment.NewLine

[<Test>]
let ``run generic tests`` () =
  
    let parseTest (directory: DirectoryInfo) =
        let source   = directory.FullName </> "input.fs"      |> File.ReadAllText
        let expected = directory.FullName </> "expected.fs"   |> File.ReadAllText
        let settings = directory.FullName </> "settings.json" |> File.ReadAllText |> GenericTestSettings.Parse
        let caretPos = mkPos settings.Pos.Line settings.Pos.Col
        let signatureValidation =
          match settings.Validation with
          | Some false -> WithoutValidation
          | None | Some true -> WithValidation
          
        let ignoreOnUnix = 
            match settings.IgnoreOnUnix with
            | Some true -> true
            | _ -> false
        if ignoreOnUnix && PlatformID.Win32NT <> Environment.OSVersion.Platform then
            Skip "ignore on unix"
        else
            match settings.AssertType with
            | "eq"           -> AssertEqual(source, caretPos, expected, signatureValidation)
            | "firstLinesEq" -> AssertEqualFirstLines(source, caretPos, expected, settings.FirstLineCount.Value)
            | _ -> failwithf "assertType '%s' is invalid, expected 'eq' or 'firstLinesEq'" settings.AssertType

    let executeTest directory =
        let test =
            try
               Ok (parseTest directory)
            with
            | e -> Fail (string e)
        directory.Name,
        match test with
        | Fail message -> IncorrectFolderSetup message
        | Ok test -> 
            let expectedAndActual =
                match test with
                | Skip _ -> None
                | AssertEqual (input, pos, expected, WithoutValidation) -> 
                    let actual = generateDefinitionFromPosNoValidation pos input
                    Some (expected, actual)
                | AssertEqual (input, pos, expected, WithValidation) -> 
                    let actual = generateDefinitionFromPos pos input
                    Some (expected, actual)
                | AssertEqualFirstLines (input, pos, expected, linecount) ->
                    let actual = generateDefinitionFromPos pos input
                    let expected = expected 
                    Some (expected |> getNLines linecount, actual |> getNLines linecount)
            match expectedAndActual with
            | Some (expected, actual) ->
                match compareString expected actual with
                | Equal -> Passed
                | NotEqual message -> IncorrectResult message
            | None ->
                match test with
                | Skip reason -> printfn "%s skipped: %s" directory.Name reason; Inconclusive
                | _ -> failwithf "unexpected to reach this %s" directory.Name
        
    let results = 
        genericCasesDirectory.EnumerateDirectories()
        |> Seq.map executeTest
        |> Seq.toArray

    let inconclusiveCount =
        results 
        |> Seq.filter (snd >> (=) Inconclusive)
        |> Seq.length

    match inconclusiveCount with
    | 0 -> printfn "tested %i cases" results.Length
    | _ -> printfn "tested %i cases (%i inconclusive(s))" results.Length inconclusiveCount
  
    let failures = results |> Array.filter (snd >> GenericTestResult.HasFailed)
    if failures.Length > 0 then
        for n, r in failures do
            printfn "%s failed: %A" n r
        printfn "%i failed" failures.Length
        Assert.Fail()

[<Test>]
let ``handle active patterns as parts of module declarations`` () =
    """open Microsoft.FSharp.Quotations
let f = Patterns.(|AddressOf|_|)"""
    |> generateDefinitionFromPos (Pos.fromZ 1 13)
    |> fun str -> str.Contains("val (|AddressSet|_|) : input:Expr -> (Expr * Expr) option")
    |> assertTrue

[<Test>]
let ``handle generic definitions 2`` () =
    """open System
let x: Collections.Generic.Dictionary<string, int> = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 1 30)
    |> fun str -> str.Contains("member Values : System.Collections.Generic.Dictionary<'TKey,'TValue>.ValueCollection")
    |> assertTrue

[<Test>]
let ``handle operators as compiled members`` () =
    """let x: System.DateTime = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 0 20)
    |> fun str -> str.Contains("static member op_GreaterThanOrEqual : t1:System.DateTime * t2:System.DateTime -> bool")
    |> assertTrue

[<Test>]
let ``handle uninstantiated type parameters`` () =
    """let f x = array2D x"""
    |> generateDefinitionFromPos (Pos.fromZ 0 12)
    // Resulting types from FCS are non-deterministic so we are not yet able to assert a specific answer.
    |> fun str -> str.Contains("val array2D : rows:seq") && not <| str.Contains("'?")
    |> assertTrue

let generateFileNameForSymbol caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    let projectOptions = projectOptions document.FullName

    asyncMaybe {
        let! _range, symbolAtPos = codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)
        
        return getFileNameFromSymbol symbolUse.Symbol
    }
    |> Async.RunSynchronously
    |> Option.get

[<Test>]
let ``file names for union cases should refer to union type names`` () =
    """let x = Some 0 "" """
    |> generateFileNameForSymbol (Pos.fromZ 0 8)
    |> assertSrcAreEqual "Microsoft.FSharp.Core.option.fsi"

[<Test>]
let ``file names for record fields should refer to record type names`` () =
     """
type MyRecord =
    {
        Field1: int
        Field2: string -> string
    }

let r = { Field1 = 0; Field2 = id }"""
    |> generateFileNameForSymbol (Pos.fromZ 7 11)
    |> assertSrcAreEqual "File.MyRecord.fsi"

[<Test>]
let ``file names for members should refer to type names`` () =
    """open System
let _ = Async.AwaitTask"""
    |> generateFileNameForSymbol (Pos.fromZ 1 16)
    |> assertSrcAreEqual "Microsoft.FSharp.Control.FSharpAsync.fsi"

// Tests to add:
// TODO: buffer should have the same behavior as C#'s generated metadata ([from metadata] instead of [read-only] header, preview buffer and not permanent buffer)
// TODO: set cursor on method/... when symbol is a method/union case/enum value/record field
