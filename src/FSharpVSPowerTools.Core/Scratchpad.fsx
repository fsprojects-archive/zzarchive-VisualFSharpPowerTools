#r "../../bin/FSharp.Compiler.Service.dll"

#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/UntypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
//      "../../src/FSharpVSPowerTools.Core/XmlDocParser.fs"
//      "../../src/FSharpVSPowerTools.Core/DepthParser.fs"
//      "../../src/FSharpVSPowerTools.Core/NavigableItemsCollector.fs"
//      "../../src/FSharpVSPowerTools.Core/NavigateToIndex.fs"
//      "../../src/FSharpVSPowerTools.Core/IdentifierUtils.fs"
//      "../../src/FSharpVSPowerTools.Core/OpenDeclarationsGetter.fs"
//      "../../src/FSharpVSPowerTools.Core/SourceCodeClassifier.fs"
//      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
//      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
//      "../../src/FSharpVSPowerTools.Core/RecordStubGenerator.fs"
//      "../../src/FSharpVSPowerTools.Core/UnionPatternMatchCaseGenerator.fs"
//      "../../src/FSharpVSPowerTools.Core/UnopenedNamespacesResolver.fs"
//      "../../src/FSharpVSPowerTools.Core/SignatureGenerator.fs"
//      "../../src/FSharpVSPowerTools.Core/TaskListCommentExtractor.fs"

open System.IO
open FSharpVSPowerTools

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()

let args =
        [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"|]

let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let getLexer source =
    let sourceLines = String.getLines source
    let lexer = 
        { new LexerBase() with
            member __.GetSymbolFromTokensAtLocation (_tokens, line, col) =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr SymbolLookupKind.ByLongIdent args Lexer.queryLexState
            member __.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source args line lineStr Lexer.queryLexState 
            member __.LineCount = sourceLines.Length }
    lexer

#time "on";;

let lexer = 
    getLexer """open A.B.C.D
"""

//let tokens = lexer.TokenizeAll();;
let symbol = lexer.GetSymbolFromTokensAtLocation([], 0, 6);;