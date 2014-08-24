#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/SignatureGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharpVSPowerTools.Core.Tests.GoToDefinitionTests
#endif

open NUnit.Framework
open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.SignatureGenerator
open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure

let args = 
    [|
        "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"
    |]

let languageService = LanguageService(fun _ -> ())
let project() =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]
    { ProjectFileName = projFileName
      ProjectFileNames = [| fileName |]
      ProjectOptions = args
      ReferencedProjects = Array.empty
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.UtcNow
      UnresolvedReferences = None }

let tryGenerateDefinitionFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)
    let projectOptions = project()

    asyncMaybe {
        let! _range, symbolAtPos =
            liftMaybe <| codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)

        let generatedCode = formatSymbol symbolUse.DisplayContext symbolUse.Symbol
        return generatedCode
    }
    |> Async.RunSynchronously

let generateDefinitionFromPos caretPos src =
    Option.get (tryGenerateDefinitionFromPos caretPos src)

[<Test; Ignore>]
let ``go to Tuple<'T1, 'T2> definition`` () =
    let _ = new Tuple<int, int>(1, 2)
    // TODO: accessibility modifiers
    // TODO: interface inheritance
    // TODO: class type attributes
    // TODO: member types attributes
    // TODO: member generic types
    // TODO: method arguments attributes
    // TODO: method arguments generic types
    // TODO: xml comments

    """open System

let x = new Tuple<int, int>(1, 2)"""
    |> generateDefinitionFromPos (Pos.fromZ 2 12)
    |> assertSrcAreEqual """namespace System

type Tuple<'T1, 'T2> =
    new : 'T1 * 'T2 -> Tuple<'T1, 'T2>
    member Equals : obj -> bool
    member GetHashCode : unit -> int
    member ToString : unit -> string
    member Item1 : 'T1
    member Item2 : 'T2
"""

(*
namespace System
{
    [Serializable]
    public class Tuple<T1, T2> : IStructuralEquatable, IStructuralComparable, IComparable, ITuple
    {
        // Summary:
        //     Initializes a new instance of the System.Tuple<T1,T2> class.
        //
        // Parameters:
        //   item1:
        //     The value of the tuple's first component.
        //
        //   item2:
        //     The value of the tuple's second component.
        public Tuple(T1 item1, T2 item2);

        // Summary:
        //     Gets the value of the current System.Tuple<T1,T2> object's first component.
        //
        // Returns:
        //     The value of the current System.Tuple<T1,T2> object's first component.
        public T1 Item1 { get; }
        //
        // Summary:
        //     Gets the value of the current System.Tuple<T1,T2> object's second component.
        //
        // Returns:
        //     The value of the current System.Tuple<T1,T2> object's second component.
        public T2 Item2 { get; }

        // Summary:
        //     Returns a value that indicates whether the current System.Tuple<T1,T2> object
        //     is equal to a specified object.
        //
        // Parameters:
        //   obj:
        //     The object to compare with this instance.
        //
        // Returns:
        //     true if the current instance is equal to the specified object; otherwise,
        //     false.
        public override bool Equals(object obj);
        //
        // Summary:
        //     Returns the hash code for the current System.Tuple<T1,T2> object.
        //
        // Returns:
        //     A 32-bit signed integer hash code.
        public override int GetHashCode();
        //
        // Summary:
        //     Returns a string that represents the value of this System.Tuple<T1,T2> instance.
        //
        // Returns:
        //     The string representation of this System.Tuple<T1,T2> object.
        public override string ToString();
    }
}
*)