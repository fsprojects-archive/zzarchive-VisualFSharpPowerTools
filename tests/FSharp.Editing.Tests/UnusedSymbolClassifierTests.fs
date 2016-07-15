module FSharp.Editing.Tests.UnusedSymbolClassifierTests

open System.IO
open NUnit.Framework
open FSharp.Editing
open FSharp.Editing.Features

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()

type private Cat = Category

let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let (=>) source (expected: (int * ((Cat * int * int) list)) list) = 
    let opts = opts source
    
    let sourceLines = String.getLines source

    let lexer = 
        { new LexerBase() with
            member __.GetSymbolFromTokensAtLocation (_tokens, line, col) =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr SymbolLookupKind.ByRightColumn LanguageServiceTestHelper.args Lexer.queryLexState
            member __.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source LanguageServiceTestHelper.args line lineStr Lexer.queryLexState 
            member __.LineCount = sourceLines.Length }

    let symbolsUses = 
        async {
            let! symbolUses = 
                languageService.GetAllUsesOfAllSymbolsInFile (opts, fileName, source, AllowStaleResults.No, true)
            return! languageService.GetUnusedDeclarations (symbolUses, opts, (fun _ -> async { return Some [opts] }))
        } |> Async.RunSynchronously

    let checkResults = 
        languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No) |> Async.RunSynchronously

    let actualCategories =
        let entities =
            languageService.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source)
            |> Async.RunSynchronously

        let getLineStr line = sourceLines.[line - 1]

        let qualifyOpenDeclarations line endColumn idents = async {
            let! tooltip = languageService.GetIdentTooltip (line, endColumn, getLineStr line, Array.toList idents, opts, fileName, source)
            return 
                match tooltip with
                | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                | None -> []
        }

        let openDeclarations = 
            OpenDeclarationGetter.getOpenDeclarations checkResults.ParseTree entities qualifyOpenDeclarations
            |> Async.RunSynchronously

        let allEntities =
            entities
            |> Option.map (fun entities -> 
                entities 
                |> Seq.groupBy (fun e -> e.FullName)
                |> Seq.map (fun (key, es) -> key, es |> Seq.map (fun e -> e.CleanedIdents) |> Seq.toList)
                |> Dict.ofSeq)

        SourceCodeClassifier.getCategorizedSpansForUnusedSymbols (symbolsUses, checkResults, lexer, openDeclarations, allEntities)
        |> Seq.groupBy (fun span -> span.WordSpan.Line)

    let actual =
        expected
        |> List.map (fun (line, _) ->
            match actualCategories |> Seq.tryFind (fun (actualLine, _) -> actualLine = line) with
            | Some (_, spans) -> 
                line,
                spans
                |> Seq.choose (fun span ->
                    match span.Category with 
                    | Cat.Other -> None
                    | _ -> Some (span.Category, span.WordSpan.StartCol, span.WordSpan.EndCol))
                |> Seq.sortBy (fun (_, startCol, _) -> startCol)
                |> Seq.toList
            | None -> line, [])
        |> List.sortBy (fun (line, _) -> line)
    
    let expected = 
        expected 
        |> List.map (fun (line, spans) -> line, spans |> List.sortBy (fun (_, startCol, _) -> startCol))
        |> List.sortBy (fun (line, _) -> line)
    
    try actual |> Collection.assertEquiv expected
    with _ -> 
        debug "AST: %A" checkResults.ParseTree
        for x in actual do
            debug "Actual: %A" x
        reraise()

[<Test>]
let ``used let bindings in computation expression should not be marked as unused``() = 
    """
seq {
    let func x = x
    yield func 1
} |> ignore
"""
    => [ 2, []
         3, []
         4, []]

[<Test>]
let ``non public module``() =
    """
module private PrivateModule =
    let x = ()
"""
    => [ 2, []]

[<Test>]
let ``unused non public module function and value``() =
    """
module private PrivateModule =
    let func _ = ()
    let value = ()
"""
    => [ 3, [ Cat.Unused, 8, 12 ]  
         4, [ Cat.Unused, 8, 13 ]]

[<Test>]
let ``unused default constructor of non public class``() =
    """
type private PrivateClass() = class end
"""
    => [ 2, []]

[<Test>]
let ``used default constructor of non public class``() =
    """
type private PrivateClass() = class end
let _ = PrivateClass()
"""
    => [ 2, []]

[<Test>]
let ``unused non public class let binding``() =
    """
type PublicClass() =
    let letValue = 1
    let letFunc _ = ()
    member __.P = ()
"""
    => [ 3, [ Cat.Unused, 8, 16 ] 
         4, [ Cat.Unused, 8, 15 ]]

[<Test>]
let ``unused non public class member``() =
    """
type PublicClass() =
    member private __.Prop = ()
    member private __.Method _ = ()
"""
    => [ 3, [ Cat.Unused, 22, 26 ] 
         4, [ Cat.Unused, 22, 28 ]]

[<Test>]
let ``unused self binding``() =
    """
type PublicClass() =
    member this.PublicMethod _ = ()
""" 
    => [ 3, [ Cat.Unused, 11, 15 ]]

[<Test>]
let ``used self binding``() =
    """
type PublicClass() =
    member this.Method2 _ = this
"""
    => [ 3, []]

[<Test>]
let ``unused function / member argument``() =
    """
type PublicClass() =
    member __.Method1 (arg1: int, arg2) = arg2
let func arg1 arg2 = arg2
"""
    => [ 3, [ Cat.Unused, 23, 27 ]
         4, [ Cat.Unused, 9, 13 ]]

[<Test>]
let ``unused function / member local binding``() =
    """
type PublicClass() =
    member __.Method() =
        let local = 1
        ()
let func x =
    let local = 1
    x
"""
    => [ 4, [ Cat.Unused, 12, 17 ]
         7, [ Cat.Unused, 8, 13 ]]

[<Test>]
let ``unused DU field names are not marked as unused even though they are not used anywhere``() =
    """
type DU = Case of field1: int * field2: string
let _ = Case (1, "")
"""
    => [ 2, []]

[<Test>]
let ``unused open declaration in top level module``() =
    """
module TopModule
open System
open System.IO
let _ = DateTime.Now
"""
    => [ 3, []
         4, [ Cat.Unused, 5, 14 ]]
         
[<Test>]
let ``unused open declaration in namespace``() =
    """
namespace TopNamespace
open System
open System.IO
module Nested =
    let _ = DateTime.Now
"""
    => [ 3, []
         4, [ Cat.Unused, 5, 14 ]]
         
[<Test>]
let ``unused open declaration in nested module``() =
    """
namespace TopNamespace
module Nested =
    open System
    open System.IO
    let _ = DateTime.Now
"""
    => [ 4, []
         5, [ Cat.Unused, 9, 18 ]]

[<Test>] 
let ``unused open declaration due to partially qualified symbol``() =
    """
module TopModule
open System
open System.IO
let _ = IO.File.Create ""
"""
    => [ 3, []
         4, [ Cat.Unused, 5, 14 ]]

[<Test>]
let ``unused parent open declaration due to partially qualified symbol``() =
    """
module TopModule
open System
open System.IO
let _ = File.Create ""
"""
    => [ 3, [ Cat.Unused, 5, 11 ]
         4, []]

[<Test>]
let ``open statement duplication in parent module is unused``() =
    """
module TopModule
open System.IO
module Nested =
    open System.IO
    let _ = File.Create ""
"""
    => [ 3, [ Cat.Unused, 5, 14 ]
         5, []]

[<Test>]
let ``open statement duplication in parent module is not unused while it's actually used in its scope``() =
    """
module TopModule
open System.IO
module Nested =
    open System.IO
    let _ = File.Create ""
let _ = File.Create ""
"""
    => [ 3, []
         5, []]

[<Test>]
let ``multiple open declaration in the same line``() =
    """
open System.IO; let _ = File.Create "";; open System.IO
"""
    => [ 2, [ Cat.Unused, 46, 55 ]]

[<Test>]
let ``open a nested module inside another one is not unused``() =
    """
module Top
module M1 =
    let x = ()
module M2 =
    open M1
    let y = x
"""
    => [ 6, []]

[<Test>]
let ``open a nested module inside another one is not unused, complex hierarchy``() =
    """
module Top =
    module M1 =
        module M11 =
            let x = ()
    module M2 =
        module M22 =
            open M1.M11
            let y = x
"""
    => [ 8, []]

[<Test>]
let ``open a nested module inside another one is not unused, even more complex hierarchy``() =
    """
module Top =
    module M1 =
        module M11 =
            module M111 =
                module M1111 =
                    let x = ()
    module M2 =
        module M22 =
            open M1.M11.M111.M1111
                let y = x
"""
    => [ 10, []]

[<Test>]
let ``last of several equivalent open declarations is market as used, the rest of them are marked as unused``() =
    """
module NormalModule =
    [<AutoOpen>]
    module AutoOpenModule1 =
        module NestedNormalModule =
            [<AutoOpen>]
            module AutoOpenModule2 =
                [<AutoOpen>]
                module AutoOpenModule3 =
                    type Class() = class end

open NormalModule.AutoOpenModule1.NestedNormalModule.AutoOpenModule2
open NormalModule.AutoOpenModule1.NestedNormalModule
let _ = Class()
"""
    => [ 12, [ Cat.Unused, 5, 68 ]
         13, []]
    
[<Test>]
let ``open declaration is not marked as unused if there is a shortened attribute symbol from it``() =
    """
open System
[<Serializable>]
type Class() = class end
"""
    => [ 2, []]
    
[<Test>]
let ``open declaration is not marked as unused if an extension property is used``() =
    """
module Module =
    type System.String with
        member __.ExtensionProperty = ()
open Module
let _ = "a long string".ExtensionProperty
"""
    => [ 5, []]

[<Test>]
let ``open declaration is marked as unused if an extension property is not used``() =
    """
module Module =
    type System.String with
        member __.ExtensionProperty = ()
open Module
let _ = "a long string".Trim()
"""
    => [ 5, [ Cat.Unused, 5, 11 ]]

[<Test>]
let ``open declaration is not marked as unused if an extension method is used``() =
    """
type Class() = class end
module Module =
    type Class with
        member __.ExtensionMethod() = ()
open Module
let x = Class()
let _ = x.ExtensionMethod()
"""
    => [ 6, []]

[<Test>]
let ``open declaration is marked as unused if an extension method is not used``() =
    """
type Class() = class end
module Module =
    type Class with
        member __.ExtensionMethod() = ()
open Module
let x = Class()
"""
    => [ 6, [ Cat.Unused, 5, 11 ]]

[<Test>]
let ``open declaration is not marked as unused if one of its types is used in a constructor signature``() =
    """
module M =
    type Class() = class end
open M
type Site (x: Class -> unit) = class end
"""
    => [ 4, []]   

[<Test>]
let ``open declaration is marked as unused if nothing from it is used``() =
    """
module M =
    type Class() = class end
open M
type Site (x: int -> unit) = class end
"""
    => [ 4, [ Cat.Unused, 5, 6 ] ]

[<Test>]
let ``static extension method applied to a type results that both namespaces /where the type is declared and where the extension is declared/ is not marked as unused``() =
    """
module Extensions =
    type System.DateTime with
        static member ExtensionMethod() = ()
open System
open Extensions
let _ = DateTime.ExtensionMethod
"""
    => [ 5, []; 6, []]
    
[<Test>]
let ``static extension property applied to a type results that both namespaces /where the type is declared and where the extension is declared/ is not marked as unused``() =
    """
module Extensions =
    type System.DateTime with
        static member ExtensionProperty = ()
open System
open Extensions
let _ = DateTime.ExtensionProperty
"""
    => [ 5, []; 6, []]

[<Test>]
let ``accessing property on a variable should not force the namespace in which the type is declared to be marked as used``() =
    """
let dt = System.DateTime.Now
module M =
    open System
    let _ = dt.Hour
"""
    => [4, [ Cat.Unused, 9, 15 ]]

[<Test>]
let ``either of two open declarations are not marked as unused if symbols from both of them are used``() =
    """
module M1 =
    module M2 =
        let func1 _ = ()
        module M3 =
            let func2 _ = ()
open M1.M2.M3
open M1.M2
let _ = func1()
let _ = func2()
"""
    => [ 7, []; 8, []]
        
[<Test>]
let ``open module with ModuleSuffix attribute value applied is not marked as unused if a symbol declared in it is used``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module M =
    let func _ = ()
open M
let _ = func()
"""
    => [ 5, []]

[<Test>]
let ``open module all of which symbols are used by qualifier is marked as unused``() =
    """
module M =
    let func _ = ()
open M
let _ = M.func 1
"""
    => [4, [Cat.Unused, 5, 6 ]]

[<Test>]
let ``open module is not marked as unused if a symbol defined in it is used in OCaml-style type annotation``() =
    """
module M =
    type Class() = class end
open M
let func (arg: Class list) = ()
"""
    => [ 4, []]

[<Test>]
let ``auto open module``() =
    """
module Top =
    [<AutoOpen>]
    module M =
        let func _ = ()
open Top
let _ = func()
"""
    => [ 6, []]

[<Test>]
let ``auto open module in the middle of hierarchy``() =
    """
namespace Ns
module M1 =
    [<AutoOpen>]
    module MA1 = 
        let func _ = ()
open M1
module M2 =
    let _ = func()
"""
    => [ 7, []]

[<Test>]
let ``open declaration is not marked as unused if a delegate defined in it is used``() =
    """
open System
let _ = Func<int, int>(fun _ -> 1)
"""
    => [ 2, []]

[<Test>]
let ``open declaration is not marked as unused if a unit of measure defined in it is used``() =
    """
module M = 
    type [<Measure>] m
module N =
    open M
    let _ = 1<m>
"""
    => [ 5, []]

[<Test>]
let ``open declaration is not marked as unused if an attribute defined in it is applied on an interface member argument``() =
    """
open System.Runtime.InteropServices
type T = abstract M: [<DefaultParameterValue(null)>] ?x: int -> unit
"""
    => [ 2, []]

[<Test>]
let ``relative module open declaration``() =
    """
module Top =
    module Nested = 
        let x = 1
open Top
open Nested
let _ = x
"""
    => [ 5, []; 6, []]

[<Test>]
let ``open declaration is used if a symbol defined in it is used in a module top-level do expression``() =
    """
module Top
open System.IO
File.ReadAllLines ""
|> ignore
"""
    => [ 3, []]

[<Test>]
let ``redundant opening a module with ModuleSuffix attribute value is marks as unused``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InternalModuleWithSuffix =
    let func1 _ = ()
module M =
    open InternalModuleWithSuffix
    let _ = InternalModuleWithSuffix.func1()
"""
    => [ 6, [Cat.Unused, 9, 33 ]]
    
[<Test>]
let ``redundant opening a module is marks as unused``() =
    """
module InternalModuleWithSuffix =
    let func1 _ = ()
module M =
    open InternalModuleWithSuffix
    let _ = InternalModuleWithSuffix.func1()
"""
    => [ 5, [Cat.Unused, 9, 33 ]]

[<Test>]
let ``usage of an unqualified union case doesn't make an opening module where it's defined to be marked as unused``() =
    """
module M =
    type DU = Case1
open M
let _ = Case1
"""
    => [ 4, []]

[<Test>]
let ``usage of qualified union case doesn't make an opening module where it's defined to be marked as unused``() =
    """
module M =
    type DU = Case1
open M
let _ = DU.Case1
"""
    => [ 4, []]

[<Test>]
let ``type with different DisplayName``() =
    """
open Microsoft.FSharp.Quotations
let _ = Expr.Coerce (<@@ 1 @@>, typeof<int>)
"""
    => [ 2, []]

[<Test>]
let ``auto open module with ModuleSuffix attribute value``() =
    """
module Top =
    [<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Module =
        let func _ = ()
open Top
module Module1 =
    let _ = func()
"""
    => [ 6, []]

[<Test>]
let ``a type which has more than one DisplayName causes the namespace it's defined in to be not marked as unused``() =
    """
open System
let _ = IntPtr.Zero
""" 
    => [2, []]

[<Test>]
let ``usage of an operator makes the module it's defined in to be not marked as unused``() =
    """
module M =
    let (++|) x y = ()
open M
let _ = 1 ++| 2
"""
    => [ 4, []]

[<Test>]
let ``usage of an operator makes the module /with Module suffix/ it's defined in to be not marked as unused``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module M =
    let (++|) x y = ()
open M
let _ = 1 ++| 2
"""
    => [ 5, []]

[<Test>]
let ``type used in pattern matching with "as" keyword causes the module in which the type is defined to be not marked as unused``() =
    """
module M = 
    type Class() = class end
open M
let _ = match obj() with 
        | :? Class as c -> ()
        | _ -> ()
"""
    => [ 4, []]

[<Test>]
let ``a function from printf family prevents Printf module from marking as unused``() =
    """
open Microsoft.FSharp.Core.Printf
open System.Text
let _ = bprintf (StringBuilder()) "%A" 1
"""
    => [ 2, []]

[<Test>]
let ``assembly level attribute prevents namespace in which it's defined to be marked as unused``() =
    """
open System
[<assembly: Version("1")>]
()
"""
    => [ 2, []]

[<Test>]
let ``open declaration is not marked as unused if a related type extension is used``() =
    """
module Module =
    open System
    type String with
        member __.Method() = ()
"""
    => [ 3, []]

[<Test>]
let ``open declaration is not marked as unused if a symbol defined in it is used in type do block``() =
    """
open System.IO.Compression

type OutliningHint() as self =
    do self.E.Add (fun (e: GZipStream) -> ()) 
    member __.E: IEvent<_> = Unchecked.defaultof<_> 
"""
    => [ 2, []]

[<Test>]
let ``should not mark open declaration with global prefix``() =
    """
module Module =
    open global.System
    let _ = String("")
"""
    => [ 3, []]

[<Test>]
let ``should mark open declaration with global prefix in double backticks``() =
    """
module Module =
    open ``global``.Namesp
    let _ = System.String("")
"""
    => [ 3, [Cat.Unused, 9, 26]]

[<Test>]
let ``record fields should be taken into account``() = 
    """
module M1 =
    type Record = { Field: int }
module M2 =
    open M1
    let x = { Field = 0 }
"""
    => [ 5, []]

[<Test>]
let ``handle type alias``() = 
    """
module TypeAlias =
    type MyInt = int
module Usage =
    open TypeAlias
    let f (x:MyInt) = x
"""
    => [ 5, []]

[<Test>]
let ``handle override members``() = 
    """
type IInterface =
    abstract Property: int

type IClass() =
    interface IInterface with
        member __.Property = 0

let f (x: IClass) = (x :> IInterface).Property
"""
    => [ 7, []]

[<Test>]
let ``active pattern cases should be taken into account``() =
    """
module M = 
    let (|Pattern|_|) _ = Some()
open M
let f (Pattern _) = ()
"""
    => [ 4, []]

[<Test>]
let ``active patterns applied as a function should be taken into account``() =
    """
module M = 
    let (|Pattern|_|) _ = Some()
open M
let _ = (|Pattern|_|) ()
"""
    => [ 4, []]

[<Test>]
let ``not used active pattern does not make the module in which it's defined to not mark as unused``() =
    """
module M = 
    let (|Pattern|_|) _ = Some()
open M
let _ = 1
"""
    => [ 4, [ Cat.Unused, 5, 6 ]]
    
[<Test>]
let ``type in type parameter constraint should be taken into account``() =
    """
open System
let f (x: 'a when 'a :> IDisposable) = ()
"""
    => [ 2, []]