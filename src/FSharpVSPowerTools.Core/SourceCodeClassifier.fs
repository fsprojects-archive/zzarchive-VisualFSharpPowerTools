module FSharpVSPowerTools.Core.SourceCodeClassifier

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

type TypeLocation = 
    { TypeName: string
      Range: Range.range }

    static member FromLongIdentWithDots ((LongIdentWithDots(ident, _) as i)) =
        TypeLocation.FromIdentAndRange ident i.Range
        
    static member FromIdentAndRange (ident: LongIdent) range =
        let name, range =
            ident
            |> List.rev
            |> (function
                | h :: _ -> h.idText, h.idRange
                | [] -> "", new Range.range())

        { TypeName = name; Range = range }


/// Get untyped tree for a specified input
let getUntypedTree (input: string) = 
    let file = @"/file.fs"
    let checker = InteractiveChecker.Create()
    // Get compiler options for the 'project' implied by a single script file
    let projOptions = checker.GetProjectOptionsFromScript(file, input)
    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults =
        checker.ParseFileInProject(file, input, projOptions)
        |> Async.RunSynchronously

    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"

let rec visitType t = 
    let traceLine x =
        Printf.ksprintf (fun s -> printfn "  [visitType] %s" s) x

    match t with
    | SynType.LongIdent(longIdent) -> 
        [ TypeLocation.FromLongIdentWithDots longIdent ]
    | SynType.App(higherOrderType, _, typeList, _, _, _, _) ->
        traceLine "app"
        [
            yield! visitType higherOrderType
            for t in typeList do
                yield! visitType t
        ]
    | SynType.LongIdentApp(synType, longIdent, _, synTypeList, _, _, _) ->
        traceLine "LongIdentApp"
        [
            yield! visitType synType
            yield TypeLocation.FromLongIdentWithDots longIdent

            for synType in synTypeList do
                yield! visitType synType
        ]
    | SynType.Tuple(boolTypePairs, _) ->
        traceLine "tuple"
        [
            for _, synType in boolTypePairs do
                yield! visitType synType
        ]
    | SynType.Array(_, eltType, _)
    | SynType.HashConstraint(eltType, _) -> visitType eltType
    | SynType.Fun(inputType, outputType, _) ->
        traceLine "fun"
        [
            yield! visitType inputType
            yield! visitType outputType
        ]
    | t -> 
        traceLine "other type: %A" t
        []

let rec visitConstructorArgs =
    function
    | SynConstructorArgs.Pats synPats ->
        synPats
        |> List.collect visitPattern
    | cArgs -> printfn "  .. [visitConstructorArgs] other constructor args: %A" cArgs; []

/// Walk over a pattern - this is for example used in 
/// let <pat> = <expr> or in the 'match' expression
and visitPattern = 
    function 
    | SynPat.Wild(_) -> printfn "  .. underscore pattern"; []
    | SynPat.Named(pat, _, _, _, _) -> 
        printfn " .. named"
        visitPattern pat
    | SynPat.LongIdent(_, _, _, cArgs, _, _) -> 
        printfn "  .. constructor args"
        visitConstructorArgs cArgs
    | SynPat.Typed(_, synType, _) ->
        printfn " .. type"
        visitType synType
    | SynPat.Paren(pat, _) -> visitPattern pat
    | SynPat.Tuple(pats, _) ->
        pats
        |> List.collect visitPattern
    | pat -> printfn "  .. other pattern: %A" pat; []

let rec visitSimplePat =
    function
    | SynSimplePat.Typed(simplePat, synType, _) ->
        [ yield! visitSimplePat simplePat
          yield! visitType synType ]
    | p -> printfn " - not supported simple pat: %A" p; []

let rec visitSimplePats =
    function
    | SynSimplePats.SimplePats(patList, _) ->
        patList
        |> List.collect visitSimplePat
    | SynSimplePats.Typed(simplePats, synType, _) ->
        [
            yield! visitSimplePats simplePats
            yield! visitType synType
        ]

let rec visitAttributes (attrs: SynAttributes) =
    [
        for attr in attrs do
            let (LongIdentWithDots(ident, _)) = attr.TypeName
            yield TypeLocation.FromIdentAndRange ident attr.TypeName.Range
            yield! visitExpression attr.ArgExpr
    ]

and visitBindings (bindings: SynBinding list) =
    [
        for binding in bindings do
            let (Binding(access, _, _, _, attrs, _, _, pat, _, body, _, _)) = binding
            yield! visitAttributes attrs
            yield! visitPattern pat
            yield! visitExpression body
    ]

/// Walk over an expression - if expression contains two or three 
/// sub-expressions (two if the 'else' branch is missing), let expression
/// contains pattern and two sub-expressions
and visitExpression = 
    function 
    | SynExpr.Paren(expr, _, _, _)
    | SynExpr.Do(expr, _)
    | SynExpr.Assert(expr, _)
    | SynExpr.Lazy(expr, _)
    | SynExpr.InferredDowncast(expr, _)
    | SynExpr.InferredUpcast(expr, _)
        -> visitExpression expr
    | SynExpr.App(_, _, expr1, expr2, _) ->
        [
            yield! visitExpression expr1
            yield! visitExpression expr2
        ]
    | SynExpr.Typed(expr, synType, _)
    | SynExpr.New(_, synType, expr, _)
    | SynExpr.TypeTest(expr, synType, _)
    | SynExpr.Upcast(expr, synType, _)
    | SynExpr.Downcast(expr, synType, _)
        ->
        [
            yield! visitExpression expr
            yield! visitType synType
        ]
    | SynExpr.Tuple(exprList, _, _)
    | SynExpr.ArrayOrList(_, exprList, _) ->
        exprList
        |> List.collect visitExpression
    | SynExpr.Lambda(_, _, simplePats, expr, _) ->
        [
            yield! visitSimplePats simplePats
            yield! visitExpression expr
        ]
    | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) -> 
        // Visit all sub-expressions
        printfn "Conditional:"
        [
            visitExpression cond
            visitExpression trueBranch
            falseBranchOpt |> Option.fold (fun _ e -> visitExpression e) []
        ]
        |> List.concat
    | SynExpr.LetOrUse(_, _, bindings, body, _) -> 
        // for 'let <binding> = .. and .. = .. in <body>'
        [
            // Visit bindings (there may be multiple 
            yield! visitBindings bindings

            // Visit the body expression
            yield! visitExpression body
        ]
    | expr -> printfn " - not supported expression: %A" expr; []

let visitField (SynField.Field(_, _, _, fieldType, _, _, _, range)) =
    visitType fieldType

let visitUnionCase (SynUnionCase.UnionCase(_, _, caseType, _, _, _)) =
    match caseType with
    | UnionCaseFields(synFields) -> synFields |> List.collect visitField
    | t -> printfn " - not supported union case type: %A" t; []

let rec visitMemberDefn =
    function
    | SynMemberDefn.ImplicitCtor(_, _, patList, _, _) ->
        patList
        |> List.collect visitSimplePat
    // TODO: implicit inherit
    | SynMemberDefn.LetBindings(bindings, _, _, _) ->
        visitBindings bindings
    | SynMemberDefn.Inherit(synType, _, _) -> visitType synType
    | SynMemberDefn.Interface(synType, defns0, _) ->
        [
            yield! visitType synType

            match defns0 with
            | Some defns ->
                for defn in defns do
                    yield! visitMemberDefn defn
            | _ -> ()
        ]
    | SynMemberDefn.Member(binding, _) -> visitBindings [binding]
    | def -> printfn " - not supported member definition: %A" def; []

let visitTypeRepr =
    function
    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
        match simpleRepr with
        | SynTypeDefnSimpleRepr.Union(_, cases, _) ->
            printfn "  .. union cases"
            cases
            |> List.collect visitUnionCase
        | SynTypeDefnSimpleRepr.Record(_, fields, _) ->
            printfn "  .. record fields"
            fields
            |> List.collect visitField
        | SynTypeDefnSimpleRepr.TypeAbbrev(detail, synType, _) ->
            printfn "  .. type abbrev"
            visitType synType
        | repr -> printfn " - not supported representation: %A" repr; []
    | SynTypeDefnRepr.ObjectModel(_, defns, _) ->
        [
            for def in defns do
                yield! visitMemberDefn def
        ]
    | repr -> printfn " - doesn't support object model yet"; []

let visitComponentInfo (SynComponentInfo.ComponentInfo(attrs, _, _, ident, _, _, _, range)) (isTypeDefinition: bool) =
    [
        yield! visitAttributes attrs
        
        if isTypeDefinition then
            yield TypeLocation.FromIdentAndRange ident range
    ]

let rec visitDeclarations decls = 
    [ for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            // Let binding as a declaration is similar to let binding
            // as an expression (in visitExpression), but has no body
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
                yield! visitAttributes attrs
                yield! visitPattern pat
                yield! visitExpression body
        | SynModuleDecl.Types(typeDefnList, range) ->
            for t in typeDefnList do
                let (SynTypeDefn.TypeDefn(compInfo, repr, defns, _)) = t
                yield! visitComponentInfo compInfo true
                yield! visitTypeRepr repr
                for defn in defns do
                    yield! visitMemberDefn defn
        | SynModuleDecl.NestedModule(compInfo, decls, _, _) ->
            yield! visitComponentInfo compInfo false
            yield! visitDeclarations decls
        | _ -> printfn " - not supported declaration: %A" declaration
    ]

/// Walk over all module or namespace declarations 
/// (basically 'module Foo =' or 'namespace Foo.Bar')
/// Note that there is one implicitly, even if the file
/// does not explicitly define it..
let visitModulesAndNamespaces modulesOrNss = 
    [ for moduleOrNs in modulesOrNss do
        let (SynModuleOrNamespace(lid, isMod, decls, xml, attrs, _, m)) = moduleOrNs
        printfn "Namespace or module: %A" lid
        yield! visitAttributes attrs
        yield! visitDeclarations decls
    ]
