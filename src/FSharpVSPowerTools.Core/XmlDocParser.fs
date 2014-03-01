namespace FSharpVSPowerTools.Core

type XmlDocable =
    | XmlDocable of (*line:*) int * (*indent:*) int * (*paramNames:*) string list

module internal XmlDocParsing =

    open System
    open System.Text
    open System.IO
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler 
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
        
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // The code below here is the essence of the extension.
    /////////////////////////////////////////////////////////////////////////////////////////////////////////

    let (|ConstructorPats|) = function
        | Pats ps -> ps
        | NamePatPairs(xs, _) -> List.map snd xs

    let rec digNamesFrom pat =
        match pat with
        | SynPat.Const _ -> []
        | SynPat.Wild _ -> []
        | SynPat.Named(_innerPat,id,_isTheThisVar,_access,_range) -> [id.idText]
        | SynPat.Typed(pat,_type,_range) -> digNamesFrom pat
        | SynPat.Attrib(pat,_attrs,_range) -> digNamesFrom pat
        | SynPat.Or(_pat1,_pat2,_range) -> [] // no one uses ors in fun decls
        | SynPat.Ands(_pats,_range) -> [] // no one uses ands in fun decls
        | SynPat.LongIdent(_lid,_idOpt,_typDeclsOpt,ConstructorPats pats,_access,_range) -> pats |> List.collect digNamesFrom 
        | SynPat.Tuple(pats,_range) -> pats |> List.collect digNamesFrom 
        | SynPat.Paren(pat,_range) -> digNamesFrom pat
        | SynPat.ArrayOrList(_,_pats,_range) -> [] // no one uses this in fun decls
        | SynPat.Record _ -> [] // no one uses this in fun decls, and Brian hates records
        | SynPat.Null _ -> []
        | SynPat.OptionalVal(id,_range) -> [id.idText]
        | SynPat.IsInst _ -> []
        | SynPat.QuoteExpr _ -> []
        | SynPat.DeprecatedCharRange _ -> []
        | SynPat.InstanceMember _ -> []
        | _ -> failwith "error"

    let GetXmlDocablesImpl(sourceCodeLinesOfTheFile:string[], sourceCodeOfTheFile, filename, checker : InteractiveChecker) =
        let indentOf (lineNum:int) =
            let mutable i = 0
            let line = sourceCodeLinesOfTheFile.[lineNum-1] // -1 because lineNum reported by xmldocs are 1-based, but array is 0-based
            while i < line.Length && line.Chars(i) = ' ' do
                i <- i + 1
            i

        let isEmptyXmlDoc (preXmlDoc: PreXmlDoc) = 
            match preXmlDoc.ToXmlDoc() with 
            | XmlDoc [||] -> true
            | XmlDoc [|x|] when x.Trim() = "" -> true
            | _ -> false

        let rec GetXmlDocablesSynModuleDecl decl =
            match decl with
            | SynModuleDecl.ModuleAbbrev(_ident, _longIdent, _range) -> []
            | SynModuleDecl.NestedModule(_synComponentInfo, synModuleDecls, _, _range) -> (synModuleDecls |> List.collect GetXmlDocablesSynModuleDecl)
            | SynModuleDecl.Let(_, synBindingList, range) -> 
                let anyXmlDoc = 
                    synBindingList |> List.exists (fun (SynBinding.Binding(_, _, _, _, _, preXmlDoc, _, _, _, _, _, _)) -> 
                        not <| isEmptyXmlDoc preXmlDoc)
                if anyXmlDoc then [] else
                let synAttributes = 
                    synBindingList |> List.collect (fun (SynBinding.Binding(_, _, _, _, a, _, _, _, _, _, _, _)) -> a)
                let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) range
                let line = fullRange.StartLine 
                let indent = indentOf line
                [for SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, _synAttributes, _preXmlDoc, synValData, synPat, _synBindingReturnInfoOption, _synExpr, _range, _sequencePointInfoForBinding) in synBindingList do
                    match synValData with
                    | SynValData(_memberFlagsOpt, SynValInfo(args, _ret), _idOpt) when args.Length > 0 ->
                        let paramNames = digNamesFrom synPat 
                        yield! paramNames
                    | _ -> ()]
                |> fun paramNames -> [XmlDocable(line,indent,paramNames)]
            | SynModuleDecl.DoExpr(_sequencePointInfoForBinding, _synExpr, _range) -> []
            | SynModuleDecl.Types(synTypeDefnList, _range) -> (synTypeDefnList |> List.collect GetXmlDocablesSynTypeDefn)
            | SynModuleDecl.Exception(_synExceptionDefn, _range) -> []
            | SynModuleDecl.Open(_longIdent, _range) -> []
            | SynModuleDecl.Attributes(_synAttributes, _range) -> []
            | SynModuleDecl.HashDirective(_parsedHashDirective, _range) -> []
            | SynModuleDecl.NamespaceFragment(synModuleOrNamespace) -> GetXmlDocablesSynModuleOrNamespace synModuleOrNamespace

        and GetXmlDocablesSynModuleOrNamespace (SynModuleOrNamespace(_longIdent, _isModule, synModuleDecls, _preXmlDoc, _synAttributes, _synAccessOpt, _range)) =
            (synModuleDecls |> List.collect GetXmlDocablesSynModuleDecl)

        and GetXmlDocablesSynTypeDefn (SynTypeDefn.TypeDefn(ComponentInfo(synAttributes, _synTyparDeclList, _synTypeConstraintList, _longIdent, preXmlDoc, _bool, _synAccessOpt, compRange), synTypeDefnRepr, synMemberDefns, tRange)) =
            let stuff = 
                match synTypeDefnRepr with
                | ObjectModel(_synTypeDefnKind, synMemberDefns, _oRange) -> (synMemberDefns |> List.collect GetXmlDocablesSynMemberDefn)
                | Simple(_synTypeDefnSimpleRepr, _range) -> []
            let docForTypeDefn = 
                if isEmptyXmlDoc preXmlDoc then
                    let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) (unionRanges compRange tRange)
                    let line = fullRange.StartLine 
                    let indent = indentOf line
                    [XmlDocable(line,indent,[])]
                else []
            docForTypeDefn @ stuff @ (synMemberDefns |> List.collect GetXmlDocablesSynMemberDefn)

        and GetXmlDocablesSynMemberDefn m =
            match m with
            | SynMemberDefn.Open(_longIdent, _range) -> []
            | SynMemberDefn.Member(SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, synAttributes, preXmlDoc, _synValData, synPat, _synBindingReturnInfoOption, _synExpr, _range, _sequencePointInfoForBinding), memRange) -> 
                if isEmptyXmlDoc preXmlDoc then
                    let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) memRange
                    let line = fullRange.StartLine 
                    let indent = indentOf line
                    let paramNames = digNamesFrom synPat 
                    [XmlDocable(line,indent,paramNames)]
                else []
            | SynMemberDefn.ImplicitCtor(_synAccessOption, _synAttributes, _synSimplePatList, _identOption, _range) -> []
            | SynMemberDefn.ImplicitInherit(_synType, _synExpr, _identOption, _range) -> []
            | SynMemberDefn.LetBindings(_synBindingList, _, _, _range) -> []
            | SynMemberDefn.AbstractSlot(ValSpfn(synAttributes, _ident, _synValTyparDecls, _synType, SynValInfo(args, _ret), _, _, preXmlDoc, _synAccessOpt, _synExprOpt, _range), _memberFlags, range) -> 
                if isEmptyXmlDoc preXmlDoc then
                    let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) range
                    let line = fullRange.StartLine 
                    let indent = indentOf line
                    let paramNames = args |> List.collect (fun az -> az |> List.choose (fun (SynArgInfo(_synAttributes, _, idOpt)) -> match idOpt with | Some id -> Some(id.idText) | _ -> None))
                    [XmlDocable(line,indent,paramNames)]
                else []
            | SynMemberDefn.Interface(_synType, synMemberDefnsOption, _range) -> 
                match synMemberDefnsOption with 
                | None -> [] 
                | Some(x) -> x |> List.collect GetXmlDocablesSynMemberDefn
            | SynMemberDefn.Inherit(_synType, _identOption, _range) -> []
            | SynMemberDefn.ValField(_synField, _range) -> []
            | SynMemberDefn.NestedType(synTypeDefn, _synAccessOption, _range) -> GetXmlDocablesSynTypeDefn synTypeDefn
            | _ -> failwith "error"

        and GetXmlDocablesInput input =
            match input with
            | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,l,_))-> 
                l |> List.collect GetXmlDocablesSynModuleOrNamespace
            | ParsedInput.SigFile _sigFile -> []

        // Get compiler options for the 'project' implied by a single script file
        let projOptions = checker.GetProjectOptionsFromScript(filename, sourceCodeOfTheFile)
        let input = checker.ParseFileInProject (filename, sourceCodeOfTheFile, projOptions)
    
        match input.ParseTree with
        | Some input -> GetXmlDocablesInput input
        | None ->
            // Should not fail here, just in case 
            []

module XmlDocComment =
    let private ws (s: string, pos) = 
        let res = s.TrimStart()
        Some (res, pos + (s.Length - res.Length))

    let private str (prefix: string) (s: string, pos) =
        match s.StartsWith prefix with
        | true -> 
            let res = s.Substring prefix.Length
            Some (res, pos + (s.Length - res.Length))
        | _ -> None

    let private eol (s: string, pos) = 
        match s with
        | "" -> Some ("", pos)
        | _ -> None

    let inline private (>=>) f g = fun x -> f x |> Option.bind g

    // if it's a blank XML comment with trailing "<", returns Some (index of the "<"), otherwise returns None
    let isBlank (s: string) =
        let parser = ws >=> str "///" >=> ws >=> str "<" >=> eol
        let res = parser (s, 0) |> Option.map snd |> Option.map (fun x -> x - 1)
        res

open Microsoft.FSharp.Compiler.SourceCodeServices
open XmlDocParsing
open System

type XmlDocParser private () =
    let checker = lazy (InteractiveChecker.Create())

    member internal x.Checker = checker.Value

    static member internal Instance = XmlDocParser()

    // Main API.
    static member GetXmlDocables(sourceCodeOfTheFile : string, filename, ?checker) =
        let sourceCodeLinesOfTheFile = sourceCodeOfTheFile.Split [|'\n'|]
        let checker = defaultArg checker XmlDocParser.Instance.Checker
        GetXmlDocablesImpl(sourceCodeLinesOfTheFile, sourceCodeOfTheFile, filename, checker)



