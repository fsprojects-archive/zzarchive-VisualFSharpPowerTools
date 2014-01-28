module MyParsing

// This file has two main parts:
//  - first, nearly verbatim code from misc portions of the F# compiler
//  - second, the essence of the extension for getting range information out of the AST
// There's a comment about halfway down that delineates the break.

open System
open System.Text
open System.IO
open Internal.Utilities
open Internal.Utilities.Text
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.SR
open Microsoft.FSharp.Compiler.DiagnosticMessage

module SR = Microsoft.FSharp.Compiler.SR
module DM = Microsoft.FSharp.Compiler.DiagnosticMessage

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.PrettyNaming
open Internal.Utilities.FileSystem
open Internal.Utilities.Collections

//----------------------------------------------------------------------------
// Some Globals
//--------------------------------------------------------------------------

let sigSuffixes = [".mli";".fsi"]
let mlCompatSuffixes = [".mli";".ml"]
let implSuffixes = [".ml";".fs";".fsscript";".fsx"]
let resSuffixes = [".resx"]
let scriptSuffixes = [".fsscript";".fsx"]
let doNotRequireNamespaceOrModuleSuffixes = [".mli";".ml"] @ scriptSuffixes
let lightSyntaxDefaultExtensions : string list = [ ".fs";".fsscript";".fsx";".fsi" ]


let GetWarningNumber(_m,s:string) =
    try 
        Some (int32 s)
    with err -> 
        None


//----------------------------------------------------------------------------
// Scoped #nowarn pragmas

let GetScopedPragmasForHashDirective hd = 
    [ match hd with 
      | ParsedHashDirective("nowarn",numbers,m) ->
          for s in numbers do
          match GetWarningNumber(m,s) with 
            | None -> ()
            | Some n -> yield WarningOff(m,n) 
      | _ -> () ]

let GetScopedPragmasForInput input = 
    match input with 
    | SigFileInput (SigFile(_,_,pragmas,_,_)) -> pragmas
    | ImplFileInput (ImplFile(_,_,_,pragmas,_,_,_)) ->pragmas

//----------------------------------------------------------------------------
// Parsing
//--------------------------------------------------------------------------

let CanonicalizeFilename filename = 
  let basic = Path.GetFileName filename
  String.capitalize (try Filename.chopExtension basic with _ -> basic)

let IsScript filename = 
    let lower = String.lowercase filename 
    scriptSuffixes |> List.exists (Filename.checkSuffix lower)
    
// Give a unique name to the different kinds of inputs. Used to correlate signature and implementation files
//   QualFileNameOfModuleName - files with a single module declaration or an anonymous module
let QualFileNameOfModuleName m filename modname = QualifiedNameOfFile(mkSynId m (textOfLid modname + (if IsScript filename then "$fsx" else "")))
let QualFileNameOfFilename m filename = QualifiedNameOfFile(mkSynId m (CanonicalizeFilename filename + (if IsScript filename then "$fsx" else "")))

// Interactive fragments
let QualFileNameOfUniquePath (m, p: string list) = QualifiedNameOfFile(mkSynId m (String.concat "_" p))

let QualFileNameOfSpecs filename specs = 
    match specs with 
    | [ModuleOrNamespaceSig(modname,true,_,_,_,_,m)] -> QualFileNameOfModuleName m filename modname
    | _ -> QualFileNameOfFilename (rangeN filename 1) filename

let QualFileNameOfImpls filename specs = 
    match specs with 
    | [ModuleOrNamespace(modname,true,_,_,_,_,m)] -> QualFileNameOfModuleName m filename modname
    | _ -> QualFileNameOfFilename (rangeN filename 1) filename

let PrepandPathToQualFileName x (QualifiedNameOfFile(q)) = QualFileNameOfUniquePath (q.idRange,pathOfLid x@[q.idText])
let PrepandPathToImpl x (ModuleOrNamespace(p,c,d,e,f,g,h)) = ModuleOrNamespace(x@p,c,d,e,f,g,h)
let PrepandPathToSpec x (ModuleOrNamespaceSig(p,c,d,e,f,g,h)) = ModuleOrNamespaceSig(x@p,c,d,e,f,g,h)

let PrependPathToInput x inp = 
    match inp with 
    | ImplFileInput (ImplFile(b,c,q,d,hd,impls,e)) -> ImplFileInput (ImplFile(b,c,PrepandPathToQualFileName x q,d,hd,List.map (PrepandPathToImpl x) impls,e))
    | SigFileInput (SigFile(b,q,d,hd,specs)) -> SigFileInput(SigFile(b,PrepandPathToQualFileName x q,d,hd,List.map (PrepandPathToSpec x) specs))

let ComputeAnonModuleName check defaultNamespace filename m = 
    let modname = CanonicalizeFilename filename
    if check && not (modname |> String.forall (fun c -> System.Char.IsLetterOrDigit(c) || c = '_')) then
          if not (filename.EndsWith("fsx",StringComparison.OrdinalIgnoreCase) || filename.EndsWith("fsscript",StringComparison.OrdinalIgnoreCase)) then // bug://2893
              warning(Error(FSComp.SR.buildImplicitModuleIsNotLegalIdentifier(modname,(Path.GetFileName filename)),m));
    let combined = 
      match defaultNamespace with 
      | None -> modname
      | Some ns -> textOfPath [ns;modname]
    pathToSynLid m (splitNamespace combined)

let PostParseModuleImpl (_i,defaultNamespace,isLastCompiland,filename,impl) = 
    match impl with 
    | NamedTopModule(ModuleOrNamespace(lid,isModule,decls,xmlDoc,attribs,access,m)) -> 
        let lid = 
            match lid with 
            | [id] when isModule && id.idText = MangledGlobalName -> error(Error(FSComp.SR.buildInvalidModuleOrNamespaceName(),id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid
        ModuleOrNamespace(lid,isModule,decls,xmlDoc,attribs,access,m)

    | AnonTopModule (defs,m)-> 
        if not isLastCompiland && not (doNotRequireNamespaceOrModuleSuffixes |> List.exists (Filename.checkSuffix (String.lowercase filename))) then 
            errorR(Error(FSComp.SR.buildMultiFileRequiresNamespaceOrModule(),trimRangeToLine m))
        let modname = ComputeAnonModuleName (nonNil defs) defaultNamespace filename m
        ModuleOrNamespace(modname,true,defs,PreXmlDoc.Empty,[],None,m)

    | NamespaceFragment (lid,b,c,d,e,m)-> 
        let lid = 
            match lid with 
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid
        ModuleOrNamespace(lid,b,c,d,e,None,m)

let PostParseModuleSpec (_i,defaultNamespace,isLastCompiland,filename,intf) = 
    match intf with 
    | NamedTopModuleSig(ModuleOrNamespaceSig(lid,isModule,decls,xmlDoc,attribs,access,m)) -> 
        let lid = 
            match lid with 
            | [id] when isModule && id.idText = MangledGlobalName -> error(Error(FSComp.SR.buildInvalidModuleOrNamespaceName(),id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid
        ModuleOrNamespaceSig(lid,isModule,decls,xmlDoc,attribs,access,m)

    | AnonTopModuleSig (defs,m) -> 
        if not isLastCompiland && not (doNotRequireNamespaceOrModuleSuffixes |> List.exists (Filename.checkSuffix (String.lowercase filename))) then 
            errorR(Error(FSComp.SR.buildMultiFileRequiresNamespaceOrModule(),m))
        let modname = ComputeAnonModuleName (nonNil defs) defaultNamespace filename m
        ModuleOrNamespaceSig(modname,true,defs,PreXmlDoc.Empty,[],None,m)

    | NamespaceFragmentSig (lid,b,c,d,e,m)-> 
        let lid = 
            match lid with 
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid
        ModuleOrNamespaceSig(lid,b,c,d,e,None,m)

let PostParseModuleImpls (defaultNamespace,filename,isLastCompiland,ParsedImplFile(hashDirectives,impls)) = 
    match impls |> List.rev |> List.tryPick (function NamedTopModule(ModuleOrNamespace(lid,_,_,_,_,_,_)) -> Some(lid) | _ -> None) with
    | Some lid when impls.Length > 1 -> 
        errorR(Error(FSComp.SR.buildMultipleToplevelModules(),rangeOfLid lid))
    | _ -> 
        ()
    let impls = impls |> List.mapi (fun i x -> PostParseModuleImpl (i, defaultNamespace, isLastCompiland, filename, x)) 
    let qualName = QualFileNameOfImpls filename impls
    let isScript = IsScript filename

    let scopedPragmas = 
        [ for (ModuleOrNamespace(_,_,decls,_,_,_,_)) in impls do 
            for d in decls do
                match d with 
                | SynModuleDecl.HashDirective (hd,_) -> yield! GetScopedPragmasForHashDirective hd
                | _ -> () 
          for hd in hashDirectives do 
              yield! GetScopedPragmasForHashDirective hd ]
    ImplFileInput(ImplFile(filename,isScript,qualName,scopedPragmas,hashDirectives,impls,isLastCompiland))
  
let PostParseModuleSpecs (defaultNamespace,filename,isLastCompiland,ParsedSigFile(hashDirectives,specs)) = 
    match specs |> List.rev |> List.tryPick (function NamedTopModuleSig(ModuleOrNamespaceSig(lid,_,_,_,_,_,_)) -> Some(lid) | _ -> None) with
    | Some  lid when specs.Length > 1 -> 
        errorR(Error(FSComp.SR.buildMultipleToplevelModules(),rangeOfLid lid))
    | _ -> 
        ()
        
    let specs = specs |> List.mapi (fun i x -> PostParseModuleSpec(i,defaultNamespace,isLastCompiland,filename,x)) 
    let qualName = QualFileNameOfSpecs filename specs
    let scopedPragmas = 
        [ for (ModuleOrNamespaceSig(_,_,decls,_,_,_,_)) in specs do 
            for d in decls do
                match d with 
                | SynModuleSigDecl.HashDirective(hd,_) -> yield! GetScopedPragmasForHashDirective hd
                | _ -> () 
          for hd in hashDirectives do 
              yield! GetScopedPragmasForHashDirective hd ]

    SigFileInput(SigFile(filename,qualName,scopedPragmas,hashDirectives,specs))

let ParseInput (lexer,lexbuf:UnicodeLexing.Lexbuf,defaultNamespace,filename,isLastCompiland) = 
    // The assert below is almost ok, but it fires in two cases:
    //  - fsi.exe sometimes passes "stdin" as a dummy filename
    //  - if you have a #line directive, e.g. 
    //        # 1000 "Line01.fs"
    //    then it also asserts.  But these are edge cases that can be fixed later, e.g. in bug 4651.
    //System.Diagnostics.Debug.Assert(System.IO.Path.IsPathRooted(filename), sprintf "should be absolute: '%s'" filename)
    let lower = String.lowercase filename 
    // Delay sending errors and warnings until after the file is parsed. This gives us a chance to scrape the
    // #nowarn declarations for the file
    let input = 
        if implSuffixes |> List.exists (Filename.checkSuffix lower)   then  
            let impl = Parser.implementationFile lexer lexbuf 
            PostParseModuleImpls (defaultNamespace,filename,isLastCompiland,impl)
        elif sigSuffixes |> List.exists (Filename.checkSuffix lower)  then  
            let intfs = Parser.signatureFile lexer lexbuf 
            PostParseModuleSpecs (defaultNamespace,filename,isLastCompiland,intfs)
        else 
            failwith "bad suffix"
    input

let ParseOneInputLexbuf (lexResourceManager,conditionalCompilationDefines,lexbuf,filename,isLastCompiland,errorLogger) =
    let skip = true in (* don't report whitespace from lexer *)
    let lightSyntaxStatus = LightSyntaxStatus (true,true) 
    let lexargs = mkLexargs (conditionalCompilationDefines,lightSyntaxStatus,lexResourceManager, ref [],errorLogger)
    let input = 
        Lexhelp.usingLexbufForParsing (lexbuf,filename) (fun lexbuf ->
            let tokenizer = Lexfilter.LexFilter(lightSyntaxStatus, Lexer.token lexargs skip, lexbuf)

            let res = ParseInput(tokenizer.Lexer,lexbuf,None,filename,isLastCompiland)
            res
        )
    input

// Share intern'd strings across all lexing/parsing
let lexResourceManager = new Lexhelp.LexResourceManager()

let ParseOneInputFile (conditionalCompilationDefines,(sourceCodeOfTheFile:string),filename,errorLogger) =
    // filename is only used for things like error reporting, looking at extension to determine if .fs versus .fsi, etc
    // sourceCodeOfTheFile is the actual code it will parse, won't use filename to load off disk
    let lexbuf = Internal.Utilities.Text.Lexing.LexBuffer<char>.FromChars(sourceCodeOfTheFile.ToCharArray())  
    ParseOneInputLexbuf(lexResourceManager,conditionalCompilationDefines,lexbuf,filename,true(*isLastCompiland*),errorLogger)


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// The code above this point is nearly verbatim code from the F# compiler with only a few minor edits.
//
// The code below here is the essence of the extension.
/////////////////////////////////////////////////////////////////////////////////////////////////////////

let rec digNamesFrom pat =
    match pat with
    | SynPat.Const _ -> []
    | SynPat.Wild _ -> []
    | SynPat.Named(_innerPat,id,_isTheThisVar,_access,_range) -> [id.idText]
    | SynPat.Typed(pat,_type,_range) -> digNamesFrom pat
    | SynPat.Attrib(pat,_attrs,_range) -> digNamesFrom pat
    | SynPat.Or(_pat1,_pat2,_range) -> [] // no one uses ors in fun decls
    | SynPat.Ands(_pats,_range) -> [] // no one uses ands in fun decls
    | SynPat.LongIdent(_lid,_idOpt,_typDeclsOpt,pats,_access,_range) -> pats |> List.collect digNamesFrom 
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

type XmlDocable =
    | XmlDocable of (*line:*) int * (*indent:*) int * (*paramNames:*) string list

let GetXmlDocablesImpl(sourceCodeLinesOfTheFile:string[], sourceCodeOfTheFile, filename) =
    let indentOf (lineNum:int) =
        let mutable i = 0
        let line = sourceCodeLinesOfTheFile.[lineNum-1] // -1 because lineNum reported by xmldocs are 1-based, but array is 0-based
        while i < line.Length && line.Chars(i) = ' ' do
            i <- i + 1
        i

    let rec GetXmlDocablesSynModuleDecl decl =
        match decl with
        | SynModuleDecl.ModuleAbbrev(_ident, _longIdent, _range) -> []
        | SynModuleDecl.NestedModule(_synComponentInfo, synModuleDecls, _, _range) -> (synModuleDecls |> List.collect GetXmlDocablesSynModuleDecl)
        | SynModuleDecl.Let(_, synBindingList, range) -> 
            let anyXmlDoc = 
                synBindingList |> List.exists (fun (SynBinding.Binding(_, _, _, _, _, preXmlDoc, _, _, _, _, _, _)) -> match preXmlDoc.ToXmlDoc() with | XmlDoc [| |] -> false | _ -> true)
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

    and GetXmlDocablesSynModuleOrNamespace (ModuleOrNamespace(_longIdent, _isModule, synModuleDecls, _preXmlDoc, _synAttributes, _synAccessOpt, _range)) =
        (synModuleDecls |> List.collect GetXmlDocablesSynModuleDecl)

    and GetXmlDocablesSynTypeDefn (SynTypeDefn.TypeDefn(ComponentInfo(synAttributes, _synTyparDeclList, _synTypeConstraintList, _longIdent, preXmlDoc, _bool, _synAccessOpt, compRange), synTypeDefnRepr, synMemberDefns, tRange)) =
        let stuff = 
            match synTypeDefnRepr with
            | ObjectModel(_synTypeDefnKind, synMemberDefns, _oRange) -> (synMemberDefns |> List.collect GetXmlDocablesSynMemberDefn)
            | Simple(_synTypeDefnSimpleRepr, _range) -> []
        let docForTypeDefn = 
            match preXmlDoc.ToXmlDoc() with 
            | XmlDoc [| |] -> 
                let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) (unionRanges compRange tRange)
                let line = fullRange.StartLine 
                let indent = indentOf line
                [XmlDocable(line,indent,[])]
            | _ -> []
        docForTypeDefn @ stuff @ (synMemberDefns |> List.collect GetXmlDocablesSynMemberDefn)

    and GetXmlDocablesSynMemberDefn m =
        match m with
        | SynMemberDefn.Open(_longIdent, _range) -> []
        | SynMemberDefn.Member(SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, synAttributes, preXmlDoc, _synValData, synPat, _synBindingReturnInfoOption, _synExpr, _range, _sequencePointInfoForBinding), memRange) -> 
            match preXmlDoc.ToXmlDoc() with
            | XmlDoc [| |] ->
                let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) memRange
                let line = fullRange.StartLine 
                let indent = indentOf line
                let paramNames = digNamesFrom synPat 
                [XmlDocable(line,indent,paramNames)]
            | _ -> []
        | SynMemberDefn.ImplicitCtor(_synAccessOption, _synAttributes, _synSimplePatList, _identOption, _range) -> []
        | SynMemberDefn.ImplicitInherit(_synType, _synExpr, _identOption, _range) -> []
        | SynMemberDefn.LetBindings(_synBindingList, _, _, _range) -> []
        | SynMemberDefn.AbstractSlot(ValSpfn(synAttributes, _ident, _synValTyparDecls, _synType, SynValInfo(args, _ret), _, _, preXmlDoc, _synAccessOpt, _synExprOpt, _range), _memberFlags, range) -> 
            match preXmlDoc.ToXmlDoc() with
            | XmlDoc [| |] ->
                let fullRange = synAttributes |> List.fold (fun r a -> unionRanges r a.Range) range
                let line = fullRange.StartLine 
                let indent = indentOf line
                let paramNames = args |> List.collect (fun az -> az |> List.choose (fun (SynArgInfo(_synAttributes, _, idOpt)) -> match idOpt with | Some id -> Some(id.idText) | _ -> None))
                [XmlDocable(line,indent,paramNames)]
            | _ -> []
        | SynMemberDefn.Interface(_synType, synMemberDefnsOption, _range) -> 
            match synMemberDefnsOption with 
            | None -> [] 
            | Some(x) -> x |> List.collect GetXmlDocablesSynMemberDefn
        | SynMemberDefn.Inherit(_synType, _identOption, _range) -> []
        | SynMemberDefn.ValField(_synField, _range) -> []
        | SynMemberDefn.NestedType(synTypeDefn, _synAccessOption, _range) -> GetXmlDocablesSynTypeDefn synTypeDefn

    and GetXmlDocablesInput input =
        match input with
        | ImplFileInput(ImplFile(_,_,_,_,_,l,_))-> 
            l |> List.collect GetXmlDocablesSynModuleOrNamespace
        | SigFileInput _sigFile -> []

    let conditionalCompilationDefines= []
    let input = ParseOneInputFile (conditionalCompilationDefines,sourceCodeOfTheFile,filename,new ErrorLogger())
    GetXmlDocablesInput input

// Main API.
let GetXmlDocables(sourceCodeOfTheFile : string, filename) =
    let sourceCodeLinesOfTheFile = sourceCodeOfTheFile.Split [|'\n'|]
    GetXmlDocablesImpl(sourceCodeLinesOfTheFile, sourceCodeOfTheFile, filename)

