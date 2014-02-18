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
// The code below here is the essence of the extension for getting range information out of the AST.
/////////////////////////////////////////////////////////////////////////////////////////////////////////

// Top-level entities report their range as going up to and through the 'next starter token', e.g. the 'let' or 'type'
// that begins the next construct.  This function just hacks off a range to end at the previous line, as an ad-hoc way to
// work around the ranges that have this extra ugly residue at the end of them.
let cutoffTheLineBefore (r:range) =
    let filename = r.FileName
    let startL,startC = r.StartLine,r.StartColumn
    let endL,_endC   = r.EndLine,r.EndColumn
    if endL <= startL then
        r
    else
        mkRange filename (mkPos startL startC) (mkPos (endL-1) 255) // the 'range' type only handles 9 bits of column width, I am doing 8 bits because 9 seemed to fail when I tried it, hm

// the full range of an expression
let RangeOfSynExpr expr = 
    // expr.Range // this almost works, but I have some cases I want to ad-hoc
    let result = 
        match expr with 
        | SynExpr.Paren(_synExpr, range) -> range
        | SynExpr.Quote(_synExpr, _, _synExpr2, range) -> range
        | SynExpr.Const(_synConst, range) -> range
        | SynExpr.Typed(_synExpr, _synType, range) -> range
        | SynExpr.Tuple(_synExprList, range) -> range
        | SynExpr.ArrayOrList(_, _synExprList, range) -> range
        | SynExpr.Record(_,_,_,range) -> range
        | SynExpr.New(_, _synType, _synExpr, range) -> range
        | SynExpr.ObjExpr(_,_,_,_,range) -> range
        | SynExpr.While(_sequencePointInfoForWhileLoop, _synExpr, _synExpr2, range) -> range
        | SynExpr.For(_sequencePointInfoForForLoop, _ident, _synExpr, _, _synExpr2, _synExpr3, range) -> range
        | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _synPat, _synExpr, _synExpr2, range) -> range
        | SynExpr.ArrayOrListOfSeqExpr(_, _synExpr, range) -> range
        | SynExpr.CompExpr(_, _, _synExpr, range) -> range
        | SynExpr.Lambda(_, _, _synSimplePats, _synExpr, range) -> range
        | SynExpr.Match(_sequencePointInfoForBinding, _synExpr, _synMatchClauseList, _, range) -> range
        | SynExpr.Do(_synExpr, range) -> range
        | SynExpr.Assert(_synExpr, range) -> range
        | SynExpr.App(_exprAtomicFlag, _synExpr, _synExpr2, range) -> range
        | SynExpr.TypeApp(_synExpr, _synTypeList, range) -> range
        | SynExpr.LetOrUse(_, _, _synBindingList, _synExpr, range) -> range
        | SynExpr.TryWith(_synExpr, _range, _synMatchClauseList, _range2, range3, _sequencePointInfoForTry, _sequencePointInfoForWith) -> range3
        | SynExpr.TryFinally(_synExpr, _synExpr2, range, _sequencePointInfoForTry, _sequencePointInfoForFinally) -> range
        | SynExpr.Lazy(_synExpr, range) -> range
        | SynExpr.Seq(_sequencePointInfoForSeq, _, _synExpr, _synExpr2, range) -> 
            // the right curly brace may be on its own line to the left of the left curly, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.IfThenElse(_synExpr, _synExpr2, _synExprOpt, _sequencePointInfoForBinding, _range, range2) -> range2
        | SynExpr.Ident(ident) -> ident.idRange
        | SynExpr.LongIdent(_, _longIdent, range) -> range
        | SynExpr.LongIdentSet(_longIdent, _synExpr, range) -> range
        | SynExpr.DotGet(_synExpr, _longIdent, range) -> range
        | SynExpr.DotSet(_synExpr, _longIdent, _synExpr2, range) -> range
        | SynExpr.DotIndexedGet(_synExpr, _synExprList, _range, range2) -> range2
        | SynExpr.DotIndexedSet(_synExpr, _synExprList, _synExpr2, _range, range2) -> range2
        | SynExpr.NamedIndexedPropertySet(_longIdent, _synExpr, _synExpr2, range) -> range
        | SynExpr.DotNamedIndexedPropertySet(_synExpr, _longIdent, _synExpr2, _synExpr3, range) -> range
        | SynExpr.TypeTest(_synExpr, _synType, range) -> range
        | SynExpr.Upcast(_synExpr, _synType, range) -> range
        | SynExpr.Downcast(_synExpr, _synType, range) -> range
        | SynExpr.InferredUpcast(_synExpr, range) -> range
        | SynExpr.InferredDowncast(_synExpr, range) -> range
        | SynExpr.Null(range) -> range
        | SynExpr.AddressOf(_, _synExpr, _range, range2) -> range2
        | SynExpr.TraitCall(_synTyparList, _synMemberSig, _synExpr, range) -> range
        | SynExpr.ImplicitZero(range) -> range
        | SynExpr.YieldOrReturn(_, _synExpr, range) -> range
        | SynExpr.YieldOrReturnFrom(_, _synExpr, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _synPat, _synExpr, _synExpr2, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.DoBang(_synExpr, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.DeprecatedTypeOf(_synType, range) -> range
        | SynExpr.LibraryOnlyILAssembly(_,_,_,_,range) -> range
        | SynExpr.LibraryOnlyStaticOptimization(_,_,_,range) -> range
        | SynExpr.LibraryOnlyUnionCaseFieldGet(_,_,_,range) -> range
        | SynExpr.LibraryOnlyUnionCaseFieldSet(_,_,_,_,range) -> range
        | SynExpr.ArbitraryAfterError(range) -> range
        | SynExpr.DiscardAfterError(_synExpr, range) -> range
    System.Diagnostics.Debug.WriteLine(result)
    result

// the full range of a member
let RangeOfSynMemberDefn m =
    match m with
    | SynMemberDefn.Open(_longIdent, range) -> range
    | SynMemberDefn.Member(SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, _synAttributes, _preXmlDoc, _synValData, _synPat, 
                                              _synBindingReturnInfoOption, synExpr, brange, _sequencePointInfoForBinding), 
                           range) -> 
        unionRanges range (unionRanges brange (RangeOfSynExpr synExpr))
    | SynMemberDefn.ImplicitCtor(_synAccessOption, _synAttributes, _synSimplePatList, _identOption, range) -> range
    | SynMemberDefn.ImplicitInherit(_synType, _synExpr, _identOption, range) -> range
    | SynMemberDefn.LetBindings(_synBindingList, _, _, range) -> range
    | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, range) -> range
    | SynMemberDefn.Interface(_synType, _synMemberDefnsOption, range) -> range
    | SynMemberDefn.Inherit(_synType, _identOption, range) -> range
    | SynMemberDefn.ValField(_synField, range) -> range
    | SynMemberDefn.NestedType(_synTypeDefn, _synAccessOption, range) -> range

///////////
// The functions below recursively compute a list of all the nested ranges in their constructs; the choices of exactly what to
// include are a little ad-hoc, based on what Brian thought looked good on the screen.
let rec GetRangesSynModuleDecl decl =
    match decl with
    | SynModuleDecl.ModuleAbbrev(_ident, _longIdent, _range) -> []
    | SynModuleDecl.NestedModule(_synComponentInfo, synModuleDecls, _, range) -> (cutoffTheLineBefore range) :: (synModuleDecls |> List.collect GetRangesSynModuleDecl)
    | SynModuleDecl.Let(_, synBindingList, range) -> (cutoffTheLineBefore range) :: (synBindingList |> List.collect GetRangesSynBinding)
    | SynModuleDecl.DoExpr(_sequencePointInfoForBinding, synExpr, range) -> (cutoffTheLineBefore range) :: (GetRangesSynExpr synExpr)
    | SynModuleDecl.Types(synTypeDefnList, range) -> (cutoffTheLineBefore range) :: (synTypeDefnList |> List.collect GetRangesSynTypeDefn)
    | SynModuleDecl.Exception(_synExceptionDefn, _range) -> []
    | SynModuleDecl.Open(_longIdent, _range) -> []
    | SynModuleDecl.Attributes(_synAttributes, _range) -> []
    | SynModuleDecl.HashDirective(_parsedHashDirective, _range) -> []
    | SynModuleDecl.NamespaceFragment(synModuleOrNamespace) -> GetRangesSynModuleOrNamespace synModuleOrNamespace

and GetRangesSynModuleOrNamespace (ModuleOrNamespace(_longIdent, _isModule, synModuleDecls, _preXmlDoc, _synAttributes, _synAccessOpt, _range)) =
    (synModuleDecls |> List.collect GetRangesSynModuleDecl)

and GetRangesSynExpr expr =
    GetRangesSynExprK expr id

and GetRangesSynExprK expr k=
    match expr with 
    | SynExpr.Paren(synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Quote(synExpr, _, synExpr2, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.Const(_synConst, _range) -> k[]
    | SynExpr.Typed(synExpr, _synType, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Tuple(synExprList, _range) -> synExprList |> List.collect GetRangesSynExpr
    | SynExpr.ArrayOrList(_, synExprList, _range) -> k(synExprList |> List.collect GetRangesSynExpr)
    | SynExpr.Record(_) -> k[]
    | SynExpr.New(_, _synType, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.ObjExpr(_) -> k[]
    | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr, synExpr2, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: r1 @ r2)))
    | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr, _, synExpr2, synExpr3, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        GetRangesSynExprK synExpr3 (fun r3 ->
        k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: RangeOfSynExpr synExpr3 :: r1 @ r2 @ r3))))
    | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _synPat, synExpr, synExpr2, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: r1 @ r2)))
    | SynExpr.ArrayOrListOfSeqExpr(_, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.CompExpr(_, _, synExpr, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.Lambda(_, _, _synSimplePats, synExpr, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.Match(_sequencePointInfoForBinding, synExpr, synMatchClauseList, _, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k ( r1 @ (synMatchClauseList |> List.collect GetRangesSynMatchClause)))
    | SynExpr.Do(synExpr, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.Assert(synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.App(_exprAtomicFlag, synExpr, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.TypeApp(synExpr, _synTypeList, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.LetOrUse(_, _, synBindingList, synExpr, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (r1 @ (synBindingList |> List.collect GetRangesSynBinding)))
    | SynExpr.TryWith(synExpr, _range, synMatchClauseList, _range2, _range3, _sequencePointInfoForTry, _sequencePointInfoForWith) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1 @ (synMatchClauseList |> List.collect GetRangesSynMatchClause)))
    | SynExpr.TryFinally(synExpr, synExpr2, _range, _sequencePointInfoForTry, _sequencePointInfoForFinally) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: r1 @ r2)))
    | SynExpr.Lazy(synExpr, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.Seq(_sequencePointInfoForSeq, _, synExpr, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.IfThenElse(synExpr, synExpr2, synExprOpt, _sequencePointInfoForBinding, _range, _range2) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        match synExprOpt with 
        | None -> 
            k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: r1 @ r2)
        | Some(x) -> 
            GetRangesSynExprK x (fun r3 ->
            k (RangeOfSynExpr synExpr :: RangeOfSynExpr synExpr2 :: r1 @ r2 @ (RangeOfSynExpr x :: r3)))
        ))
    | SynExpr.Ident(_ident) -> k[]
    | SynExpr.LongIdent(_, _longIdent, _range) -> k[]
    | SynExpr.LongIdentSet(_longIdent, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.DotGet(synExpr, _longIdent, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.DotSet(synExpr, _longIdent, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.DotIndexedGet(synExpr, synExprList, _range, _range2) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (r1 @ (synExprList |> List.collect GetRangesSynExpr)))
    | SynExpr.DotIndexedSet(synExpr, synExprList, synExpr2, _range, _range2) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ (synExprList |> List.collect GetRangesSynExpr) @ r2)))
    | SynExpr.NamedIndexedPropertySet(_longIdent, synExpr, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.DotNamedIndexedPropertySet(synExpr, _longIdent, synExpr2, synExpr3, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        GetRangesSynExprK synExpr3 (fun r3 ->
        k (r1 @ r2 @ r3))))
    | SynExpr.TypeTest(synExpr, _synType, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Upcast(synExpr, _synType, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Downcast(synExpr, _synType, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.InferredUpcast(synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.InferredDowncast(synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Null(_range) -> k[]
    | SynExpr.AddressOf(_, synExpr, _range, _range2) -> GetRangesSynExprK synExpr k
    | SynExpr.TraitCall(_synTyparList, _synMemberSig, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.ImplicitZero(_range) -> k[]
    | SynExpr.YieldOrReturn(_, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.YieldOrReturnFrom(_, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _synPat, synExpr, synExpr2, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (RangeOfSynExpr synExpr :: (* RangeOfSynExpr synExpr2 :: *) r1 @ r2)))
    | SynExpr.DoBang(synExpr, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.DeprecatedTypeOf(_synType, _range) -> k[]
    | SynExpr.LibraryOnlyILAssembly _ -> k[]
    | SynExpr.LibraryOnlyStaticOptimization _ -> k[]
    | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> k[]
    | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> k[]
    | SynExpr.ArbitraryAfterError(_range) -> k[]
    | SynExpr.DiscardAfterError(synExpr, _range) -> GetRangesSynExprK synExpr k

and GetRangesSynTypeDefn (SynTypeDefn.TypeDefn(_synComponentInfo, synTypeDefnRepr, synMemberDefns, _tRange)) =
    let stuff = 
        match synTypeDefnRepr with
        | ObjectModel(_synTypeDefnKind, synMemberDefns, oRange) ->
            let whole = 
                synMemberDefns 
                |> Seq.filter (function | SynMemberDefn.ImplicitCtor _ | SynMemberDefn.ImplicitInherit _ -> false | _ -> true)
                |> Seq.map RangeOfSynMemberDefn |> Seq.fold unionRanges oRange
                |> cutoffTheLineBefore
            whole :: (synMemberDefns |> List.collect GetRangesSynMemberDefn)
        | Simple(_synTypeDefnSimpleRepr, _range) -> []
    stuff @ (synMemberDefns |> List.collect GetRangesSynMemberDefn)

and GetRangesSynMemberDefn m =
    match m with
    | SynMemberDefn.Open(_longIdent, _range) -> []
    | SynMemberDefn.Member(synBinding, _range) -> GetRangesSynBinding synBinding
    | SynMemberDefn.ImplicitCtor(_synAccessOption, _synAttributes, _synSimplePatList, _identOption, _range) -> []
    | SynMemberDefn.ImplicitInherit(_synType, synExpr, _identOption, _range) -> GetRangesSynExpr synExpr
    | SynMemberDefn.LetBindings(synBindingList, _, _, _range) -> synBindingList |> List.collect GetRangesSynBinding
    | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) -> []
    | SynMemberDefn.Interface(_synType, synMemberDefnsOption, _range) -> 
        match synMemberDefnsOption with 
        | None -> [] 
        | Some(x) -> 
            let r = x |> List.collect GetRangesSynMemberDefn
            match x with
            | [] -> r
            | _ -> (x |> List.map RangeOfSynMemberDefn |> List.reduce unionRanges) :: r
    | SynMemberDefn.Inherit(_synType, _identOption, _range) -> []
    | SynMemberDefn.ValField(_synField, _range) -> []
    | SynMemberDefn.NestedType(synTypeDefn, _synAccessOption, _range) -> GetRangesSynTypeDefn synTypeDefn

and GetRangesSynMatchClause (SynMatchClause.Clause(_synPat, _synExprOption, synExpr, _range, _sequencePointInfoForTarget)) =
    RangeOfSynExpr synExpr :: GetRangesSynExpr synExpr

and GetRangesSynBinding (SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, _synAttributes, _preXmlDoc, _synValData, _synPat, _synBindingReturnInfoOption, synExpr, _range, _sequencePointInfoForBinding)) =
    RangeOfSynExpr synExpr :: GetRangesSynExpr synExpr

/////////////////////////////////////////////////////

// Top-level entry point into the range-computing code.
// For now, only implementation files report any ranges; signature files are ignored.
let GetRangesInput input =
    match input with
    | ImplFileInput(ImplFile(_,_,_,_,_,l,_))-> 
        l |> List.collect GetRangesSynModuleOrNamespace
    | SigFileInput _sigFile -> []

/////////////////////////////////////////////////////

// How indented is each line of the file?  
// Report MaxValue for lines containing only whitespace or that appear to start with a hash directive or comment.
let ComputeMinIndentArray(sourceCodeLinesOfTheFile) =
    let indent (line:string) =
        let mutable i = 0
        while i < line.Length && line.Chars(i) = ' ' do
            i <- i + 1
        if i = line.Length || ((i = line.Length - 1) && (line.Chars(i) = '\r'))
           || (line.Chars(i) = '#') // we treat any line whose first non-whitespace char is '#' as a blank line, so that unindented "#if" don't screw things up
           || (line.Chars(i) = '/' && (i+1< line.Length) && line.Chars(i+1) = '/')  // same for "//comments"
           then
            System.Int32.MaxValue // line is entirely whitespace
        else
            i
    sourceCodeLinesOfTheFile
    |> Seq.map indent
    |> (fun s -> Seq.append s [0;0])  // some ranges are one line past EOF
    |> Seq.toArray

/////////////////////////////////////////////////////

// TODO, consider perf, way just to consider viewport and do as little work as necessary?

let GetRangesImpl(sourceCodeLinesOfTheFile, sourceCodeOfTheFile, filename) =
    let mindents = ComputeMinIndentArray sourceCodeLinesOfTheFile
    let indent startLine endLine =
        let mutable i, n = mindents.[startLine-1], startLine+1
        while n <= endLine do
            i <- min i mindents.[n-1] // line nums are 1-based
            n <- n + 1
        i
    let conditionalCompilationDefines= []
    let input = ParseOneInputFile (conditionalCompilationDefines,sourceCodeOfTheFile,filename,new ErrorLogger())
    GetRangesInput input
    |> List.map (fun r -> r.StartLine, r.StartColumn, r.EndLine, r.EndColumn, indent r.StartLine r.EndLine)
    |> List.toArray 

// One of the two main APIs.  This reports all the nested ranges in a file, along with the 'indent' of that range.
// This is not used by the colorizer, but may be useful to other "scope tools", and it used as an implementation detail
// of the other API, below.
let GetRanges(sourceCodeOfTheFile : string, filename) =
    let sourceCodeLinesOfTheFile = sourceCodeOfTheFile.Split [|'\n'|]
    GetRangesImpl(sourceCodeLinesOfTheFile, sourceCodeOfTheFile, filename)

/////////////////////////////////////////////////////

// If we want to draw colored rectangular adornments, it is useful to post-process the 'nested' ranges into a list of 
// unnested ranges, where each range is entirely on a single line, and no ranges overlap.  That is, each range corresponds
// to a colored rectangle on the screen.  
//
// The algorithm below is the simplest thing I came up with to do this post-processing.
//
// One implementation artifact of the editor creeps in here.  The editor can only 'tag' a location if the location actually
// exists as a character in the file.  However, we want to mark e.g. a series of indents in the middle of a function, 
// even for blank lines, which may have no characters (not even whitespace) on which to hang editor tags.
// We notice when we would be hanging information on a "nonexistent character" here, and mark these non-existers with
// a negative depth (e.g. -3 instead of 3).  Then the part that draws the adornments can notice this and do clever things 
// to draw the rectangles even in the absence of a character to draw text-relative to.
//
// (An alternative implementation, which I did not explore, would be one that represents rectangles as overlapping areas 
//  that may span multiple source code lines.)

type Range = int * int * int * int * int  // start line, column, end line, column, minIndent
type QueueEvent = | Start of Range | End of Range
    with member this.Info = 
            match this with
            | Start(sl,sc,_,_,_) -> sl,sc
            | End(_,_,el,ec,_) -> el,ec
         member this.UniqueInfo = 
            match this with
            | Start(data) -> data
            | End(sl,sc,el,ec,m) -> el,ec,sl,sc,m

let qevComp = { new System.Collections.Generic.IComparer<QueueEvent> with
                    member this.Compare(x, y) =
                        compare x.UniqueInfo y.UniqueInfo }

// The other main API, the one consumed by the colorizer.
// This reports a list of non-overlapping ranges, each contained on a single line, along with the 'semantic depth'
// of that range (possibly negated, if in virtual whitespace).
let GetNonoverlappingDepthRanges(sourceCodeOfTheFile : string, filename) =
    let sourceCodeLinesOfTheFile = sourceCodeOfTheFile.Split([|"\r\n";"\n"|], StringSplitOptions.None)
    let lineLens = sourceCodeLinesOfTheFile |> Seq.map (fun s -> s.TrimEnd(null).Length) |> (fun s -> Seq.append s [0]) |> Seq.toArray 
    let len line = lineLens.[line-1]  // line #s are 1-based
    let nestedRanges = GetRangesImpl(sourceCodeLinesOfTheFile, sourceCodeOfTheFile, filename)
    let q = System.Collections.Generic.SortedSet<_>(qevComp)  // priority queue
    for r in nestedRanges do
        System.Diagnostics.Debug.WriteLine(sprintf "%A" r)
        let sl,sc,el,_ec,m = r
        if sl <> el then
            q.Add(Start r) |> ignore
        else
            // filter out ranges that start and end on same line, unless there are no other tokens on the line
            // approximate the 'no other tokens on line' info thusly (only check that expr is first thing on this line):
            if sc = m then // && ec = len el then // would fail on comments, trailing whitespace, etc
                q.Add(Start r) |> ignore
    let mutable curLine, curCol, curDepth = 1, 0, 0  // lines are 1-based
    let mindentStack = ResizeArray<(int*int)>() // numCharsIndent, semanticDepth
    mindentStack.Add(0,0)
    let results = ResizeArray<_>()
    let add((line,sc,ec,d) as info) =
        assert(sc <= ec)
        let l = len line
        if l > 0 && sc >= l then
            () // don't report spans past end of line (don't close color scopes in whitespace after end of line), (empty lines are exempt, we need to report something)
        else
            if l < ec then
                results.Add((line,sc,ec,-d))  // negative depth means this is on a whitespace-only line that is not long enough to hang a tag on
            else
                results.Add(info)
    while q.Count <> 0 do
        // dequeue
        let min = q.Min 
        q.Remove(min) |> ignore
        // yield spans
        let line, col = min.Info
        if line = curLine then
            // ... only line
            add(curLine, curCol, col, curDepth)
        else
            // ...first line
            add(curLine, curCol, (max curCol (len curLine)), curDepth)
            // ...rest of lines
            let curDent = fst mindentStack.[mindentStack.Count-1]
            let mutable n = curLine+1
            while n <= line do
                // indents
                for (a,d),(b,_) in Seq.pairwise mindentStack do
                    if a<>b then
                        add(n, a, b, d)
                // tokens from this line
                if n < line then
                    add(n, curDent, (max curDent (len n)), curDepth)
                else
                    add(n, curDent, col, curDepth)  // TODO any chance col > len line?
                n <- n + 1
        // update
        curLine <- line
        curCol <- col
        match min with
        | Start((_,_,_,_,m) as r) -> 
            q.Add(End r) |> ignore
            curDepth <- curDepth + 1
            mindentStack.Add(m, curDepth)
        | End _ ->
            curDepth <- curDepth - 1
            mindentStack.RemoveAt(mindentStack.Count-1)
    results |> ResizeArray.toArray 
