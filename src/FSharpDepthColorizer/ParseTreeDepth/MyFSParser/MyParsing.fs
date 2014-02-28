﻿module MyParsing

// This file has two main parts:
//  - first, nearly verbatim code from misc portions of the F# compiler
//  - second, the essence of the extension for getting range information out of the AST
// There's a comment about halfway down that delineates the break.

open System
open System.Text
open System.IO
open Internal.Utilities
open Internal.Utilities.Text
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler 

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.PrettyNaming
open Internal.Utilities.FileSystem
open Internal.Utilities.Collections

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
        | SynExpr.Sequential(_sequencePointInfoForSeq, _, _synExpr, _synExpr2, range) -> 
            // the right curly brace may be on its own line to the left of the left curly, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.YieldOrReturnFrom(_, _synExpr, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _synPat, _synExpr, _synExpr2,_, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | SynExpr.DoBang(_synExpr, range) ->
            // the right curly brace may be on its own line to the left of this code, and this screws up indents
            if range.EndColumn < range.StartColumn then cutoffTheLineBefore range else range
        | _ -> expr.Range
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
    | SynMemberDefn.AutoProperty(_,_,_,_,_,_,_,_,_,_,range) -> range

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

and GetRangesSynModuleOrNamespace (SynModuleOrNamespace(_longIdent, _isModule, synModuleDecls, _preXmlDoc, _synAttributes, _synAccessOpt, _range)) =
    (synModuleDecls |> List.collect GetRangesSynModuleDecl)

and GetRangesSynExpr expr =
    GetRangesSynExprK expr id

and GetRangesSynExprK expr k=
    match expr with 
    | SynExpr.Paren(synExpr,_,_, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Quote(synExpr, _, synExpr2, _, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.Const(_synConst, _range) -> k[]
    | SynExpr.Typed(synExpr, _synType, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.Tuple(synExprList, _,  _range) -> synExprList |> List.collect GetRangesSynExpr
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
    | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _, _synPat, synExpr, synExpr2, _range) -> 
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
    | SynExpr.App(_exprAtomicFlag,_, synExpr, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.TypeApp(synExpr, _,_,_,_, _synTypeList, _range) -> GetRangesSynExprK synExpr k
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
    | SynExpr.Sequential(_sequencePointInfoForSeq, _, synExpr, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.IfThenElse(synExpr, synExpr2, synExprOpt, _sequencePointInfoForBinding, _,_range, _range2) ->
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
    | SynExpr.LongIdent(_,_, _longIdent, _range) -> k[]
    | SynExpr.LongIdentSet(_longIdent, synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.DotGet(synExpr, _longIdent, _,  _range) -> GetRangesSynExprK synExpr k
    | SynExpr.DotSet(synExpr, _longIdent, synExpr2, _range) ->
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.DotIndexedGet(synExpr, synExprList, _range, _range2) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (r1 @ (synExprList |> List.collect (function SynIndexerArg.One e -> GetRangesSynExpr e | SynIndexerArg.Two(e1, e2) -> GetRangesSynExpr e1 @ GetRangesSynExpr e2))))
    | SynExpr.DotIndexedSet(synExpr, synExprList, synExpr2, _range, _range2, _range3) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ (synExprList |> List.collect (function SynIndexerArg.One e -> GetRangesSynExpr e | SynIndexerArg.Two(e1, e2) -> GetRangesSynExpr e1 @ GetRangesSynExpr e2)) @ r2)))
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
    | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _, _synPat, synExpr, synExpr2, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (RangeOfSynExpr synExpr :: (* RangeOfSynExpr synExpr2 :: *) r1 @ r2)))
    | SynExpr.DoBang(synExpr, _range) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        k (RangeOfSynExpr synExpr :: r1))
    | SynExpr.LibraryOnlyILAssembly _ -> k[]
    | SynExpr.LibraryOnlyStaticOptimization _ -> k[]
    | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> k[]
    | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> k[]
    | SynExpr.ArbitraryAfterError(_string, _range) -> k[]
    | SynExpr.DiscardAfterMissingQualificationAfterDot(synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.FromParseError (synExpr, _range) -> GetRangesSynExprK synExpr k
    | SynExpr.JoinIn (synExpr, _range, synExpr2, _range2) -> 
        GetRangesSynExprK synExpr (fun r1 ->
        GetRangesSynExprK synExpr2 (fun r2 ->
        k (r1 @ r2)))
    | SynExpr.MatchLambda(_,_range,synMatchClauses, _seqInfo, _range2) -> 
        k (List.collect GetRangesSynMatchClause synMatchClauses)
    
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
    | SynMemberDefn.AutoProperty(_,_,_,_,_,_,_,_,synExpr,_,_) ->
        GetRangesSynExpr synExpr

and GetRangesSynMatchClause (SynMatchClause.Clause(_synPat, _synExprOption, synExpr, _range, _sequencePointInfoForTarget)) =
    RangeOfSynExpr synExpr :: GetRangesSynExpr synExpr

and GetRangesSynBinding (SynBinding.Binding(_synAccessOption, _synBindingKind, _, _, _synAttributes, _preXmlDoc, _synValData, _synPat, _synBindingReturnInfoOption, synExpr, _range, _sequencePointInfoForBinding)) =
    RangeOfSynExpr synExpr :: GetRangesSynExpr synExpr

/////////////////////////////////////////////////////

// Top-level entry point into the range-computing code.
// For now, only implementation files report any ranges; signature files are ignored.
let GetRangesInput input =
   match input with
    | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,l,_))-> 
       l |> List.collect GetRangesSynModuleOrNamespace
    | ParsedInput.SigFile _sigFile -> []

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

open Microsoft.FSharp.Compiler.SourceCodeServices
let checker = InteractiveChecker.Create()

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
    
    // Get compiler options for the 'project' implied by a single script file
    let projOptions = checker.GetProjectOptionsFromScript(filename, sourceCodeOfTheFile)
    let input = checker.ParseFileInProject (filename, sourceCodeOfTheFile, projOptions)
    
    match input.ParseTree with
    | Some tree -> 
        tree
        |> GetRangesInput 
        |> List.map (fun r -> r.StartLine, r.StartColumn, r.EndLine, r.EndColumn, indent r.StartLine r.EndLine)
        |> List.toArray 
    | None -> failwith "Failed to parse"

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
    results |> Seq.toArray
