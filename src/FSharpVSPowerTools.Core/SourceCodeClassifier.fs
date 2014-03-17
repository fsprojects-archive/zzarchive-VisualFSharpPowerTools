module FSharpVSPowerTools.Core.SourceCodeClassifier

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

type Category =
    | ReferenceType
    | ValueType
    | PatternCase
    | TypeParameter
    | Function
    | PublicField
    | Other
    override x.ToString() = sprintf "%A" x

let internal getCategory (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter -> 
        TypeParameter
    | :? FSharpUnionCase
    | :? FSharpActivePatternCase -> 
        PatternCase

    | :? FSharpField as f ->
        if f.Accessibility.IsPublic then PublicField else Other

    | :? FSharpEntity as e ->
        //debug "%A (type: %s)" e (e.GetType().Name)
        if e.IsEnum || e.IsValueType then
            ValueType
        elif e.IsFSharpAbbreviation then
            let typ = e.AbbreviatedType
            if typ.HasTypeDefinition && (typ.TypeDefinition.IsEnum || typ.TypeDefinition.IsValueType) then
                ValueType
            else ReferenceType
        elif e.IsClass || e.IsDelegate || e.IsFSharpExceptionDeclaration
           || e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsMeasure || e.IsProvided
           || e.IsProvidedAndErased || e.IsProvidedAndGenerated then
            ReferenceType
        else Other
    
    | :? FSharpMemberFunctionOrValue as func ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if func.CompiledName = ".ctor" then ReferenceType
        elif func.FullType.IsFunctionType && not func.IsGetterMethod && not func.IsSetterMethod then 
            if func.DisplayName.StartsWith "( " && func.DisplayName.EndsWith " )" then
                Other
            else
                Function
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

let getTypeLocations (allSymbolsUses: FSharpSymbolUse[]) =
    allSymbolsUses
    |> Array.map (fun x ->
        let r = x.RangeAlternate
        let symbolLength = r.EndColumn - r.StartColumn
        let name = x.Symbol.DisplayName
        let fullName =
            if name = "( .ctor )" then 
                x.Symbol.FullName.Remove (x.Symbol.FullName.Length - name.Length)
            else
                x.Symbol.FullName
        let visibleName = fullName.Substring (max 0 (fullName.Length - symbolLength))
        
        let namespaceLength = visibleName.Length - name.Length
        
        let location = 
            if namespaceLength > 0 && symbolLength > name.Length && visibleName.EndsWith name then
                let startCol = r.StartColumn + namespaceLength
                let startCol' = 
                    if startCol < r.EndColumn then startCol
                    else r.StartColumn
                r.StartLine, startCol', r.EndLine, r.EndColumn
            else 
                r.StartLine, r.StartColumn, r.EndLine, r.EndColumn
        let category = getCategory x
        //debug "-=O=- %A: FullName = %s, VisibleName = %s, Name = %s, range = %A, symbol = %A" 
        //      category x.Symbol.FullName visibleName name location x.Symbol
        category, location)
    |> Seq.distinct
    |> Seq.toArray