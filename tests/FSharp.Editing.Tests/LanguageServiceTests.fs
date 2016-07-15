module FSharp.Editing.Tests.LanguageServiceTests

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()

let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let getFirstSymbol line col symbolText source =
    async {
        let line = line - 1
        let sourceLines = String.getLines source
        let lineStr = sourceLines.[line]
        let! results = languageService.ParseAndCheckFileInProject(opts source, fileName, source, AllowStaleResults.No)
        if results.Errors <> [||] then failwithf "Parse and check errors: %A" results.Errors
        let! su = results.GetSymbolUseAtLocation (line + 1, col, lineStr, [symbolText]) 
        return
            match su with
            | Some su -> su
            | None -> failwithf "No symbol at location. ParseTree = %A" results.ParseTree
    }
    |> Async.RunSynchronously

[<Test; Ignore "Ignored">]
let ``should instantiate types correctly``() =
    let source = 
        """
open System.Collections.Generic
let x: KeyValuePair<string, int> = failwith ""
let genericDictionary =
    { new IDictionary<string, 'TV> with
          member x.get_Item(key: string): 'TV = 
              raise (System.NotImplementedException())
          
          member x.set_Item(key: string, value: 'TV): unit = 
              raise (System.NotImplementedException())
          
          member x.get_Keys(): System.Collections.Generic.ICollection<string> = 
              raise (System.NotImplementedException())
          
          member x.get_Values(): System.Collections.Generic.ICollection<'TV> = 
              raise (System.NotImplementedException())
          
          member x.ContainsKey(key: string): bool = 
              raise (System.NotImplementedException())
          
          member x.Add(key: string, value: 'TV): unit = 
              raise (System.NotImplementedException())
          
          member x.Remove(key: string): bool = 
              raise (System.NotImplementedException())
          
          member x.TryGetValue(key: string, value: byref<'TV>): bool = 
              raise (System.NotImplementedException())
          
          member x.get_Count(): int = 
              raise (System.NotImplementedException())
          
          member x.get_IsReadOnly(): bool = 
              raise (System.NotImplementedException())
          
          member x.Add(item: KeyValuePair<string, 'TV>): unit = 
              raise (System.NotImplementedException())
          
          member x.Clear(): unit = 
              raise (System.NotImplementedException())
          
          member x.Contains(item: KeyValuePair<string, 'TV>): bool = 
              raise (System.NotImplementedException())
          
          member x.CopyTo(array: KeyValuePair<string, 'TV> [], arrayIndex: int): unit = 
              raise (System.NotImplementedException())
          
          member x.Remove(item: KeyValuePair<string, 'TV>): bool = 
              raise (System.NotImplementedException())
          
          member x.GetEnumerator(): System.Collections.Generic.IEnumerator<KeyValuePair<string, 'TV>> = 
              raise (System.NotImplementedException())
          
          member x.GetEnumerator(): System.Collections.IEnumerator = 
              raise (System.NotImplementedException())
}"""
    let symbolUse = getFirstSymbol 36 22 "Add" source
    let symbol = symbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
    let genericType = symbol.FullType.GenericArguments.[0]
    let genericSymbolUse = getFirstSymbol 5 21 "IDictionary" source
    let genericParams = (genericSymbolUse.Symbol :?> FSharpEntity).GenericParameters
    let context = genericSymbolUse.DisplayContext
    let specificSymbolUse = getFirstSymbol 3 5 "x" source
    let specificType = (specificSymbolUse.Symbol :?> FSharpMemberOrFunctionOrValue).FullType
    let specificParams = specificType.GenericArguments
    let instantiatedType = genericType.Instantiate(Seq.zip genericParams specificParams |> Seq.toList)
    instantiatedType.Format context |> assertEqual (specificType.Format context)

[<Test>]
let ``should instantiate types on a single entity``() =
    let source =
        """
open System.Collections.Generic
let specificDictionary =
    { new IDictionary<string, int> with
          member x.get_Item(key: string): int = 
              raise (System.NotImplementedException())
            
          member x.set_Item(key: string, value: int): unit = 
              raise (System.NotImplementedException())
            
          member x.get_Keys(): System.Collections.Generic.ICollection<string> = 
              raise (System.NotImplementedException())
            
          member x.get_Values(): System.Collections.Generic.ICollection<int> = 
              raise (System.NotImplementedException())
            
          member x.ContainsKey(key: string): bool = 
              raise (System.NotImplementedException())
            
          member x.Add(key: string, value: int): unit = 
              raise (System.NotImplementedException())
            
          member x.Remove(key: string): bool = 
              raise (System.NotImplementedException())
            
          member x.TryGetValue(key: string, value: byref<int>): bool = 
              raise (System.NotImplementedException())
            
          member x.get_Count(): int = 
              raise (System.NotImplementedException())
            
          member x.get_IsReadOnly(): bool = 
              raise (System.NotImplementedException())
            
          member x.Add(item: KeyValuePair<string, int>): unit = 
              raise (System.NotImplementedException())
            
          member x.Clear(): unit = 
              raise (System.NotImplementedException())
            
          member x.Contains(item: KeyValuePair<string, int>): bool = 
              raise (System.NotImplementedException())
            
          member x.CopyTo(array: KeyValuePair<string, int> [], arrayIndex: int): unit = 
              raise (System.NotImplementedException())
            
          member x.Remove(item: KeyValuePair<string, int>): bool = 
              raise (System.NotImplementedException())
            
          member x.GetEnumerator(): System.Collections.Generic.IEnumerator<KeyValuePair<string, int>> = 
              raise (System.NotImplementedException())
            
          member x.GetEnumerator(): System.Collections.IEnumerator = 
              raise (System.NotImplementedException())
}"""

    let symbolUse = getFirstSymbol 4 34 "IDictionary" source
    let context = symbolUse.DisplayContext
    let symbol = symbolUse.Symbol :?> FSharpEntity
    let entity = symbol.DeclaredInterfaces.[0].TypeDefinition
    let genericParams = entity.GenericParameters
    let specificParams = symbol.DeclaredInterfaces.[0].GenericArguments
    let currentMember = entity.MembersFunctionsAndValues.[5]
    let genericType = currentMember.FullType
    let instantiatedType = genericType.Instantiate(Seq.zip genericParams specificParams |> Seq.toList)
    instantiatedType.Format context |> assertEqual "KeyValuePair<'TKey,'TValue> [] * int -> unit"