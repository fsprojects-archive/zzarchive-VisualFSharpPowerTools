// Borrow from https://github.com/fsharp/fsharpbinding/blob/master/FSharp.AutoComplete/ProjectParser.fs
// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FSharpVSPowerTools.Tests

// Disable warnings for obsolete MSBuild.
// Mono doesn't support the latest API.
#nowarn "0044"

open System
open System.IO
open Microsoft.Build.BuildEngine
open FSharpVSPowerTools

module ProjectParser = 
    type ProjectResolver = 
        { Project: Project
          LoadTime: DateTime }
    
    let load (uri: string): Option<ProjectResolver> = 
        let p = new Project()
        try 
            p.Load(uri)
            Some { Project = p
                   LoadTime = DateTime.Now }
        with :? InvalidProjectFileException -> None
    
    let getFileName (p: ProjectResolver): string = p.Project.FullFileName
    
    let getOutputPath (p: ProjectResolver): string = 
        seq { 
            for item in p.Project.GetEvaluatedItemsByName("BuiltProjectOutputGroupKeyOutput") do
                yield item.FinalItemSpec
        }
        |> Seq.tryHead
        |> Option.getOrElse String.Empty
    
    let getLoadTime (p: ProjectResolver): DateTime = 
        p.LoadTime

    let getDirectory (p: ProjectResolver): string = 
        Path.GetDirectoryName p.Project.FullFileName
    
    let getFiles (p: ProjectResolver): string [] = 
        let fs = p.Project.GetEvaluatedItemsByName("Compile")
        let dir = getDirectory p
        [| for f in fs do
               yield Path.Combine(dir, f.FinalItemSpec) |]
    
    let getFrameworkVersion (p: ProjectResolver) = 
        match p.Project.GetEvaluatedProperty("TargetFrameworkVersion") with
        | "v2.0" -> FSharpTargetFramework.NET_2_0
        | "v3.0" -> FSharpTargetFramework.NET_3_0
        | "v3.5" -> FSharpTargetFramework.NET_3_5
        | "v4.0" -> FSharpTargetFramework.NET_4_0
        | "v4.5" -> FSharpTargetFramework.NET_4_5
        | _ -> FSharpTargetFramework.NET_4_5
    
    let getProjectReferences (p: ProjectResolver) = 
        let dir = getDirectory p
        [| for i in p.Project.GetEvaluatedItemsByName("ProjectReference") do
               yield Path.Combine(dir, i.FinalItemSpec) |]
    
    // We really want the output of ResolveAssemblyReferences. However, this
    // needs as input ChildProjectReferences, which is populated by
    // ResolveProjectReferences. For some reason ResolveAssemblyReferences
    // does not depend on ResolveProjectReferences, so if we don't run it first
    // then we won't get the dll files for imported projects in this list.
    // We can therefore build ResolveReferences, which depends on both of them,
    // or [|"ResolveProjectReferences";"ResolveAssemblyReferences"|]. These seem
    // to be equivalent. See Microsoft.Common.targets if you want more info.
    let getReferences (p: ProjectResolver): string [] = 
        ignore <| p.Project.Build([| "ResolveReferences" |])
        [| for i in p.Project.GetEvaluatedItemsByName("ReferencePath") do
               yield "-r:" + i.FinalItemSpec |]
    
    let getOptions (p: ProjectResolver): string [] = 
        let getProp s = p.Project.GetEvaluatedProperty s
        
        let split (s: string) (cs: char []) = 
            if s = null then [||]
            else s.Split(cs, StringSplitOptions.RemoveEmptyEntries)
        
        let getBool (s: string) = 
            match Boolean.TryParse s with
            | (true, result) -> result
            | (false, _) -> false
        
        let optimize = getProp "Optimize" |> getBool
        let tailCalls = getProp "Tailcalls" |> getBool
        let debugSymbols = getProp "DebugSymbols" |> getBool
        let defines = split (getProp "DefineConstants") [| ';'; ','; ' ' |]
        let otherFlags = getProp "OtherFlags"
        
        let otherFlags = 
            if otherFlags = null then [||]
            else split otherFlags [| ' ' |]

        [| yield "--noframework"
           for symbol in defines do
               yield "--define:" + symbol
           yield if debugSymbols then "--debug+"
                 else "--debug-"
           yield if optimize then "--optimize+"
                 else "--optimize-"
           yield if tailCalls then "--tailcalls+"
                 else "--tailcalls-"
           yield! otherFlags
           yield! getReferences p |]
