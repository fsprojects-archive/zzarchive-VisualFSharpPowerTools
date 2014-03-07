// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Diagnostics
open Microsoft.Build.BuildEngine

#nowarn "0044"

module ProjectParser = 
    [<NoComparison>]
    type ProjectResolver = 
        { Project: Project
          LoadTime: DateTime }
    
    let load (uri: string): Option<ProjectResolver> = 
        let p = Project()
        try 
            p.Load(uri)
            Some { Project = p
                   LoadTime = DateTime.Now }
        with e -> 
            Debug.WriteLine(sprintf "[Project System] %O exception occurs." e)
            None
    
    let getFileName (p: ProjectResolver): string = p.Project.FullFileName
    let getLoadTime (p: ProjectResolver): DateTime = p.LoadTime
    let getDirectory (p: ProjectResolver): string = IO.Path.GetDirectoryName p.Project.FullFileName
    
    let getFiles (p: ProjectResolver): string [] = 
        let fs = p.Project.GetEvaluatedItemsByName("Compile")
        let dir = getDirectory p
        [| for f in fs do
               yield IO.Path.Combine(dir, f.FinalItemSpec) |]
    
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
        [| for i in p.Project.GetEvaluatedItemsByName("ResolvedFiles") do
               yield "-r:" + i.FinalItemSpec |]
    
    let getOptionsWithoutReferences (p: ProjectResolver): string [] = 
        let getProp s = p.Project.GetEvaluatedProperty s
        
        let split (s: string) (cs: char []) = 
            if s = null then [||]
            else s.Split(cs, StringSplitOptions.RemoveEmptyEntries)
        
        let getBool (s: string) = 
            match (Boolean.TryParse s) with
            | (true, result) -> result
            | (false, _) -> false
        
        let optimize = getProp "Optimize" |> getBool
        let tailcalls = getProp "Tailcalls" |> getBool
        let debugSymbols = getProp "DebugSymbols" |> getBool
        let defines = split (getProp "DefineConstants") [| ';'; ','; ' ' |]
        let otherFlags = getProp "OtherFlags"
        
        let otherflags = 
            if otherFlags = null then [||]
            else split otherFlags [| ' ' |]
        [| yield "--noframework"
           for symbol in defines do
               yield "--define:" + symbol
           yield if debugSymbols then "--debug+"
                 else "--debug-"
           yield if optimize then "--optimize+"
                 else "--optimize-"
           yield if tailcalls then "--tailcalls+"
                 else "--tailcalls-"
           yield! otherflags |]
