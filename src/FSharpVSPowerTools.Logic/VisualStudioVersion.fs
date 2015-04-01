namespace FSharpVSPowerTools.ProjectSystem

open System
open System.ComponentModel.Composition

type VisualStudioVersion = 
    | Unknown = 0
    | VS2012 = 11
    | VS2013 = 12
    | VS2015 = 14

type IMinimalVisualStudioVersionMetadata =
    abstract Version: VisualStudioVersion with get

[<MetadataAttribute>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Interface, AllowMultiple = false)>]
type ExportWithMinimalVisualStudioVersionAttribute(contractType: Type) =
    inherit ExportAttribute(contractType)
    member val Version = VisualStudioVersion.Unknown with get,set

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VisualStudioVersion =    
    let fromDTEVersion(s: string) =
        if String.IsNullOrEmpty s then 
            VisualStudioVersion.Unknown
        else
            let parts = s.Split('.')
            if parts.Length = 0 then 
                VisualStudioVersion.Unknown
            else
                match parts.[0] with
                | "11" -> VisualStudioVersion.VS2012
                | "12" -> VisualStudioVersion.VS2013
                | "14" -> VisualStudioVersion.VS2015
                | _ -> VisualStudioVersion.Unknown
    
    let matches (currentVersion: VisualStudioVersion) (featureVersion: VisualStudioVersion) = 
        currentVersion = featureVersion

    let toString(version: VisualStudioVersion) =
        version.ToString("d")

