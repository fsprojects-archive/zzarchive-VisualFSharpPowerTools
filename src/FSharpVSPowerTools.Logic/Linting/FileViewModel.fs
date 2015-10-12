﻿namespace FSharpVSPowerTools.Linting

open System.IO
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Configuration.Management
open FSharpVSPowerTools

type FileViewModel(name:string, path:Path, files:FileViewModel seq, hasConfigFile:bool) =
    member __.Name = name

    member __.Path = path

    member __.Files = files

    member __.HasConfigFile = hasConfigFile

    member __.GetDirectory() =
        let directorySeparator = Path.DirectorySeparatorChar.ToString()
        (String.concat directorySeparator path) + directorySeparator

    member this.GetFilePath() =
        this.GetDirectory() </> SettingsFileName