namespace FSharpVSPowerTools.Linting

open System.IO
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Configuration.Management

type FileViewModel(name:string, path:Path, childFiles:FileViewModel seq, hasConfigFile:bool, isUserWideSettings:bool) =
    member __.Name = name

    member __.IsUserWideSettings = isUserWideSettings

    member __.Files = childFiles

    member __.HasConfigFile = hasConfigFile

    member __.GetDirectory() =
        let directorySeparator = Path.DirectorySeparatorChar.ToString()
        (String.concat directorySeparator path) + directorySeparator

    static member GlobalSettings(name, path) = FileViewModel(name, path, [], true, true)