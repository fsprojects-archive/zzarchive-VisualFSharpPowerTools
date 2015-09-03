namespace FSharpVSPowerTools.Linting

type FileViewModel(name:string, files:FileViewModel seq, hasConfigFile:bool) =
    member __.Name = name

    member __.Files = files

    member __.HasConfigFile = hasConfigFile