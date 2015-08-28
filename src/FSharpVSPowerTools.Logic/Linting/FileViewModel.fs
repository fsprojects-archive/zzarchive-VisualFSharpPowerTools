namespace FSharpVSPowerTools.Linting

type FileViewModel(name:string, files:FileViewModel seq) =
    member __.Name = name

    member __.Files = files