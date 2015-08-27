namespace FSharpVSPowerTools.Linting

type FileViewModel(name:string, files:FileViewModel seq) =
    member this.Name
        with get() = name

    member this.Files
        with get() = files