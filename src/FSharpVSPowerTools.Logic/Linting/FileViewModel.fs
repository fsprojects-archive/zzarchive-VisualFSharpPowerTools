namespace FSharpVSPowerTools.Linting

type FileViewModel() =
    member val Name:string = null with get, set

    member val Files:FileViewModel seq = null with get, set