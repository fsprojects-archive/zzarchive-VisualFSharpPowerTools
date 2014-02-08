namespace FSharpVSPowerTools.ProjectSystem

type internal VSLanguageService private () =
    // TODO: we should reparse the stale document and cache it
    static let instance = FSharp.CompilerBinding.LanguageService(fun _ -> ())
    static member Instance = instance
