namespace FSharpVSPowerTools.ProjectSystem

[<RequireQualifiedAccess>]
module VSLanguageService =
    // TODO: we should reparse the stale document and cache it
    let Instance = FSharp.CompilerBinding.LanguageService(fun _ -> ())
