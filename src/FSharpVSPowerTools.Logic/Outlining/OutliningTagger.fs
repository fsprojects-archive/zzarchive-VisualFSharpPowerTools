namespace FSharpVSPowerTools.Outlining

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.ProjectSystem

type OutliningTagger(buffer: ITextBuffer, languageService: VSLanguageService) = 

    let tagsChanged = Event<_,_>()

    interface ITagger<IOutliningRegionTag> with

        member __.GetTags(_) = Seq.empty
        [<CLIEvent>]
        member x.TagsChanged : IEvent<_,_> = tagsChanged.Publish