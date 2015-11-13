namespace FSharpVSPowerTools.Navigation

open System.ComponentModel.Composition
open System

type FileNavigableItems =
    { FilePath: string
      FileLastWriteTime: DateTime
      Items: NavigableItem[] }

[<Export>]
type NavigableItemCache() =
    member __.TryGet (_filePath: string): NavigableItem[] option = None
    member __.Set (_entry: FileNavigableItems) = ()
    member __.Remove (_filePath: string) = ()