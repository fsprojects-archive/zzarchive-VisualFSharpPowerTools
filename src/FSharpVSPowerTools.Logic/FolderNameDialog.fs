﻿namespace FSharpVSPowerTools.Folders

open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding

type NewFolderNameDialog = FsXaml.XAML<"FolderNameDialog.xaml">

[<NoEquality; NoComparison>]
type NewFolderNameDialogResources =
    { WindowTitle : string
      FolderNames : Set<string> }

type NewFolderNameDialogModel(resources :NewFolderNameDialogResources) =

    let mutable name = ""

    let validate (name :string) =
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 Resource.validatingEmptyName
        | a when resources.FolderNames.Contains a -> Choice2Of2 Resource.validationFolderWithGivenNameAlreadyExists
        | _ -> Choice1Of2()

    let mutable validationResult = validate name

    let errorsChanged = Event<_,_>() 

    member x.Result = validationResult

    member x.WindowTitle = resources.WindowTitle

    member x.Name
        with get() = name
        and set (v :string) =
            name <- v
            validationResult <- validate name
            errorsChanged.Trigger(x :> obj, DataErrorsChangedEventArgs("Name"))

    interface INotifyDataErrorInfo with
        member x.GetErrors _ = 
            match validationResult with
            | Choice2Of2 e -> [e]
            | _ -> []
            :> Collections.IEnumerable

        member x.HasErrors = match validationResult with Choice2Of2 _ -> true | _ -> false
        [<CLIEvent>]
        member x.ErrorsChanged = errorsChanged.Publish
        
