namespace FSharp.Editing.VisualStudio.Linting

open System.Windows
open System.Windows.Controls
open FsXaml
open FSharpLint.Framework.Configuration

type LintOptionsControlProviderBase = XAML< @"Gui/LintOptionsPageControl.xaml">
       
type LintOptionsControlProvider() as control =
    inherit LintOptionsControlProviderBase()

    let selectionChanged =
        SelectionChangedEventHandler(fun control _ -> 
            match control with
            | :? ListView as control ->
                control.ScrollIntoView(control.SelectedItem)
            | _ -> ())

    let onKeydown =
        Input.KeyEventHandler(fun control e -> 
            if e.Key = Input.Key.Delete then
                match control with 
                | :? ListView as control ->
                    match control.DataContext with
                    | :? IRemovable as viewModel ->
                        let selectedItems = 
                            control.SelectedItems 
                            |> Seq.cast<Hint>
                            |> Seq.toList

                        if viewModel.RemoveManyCommand.CanExecute(selectedItems) then
                            viewModel.RemoveManyCommand.Execute(selectedItems)
                    | _ -> ()
                | _ -> ())

    let selectedTreeViewItemChanged handleChange =
        RoutedPropertyChangedEventHandler(fun control _ -> 
            match control with
            | :? TreeView as control ->
                handleChange (control.DataContext, control.SelectedItem)
            | _ -> ())

    let selectedFileChanged =
        selectedTreeViewItemChanged 
            (function
            | (:? OptionsViewModel as viewModel), (:? FileViewModel as selected) ->
                viewModel.SelectedFile <- selected
            | _ -> ())

    let selectedRuleChanged =
        selectedTreeViewItemChanged 
            (function
            | (:? OptionsViewModel as viewModel), (:? RuleViewModel as selected) ->
                viewModel.SelectedRule <- Some(selected)
            | _ -> ())
    do
        control.Loaded.Add(fun _ ->      
            control.HintsListView.SelectionChanged.AddHandler(selectionChanged)
            control.IgnoreFilesListView.KeyDown.AddHandler(onKeydown)

            control.IgnoreFilesListView.SelectionChanged.AddHandler(selectionChanged)
            control.HintsListView.KeyDown.AddHandler(onKeydown)

            control.FilesTreeView.SelectedItemChanged.AddHandler(selectedFileChanged)

            control.RulesTreeView.SelectedItemChanged.AddHandler(selectedRuleChanged))

    
        control.Unloaded.Add(fun _ ->
            control.HintsListView.SelectionChanged.RemoveHandler(selectionChanged)
            control.IgnoreFilesListView.KeyDown.RemoveHandler(onKeydown)

            control.IgnoreFilesListView.SelectionChanged.RemoveHandler(selectionChanged)
            control.HintsListView.KeyDown.RemoveHandler(onKeydown)

            control.FilesTreeView.SelectedItemChanged.RemoveHandler(selectedFileChanged)

            control.RulesTreeView.SelectedItemChanged.RemoveHandler(selectedRuleChanged))