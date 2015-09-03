namespace FSharpVSPowerTools.Linting

open System.Linq
open System.Windows
open System.Windows.Controls
open FsXaml

type LintOptionsControlProvider = XAML<"LintOptionsPageControl.xaml", true>
       
type LintOptionsPageControl() =
    inherit UserControlViewController<LintOptionsControlProvider>()

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
                        let selectedItems = control.SelectedItems |> Seq.cast<string> |> Seq.toList
                        if viewModel.RemoveManyCommand.CanExecute(selectedItems) then
                            viewModel.RemoveManyCommand.Execute(selectedItems)
                    | _ -> ()
                | _ -> ())
    
    override this.OnLoaded control = 
        control.HintsListView.SelectionChanged.AddHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.AddHandler(onKeydown)

        control.IgnoreFilesListView.SelectionChanged.AddHandler(selectionChanged)
        control.HintsListView.KeyDown.AddHandler(onKeydown)

    override this.OnUnloaded control =
        control.HintsListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.RemoveHandler(onKeydown)

        control.IgnoreFilesListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.HintsListView.KeyDown.RemoveHandler(onKeydown)