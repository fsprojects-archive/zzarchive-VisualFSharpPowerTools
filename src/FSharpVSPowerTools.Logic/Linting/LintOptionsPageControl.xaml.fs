namespace FSharpVSPowerTools.Linting

open System.Linq
open System.Windows
open System.Windows.Controls
open FsXaml

type LintOptionsControlProvider = XAML<"LintOptionsPageControl.xaml", true>
       
type LintOptionsPageControl() =
    inherit UserControlViewController<LintOptionsControlProvider>()

    let config = FSharpLint.Framework.Configuration.defaultConfiguration

    let viewModel = OptionsViewModel(config, [FileViewModel("SomeFile", [])])

    let rulesTreeViewSelectedItemChanged =
        RoutedPropertyChangedEventHandler(fun _ (e:RoutedPropertyChangedEventArgs<_>) ->
            viewModel.SelectedRule <- Some(e.NewValue))

    let selectionChanged =
        SelectionChangedEventHandler(fun control _ -> 
            match control with
            | :? ListView as control ->
                control.ScrollIntoView(control.SelectedItem)
            | _ -> ())

    let onKeydown (command:FSharp.ViewModule.INotifyCommand) =
        Input.KeyEventHandler(fun control e -> 
            if e.Key = Input.Key.Delete then
                match control with 
                | :? ListView as control ->
                    let selectedItem = control.SelectedItem

                    if command.CanExecute(selectedItem) then
                        command.Execute(selectedItem)
                | _ -> ())

    let ignoreFilesOnKeyDown = onKeydown viewModel.RemoveIgnoreFileCommand

    let hintsOnKeyDown = onKeydown viewModel.Hints.RemoveHintCommand
    
    override this.OnLoaded control = 
        control.RulesTreeView.SelectedItemChanged.AddHandler(rulesTreeViewSelectedItemChanged)

        control.HintsListView.SelectionChanged.AddHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.AddHandler(ignoreFilesOnKeyDown)

        control.IgnoreFilesListView.SelectionChanged.AddHandler(selectionChanged)
        control.HintsListView.KeyDown.AddHandler(hintsOnKeyDown)

        control.DataContext <- viewModel

    override this.OnUnloaded control =
        control.RulesTreeView.SelectedItemChanged.RemoveHandler(rulesTreeViewSelectedItemChanged)

        control.HintsListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.RemoveHandler(ignoreFilesOnKeyDown)

        control.IgnoreFilesListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.HintsListView.KeyDown.RemoveHandler(hintsOnKeyDown)