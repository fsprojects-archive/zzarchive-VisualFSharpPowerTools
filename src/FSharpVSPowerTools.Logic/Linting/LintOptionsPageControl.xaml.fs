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

    let setParentRules (rules:RuleViewModel seq) =
        for rule in rules do
            for child in rule.Rules do
                child.ParentRule <- Some(rule)

    let rulesTreeViewSelectedItemChanged =
        RoutedPropertyChangedEventHandler(fun _ (e:RoutedPropertyChangedEventArgs<obj>) ->
            viewModel.SelectedRule <- Some(e.NewValue))

    let selectionChanged =
        SelectionChangedEventHandler(fun control _ -> 
            let control = control :?> ListView
            control.ScrollIntoView(control.SelectedItem))

    let onKeydown (command:FSharp.ViewModule.INotifyCommand) =
        Input.KeyEventHandler(fun control e -> 
            if e.Key = Input.Key.Delete then
                let control = control :?> ListView
                let selectedItem = control.SelectedItem

                if command.CanExecute(selectedItem) then
                    command.Execute(selectedItem))

    let ignoreFilesOnKeyDown = onKeydown viewModel.RemoveIgnoreFileCommand

    let hintsOnKeyDown = onKeydown viewModel.Hints.RemoveHintCommand
    
    override this.OnLoaded control = 
        control.RulesTreeView.SelectedItemChanged.AddHandler(rulesTreeViewSelectedItemChanged)

        control.HintsListView.SelectionChanged.AddHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.AddHandler(ignoreFilesOnKeyDown)

        control.IgnoreFilesListView.SelectionChanged.AddHandler(selectionChanged)
        control.HintsListView.KeyDown.AddHandler(hintsOnKeyDown)

        control.DataContext <- viewModel

        setParentRules viewModel.Rules

    override this.OnUnloaded control =
        control.RulesTreeView.SelectedItemChanged.RemoveHandler(rulesTreeViewSelectedItemChanged)

        control.HintsListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.IgnoreFilesListView.KeyDown.RemoveHandler(ignoreFilesOnKeyDown)

        control.IgnoreFilesListView.SelectionChanged.RemoveHandler(selectionChanged)
        control.HintsListView.KeyDown.RemoveHandler(hintsOnKeyDown)