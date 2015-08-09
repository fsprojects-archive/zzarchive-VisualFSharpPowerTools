namespace FSharpVSPowerTools.Linting

open System.Linq
open System.Windows
open System.Windows.Controls
open FsXaml

type LintOptionsControlProvider = XAML<"LintOptionsPageControl.xaml", true>
       
type LintOptionsPageControl() =
    inherit UserControlViewController<LintOptionsControlProvider>()

    let config = FSharpLint.Framework.Configuration.defaultConfiguration

    let viewModel = OptionsViewModel(config,
                                     Files = [FileViewModel(Name = "SomeFile")])

    let setParentRules (rules:RuleViewModel seq) =
        if rules <> null then
            for rule in rules do
                if rule.Rules <> null then
                    for child in rule.Rules do
                        child.ParentRule <- rule

    let rulesTreeViewSelectedItemChanged _ (e:RoutedPropertyChangedEventArgs<obj>) =
        viewModel.SelectedRule <- e.NewValue :?> RuleViewModel

    let selectionChanged (control:ListView) =
        SelectionChangedEventHandler(fun _ _ -> control.ScrollIntoView(control.SelectedItem))

    let onKeydown (control:ListView) (command:FSharp.ViewModule.INotifyCommand) =
        Input.KeyEventHandler(fun _ e -> 
            if e.Key = Input.Key.Delete then
                let selectedItem = control.SelectedItem

                if command.CanExecute(selectedItem) then
                    command.Execute(selectedItem))
    
    override this.OnLoaded control = 
        let handler = RoutedPropertyChangedEventHandler(rulesTreeViewSelectedItemChanged)

        control.RulesTreeView.SelectedItemChanged.AddHandler(handler)

        control.HintsListView.SelectionChanged.AddHandler(selectionChanged control.HintsListView)

        control.HintsListView.KeyDown.AddHandler(onKeydown control.HintsListView viewModel.Hints.RemoveHintCommand)

        control.IgnoreFilesListView.SelectionChanged.AddHandler(selectionChanged control.IgnoreFilesListView)

        control.IgnoreFilesListView.KeyDown.AddHandler(onKeydown control.IgnoreFilesListView viewModel.RemoveIgnoreFileCommand)

        control.DataContext <- viewModel

        setParentRules viewModel.Rules