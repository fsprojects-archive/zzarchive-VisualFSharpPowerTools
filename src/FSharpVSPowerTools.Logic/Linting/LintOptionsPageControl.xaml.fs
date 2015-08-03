namespace FSharpVSPowerTools.Linting

open System.Windows
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
    
    override this.OnLoaded control = 
        let handler = RoutedPropertyChangedEventHandler(rulesTreeViewSelectedItemChanged)

        control.RulesTreeView.SelectedItemChanged.AddHandler(handler)

        control.DataContext <- viewModel

        setParentRules viewModel.Rules