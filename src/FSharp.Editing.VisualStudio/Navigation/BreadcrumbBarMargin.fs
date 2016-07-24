namespace FSharp.Editing.VisualStudio.Navigation
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text

open FSharp.Editing
open FSharp.Editing.VisualStudio

type BreadcrumbBarVisual = FsXaml.XAML< @"Gui/BreadcrumbBar.xaml">

type BreadcrumbBarMargin(view: IWpfTextView) =
    let visual = BreadcrumbBarVisual()

    let tryBindLeftPadding =
        // Climbing the visual tree here, slight hack.
        // Potential for improvement but probably not an issue.
        let parent = VisualTreeHelper.GetParent view.VisualElement

        let leftMargin =
            if (parent :? Grid) then 
                let grid = parent :?> Grid
                grid.Children
                |> Seq.ofType<IWpfTextViewMargin>
                |> Seq.tryFind (fun m -> m.GetType().Name = "LeftMargin") // LeftMargin is internal I think
            else
                None

        let actualWidthToLeftPaddingConverter = 
            { new IValueConverter with 
                member x.Convert(value, _, _, _) =
                    let left =
                        match value with
                        | :? float as d -> d
                        | _ -> 0.0
                    Thickness(left, 0., 0., 0.) |> box
                member x.ConvertBack(_, _, _, _) =
                    failwithf "Only for one way bindings"
             }

        let bindLeftPadding (leftMargin: IWpfTextViewMargin) =
            let binding = Binding("ActualWidth") 
            binding.Source <- leftMargin.VisualElement
            binding.Mode <- BindingMode.OneWay
            binding.Converter <- actualWidthToLeftPaddingConverter
            BindingOperations.SetBinding(visual, BreadcrumbBarMargin.LeftPaddingProperty, binding) |> ignore

        match leftMargin with
            | Some x -> bindLeftPadding x
            | None -> ()

    do tryBindLeftPadding

    static member LeftPaddingProperty = DependencyProperty.RegisterAttached("LeftPadding", typeof<Thickness>, typeof<BreadcrumbBarMargin>, PropertyMetadata(Thickness(0.0, 0.0, 0.0, 0.0)))
    
    static member SetLeftPadding (element: UserControl, value : Thickness) = 
        element.SetValue(BreadcrumbBarMargin.LeftPaddingProperty, value)
    
    static member GetLeftPadding (element: UserControl) : Thickness = 
        element.GetValue(BreadcrumbBarMargin.LeftPaddingProperty) :?> _

    interface IWpfTextViewMargin with
        member __.VisualElement = upcast visual
        member __.MarginSize = visual.ActualHeight
        member __.Enabled = true

        member x.GetTextViewMargin name =
            match name with
            | Constants.BreadcrumbBarMarginName -> upcast x
            | _ -> Unchecked.defaultof<_>

    interface IDisposable with
        member __.Dispose() = ()

