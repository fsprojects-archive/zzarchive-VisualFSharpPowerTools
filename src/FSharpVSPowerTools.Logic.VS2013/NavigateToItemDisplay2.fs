namespace FSharpVSPowerTools.Logic.VS2013

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Language.NavigateTo.Interfaces
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.TextManager.Interop
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

[<Export>]
type DocumentNavigator() =
    [<Import(typeof<SVsServiceProvider>); DefaultValue>]
    val mutable serviceProvider: IServiceProvider

    member internal x.NavigateTo(position: NavigateToItemExtraData) =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>

        let canShow = 
            VsShellUtilities.IsDocumentOpen(
                x.serviceProvider, position.FileName, Constants.guidLogicalTextView, &hierarchy, &itemId, &windowFrame) ||
                // TODO: track the project that contains document and open document in project context
            (VsShellUtilities.TryOpenDocument(
                x.serviceProvider, position.FileName, Constants.guidLogicalTextView, &hierarchy, &itemId, &windowFrame)
             |> ErrorHandler.Succeeded)

        if canShow then
            windowFrame.Show() |> ensureSucceeded
            let vsTextView = VsShellUtilities.GetTextView(windowFrame)
            let vsTextManager = x.serviceProvider.GetService(typeof<SVsTextManager>) :?> IVsTextManager
            let mutable vsTextBuffer = Unchecked.defaultof<_>
            vsTextView.GetBuffer(&vsTextBuffer) |> ensureSucceeded
            vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.guidLogicalTextView, 
                  position.Span.Start.Row, position.Span.Start.Col, position.Span.End.Row, position.Span.End.Col)
            |> ensureSucceeded

    member internal __.GetProvisionalViewingStatus(position: NavigateToItemExtraData) =
        VsShellUtilities.GetProvisionalViewingStatus position.FileName |> int

    member internal x.PreviewItem(position: NavigateToItemExtraData) = x.NavigateTo position

type NavigateToItemDisplay(item: NavigateToItem, icon, navigator: DocumentNavigator) =
        let extraData: NavigateToItemExtraData = unbox item.Tag
        interface INavigateToItemDisplay2 with
            member __.Name = item.Name
            member __.Glyph = icon
            member __.AdditionalInformation = extraData.FileName
            member __.Description = extraData.Description
            member __.DescriptionItems = Constants.EmptyReadOnlyCollection
            member __.NavigateTo() = navigator.NavigateTo(extraData)
            member __.GetProvisionalViewingStatus() = navigator.GetProvisionalViewingStatus(extraData)
            member __.PreviewItem() = navigator.PreviewItem extraData

[<ExportWithMinimalVisualStudioVersion(typeof<INavigateToItemDisplayFactory>, Version = VisualStudioVersion.VS2013)>]
type VS2013NavigateToItemDisplayFactory() =
    [<Import; DefaultValue>]
    val mutable navigator: DocumentNavigator

    [<Import; DefaultValue>]
    val mutable iconCache: NavigationItemIconCache
    
    interface INavigateToItemDisplayFactory with
        member x.CreateItemDisplay item = 
            let icon = x.iconCache.GetIconForNavigationItemKind item.Kind
            upcast NavigateToItemDisplay(item, icon, x.navigator)

