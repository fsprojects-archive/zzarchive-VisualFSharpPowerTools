namespace FSharpVSPowerTools.ProjectSystem.VS2013

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Language.NavigateTo.Interfaces
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.TextManager.Interop
open FSharpVSPowerTools.ProjectSystem

type
    [<ExportWithMinimalVisualStudioVersion(typeof<INavigateToItemDisplayFactory>, Version = VisualStudioVersion.VS2013)>]
    VS2013NavigateToItemDisplayFactory() =

        [<Import; DefaultValue>]
        val mutable navigator: DocumentNavigator
        [<Import; DefaultValue>]
        val mutable iconCache: NavigationItemIconCache
        
        interface INavigateToItemDisplayFactory with
            member x.CreateItemDisplay(item) = 
                let icon = x.iconCache.GetIconForNavigationItemKind(item.Kind)
                NavigateToItemDisplay(item, icon, x.navigator) :> _
and
    NavigateToItemDisplay(item: NavigateToItem, icon, navigator: DocumentNavigator) =
        let extraData: NavigateToItemExtraData = unbox item.Tag
        interface INavigateToItemDisplay2 with
            member x.Name = item.Name
            member x.Glyph = icon
            member x.AdditionalInformation = extraData.FileName
            member x.Description = extraData.Description
            member x.DescriptionItems = Constants.EmptyReadOnlyCollection
            member x.NavigateTo() = navigator.NavigateTo(extraData)
            member x.GetProvisionalViewingStatus() = navigator.GetProvisionalViewingStatus(extraData)
            member x.PreviewItem() = navigator.PreviewItem(extraData)
and
    [<Export>]
    DocumentNavigator() =

        [<Import(typeof<Microsoft.VisualStudio.Shell.SVsServiceProvider>); DefaultValue>]
        val mutable serviceProvider: IServiceProvider

        member internal x.NavigateTo(position: NavigateToItemExtraData) =
            let mutable hierarchy = Unchecked.defaultof<_>
            let mutable itemId = Unchecked.defaultof<_>
            let mutable windowFrame = Unchecked.defaultof<_>

            let isOpened = 
                VsShellUtilities.IsDocumentOpen(
                    x.serviceProvider, 
                    position.FileName, 
                    Constants.LogicalViewTextGuid,
                    &hierarchy,
                    &itemId,
                    &windowFrame)
            let canShow = 
                if isOpened then true
                else
                    // TODO: track the project that contains document and open document in project context
                    let opened = 
                        VsShellUtilities.TryOpenDocument(
                            x.serviceProvider, 
                            position.FileName, 
                            Constants.LogicalViewTextGuid, 
                            &hierarchy,
                            &itemId,
                            &windowFrame)
                    ErrorHandler.Succeeded(opened)
            if canShow then
                windowFrame.Show()
                |> ensureSucceeded

                let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                let vsTextManager = x.serviceProvider.GetService(typeof<SVsTextManager>) :?> IVsTextManager
                let mutable vsTextBuffer = Unchecked.defaultof<_>
                vsTextView.GetBuffer(&vsTextBuffer)
                |> ensureSucceeded

                let (startRow, startCol), (endRow, endCol) = position.Span
                vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, startRow, startCol, endRow, endCol)
                |> ensureSucceeded
        member internal x.GetProvisionalViewingStatus(position: NavigateToItemExtraData) =
            int (VsShellUtilities.GetProvisionalViewingStatus(position.FileName))
        member internal x.PreviewItem(position: NavigateToItemExtraData) =
            x.NavigateTo(position)
