namespace FSharpVSPowerTools.Navigation

open System
open System.IO
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
module Constants =
    let [<Literal>] FindReferencesResults = 0x11223344u

type FSharpLibraryNode(name: string, serviceProvider: System.IServiceProvider, ?symbolUse: FSharpSymbolUse) =
    inherit LibraryNode(name)
    do
        base.CanGoToSource <- true

    let navigationData =
        match symbolUse with
        | Some symbolUse ->
            let mutable hierarchy = Unchecked.defaultof<_>
            let mutable itemId = Unchecked.defaultof<_>
            let mutable windowFrame = Unchecked.defaultof<_>
            let isOpened = 
                VsShellUtilities.IsDocumentOpen(
                    serviceProvider, 
                    symbolUse.FileName, 
                    Constants.LogicalViewTextGuid,
                    &hierarchy,
                    &itemId,
                    &windowFrame)
            let canShow = 
                if isOpened then true
                else
                    try
                        VsShellUtilities.OpenDocument(
                            serviceProvider, 
                            symbolUse.FileName, 
                            Constants.LogicalViewTextGuid, 
                            &hierarchy,
                            &itemId,
                            &windowFrame)
                        // Hide it until actually navigating to the window
                        windowFrame.Hide() |> ensureSucceeded
                        true
                    with _ -> false
            if canShow then
                let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
                let mutable vsTextBuffer = Unchecked.defaultof<_>
                vsTextView.GetBuffer(&vsTextBuffer) |> ensureSucceeded
                Some (windowFrame, vsTextManager, vsTextBuffer)
            else None
        | _ -> None
    
    let textData =
        match navigationData, symbolUse with
        | Some(_, _, vsTextBuffer), Some symbolUse ->
            let range = symbolUse.RangeAlternate
            let prefix = sprintf "%s - (%i, %i) : " (Path.GetFullPathSafe(symbolUse.FileName)) 
                            range.StartLine range.StartColumn
            let line = range.StartLine-1
            let (_, lineLength) = vsTextBuffer.GetLengthOfLine(line)
            let (_, lineStr) = vsTextBuffer.GetLineText(line, 0, line, lineLength)
            // Trimming for display purpose, but we should not touch the trailing spaces.
            let content = lineStr.TrimStart()
            let numOfWhitespaces = lineStr.Length - content.Length
            let (_, rangeText) = vsTextBuffer.GetLineText(range.StartLine-1, range.StartColumn, range.EndLine-1, range.EndColumn)
            // We use name since ranges might not be correct on fully qualified symbols
            let offset = max 0 (rangeText.Length - name.Length)
            // Get the index of symbol in the trimmed text
            let highlightStart = prefix.Length + range.StartColumn + offset - numOfWhitespaces
            let highlightLength = name.Length
            let text = prefix + content.Trim()
            Some (highlightStart, highlightLength, text)
        | _ -> None

    override x.DisplayData: VSTREEDISPLAYDATA = 
        let glyphType =
            match symbolUse with
            | Some symbolUse when symbolUse.IsFromDefinition ->
                uint16 StandardGlyphGroup.GlyphLibrary
            | _ ->
                uint16 StandardGlyphGroup.GlyphForwardType

        match textData with
        | Some(start, length, _) ->
            // Highlight symbol in current line and show different icons for definitions and uses
            VSTREEDISPLAYDATA(
                Image = glyphType,
                SelectedImage = glyphType,
                State = uint32 _VSTREEDISPLAYSTATE.TDS_FORCESELECT,
                ForceSelectStart = uint16 start,
                ForceSelectLength = uint16 length)
        | None -> 
            VSTREEDISPLAYDATA(
                Image = glyphType,
                SelectedImage = glyphType)

    override x.CategoryField(_category: LIB_CATEGORY) =
        uint32 LibraryNodeType.None

    override x.GetTextWithOwnership(tto: VSTREETEXTOPTIONS) = 
        match tto with
        | VSTREETEXTOPTIONS.TTO_DEFAULT ->
            // Although symbols have been sorted outside,
            // returned text determines the order of results in 'Find Symbol Results' window
            match textData with
            | Some(_, _, text) -> text
            | None -> String.Empty
        | _ -> String.Empty

    override x.GotoSource(_gotoType: VSOBJGOTOSRCTYPE) = 
        match navigationData, symbolUse with
        | Some(windowFrame, vsTextManager, vsTextBuffer), Some symbolUse ->
            // Only show windows when going to source
            windowFrame.Show() |> ensureSucceeded
            let range = symbolUse.RangeAlternate
            let (_, rangeText) = vsTextBuffer.GetLineText(range.StartLine-1, range.StartColumn, range.EndLine-1, range.EndColumn)
            // FCS may return ranges for fully-qualified symbols
            let offset = max 0 (rangeText.Length - name.Length)
            let (startRow, startCol, endRow, endCol) = (range.StartLine-1, range.StartColumn + offset, range.EndLine-1, range.EndColumn)
            vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid, 
                startRow, startCol, endRow, endCol)
            |> ensureSucceeded
        | _ -> ()

    // This class implement 'IVSNavInfo' in order to be attached in search criteria
    // The interface implementation isn't used for another purpose for now
    interface IVsNavInfo with
        member x.GetLibGuid(_pGuid: byref<Guid>): int = 
            raise (NotImplementedException())
              
        member x.GetSymbolType(_pdwType: byref<uint32>): int = 
            raise (NotImplementedException())
              
        member x.EnumPresentationNodes(_dwFlags: uint32, _ppEnum: byref<IVsEnumNavInfoNodes>): int = 
            raise (NotImplementedException())
              
        member x.EnumCanonicalNodes(ppEnum: byref<IVsEnumNavInfoNodes>): int = 
            raise (NotImplementedException())

type FSharpLibrary(guid: Guid) =
    let mutable capabilities: _LIB_FLAGS2 = Unchecked.defaultof<_>
    let mutable root = LibraryNode("", LibraryNodeType.Package)

    member x.LibraryCapabilities
        with get () = capabilities
        and set v = capabilities <- v
    
    interface IVsSimpleLibrary2 with
        member x.GetSupportedCategoryFields2(_category: int, pgrfCatField: byref<uint32>): int = 
            pgrfCatField <- uint32 _LIB_CATEGORY2.LC_HIERARCHYTYPE ||| uint32 _LIB_CATEGORY2.LC_PHYSICALCONTAINERTYPE
            VSConstants.S_OK
              
        member x.GetList2(listType: uint32, _flags: uint32, pobSrch: VSOBSEARCHCRITERIA2 [], 
                          ppIVsSimpleObjectList2: byref<IVsSimpleObjectList2>): int = 
            if pobSrch <> null && not (Array.isEmpty pobSrch) && pobSrch.[0].dwCustom = Constants.FindReferencesResults then
                // Filter duplicated results
                // Reference at http://social.msdn.microsoft.com/Forums/en-US/267c38d3-1732-465c-82cd-c36fceb486be/find-symbol-result-window-got-double-results-when-i-search-object-use-ivssimplelibrary2?forum=vsx
                match enum<_>(int listType) with
                |  _LIB_LISTTYPE.LLT_NAMESPACES ->
                    ppIVsSimpleObjectList2 <- pobSrch.[0].pIVsNavInfo :?> FSharpLibraryNode
                    VSConstants.S_OK
                | _ ->
                    ppIVsSimpleObjectList2 <- null
                    VSConstants.E_FAIL
            else
                // We don't support call hierarchy, object browser and the like
                ppIVsSimpleObjectList2 <- null
                VSConstants.E_NOTIMPL
              
        member x.GetLibFlags2(pgrfFlags: byref<uint32>): int = 
            pgrfFlags <- uint32 x.LibraryCapabilities
            VSConstants.S_OK
              
        member x.UpdateCounter(pCurUpdate: byref<uint32>): int = 
            (root :> IVsSimpleObjectList2).UpdateCounter(&pCurUpdate)
              
        member x.GetGuid(pguidLib: byref<Guid>): int = 
            pguidLib <- guid
            VSConstants.S_OK
              
        member x.GetSeparatorStringWithOwnership(pbstrSeparator: byref<string>): int = 
            pbstrSeparator <- "."
            VSConstants.S_OK
              
        member x.LoadState(_pIStream: IStream, _lptType: LIB_PERSISTTYPE): int = 
            VSConstants.S_OK
              
        member x.SaveState(_pIStream: IStream, _lptType: LIB_PERSISTTYPE): int = 
            VSConstants.S_OK
              
        member x.GetBrowseContainersForHierarchy(_pHierarchy: IVsHierarchy, _celt: uint32, 
                                                 _rgBrowseContainers: VSBROWSECONTAINER [], _pcActual: uint32 []): int = 
            VSConstants.E_NOTIMPL
              
        member x.AddBrowseContainer(_pcdComponent: VSCOMPONENTSELECTORDATA [], _pgrfOptions: byref<uint32>, 
                                    pbstrComponentAdded: byref<string>): int = 
            pbstrComponentAdded <- null
            VSConstants.E_NOTIMPL
              
        member x.RemoveBrowseContainer(_dwReserved: uint32, _pszLibName: string): int = 
            VSConstants.E_NOTIMPL
              
        member x.CreateNavInfo(_rgSymbolNodes: SYMBOL_DESCRIPTION_NODE [], _ulcNodes: uint32, 
                               ppNavInfo: byref<IVsNavInfo>): int = 
            ppNavInfo <- null
            VSConstants.E_NOTIMPL

