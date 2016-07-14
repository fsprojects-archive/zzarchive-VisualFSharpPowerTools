namespace FSharp.Editing.VisualStudio.Symbol

open System
open System.IO
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.VisualStudio
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

type FSharpLibraryNode(name: string, serviceProvider: System.IServiceProvider, fileSystem: IFileSystem, ?symbolUse: FSharpSymbolUse) =
    inherit LibraryNode(name)
    do base.CanGoToSource <- true

    let navigationData = lazy (
        match symbolUse with 
        | Some symbolUse ->
            let mutable hierarchy = Unchecked.defaultof<_>
            let mutable itemId = Unchecked.defaultof<_>
            let mutable windowFrame = Unchecked.defaultof<_>
            let isOpened = 
                VsShellUtilities.IsDocumentOpen(
                    serviceProvider, 
                    symbolUse.FileName, 
                    Constants.guidLogicalTextView,
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
                            Constants.guidLogicalTextView, 
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
        | _ -> None)
    
    let readLine (reader: StreamReader) line = 
        let rec loop n =
            match n, reader.EndOfStream with
            | _, true -> None
            | _ when n = line -> Some (reader.ReadLine())
            | _ -> 
                reader.ReadLine() |> ignore
                loop (n + 1)
        loop 0

    let textData = maybe {
        let! symbolUse = symbolUse
        use! file = Option.attempt (fun _ -> fileSystem.FileStreamReadShim symbolUse.FileName)
        use reader = new StreamReader(file)
        let range = symbolUse.RangeAlternate
        let prefix = sprintf "%s - (%i, %i) : " (Path.GetFullPathSafe(symbolUse.FileName)) 
                        range.StartLine range.StartColumn
        let line = range.StartLine - 1
        let! lineStr = readLine reader line
        // Trimming for display purpose, but we should not touch the trailing spaces.
        let content = lineStr.TrimStart()
        let numOfWhitespaces = lineStr.Length - content.Length
        let rangeText = 
            // if it's a multi-line range, we take characters starting from StartColumn to the end of the start line
            if range.EndLine <> range.StartLine then
                lineStr.Substring(range.StartColumn)
            else
                lineStr.Substring(range.StartColumn, range.EndColumn - range.StartColumn)
        let offset = 
            if rangeText.LastIndexOf name > 0 then
                // Trim fully qualified symbols
                max 0 (rangeText.Length - name.Length)
            else 0
        // Get the index of symbol in the trimmed text
        let highlightStart = prefix.Length + range.StartColumn + offset - numOfWhitespaces
        let highlightLength = name.Length
        let text = prefix + content.Trim()
        return highlightStart, highlightLength, text
    }

    override __.DisplayData: VSTREEDISPLAYDATA = 
        let glyphType =
            match symbolUse with
            | Some symbolUse when symbolUse.IsFromDefinition -> StandardGlyphGroup.GlyphLibrary
            | _ -> StandardGlyphGroup.GlyphForwardType
            |> uint16

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

    override __.CategoryField(_category: LIB_CATEGORY) =
        uint32 LibraryNodeType.None

    override __.GetTextWithOwnership(tto: VSTREETEXTOPTIONS) = 
        match tto with
        | VSTREETEXTOPTIONS.TTO_DEFAULT ->
            // Although symbols have been sorted outside,
            // returned text determines the order of results in 'Find Symbol Results' window
            match textData with
            | Some(_, _, text) -> text
            | None -> 
                // This part supposes to turn up on unit testing only
                symbolUse
                |> Option.map (fun symbolUse ->
                    let range = symbolUse.RangeAlternate
                    sprintf "%s - (%i, %i) : %O" (Path.GetFullPathSafe(symbolUse.FileName)) 
                                range.StartLine range.StartColumn symbolUse.Symbol)
                |> Option.getOrElse String.Empty
        | _ -> String.Empty

    override __.GotoSource(_gotoType: VSOBJGOTOSRCTYPE) = 
        match navigationData.Value, symbolUse with
        | Some(windowFrame, vsTextManager, vsTextBuffer), Some symbolUse ->
            // Only show windows when going to source
            windowFrame.Show() |> ensureSucceeded
            let range = symbolUse.RangeAlternate
            let (_, rangeText) = vsTextBuffer.GetLineText(range.StartLine-1, range.StartColumn, range.EndLine-1, range.EndColumn)
            // FCS may return ranges for fully-qualified symbols
            let startOffset, endOffset = 
                let index = rangeText.LastIndexOf name
                if index > 0 then
                    // Trim fully qualified symbols
                    max 0 (rangeText.Length - name.Length), 0
                elif index = 0 then 0, (rangeText.Length - name.Length)
                else 0, 0
            let (startRow, startCol, endRow, endCol) = (range.StartLine-1, range.StartColumn + startOffset, range.EndLine-1, range.EndColumn - endOffset)
            vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.guidLogicalTextView, 
                startRow, startCol, endRow, endCol)
            |> ensureSucceeded
        | _ -> ()

    // This class implement 'IVSNavInfo' in order to be attached in search criteria
    // The interface implementation isn't used for another purpose for now
    interface IVsNavInfo with
        member __.GetLibGuid(_pGuid: byref<Guid>): int = raise (NotImplementedException())
        member __.GetSymbolType(_pdwType: byref<uint32>): int = raise (NotImplementedException())
              
        member __.EnumPresentationNodes(_dwFlags: uint32, _ppEnum: byref<IVsEnumNavInfoNodes>): int = 
            raise (NotImplementedException())
              
        member __.EnumCanonicalNodes(_ppEnum: byref<IVsEnumNavInfoNodes>): int = 
            raise (NotImplementedException())

type FSharpLibrary(guid: Guid) =
    let mutable capabilities: _LIB_FLAGS2 = Unchecked.defaultof<_>
    let mutable root = LibraryNode("", LibraryNodeType.Package)

    member __.LibraryCapabilities
        with get () = capabilities
        and set v = capabilities <- v
    
    interface IVsSimpleLibrary2 with
        member __.GetSupportedCategoryFields2(_category: int, pgrfCatField: byref<uint32>): int = 
            pgrfCatField <- uint32 _LIB_CATEGORY2.LC_HIERARCHYTYPE ||| uint32 _LIB_CATEGORY2.LC_PHYSICALCONTAINERTYPE
            VSConstants.S_OK
              
        member __.GetList2(listType: uint32, _flags: uint32, pobSrch: VSOBSEARCHCRITERIA2 [], 
                           ppIVsSimpleObjectList2: byref<IVsSimpleObjectList2>): int = 
            if pobSrch <> null && not (Array.isEmpty pobSrch) && pobSrch.[0].dwCustom = Constants.findReferencesResults then
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
              
        member __.UpdateCounter(pCurUpdate: byref<uint32>): int = 
            (root :> IVsSimpleObjectList2).UpdateCounter(&pCurUpdate)
              
        member __.GetGuid(pguidLib: byref<Guid>): int = 
            pguidLib <- guid
            VSConstants.S_OK
              
        member __.GetSeparatorStringWithOwnership(pbstrSeparator: byref<string>): int = 
            pbstrSeparator <- "."
            VSConstants.S_OK
              
        member __.LoadState(_pIStream: IStream, _lptType: LIB_PERSISTTYPE): int = VSConstants.S_OK
        member __.SaveState(_pIStream: IStream, _lptType: LIB_PERSISTTYPE): int = VSConstants.S_OK
              
        member __.GetBrowseContainersForHierarchy(_pHierarchy: IVsHierarchy, _celt: uint32, 
                                                  _rgBrowseContainers: VSBROWSECONTAINER [], _pcActual: uint32 []): int = 
            VSConstants.E_NOTIMPL
              
        member __.AddBrowseContainer(_pcdComponent: VSCOMPONENTSELECTORDATA [], _pgrfOptions: byref<uint32>, 
                                     pbstrComponentAdded: byref<string>): int = 
            pbstrComponentAdded <- null
            VSConstants.E_NOTIMPL
              
        member __.RemoveBrowseContainer(_dwReserved: uint32, _pszLibName: string): int = VSConstants.E_NOTIMPL
              
        member __.CreateNavInfo(_rgSymbolNodes: SYMBOL_DESCRIPTION_NODE [], _ulcNodes: uint32, ppNavInfo: byref<IVsNavInfo>): int = 
            ppNavInfo <- null
            VSConstants.E_NOTIMPL

