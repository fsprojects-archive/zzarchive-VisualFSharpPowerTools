namespace FSharp.Editing.VisualStudio.Symbol

open System
open System.Collections.Generic
open System.ComponentModel.Design
open System.Runtime.InteropServices
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharp.Editing.VisualStudio

type ErrorHandler = Microsoft.VisualStudio.ErrorHandler
type VSConstants = Microsoft.VisualStudio.VSConstants

/// Enumeration of the capabilities of a node. It is possible to combine different values
/// to support more capabilities.
/// This enumeration is a copy of _LIB_LISTCAPABILITIES with the Flags attribute set.
[<Flags>]
type LibraryNodeCapabilities =
    | None = 0 // _LIB_LISTCAPABILITIES.LLC_NONE
    | HasBrowseObject = 1 // _LIB_LISTCAPABILITIES.LLC_HASBROWSEOBJ
    | HasDescriptionPane = 2 // _LIB_LISTCAPABILITIES.LLC_HASDESCPANE
    | HasSourceContext = 4 // _LIB_LISTCAPABILITIES.LLC_HASSOURCECONTEXT
    | HasCommands = 8 // _LIB_LISTCAPABILITIES.LLC_HASCOMMANDS
    | AllowDragDrop = 16 // _LIB_LISTCAPABILITIES.LLC_ALLOWDRAGDROP
    | AllowRename = 32 // _LIB_LISTCAPABILITIES.LLC_ALLOWRENAME
    | AllowDelete = 64 // _LIB_LISTCAPABILITIES.LLC_ALLOWDELETE
    | AllowSourceControl = 128 // _LIB_LISTCAPABILITIES.LLC_ALLOWSCCOPS

/// Enumeration of the possible types of node. The type of a node can be the combination
/// of one of more of these values.
/// This is actually a copy of the _LIB_LISTTYPE enumeration with the difference that the
/// Flags attribute is set so that it is possible to specify more than one value.
[<Flags>]
type LibraryNodeType =
    | None = 0
    | Hierarchy = 1 // _LIB_LISTTYPE.LLT_HIERARCHY,
    | Namespaces  = 2 // _LIB_LISTTYPE.LLT_NAMESPACES,
    | Classes = 4 // _LIB_LISTTYPE.LLT_CLASSES,
    | Members = 8 // _LIB_LISTTYPE.LLT_MEMBERS,
    | Package = 16 // _LIB_LISTTYPE.LLT_PACKAGE,
    | PhysicalContainer = 16 // _LIB_LISTTYPE.LLT_PHYSICALCONTAINERS,
    | Containment = 32 // _LIB_LISTTYPE.LLT_CONTAINMENT,
    | ContainedBy = 64 // _LIB_LISTTYPE.LLT_CONTAINEDBY,
    | UsesClasses = 128 // _LIB_LISTTYPE.LLT_USESCLASSES,
    | UsedByClasses = 256 // _LIB_LISTTYPE.LLT_USEDBYCLASSES,
    | NestedClasses = 512 // _LIB_LISTTYPE.LLT_NESTEDCLASSES,
    | InheritedInterface = 1024 // _LIB_LISTTYPE.LLT_INHERITEDINTERFACES,
    | InterfaceUsedByClasses = 2048 // _LIB_LISTTYPE.LLT_INTERFACEUSEDBYCLASSES,
    | Definitions = 4096 // _LIB_LISTTYPE.LLT_DEFINITIONS,
    | References = 8192 // _LIB_LISTTYPE.LLT_REFERENCES,
    | DeferExpansion = 1048576 // _LIB_LISTTYPE.LLT_DEFEREXPANSION

/// Single node inside the tree of the libraries in the object browser or class view.
type LibraryNode(name: string, ?nodeType: LibraryNodeType, ?capabilities: LibraryNodeCapabilities, ?contextMenuID: CommandID) =
    static let NullIndex = uint32 0xFFFFFFFF

    let mutable name = name
    let mutable nodeType = defaultArg nodeType LibraryNodeType.None
    let mutable capabilities = defaultArg capabilities LibraryNodeCapabilities.None
    let mutable contextMenuID = defaultArg contextMenuID null
    let mutable children = ResizeArray<LibraryNode>()
    let mutable clipboardFormats = ResizeArray<VSOBJCLIPFORMAT>()
    let mutable displayData: VSTREEDISPLAYDATA = Unchecked.defaultof<_>
    let mutable flags: _VSTREEFLAGS = Unchecked.defaultof<_>
    let mutable tooltip: string = null
    let updateCount = ref 0
    let mutable filteredView = Dictionary<LibraryNodeType, LibraryNode>()

    let setCapabilityFlag(flag: LibraryNodeCapabilities, value) =
        if value then 
            capabilities <- capabilities ||| flag
        else
            capabilities <- capabilities &&& ~~~flag

    member internal __.ContextMenuID
        with get () = contextMenuID
        and set value = contextMenuID <- value
  
    member internal __.Children
        with get () = children
        and set value = children <- value

    member internal __.ClipboardFormats
        with get () = clipboardFormats
        and set value = clipboardFormats <- value

    member internal __.UpdateCount 
        with get () = !updateCount
        and set value = updateCount := value

    member internal __.FilteredView 
        with get () = filteredView
        and set value = filteredView <- value

    member __.AddNode(node: LibraryNode) =
        lock children (fun () -> children.Add(node))
        incr updateCount

    member __.RemoveNode(node: LibraryNode) =
        lock children (fun () -> children.Remove(node) |> ignore)
        incr updateCount

    /// Get or Set if the node can be deleted.
    member __.CanDelete
        with get () = (capabilities &&& LibraryNodeCapabilities.AllowDelete) <> LibraryNodeCapabilities.None
        and set value = setCapabilityFlag(LibraryNodeCapabilities.AllowDelete, value)

    /// Get or Set if the node can be associated with some source code.
    member __.CanGoToSource
        with get () = (capabilities &&& LibraryNodeCapabilities.HasSourceContext) <> LibraryNodeCapabilities.None
        and set value = setCapabilityFlag(LibraryNodeCapabilities.HasSourceContext, value)

    /// Get or Set if the node can be renamed.
    member __.CanRename
        with get () = (capabilities &&& LibraryNodeCapabilities.AllowRename) <> LibraryNodeCapabilities.None
        and set value = setCapabilityFlag(LibraryNodeCapabilities.AllowRename, value)

    member __.Capabilities
        with get () = capabilities
        and set value = capabilities <- value

    member __.Flags
        with get () = flags
        and set value = flags <- value

    member __.TooltipText
        with get () = tooltip
        and set value = tooltip <- value

    member __.Name
        with get () = name
        and set value = name <- value

    member __.NodeType
        with get () = nodeType
        and set value = nodeType <- value

    abstract DisplayData: VSTREEDISPLAYDATA
    default __.DisplayData = displayData

    abstract UniqueName: string
    default x.UniqueName = x.Name

    /// <summary>
    /// Finds the source files associated with this node.
    /// </summary>
    /// <param name="hierarchy">The hierarchy containing the items.</param>
    /// <param name="itemId">The item id of the item.</param>
    /// <param name="itemsCount">Number of items.</param>
    abstract SourceItems: byref<IVsHierarchy> * byref<uint32> * byref<uint32> -> unit
    default __.SourceItems([<Out>] hierarchy: byref<IVsHierarchy>, [<Out>] itemId: byref<uint32>, 
                           [<Out>] itemsCount: byref<uint32>) =
        hierarchy <- null
        itemId <- 0u
        itemsCount <- 0u

    /// Performs the operations needed to delete this node.
    abstract Delete: unit -> unit
    default __.Delete() = ()

    abstract Rename: string * uint32 -> unit
    default __.Rename(_, _) = ()

    /// Perform a Drag and Drop operation on this node.
    abstract DoDragDrop: OleDataObject * uint32 * uint32 -> unit
    default __.DoDragDrop(_dataObject: OleDataObject, _keyState: uint32, _effect: uint32) = ()
 
    abstract EnumClipboardFormats: _VSOBJCFFLAGS * VSOBJCLIPFORMAT [] -> uint32
    default __.EnumClipboardFormats(_flags: _VSOBJCFFLAGS, formats: VSOBJCLIPFORMAT []) =
        if formats = null || formats.Length = 0 then
            uint32 clipboardFormats.Count
        else
            let mutable itemsToCopy = uint32 clipboardFormats.Count
            if itemsToCopy > uint32 formats.Length then
                itemsToCopy <- uint32 formats.Length
            Array.Copy(clipboardFormats.ToArray(), formats, int itemsToCopy)
            itemsToCopy

    abstract FillDescription: _VSOBJDESCOPTIONS * IVsObjectBrowserDescription3 -> unit
    default __.FillDescription(_flags: _VSOBJDESCOPTIONS, description: IVsObjectBrowserDescription3) =
        description.ClearDescriptionText() |> ignore
        description.AddDescriptionText3(name, VSOBDESCRIPTIONSECTION.OBDS_NAME, null) |> ignore

    member x.FilterView(filterType: LibraryNodeType) =
        match filteredView.TryGetValue(filterType) with
        | true, filtered ->
            filtered
        | _ -> 
            let filtered = x.Clone()
            let rec loop i =
                if i = filtered.Children.Count then ()
                elif (filtered.Children.[i].NodeType &&& filterType) = enum<_> 0 then
                    filtered.Children.RemoveAt(i)
                else loop (i + 1)
            loop 0
            filteredView.Add(filterType, filtered)
            filtered

    abstract BrowseObject: obj
    default __.BrowseObject = null

    abstract CategoryField: LIB_CATEGORY -> uint32
    default __.CategoryField(category: LIB_CATEGORY) =
        match category with
        | LIB_CATEGORY.LC_LISTTYPE ->
            let mutable subTypes = LibraryNodeType.None
            for node in children do
                subTypes <- subTypes ||| node.NodeType
            uint32 subTypes
        | x when x = enum<_>(int _LIB_CATEGORY2.LC_HIERARCHYTYPE) ->
            uint32 _LIBCAT_HIERARCHYTYPE.LCHT_UNKNOWN
        | _ ->
            raise (NotImplementedException())

    abstract GotoSource: VSOBJGOTOSRCTYPE -> unit
    default __.GotoSource(_gotoType: VSOBJGOTOSRCTYPE) = ()

    abstract GetTextWithOwnership: VSTREETEXTOPTIONS -> string
    default x.GetTextWithOwnership(_tto: VSTREETEXTOPTIONS) = x.Name

    member __.GetSourceContextWithOwnership([<Out>] fileName: byref<string>, [<Out>] pulLineNum: byref<uint32>) =
        fileName <- null
        pulLineNum <- 0u

    abstract Clone: unit -> LibraryNode
    default x.Clone() = 
        let node = LibraryNode(x.Name, x.NodeType, x.Capabilities, x.ContextMenuID)
        node.Children.AddRange(x.Children)
        node.ClipboardFormats.AddRange(x.ClipboardFormats)
        node.UpdateCount <- x.UpdateCount
        node

    member internal __.CheckIndexOutOfRange(index) =
        if index >= uint32 children.Count then
           let ex = ArgumentOutOfRangeException("index")
           Logging.logException ex
           raise ex

    interface IVsSimpleObjectList2 with
        member x.GetFlags(pFlags: byref<uint32>): int = 
            pFlags <- uint32 x.Flags
            VSConstants.S_OK
        
        member __.GetCapabilities2(pgrfCapabilities: byref<uint32>): int = 
            pgrfCapabilities <- uint32 capabilities
            VSConstants.S_OK
        
        member __.GetItemCount(pCount: byref<uint32>): int = 
            pCount <- uint32 children.Count
            VSConstants.S_OK
        
        member x.GetDisplayData(index: uint32, pData: VSTREEDISPLAYDATA []): int = 
            x.CheckIndexOutOfRange(index)
            pData.[0] <- children.[int index].DisplayData
            VSConstants.S_OK
        
        member x.GetCategoryField2(index: uint32, category: int, pfCatField: byref<uint32>): int = 
            let check() = x.CheckIndexOutOfRange(index)
            let node =
                if index = NullIndex then x
                else 
                    check()
                    children.[int index]
            pfCatField <- node.CategoryField(enum<LIB_CATEGORY>(category))
            VSConstants.S_OK
        
        member x.GetBrowseObject(index: uint32, ppdispBrowseObj: byref<obj>): int = 
            x.CheckIndexOutOfRange(index)
            ppdispBrowseObj <- children.[int index].BrowseObject
            if ppdispBrowseObj = null then
                VSConstants.E_NOTIMPL
            else
                VSConstants.S_OK
        
        member x.CountSourceItems(index: uint32, ppHier: byref<IVsHierarchy>, pItemid: byref<uint32>, pcItems: byref<uint32>): int = 
            x.CheckIndexOutOfRange(index)
            children.[int index].SourceItems(&ppHier, &pItemid, &pcItems)
            VSConstants.S_OK
        
        member x.CanGoToSource(index: uint32, _srcType: VSOBJGOTOSRCTYPE, pfOK: byref<int>): int = 
            x.CheckIndexOutOfRange(index)
            pfOK <- if children.[int index].CanGoToSource then 1 else 0
            VSConstants.S_OK
        
        member x.GetContextMenu(index: uint32, pclsidActive: byref<Guid>, pnMenuId: byref<int>, 
                                ppCmdTrgtActive: byref<IOleCommandTarget>): int = 
            x.CheckIndexOutOfRange(index)
            let commandId = children.[int index].ContextMenuID
            if commandId = null then
                pclsidActive <- Guid.Empty
                pnMenuId <- 0
                ppCmdTrgtActive <- null
                VSConstants.E_NOTIMPL
            else
                pclsidActive <- commandId.Guid
                pnMenuId <- commandId.ID
                let child = children.[int index] :> obj
                ppCmdTrgtActive <-
                    match child with
                    | :? IOleCommandTarget as target -> target
                    | _ -> null
                VSConstants.S_OK

        member x.DoDragDrop(index: uint32, pDataObject: IDataObject, grfKeyState: uint32, pdwEffect: byref<uint32>): int = 
            x.CheckIndexOutOfRange(index)
            let dataObject = OleDataObject(pDataObject)
            children.[int index].DoDragDrop(dataObject, grfKeyState, pdwEffect)
            VSConstants.S_OK
        
        member x.CanRename(index: uint32, _pszNewName: string, pfOK: byref<int>): int = 
            x.CheckIndexOutOfRange(index)
            pfOK <- if children.[int index].CanRename then 1 else 0
            VSConstants.S_OK
        
        member x.DoRename(index: uint32, pszNewName: string, grfFlags: uint32): int = 
            x.CheckIndexOutOfRange(index)
            children.[int index].Rename(pszNewName, grfFlags)
            VSConstants.S_OK
        
        member x.CanDelete(index: uint32, pfOK: byref<int>): int = 
            x.CheckIndexOutOfRange(index)
            pfOK <- if children.[int index].CanDelete then 1 else 0
            VSConstants.S_OK
        
        member x.DoDelete(index: uint32, _grfFlags: uint32): int = 
            x.CheckIndexOutOfRange(index)
            children.[int index].Delete()
            children.RemoveAt(int index)
            VSConstants.S_OK
        
        member x.FillDescription2(index: uint32, grfOptions: uint32, pobDesc: IVsObjectBrowserDescription3): int = 
            x.CheckIndexOutOfRange(index)
            children.[int index].FillDescription(enum<_VSOBJDESCOPTIONS>(int grfOptions), pobDesc) |> ignore
            VSConstants.S_OK
                      
        member x.EnumClipboardFormats(index: uint32, grfFlags: uint32, _celt: uint32, rgcfFormats: VSOBJCLIPFORMAT [], pcActual: uint32 []): int = 
            x.CheckIndexOutOfRange(index)
            let copied = children.[int index].EnumClipboardFormats(enum<_VSOBJCFFLAGS>(int grfFlags), rgcfFormats)

            if pcActual <> null && pcActual.Length > 0 then
                pcActual.[0] <- copied
            VSConstants.S_OK
        
        member __.GetClipboardFormat(_index: uint32, _grfFlags: uint32, _pFormatetc: FORMATETC [], _pMedium: STGMEDIUM []): int = 
            VSConstants.E_NOTIMPL
        
        member __.GetExtendedClipboardVariant(_index: uint32, _grfFlags: uint32, _pcfFormat: VSOBJCLIPFORMAT [], 
                                              pvarFormat: byref<obj>): int = 
            pvarFormat <- null
            VSConstants.E_NOTIMPL
        
        member __.GetExpandable3(_index: uint32, _listTypeExcluded: uint32, pfExpandable: byref<int>): int = 
            // There is a not empty implementation of GetCategoryField2, so this method should
            // return E_NOTIMPL.
            pfExpandable <- 0
            VSConstants.E_NOTIMPL
        
        member x.GetList2(index: uint32, listType: uint32, _flags: uint32, _pobSrch: VSOBSEARCHCRITERIA2 [], 
                          ppIVsSimpleObjectList2: byref<IVsSimpleObjectList2>): int = 
            // TODO: Use the flags and list type to actually filter the result.
            x.CheckIndexOutOfRange(index)
            ppIVsSimpleObjectList2 <- children.[int index].FilterView(enum<LibraryNodeType> (int listType))
            VSConstants.S_OK
 
        member __.GetMultipleSourceItems(_index: uint32, _grfGSI: uint32, _cItems: uint32, _rgItemSel: VSITEMSELECTION []) =
            VSConstants.E_NOTIMPL

        member __.GetNavInfo(_index: uint32, [<Out>] ppNavInfo: byref<IVsNavInfo>) =
            ppNavInfo <- null
            VSConstants.E_NOTIMPL

        member x.GetNavInfoNode(index: uint32, [<Out>] ppNavInfoNode: byref<IVsNavInfoNode>) =
            x.CheckIndexOutOfRange(index)
            ppNavInfoNode <- children.[int index] :> IVsNavInfoNode
            VSConstants.S_OK

        member __.GetProperty(_index: uint32, _propid: int, [<Out>] pvar) =
            pvar <- null
            VSConstants.E_NOTIMPL

        member x.GetSourceContextWithOwnership(index: uint32, [<Out>] pbstrFilename: byref<string>, 
                                               [<Out>] pulLineNum: byref<uint32>) =
            x.CheckIndexOutOfRange(index)
            children.[int index].GetSourceContextWithOwnership(&pbstrFilename, &pulLineNum)
            match pbstrFilename with
            | null -> VSConstants.E_NOTIMPL
            | _ -> VSConstants.S_OK

        member x.GetTextWithOwnership(index: uint32, tto: VSTREETEXTOPTIONS, [<Out>] pbstrText: byref<string>) =
            x.CheckIndexOutOfRange(index)
            pbstrText <- children.[int index].GetTextWithOwnership(tto)
            match pbstrText with
            | null -> VSConstants.E_NOTIMPL
            | _ -> VSConstants.S_OK

        member x.GetTipTextWithOwnership(index: uint32, _eTipType: VSTREETOOLTIPTYPE, [<Out>] pbstrText: byref<string>) =
            x.CheckIndexOutOfRange(index)
            pbstrText <- children.[int index].TooltipText
            VSConstants.S_OK

        member __.GetUserContext(_index: uint32, [<Out>] ppunkUserCtx) =
            ppunkUserCtx <- null
            VSConstants.E_NOTIMPL

        member x.GoToSource(index: uint32, srcType: VSOBJGOTOSRCTYPE) =
            x.CheckIndexOutOfRange(index)
            children.[int index].GotoSource(srcType)
            VSConstants.S_OK

        member __.LocateNavInfoNode(pNavInfoNode: IVsNavInfoNode, [<Out>] pulIndex: byref<uint32>) =
            match pNavInfoNode with
            | null ->
                raise (ArgumentNullException("pNavInfoNode"))
            | _ ->
                pulIndex <- NullIndex
                let nodeName = ref null
                ErrorHandler.ThrowOnFailure(pNavInfoNode.get_Name(nodeName)) |> ignore
                let rec loop i =
                    if i = children.Count then
                        VSConstants.S_FALSE
                    elif 0 = String.Compare(children.[i].UniqueName, !nodeName, StringComparison.OrdinalIgnoreCase) then
                        VSConstants.S_OK
                    else loop (i + 1)
                loop 0

        member __.OnClose(_ptca: VSTREECLOSEACTIONS []) = VSConstants.S_OK

        member __.QueryDragDrop(_index: uint32, _pDataObject: IDataObject, _grfKeyState: uint32, _pdwEffect: byref<uint32>) =
            VSConstants.E_NOTIMPL

        member __.ShowHelp(_index: uint32) = VSConstants.E_NOTIMPL

        member __.UpdateCounter([<Out>] pCurUpdate: byref<uint32>) =
            pCurUpdate <- uint32 !updateCount
            VSConstants.S_OK

    interface IVsNavInfoNode with
        member x.get_Name([<Out>] pbstrName: byref<string>) =
            pbstrName <- x.UniqueName
            VSConstants.S_OK

        member __.get_Type([<Out>] pllt: byref<uint32>) =
            pllt <- uint32 nodeType
            VSConstants.S_OK 