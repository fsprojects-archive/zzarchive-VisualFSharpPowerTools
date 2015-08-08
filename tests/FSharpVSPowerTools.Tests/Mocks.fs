[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Foq
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio
open FSharpVSPowerTools
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.OLE.Interop

let createGeneralOptionsPage() =
    Mock<IGeneralOptions>.With(fun page ->
        <@
            page.DepthColorizerEnabled --> true
            page.FindAllReferencesEnabled --> true
            page.FolderOrganizationEnabled --> true
            page.FormattingEnabled --> true
            page.GenerateRecordStubEnabled --> true
            page.HighlightUsageEnabled --> true
            page.InterfaceImplementationEnabled --> true
            page.NavBarEnabled --> true
            page.NavigateToEnabled --> true
            page.RenameRefactoringEnabled --> true
            page.ResolveUnopenedNamespacesEnabled --> true
            page.SyntaxColoringEnabled --> true
            page.UnionPatternMatchCaseGenerationEnabled --> true
            page.UnusedReferencesEnabled --> true
            page.UnusedOpensEnabled --> true
            page.XmlDocEnabled --> true
            page.GoToSymbolSourceEnabled --> true
            page.LinterEnabled --> true
        @>)

let createClassificationTypeRegistryService() =
    Mock<IClassificationTypeRegistryService>()
        .Setup(fun x -> <@ x.GetClassificationType (any()) @>)
        .Calls<string>(fun t -> Mock<IClassificationType>.With(fun x -> <@ x.Classification --> t @>))
        .Create()

let createTextUndoHistoryRegistry() =
    let undoTransation =
        Mock<ITextUndoTransaction>.With(fun x ->
            <@
                x.Complete()
                x.Dispose()
            @>)

    let undoHistory =
        Mock<ITextUndoHistory>()
            .Setup(fun x -> <@ x.CreateTransaction (any()) @>)
            .Returns(undoTransation)
            .Create()

    Mock<ITextUndoHistoryRegistry>()
        .Setup(fun x -> <@ x.RegisterHistory (any()) @>)
        .Returns(undoHistory)
        .Create()

let createEditorOptionsFactoryService() =
    let editorOptions =
        Mock<IEditorOptions>.With (fun x ->
            <@ x.GetOptionValue ((new IndentSize()).Key) --> 4 @>)
    Mock<IEditorOptionsFactoryService>.With(fun x ->
        <@ x.GetOptions (any()) --> editorOptions @>)

let createEditorOperationsFactoryService() =
    Mock<IEditorOperationsFactoryService>().Create()

let createTextBufferUndoManagerProvider() =
    Mock<ITextBufferUndoManagerProvider>().Create()

let createDummyCommandTarget() =
    {
        new IOleCommandTarget with
            member __.Exec(_pguidCmdGroup: byref<Guid>, _nCmdID: uint32, _nCmdexecopt: uint32, _pvaIn: nativeint, _pvaOut: nativeint): int = 
                VSConstants.S_OK
            
            member __.QueryStatus(_pguidCmdGroup: byref<Guid>, _cCmds: uint32, _prgCmds: OLECMD [], _pCmdText: nativeint): int = 
                VSConstants.S_OK
    }
    
let createDocumentFactoryService() =
    { 
        new ITextDocumentFactoryService with
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType): ITextDocument = notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _encoding: Text.Encoding, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _attemptUtf8Detection: bool, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = notimpl
            member __.CreateTextDocument(_textBuffer: ITextBuffer, _filePath: string): ITextDocument = notimpl
            [<CLIEvent>]
            member __.TextDocumentCreated: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
            [<CLIEvent>]
            member __.TextDocumentDisposed: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
        
            member __.TryGetTextDocument(textBuffer: ITextBuffer, textDocument: byref<ITextDocument>): bool = 
                textBuffer.Properties.TryGetProperty(typeof<ITextDocument>, &textDocument) 
    }

let createSVsResourceManager() =
    {
        new IVsResourceManager with
            member __.GetSatelliteAssemblyPath(assemblyPath: string, lcid: int, pbstrPath: byref<string>): int = 
                notimpl
            member __.LoadResourceBitmap(guidPackage: byref<Guid>, culture: int, pszResourceName: string, hbmpValue: byref<nativeint>): int = 
                VSConstants.S_FALSE

            member __.LoadResourceBitmap2(pszAssemblyPath: string, culture: int, szResourceName: string, hbmpValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceBlob(guidPackage: byref<Guid>, culture: int, pszResourceName: string, pBytes: byref<nativeint>, lAllocated: byref<int>): int = 
                notimpl
            member __.LoadResourceBlob2(pszAssemblyPath: string, culture: int, pszResourceName: string, pBytes: byref<nativeint>, lAllocated: byref<int>): int = 
                notimpl
            member __.LoadResourceIcon(guidPackage: byref<Guid>, culture: int, pszResourceName: string, cx: int, cy: int, hicoValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceIcon2(pszAssemblyPath: string, culture: int, pszResourceName: string, cx: int, cy: int, hicoValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceString(guidPackage: byref<Guid>, culture: int, pszResourceName: string, pbstrValue: byref<string>): int = 
                notimpl
            member __.LoadResourceString2(pszAssemblyPath: string, culture: int, pszResourceName: string, pbstrValue: byref<string>): int = 
                notimpl
    }

let createVsSolutionBuildManager2() =
    {
        new IVsSolutionBuildManager2 with
            member __.AdviseUpdateSolutionEvents(pIVsUpdateSolutionEvents: IVsUpdateSolutionEvents, pdwCookie: byref<uint32>): int = 
                VSConstants.S_OK

            member __.CalculateProjectDependencies(): int = notimpl
            member __.CanCancelUpdateSolutionConfiguration(pfCanCancel: byref<int>): int = notimpl
            member __.CancelUpdateSolutionConfiguration(): int = notimpl
            member __.DebugLaunch(grfLaunch: uint32): int = notimpl
            member __.FindActiveProjectCfg(pvReserved1: nativeint, pvReserved2: nativeint, pIVsHierarchy_RequestedProject: IVsHierarchy, ppIVsProjectCfg_Active: IVsProjectCfg []): int = notimpl
            member __.GetProjectDependencies(pHier: IVsHierarchy, celt: uint32, rgpHier: IVsHierarchy [], pcActual: uint32 []): int = notimpl
            member __.QueryBuildManagerBusy(pfBuildManagerBusy: byref<int>): int = notimpl
            member __.QueryDebugLaunch(grfLaunch: uint32, pfCanLaunch: byref<int>): int = notimpl
            member __.QueryProjectDependency(pHier: IVsHierarchy, pHierDependentOn: IVsHierarchy, pfIsDependentOn: byref<int>): int = notimpl
            member __.SaveDocumentsBeforeBuild(pHier: IVsHierarchy, itemid: uint32, docCookie: uint32): int = notimpl
            member __.StartSimpleUpdateProjectConfiguration(pIVsHierarchyToBuild: IVsHierarchy, pIVsHierarchyDependent: IVsHierarchy, pszDependentConfigurationCanonicalName: string, dwFlags: uint32, dwDefQueryResults: uint32, fSuppressUI: int): int = notimpl
            member __.StartSimpleUpdateSolutionConfiguration(dwFlags: uint32, dwDefQueryResults: uint32, fSuppressUI: int): int = notimpl
            member __.StartUpdateProjectConfigurations(cProjs: uint32, rgpHierProjs: IVsHierarchy [], dwFlags: uint32, fSuppressUI: int): int = notimpl
            member __.StartUpdateSpecificProjectConfigurations(cProjs: uint32, rgpHier: IVsHierarchy [], rgpcfg: IVsCfg [], rgdwCleanFlags: uint32 [], rgdwBuildFlags: uint32 [], rgdwDeployFlags: uint32 [], dwFlags: uint32, fSuppressUI: int): int = notimpl
                       
            member __.UnadviseUpdateSolutionEvents(dwCookie: uint32): int = 
                VSConstants.S_OK
        
            member __.UpdateSolutionConfigurationIsActive(pfIsActive: byref<int>): int = notimpl
            member __.get_CodePage(puiCodePage: byref<uint32>): int = notimpl
            member __.get_IsDebug(pfIsDebug: byref<int>): int = notimpl
            member __.get_StartupProject(ppHierarchy: byref<IVsHierarchy>): int = notimpl
            member __.put_CodePage(uiCodePage: uint32): int = notimpl
            member __.put_IsDebug(fIsDebug: int): int = notimpl
            member __.set_StartupProject(pHierarchy: IVsHierarchy): int = notimpl

        interface IVsSolutionBuildManager with
            member __.UnadviseUpdateSolutionEvents(dwCookie: uint32): int = notimpl     
            member __.get_StartupProject(ppHierarchy: byref<IVsHierarchy>): int = notimpl 
            member __.QueryDebugLaunch(grfLaunch: uint32, pfCanLaunch: byref<int>): int = notimpl
            member __.put_CodePage(uiCodePage: uint32): int = notimpl
            member __.put_IsDebug(fIsDebug: int): int = notimpl
            member __.set_StartupProject(pHierarchy: IVsHierarchy): int = notimpl
            member __.get_CodePage(puiCodePage: byref<uint32>): int = notimpl
            member __.get_IsDebug(pfIsDebug: byref<int>): int = notimpl
            member __.StartSimpleUpdateProjectConfiguration(pIVsHierarchyToBuild: IVsHierarchy, pIVsHierarchyDependent: IVsHierarchy, pszDependentConfigurationCanonicalName: string, dwFlags: uint32, dwDefQueryResults: uint32, fSuppressUI: int): int = notimpl
            member __.CanCancelUpdateSolutionConfiguration(pfCanCancel: byref<int>): int = notimpl
            member __.AdviseUpdateSolutionEvents(pIVsUpdateSolutionEvents: IVsUpdateSolutionEvents, pdwCookie: byref<uint32>): int = notimpl
            member __.CancelUpdateSolutionConfiguration(): int = notimpl
            member __.DebugLaunch(grfLaunch: uint32): int = notimpl
            member __.FindActiveProjectCfg(pvReserved1: nativeint, pvReserved2: nativeint, pIVsHierarchy_RequestedProject: IVsHierarchy, ppIVsProjectCfg_Active: IVsProjectCfg []): int = notimpl
            member __.GetProjectDependencies(pHier: IVsHierarchy, celt: uint32, rgpHier: IVsHierarchy [], pcActual: uint32 []): int = notimpl
            member __.QueryBuildManagerBusy(pfBuildManagerBusy: byref<int>): int = notimpl
            member __.UpdateSolutionConfigurationIsActive(pfIsActive: byref<int>): int = notimpl
            member __.StartSimpleUpdateSolutionConfiguration(dwFlags: uint32, dwDefQueryResults: uint32, fSuppressUI: int): int = notimpl                                   
    }

let createSVsStatusbar() =
    let setText str = 
        printfn "Status bar: %s" str
        0
    Mock<IVsStatusbar>()
        .Setup(fun x -> <@ x.SetText(any()) @>)
        .Calls<string>(fun str -> setText str)
        .Create()

let createVsEditorAdaptersFactoryService() =
    let vsTextView =
        { 
          new IVsTextView with
              member __.AddCommandFilter(pNewCmdTarg: OLE.Interop.IOleCommandTarget, ppNextCmdTarg: byref<OLE.Interop.IOleCommandTarget>): int = 
                  ppNextCmdTarg <- createDummyCommandTarget()
                  VSConstants.S_OK
              
              member __.CenterColumns(iLine: int, iLeftCol: int, iColCount: int): int = 
                  notimpl
              member __.CenterLines(iTopLine: int, iCount: int): int = 
                  notimpl
              member __.ClearSelection(fMoveToAnchor: int): int = 
                  notimpl
              member __.CloseView(): int = 
                  notimpl
              member __.EnsureSpanVisible(span: TextSpan): int = 
                  notimpl
              member __.GetBuffer(ppBuffer: byref<IVsTextLines>): int = 
                  notimpl
              member __.GetCaretPos(piLine: byref<int>, piColumn: byref<int>): int = 
                  notimpl
              member __.GetLineAndColumn(iPos: int, piLine: byref<int>, piIndex: byref<int>): int = 
                  notimpl
              member __.GetLineHeight(piLineHeight: byref<int>): int = 
                  notimpl
              member __.GetNearestPosition(iLine: int, iCol: int, piPos: byref<int>, piVirtualSpaces: byref<int>): int = 
                  notimpl
              member __.GetPointOfLineColumn(iLine: int, iCol: int, ppt: OLE.Interop.POINT []): int = 
                  notimpl
              member __.GetScrollInfo(iBar: int, piMinUnit: byref<int>, piMaxUnit: byref<int>, piVisibleUnits: byref<int>, piFirstVisibleUnit: byref<int>): int = 
                  notimpl
              member __.GetSelectedText(pbstrText: byref<string>): int = 
                  notimpl
              member __.GetSelection(piAnchorLine: byref<int>, piAnchorCol: byref<int>, piEndLine: byref<int>, piEndCol: byref<int>): int = 
                  notimpl
              member __.GetSelectionDataObject(ppIDataObject: byref<OLE.Interop.IDataObject>): int = 
                  notimpl
              member __.GetSelectionMode(): TextSelMode = 
                  notimpl
              member __.GetSelectionSpan(pSpan: TextSpan []): int = 
                  notimpl
              member __.GetTextStream(iTopLine: int, iTopCol: int, iBottomLine: int, iBottomCol: int, pbstrText: byref<string>): int = 
                  notimpl
              member __.GetWindowHandle(): nativeint = 
                  notimpl
              member __.GetWordExtent(iLine: int, iCol: int, dwFlags: uint32, pSpan: TextSpan []): int = 
                  notimpl
              member __.HighlightMatchingBrace(dwFlags: uint32, cSpans: uint32, rgBaseSpans: TextSpan []): int = 
                  notimpl
              member __.Initialize(pBuffer: IVsTextLines, hwndParent: nativeint, initFlags: uint32, pInitView: INITVIEW []): int = 
                  notimpl
              member __.PositionCaretForEditing(iLine: int, cIndentLevels: int): int = 
                  notimpl
              member __.RemoveCommandFilter(pCmdTarg: OLE.Interop.IOleCommandTarget): int = 
                  notimpl
              member __.ReplaceTextOnLine(iLine: int, iStartCol: int, iCharsToReplace: int, pszNewText: string, iNewLen: int): int = 
                  notimpl
              member __.RestrictViewRange(iMinLine: int, iMaxLine: int, pClient: IVsViewRangeClient): int = 
                  notimpl
              member __.SendExplicitFocus(): int = 
                  notimpl
              member __.SetBuffer(pBuffer: IVsTextLines): int = 
                  notimpl
              member __.SetCaretPos(iLine: int, iColumn: int): int = 
                  notimpl
              member __.SetScrollPosition(iBar: int, iFirstVisibleUnit: int): int = 
                  notimpl
              member __.SetSelection(iAnchorLine: int, iAnchorCol: int, iEndLine: int, iEndCol: int): int = 
                  notimpl
              member __.SetSelectionMode(iSelMode: TextSelMode): int = 
                  notimpl
              member __.SetTopLine(iBaseLine: int): int = 
                  notimpl
              member __.UpdateCompletionStatus(pCompSet: IVsCompletionSet, dwFlags: uint32): int = 
                  notimpl
              member __.UpdateTipWindow(pTipWindow: IVsTipWindow, dwFlags: uint32): int = 
                  notimpl
              member __.UpdateViewFrameCaption(): int = 
                  notimpl
         }

    Mock<IVsEditorAdaptersFactoryService>()
        .Setup(fun x -> <@ x.GetViewAdapter (any()) @>)
        .Returns(vsTextView)
        .Create()