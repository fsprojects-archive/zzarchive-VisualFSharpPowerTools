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
open FSharpVSPowerTools.ProjectSystem
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
            page.OutliningEnabled --> true
            page.HighlightPrintfUsageEnabled --> true
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
    let editorOperations =
        Mock<IEditorOperations>.With(fun x ->
            <@
                x.AddBeforeTextBufferChangePrimitive()
                x.AddAfterTextBufferChangePrimitive()
            @>)
    Mock<IEditorOperationsFactoryService>.With(fun x ->
        <@ x.GetEditorOperations (any()) --> editorOperations @>)

let createTextBufferUndoManagerProvider() =
    Mock<ITextBufferUndoManagerProvider>.With(fun x ->
        <@ x.GetTextBufferUndoManager (any()) --> null @>)

let createDummyCommandTarget() =
    {
        new IOleCommandTarget with
            member __.Exec(_pguidCmdGroup, _nCmdID, _nCmdexecopt, _pvaIn, _pvaOut) = VSConstants.S_OK
            member __.QueryStatus(_pguidCmdGroup, _cCmds, _prgCmds, _pCmdText) = VSConstants.S_OK
    }
    
let createDocumentFactoryService() =
    { 
        new ITextDocumentFactoryService with
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType): ITextDocument = 
                notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _encoding: Text.Encoding, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
                notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _attemptUtf8Detection: bool, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
                notimpl
            member __.CreateTextDocument(_textBuffer, _filePath) = notimpl
            [<CLIEvent>]
            member __.TextDocumentCreated: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
            [<CLIEvent>]
            member __.TextDocumentDisposed: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
            
            member __.TryGetTextDocument(textBuffer, textDocument) = 
                textBuffer.Properties.TryGetProperty(typeof<ITextDocument>, &textDocument) 
    }

let createSVsResourceManager() =
    {
        new IVsResourceManager with
            member __.GetSatelliteAssemblyPath(_assemblyPath, _lcid, _pbstrPath) = notimpl
            member __.LoadResourceBitmap2(_pszAssemblyPath, _culture, _szResourceName, _hbmpValue) = notimpl
            member __.LoadResourceBlob(_guidPackage, _culture, _pszResourceName, _pBytes, _lAllocated) = notimpl
            member __.LoadResourceBlob2(_pszAssemblyPath, _culture, _pszResourceName, _pBytes, _lAllocated) = notimpl
            member __.LoadResourceIcon(_guidPackage, _culture, _pszResourceName, _cx, _cy, _hicoValue) = notimpl
            member __.LoadResourceIcon2(_pszAssemblyPath, _culture, _pszResourceName, _cx, _cy, _hicoValue) = notimpl
            member __.LoadResourceString(_guidPackage, _culture, _pszResourceName, _pbstrValue) = notimpl
            member __.LoadResourceString2(_pszAssemblyPath, _culture, _pszResourceName, _pbstrValue) = notimpl
            
            member __.LoadResourceBitmap(_guidPackage, _culture, _pszResourceName, _hbmpValue) = 
                VSConstants.S_FALSE
    }

let createVsSolutionBuildManager2() =
    {
        new IVsSolutionBuildManager2 with
            member __.CalculateProjectDependencies() = notimpl
            member __.CanCancelUpdateSolutionConfiguration(_pfCanCancel) = notimpl
            member __.CancelUpdateSolutionConfiguration() = notimpl
            member __.DebugLaunch(_grfLaunch) = notimpl
            member __.FindActiveProjectCfg(_pvReserved1, _pvReserved2, _pIVsHierarchy_RequestedProject, _ppIVsProjectCfg_Active) = notimpl
            member __.GetProjectDependencies(_pHier, _celt, _rgpHier, _pcActual) = notimpl
            member __.QueryBuildManagerBusy(_pfBuildManagerBusy) = notimpl
            member __.QueryDebugLaunch(_grfLaunch, _pfCanLaunch) = notimpl
            member __.QueryProjectDependency(_pHier, _pHierDependentOn, _pfIsDependentOn) = notimpl
            member __.SaveDocumentsBeforeBuild(_pHier, _itemid, _docCookie) = notimpl
            member __.StartSimpleUpdateProjectConfiguration(_pIVsHierarchyToBuild, _pIVsHierarchyDependent, _pszDependentConfigurationCanonicalName, _dwFlags, _dwDefQueryResults, _fSuppressUI) = notimpl
            member __.StartSimpleUpdateSolutionConfiguration(_dwFlags, _dwDefQueryResults, _fSuppressUI) = notimpl
            member __.StartUpdateProjectConfigurations(_cProjs, _rgpHierProjs, _dwFlags, _fSuppressUI) = notimpl
            member __.StartUpdateSpecificProjectConfigurations(_cProjs, _rgpHier, _rgpcfg, _rgdwCleanFlags, _rgdwBuildFlags, _rgdwDeployFlags, _dwFlags, _fSuppressUI) = notimpl
            member __.UpdateSolutionConfigurationIsActive(_pfIsActive) = notimpl
            member __.get_CodePage(_puiCodePage) = notimpl
            member __.get_IsDebug(_pfIsDebug) = notimpl
            member __.get_StartupProject(_ppHierarchy) = notimpl
            member __.put_CodePage(_uiCodePage) = notimpl
            member __.put_IsDebug(_fIsDebug) = notimpl
            member __.set_StartupProject(_pHierarchy) = notimpl
            
            member __.AdviseUpdateSolutionEvents(_pIVsUpdateSolutionEvents, _pdwCookie) = 
                VSConstants.S_OK
            member __.UnadviseUpdateSolutionEvents(_dwCookie) = 
                VSConstants.S_OK

        interface IVsSolutionBuildManager with
            member __.AdviseUpdateSolutionEvents(_pIVsUpdateSolutionEvents, _pdwCookie) = notimpl
            member __.CanCancelUpdateSolutionConfiguration(_pfCanCancel) = notimpl
            member __.CancelUpdateSolutionConfiguration() = notimpl
            member __.DebugLaunch(_grfLaunch) = notimpl
            member __.FindActiveProjectCfg(_pvReserved1, _pvReserved2, _pIVsHierarchy_RequestedProject, _ppIVsProjectCfg_Active) = notimpl
            member __.GetProjectDependencies(_pHier, _celt, _rgpHier, _pcActual) = notimpl
            member __.QueryBuildManagerBusy(_pfBuildManagerBusy) = notimpl
            member __.QueryDebugLaunch(_grfLaunch, _pfCanLaunch) = notimpl
            member __.StartSimpleUpdateProjectConfiguration(_pIVsHierarchyToBuild, _pIVsHierarchyDependent, _pszDependentConfigurationCanonicalName, _dwFlags, _dwDefQueryResults, _fSuppressUI) = notimpl
            member __.StartSimpleUpdateSolutionConfiguration(_dwFlags, _dwDefQueryResults, _fSuppressUI) = notimpl
            member __.UpdateSolutionConfigurationIsActive(_pfIsActive) = notimpl
            member __.get_CodePage(_puiCodePage) = notimpl
            member __.get_IsDebug(_pfIsDebug) = notimpl
            member __.get_StartupProject(_ppHierarchy) = notimpl
            member __.put_CodePage(_uiCodePage) = notimpl
            member __.put_IsDebug(_fIsDebug) = notimpl
            member __.set_StartupProject(_pHierarchy) = notimpl
            member __.UnadviseUpdateSolutionEvents(_dwCookie) = notimpl
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
              member __.CenterColumns(_iLine, _iLeftCol, _iColCount) = notimpl
              member __.CenterLines(_iTopLine, _iCount) = notimpl
              member __.ClearSelection(_fMoveToAnchor) = notimpl
              member __.CloseView() = notimpl
              member __.EnsureSpanVisible(_span) = notimpl
              member __.GetBuffer(_ppBuffer) = notimpl
              member __.GetCaretPos(_piLine, _piColumn) = notimpl
              member __.GetLineAndColumn(_iPos, _piLine, _piIndex) = notimpl
              member __.GetLineHeight(_piLineHeight) = notimpl
              member __.GetNearestPosition(_iLine, _iCol, _piPos, _piVirtualSpaces) = notimpl
              member __.GetPointOfLineColumn(_iLine, _iCol, _ppt) = notimpl
              member __.GetScrollInfo(_iBar, _piMinUnit, _piMaxUnit, _piVisibleUnits, _piFirstVisibleUnit) = notimpl
              member __.GetSelectedText(_pbstrText) = notimpl
              member __.GetSelection(_piAnchorLine, _piAnchorCol, _piEndLine, _piEndCol) = notimpl
              member __.GetSelectionDataObject(_ppIDataObject) = notimpl
              member __.GetSelectionMode() = notimpl
              member __.GetSelectionSpan(_pSpan) = notimpl
              member __.GetTextStream(_iTopLine, _iTopCol, _iBottomLine, _iBottomCol, _pbstrText) = notimpl
              member __.GetWindowHandle() = notimpl
              member __.GetWordExtent(_iLine, _iCol, _dwFlags, _pSpan) = notimpl
              member __.HighlightMatchingBrace(_dwFlags, _cSpans, _rgBaseSpans) = notimpl
              member __.Initialize(_pBuffer, _hwndParent, _initFlags, _pInitView) = notimpl
              member __.PositionCaretForEditing(_iLine, _cIndentLevels) = notimpl
              member __.RemoveCommandFilter(_pCmdTarg) = notimpl
              member __.ReplaceTextOnLine(_iLine, _iStartCol, _iCharsToReplace, _pszNewText, _iNewLen) = notimpl
              member __.RestrictViewRange(_iMinLine, _iMaxLine, _pClient) = notimpl
              member __.SendExplicitFocus() = notimpl
              member __.SetBuffer(_pBuffer) = notimpl
              member __.SetCaretPos(_iLine, _iColumn) = notimpl
              member __.SetScrollPosition(_iBar, _iFirstVisibleUnit) = notimpl
              member __.SetSelection(_iAnchorLine, _iAnchorCol, _iEndLine, _iEndCol) = notimpl
              member __.SetSelectionMode(_iSelMode) = notimpl
              member __.SetTopLine(_iBaseLine) = notimpl
              member __.UpdateCompletionStatus(_pCompSet, _dwFlags) = notimpl
              member __.UpdateTipWindow(_pTipWindow, _dwFlags) = notimpl
              member __.UpdateViewFrameCaption() = notimpl
              
              member __.AddCommandFilter(_pNewCmdTarg, ppNextCmdTarg) = 
                  ppNextCmdTarg <- createDummyCommandTarget()
                  VSConstants.S_OK
         }

    Mock<IVsEditorAdaptersFactoryService>()
        .Setup(fun x -> <@ x.GetViewAdapter (any()) @>)
        .Returns(vsTextView)
        .Create()

type OpenDocumentTrackerStub() =
    let mutable content: string = null
    let documentChanged = Event<_>()
    let documentClosed = Event<_>()
    member __.SetActiveDocumentContent newContent = content <- newContent
    interface IOpenDocumentsTracker with
        member __.RegisterView _ = ()
        member __.MapOpenDocuments _ = notimpl
        member __.TryFindOpenDocument _ = None
        member __.TryGetDocumentText _ = Some content
        member __.DocumentChanged = documentChanged.Publish
        member __.DocumentClosed = documentClosed.Publish