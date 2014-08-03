[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio

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