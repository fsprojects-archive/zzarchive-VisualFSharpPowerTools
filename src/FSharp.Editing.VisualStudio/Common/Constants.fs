﻿[<RequireQualifiedAccess>] 
module FSharp.Editing.VisualStudio.Constants

open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell.Interop
open System

let guidPowerToolsOutputPane = Guid("3537F4ED-FC82-45C1-8D23-AEE4297B776B")
let guidLogicalTextView = Guid(LogicalViewID.TextView)

let guidNewFolderCmdSet = Guid "{9EDC1279-C317-43A6-B554-3A4D7853D55E}"
let cmdNewFolder = 0x1071
let cmdRenameFolder = 0x1072

let guidMoveCmdSet = Guid "{7B573ACF-2772-4F46-B290-9B0EA94CBFAB}"
let cmdMoveFolderUp = 0x1073
let cmdMoveFolderDown = 0x1074
let cmdMoveToFolder = 0x1075

let guidSolutionExplorerCmdSet = Guid("{1D4A7B65-A22C-4405-837B-4214C0BED3C5}")
let fsPowerToolsSubMenuGroup = 0x1061u

let guidOldStandardCmdSet = VSConstants.GUID_VSStandardCommandSet97
let cmdStandardNewFolder = uint32 VSConstants.VSStd97CmdID.NewFolder
let cmdStandardRenameFolder = uint32 VSConstants.VSStd97CmdID.Rename

let [<Literal>] fsharpUnusedDeclarationMargin = "FSharpUnusedDeclarationMargin"

let [<Literal>] fsharpReferenceType = "FSharp.ReferenceType"
let [<Literal>] fsharpValueType = "FSharp.ValueType"
let [<Literal>] fsharpPatternCase = "FSharp.PatternCase"
let [<Literal>] fsharpFunction = "FSharp.Function"
let [<Literal>] fsharpMutableVar = "FSharp.MutableVar"
let [<Literal>] fsharpQuotation = "FSharp.Quotation"
let [<Literal>] fsharpModule = "FSharp.Module"
let [<Literal>] fsharpUnused = "FSharp.Unused"
let [<Literal>] fsharpPrintf = "FSharp.Printf"
let [<Literal>] fsharpEscaped = "FSharp.Escaped"
let [<Literal>] fsharpOperator = "FSharp.Operator"

let [<Literal>] fsharpPrintfTagType = "MarkerFormatDefinition/HighlightPrintf"

let [<Literal>] depthAdornmentLayerName = "FSharpDepthFullLineAdornment"

let cmdidStandardRenameCommand = uint32 VSConstants.VSStd2KCmdID.RENAME // ECMD_RENAME
let guidStandardCmdSet = VSConstants.VSStd2K

let cmdidFindReferences = uint32 VSConstants.VSStd97CmdID.FindReferences
let guidSymbolLibrary = Guid("2ad4e2a2-b89f-48b6-98e8-363bd1a35450")
let [<Literal>] findReferencesResults = 0x11223344u

let cmdidGoToDefinition = uint32 VSConstants.VSStd97CmdID.GotoDefn

let [<Literal>] cmdidNextHighlightedReference = 2400u
let [<Literal>] cmdidPreviousHighlightedReference = 2401u

let [<Literal>] cmdidGenerateReferencesForFsi = 0x100u
let [<Literal>] guidAddReferenceInFSICmdSetString = "8c9a49dd-2d34-4d18-905b-c557692980be"
let guidGenerateReferencesForFsiCmdSet = Guid(guidAddReferenceInFSICmdSetString)
let [<Literal>] QuickInfoMargin = "vfpt.quickinfo.margin"
let [<Literal>] BreadcrumbBarMarginName = "vfpt.breadcrumb.margin"
let [<Literal>] LintTagErrorType = "F# Lint"
