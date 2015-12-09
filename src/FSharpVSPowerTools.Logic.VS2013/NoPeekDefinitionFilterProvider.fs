namespace FSharpVSPowerTools.Logic.VS2013

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.OLE.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open System.Windows.Forms
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open EnvDTE

//type NoPeekDefinitionFilter() =
//    member val IsAdded = false with get, set
//    member val NextTarget: IOleCommandTarget = null with get, set
//
//    interface IOleCommandTarget with
//        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
//            if pguidCmdGroup = VSConstants.VsStd12 && 
//                (nCmdId = uint32 VSConstants.VSStd12CmdID.PeekDefinition
//                 || nCmdId = uint32 VSConstants.VSStd12CmdID.PeekNavigateBackward
//                 || nCmdId = uint32 VSConstants.VSStd12CmdID.PeekNavigateForward) then
//                int Constants.OLECMDERR_E_NOTSUPPORTED
//            else
//                x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)
//
//        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
//            if pguidCmdGroup = VSConstants.VsStd12 && 
//                prgCmds |> Seq.exists (fun x -> 
//                               x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekDefinition
//                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateBackward
//                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateForward) then
//                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_INVISIBLE)
//                VSConstants.S_OK
//            else
//                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)
//
//[<Export(typeof<IVsTextViewCreationListener>)>]
//[<ContentType("F#")>]
//[<TextViewRole(PredefinedTextViewRoles.PrimaryDocument)>]
//type NoPeekDefinitionFilterProvider [<ImportingConstructor>] 
//    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider,
//     editorFactory: IVsEditorAdaptersFactoryService) =
//  
//    let addCommandFilter (viewAdapter: IVsTextView) (commandFilter: NoPeekDefinitionFilter) =
//        if not commandFilter.IsAdded then
//            match viewAdapter.AddCommandFilter(commandFilter) with
//            | VSConstants.S_OK, next ->
//                commandFilter.IsAdded <- true
//                match next with
//                | null -> ()
//                | _ -> commandFilter.NextTarget <- next
//            | _ -> ()
//
//    let vsVersion = 
//        lazy(let dte = serviceProvider.GetService<DTE, DTE>()
//             VisualStudioVersion.fromDTEVersion dte.Version)
//
//    interface IVsTextViewCreationListener with
//        member __.VsTextViewCreated(textViewAdapter) = 
//            let textView = editorFactory.GetWpfTextView(textViewAdapter)
//            match textView with
//            | null -> ()
//            | _ ->
//                if Setting.getGeneralOptions(serviceProvider) 
//                   |> Option.ofNull 
//                   |> Option.map (fun x -> x.PeekDefinitionEnabled) 
//                   |> Option.getOrElse false then
//                   match vsVersion.Value with
//                   | VisualStudioVersion.Unknown
//                   | VisualStudioVersion.VS2012
//                   | VisualStudioVersion.VS2013 -> 
//                        // Make sure that Peek Definition menu items are disabled on VS2013
//                        addCommandFilter textViewAdapter (new NoPeekDefinitionFilter())
//                   | _ -> ()
//        