namespace FSharpVSPowerTools.Logic

open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.OLE.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open EnvDTE

type NoPeekDefinitionFilter() =
    interface IMenuCommand with
        member val IsAdded = false with get, set
        member val NextTarget = null with get, set

        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            if pguidCmdGroup = VSConstants.VsStd12 && 
                (nCmdId = uint32 VSConstants.VSStd12CmdID.PeekDefinition
                 || nCmdId = uint32 VSConstants.VSStd12CmdID.PeekNavigateBackward
                 || nCmdId = uint32 VSConstants.VSStd12CmdID.PeekNavigateForward) then
                int Constants.OLECMDERR_E_NOTSUPPORTED
            else
                let x = x :> IMenuCommand
                x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = VSConstants.VsStd12 && 
                prgCmds |> Seq.exists (fun x -> 
                               x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekDefinition
                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateBackward
                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateForward) then
                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_INVISIBLE)
                VSConstants.S_OK
            else
                let x = x :> IMenuCommand
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)

type AlwaysPeekDefinitionFilter() =
    interface IMenuCommand with
        member val IsAdded = false with get, set
        member val NextTarget = null with get, set

        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            let x = x :> IMenuCommand
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = VSConstants.VsStd12 && 
                prgCmds |> Seq.exists (fun x -> 
                               x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekDefinition
                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateBackward
                            || x.cmdID = uint32 VSConstants.VSStd12CmdID.PeekNavigateForward) then
                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                let x = x :> IMenuCommand
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.PrimaryDocument)>]
type NoPeekDefinitionFilterProvider [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider,
     editorFactory: IVsEditorAdaptersFactoryService) =
  
    let addCommandFilter (viewAdapter: IVsTextView) (commandFilter: IMenuCommand) =
        if not commandFilter.IsAdded then
            match viewAdapter.AddCommandFilter(commandFilter) with
            | VSConstants.S_OK, next ->
                commandFilter.IsAdded <- true
                match next with
                | null -> ()
                | _ -> commandFilter.NextTarget <- next
            | _ -> ()

    let vsVersion = 
        lazy(let dte = serviceProvider.GetService<DTE, DTE>()
             VisualStudioVersion.fromDTEVersion dte.Version)

    interface IWpfTextViewCreationListener with
        member __.TextViewCreated(textView) = 
            let textViewAdapter = editorFactory.GetViewAdapter(textView)
            match textViewAdapter with
            | null -> ()
            | _ ->
                if Setting.getGeneralOptions(serviceProvider) 
                   |> Option.ofNull 
                   |> Option.map (fun x -> x.PeekDefinitionEnabled) 
                   |> Option.getOrElse false then
                   match vsVersion.Value with
                   | VisualStudioVersion.Unknown
                   | VisualStudioVersion.VS2012
                   | VisualStudioVersion.VS2013 -> 
                        // Make sure that Peek Definition menu items are disabled on VS2013
                        addCommandFilter textViewAdapter (NoPeekDefinitionFilter())
                   | _ -> 
                       if Setting.getGlobalOptions(serviceProvider) 
                          |> Option.ofNull 
                          |> Option.map (fun x -> x.PeekStandaloneFilesEnabled) 
                          |> Option.getOrElse false then
                            // Only modify command chains if users explicitly choose this option
                            addCommandFilter textViewAdapter (AlwaysPeekDefinitionFilter())
        