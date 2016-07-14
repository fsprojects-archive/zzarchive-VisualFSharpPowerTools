namespace FSharp.Editing.VisualStudio.Outlining

open Microsoft.VisualStudio.OLE.Interop
open FSharpVSPowerTools
open Microsoft.VisualStudio

type OutliningFilter() =
    interface IMenuCommand with
        member val IsAdded = false with get, set
        member val NextTarget = null with get, set

        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            // We don't have to do anything here since most of the things are handled by VS.
            let x = x :> IMenuCommand
            x.NextTarget.Exec (&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> 
                               x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_COLLAPSE_TO_DEF
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_HIDE_SELECTION
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_START_AUTOHIDING
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_STOP_HIDING_ALL
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_STOP_HIDING_CURRENT
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_TOGGLE_ALL
                            || x.cmdID = uint32 VSConstants.VSStd2KCmdID.OUTLN_TOGGLE_CURRENT) then
                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                let x = x :> IMenuCommand
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)