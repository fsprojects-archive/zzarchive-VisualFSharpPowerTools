namespace FSharpVSPowerTools.Outlining

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

type OutliningFilter(_textView: IWpfTextView, 
                     _outliningManagerService: IOutliningManagerService) =

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            if (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidOutliningToggleCurrent) then
                MessageBox.Show("1") |> ignore
            elif (pguidCmdGroup = VSConstants.VsStd2010 && nCmdId = Constants.cmdidOutliningExpandAll) then
                MessageBox.Show("2") |> ignore
            elif (pguidCmdGroup = VSConstants.VsStd2010 && nCmdId = Constants.cmdidOutliningCollapseAll) then
                MessageBox.Show("3") |> ignore
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidOutliningToggleCurrent) then
                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_ENABLED &&& ~~~ OLECMDF.OLECMDF_INVISIBLE)
                VSConstants.S_OK
            elif pguidCmdGroup = VSConstants.VsStd2010 && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidOutliningExpandAll 
                                                  || x.cmdID = Constants.cmdidOutliningCollapseAll) then
                prgCmds.[0].cmdf <- uint32 (OLECMDF.OLECMDF_SUPPORTED ||| OLECMDF.OLECMDF_ENABLED &&& ~~~ OLECMDF.OLECMDF_INVISIBLE)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)