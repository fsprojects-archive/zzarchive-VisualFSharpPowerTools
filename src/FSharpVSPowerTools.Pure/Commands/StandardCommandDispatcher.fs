namespace FSharpVSPowerTools

open System
open System.Diagnostics
open Fantomas
open FSharpVSPowerTools.CodeFormatting
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop


type private CommandMapping (commandGroup:Guid, commandId:int, commandOptions:OLECMDF, commandCreator:unit->CommandBase) =

    member __.Matches (otherCommandGroup:Guid, otherCommandId:int) =
            commandGroup = otherCommandGroup && commandId = otherCommandId
    
    member __.GetMatchingIndex (oCommandGroup:Guid,commands:OLECMD []) =
        if commandGroup = oCommandGroup then
            commands |> Array.findIndex (fun cmd-> cmd.cmdID = uint32 commandId)
        else -1

    member __.CommandOptions with get () = uint32 commandOptions
   
    member __.CreateCommand () = commandCreator ()


type StandardCommandDispatcher private () as self =

    [<DefaultValue>] val mutable private textView       : IWpfTextView
    [<DefaultValue>] val mutable private codeServices   : CodeFormattingServices



//    let mutable codeServices    = Unchecked.defaultof<CodeFormattingServices>
    let mutable commandChain    = Unchecked.defaultof<IOleCommandTarget>
    let mutable commands        = [||]


    member __.GetConfig () =
        let editorOptions = self.codeServices.EditorOptionsFactory.GetOptions self.textView.TextBuffer
        let indentSize    = editorOptions.GetOptionValue (IndentSize ()).Key
        let customOptions = Setting.getFormattingOptions self.codeServices.ServiceProvider

        FormatConfig.FormatConfig.create
            (   indentSize                              ,
                customOptions.PageWidth                 ,
                customOptions.SemicolonAtEndOfLine      ,
                customOptions.SpaceBeforeArgument       ,
                customOptions.SpaceBeforeColon          ,
                customOptions.SpaceAfterComma           ,
                customOptions.SpaceAfterSemicolon       ,
                customOptions.IndentOnTryWith           ,
                customOptions.ReorderOpenDeclaration    ,
                customOptions.SpaceAroundDelimiter      )


    member private __.StandardCommandDispatcher () =
        commands <-
            [|  CommandMapping
                    (   typeof<VSConstants.VSStd2KCmdID>.GUID,
                        int VSConstants.VSStd2KCmdID.FORMATDOCUMENT,
                        OLECMDF.OLECMDF_ENABLED ||| OLECMDF.OLECMDF_SUPPORTED,
                        fun () -> FormatDocumentCommand (Func<_>(self.GetConfig)) :> CommandBase )
                CommandMapping
                    (   typeof<VSConstants.VSStd2KCmdID>.GUID,
                        int VSConstants.VSStd2KCmdID.FORMATSELECTION,
                        OLECMDF.OLECMDF_ENABLED ||| OLECMDF.OLECMDF_SUPPORTED,
                        fun () -> FormatDocumentCommand (Func<_>(self.GetConfig)) :> CommandBase )
            |]

    // this probably shouldn't be an option
    static member Register (interopTextView :IVsTextView,  
                            textView        :IWpfTextView, 
                            codeServices    :CodeFormattingServices ) : StandardCommandDispatcher option =
        //let generalOptions = Setting.getGeneralOptions codeServices.ServiceProvider
        //Unchecked.defaultof<StandardCommandDispatcher>
        maybe{
         //   let!  generalOptions = Setting.tryGetGeneralOptions codeServices.ServiceProvider
 //               return!
           //     if not generalOptions.FormattingEnabled then None else
                let dispatcher = StandardCommandDispatcher()
                dispatcher.textView <- textView
                return!
                    Some dispatcher
        }
 

        
    interface IOleCommandTarget with

        member __.QueryStatus (pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD [], pCmdText: nativeint): int = 
            for commandMapping in commands do
                let fmtindex = commandMapping.GetMatchingIndex (pguidCmdGroup, prgCmds)
                if  fmtindex >= 0 then
                    prgCmds.[fmtindex].cmdf <- commandMapping.CommandOptions
                    VSConstants.S_OK 
                    |> ignore // THIS PART IS PROBABLY VERY WRONG
            commandChain.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText)           


        member __.Exec (pguidCmdGroup: byref<Guid>, nCmdID: uint32, nCmdexecopt: uint32, pvaIn: nativeint, pvaOut: nativeint): int = 
            for commandMapping in commands do
                let command = commandMapping.CreateCommand()
                command.TextView <- self.textView
                command.Services <- self.codeServices
                command.Execute()
                VSConstants.S_OK 
                |> ignore // THIS PART IS PROBABLY VERY WRONG
            commandChain.Exec(ref  pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut)           


