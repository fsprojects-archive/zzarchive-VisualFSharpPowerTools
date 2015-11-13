namespace FSharpVSPowerTools

open System
open System.Windows
open FSharpVSPowerTools
open FSharp.ViewModule
open System.Configuration
open FSharpVSPowerTools
open FSharpVSPowerTools.GuiUtils

type CheckBoxData =
    {   Label : string
        Tooltip : string    
        mutable Enabled : bool
    }


[<AutoOpen>]
module internal Config =

    let xmlDocData = { 
        Label   = "Auto-Generate Xml Documentation Comment"
        Tooltip = @"typing `\\\<` above a type, function, or binding triggers the generation of an XML documentation comment for that construct"
        Enabled = true    
    }

    let sourceFormatData = {
        Label   = "Source Code Formatting"
        Tooltip = "Use the Fantomas Source code fomatting library to restructure your code according to it's rules"
        Enabled =  true
    }

    let navBarData = {
        Label   = "Navigation Bar"
        Tooltip = "Must be admin to enable, restart to see it in action"
        Enabled = false
    }

    let highlightRefData = {
        Label   = "Highlight References"
        Tooltip = ""
        Enabled = true
    }

//    let renameData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let depthColorData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let navigateData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let syntaxColorData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let greyOpenData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let greyDeclData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }
//
//    let sourceFormatData = {
//        Label   =
//        Tooltip =
//        Enabled =
//    }

    let x = ()



type public GeneralOptionsViewModel () as self =
    inherit ViewModelBase ()

    let setBacking arg1 arg2 =  self.Factory.Backing(arg1, arg2)
    let mkCmd func = self.Factory.CommandSync func
    let mkCmdParam func = self.Factory.CommandSyncParam func

 //   let mutable xmlDocEnabled                          = setBacking <@ self.XmlDocEnabled @>  xmlDocData
    let mutable formattingEnabled                      = setBacking <@ self.FormattingEnabled                         @>  true                       
    //let mutable navBarEnabledInAppConfig               setBacking <@ self.NavBarEnabled                             @>  = ge                                       tNavigationBarConfig()   
    let mutable highlightUsageEnabled                  = setBacking <@ self.HighlightUsageEnabled                     @>  true                       
    let mutable renameRefactoringEnabled               = setBacking <@ self.RenameRefactoringEnabled                  @>  true                       
    let mutable depthColorizerEnabled                  = setBacking <@ self.DepthColorizerEnabled                     @>  false                      
    let mutable navigateToEnabled                      = setBacking <@ self.NavigateToEnabled                         @>  true                       
    let mutable syntaxColoringEnabled                  = setBacking <@ self.SyntaxColoringEnabled                     @>  true                       
    let mutable interfaceImplementationEnabled         = setBacking <@ self.InterfaceImplementationEnabled            @>  true                       
    let mutable folderOrganizationEnabled              = setBacking <@ self.FolderOrganizationEnabled                 @>  false                      
    let mutable findAllReferencesEnabled               = setBacking <@ self.FindAllReferencesEnabled                  @>  true                       
    let mutable generateRecordStubEnabled              = setBacking <@ self.GenerateRecordStubEnabled                 @>  true                       
    let mutable unionPatternMatchCaseGenerationEnabled = setBacking <@ self.UnionPatternMatchCaseGenerationEnabled    @>  true                       
    let mutable resolveUnopenedNamespacesEnabled       = setBacking <@ self.ResolveUnopenedNamespacesEnabled          @>  true                       
    let mutable unusedReferencesEnabled                = setBacking <@ self.UnusedReferencesEnabled                   @>  false                      
    let mutable unusedOpensEnabled                     = setBacking <@ self.UnusedOpensEnabled                        @>  false                      
    let mutable taskListCommentsEnabled                = setBacking <@ self.TaskListCommentsEnabled                   @>  true                       
    let mutable goToMetadataEnabled                    = setBacking <@ self.GoToMetadataEnabled                       @>  true                       
    let mutable generateReferencesEnabled              = setBacking <@ self.GenerateReferencesEnabled                 @>  true                       
    let mutable goToSymbolSourceEnabled                = setBacking <@ self.GoToSymbolSourceEnabled                   @>  true                       
    let mutable quickInfoPanelEnabled                  = setBacking <@ self.QuickInfoPanelEnabled                     @>  true                       
    let mutable linterEnabled                          = setBacking <@ self.LinterEnabled                             @>  true                       
    let mutable outliningEnabled                       = setBacking <@ self.OutliningEnabled                          @>  false         
    
    
   // member __.XmlDocEnabled                          with get () = xmlDocEnabled                         .Value  and set v =  xmlDocEnabled                          .Value <- v
    member __.FormattingEnabled                      with get () = formattingEnabled                     .Value  and set v =  formattingEnabled                      .Value <- v
  //  member __.NavBarEnabled                          with get () =le navBarEnabledInAppConfig          .Value    and set v = le navBarEnabledInAppConfig           .Value   <- v
    member __.HighlightUsageEnabled                  with get () = highlightUsageEnabled                 .Value  and set v =  highlightUsageEnabled                  .Value <- v
    member __.RenameRefactoringEnabled               with get () = renameRefactoringEnabled              .Value  and set v =  renameRefactoringEnabled               .Value <- v
    member __.DepthColorizerEnabled                  with get () = depthColorizerEnabled                 .Value  and set v =  depthColorizerEnabled                  .Value <- v
    member __.NavigateToEnabled                      with get () = navigateToEnabled                     .Value  and set v =  navigateToEnabled                      .Value <- v
    member __.SyntaxColoringEnabled                  with get () = syntaxColoringEnabled                 .Value  and set v =  syntaxColoringEnabled                  .Value <- v
    member __.InterfaceImplementationEnabled         with get () = interfaceImplementationEnabled        .Value  and set v =  interfaceImplementationEnabled         .Value <- v
    member __.FolderOrganizationEnabled              with get () = folderOrganizationEnabled             .Value  and set v =  folderOrganizationEnabled              .Value <- v
    member __.FindAllReferencesEnabled               with get () = findAllReferencesEnabled              .Value  and set v =  findAllReferencesEnabled               .Value <- v
    member __.GenerateRecordStubEnabled              with get () = generateRecordStubEnabled             .Value  and set v =  generateRecordStubEnabled              .Value <- v
    member __.UnionPatternMatchCaseGenerationEnabled with get () = unionPatternMatchCaseGenerationEnabled.Value  and set v =  unionPatternMatchCaseGenerationEnabled .Value <- v
    member __.ResolveUnopenedNamespacesEnabled       with get () = resolveUnopenedNamespacesEnabled      .Value  and set v =  resolveUnopenedNamespacesEnabled       .Value <- v
    member __.UnusedReferencesEnabled                with get () = unusedReferencesEnabled               .Value  and set v =  unusedReferencesEnabled                .Value <- v
    member __.UnusedOpensEnabled                     with get () = unusedOpensEnabled                    .Value  and set v =  unusedOpensEnabled                     .Value <- v
    member __.TaskListCommentsEnabled                with get () = taskListCommentsEnabled               .Value  and set v =  taskListCommentsEnabled                .Value <- v
    member __.GoToMetadataEnabled                    with get () = goToMetadataEnabled                   .Value  and set v =  goToMetadataEnabled                    .Value <- v
    member __.GenerateReferencesEnabled              with get () = generateReferencesEnabled             .Value  and set v =  generateReferencesEnabled              .Value <- v
    member __.GoToSymbolSourceEnabled                with get () = goToSymbolSourceEnabled               .Value  and set v =  goToSymbolSourceEnabled                .Value <- v
    member __.QuickInfoPanelEnabled                  with get () = quickInfoPanelEnabled                 .Value  and set v =  quickInfoPanelEnabled                  .Value <- v
    member __.LinterEnabled                          with get () = linterEnabled                         .Value  and set v =  linterEnabled                          .Value <- v
    member __.OutliningEnabled                       with get () = outliningEnabled                      .Value  and set v =  outliningEnabled                       .Value <- v             
    


type GeneralOptionsDesignVM () =
    inherit GeneralOptionsViewModel ()