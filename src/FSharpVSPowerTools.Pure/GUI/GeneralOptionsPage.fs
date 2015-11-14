namespace FSharpVSPowerTools

open Microsoft.VisualStudio.Shell
open System
open System.ComponentModel
open System.Configuration
open System.Runtime.InteropServices
open System.Security.Principal
open System.Windows
open System.Windows.Forms
open Microsoft.VisualStudio.ComponentModelHost
open FSharpVSPowerTools



[<AutoOpen>]
module internal Utils =
    open System.Configuration
    open FSharpVSPowerTools

    type ConfigurationManager with
    
        /// Trys to get the config of a Windows Forms Application
        static member TryOpenExeConfiguration (): Configuration option =
            let config = ConfigurationManager.OpenExeConfiguration(System.Windows.Forms.Application.ExecutablePath)
            if isNull config then None else Some config
    type KeyValueConfigurationCollection with
        /// Returns an Option that may contain the value associated with the key in this collection
        member kvc.TryFind key =
            Option.ofNull kvc.[key]
            
            



[<ClassInterface(ClassInterfaceType.AutoDual)>]
[<ComVisible true>]
[<CLSCompliant(false)>]
[<Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")>]
type GeneralOptionsPage () as self =
    inherit UIElementDialogPage () 

    let mutable generalOptionsControl = Unchecked.defaultof<_> 
    
    let componentModel = Package.GetGlobalService(typeof<SComponentModel>) :?> IComponentModel 
        
    

    let [<Literal>] navBarConfig = "fsharp-navigationbar-enabled"       

    let getNavigationBarConfig () =
        maybe {
            let! config = ConfigurationManager.TryOpenExeConfiguration ()
            let! configValue = string config |> config.AppSettings.Settings.TryFind 
            let mutable result = false
            bool.TryParse(configValue.Value, &result ) |> ignore
            return result
        } |> Option.getOrElse false


    let mutable xmlDocEnabled = true 
    let mutable formattingEnabled                      = true                       
    let mutable navBarEnabledInAppConfig               = getNavigationBarConfig()   
    let mutable highlightUsageEnabled                  = true                       
    let mutable renameRefactoringEnabled               = true                       
    let mutable depthColorizerEnabled                  = false                      
    let mutable navigateToEnabled                      = true                       
    let mutable syntaxColoringEnabled                  = true                       
    let mutable interfaceImplementationEnabled         = true                       
    let mutable folderOrganizationEnabled              = false                      
    let mutable findAllReferencesEnabled               = true                       
    let mutable generateRecordStubEnabled              = true                       
    let mutable unionPatternMatchCaseGenerationEnabled = true                       
    let mutable resolveUnopenedNamespacesEnabled       = true                       
    let mutable unusedReferencesEnabled                = false                      
    let mutable unusedOpensEnabled                     = false                      
    let mutable taskListCommentsEnabled                = true                       
    let mutable goToMetadataEnabled                    = true                       
    let mutable generateReferencesEnabled              = true                       
    let mutable goToSymbolSourceEnabled                = true                       
    let mutable quickInfoPanelEnabled                  = true                       
    let mutable linterEnabled                          = true                       
    let mutable outliningEnabled                       = false                      
    



    override __.Child 
        with get () = generalOptionsControl :> UIElement

    override __.OnActivate cancelArgs =
        generalOptionsControl <- GeneralOptionsControlView ()
        generalOptionsControl.DataContext <- GeneralOptionsViewModel ()
        
        base.OnActivate cancelArgs

    override __.OnApply pageApplyArgs =
        base.OnApply pageApplyArgs

    [<Browsable(false); DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>]
    override __.Window
        with get () = self:> IWin32Window

    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.XmlDocEnabled with get() = xmlDocEnabled and set v = xmlDocEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.FormattingEnabled with get() = formattingEnabled and set v = formattingEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.NavBarEnabled with get() = navBarEnabledInAppConfig and set v = navBarEnabledInAppConfig <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.HighlightUsageEnabled with get() = highlightUsageEnabled and set v = highlightUsageEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.RenameRefactoringEnabled with get() = renameRefactoringEnabled and set v = renameRefactoringEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.DepthColorizerEnabled with get() = depthColorizerEnabled and set v = depthColorizerEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.NavigateToEnabled with get() = navigateToEnabled and set v = navigateToEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.SyntaxColoringEnabled with get() = syntaxColoringEnabled and set v = syntaxColoringEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.InterfaceImplementationEnabled with get() = interfaceImplementationEnabled and set v = interfaceImplementationEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.FolderOrganizationEnabled with get() = folderOrganizationEnabled and set v = folderOrganizationEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.FindAllReferencesEnabled with get() = findAllReferencesEnabled and set v = findAllReferencesEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.GenerateRecordStubEnabled with get() = generateRecordStubEnabled and set v = generateRecordStubEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.UnionPatternMatchCaseGenerationEnabled with get() = unionPatternMatchCaseGenerationEnabled and set v = unionPatternMatchCaseGenerationEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.ResolveUnopenedNamespacesEnabled with get() = resolveUnopenedNamespacesEnabled and set v = resolveUnopenedNamespacesEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.UnusedReferencesEnabled with get() = unusedReferencesEnabled and set v = unusedReferencesEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.UnusedOpensEnabled with get() = unusedOpensEnabled and set v = unusedOpensEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.TaskListCommentsEnabled with get() = taskListCommentsEnabled and set v = taskListCommentsEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.GoToMetadataEnabled with get() = goToMetadataEnabled and set v = goToMetadataEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.GenerateReferencesEnabled with get() = generateReferencesEnabled and set v = generateReferencesEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.GoToSymbolSourceEnabled with get() = goToSymbolSourceEnabled and set v = goToSymbolSourceEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.QuickInfoPanelEnabled with get() = quickInfoPanelEnabled and set v = quickInfoPanelEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.LinterEnabled with get() = linterEnabled and set v = linterEnabled <- v
    [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
    member __.OutliningEnabled with get() = outliningEnabled and set v = outliningEnabled <- v        

    interface IGeneralOptions with
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.XmlDocEnabled                          with get () = self.XmlDocEnabled                           and set v = self.XmlDocEnabled                            <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.FormattingEnabled                      with get () = self.FormattingEnabled                       and set v = self.FormattingEnabled                        <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.NavBarEnabled                          with get () = self.NavBarEnabled                           and set v = self.NavBarEnabled                            <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.HighlightUsageEnabled                  with get () = self.HighlightUsageEnabled                   and set v = self.HighlightUsageEnabled                    <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.RenameRefactoringEnabled               with get () = self.RenameRefactoringEnabled                and set v = self.RenameRefactoringEnabled                 <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.DepthColorizerEnabled                  with get () = self.DepthColorizerEnabled                   and set v = self.DepthColorizerEnabled                    <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.NavigateToEnabled                      with get () = self.NavigateToEnabled                       and set v = self.NavigateToEnabled                        <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.SyntaxColoringEnabled                  with get () = self.SyntaxColoringEnabled                   and set v = self.SyntaxColoringEnabled                    <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.InterfaceImplementationEnabled         with get () = self.InterfaceImplementationEnabled          and set v = self.InterfaceImplementationEnabled           <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.FolderOrganizationEnabled              with get () = self.FolderOrganizationEnabled               and set v = self.FolderOrganizationEnabled                <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.FindAllReferencesEnabled               with get () = self.FindAllReferencesEnabled                and set v = self.FindAllReferencesEnabled                 <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.GenerateRecordStubEnabled              with get () = self.GenerateRecordStubEnabled               and set v = self.GenerateRecordStubEnabled                <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.UnionPatternMatchCaseGenerationEnabled with get () = self.UnionPatternMatchCaseGenerationEnabled  and set v = self.UnionPatternMatchCaseGenerationEnabled   <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.ResolveUnopenedNamespacesEnabled       with get () = self.ResolveUnopenedNamespacesEnabled        and set v = self.ResolveUnopenedNamespacesEnabled         <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.UnusedReferencesEnabled                with get () = self.UnusedReferencesEnabled                 and set v = self.UnusedReferencesEnabled                  <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.UnusedOpensEnabled                     with get () = self.UnusedOpensEnabled                      and set v = self.UnusedOpensEnabled                       <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.TaskListCommentsEnabled                with get () = self.TaskListCommentsEnabled                 and set v = self.TaskListCommentsEnabled                  <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.GoToMetadataEnabled                    with get () = self.GoToMetadataEnabled                     and set v = self.GoToMetadataEnabled                      <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.GenerateReferencesEnabled              with get () = self.GenerateReferencesEnabled               and set v = self.GenerateReferencesEnabled                <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.GoToSymbolSourceEnabled                with get () = self.GoToSymbolSourceEnabled                 and set v = self.GoToSymbolSourceEnabled                  <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.QuickInfoPanelEnabled                  with get () = self.QuickInfoPanelEnabled                   and set v = self.QuickInfoPanelEnabled                    <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.LinterEnabled                          with get () = self.LinterEnabled                           and set v = self.LinterEnabled                            <- v
        [<DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>]
        member __.OutliningEnabled                       with get () = self.OutliningEnabled                        and set v = self.OutliningEnabled                         <- v


        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so
        // that Visual Studio saves it.
//        override __.OnApply (e:DialogPage.PageApplyEventArgs) =
//            if e.ApplyBehavior = self.ApplyKind.Apply then
//                if self.NavBarEnabled <> optionsControl.NavBarEnabled then
//                    if not self.SetNavigationBarConfig (optionsControl.NavBarEnabled) then
//                        // Keep the dialog open in the case of errors
//                        e.ApplyBehavior = ApplyKind.CancelNoNavigate
//                        base.OnApply(e)
//
//                    //NavBarEnabled = _optionsControl.NavBarEnabled
//                   // _navBarEnabledInAppConfig = _optionsControl.NavBarEnabled
//            base.OnApply(e)
//        
//
//        private bool IsUserAdministrator()
//        {
//            bool isAdmin
//            try
//            {
//                // Get the currently logged in user
//                WindowsIdentity user = WindowsIdentity.GetCurrent()
//                WindowsPrincipal principal = new WindowsPrincipal(user)
//                isAdmin = principal.IsInRole(WindowsBuiltInRole.Administrator)
//            }
//            catch (UnauthorizedAccessException)
//            {
//                isAdmin = false
//            }
//
//            return isAdmin
//        }
//
//        // Return true if navigation bar config is set successfully
//        private bool SetNavigationBarConfig(bool v)
//        {
//            try
//            {
//                if (IsUserAdministrator())
//                {
//                    var config = ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath)
//                    config.AppSettings.Settings.Remove(navBarConfig)
//                    config.AppSettings.Settings.Add(navBarConfig, v.ToString().ToLower())
//                    config.Save(ConfigurationSaveMode.Minimal)
//
//                    return true
//                }
//                else
//                {
//                    LoggingModule.messageBoxError(Resource.navBarUnauthorizedMessage)
//                    return false
//                }
//            }
//            catch (Exception ex)
//            {
//                LoggingModule.messageBoxError(Resource.navBarErrorMessage)
//                LoggingModule.logException(ex)
//                return false
//            }
//        }
//

//
//        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
//        protected override IWin32Window Window
//        {
//            get
//            {
//                _optionsControl = new GeneralOptionsControl(this)
//                return _optionsControl
//            }
//        }
//
//        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so
//        // that Visual Studio saves it.
//        protected override void OnApply(DialogPage.PageApplyEventArgs e)
//        {
//            if (e.ApplyBehavior == ApplyKind.Apply)
//            {
//                if (NavBarEnabled != _optionsControl.NavBarEnabled)
//                {
//                    if (!SetNavigationBarConfig(_optionsControl.NavBarEnabled))
//                    {
//                        // Keep the dialog open in the case of errors
//                        e.ApplyBehavior = ApplyKind.CancelNoNavigate
//                        base.OnApply(e)
//                        return
//                    }
//
//                    NavBarEnabled = _optionsControl.NavBarEnabled
//                    _navBarEnabledInAppConfig = _optionsControl.NavBarEnabled
//                }
//
//                XmlDocEnabled = _optionsControl.XmlDocEnabled
//                FormattingEnabled = _optionsControl.FormattingEnabled
//
//                HighlightUsageEnabled = _optionsControl.HighlightUsageEnabled
//                RenameRefactoringEnabled = _optionsControl.RenameRefactoringEnabled
//                DepthColorizerEnabled = _optionsControl.DepthColorizerEnabled
//                NavigateToEnabled = _optionsControl.NavigateToEnabled
//                SyntaxColoringEnabled = _optionsControl.SyntaxColoringEnabled
//                InterfaceImplementationEnabled = _optionsControl.InterfaceImplementationEnabled
//                FolderOrganizationEnabled = _optionsControl.FolderOrganizationEnabled
//                FindAllReferencesEnabled = _optionsControl.FindAllReferencesEnabled
//                GenerateRecordStubEnabled = _optionsControl.GenerateRecordStubEnabled
//                UnionPatternMatchCaseGenerationEnabled = _optionsControl.UnionPatternMatchCaseGenerationEnabled
//                ResolveUnopenedNamespacesEnabled = _optionsControl.ResolveUnopenedNamespacesEnabled
//                UnusedReferencesEnabled = _optionsControl.UnusedReferencesEnabled
//                UnusedOpensEnabled = _optionsControl.UnusedOpensEnabled
//                TaskListCommentsEnabled = _optionsControl.TaskListCommentsEnabled
//                GoToMetadataEnabled = _optionsControl.GoToMetadataEnabled
//                GenerateReferencesEnabled = _optionsControl.GenerateReferencesEnabled
//                GoToSymbolSourceEnabled = _optionsControl.GoToSymbolSourceEnabled
//                QuickInfoPanelEnabled = _optionsControl.QuickInfoPanelEnabled
//                LinterEnabled = _optionsControl.LinterEnabled
//                OutliningEnabled = _optionsControl.OutliningEnabled
//            }
//
//            base.OnApply(e)
//        }
//    }
//}