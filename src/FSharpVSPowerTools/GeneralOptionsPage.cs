﻿using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Configuration;
using System.Runtime.InteropServices;
using System.Security.Principal;
using System.Windows.Forms;
using Microsoft.VisualStudio.ComponentModelHost;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")]
    public class GeneralOptionsPage : DialogPage, IGeneralOptions
    {   
        private GeneralOptionsControl _optionsControl;
        private const string navBarConfig = "fsharp-navigationbar-enabled";
        private bool _navBarEnabledInAppConfig;

        private readonly Logger logger;

        public GeneralOptionsPage()
        {
            var componentModel = Package.GetGlobalService(typeof(SComponentModel)) as IComponentModel;
            logger = componentModel.DefaultExportProvider.GetExportedValue<Logger>();

            XmlDocEnabled = true;
            FormattingEnabled = true;
            _navBarEnabledInAppConfig = GetNavigationBarConfig();
            HighlightUsageEnabled = true;
            RenameRefactoringEnabled = true;
            DepthColorizerEnabled = false;
            NavigateToEnabled = true;
            SyntaxColoringEnabled = true;
            InterfaceImplementationEnabled = true;
            FolderOrganizationEnabled = false;
            FindAllReferencesEnabled = true;
            GenerateRecordStubEnabled = true;
            UnionPatternMatchCaseGenerationEnabled = true;
            ResolveUnopenedNamespacesEnabled = true;
            UnusedReferencesEnabled = false;
            UnusedOpensEnabled = false;
            TaskListCommentsEnabled = true;
            GoToMetadataEnabled = true;
            GenerateReferencesEnabled = true;
            CSharpToFSharpEnabled = true;
        }

        private bool GetNavigationBarConfig()
        {
            try
            {
                var config = ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath);
                var configValue = config.AppSettings.Settings[navBarConfig];
                bool result;
                return configValue != null && bool.TryParse(configValue.Value, out result) ? result : false;
            }
            catch(Exception ex)
            {
                logger.LogException(ex);
                return false;
            }
        }

        private bool IsUserAdministrator()
        {
            bool isAdmin;
            try
            {
                // Get the currently logged in user
                WindowsIdentity user = WindowsIdentity.GetCurrent();
                WindowsPrincipal principal = new WindowsPrincipal(user);
                isAdmin = principal.IsInRole(WindowsBuiltInRole.Administrator);
            }
            catch (UnauthorizedAccessException)
            {
                isAdmin = false;
            }

            return isAdmin;
        }

        // Return true if navigation bar config is set successfully
        private bool SetNavigationBarConfig(bool v)
        {
            try
            {
                if (IsUserAdministrator())
                {
                    var config = ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath);
                    config.AppSettings.Settings.Remove(navBarConfig);
                    config.AppSettings.Settings.Add(navBarConfig, v.ToString().ToLower());
                    config.Save(ConfigurationSaveMode.Minimal);
                    
                    return true;
                }
                else
                {
                    logger.MessageBox(LogType.Error, Resource.navBarUnauthorizedMessage);
                    return false;
                }
            }
            catch (Exception ex)
            {
                logger.MessageBox(LogType.Error, Resource.navBarErrorMessage);
                logger.LogException(ex);
                return false;
            }
        }

        // We are letting Visual Studio know that these property value needs to be persisted	       

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XmlDocEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FormattingEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool NavBarEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool HighlightUsageEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool RenameRefactoringEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool DepthColorizerEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool NavigateToEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool SyntaxColoringEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool InterfaceImplementationEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FolderOrganizationEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FindAllReferencesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool GenerateRecordStubEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool UnionPatternMatchCaseGenerationEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool ResolveUnopenedNamespacesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool UnusedReferencesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool UnusedOpensEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TaskListCommentsEnabled { get; set; }

	    [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool GoToMetadataEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool GenerateReferencesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CSharpToFSharpEnabled { get; set; }

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                _optionsControl = new GeneralOptionsControl(this);
                return _optionsControl;
            }
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so         
        // that Visual Studio saves it.        
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                XmlDocEnabled = _optionsControl.XmlDocEnabled;
                FormattingEnabled = _optionsControl.FormattingEnabled;

                if (NavBarEnabled != _optionsControl.NavBarEnabled && SetNavigationBarConfig(_optionsControl.NavBarEnabled))
                {
                    NavBarEnabled = _optionsControl.NavBarEnabled;
                    _navBarEnabledInAppConfig = _optionsControl.NavBarEnabled;
                }

                HighlightUsageEnabled = _optionsControl.HighlightUsageEnabled;
                RenameRefactoringEnabled = _optionsControl.RenameRefactoringEnabled;
                DepthColorizerEnabled = _optionsControl.DepthColorizerEnabled;
                NavigateToEnabled = _optionsControl.NavigateToEnabled;
                SyntaxColoringEnabled = _optionsControl.SyntaxColoringEnabled;
                InterfaceImplementationEnabled = _optionsControl.InterfaceImplementationEnabled;
                FolderOrganizationEnabled = _optionsControl.FolderOrganizationEnabled;
                FindAllReferencesEnabled = _optionsControl.FindAllReferencesEnabled;
                GenerateRecordStubEnabled = _optionsControl.GenerateRecordStubEnabled;
                UnionPatternMatchCaseGenerationEnabled = _optionsControl.UnionPatternMatchCaseGenerationEnabled;
                ResolveUnopenedNamespacesEnabled = _optionsControl.ResolveUnopenedNamespacesEnabled;
                UnusedReferencesEnabled = _optionsControl.UnusedReferencesEnabled;
                UnusedOpensEnabled = _optionsControl.UnusedOpensEnabled;
                TaskListCommentsEnabled = _optionsControl.TaskListCommentsEnabled;
                GoToMetadataEnabled = _optionsControl.GoToMetadataEnabled;
                GenerateReferencesEnabled = _optionsControl.GenerateReferencesEnabled;
                CSharpToFSharpEnabled = _optionsControl.CSharpToFSharpEnabled;
            }

            base.OnApply(e);
        }
    }
}
