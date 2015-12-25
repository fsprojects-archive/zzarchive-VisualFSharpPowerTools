﻿using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Configuration;
using System.Runtime.InteropServices;
using System.Security.Principal;
using System.Windows.Forms;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Shell.Interop;
using EnvDTE;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")]
    public class GeneralOptionsPage : DialogPage, IGeneralOptions
    {
        GeneralOptionsControl _optionsControl;
        IGeneralOptions settings;
        const string navBarConfig = "fsharp-navigationbar-enabled";
        //bool _navBarEnabledInAppConfig;
        //                                           [ImportingConstructor]
        //public LintTaggerProvider(
        //    [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
        //[ImportingConstructor]
        //public GeneralOptionsPage ( IGeneralOptions _settings){
        //    settings = _settings;
        //}


        public GeneralOptionsPage()
        {
            base.LoadSettingsFromStorage();
            settings = VFPT_Settings.getGeneralOptions();
            XmlDocEnabled                               = settings.XmlDocEnabled                                  ;
            FormattingEnabled                           = settings.FormattingEnabled                              ;
            NavBarEnabled                               = settings.NavBarEnabled                                  ;
            HighlightUsageEnabled                       = settings.HighlightUsageEnabled                          ;
            HighlightPrintfUsageEnabled                 = settings.HighlightPrintfUsageEnabled                    ;
            RenameRefactoringEnabled                    = settings.RenameRefactoringEnabled                       ;
            DepthColorizerEnabled                       = settings.DepthColorizerEnabled                          ;
            NavigateToEnabled                           = settings.NavigateToEnabled                              ;
            SyntaxColoringEnabled                       = settings.SyntaxColoringEnabled                          ;
            InterfaceImplementationEnabled              = settings.InterfaceImplementationEnabled                 ;
            FolderOrganizationEnabled                   = settings.FolderOrganizationEnabled                      ;
            FindAllReferencesEnabled                    = settings.FindAllReferencesEnabled                       ;
            GenerateRecordStubEnabled                   = settings.GenerateRecordStubEnabled                      ;
            UnionPatternMatchCaseGenerationEnabled      = settings.UnionPatternMatchCaseGenerationEnabled         ;
            ResolveUnopenedNamespacesEnabled            = settings.ResolveUnopenedNamespacesEnabled               ;
            UnusedReferencesEnabled                     = settings.UnusedReferencesEnabled                        ;
            UnusedOpensEnabled                          = settings.UnusedOpensEnabled                             ;
            TaskListCommentsEnabled                     = settings.TaskListCommentsEnabled                        ;
            GoToMetadataEnabled                         = settings.GoToMetadataEnabled                            ;
            GenerateReferencesEnabled                   = settings.GenerateReferencesEnabled                      ;
            GoToSymbolSourceEnabled                     = settings.GoToSymbolSourceEnabled                        ;
            QuickInfoPanelEnabled                       = settings.QuickInfoPanelEnabled                          ;
            LinterEnabled                               = settings.LinterEnabled                                  ;
            OutliningEnabled                            = settings.OutliningEnabled                               ;
        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            settings = VFPT_Settings.getGeneralOptions();
            settings.XmlDocEnabled                               =      XmlDocEnabled                                ;
            settings.FormattingEnabled                           =      FormattingEnabled                            ;
            settings.NavBarEnabled                               =      NavBarEnabled                                ;
            settings.HighlightUsageEnabled                       =      HighlightUsageEnabled                        ;
            settings.HighlightPrintfUsageEnabled                 =      HighlightPrintfUsageEnabled                  ;
            settings.RenameRefactoringEnabled                    =      RenameRefactoringEnabled                     ;
            settings.DepthColorizerEnabled                       =      DepthColorizerEnabled                        ;
            settings.NavigateToEnabled                           =      NavigateToEnabled                            ;
            settings.SyntaxColoringEnabled                       =      SyntaxColoringEnabled                        ;
            settings.InterfaceImplementationEnabled              =      InterfaceImplementationEnabled               ;
            settings.FolderOrganizationEnabled                   =      FolderOrganizationEnabled                    ;
            settings.FindAllReferencesEnabled                    =      FindAllReferencesEnabled                     ;
            settings.GenerateRecordStubEnabled                   =      GenerateRecordStubEnabled                    ;
            settings.UnionPatternMatchCaseGenerationEnabled      =      UnionPatternMatchCaseGenerationEnabled       ;
            settings.ResolveUnopenedNamespacesEnabled            =      ResolveUnopenedNamespacesEnabled             ;
            settings.UnusedReferencesEnabled                     =      UnusedReferencesEnabled                      ;
            settings.UnusedOpensEnabled                          =      UnusedOpensEnabled                           ;
            settings.TaskListCommentsEnabled                     =      TaskListCommentsEnabled                      ;
            settings.GoToMetadataEnabled                         =      GoToMetadataEnabled                          ;
            settings.GenerateReferencesEnabled                   =      GenerateReferencesEnabled                    ;
            settings.GoToSymbolSourceEnabled                     =      GoToSymbolSourceEnabled                      ;
            settings.QuickInfoPanelEnabled                       =      QuickInfoPanelEnabled                        ;
            settings.LinterEnabled                               =      LinterEnabled                                ;
            settings.OutliningEnabled                            =      OutliningEnabled                             ;
            settings.Save();
        }

        bool GetNavigationBarConfig()
        {
            try
            {
                var config = System.Configuration.ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath);
                var configValue = config.AppSettings.Settings[navBarConfig];
                bool result;
                return configValue != null && bool.TryParse(configValue.Value, out result) ? result : false;
            }
            catch (Exception ex)
            {
                LoggingModule.logException(ex);
                return false;
            }
        }

        bool IsUserAdministrator()
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
                    var config = System.Configuration.ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath);
                    config.AppSettings.Settings.Remove(navBarConfig);
                    config.AppSettings.Settings.Add(navBarConfig, v.ToString().ToLower());
                    config.Save(ConfigurationSaveMode.Minimal);

                    return true;
                }
                else
                {
                    LoggingModule.messageBoxError(Resource.navBarUnauthorizedMessage);
                    return false;
                }
            }
            catch (Exception ex)
            {
                LoggingModule.messageBoxError(Resource.navBarErrorMessage);
                LoggingModule.logException(ex);
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
        public bool HighlightPrintfUsageEnabled { get; set; }

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
        public bool GoToSymbolSourceEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool QuickInfoPanelEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool LinterEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool OutliningEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool PeekDefinitionEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public bool PeekDefinitionAvailable { get; private set; }

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
                if (NavBarEnabled != _optionsControl.NavBarEnabled)
                {
                    if (!SetNavigationBarConfig(_optionsControl.NavBarEnabled))
                    {
                        // Keep the dialog open in the case of errors
                        e.ApplyBehavior = ApplyKind.CancelNoNavigate;
                        base.OnApply(e);
                        return;
                    }

                    NavBarEnabled = _optionsControl.NavBarEnabled;
                    _navBarEnabledInAppConfig = _optionsControl.NavBarEnabled;
                }

                XmlDocEnabled = _optionsControl.XmlDocEnabled;
                FormattingEnabled = _optionsControl.FormattingEnabled;

                HighlightUsageEnabled = _optionsControl.HighlightUsageEnabled;
                HighlightPrintfUsageEnabled = _optionsControl.HighlightPrintfUsageEnabled;
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
                GoToSymbolSourceEnabled = _optionsControl.GoToSymbolSourceEnabled;
                QuickInfoPanelEnabled = _optionsControl.QuickInfoPanelEnabled;
                LinterEnabled = _optionsControl.LinterEnabled;
                OutliningEnabled = _optionsControl.OutliningEnabled;
                PeekDefinitionEnabled = _optionsControl.PeekDefinitionEnabled;
            }

            base.OnApply(e);
        }
    }
}
