using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Principal;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")]
    [DefaultProperty("FormattingEnabled")]
    public class GeneralOptionsPage : DialogPage, IGeneralOptions
    {
        private const string navBarConfig = "fsharp-navigationbar-enabled";
        private bool _navBarEnabledInAppConfig;

        public GeneralOptionsPage()
        {
            var componentModel = Package.GetGlobalService(typeof(SComponentModel)) as IComponentModel;

            if (CheckUserIsAdministrator())
            {
                // TODO: use nameof in a future
                PropertyDescriptor descriptor = TypeDescriptor.GetProperties(this.GetType())["NavBarEnabled"];
                ReadOnlyAttribute attribute = (ReadOnlyAttribute)
                                              descriptor.Attributes[typeof(ReadOnlyAttribute)];
                FieldInfo fieldToChange = attribute.GetType().GetField("isReadOnly",
                                                 System.Reflection.BindingFlags.NonPublic |
                                                 System.Reflection.BindingFlags.Instance);

                fieldToChange.SetValue(attribute, false);
            }

            _navBarEnabledInAppConfig = GetNavigationBarConfig();
            NavBarEnabled = _navBarEnabledInAppConfig;

            XmlDocEnabled = true;
            FormattingEnabled = true;
            HighlightUsageEnabled = true;
            DepthColorizerEnabled = false;
            SyntaxColoringEnabled = true;
            UnusedReferencesEnabled = false;
            UnusedOpensEnabled = false;
            QuickInfoPanelEnabled = true;
            LinterEnabled = true;

            RenameRefactoringEnabled = true;
            ResolveUnopenedNamespacesEnabled = true;
            GenerateReferencesEnabled = true;
            InterfaceImplementationEnabled = true;
            GenerateRecordStubEnabled = true;
            UnionPatternMatchCaseGenerationEnabled = true;

            NavigateToEnabled = true;
            GoToSymbolSourceEnabled = true;
            GoToMetadataEnabled = true;
            FindAllReferencesEnabled = true;

            TaskListCommentsEnabled = true;
            FolderOrganizationEnabled = false;
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
            catch (Exception ex)
            {
                LoggingModule.logException(ex);
                return false;
            }
        }

        private bool CheckUserIsAdministrator()
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
                if (CheckUserIsAdministrator())
                {
                    var config = ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath);
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

        #region Properties

        // We are letting Visual Studio know that these property value needs to be persisted

        #region Editor enhancements

        [Category("Editor enhancements")]
        [DisplayName("XML comments generation")]
        [Description("Enables or disables XML comments generation.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool XmlDocEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Code formatting")]
        [Description("Enables or disables code formatting using Format Document command.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool FormattingEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Navigation Bar ***")]
        [Description("Enables or disables Navigation Bar.\nRequires admin privileges and Visual Studio restart to take effect.")]
        [DefaultValue(false)]
        [ReadOnly(true)]
        public bool NavBarEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Highlight usage")]
        [Description("Enables or disables reference usage highlighting.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool HighlightUsageEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Depth colorizer")]
        [Description("Enables or disables alternate coloring depending on binding depth.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(false)]
        public bool DepthColorizerEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Syntax coloring")]
        [Description("Enables or disables syntax coloring.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool SyntaxColoringEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Gray out unused references")]
        [Description("Enables or disables graying out unused references.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool UnusedReferencesEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("Gray out unused opens")]
        [Description("Enables or disables graying out unused opens.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool UnusedOpensEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("QuickInfo panel")]
        [Description("Enables or disables QuickInfo panel.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool QuickInfoPanelEnabled { get; set; }

        [Category("Editor enhancements")]
        [DisplayName("FSharpLint integration")]
        [Description("Enables or disables integration with FSharpLint.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool LinterEnabled { get; set; }

        #endregion Editor enhancements

        #region Refactorings

        [Category("Refactorings")]
        [DisplayName("Rename reference")]
        [Description("Enables or disables rename reference refactoring.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool RenameRefactoringEnabled { get; set; }

        [Category("Refactorings")]
        [DisplayName("Resolve unopened namespaces")]
        [Description("Enables or disables resolving unopened namespaces.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool ResolveUnopenedNamespacesEnabled { get; set; }

        [Category("Refactorings")]
        [DisplayName("Generate references for FSI **")]
        [Description("Enables or disables refactoring to generate references for FSI.\nRequires Visual Studio restart to take effect")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool GenerateReferencesEnabled { get; set; }

        [Category("Refactorings")]
        [DisplayName("Implement interface")]
        [Description("Enables or disables interface implementation refactoring.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool InterfaceImplementationEnabled { get; set; }

        [Category("Refactorings")]
        [DisplayName("Generate record stub")]
        [Description("Enables or disables record stub generation refactoring.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool GenerateRecordStubEnabled { get; set; }

        [Category("Refactorings")]
        [DisplayName("Generate union pattern match case")]
        [Description("Enables or disables union pattern match case generation.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool UnionPatternMatchCaseGenerationEnabled { get; set; }

        #endregion Refactorings

        #region Navigation

        [Category("Navigation")]
        [DisplayName("NavigateTo")]
        [Description("Enables or disables NavigateTo.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool NavigateToEnabled { get; set; }

        [Category("Navigation")]
        [DisplayName("Navigate to source")]
        [Description("Enables or disables navigation to source.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool GoToSymbolSourceEnabled { get; set; }

        [Category("Navigation")]
        [DisplayName("Go to metadata")]
        [Description("Enables or disables going to metadata.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool GoToMetadataEnabled { get; set; }

        [Category("Navigation")]
        [DisplayName("Find all references")]
        [Description("Enables or disables finding all references.")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool FindAllReferencesEnabled { get; set; }

        #endregion Navigation

        #region Tools

        [Category("Tools")]
        [DisplayName("Task List comments **")]
        [Description("Enables or disables Task List comments.\nRequires Visual Studio restart to take effect")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(true)]
        public bool TaskListCommentsEnabled { get; set; }

        [Category("Tools")]
        [DisplayName("Folder organization **")]
        [Description("Enables or disables organizing files into folders in Solution Explorer.\nRequires Visual Studio restart to take effect")]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        [DefaultValue(false)]
        public bool FolderOrganizationEnabled { get; set; }

        #endregion Tools

        #endregion Properties

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                var propertyGrid = base.Window as PropertyGrid;
                propertyGrid.Dock = DockStyle.Fill;
                propertyGrid.Margin = new Padding(0);
                var optionsControl = new GeneralOptionsControl(propertyGrid);
                return optionsControl;
            }
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so
        // that Visual Studio saves it.
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                if (NavBarEnabled != _navBarEnabledInAppConfig && SetNavigationBarConfig(NavBarEnabled))
                {
                    _navBarEnabledInAppConfig = NavBarEnabled;
                }
            }

            base.OnApply(e);
        }
    }
}
