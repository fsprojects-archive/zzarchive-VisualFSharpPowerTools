using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Windows.Forms;
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

        public GeneralOptionsPage()
        {
            var dte = Package.GetGlobalService(typeof(SDTE)) as DTE;
            var vsVersion = VisualStudioVersionModule.fromDTEVersion(dte.Version);

            XmlDocEnabled = true;
            FormattingEnabled = true;
            HighlightUsageEnabled = true;
            HighlightPrintfUsageEnabled = true;
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
            GoToSymbolSourceEnabled = true;
            QuickInfoPanelEnabled = true;
            LinterEnabled = false;
            OutliningEnabled = false;
            PeekDefinitionEnabled = true;
            PeekDefinitionAvailable = vsVersion >= VisualStudioVersion.VS2015;
        }

        // We are letting Visual Studio know that these property value needs to be persisted

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XmlDocEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FormattingEnabled { get; set; }

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
