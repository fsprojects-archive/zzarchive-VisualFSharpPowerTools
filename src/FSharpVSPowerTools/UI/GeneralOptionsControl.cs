using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    public partial class GeneralOptionsControl : UserControl
    {
        const string vs2015Suffix = " (VS2015+ only)";
        private GeneralOptionsPage _optionsPage;
        public GeneralOptionsControl(GeneralOptionsPage optionsPage)
        {
            InitializeComponent();

            _optionsPage = optionsPage;
        }
        public bool XmlDocEnabled
        {
            get { return chbXmlDoc.Checked; }
            set { chbXmlDoc.Checked = value; }
        }

        public bool FormattingEnabled
        {
            get { return chbFormatting.Checked; }
            set { chbFormatting.Checked = value; }
        }

        public bool NavBarEnabled
        {
            get { return chbNavBar.Checked; }
            set { chbNavBar.Checked = value; }
        }

        public bool HighlightUsageEnabled
        {
            get { return chbHighlightUsage.Checked; }
            set { chbHighlightUsage.Checked = value; }
        }

        public bool HighlightPrintfUsageEnabled
        {
            get { return chbHighlightPrintf.Checked; }
            set { chbHighlightPrintf.Checked = value; }
        }

        public bool RenameRefactoringEnabled
        {
            get { return chbRenameRefactoring.Checked; }
            set { chbRenameRefactoring.Checked = value; }
        }
        public bool DepthColorizerEnabled
        {
            get { return chbDepthColorizer.Checked; }
            set { chbDepthColorizer.Checked = value; }
        }

        public bool NavigateToEnabled
        {
            get { return chbNavigateTo.Checked; }
            set { chbNavigateTo.Checked = value; }
        }

        public bool SyntaxColoringEnabled
        {
            get { return chbSyntaxColoring.Checked; }
            set { chbSyntaxColoring.Checked = value; }
        }

        public bool InterfaceImplementationEnabled
        {
            get { return chbInterfaceImplementation.Checked; }
            set { chbInterfaceImplementation.Checked = value; }
        }

        public bool FolderOrganizationEnabled
        {
            get { return chbFolderOrganization.Checked; }
            set { chbFolderOrganization.Checked = value; }
        }

        public bool FindAllReferencesEnabled
        {
            get { return chbFindAllReferences.Checked; }
            set { chbFindAllReferences.Checked = value; }
        }

        public bool GenerateRecordStubEnabled
        {
            get { return chbRecordStubGeneration.Checked; }
            set { chbRecordStubGeneration.Checked = value; }
        }

        public bool UnionPatternMatchCaseGenerationEnabled
        {
            get { return chbUnionPatternMatchCaseGeneration.Checked; }
            set { chbUnionPatternMatchCaseGeneration.Checked = value; }
        }

        public bool ResolveUnopenedNamespacesEnabled
        {
            get { return chbResolveUnopenedNamespaces.Checked; }
            set { chbResolveUnopenedNamespaces.Checked = value;  }
        }

        public bool UnusedReferencesEnabled
        {
            get { return chbUnusedReferences.Checked; }
            set { chbUnusedReferences.Checked = value; }
        }

        public bool UnusedOpensEnabled
        {
            get { return chbUnusedOpens.Checked; }
            set { chbUnusedOpens.Checked = value; }
        }

        public bool TaskListCommentsEnabled
        {
            get { return chbTaskListComments.Checked; }
            set { chbTaskListComments.Checked = value; }
        }

        public bool GoToMetadataEnabled
        {
            get { return chbGoToMetadata.Checked; }
            set { chbGoToMetadata.Checked = value; }
        }

        public bool GenerateReferencesEnabled
        {
            get { return chbGenerateReferences.Checked; }
            set { chbGenerateReferences.Checked = value; }
        }

        public bool GoToSymbolSourceEnabled
        {
            get { return chbGoToSymbolSource.Checked; }
            set { chbGoToSymbolSource.Checked = value; }
        }

        public bool QuickInfoPanelEnabled
        {
            get { return chbQuickInfoPanel.Checked; }
            set { chbQuickInfoPanel.Checked = value; }
        }

        public bool LinterEnabled
        {
            get { return chbLinter.Checked; }
            set { chbLinter.Checked = value; }
        }

        public bool OutliningEnabled
        {
            get { return chbOutlining.Checked; }
            set { chbOutlining.Checked = value; }
        }

        public bool PeekDefinitionEnabled
        {
            get { return chbPeekDefinition.Checked; }
            set { chbPeekDefinition.Checked = value; }
        }

        private void GeneralOptionsControl_Load(object sender, EventArgs e)
        {
            XmlDocEnabled = _optionsPage.XmlDocEnabled;
            FormattingEnabled = _optionsPage.FormattingEnabled;
            NavBarEnabled = _optionsPage.NavBarEnabled;
            HighlightUsageEnabled = _optionsPage.HighlightUsageEnabled;
            HighlightPrintfUsageEnabled = _optionsPage.HighlightPrintfUsageEnabled;
            RenameRefactoringEnabled = _optionsPage.RenameRefactoringEnabled;
            DepthColorizerEnabled = _optionsPage.DepthColorizerEnabled;
            NavigateToEnabled = _optionsPage.NavigateToEnabled;
            SyntaxColoringEnabled = _optionsPage.SyntaxColoringEnabled;
            InterfaceImplementationEnabled = _optionsPage.InterfaceImplementationEnabled;
            FolderOrganizationEnabled = _optionsPage.FolderOrganizationEnabled;
            FindAllReferencesEnabled = _optionsPage.FindAllReferencesEnabled;
            GenerateRecordStubEnabled = _optionsPage.GenerateRecordStubEnabled;
            UnionPatternMatchCaseGenerationEnabled = _optionsPage.UnionPatternMatchCaseGenerationEnabled;
            ResolveUnopenedNamespacesEnabled = _optionsPage.ResolveUnopenedNamespacesEnabled;
            UnusedReferencesEnabled = _optionsPage.UnusedReferencesEnabled;
            UnusedOpensEnabled = _optionsPage.UnusedOpensEnabled;
            TaskListCommentsEnabled = _optionsPage.TaskListCommentsEnabled;
            GoToMetadataEnabled = _optionsPage.GoToMetadataEnabled;
            GenerateReferencesEnabled = _optionsPage.GenerateReferencesEnabled;
            GoToSymbolSourceEnabled = _optionsPage.GoToSymbolSourceEnabled;
            QuickInfoPanelEnabled = _optionsPage.QuickInfoPanelEnabled;
            LinterEnabled = _optionsPage.LinterEnabled;
            OutliningEnabled = _optionsPage.OutliningEnabled;
            PeekDefinitionEnabled = _optionsPage.PeekDefinitionEnabled;

            if ( !_optionsPage.PeekDefinitionAvailable )
            {
                chbPeekDefinition.Enabled = false;
                var peekDefinitionText = chbPeekDefinition.Text;
                chbPeekDefinition.Text = peekDefinitionText.Contains ( vs2015Suffix ) ? peekDefinitionText : peekDefinitionText + vs2015Suffix;
            }
        }
    }
}
