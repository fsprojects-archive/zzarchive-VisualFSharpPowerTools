using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    public partial class GeneralOptionsControl : UserControl
    {
        public GeneralOptionsControl()
        {
            InitializeComponent();
        }
        public GeneralOptionsPage OptionsPage { get; set; }

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

        private void GeneralOptionsControl_Load(object sender, EventArgs e)
        {
            chbXmlDoc.Checked = OptionsPage.XmlDocEnabled;
            chbFormatting.Checked = OptionsPage.FormattingEnabled;
            chbNavBar.Checked = OptionsPage.NavBarEnabled;
            chbHighlightUsage.Checked = OptionsPage.HighlightUsageEnabled;
            chbRenameRefactoring.Checked = OptionsPage.RenameRefactoringEnabled;
            chbDepthColorizer.Checked = OptionsPage.DepthColorizerEnabled;
            chbNavigateTo.Checked = OptionsPage.NavigateToEnabled;
            chbSyntaxColoring.Checked = OptionsPage.SyntaxColoringEnabled;
            chbInterfaceImplementation.Checked = OptionsPage.InterfaceImplementationEnabled;
            chbFolderOrganization.Checked = OptionsPage.FolderOrganizationEnabled;
            chbFindAllReferences.Checked = OptionsPage.FindAllReferencesEnabled;
        }

        private void lblHome_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start(lblHome.Text);
        }
    }
}
