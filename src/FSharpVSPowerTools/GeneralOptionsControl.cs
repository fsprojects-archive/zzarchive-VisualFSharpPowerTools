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

        public bool XMLDocEnabled 
        {
            get { return chbXMLDoc.Checked; }
            set { chbXMLDoc.Checked = value;  }
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

        private void GeneralOptionsControl_Load(object sender, EventArgs e)
        {
            chbXMLDoc.Checked = OptionsPage.XMLDocEnabled;
            chbFormatting.Checked = OptionsPage.FormattingEnabled;
            chbNavBar.Checked = OptionsPage.NavBarEnabled;
            chbHighlightUsage.Checked = OptionsPage.HighlightUsageEnabled;
            chbRenameRefactoring.Checked = OptionsPage.RenameRefactoringEnabled;
            chbDepthColorizer.Checked = OptionsPage.DepthColorizerEnabled;
            chbNavigateTo.Checked = OptionsPage.NavigateToEnabled;
        }

        private void lblHome_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start(lblHome.Text);
        }

    }
}
