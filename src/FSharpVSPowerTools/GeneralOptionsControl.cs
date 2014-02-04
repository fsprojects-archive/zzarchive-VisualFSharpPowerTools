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

        private void GeneralOptionsControl_Load(object sender, EventArgs e)
        {
            chbXMLDoc.Checked = OptionsPage.XMLDocEnabled;
            chbFormatting.Checked = OptionsPage.FormattingEnabled;
        }

        private void lblHome_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start(lblHome.Text);
        }

    }
}
