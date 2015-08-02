using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    public partial class GeneralOptionsControl : UserControl
    {
        public GeneralOptionsControl(PropertyGrid propertyGrid)
        {
            InitializeComponent();

            this.tblLayoutPanel.Controls.Add(propertyGrid, 0, 0);
        }
    }
}
