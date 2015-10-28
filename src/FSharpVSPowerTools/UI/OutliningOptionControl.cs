using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools.UI {
    public partial class OutliningOptionControl : UserControl {
        readonly string _groupName;

        public OutliningOptionControl() {
            InitializeComponent();
        }

        public OutliningOptionControl(string groupName) : this() {
            _groupName = groupName;
        }

        protected override void OnLoad(EventArgs e) {
            lblOutliningGroup.Text = _groupName;
            cbEnabled.CheckedChanged += (sender, args) =>
                GreyOutIfDisabled();
            GreyOutIfDisabled();
        }

        void GreyOutIfDisabled() {
            cbCollapsedByDefault.Enabled = cbEnabled.Checked;
        }

        public bool OutliningEnabled {
            get { return cbEnabled.Checked; }
            set { cbEnabled.Checked = value; }
        }

        public bool CollapsedByDefault {
            get { return cbCollapsedByDefault.Checked; }
            set { cbCollapsedByDefault.Checked = value; }
        }
    }
}
