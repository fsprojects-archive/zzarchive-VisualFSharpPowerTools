using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools.UI {
    public partial class OutliningTooltipZoomLevelControl : UserControl {
        double _value;

        public OutliningTooltipZoomLevelControl() {
            InitializeComponent();
        }

        protected override void OnLoad(EventArgs e) {
            tb.TextChanged += (sender, args) => {
                if (IsTextAllowed(tb.Text)) {
                    _value = Convert.ToDouble(tb.Text);
                }
            };
        }

        private bool IsTextAllowed(string text) {
            try {
                var n = Convert.ToDouble(text);
                return n >= 0;
            }
            catch {
                return false;
            }
        }

        public double InputValue {
            get {
                return _value;
            }
            set { 
                _value = value;
                tb.Text = value.ToString();
            }
        }
    }
}
