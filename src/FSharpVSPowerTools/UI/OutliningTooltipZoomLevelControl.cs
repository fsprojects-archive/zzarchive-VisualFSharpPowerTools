using System;
using System.Drawing;
using System.Windows.Forms;

namespace FSharpVSPowerTools.UI {
    public partial class OutliningTooltipZoomLevelControl : UserControl {
        Int32 _percent;

        public OutliningTooltipZoomLevelControl() {
            InitializeComponent();
        }

        protected override void OnLoad(EventArgs e) {
            var zoomLevelOptions = new[] { 100, 80, 60, 40, 20 };
            foreach (var level in zoomLevelOptions) {
                cb.Items.Add(level + " %");
            }

            cb.LostFocus += (sender, args) => {
                var percent = ParseText(cb.Text);
                if (percent != null) {
                    InputValue = percent.Value;
                    cb.ForeColor = SystemColors.WindowText;
                } else {
                    cb.ForeColor = Color.Red;
                }
            };

            cb.SelectedIndexChanged += (sender, args) => {
                InputValue = ParseText(cb.SelectedItem as string).Value;
                cb.ForeColor = SystemColors.WindowText;
            };
        }

        private int? ParseText(string text) {
            try {
                var percent = text.TrimEnd(new[] { '%', ' ' });
                var n = Convert.ToInt32(percent);
                if (0 <= n && n <= 100) return n;
                return null;
            }
            catch {
                return null;
            }
        }

        public int InputValue {
            get {
                return _percent;
            }
            set { 
                _percent = value;
                cb.Text = value.ToString() + " %";
            }
        }
    }
}
