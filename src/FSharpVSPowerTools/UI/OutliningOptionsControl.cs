using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools.UI {
    public partial class OutliningOptionsControl : UserControl {
        readonly IOutliningOptions _outliningOptions;

        public OutliningOptionsControl() {
            InitializeComponent();
        }

        public OutliningOptionsControl(IOutliningOptions outliningOptions) {
            InitializeComponent();
            _outliningOptions = outliningOptions;
        }

        protected override void OnLoad(EventArgs _) {
            TopLevelEnabled                  = _outliningOptions.ToplevelEnabled;
            TopLevelCollapsedByDefault       = _outliningOptions.ToplevelCollapsedByDefault;
            MatchStatementEnabled            = _outliningOptions.MatchStatementEnabled;
            MatchStatementCollapsedByDefault = _outliningOptions.MatchStatementCollapsedByDefault;
        }

        public bool TopLevelEnabled {
            get { return cbTopLevelEnabled.Checked; }
            private set { cbTopLevelEnabled.Checked = value; }
        }

        public bool TopLevelCollapsedByDefault {
            get { return cbTopLevelCollapsedByDefault.Checked; }
            private set { cbTopLevelCollapsedByDefault.Checked = value; }
        }

        public bool MatchStatementEnabled {
            get { return cbMatchStatementEnabled.Checked; }
            private set { cbMatchStatementEnabled.Checked = value; }
        }

        public bool MatchStatementCollapsedByDefault {
            get { return cbMatchStatementCollapsedByDefault.Checked; }
            private set { cbMatchStatementCollapsedByDefault.Checked = value; }
        }
    }
}
