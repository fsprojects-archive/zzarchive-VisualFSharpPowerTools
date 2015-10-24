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

        public OutliningOptionControl TopLevel { get; private set; }
        public OutliningOptionControl Types { get; private set; }
        public OutliningOptionControl MatchStatements { get; private set; }
        public OutliningOptionControl ExceptionHandling { get; set; }
        public OutliningOptionControl ComputationConstructs { get; set; }
        public OutliningOptionControl CollectionsTuplesCexprs { get; set; }

        protected override void OnLoad(EventArgs _) {
            TopLevel = new OutliningOptionControl("Top Level:") {
                OutliningEnabled = _outliningOptions.ToplevelEnabled,
                CollapsedByDefault = _outliningOptions.ToplevelCollapsedByDefault
            };
            Types = new OutliningOptionControl("Types:");
            MatchStatements = new OutliningOptionControl("Match Statements:") {
                OutliningEnabled = _outliningOptions.MatchStatementEnabled,
                CollapsedByDefault = _outliningOptions.MatchStatementCollapsedByDefault
            };
            ExceptionHandling = new OutliningOptionControl("Exception Handling:");
            ComputationConstructs = new OutliningOptionControl("Computation Constructs:");
            CollectionsTuplesCexprs = new OutliningOptionControl("Collections/Tuples/Cexprs:");

            flowLayoutPanel1.Controls.Add(TopLevel);
            flowLayoutPanel1.Controls.Add(Types);
            flowLayoutPanel1.Controls.Add(MatchStatements);
            flowLayoutPanel1.Controls.Add(ExceptionHandling);
            flowLayoutPanel1.Controls.Add(ComputationConstructs);
            flowLayoutPanel1.Controls.Add(CollectionsTuplesCexprs);
        }
    }
}
