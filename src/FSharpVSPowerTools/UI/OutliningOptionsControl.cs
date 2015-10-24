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

        public OutliningOptionControl Opens { get; private set; }
        public OutliningOptionControl Modules { get; private set; }
        public OutliningOptionControl HashDirectives { get; private set; }
        public OutliningOptionControl Types { get; private set; }
        public OutliningOptionControl SimpleTypes { get; private set; }
        public OutliningOptionControl TypeExpressions { get; private set; }
        public OutliningOptionControl Members { get; set; }
        public OutliningOptionControl LetOrUse{ get; set; }
        public OutliningOptionControl Collections{ get; set; }
        public OutliningOptionControl PatternMatches { get; private set; }
        public OutliningOptionControl TryWithFinally { get; set; }
        public OutliningOptionControl IfThenElse { get; set; }
        public OutliningOptionControl CExpressionMembers { get; set; }
        public OutliningOptionControl Loops { get; set; }
        public OutliningOptionControl Attributes { get; set; }

        protected override void OnLoad(EventArgs _) {
            Opens = new OutliningOptionControl("Open :") {
                OutliningEnabled = _outliningOptions.OpensEnabled,
                CollapsedByDefault = _outliningOptions.OpensCollapsedByDefault
            };
            Modules = new OutliningOptionControl("Module :") {
                OutliningEnabled = _outliningOptions.ModulesEnabled,
                CollapsedByDefault = _outliningOptions.ModulesCollapsedByDefault
            };
            HashDirectives = new OutliningOptionControl("#I / #r / #load :") {
                OutliningEnabled = _outliningOptions.HashDirectivesEnabled,
                CollapsedByDefault = _outliningOptions.HashDirectivesCollapsedByDefault
            };
            Types = new OutliningOptionControl("Type / Interface / Type Extension :") {
                OutliningEnabled = _outliningOptions.TypesEnabled,
                CollapsedByDefault = _outliningOptions.TypesCollapsedByDefault
            };
            SimpleTypes = new OutliningOptionControl("Record and DU Definition :"){              
                OutliningEnabled =   _outliningOptions.SimpleTypesEnabled,
                CollapsedByDefault = _outliningOptions.SimpleTypesCollapsedByDefault
            };
            TypeExpressions = new OutliningOptionControl("CExpr / ObjExpr / Record / Tuple / Code Quotation :") {
                OutliningEnabled =   _outliningOptions.TypeExpressionsEnabled,
                CollapsedByDefault = _outliningOptions.TypeExpressionsCollapsedByDefault
            };
            Members = new OutliningOptionControl("Member :") {
                OutliningEnabled =   _outliningOptions.MembersEnabled,
                CollapsedByDefault = _outliningOptions.MembersCollapsedByDefault
            };
            LetOrUse = new OutliningOptionControl("Let / Use :") {
                OutliningEnabled =   _outliningOptions.LetOrUseEnabled,
                CollapsedByDefault = _outliningOptions.LetOrUseCollapsedByDefault
            };
            Collections = new OutliningOptionControl("Array / List :") {
                OutliningEnabled =   _outliningOptions.CollectionsEnabled,
                CollapsedByDefault = _outliningOptions.CollectionsCollapsedByDefault
            };
            PatternMatches = new OutliningOptionControl("Pattern Match :") {
                OutliningEnabled =   _outliningOptions.PatternMatchesEnabled ,
                CollapsedByDefault = _outliningOptions.PatternMatchesCollapsedByDefault
            };
            TryWithFinally = new OutliningOptionControl("Try-With-Finally :") {
                OutliningEnabled =   _outliningOptions.TryWithFinallyEnabled,
                CollapsedByDefault = _outliningOptions.TryWithFinallyCollapsedByDefault
            };
            IfThenElse = new OutliningOptionControl("If-Then-Else :") {
                OutliningEnabled =   _outliningOptions.IfThenElseEnabled,
                CollapsedByDefault = _outliningOptions.IfThenElseCollapsedByDefault
            };
            CExpressionMembers = new OutliningOptionControl("CExpr Members :") {
                OutliningEnabled =   _outliningOptions.CExpressionMembersEnabled,
                CollapsedByDefault = _outliningOptions.CExpressionMembersCollapsedByDefault
            };
            Loops = new OutliningOptionControl("For / ForEach / While :") {
                OutliningEnabled =   _outliningOptions.LoopsEnabled,
                CollapsedByDefault = _outliningOptions.LoopsCollapsedByDefault
            };
            Attributes = new OutliningOptionControl("For / ForEach / While :")
            {
                OutliningEnabled = _outliningOptions.AttributesEnabled,
                CollapsedByDefault = _outliningOptions.AttributesCollapsedByDefault
            };

            flowLayoutPanel1.Controls.Add(Opens);
            flowLayoutPanel1.Controls.Add(Modules);
            flowLayoutPanel1.Controls.Add(HashDirectives);            
            flowLayoutPanel1.Controls.Add(Types);
            flowLayoutPanel1.Controls.Add(SimpleTypes);
            flowLayoutPanel1.Controls.Add(TypeExpressions);
            flowLayoutPanel1.Controls.Add(Members);
            flowLayoutPanel1.Controls.Add(LetOrUse);
            flowLayoutPanel1.Controls.Add(Collections);
            flowLayoutPanel1.Controls.Add(PatternMatches);
            flowLayoutPanel1.Controls.Add(TryWithFinally);
            flowLayoutPanel1.Controls.Add(IfThenElse);
            flowLayoutPanel1.Controls.Add(CExpressionMembers);
            flowLayoutPanel1.Controls.Add(Loops);
        }
    }
}

            // = new OutliningOptionControl(" :") {
            //    OutliningEnabled =   _outliningOptions.Enabled,
            //    CollapsedByDefault = _outliningOptions.CollapsedByDefault
            //};

