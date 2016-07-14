using FSharp.Editing.VisualStudio;
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
        public OutliningOptionControl Members { get; private set; }
        public OutliningOptionControl LetOrUse{ get; private set; }
        public OutliningOptionControl Collections{ get; private set; }
        public OutliningOptionControl PatternMatches { get; private set; }
        public OutliningOptionControl TryWithFinally { get; private set; }
        public OutliningOptionControl IfThenElse { get; private set; }
        public OutliningOptionControl CExpressionMembers { get; private set; }
        public OutliningOptionControl Loops { get; private set; }
        public OutliningOptionControl Attributes { get; private set; }
        public OutliningOptionControl Comments { get; private set; }
        public OutliningOptionControl XmlDocComments { get; private set; }
        public OutliningTooltipZoomLevelControl Tooltip { get; private set; }

        protected override void OnLoad(EventArgs e) {
            base.OnLoad(e);

            Opens = new OutliningOptionControl("Open declarations") {
                OutliningEnabled = _outliningOptions.OpensEnabled,
                CollapsedByDefault = _outliningOptions.OpensCollapsedByDefault
            };
            Modules = new OutliningOptionControl("Module declarations") {
                OutliningEnabled = _outliningOptions.ModulesEnabled,
                CollapsedByDefault = _outliningOptions.ModulesCollapsedByDefault
            };
            HashDirectives = new OutliningOptionControl("#I/#r/#load directives") {
                OutliningEnabled = _outliningOptions.HashDirectivesEnabled,
                CollapsedByDefault = _outliningOptions.HashDirectivesCollapsedByDefault
            };
            Types = new OutliningOptionControl("Type declarations/extensions") {
                OutliningEnabled = _outliningOptions.TypesEnabled,
                CollapsedByDefault = _outliningOptions.TypesCollapsedByDefault
            };
            SimpleTypes = new OutliningOptionControl("Record/DU definitions"){              
                OutliningEnabled =   _outliningOptions.SimpleTypesEnabled,
                CollapsedByDefault = _outliningOptions.SimpleTypesCollapsedByDefault
            };
            TypeExpressions = new OutliningOptionControl("Expressions") {
                OutliningEnabled =   _outliningOptions.TypeExpressionsEnabled,
                CollapsedByDefault = _outliningOptions.TypeExpressionsCollapsedByDefault
            };
            Members = new OutliningOptionControl("Members") {
                OutliningEnabled =   _outliningOptions.MembersEnabled,
                CollapsedByDefault = _outliningOptions.MembersCollapsedByDefault
            };
            LetOrUse = new OutliningOptionControl("Let/use bindings") {
                OutliningEnabled =   _outliningOptions.LetOrUseEnabled,
                CollapsedByDefault = _outliningOptions.LetOrUseCollapsedByDefault
            };
            Collections = new OutliningOptionControl("Array/list comprehensions") {
                OutliningEnabled =   _outliningOptions.CollectionsEnabled,
                CollapsedByDefault = _outliningOptions.CollectionsCollapsedByDefault
            };
            PatternMatches = new OutliningOptionControl("Pattern matching") {
                OutliningEnabled =   _outliningOptions.PatternMatchesEnabled ,
                CollapsedByDefault = _outliningOptions.PatternMatchesCollapsedByDefault
            };
            TryWithFinally = new OutliningOptionControl("Try-with-finally") {
                OutliningEnabled =   _outliningOptions.TryWithFinallyEnabled,
                CollapsedByDefault = _outliningOptions.TryWithFinallyCollapsedByDefault
            };
            IfThenElse = new OutliningOptionControl("If-then-else") {
                OutliningEnabled =   _outliningOptions.IfThenElseEnabled,
                CollapsedByDefault = _outliningOptions.IfThenElseCollapsedByDefault
            };
            CExpressionMembers = new OutliningOptionControl("Computation expr members") {
                OutliningEnabled =   _outliningOptions.CExpressionMembersEnabled,
                CollapsedByDefault = _outliningOptions.CExpressionMembersCollapsedByDefault
            };
            Loops = new OutliningOptionControl("For/while loops") {
                OutliningEnabled =   _outliningOptions.LoopsEnabled,
                CollapsedByDefault = _outliningOptions.LoopsCollapsedByDefault
            };
            Attributes = new OutliningOptionControl("Attributes") {
                OutliningEnabled = _outliningOptions.AttributesEnabled,
                CollapsedByDefault = _outliningOptions.AttributesCollapsedByDefault
            };
            Comments = new OutliningOptionControl("Comments") {
                OutliningEnabled = _outliningOptions.CommentsEnabled,
                CollapsedByDefault = _outliningOptions.CommentsCollapsedByDefault
            };
            XmlDocComments = new OutliningOptionControl("XML doc comments") {
                OutliningEnabled = _outliningOptions.XmlDocCommentsEnabled,
                CollapsedByDefault = _outliningOptions.XmlDocCommentsCollapsedByDefault
            };
            Tooltip = new OutliningTooltipZoomLevelControl() {
                InputValue = _outliningOptions.TooltipZoomLevel
            };

            flowLayoutPanelMain.Controls.Clear();
            flowLayoutPanelMain.Controls.Add(Opens);
            flowLayoutPanelMain.Controls.Add(Modules);
            flowLayoutPanelMain.Controls.Add(HashDirectives);            
            flowLayoutPanelMain.Controls.Add(Types);
            flowLayoutPanelMain.Controls.Add(SimpleTypes);
            flowLayoutPanelMain.Controls.Add(TypeExpressions);
            flowLayoutPanelMain.Controls.Add(Members);
            flowLayoutPanelMain.Controls.Add(LetOrUse);
            flowLayoutPanelMain.Controls.Add(Collections);
            flowLayoutPanelMain.Controls.Add(PatternMatches);
            flowLayoutPanelMain.Controls.Add(TryWithFinally);
            flowLayoutPanelMain.Controls.Add(IfThenElse);
            flowLayoutPanelMain.Controls.Add(CExpressionMembers);
            flowLayoutPanelMain.Controls.Add(Loops);
            flowLayoutPanelMain.Controls.Add(Attributes);
            flowLayoutPanelMain.Controls.Add(Comments);
            flowLayoutPanelMain.Controls.Add(XmlDocComments);
            flowLayoutPanelMain.Controls.Add(Tooltip);
        }
    }
}