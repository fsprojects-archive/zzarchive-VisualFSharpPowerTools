using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using FSharpVSPowerTools.UI;
using Microsoft.VisualStudio.Shell;

namespace FSharpVSPowerTools {
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("f7114e2b-7ef5-40f7-87cf-95360f13bd8f")]
    public class OutliningOptionsPage : DialogPage, IOutliningOptions
    {
        OutliningOptionsControl _control;

        public OutliningOptionsPage()
        {
            OpensEnabled = true;
            OpensCollapsedByDefault = true;
            ModulesEnabled = true;
            ModulesCollapsedByDefault = false;
            HashDirectivesEnabled = true;
            HashDirectivesCollapsedByDefault = true;
            TypesEnabled = true;
            TypesCollapsedByDefault = false;
            SimpleTypesEnabled = true;
            SimpleTypesCollapsedByDefault = false;
            TypeExpressionsEnabled = true;
            TypeExpressionsCollapsedByDefault = false;
            MembersEnabled = true;
            MembersCollapsedByDefault = false;
            LetOrUseEnabled = true;
            LetOrUseCollapsedByDefault = false;
            CollectionsEnabled = true;
            CollectionsCollapsedByDefault = false;
            PatternMatchesEnabled = true;
            PatternMatchesCollapsedByDefault = false;
            TryWithFinallyEnabled = true;
            TryWithFinallyCollapsedByDefault = false;
            IfThenElseEnabled = true;
            IfThenElseCollapsedByDefault = false;
            CExpressionMembersEnabled = true;
            CExpressionMembersCollapsedByDefault = false;
            LoopsEnabled = true;
            LoopsCollapsedByDefault = false;
        
        }

        protected override void OnApply(PageApplyEventArgs e) {
            OpensEnabled = _control.Opens.Enabled;
            OpensCollapsedByDefault = _control.Opens.CollapsedByDefault;
            ModulesEnabled = _control.Modules.Enabled;
            ModulesCollapsedByDefault = _control.Modules.CollapsedByDefault;
            HashDirectivesEnabled = _control.HashDirectives.Enabled;
            HashDirectivesCollapsedByDefault = _control.HashDirectives.CollapsedByDefault;
            TypesEnabled = _control.Types.Enabled;
            TypesCollapsedByDefault = _control.Types.CollapsedByDefault;
            SimpleTypesEnabled = _control.SimpleTypes.Enabled;
            SimpleTypesCollapsedByDefault = _control.SimpleTypes.CollapsedByDefault;
            TypeExpressionsEnabled = _control.TypeExpressions.Enabled;
            TypeExpressionsCollapsedByDefault = _control.TypeExpressions.CollapsedByDefault;
            MembersEnabled = _control.Members.Enabled;
            MembersCollapsedByDefault = _control.Members.CollapsedByDefault;
            LetOrUseEnabled = _control.LetOrUse.Enabled;
            LetOrUseCollapsedByDefault = _control.LetOrUse.CollapsedByDefault;
            CollectionsEnabled = _control.Collections.Enabled;
            CollectionsCollapsedByDefault = _control.Collections.CollapsedByDefault;
            PatternMatchesEnabled = _control.PatternMatches.Enabled;
            PatternMatchesCollapsedByDefault = _control.PatternMatches.CollapsedByDefault;
            TryWithFinallyEnabled = _control.TryWithFinally.Enabled;
            TryWithFinallyCollapsedByDefault = _control.TryWithFinally.CollapsedByDefault;
            IfThenElseEnabled = _control.IfThenElse.Enabled;
            IfThenElseCollapsedByDefault = _control.IfThenElse.CollapsedByDefault;
            CExpressionMembersEnabled = _control.CExpressionMembers.Enabled;
            CExpressionMembersCollapsedByDefault = _control.CExpressionMembers.CollapsedByDefault;
            LoopsEnabled = _control.Loops.Enabled;
            LoopsCollapsedByDefault = _control.Loops.CollapsedByDefault;
        

            base.OnApply(e);
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool OpensEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool OpensCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool ModulesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool ModulesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool HashDirectivesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool HashDirectivesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TypesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TypesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool SimpleTypesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool SimpleTypesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TypeExpressionsEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TypeExpressionsCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool MembersEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool MembersCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool LetOrUseEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool LetOrUseCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CollectionsEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CollectionsCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool PatternMatchesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool PatternMatchesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TryWithFinallyEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool TryWithFinallyCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool IfThenElseEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool IfThenElseCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CExpressionMembersEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CExpressionMembersCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool LoopsEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool LoopsCollapsedByDefault { get; set; }

        protected override IWin32Window Window {
            get { return _control = new OutliningOptionsControl(this); }
        }
    }
}
