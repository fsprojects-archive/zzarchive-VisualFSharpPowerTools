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

            CollectionsEnabled = false;
            CollectionsCollapsedByDefault = false;

            PatternMatchesEnabled = false;
            PatternMatchesCollapsedByDefault = false;

            TryWithFinallyEnabled = false;
            TryWithFinallyCollapsedByDefault = false;

            IfThenElseEnabled = false;
            IfThenElseCollapsedByDefault = false;

            CExpressionMembersEnabled = false;
            CExpressionMembersCollapsedByDefault = false;

            LoopsEnabled = false;
            LoopsCollapsedByDefault = false;

            AttributesEnabled = false;
            AttributesCollapsedByDefault = false;

            CommentsEnabled = true;
            CommentsCollapsedByDefault = true;

            XmlDocCommentsEnabled = true;
            XmlDocCommentsCollapsedByDefault = true;
            
            TooltipZoomLevel = 80;
        }

        protected override void OnApply(PageApplyEventArgs e) {
            OpensEnabled = _control.Opens.OutliningEnabled;
            OpensCollapsedByDefault = _control.Opens.CollapsedByDefault;
            ModulesEnabled = _control.Modules.OutliningEnabled;
            ModulesCollapsedByDefault = _control.Modules.CollapsedByDefault;
            HashDirectivesEnabled = _control.HashDirectives.OutliningEnabled;
            HashDirectivesCollapsedByDefault = _control.HashDirectives.CollapsedByDefault;
            TypesEnabled = _control.Types.OutliningEnabled;
            TypesCollapsedByDefault = _control.Types.CollapsedByDefault;
            SimpleTypesEnabled = _control.SimpleTypes.OutliningEnabled;
            SimpleTypesCollapsedByDefault = _control.SimpleTypes.CollapsedByDefault;
            TypeExpressionsEnabled = _control.TypeExpressions.OutliningEnabled;
            TypeExpressionsCollapsedByDefault = _control.TypeExpressions.CollapsedByDefault;
            MembersEnabled = _control.Members.OutliningEnabled;
            MembersCollapsedByDefault = _control.Members.CollapsedByDefault;
            LetOrUseEnabled = _control.LetOrUse.OutliningEnabled;
            LetOrUseCollapsedByDefault = _control.LetOrUse.CollapsedByDefault;
            CollectionsEnabled = _control.Collections.OutliningEnabled;
            CollectionsCollapsedByDefault = _control.Collections.CollapsedByDefault;
            PatternMatchesEnabled = _control.PatternMatches.OutliningEnabled;
            PatternMatchesCollapsedByDefault = _control.PatternMatches.CollapsedByDefault;
            TryWithFinallyEnabled = _control.TryWithFinally.OutliningEnabled;
            TryWithFinallyCollapsedByDefault = _control.TryWithFinally.CollapsedByDefault;
            IfThenElseEnabled = _control.IfThenElse.OutliningEnabled;
            IfThenElseCollapsedByDefault = _control.IfThenElse.CollapsedByDefault;
            CExpressionMembersEnabled = _control.CExpressionMembers.OutliningEnabled;
            CExpressionMembersCollapsedByDefault = _control.CExpressionMembers.CollapsedByDefault;
            LoopsEnabled = _control.Loops.OutliningEnabled;
            LoopsCollapsedByDefault = _control.Loops.CollapsedByDefault;
            AttributesEnabled = _control.Attributes.OutliningEnabled;
            AttributesCollapsedByDefault = _control.Attributes.CollapsedByDefault;
            CommentsEnabled = _control.Comments.OutliningEnabled;
            CommentsCollapsedByDefault = _control.Comments.CollapsedByDefault;
            XmlDocCommentsEnabled = _control.XmlDocComments.OutliningEnabled;
            XmlDocCommentsCollapsedByDefault = _control.XmlDocComments.CollapsedByDefault;
            TooltipZoomLevel = _control.Tooltip.InputValue;

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

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool AttributesEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool AttributesCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CommentsEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool CommentsCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XmlDocCommentsEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XmlDocCommentsCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public int TooltipZoomLevel { get; set; }

        protected override IWin32Window Window {
            get { return _control = new OutliningOptionsControl(this); }
        }
    }
}
