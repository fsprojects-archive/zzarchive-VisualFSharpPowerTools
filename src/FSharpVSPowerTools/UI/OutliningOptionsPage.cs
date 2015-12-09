using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using FSharpVSPowerTools.UI;
using Microsoft.VisualStudio.Shell;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("f7114e2b-7ef5-40f7-87cf-95360f13bd8f")]
    public class OutliningOptionsPage : DialogPage
    {
        OutliningOptionsControl _control;
        IOutliningOptions settings;

        public OutliningOptionsPage()
        {
            settings = new OutliningOptions();
        }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            settings.Load();
            OpensEnabled = settings.OpensEnabled;
            OpensCollapsedByDefault = settings.OpensCollapsedByDefault;
            ModulesEnabled = settings.ModulesEnabled;
            ModulesCollapsedByDefault = settings.ModulesCollapsedByDefault;
            HashDirectivesEnabled = settings.HashDirectivesEnabled;
            HashDirectivesCollapsedByDefault = settings.HashDirectivesCollapsedByDefault;
            TypesEnabled = settings.TypesEnabled;
            TypesCollapsedByDefault = settings.TypesCollapsedByDefault;
            SimpleTypesEnabled = settings.SimpleTypesEnabled;
            SimpleTypesCollapsedByDefault = settings.SimpleTypesCollapsedByDefault;
            TypeExpressionsEnabled = settings.TypeExpressionsEnabled;
            TypeExpressionsCollapsedByDefault = settings.TypeExpressionsCollapsedByDefault;
            MembersEnabled = settings.MembersEnabled;
            MembersCollapsedByDefault = settings.MembersCollapsedByDefault;
            LetOrUseEnabled = settings.LetOrUseEnabled;
            LetOrUseCollapsedByDefault = settings.LetOrUseCollapsedByDefault;
            CollectionsEnabled = settings.CollectionsEnabled;
            CollectionsCollapsedByDefault = settings.CollectionsCollapsedByDefault;
            PatternMatchesEnabled = settings.PatternMatchesEnabled;
            PatternMatchesCollapsedByDefault = settings.PatternMatchesCollapsedByDefault;
            TryWithFinallyEnabled = settings.TryWithFinallyEnabled;
            TryWithFinallyCollapsedByDefault = settings.TryWithFinallyCollapsedByDefault;
            IfThenElseEnabled = settings.IfThenElseEnabled;
            IfThenElseCollapsedByDefault = settings.IfThenElseCollapsedByDefault;
            CExpressionMembersEnabled = settings.CExpressionMembersEnabled;
            CExpressionMembersCollapsedByDefault = settings.CExpressionMembersCollapsedByDefault;
            LoopsEnabled = settings.LoopsEnabled;
            LoopsCollapsedByDefault = settings.LoopsCollapsedByDefault;
            AttributesEnabled = settings.AttributesEnabled;
            AttributesCollapsedByDefault = settings.AttributesCollapsedByDefault;
            XmlDocCommentsEnabled = settings.XmlDocCommentsEnabled;
            XmlDocCommentsCollapsedByDefault = settings.XmlDocCommentsCollapsedByDefault;
            CommentsEnabled = settings.CommentsEnabled;
            CommentsCollapsedByDefault = settings.CommentsCollapsedByDefault;
            TooltipZoomLevel = settings.TooltipZoomLevel;
        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            settings.OpensCollapsedByDefault = OpensCollapsedByDefault;
            settings.ModulesEnabled = ModulesEnabled;
            settings.ModulesCollapsedByDefault = ModulesCollapsedByDefault;
            settings.HashDirectivesEnabled = HashDirectivesEnabled;
            settings.HashDirectivesCollapsedByDefault = HashDirectivesCollapsedByDefault;
            settings.TypesEnabled = TypesEnabled;
            settings.TypesCollapsedByDefault = TypesCollapsedByDefault;
            settings.SimpleTypesEnabled = SimpleTypesEnabled;
            settings.SimpleTypesCollapsedByDefault = SimpleTypesCollapsedByDefault;
            settings.TypeExpressionsEnabled = TypeExpressionsEnabled;
            settings.TypeExpressionsCollapsedByDefault = TypeExpressionsCollapsedByDefault;
            settings.MembersEnabled = MembersEnabled;
            settings.MembersCollapsedByDefault = MembersCollapsedByDefault;
            settings.LetOrUseEnabled = LetOrUseEnabled;
            settings.LetOrUseCollapsedByDefault = LetOrUseCollapsedByDefault;
            settings.CollectionsEnabled = CollectionsEnabled;
            settings.CollectionsCollapsedByDefault = CollectionsCollapsedByDefault;
            settings.PatternMatchesEnabled = PatternMatchesEnabled;
            settings.PatternMatchesCollapsedByDefault = PatternMatchesCollapsedByDefault;
            settings.TryWithFinallyEnabled = TryWithFinallyEnabled;
            settings.TryWithFinallyCollapsedByDefault = TryWithFinallyCollapsedByDefault;
            settings.IfThenElseEnabled = IfThenElseEnabled;
            settings.IfThenElseCollapsedByDefault = IfThenElseCollapsedByDefault;
            settings.CExpressionMembersEnabled = CExpressionMembersEnabled;
            settings.CExpressionMembersCollapsedByDefault = CExpressionMembersCollapsedByDefault;
            settings.LoopsEnabled = LoopsEnabled;
            settings.LoopsCollapsedByDefault = LoopsCollapsedByDefault;
            settings.AttributesEnabled = AttributesEnabled;
            settings.AttributesCollapsedByDefault = AttributesCollapsedByDefault;
            settings.XmlDocCommentsEnabled = XmlDocCommentsEnabled;
            settings.XmlDocCommentsCollapsedByDefault = XmlDocCommentsCollapsedByDefault;
            settings.CommentsEnabled = CommentsEnabled;
            settings.CommentsCollapsedByDefault = CommentsCollapsedByDefault;
            settings.TooltipZoomLevel = TooltipZoomLevel;
            settings.Save();

        }


        public bool OpensEnabled { get; set; }
        public bool OpensCollapsedByDefault { get; set; }
        public bool ModulesEnabled { get; set; }
        public bool ModulesCollapsedByDefault { get; set; }
        public bool HashDirectivesEnabled { get; set; }
        public bool HashDirectivesCollapsedByDefault { get; set; }
        public bool TypesEnabled { get; set; }
        public bool TypesCollapsedByDefault { get; set; }
        public bool SimpleTypesEnabled { get; set; }
        public bool SimpleTypesCollapsedByDefault { get; set; }
        public bool TypeExpressionsEnabled { get; set; }
        public bool TypeExpressionsCollapsedByDefault { get; set; }
        public bool MembersEnabled { get; set; }
        public bool MembersCollapsedByDefault { get; set; }
        public bool LetOrUseEnabled { get; set; }
        public bool LetOrUseCollapsedByDefault { get; set; }
        public bool CollectionsEnabled { get; set; }
        public bool CollectionsCollapsedByDefault { get; set; }
        public bool PatternMatchesEnabled { get; set; }
        public bool PatternMatchesCollapsedByDefault { get; set; }
        public bool TryWithFinallyEnabled { get; set; }
        public bool TryWithFinallyCollapsedByDefault { get; set; }
        public bool IfThenElseEnabled { get; set; }
        public bool IfThenElseCollapsedByDefault { get; set; }
        public bool CExpressionMembersEnabled { get; set; }
        public bool CExpressionMembersCollapsedByDefault { get; set; }
        public bool LoopsEnabled { get; set; }
        public bool LoopsCollapsedByDefault { get; set; }
        public bool AttributesEnabled { get; set; }
        public bool AttributesCollapsedByDefault { get; set; }
        public bool CommentsEnabled { get; set; }
        public bool CommentsCollapsedByDefault { get; set; }
        public bool XmlDocCommentsEnabled { get; set; }
        public bool XmlDocCommentsCollapsedByDefault { get; set; }
        public int TooltipZoomLevel { get; set; }

        protected override IWin32Window Window
        {
            get { return _control = new OutliningOptionsControl(this); }
        }

        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            base.OnApply(e);

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
            AttributesEnabled = _control.Attributes.Enabled;
            AttributesCollapsedByDefault = _control.Attributes.CollapsedByDefault;
            XmlDocCommentsEnabled = _control.XmlDocComments.Enabled;
            XmlDocCommentsCollapsedByDefault = _control.XmlDocComments.CollapsedByDefault;
            CommentsEnabled = _control.Comments.Enabled;
            CommentsCollapsedByDefault = _control.Comments.CollapsedByDefault;
            TooltipZoomLevel = _control.Tooltip.InputValue;


            SaveSettingsToStorage();
            SettingsContext.triggerSettingsChanged(e);
        }

    }
}
