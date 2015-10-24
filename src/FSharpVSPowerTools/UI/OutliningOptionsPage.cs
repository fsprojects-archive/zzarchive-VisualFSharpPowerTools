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

        public OutliningOptionsPage() {
            ToplevelEnabled = true;
        }

        protected override void OnApply(PageApplyEventArgs e) {
            ToplevelEnabled                  = _control.TopLevel.OutliningEnabled;
            ToplevelCollapsedByDefault       = _control.TopLevel.CollapsedByDefault;
            MatchStatementEnabled            = _control.MatchStatements.OutliningEnabled;
            MatchStatementCollapsedByDefault = _control.MatchStatements.CollapsedByDefault;

            base.OnApply(e);
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool ToplevelEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool ToplevelCollapsedByDefault { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool MatchStatementEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool MatchStatementCollapsedByDefault { get; set; }

        protected override IWin32Window Window {
            get { return _control = new OutliningOptionsControl(this); }
        }
    }
}
