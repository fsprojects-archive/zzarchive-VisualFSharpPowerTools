using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")]
    public class GeneralOptionsPage : DialogPage
    {
        
        private GeneralOptionsControl _optionsControl;

        // We are letting Visual Studio know that this property value needs to be persisted	       

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XMLDocEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FormattingEnabled { get; set; }

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                _optionsControl = new GeneralOptionsControl();
                _optionsControl.OptionsPage = this;
                _optionsControl.XMLDocEnabled = XMLDocEnabled;
                _optionsControl.FormattingEnabled = FormattingEnabled;

                return _optionsControl;
            }
        }
        public GeneralOptionsPage()
        {
            XMLDocEnabled = true;
            FormattingEnabled = true;
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so         
        // that Visual Studio saves it.        
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                XMLDocEnabled = _optionsControl.XMLDocEnabled;
                FormattingEnabled = _optionsControl.FormattingEnabled;

                // FIXME: global mutable state
                Program.XmlDocFilter.XMLDocEnabled = XMLDocEnabled;
            }
            base.OnApply(e);
        }
    }
}
