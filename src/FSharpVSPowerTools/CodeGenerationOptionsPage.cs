using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("1023983E-7F9C-4DB5-AF81-2A1546B209A6")]
    public class CodeGenerationOptionsPage : DialogPage
    {
        private CodeGenerationOptionsControl _optionsControl;

        public CodeGenerationOptionsPage()
        {
            DefaultBody = "??";
            CodeGenerationOptions = CodeGenerationOptions.Failwith;
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public string DefaultBody { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public CodeGenerationOptions CodeGenerationOptions { get; set; }


        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                _optionsControl = new CodeGenerationOptionsControl
                                    {
                                        OptionsPage = this,
                                        DefaultBody = this.DefaultBody,
                                        CodeGenerationOptions = this.CodeGenerationOptions
                                    };
                
                return _optionsControl;
            }
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so         
        // that Visual Studio saves it.        
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                this.DefaultBody = _optionsControl.DefaultBody;
                this.CodeGenerationOptions = _optionsControl.CodeGenerationOptions;
            }

            base.OnApply(e);
        }

    }

}
