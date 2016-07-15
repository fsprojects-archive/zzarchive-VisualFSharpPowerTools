using FSharp.Editing;
using FSharp.Editing.VisualStudio;
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
    public class CodeGenerationOptionsPage : DialogPage, ICodeGenerationOptions
    {
        private CodeGenerationOptionsControl _optionsControl;

        public CodeGenerationOptionsPage()
        {
            DefaultBody = "??";
            CodeGenerationOptions = CodeGenerationKinds.Failwith;
            InterfaceMemberIdentifier = "x";
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public string DefaultBody { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public CodeGenerationKinds CodeGenerationOptions { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public string InterfaceMemberIdentifier { get; set; }

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                _optionsControl = new CodeGenerationOptionsControl(this);
                return _optionsControl;
            }
        }

        private bool isValidIdentifier(string ident)
        {
            bool valid = IdentifierUtils.isFixableIdentifier(ident);
            if (!valid)
            {
                LoggingModule.messageBoxError(Resource.invalidIdentifierMessage);
            }
            return valid;
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so         
        // that Visual Studio saves it.        
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                if (InterfaceMemberIdentifier != _optionsControl.InterfaceMemberIdentifier)
                {
                    if (!isValidIdentifier(_optionsControl.InterfaceMemberIdentifier))
                    {
                        // Keep the dialog open in the case of errors
                        e.ApplyBehavior = ApplyKind.CancelNoNavigate;
                        base.OnApply(e);
                        return;
                    }
                    InterfaceMemberIdentifier = _optionsControl.InterfaceMemberIdentifier;
                }

                DefaultBody = _optionsControl.DefaultBody;
                CodeGenerationOptions = _optionsControl.CodeGenerationOptions;
            }

            base.OnApply(e);
        }

    }

}
