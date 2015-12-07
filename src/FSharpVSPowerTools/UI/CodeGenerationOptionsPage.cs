using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
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

        ICodeGenerationOptions settings;
        //[ImportingConstructor]
        //public CodeGenerationOptionsPage(ICodeGenerationOptions _settings)
        //{
        //    settings = _settings;
        //}


        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            settings = VFPT_Settings.getCodeGenerationOptions();
            DefaultBody = settings.DefaultBody;
            CodeGenerationOptions = settings.CodeGenerationOptions;
            InterfaceMemberIdentifier = settings.InterfaceMemberIdentifier;
        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            settings = VFPT_Settings.getCodeGenerationOptions();
            settings.DefaultBody = DefaultBody;
            settings.CodeGenerationOptions = CodeGenerationOptions;
            settings.InterfaceMemberIdentifier = InterfaceMemberIdentifier;
            settings.Save();
        }

        public string DefaultBody { get; set; }

        public CodeGenerationKinds CodeGenerationOptions { get; set; }

        public string InterfaceMemberIdentifier { get; set; }

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
            SaveSettingsToStorage();
            base.OnApply(e);
            SettingsContext.triggerSettingsChanged(e);
        }


            base.OnApply(e);
        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
        }

    }

}
