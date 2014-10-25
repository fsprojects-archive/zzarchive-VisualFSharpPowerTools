using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    public partial class CodeGenerationOptionsControl : UserControl
    {
        private CodeGenerationOptionsPage _optionsPage;
        public CodeGenerationOptionsControl(CodeGenerationOptionsPage optionsPage)
        {
            InitializeComponent();

            _optionsPage = optionsPage;
        }
        
        public CodeGenerationOptionsPage OptionsPage { get; set; }

        public string DefaultBody
        {
            get { return tbxSourceCode.Text; }
            set { tbxSourceCode.Text = value; }
        }

        public CodeGenerationKinds CodeGenerationOptions
        {
            get
            {
                if (rdbFailwith.Checked) return CodeGenerationKinds.Failwith;
                if (rdbNotImplementedYet.Checked) return CodeGenerationKinds.NotImplementedYet;
                if (rdbDefaultValue.Checked) return CodeGenerationKinds.DefaultValue;
                return CodeGenerationKinds.Uncompilable;
            }

            set
            {
                switch (value)
                {
                    case CodeGenerationKinds.Failwith:
                        rdbFailwith.Checked = true;
                        break;
                    case CodeGenerationKinds.NotImplementedYet:
                        rdbNotImplementedYet.Checked = true;
                        break;
                    case CodeGenerationKinds.DefaultValue:
                        rdbDefaultValue.Checked = true;
                        break;
                    default:
                        rdbUncompilable.Checked = true;
                        break;
                }
            }
        }

        private void CodeGenerationOptionsControl_Load(object sender, EventArgs e)
        {
            DefaultBody = _optionsPage.DefaultBody;
            CodeGenerationOptions = _optionsPage.CodeGenerationOptions;
        }

    }
}
