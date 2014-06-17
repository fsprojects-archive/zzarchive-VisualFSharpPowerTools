using System;
using System.Windows.Forms;

namespace FSharpVSPowerTools
{
    public enum CodeGenerationOptions
    {
        Failwith,
        NotImplementedYet,
        DefaultValue,
        Uncompilable
    }
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

        public CodeGenerationOptions CodeGenerationOptions
        {
            get
            {
                if (rdbFailwith.Checked) return CodeGenerationOptions.Failwith;
                if (rdbNotImplementedYet.Checked) return CodeGenerationOptions.NotImplementedYet;
                if (rdbDefaultValue.Checked) return CodeGenerationOptions.DefaultValue;
                return CodeGenerationOptions.Uncompilable;
            }

            set
            {
                switch (value)
                {
                    case CodeGenerationOptions.Failwith:
                        rdbFailwith.Checked = true;
                        break;
                    case CodeGenerationOptions.NotImplementedYet:
                        rdbNotImplementedYet.Checked = true;
                        break;
                    case CodeGenerationOptions.DefaultValue:
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
