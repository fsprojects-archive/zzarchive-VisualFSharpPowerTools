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
        public CodeGenerationOptionsControl()
        {
            InitializeComponent();
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

    }
}
