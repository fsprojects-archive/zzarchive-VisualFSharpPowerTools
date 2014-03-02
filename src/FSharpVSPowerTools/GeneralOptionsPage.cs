using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Configuration;

namespace FSharpVSPowerTools
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("45eabfdf-0a20-4e5e-8780-c3e52360b0f0")]
    public class GeneralOptionsPage : DialogPage
    {   
        private GeneralOptionsControl _optionsControl;
        private const string navBarConfig = "fsharp-navigationbar-enabled";
        
        private bool GetNavigationBarConfig()
        {
            var b = ConfigurationManager.AppSettings.Get(navBarConfig);
            bool result;
            if (b != null && bool.TryParse(b, out result)) return result;
            return false;
        }

        // Return true if set navigation bar config successfully
        private bool SetNavigationBarConfig(bool v)
        {
            try
            {
                // Strangely it doesn't work when debugging inside VS but it works in normal circumstances
                var config = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None);
                config.AppSettings.Settings.Remove(navBarConfig);
                config.AppSettings.Settings.Add(navBarConfig, v.ToString().ToLower());
                config.Save(ConfigurationSaveMode.Minimal);
                return true;
            }
            catch (Exception)
            {
                MessageBox.Show("Can't save navigation bar configuration. Please consider to submit a bug report.", "F# Power Tools");
                return false;
            }
        }

        // We are letting Visual Studio know that these property value needs to be persisted	       

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool XMLDocEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool FormattingEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool NavBarEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool HighlightUsageEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool RenameRefactoringEnabled { get; set; }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)]
        public bool DepthColorizerEnabled { get; set; }


        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        protected override IWin32Window Window
        {
            get
            {
                _optionsControl = new GeneralOptionsControl();
                _optionsControl.OptionsPage = this;
                _optionsControl.XMLDocEnabled = XMLDocEnabled;
                _optionsControl.FormattingEnabled = FormattingEnabled;
                _optionsControl.NavBarEnabled = NavBarEnabled;
                _optionsControl.HighlightUsageEnabled = HighlightUsageEnabled;
                _optionsControl.RenameRefactoringEnabled = RenameRefactoringEnabled;
                _optionsControl.DepthColorizerEnabled = DepthColorizerEnabled;

                return _optionsControl;
            }
        }
        public GeneralOptionsPage()
        {
            XMLDocEnabled = true;
            FormattingEnabled = true;
            NavBarEnabled = GetNavigationBarConfig();
            HighlightUsageEnabled = true;
            RenameRefactoringEnabled = true;
            DepthColorizerEnabled = true;
        }

        // When user clicks on Apply in Options window, get the path selected from control and set it to property of this class so         
        // that Visual Studio saves it.        
        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            if (e.ApplyBehavior == ApplyKind.Apply)
            {
                XMLDocEnabled = _optionsControl.XMLDocEnabled;
                FormattingEnabled = _optionsControl.FormattingEnabled;

                if (SetNavigationBarConfig(_optionsControl.NavBarEnabled))
                {
                    NavBarEnabled = _optionsControl.NavBarEnabled;
                };

                HighlightUsageEnabled = _optionsControl.HighlightUsageEnabled;
                RenameRefactoringEnabled = _optionsControl.RenameRefactoringEnabled;
                DepthColorizerEnabled = _optionsControl.DepthColorizerEnabled;
            }
            base.OnApply(e);
        }
    }
}
