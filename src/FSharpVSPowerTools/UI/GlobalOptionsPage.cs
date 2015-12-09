using System.ComponentModel;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;

namespace FSharpVSPowerTools
{
    [Guid("CE38C84E-BE03-472C-8741-952DAE4EDA2B")]
    public class GlobalOptionsPage : DialogPage
    {


        IGlobalOptions settings;

        public GlobalOptionsPage ()
        {
            settings = new GlobalOptions();
        }


        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            settings.Load();
            BackgroundCompilation = settings.BackgroundCompilation;
            DiagnosticMode        = settings.DiagnosticMode;
            ProjectCacheSize      = settings.ProjectCacheSize;
        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            settings.BackgroundCompilation = BackgroundCompilation;
            settings.DiagnosticMode = DiagnosticMode;
            settings.ProjectCacheSize = ProjectCacheSize;
            settings.Save();

        }

        [Category("Debugging")]
        [DisplayName("Diagnostic Mode")]
        [Description("Print out stacktraces and log information to Visual Studio Output panel.")]
        public bool DiagnosticMode { get; set; }

        [Category("Performance")]
        [DisplayName("Background Compilation")]
        [Description("Compile current project in background. Enabling the option may cause high CPU load on large projects.")]
        public bool BackgroundCompilation { get; set; }

        [Category("Performance")]
        [DisplayName("Project Cache Size")]
        [Description("The number of projects where their parse and check results are cached. A large value may cause high memory load, " +
                     "which will make Visual Studio sluggish.")]
        public int ProjectCacheSize { get; set; }

        protected override void OnApply(DialogPage.PageApplyEventArgs e)
        {
            base.OnApply(e);
            SaveSettingsToStorage();
            SettingsContext.triggerSettingsChanged(e);
        }


    }
}
