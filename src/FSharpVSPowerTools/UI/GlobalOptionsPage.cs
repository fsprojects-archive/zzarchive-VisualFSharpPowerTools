using System.ComponentModel;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using FSharp.Editing.VisualStudio;

namespace FSharpVSPowerTools
{
    [Guid("CE38C84E-BE03-472C-8741-952DAE4EDA2B")]
    public class GlobalOptionsPage : DialogPage, IGlobalOptions
    {
        public GlobalOptionsPage()
        {
            DiagnosticMode = false;
            BackgroundCompilation = true;
            ProjectCacheSize = 50;
            PeekStandaloneFilesEnabled = false;
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

        [Category("Miscellaneous")]
        [DisplayName("Enable Peek Definition on standalone files")]
        [Description("Caution: Enabling this feature may block AltGr keys on non-English keyboard layouts.")]
        public bool PeekStandaloneFilesEnabled { get; set; }

    }
}
