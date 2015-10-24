using System.ComponentModel;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;

namespace FSharpVSPowerTools
{
    [Guid("CE38C84E-BE03-472C-8741-952DAE4EDA2B")]
    public class GlobalOptionsPage : DialogPage, IGlobalOptions
    {
        public GlobalOptionsPage()
        {
            StrictMode = false;
            DiagnosticMode = false;
            BackgroundCompilation = true;
            ProjectCacheSize = 50;
        }

        [Category("Caching")]
        [DisplayName("Strict Mode")]
        [Description("Invalidate caches aggressively in order to provide more up-to-date results.")]
        public bool StrictMode { get; set; }

        [Category("Debugging")]
        [DisplayName("Diagnostic Mode")]
        [Description("Print out stacktraces and log information to Visual Studio Output panel.")]
        public bool DiagnosticMode { get; set; }

        [Category("Performance")]
        [DisplayName("Background Compilation")]
        [Description("Compiling current project in background. May cause high CPU load on large projects.")]
        public bool BackgroundCompilation { get; set; }

        [Category("Performance")]
        [DisplayName("Project Cache Size")]
        [Description("How many project's parse and check results are cached. Too large value may cause high memory load, " +
                     "which will make Visual Studio unusable.")]
        public int ProjectCacheSize { get; set; }
    }
}
