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
        [DisplayName("Background compilation")]
        [Description("Compiling current project in background. May cause high CPU load on large projects.")]
        public bool BackgroundCompilation { get; set; }
    }
}
