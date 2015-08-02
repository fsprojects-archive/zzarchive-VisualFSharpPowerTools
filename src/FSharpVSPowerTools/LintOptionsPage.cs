using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Windows;
using FSharpVSPowerTools.Linting;

namespace FSharpVSPowerTools
{
    [Guid("f0bb4785-e75a-485f-86e8-e382dd5934a4")]
    public class LintOptionsPage : UIElementDialogPage
    {
        UIElement lintOptionsPageControl;

        protected override UIElement Child
        {
            get {
                return lintOptionsPageControl ?? (lintOptionsPageControl = new LintOptionsControlProvider());
            }
        }
    }
}