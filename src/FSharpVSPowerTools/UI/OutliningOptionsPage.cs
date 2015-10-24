using System.Runtime.InteropServices;
using System.Windows.Forms;
using FSharpVSPowerTools.UI;
using Microsoft.VisualStudio.Shell;

namespace FSharpVSPowerTools {
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("f7114e2b-7ef5-40f7-87cf-95360f13bd8f")]
    public class OutliningOptionsPage : DialogPage {
        protected override IWin32Window Window {
            get {
                return new OutliningOptionsControl();
            }
        }
    }
}