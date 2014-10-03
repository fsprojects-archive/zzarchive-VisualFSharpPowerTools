using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpVSPowerTools
{
    /// <summary>
    /// A class that encapsulates listening for shell events.
    /// </summary>
    public class ShellEventListener : IVsBroadcastMessageEvents, IDisposable
    {
        private const uint WM_SYSCOLORCHANGE = 0x0015;

        private IVsShell shellService;
        private uint broadcastEventCookie;

        private readonly IServiceProvider serviceProvider;

        public event EventHandler OnThemeChanged;

        public ShellEventListener(IServiceProvider serviceProvider)
        {
            this.serviceProvider = serviceProvider;
        }

        private void AdviseBroadcastMessages()
        {
            shellService = serviceProvider.GetService(typeof(SVsShell)) as IVsShell;

            if (shellService != null)
                ErrorHandler.ThrowOnFailure(shellService.AdviseBroadcastMessages(this, out broadcastEventCookie));
        }

        private void UnadviseBroadcastMessages()
        {
            if (shellService != null && broadcastEventCookie != 0)
            {
                shellService.UnadviseBroadcastMessages(broadcastEventCookie);
                broadcastEventCookie = 0;
            }
        }

        /// <summary>
        /// Watch for system color and font change event from Visual Studio.
        /// </summary>
        public int OnBroadcastMessage(uint msg, IntPtr wParam, IntPtr lParam)
        {
            if (msg == WM_SYSCOLORCHANGE)
            {
                if (OnThemeChanged != null)
                {
                    OnThemeChanged(this, EventArgs.Empty);
                }
            }
            return VSConstants.S_OK;
        }

        public void Initialize()
        {
            AdviseBroadcastMessages();
        }

        public void Dispose()
        {
            UnadviseBroadcastMessages();
        }
    }
}
