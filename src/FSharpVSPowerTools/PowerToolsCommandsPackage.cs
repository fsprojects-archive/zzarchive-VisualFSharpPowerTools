using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.ComponentModelHost;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Formatting;

namespace FSharpVSPowerTools
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [ProvideOptionPage(typeof(GeneralOptionsPage), Resource.vsPackageTitle, "General", 0, 0, true, 0)]
    [ProvideOptionPage(typeof(FantomasOptionsPage), Resource.vsPackageTitle, "Formatting", 0, 0, true, 0)]
    [Guid("f152487e-9a22-4cf9-bee6-a8f7c77f828d")]
    [ProvideService(typeof(GeneralOptionsPage))]
    [ProvideService(typeof(FantomasOptionsPage))]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.FSharpProject_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string)]
    public class PowerToolsCommandsPackage : Package, IVsBroadcastMessageEvents, IDisposable
    {
        private const uint WM_SYSCOLORCHANGE = 0x0015;
        private IVsShell shellService;
        private uint broadcastEventCookie;
        
        private ClassificationColorManager colorManager = null;
        private ThemeManager themeManager = null;

        protected override void Initialize()
        {
            base.Initialize();
            VSUtils.ForegroundThreadGuard.BindThread();

            var componentModel = (IComponentModel)GetService(typeof(SComponentModel));
            colorManager = componentModel.DefaultExportProvider.GetExportedValue<ClassificationColorManager>();
            themeManager = componentModel.DefaultExportProvider.GetExportedValue<ThemeManager>();

            shellService = GetService(typeof(SVsShell)) as IVsShell;

            if (shellService != null)
                ErrorHandler.ThrowOnFailure(shellService.AdviseBroadcastMessages(this, out broadcastEventCookie));

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote:true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote:true);
        }

        public int OnBroadcastMessage(uint msg, IntPtr wParam, IntPtr lParam)
        {
            if (msg == WM_SYSCOLORCHANGE)
            {
                colorManager.UpdateColors(themeManager.GetCurrentTheme());
            }
            return VSConstants.S_OK;
        }

        public void Dispose()
        {
            if (shellService != null && broadcastEventCookie != 0)
            {
                shellService.UnadviseBroadcastMessages(broadcastEventCookie);
                broadcastEventCookie = 0;
            }
        }
    }
}
