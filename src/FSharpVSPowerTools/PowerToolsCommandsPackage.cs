using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.ComponentModelHost;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Formatting;
using EnvDTE;
using EnvDTE80;
using FSharpVSPowerTools.Folders;

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
        
        internal static Lazy<DTE2> DTE
            = new Lazy<DTE2>(() => ServiceProvider.GlobalProvider.GetService(typeof(DTE)) as DTE2);

        private ClassificationColorManager classificationColorManager;

        protected override void Initialize()
        {
            base.Initialize();
            VSUtils.ForegroundThreadGuard.BindThread();

            var componentModel = (IComponentModel)GetService(typeof(SComponentModel));
            classificationColorManager = componentModel.DefaultExportProvider.GetExportedValue<ClassificationColorManager>();
            
            shellService = GetService(typeof(SVsShell)) as IVsShell;

            if (shellService != null)
                ErrorHandler.ThrowOnFailure(shellService.AdviseBroadcastMessages(this, out broadcastEventCookie));

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote:true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote:true);

            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            var shell = GetService(typeof(SVsUIShell)) as IVsUIShell;
            var logger = new Logger(ServiceProvider.GlobalProvider);

            if (mcs != null)
            {
                var newFolderMenu = new FolderMenuCommands(DTE.Value, mcs, shell);
                newFolderMenu.SetupCommands();
            }

        }

        public int OnBroadcastMessage(uint msg, IntPtr wParam, IntPtr lParam)
        {
            if (msg == WM_SYSCOLORCHANGE)
            {
                classificationColorManager.UpdateColors();
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
