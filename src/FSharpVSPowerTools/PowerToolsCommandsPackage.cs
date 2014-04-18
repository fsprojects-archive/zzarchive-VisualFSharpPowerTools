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
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Formatting;
using EnvDTE;
using EnvDTE80;
using FSharpVSPowerTools;
using FSharpVSPowerTools.Navigation;
using FSharpVSPowerTools.Folders;
using FSharpVSPowerTools.ProjectSystem;

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
        private FolderMenuCommands newFolderMenu;
        private FSharpLibrary library;

        private uint broadcastEventCookie;
        private uint pctCookie;
        private uint objectManagerCookie;
        
        internal static Lazy<DTE2> DTE
            = new Lazy<DTE2>(() => ServiceProvider.GlobalProvider.GetService(typeof(DTE)) as DTE2);

        private ClassificationColorManager classificationColorManager;

        protected override void Initialize()
        {
            base.Initialize();
            VSUtils.ForegroundThreadGuard.BindThread();

            var componentModel = (IComponentModel)GetService(typeof(SComponentModel));
            classificationColorManager = componentModel.DefaultExportProvider.GetExportedValue<ClassificationColorManager>();

            AdviseBroadcastMessages();

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote: true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote: true);

            var generalOptions = GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (generalOptions.FolderOrganizationEnabled)
            {
                SetupMenu();
                RegisterPriorityCommandTarget();
            }

            library = new FSharpLibrary(Navigation.PkgCmdConst.guidSymbolLibrary);
            library.LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT;

            RegisterLibrary();
        }

        private void AdviseBroadcastMessages()
        {
            shellService = GetService(typeof(SVsShell)) as IVsShell;

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

        private void SetupMenu()
        {
            var mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            var shell = GetService(typeof(SVsUIShell)) as IVsUIShell;

            if (mcs != null)
            {
                newFolderMenu = new FolderMenuCommands(DTE.Value, mcs, shell);
                newFolderMenu.SetupCommands();
            }
        }

        private void RegisterPriorityCommandTarget()
        {
            var rpct = (IVsRegisterPriorityCommandTarget)GetService(typeof(SVsRegisterPriorityCommandTarget));
            rpct.RegisterPriorityCommandTarget(0, newFolderMenu, out pctCookie);
        }

        private void UnregisterPriorityCommandTarget()
        {
            if (pctCookie != 0)
            {
                var rpct = (IVsRegisterPriorityCommandTarget)GetService(typeof(SVsRegisterPriorityCommandTarget));
                if (rpct != null)
                {
                    rpct.UnregisterPriorityCommandTarget(pctCookie);
                    pctCookie = 0;
                }
            }
        }    

        private void RegisterLibrary()
        {
            if (objectManagerCookie == 0)
            {
                IVsObjectManager2 objManager = GetService(typeof(SVsObjectManager)) as IVsObjectManager2;
                if (objManager == null) return;
                ErrorHandler.ThrowOnFailure(objManager.RegisterSimpleLibrary(library, out objectManagerCookie));
            }
        }

        private void UnregisterLibrary()
        {
            if (objectManagerCookie != 0)
            {
                IVsObjectManager2 objManager = GetService(typeof(SVsObjectManager)) as IVsObjectManager2;
                if (objManager != null)
                    objManager.UnregisterLibrary(objectManagerCookie);
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
            UnadviseBroadcastMessages();
            UnregisterPriorityCommandTarget();
            UnregisterLibrary();
        }   
    }
}
