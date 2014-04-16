using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.Navigation;

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
    public class PowerToolsCommandsPackage : Package
    {
        private uint objectManagerCookie;
        private FSharpLibrary library;

        protected override void Initialize()
        {
            base.Initialize();
            VSUtils.ForegroundThreadGuard.BindThread();

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote:true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote:true);

            library = new FSharpLibrary(PkgCmdConst.guidSymbolLibrary);
            library.LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT;

            RegisterLibrary();
        }

        private void RegisterLibrary()
        {
            if (0 == objectManagerCookie)
            {
                IVsObjectManager2 objManager = GetService(typeof(SVsObjectManager)) as IVsObjectManager2;
                if (null == objManager)
                {
                    return;
                }
                ErrorHandler.ThrowOnFailure(objManager.RegisterSimpleLibrary(library, out objectManagerCookie));
            }
        }
    }
}
