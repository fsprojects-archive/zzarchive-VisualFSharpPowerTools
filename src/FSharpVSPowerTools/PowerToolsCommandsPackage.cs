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
    public class PowerToolsCommandsPackage : Package, IVsShellPropertyEvents
    {
        private DTE dte;
        private uint cookie;

        [Import] private IClassificationFormatMapService classificationFormatMapService;

        [Import] private IClassificationTypeRegistryService classificationTypeRegistry;

        protected override void Initialize()
        {
            base.Initialize();
            VSUtils.ForegroundThreadGuard.BindThread();

            IVsShell shellService = GetService(typeof(SVsShell)) as IVsShell;

            dte = GetService(typeof (Microsoft.VisualStudio.Shell.Interop.SDTE)) as DTE;

            if (shellService != null)
                ErrorHandler.ThrowOnFailure(shellService.AdviseShellPropertyChanges(this, out cookie));

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote:true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote:true);
        }

        public int OnShellPropertyChange(int propid, object var)
        {
            // when zombie state changes to false, finish package initialization
            if ((int)__VSSPROPID.VSSPROPID_Zombie == propid)
            {
                if ((bool)var == false)
                {
                    // zombie state dependent code
                    this.dte = GetService(typeof(SDTE)) as DTE;

                    // eventlistener no longer needed
                    IVsShell shellService = GetService(typeof(SVsShell)) as IVsShell;

                    if (shellService != null)
                        ErrorHandler.ThrowOnFailure(shellService.UnadviseShellPropertyChanges(this.cookie));

                    this.cookie = 0;

                    // Update colors with the correct theme for the current Visual Studio version
                    InitializeSyntaxColoring(dte);
                }
            }

            return VSConstants.S_OK;
        }

        private void InitializeSyntaxColoring(DTE dte)
        {
            ClassificationFormats.VSVersion = VisualStudioVersionModule.fromDTEVersion(dte.Version);
        }
    }
}
