using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.ComponentModelHost;

namespace FSharpVSPowerTools
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [ProvideOptionPage(typeof(GeneralOptionsPage), "F# Power Tools", "General", 0, 0, true, 0)]
    [ProvideOptionPage(typeof(FantomasOptionsPage), "F# Power Tools", "Formatting", 0, 0, true, 0)]
    [Guid("f152487e-9a22-4cf9-bee6-a8f7c77f828d")]
    [ProvideService(typeof(GeneralOptionsPage))]
    [ProvideService(typeof(FantomasOptionsPage))]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.FSharpProject_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string)]
    public class PowerToolsCommandsPackage : Package
    {
        private VSUtils.SolutionEvents solutionEvents;

        protected override void Initialize()
        {
            base.Initialize();

            var componentModel = (IComponentModel)GetService(typeof(SComponentModel));
            var threadGuard = componentModel.DefaultExportProvider.GetExportedValue<VSUtils.ThreadGuard>();
            threadGuard.BindToCurrentThread();

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
                delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, promote:true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
                delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, promote:true);

            solutionEvents = new VSUtils.SolutionEvents(this);
            ProjectCache.connect(solutionEvents);
        }
    }
}
