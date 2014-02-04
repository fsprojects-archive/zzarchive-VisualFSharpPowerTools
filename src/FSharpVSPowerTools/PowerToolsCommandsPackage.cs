using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;

namespace FSharpVSPowerTools
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideOptionPage(typeof(GeneralOptionsPage), "F# Power Tools", "General", 0, 0, true, 0)]
    [ProvideOptionPage(typeof(FantomasOptionsPage), "F# Power Tools", "Formatting", 0, 0, true, 0)]
    [Guid("684211D1-B47C-44FE-AECF-E9D3B5FF67E3")]
    [ProvideService(typeof(GeneralOptionsPage))]
    [ProvideService(typeof(FantomasOptionsPage))]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.FSharpProject_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string)]
    public class PowerToolsCommandsPackage : Package
    {
        private static PowerToolsCommandsPackage _instance;

        public PowerToolsCommandsPackage()
        {
            _instance = this;
        }

        protected override void Initialize()
        {
            base.Initialize();

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(GeneralOptionsPage),
              delegate { return GetDialogPage(typeof(GeneralOptionsPage)); }, true);
            serviceContainer.AddService(typeof(FantomasOptionsPage),
              delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, true);
        }
    }
}
