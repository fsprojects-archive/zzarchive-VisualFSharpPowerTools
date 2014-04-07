using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using EnvDTE;
using System.IO;
using Microsoft.VsSDK.IntegrationTestLibrary;

namespace FSharpVSPowerTools.IntegrationTests.IntegrationTests
{
    [TestClass]
    public class SolutionTests
    {
        private delegate void ThreadInvoker();

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext { get; set; }

        [TestMethod]
        [HostType("VS IDE")]
        public void CreateEmptySolution()
        {
            UIThreadInvoker.Invoke((ThreadInvoker)delegate()
            {
                TestUtils.CloseCurrentSolution(__VSSLNSAVEOPTIONS.SLNSAVEOPT_NoSave);
                TestUtils.CreateEmptySolution(TestContext.TestDir, "EmptySolution");
            });
        }
    }
}