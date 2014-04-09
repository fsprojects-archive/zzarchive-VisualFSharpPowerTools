using System;
using System.Globalization;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VsSDK.IntegrationTestLibrary;
using Microsoft.VSSDK.Tools.VsIdeTesting;

namespace FSharpVSPowerTools.IntegrationTests
{
    [TestClass()]
    public class MenuItemTest
    {
        private delegate void ThreadInvoker();

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext { get; set; }

        /// <summary>
        ///A test for lauching the command and closing the associated dialogbox
        ///</summary>
        [TestMethod(), Ignore()]
        [HostType("VS IDE")]
        public void LaunchCommand()
        {
            UIThreadInvoker.Invoke((ThreadInvoker)delegate()
            {
                var menuItemCmd = new CommandID(FSharpVSPowerTools.Refactoring.PkgCmdIDList.GuidBuiltinCmdSet,
                                                (int) FSharpVSPowerTools.Refactoring.PkgCmdIDList.CmdidBuiltinRenameCommand);
                // Create the DialogBoxListener Thread.
                var expectedDialogBoxText = string.Format(CultureInfo.CurrentCulture, "{0}\n\nInside {1}.MenuItemCallback()", "PowerToolsCommandsPackage", "FSharpVSPowerTools.PowerToolsCommandsPackage");
                var purger = new DialogBoxPurger(NativeMethods.IDOK, expectedDialogBoxText);

                try
                {
                    purger.Start();
                    TestUtils.ExecuteCommand(menuItemCmd);
                }
                finally
                {
                    Assert.IsTrue(purger.WaitForDialogThreadToTerminate(), "The dialog box has not shown");
                }
            });
        }
    }
}