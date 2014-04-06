/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections;
using System.Text;
using System.Reflection;
using System.ComponentModel.Design;
using Microsoft.VsSDK.UnitTestLibrary;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools;

namespace FSharpVSPowerTools.Tests.MenuItemTests
{
    [TestClass()]
    public class MenuItemTest
    {
        /// <summary>
        /// Verify that a new menu command object gets added to the OleMenuCommandService. 
        /// This action takes place In the Initialize method of the Package object
        /// </summary>
        [TestMethod]
        public void InitializeMenuCommand()
        {
            // Create the package
            IVsPackage package = new PowerToolsCommandsPackage() as IVsPackage;
            Assert.IsNotNull(package, "The object does not implement IVsPackage");

            System.Reflection.MethodInfo methodInfo = package.GetType().GetMethod("SetupMenu", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.IsNotNull(methodInfo, "Failed to get the protected method SetupMenu through reflection");
            methodInfo.Invoke(package, null);

            // Create a basic service provider
            OleServiceProvider serviceProvider = OleServiceProvider.CreateOleServiceProviderWithBasicServices();

            // Site the package
            // Assert.AreEqual(0, package.SetSite(serviceProvider), "SetSite did not return S_OK");

            // Verify that the menu command can be found
            CommandID menuCommandID = new CommandID(FSharpVSPowerTools.Folders.PkgCmdConst.guidNewFolderCmdSet,
                                        (int)FSharpVSPowerTools.Folders.PkgCmdConst.cmdNewFolder);
            System.Reflection.MethodInfo info = typeof(Package).GetMethod("GetService", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.IsNotNull(info);
            OleMenuCommandService mcs = info.Invoke(package, new object[] { (typeof(IMenuCommandService)) }) as OleMenuCommandService;
            Assert.IsNotNull(mcs.FindCommand(menuCommandID));
        }

        [TestMethod]
        public void MenuItemCallback()
        {
            // Create the package
            IVsPackage package = new PowerToolsCommandsPackage() as IVsPackage;
            Assert.IsNotNull(package, "The object does not implement IVsPackage");

            // Create a basic service provider
            OleServiceProvider serviceProvider = OleServiceProvider.CreateOleServiceProviderWithBasicServices();

            // Create a UIShell service mock and proffer the service so that it can called from the MenuItemCallback method
            BaseMock uishellMock = UIShellServiceMock.GetUiShellInstance();
            serviceProvider.AddService(typeof(SVsUIShell), uishellMock, true);

            // Site the package
            // Assert.AreEqual(0, package.SetSite(serviceProvider), "SetSite did not return S_OK");

            //Invoke private method on package class and observe that the method does not throw
            System.Reflection.MethodInfo info = package.GetType().GetMethod("SetupMenu", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.IsNotNull(info, "Failed to get the protected method SetupMenu through reflection");
            info.Invoke(package, null);

            //Clean up services
            serviceProvider.RemoveService(typeof(SVsUIShell));

        }
    }
}
