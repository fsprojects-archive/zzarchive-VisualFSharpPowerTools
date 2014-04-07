using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.IntegrationTestLibrary;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using EnvDTE;

namespace FSharpVSPowerTools.IntegrationTests.IntegrationTests
{
    [TestClass]
    public class FSharpProjectTests
    {
        private delegate void ThreadInvoker();

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext {get; set;}

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        [HostType("VS IDE")]
        [TestMethod]
        public void FSharpConsoleApplication()
        {
            UIThreadInvoker.Invoke((ThreadInvoker)delegate()
            {
                //Solution and project creation parameters
                const string solutionName = "ConsoleApp";
                const string projectName = "ConsoleApp";

                //Template parameters
                const string language = "FSharp";
                const string projectTemplateName = "Console Application";
                const string itemTemplateName = "Source File";
                const string newFileName = "Test.fs";

                var dte = (DTE)VsIdeTestHostContext.ServiceProvider.GetService(typeof(DTE));

                TestUtils.CreateEmptySolution(TestContext.TestDir, solutionName);
                Assert.AreEqual(0, TestUtils.ProjectCount());

                //Add new  console application project to existing solution
                TestUtils.CreateProjectFromTemplate(projectName, projectTemplateName, language, exclusive: false);

                //Verify that the new project has been added to the solution
                Assert.AreEqual(1, TestUtils.ProjectCount());

                //Get the project
                var project = dte.Solution.Item(1);
                Assert.IsNotNull(project);
                Assert.IsTrue(string.Compare(project.Name, projectName, StringComparison.InvariantCultureIgnoreCase) == 0);

                //Verify Adding new code file to project
                var newCodeFileItem = TestUtils.AddNewItemFromVsTemplate(project.ProjectItems, itemTemplateName, language, newFileName);
                Assert.IsNotNull(newCodeFileItem, "Could not create new project item");
            });
        }
    }
}