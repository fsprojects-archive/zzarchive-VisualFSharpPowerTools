using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.Win32;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using System.Linq;
using Microsoft.VisualStudio.Text.Editor;
using EnvDTE;
using EnvDTE80;
using System.Collections.Generic;
using System.Linq;

namespace TaoLiu.AddReferenceInFSI
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    ///
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the 
    /// IVsPackage interface and uses the registration attributes defined in the framework to 
    /// register itself and its components with the shell.
    /// </summary>
    // This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    // a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]
    // This attribute is used to register the information needed to show this package
    // in the Help/About dialog of Visual Studio.
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    // This attribute is needed to let the shell know that this package exposes some menus.
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [Guid(GuidList.guidAddReferenceInFSIPkgString)]
    [ProvideAutoLoad(Microsoft.VisualStudio.VSConstants.UICONTEXT.NoSolution_string)]
    public sealed class AddReferenceInFSIPackage : Package
    {
        
        static DTE2 dte2 = Package.GetGlobalService(typeof(DTE)) as DTE2;
        static EnvDTE.BuildEvents BuildEvents = dte2.Events.BuildEvents;
        /// <summary>
        /// Default constructor of the package.
        /// Inside this method you can place any initialization code that does not require 
        /// any Visual Studio service because at this point the package object is created but 
        /// not sited yet inside Visual Studio environment. The place to do all the other 
        /// initialization is the Initialize method.
        /// </summary>
        public AddReferenceInFSIPackage()
        {
            BuildEvents.OnBuildDone += BuildEvents_OnBuildDone;
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this.ToString()));
        }

        void BuildEvents_OnBuildDone(vsBuildScope Scope, vsBuildAction Action)
        {
            foreach (Project project in dte2.Solution.Projects)
            {
                if (this.IsCurrentProjectFSharp(project) && 
                    this.ContainsRefScript(project))
                {
                    this.GenerateFile(project);
                }
            }
        }

        private bool ContainsRefScript(Project project)
        {
            var scriptItem = project.ProjectItems.Item("Scripts");
            if (scriptItem == null)
                return false;

            if (scriptItem.ProjectItems != null && scriptItem.ProjectItems.Count > 0)
            {
                for (int i = 0; i < scriptItem.ProjectItems.Count; i++)
                {
                    if (scriptItem.ProjectItems.Item(i + 1).Name.Contains("load-refs"))
                        return true;
                }
            }

            return false;
        }

        private Project GetActiveProject()
        {
            DTE dte = Package.GetGlobalService(typeof(SDTE)) as DTE;
            return GetActiveProject(dte);
        }

        private Project GetActiveProject(DTE dte)
        {
            Project activeProject = null;

            Array activeSolutionProjects = dte.ActiveSolutionProjects as Array;
            if (activeSolutionProjects != null && activeSolutionProjects.Length > 0)
            {
                activeProject = activeSolutionProjects.GetValue(0) as Project;
            }

            return activeProject;
        }

        private bool IsCurrentProjectFSharp(Project proj)
        {
            var result = proj.Kind.ToLower() == "{f2a71f9b-5d33-465a-a702-920d77279786}";
            return result;
        }

        private void AddFileToActiveProject(Project project, string fileName, string content)
        {
            var proj = project;
            var subfolderName = "Scripts";
            if (this.IsCurrentProjectFSharp(proj))
            {
                //create Script folder
                var projectFolder = this.GetProjectFolder(proj);
                var scriptFolder = System.IO.Path.Combine(projectFolder, subfolderName);
                if (!System.IO.Directory.Exists(scriptFolder))
                {
                    System.IO.Directory.CreateDirectory(scriptFolder);
                }

                var path = scriptFolder;
                var textFile = System.IO.Path.Combine(path, fileName);
                using (var writer = System.IO.File.CreateText(textFile))
                {
                    writer.Write(content);
                }
                var projectFolderScript = proj.ProjectItems.Item(subfolderName) != null ?
                        proj.ProjectItems.Item(subfolderName) : proj.ProjectItems.AddFolder(subfolderName);
                projectFolderScript.ProjectItems.AddFromFile(textFile);
                proj.Save();
            }
        }

        private Dictionary<string, string> GetOutputFileFullPathes(VSLangProj.Reference reference)
        {
            var sourceProject = reference.GetType().GetProperty("SourceProject");
            if (sourceProject==null)
                return null;

            var dict = new Dictionary<string, string>();
            var project = sourceProject.GetValue(reference) as Project;
            var result = this.GetProjectOuputs(project);
            return result;            
        }

        private bool IsReferenceProject(VSLangProj.Reference reference)
        {
            var sourceProject = reference.GetType().GetProperty("SourceProject");
            var result = sourceProject != null && sourceProject.GetValue(reference)!=null;
            return result;
        }

        private string GenerateFileContent(Project project, string tag)
        {
            var excludingList = new string[] { "FSharp.Core", "mscorlib" };

            var list = new List<string>();
            var projectRefList = new List<string>();

            if (project.Object is VSLangProj.VSProject)
            {
                VSLangProj.VSProject vsproject = (VSLangProj.VSProject)project.Object;
                for (int i = 0; i < vsproject.References.Count; i++)
                {
                    var reference = vsproject.References.Item(i + 1);
                    if (excludingList.Contains(reference.Name))
                        continue;

                    if (this.IsReferenceProject(reference))
                    {
                        var outputFilePath = this.GetOutputFileFullPathes(reference);
                        if (outputFilePath != null && outputFilePath.ContainsKey(tag))
                        {
                            projectRefList.Add(outputFilePath[tag]);
                        }
                    }
                    else
                    {
                        var fullPath = reference.Path;
                        if (System.IO.File.Exists(fullPath))
                            list.Add(reference.Path);
                    }
                }
            }

            list = list.Select(n => String.Format("#r @\"{0}\"", n)).ToList();
            projectRefList = projectRefList.Select(n=>String.Format("#r @\"{0}\"", n)).ToList();
            var result = String.Format("// Warning: Generated file, your change could be losted when new file is generated. \r\n{0}\r\n\r\n{1}", 
                            String.Join("\r\n", list), 
                            String.Join("\r\n", projectRefList));
            return result;
        }

        private string GetProjectFolder(Project project)
        {
            var projectPath = project.Properties.Item("FullPath").Value.ToString();
            return projectPath;
        }

        private Dictionary<string, string> GetProjectOuputs(Project project)
        {
            var dict = new Dictionary<string, string>();
            var projectPath = this.GetProjectFolder(project);
            var outputFileName = project.Properties.Item("OutputFileName").Value.ToString();
            for (int i = 0; i < project.ConfigurationManager.Count; i++)
            {
                var config = project.ConfigurationManager.Item(i + 1);
                var outputPath = config.Properties.Item("OutputPath").Value.ToString();
                var p = System.IO.Path.Combine(System.IO.Path.Combine(projectPath, outputPath), outputFileName);
                dict.Add(config.ConfigurationName, p);
            }
            return dict;
        }

        private string GenerateLoadScriptContent(Project project, string scriptFile, string tag)
        {
            var projectfolder = System.IO.Path.Combine(this.GetProjectFolder(project), "Scripts");
            var load = String.Format("#load @\"{0}\"", System.IO.Path.Combine(projectfolder, scriptFile));
            var outputs = this.GetProjectOuputs(project);
            if (outputs.ContainsKey(tag))
            {
                var output = outputs[tag];
                var result = String.Format("#r @\"{0}\"\r\n", output);
                return String.Format("{0}\r\n{1}", load, result);
            }
            else
            {
                return String.Format("{0}\r\n", load);
            }
        }

        private void GenerateFile(Project project)
        {
            if (project == null)
                return;            

            var outputs = this.GetProjectOuputs(project);
            foreach (var output in outputs)
            {
                var tag = output.Key;
                var fileName = tag == "Debug" ? "load-refs.fsx" : String.Format("load-refs-{0}.fsx", tag);
                var content = this.GenerateFileContent(project, tag);
                this.AddFileToActiveProject(project, fileName, content);
                content = this.GenerateLoadScriptContent(project, fileName, tag);
                fileName = tag=="Debug" ? "load-project.fsx" : String.Format("load-project-{0}.fsx", tag);
                this.AddFileToActiveProject(project, fileName, content);
            }
        }

        private System.Collections.Generic.List<string> GetReferences(Project project)
        {
            var excludingList = new string[] { "FSharp.Core", "mscorlib" };

            var list = new System.Collections.Generic.List<string>();

            if (project.Object is VSLangProj.VSProject)
            {
                VSLangProj.VSProject vsproject = (VSLangProj.VSProject)project.Object;
                for (int i = 0; i < vsproject.References.Count; i++)
                {
                    var reference = vsproject.References.Item(i+1);                    
                    if (excludingList.Contains(reference.Name))
                        continue;

                    var fullPath = reference.Path;
                    if (System.IO.File.Exists(fullPath))
                        list.Add(reference.Path);
                }
            }

            return list;
        }

        /////////////////////////////////////////////////////////////////////////////
        // Overridden Package Implementation
        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            Trace.WriteLine (string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();

            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if ( null != mcs )
            {
                // Create the command for the menu item.
                CommandID menuCommandID = new CommandID(GuidList.guidAddReferenceInFSICmdSet, (int)PkgCmdIDList.cmdidMyCommand);
                MenuCommand menuItem = new MenuCommand(MenuItemCallback, menuCommandID );
                mcs.AddCommand( menuItem );
            }
        }

        #endregion

        /// <summary>
        /// This function is the callback used to execute a command when the a menu item is clicked.
        /// See the Initialize method to see how the menu item is associated to this function using
        /// the OleMenuCommandService service and the MenuCommand class.
        /// </summary>
        private void MenuItemCallback(object sender, EventArgs e)
        {
            // Show a Message Box to prove we were here
            IVsUIShell shell = (IVsUIShell)GetService(typeof(SVsUIShell));

            IVsWindowFrame frame;
            Guid guid = new Guid("dee22b65-9761-4a26-8fb2-759b971d6dfc");
            shell.FindToolWindow((uint)__VSFINDTOOLWIN.FTW_fForceCreate, ref guid, out frame);
            if (frame != null)
            {
                //if (frame.IsVisible() != 0)
                {
                    frame.Show();
                }

                var project = GetActiveProject();
                var l = GetReferences(project);

                var t = frame.GetType();
                var mi = t.GetProperty("FrameView", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
                var frameView = mi.GetValue(frame);
                var v = frameView.GetType().GetProperty("Content").GetValue(frameView) as System.Windows.DependencyObject;
                var content = v.GetType().GetProperty("Content").GetValue(v) as System.Windows.DependencyObject;
                var content2 = content.GetType().GetProperty("Content").GetValue(content);
                var content3 = content2.GetType().GetProperty("Content").GetValue(content2);
                var content4 = content3.GetType().GetProperty("TextView").GetValue(content3);
                var wpfView = content4 as IWpfTextView;
                var textBuffer = wpfView.TextBuffer;                
                using (var edit = textBuffer.CreateEdit())
                {
                    var line = wpfView.Caret.ContainingTextViewLine;
                    var pos = line.End.Position;

                    string resultString = "";
                    foreach (var item in l)
                    {
                        resultString += String.Format("#r @\"{0}\"\r\n", item);
                    }

                    edit.Insert(pos, resultString.TrimEnd() + ";;");
                    edit.Apply();
                }

                //generate script files
                if (this.IsCurrentProjectFSharp(project))
                {
                    this.GenerateFile(project);
                }
            }
            else
            {
                Guid clsid = Guid.Empty;
                int result;
                Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(shell.ShowMessageBox(
                           0,
                           ref clsid,
                           "",
                           string.Format(CultureInfo.CurrentCulture, "Please open FSI.", this.ToString()),
                           string.Empty,
                           0,
                           OLEMSGBUTTON.OLEMSGBUTTON_OK,
                           OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                           OLEMSGICON.OLEMSGICON_INFO,
                           0,        // false
                           out result));
            }
        }

    }
}
