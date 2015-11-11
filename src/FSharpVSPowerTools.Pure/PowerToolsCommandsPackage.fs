namespace FSharpVSPowerTools

open System                                    
open System.ComponentModel.Design              
open System.Runtime.InteropServices            
open Microsoft.VisualStudio                    
open Microsoft.VisualStudio.Shell              
open Microsoft.VisualStudio.Shell.Interop      
open EnvDTE                                    
open EnvDTE80                                  
open FSharpVSPowerTools.Navigation             
open FSharpVSPowerTools.Folders                
open FSharpVSPowerTools.ProjectSystem          
open FSharpVSPowerTools.TaskList               
open System.ComponentModel.Composition         
open System.Diagnostics                        
open Microsoft.VisualStudio.ComponentModelHost 
open FSharpVSPowerTools.Reference              

module private useless = ()

[<PackageRegistration(UseManagedResourcesOnly = true)>]
[<ProvideMenuResource(resourceID= "Menus.ctmenu", version=1)>]
[<InstalledProductRegistration("#110", "#112", AssemblyVersionInformation.Version, IconResourceID = 400)>]
[<ProvideBindingPath>]
//[<ProvideOptionPage(typeof<GeneralOptionsPage>, Resource.vsPackageTitle, "General", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[<ProvideOptionPage(typeof<FantomasOptionsPage>, Resource.vsPackageTitle, "Formatting", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[<ProvideOptionPage(typeof<CodeGenerationOptionsPage>, Resource.vsPackageTitle, "Code Generation", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[<ProvideOptionPage(typeof<GlobalOptionsPage>, Resource.vsPackageTitle, "Configuration", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[<ProvideOptionPage(typeof<Linting.LintOptionsPage>, Resource.vsPackageTitle, "Lint", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[<ProvideOptionPage(typeof<OutliningOptionsPage>, Resource.vsPackageTitle, "Outlining", categoryResourceID= 0, pageNameResourceID= 0, supportsAutomation= true, keywordListResourceId= 0)>]
//[< ProvideService (typeof<IGeneralOptions>)>]   
//[< ProvideService (typeof<IFormattingOptions>)>]
//[< ProvideService (typeof<ICodeGenerationOptions>)>]
//[< ProvideService (typeof<IGlobalOptions>)>]
//[< ProvideService (typeof<ILintOptions>)>]
[< Guid "1F699E38-7D87-44F4-BC08-6B1DD5A6F926">]
[< ProvideAutoLoad (VSConstants.UICONTEXT.SolutionExists_string)>]
[< ProvideAutoLoad (VSConstants.UICONTEXT.FSharpProject_string)>]
[< ProvideAutoLoad (VSConstants.UICONTEXT.NoSolution_string)>]
type PowerToolsCommandsPackage () as self =
    inherit Package()

    let mutable  pctCookie = 0u
    let mutable  objectManagerCookie = 0u
    let mutable  library = Unchecked.defaultof<FSharpLibrary>

    static let DTE = 
        Lazy<DTE2> (fun () -> ServiceProvider.GlobalProvider.GetService<DTE2,DTE> ())

    member __.GetDialogPage<'a>() = base.GetDialogPage(typeof<'a>)
    
    member __.GetService<'IVs,'SVs>() = self.GetService(typeof<'SVs>) :?> 'IVs

    override __.Initialize () =
        base.Initialize ()
        VSUtils.ForegroundThreadGuard.BindThread ()
        
        let serviceContainer = self :> IServiceContainer
//        serviceContainer.AddService<IGeneralOptions> 
//            (self.GetDialogPage<IGeneralOptions>()
//            
//            )
//        
        //let generalOptions 
        library <- FSharpLibrary Constants.guidSymbolLibrary
        library.LibraryCapabilities <-  Enum.Parse( typedefof<_LIB_FLAGS2>, _LIB_FLAGS.LF_PROJECT.ToString()) :?> _LIB_FLAGS2
       // library.LibraryCapabilities <-   asEnum<_LIB_FLAGS2>  _LIB_FLAGS.LF_PROJECT

        self.RegisterLibrary ()




    member private __.RegisterLibrary () =
        if objectManagerCookie = 0u then
            let objManager = self.TryGetService<IVsObjectManager2, SVsObjectManager>()
            match  objManager with
            | None -> () 
            | Some objManager -> ErrorHandler.ThrowOnFailure 
                                    (objManager.RegisterSimpleLibrary (library, &objectManagerCookie)) |> ignore

    member private __.UnregisterLibrary () =
        if objectManagerCookie <> 0u then
            let objManager = self.TryGetService<IVsObjectManager2, SVsObjectManager>()
            match  objManager with
            | None -> () 
            | Some objManager -> ErrorHandler.ThrowOnFailure 
                                    (objManager.RegisterSimpleLibrary (library, &objectManagerCookie)) |> ignore

    interface IDisposable with
        member x.Dispose(): unit = 
        //    UnregisterPriorityCommandTarget()
            self.UnregisterLibrary()
//            if (taskListCommentManager != null)
//                (taskListCommentManager as IDisposable).Dispose()
//            if (fsiReferenceMenu != null)
//                (fsiReferenceMenu as IDisposable).Dispose()
//        }
//

//    public class PowerToolsCommandsPackage : Package, IDisposable
//    {
//        private FolderMenuCommands newFolderMenu
//        private FsiReferenceCommand fsiReferenceMenu
//        private FSharpLibrary library
//
//        private uint pctCookie
//        private uint objectManagerCookie
//        
//        internal static Lazy<DTE2> DTE
//            = new Lazy<DTE2>(() => ServiceProvider.GlobalProvider.GetService(typeof(DTE)) as DTE2)
//
//        internal CrossSolutionTaskListCommentManager taskListCommentManager
//
//        protected override void Initialize()
//        {
//            base.Initialize()
//            VSUtils.ForegroundThreadGuard.BindThread()
//
//            IServiceContainer serviceContainer = this
//            serviceContainer.AddService(typeof(IGeneralOptions),
//                delegate { return GetDialogPage(typeof(GeneralOptionsPage)) }, promote: true)
//
//            serviceContainer.AddService(typeof(IFormattingOptions),
//                delegate { return GetDialogPage(typeof(FantomasOptionsPage)) }, promote: true)
//
//            serviceContainer.AddService(typeof(ICodeGenerationOptions),
//                delegate { return GetDialogPage(typeof(CodeGenerationOptionsPage)) }, promote: true)
//
//            serviceContainer.AddService(typeof(IGlobalOptions),
//                delegate { return GetDialogPage(typeof(GlobalOptionsPage)) }, promote: true)
//
//            serviceContainer.AddService(typeof(ILintOptions),
//                delegate { return GetDialogPage(typeof(Linting.LintOptionsPage)) }, promote: true)
//
//            serviceContainer.AddService(typeof(IOutliningOptions),
//                delegate { return GetDialogPage(typeof(OutliningOptionsPage)) }, promote: true)
//
//            var generalOptions = GetService(typeof(IGeneralOptions)) as IGeneralOptions
//            PerformRegistrations(generalOptions)
//
//            library = new FSharpLibrary(Constants.guidSymbolLibrary)
//            library.LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT
//
//            RegisterLibrary()
//        }
//
//        private void PerformRegistrations(IGeneralOptions generalOptions)
//        {
//            if (generalOptions.FolderOrganizationEnabled)
//            {
//                SetupFolderMenu()
//                RegisterPriorityCommandTarget()
//            }
//
//            if (generalOptions.GenerateReferencesEnabled)
//            {
//                SetupReferenceMenu()
//            }
//
//            if (generalOptions.TaskListCommentsEnabled)
//            {
//                try
//                {
//                    var componentModel = GetService(typeof(SComponentModel)) as IComponentModel
//                    taskListCommentManager = componentModel.DefaultExportProvider.GetExportedValue<CrossSolutionTaskListCommentManager>()
//                    Debug.Assert(taskListCommentManager != null, "This instance should have been MEF exported.")
//                    taskListCommentManager.Activate()
//                }
//                catch (Exception ex)
//                {
//                    LoggingModule.logException(ex)
//                }
//            }
//        }
//
//        private void SetupReferenceMenu()
//        {
//            var mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService
//            
//            if (mcs != null)
//            {
//                fsiReferenceMenu = new FsiReferenceCommand(DTE.Value, mcs)
//                fsiReferenceMenu.SetupCommands()
//            }
//        }
//
//        private void SetupFolderMenu()
//        {
//            var mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService
//            var shell = GetService(typeof(SVsUIShell)) as IVsUIShell
//
//            if (mcs != null)
//            {
//                newFolderMenu = new FolderMenuCommands(DTE.Value, mcs, shell)
//                newFolderMenu.SetupCommands()
//            }
//        }
//
//        private void RegisterPriorityCommandTarget()
//        {
//            var rpct = GetService(typeof(SVsRegisterPriorityCommandTarget)) as IVsRegisterPriorityCommandTarget
//            rpct.RegisterPriorityCommandTarget(0, newFolderMenu, out pctCookie)
//        }
//
//        private void UnregisterPriorityCommandTarget()
//        {
//            if (pctCookie != 0)
//            {
//                var rpct = GetService(typeof(SVsRegisterPriorityCommandTarget)) as IVsRegisterPriorityCommandTarget
//                if (rpct != null)
//                {
//                    rpct.UnregisterPriorityCommandTarget(pctCookie)
//                    pctCookie = 0
//                }
//            }
//        }    
//
//        private void RegisterLibrary()
//        {
//            if (objectManagerCookie == 0)
//            {
//                IVsObjectManager2 objManager = GetService(typeof(SVsObjectManager)) as IVsObjectManager2
//                if (objManager == null) return
//                ErrorHandler.ThrowOnFailure(objManager.RegisterSimpleLibrary(library, out objectManagerCookie))
//            }
//        }
//
//        private void UnregisterLibrary()
//        {
//            if (objectManagerCookie != 0)
//            {
//                IVsObjectManager2 objManager = GetService(typeof(SVsObjectManager)) as IVsObjectManager2
//                if (objManager != null)
//                    objManager.UnregisterLibrary(objectManagerCookie)
//            }
//        }
//
//        public void Dispose()
//        {
//            UnregisterPriorityCommandTarget()
//            UnregisterLibrary()
//            if (taskListCommentManager != null)
//                (taskListCommentManager as IDisposable).Dispose()
//            if (fsiReferenceMenu != null)
//                (fsiReferenceMenu as IDisposable).Dispose()
//        }
//    }
//}
