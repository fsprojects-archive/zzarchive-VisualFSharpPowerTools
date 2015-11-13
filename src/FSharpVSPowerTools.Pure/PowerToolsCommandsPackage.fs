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

[<AutoOpen>]
module internal PackageHelpers =
    let addService<'a> (serviceContainer:IServiceContainer) page = serviceContainer.AddService<'a> ((fun _ _ -> page :> obj), true)

[< ProvideBindingPath >]
[< Guid "1F699E38-7D87-44F4-BC08-6B1DD5A6F926" >]
[< PackageRegistration (UseManagedResourcesOnly = true) >]
[< ProvideMenuResource (resourceID= "Menus.ctmenu", version=1) >]
[< InstalledProductRegistration ("#110", "#112", AssemblyVersionInformation.Version, IconResourceID = 400) >]
[< ProvideOptionPage (typeof<GeneralOptionsPage>, Resource.vsPackageTitle, "General"        , 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<FantomasOptionsPage>      , Resource.vsPackageTitle, "Formatting"     , 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<CodeGenerationOptionsPage>, Resource.vsPackageTitle, "Code Generation", 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<GlobalOptionsPage>        , Resource.vsPackageTitle, "Configuration"  , 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<OutliningOptionsPage>     , Resource.vsPackageTitle, "Outlining"      , 0s, 0s, true, 0) >]
[< ProvideOptionPage (typeof<Linting.LintOptionsPage>  , Resource.vsPackageTitle, "Lint"           , 0s, 0s, true, 0) >]
[< ProvideService (typeof<IGeneralOptions>) >]   
[< ProvideService (typeof<IFormattingOptions>) >]
[< ProvideService (typeof<ICodeGenerationOptions>) >]
[< ProvideService (typeof<IGlobalOptions>) >]
[< ProvideService (typeof<ILintOptions>) >]
[< ProvideAutoLoad (VSConstants.UICONTEXT.SolutionExists_string) >]
[< ProvideAutoLoad (VSConstants.UICONTEXT.FSharpProject_string) >]
[< ProvideAutoLoad (VSConstants.UICONTEXT.NoSolution_string) >]
type PowerToolsCommandsPackage () as self =
    inherit Package ()

    static let DTE = Lazy<DTE2> (fun () -> ServiceProvider.GlobalProvider.GetService<DTE2,DTE> ())

    let mutable pctCookie = 0u
    let mutable objectManagerCookie = 0u
    let mutable newFolderMenu : FolderMenuCommands option = None
    let mutable fsharpLibrary : FSharpLibrary option = None
    let mutable fsiReferenceMenu : FsiReferenceCommand option = None
    let mutable taskListCommentManager : CrossSolutionTaskListCommentManager option = None

    let serviceProvider     = self :> IServiceProvider
    let serviceContainer    = self :> IServiceContainer

    let setupReferenceMenu () =
        unitMaybe {
            let! mcs = serviceProvider.TryGetService<OleMenuCommandService,IMenuCommandService> ()
            (new FsiReferenceCommand (DTE.Value, mcs)).SetupCommands ()
        }

    let setupFolderMenu () =
        unitMaybe {
            let! mcs = serviceProvider.TryGetService<OleMenuCommandService,IMenuCommandService> ()
            let! shell = serviceProvider.TryGetService<IVsUIShell,SVsUIShell>()
            let folderMenu = new FolderMenuCommands (DTE.Value, mcs, shell)
            folderMenu.SetupCommands ()
            newFolderMenu <- Some folderMenu
        }

    let registerPriorityCommandTarget () =
        unitMaybe {
            let! rpct = serviceProvider.TryGetService<IVsRegisterPriorityCommandTarget,SVsRegisterPriorityCommandTarget>()
            let! folderMenu = newFolderMenu
            rpct.RegisterPriorityCommandTarget (0u,folderMenu, &pctCookie) |> ignore
        }

    let unregisterPriorityCommandTarget () =
        unitMaybe {
            if pctCookie <> 0u then 
                let! rpct = serviceProvider.TryGetService<IVsRegisterPriorityCommandTarget,SVsRegisterPriorityCommandTarget>()
                let! folderMenu = newFolderMenu
                rpct.RegisterPriorityCommandTarget (0u,folderMenu, &pctCookie) |> ignore
        }

    let registerLibrary () =
        unitMaybe {
            if objectManagerCookie = 0u then
                let! objManager = serviceProvider.TryGetService<IVsObjectManager2,SVsObjectManager> ()
                let! library = fsharpLibrary
                ErrorHandler.ThrowOnFailure (objManager.RegisterSimpleLibrary (library, &objectManagerCookie)) |> ignore
        }

    let unregisterLibrary () =
        unitMaybe {
            if objectManagerCookie <> 0u then
                let! objManager = serviceProvider.TryGetService<IVsObjectManager2,SVsObjectManager> ()
                objManager.UnregisterLibrary (objectManagerCookie) |> ignore
        }

    let performRegistrations (generalOptions:IGeneralOptions) =
        if generalOptions.FolderOrganizationEnabled then setupFolderMenu(); registerPriorityCommandTarget()
        if generalOptions.GenerateReferencesEnabled then setupReferenceMenu()
        if generalOptions.TaskListCommentsEnabled then
            try unitMaybe {
                let! componentModel = serviceProvider.TryGetService<IComponentModel,SComponentModel>()
                let commentManager = componentModel.DefaultExportProvider.GetExportedValue<CrossSolutionTaskListCommentManager>()
                // Debug.Assert(isNotNull taskListCommentManager, "This instance should have been MEF exported")
                commentManager.Activate ()
                taskListCommentManager <- Some commentManager
              }
            with ex -> Logging.logException ex


    member __.GetDialogPage<'a> () =  base.GetDialogPage (typeof<'a>)
    

    override __.Initialize () =
        base.Initialize ()

        VSUtils.ForegroundThreadGuard.BindThread ()
        
        self.GetDialogPage<GeneralOptionsPage> () |> addService<IGeneralOptions> serviceContainer
//        self.GetDialogPage<FantomasOptionsPage> () |> addService<IFormattingOptions> 
//        self.GetDialogPage<CodeGenerationOptionsPage> () |> addService<ICodeGenerationOptions> 
//        self.GetDialogPage<GlobalOptionsPage> () |> addService<IGlobalOptions>
        self.GetDialogPage<Linting.LintOptionsPage> () |> addService<ILintOptions> serviceContainer
        //self.GetDialogPage<OutliningOptionsPage> () |> addService<IOutliningOptions> serviceContainer 

        let generalOptions = self.GetService<IGeneralOptions> ()

        performRegistrations generalOptions

        let library = FSharpLibrary Constants.guidSymbolLibrary
        library.LibraryCapabilities <-  Enum.Parse( typedefof<_LIB_FLAGS2>, _LIB_FLAGS.LF_PROJECT.ToString()) :?> _LIB_FLAGS2
//        library.LibraryCapabilities <-   asEnum<_LIB_FLAGS2>  _LIB_FLAGS.LF_PROJECT
        fsharpLibrary <- Some library
        registerLibrary ()


    interface IDisposable with
        member x.Dispose () : unit = 
            unregisterPriorityCommandTarget ()
            unregisterLibrary ()
            taskListCommentManager |> Option.iter dispose
            fsiReferenceMenu |> Option.iter dispose
