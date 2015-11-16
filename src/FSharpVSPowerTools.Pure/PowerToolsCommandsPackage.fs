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

//[< ProvideAutoLoad (VSConstants.UICONTEXT. NoSolution_string) >]
[< ProvideAutoLoad (VSConstants.UICONTEXT.SolutionExists_string) >]
//[< ProvideAutoLoad (VSConstants.UICONTEXT.FSharpProject_string) >]
[< ProvideBindingPath >]
[< Guid "1F699E38-7D87-44F4-BC08-6B1DD5A6F926" >]
[< PackageRegistration (UseManagedResourcesOnly = true) >]
[< ProvideMenuResource (resourceID= "Menus.ctmenu", version=1) >]
[< InstalledProductRegistration ("#110", "#112", AssemblyVersionInformation.Version, IconResourceID = 400) >]
[< ProvideOptionPage (typeof<GeneralOptionsPage>, Resource.vsPackageTitle, "Pure General", 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<FantomasOptionsPage>      , Resource.vsPackageTitle, "Formatting"     , 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<CodeGenerationOptionsPage>, Resource.vsPackageTitle, "Code Generation", 0s, 0s, true, 0) >]
[< ProvideOptionPage (typeof<GlobalOptionsPage>        , Resource.vsPackageTitle, "Pure Configuration"  , 0s, 0s, true, 0) >]
//[< ProvideOptionPage (typeof<OutliningOptionsPage>     , Resource.vsPackageTitle, "Outlining"      , 0s, 0s, true, 0) >]
[< ProvideOptionPage (typeof<Linting.LintOptionsPage>  , Resource.vsPackageTitle, "Pure Lint"           , 0s, 0s, true, 0) >]
[<ComVisible true>]
type PowerToolsCommandsPackage () as self =
    inherit Package ()

    // NOTE - THIS PART OF THE CONSTRUCTOR RUNS BEFORE `INITIALIZE` IS CALLED UNLIKE IN C# CLASSES
    //        IT CANNOT CONNECT TO ANY OF VISUAL STUDIO'S SERVICES

    let mutable pctCookie = 0u
    let mutable objectManagerCookie = 0u

    // Store items created by VFPT to be disposed later 
    let mutable fsharpLibrary : FSharpLibrary option = None
    let mutable fsiReferenceMenu : FsiReferenceCommand option = None


    override __.Initialize () =
        base.Initialize ()

        let dte = Package.GetGlobalService (typeof<DTE>) :?> DTE2 
    //    let mcs = Package.GetGlobalService (typeof<IMenuCommandService>) :?> OleMenuCommandService


        // Setup the menu commands
        //-------------------------
        //let mcs = Package.GetGlobalService (typeof<IMenuCommandService>) :?> OleMenuCommandService
  //      let refMenu  = new FsiReferenceCommand (dte, mcs)
  //      refMenu.SetupCommands ()
   //     fsiReferenceMenu <- Some refMenu

        // Setup the folder menu
        //-----------------------
        let shell = Package.GetGlobalService (typeof<SVsUIShell>) :?> IVsUIShell
     //   let folderMenu = new FolderMenuCommands (dte, mcs, shell)
   //     folderMenu.SetupCommands ()


        // Register the Command Targets
        //-----------------------------
        let rpct = Package.GetGlobalService (typeof<SVsRegisterPriorityCommandTarget>) :?> IVsRegisterPriorityCommandTarget
    //    rpct.RegisterPriorityCommandTarget (0u,folderMenu, &pctCookie) |> ignore

        // Create and Register the F# Library
        //-----------------------------------
        let library = FSharpLibrary Constants.guidSymbolLibrary
        library.LibraryCapabilities  <-  enum<_LIB_FLAGS2> (int _LIB_FLAGS.LF_PROJECT)
        let objManager = Package.GetGlobalService (typeof<SVsObjectManager>) :?> IVsObjectManager2
        if objectManagerCookie = 0u then
            ErrorHandler.ThrowOnFailure (objManager.RegisterSimpleLibrary (library, &objectManagerCookie)) |> ignore
        fsharpLibrary <- Some library


        VSUtils.ForegroundThreadGuard.BindThread ()





    member __.Unregister () =
        // unregisterLibrary
        //------------------
        if objectManagerCookie <> 0u then
            let objManager = Package.GetGlobalService (typeof<SVsObjectManager>) :?> IVsObjectManager2
            objManager.UnregisterLibrary (objectManagerCookie) |> ignore

        // unregisterPriorityCommandTarget
        //--------------------------------
        let rpct = Package.GetGlobalService (typeof<SVsRegisterPriorityCommandTarget>) :?> IVsRegisterPriorityCommandTarget
        rpct.UnregisterPriorityCommandTarget pctCookie |> ignore

    interface IDisposable with
        member x.Dispose () : unit = 
            self.Unregister()
            fsiReferenceMenu |> Option.iter dispose
