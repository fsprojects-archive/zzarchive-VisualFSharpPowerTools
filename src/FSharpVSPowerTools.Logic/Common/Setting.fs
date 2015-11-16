namespace FSharpVSPowerTools

type SettingsStore =


    static member Load () = ()



type IGeneralOptions =
    abstract XmlDocEnabled: bool with get, set
    abstract FormattingEnabled: bool with get, set
    abstract NavBarEnabled: bool with get, set
    abstract HighlightUsageEnabled: bool with get, set
    abstract RenameRefactoringEnabled: bool with get, set
    abstract DepthColorizerEnabled: bool with get, set
    abstract NavigateToEnabled: bool with get, set
    abstract SyntaxColoringEnabled: bool with get, set
    abstract InterfaceImplementationEnabled: bool with get, set
    abstract FolderOrganizationEnabled: bool with get, set
    abstract FindAllReferencesEnabled: bool with get, set
    abstract GenerateRecordStubEnabled: bool with get, set
    abstract UnionPatternMatchCaseGenerationEnabled: bool with get, set
    abstract ResolveUnopenedNamespacesEnabled: bool with get, set
    abstract UnusedReferencesEnabled: bool with get, set
    abstract UnusedOpensEnabled: bool with get, set
    abstract TaskListCommentsEnabled: bool with get, set
    abstract GoToMetadataEnabled: bool with get, set
    abstract GenerateReferencesEnabled: bool with get, set
    abstract GoToSymbolSourceEnabled: bool with get, set
    abstract QuickInfoPanelEnabled: bool with get, set
    abstract LinterEnabled: bool with get, set
    abstract OutliningEnabled: bool with get, set

type IFormattingOptions =
    abstract PageWidth: int with get, set
    abstract SemicolonAtEndOfLine: bool with get, set
    abstract SpaceBeforeArgument: bool with get, set
    abstract SpaceBeforeColon: bool with get, set
    abstract SpaceAfterComma: bool with get, set
    abstract SpaceAfterSemicolon: bool with get, set
    abstract SpaceAroundDelimiter: bool with get, set
    abstract IndentOnTryWith: bool with get, set
    abstract ReorderOpenDeclaration: bool with get, set

type CodeGenerationKinds =
    | Failwith = 0
    | NotImplementedYet = 1
    | DefaultValue = 2
    | Uncompilable = 3

type ICodeGenerationOptions =
    abstract DefaultBody: string with get, set
    abstract CodeGenerationOptions: CodeGenerationKinds with get, set
    abstract InterfaceMemberIdentifier: string with get, set
     
type IGlobalOptions =
    abstract DiagnosticMode: bool with get, set
    abstract BackgroundCompilation: bool with get, set
    abstract ProjectCacheSize: int with get, set

type ILintOptions =
    abstract UpdateDirectories: unit -> unit
    abstract GetConfigurationForDirectory: string -> FSharpLint.Framework.Configuration.Configuration

type IOutliningOptions =
    abstract OpensEnabled: bool with get, set
    abstract OpensCollapsedByDefault: bool with get, set
    abstract ModulesEnabled: bool with get, set
    abstract ModulesCollapsedByDefault: bool with get, set
    abstract HashDirectivesEnabled: bool with get, set
    abstract HashDirectivesCollapsedByDefault: bool with get, set
    abstract TypesEnabled: bool with get, set
    abstract TypesCollapsedByDefault: bool with get, set
    abstract SimpleTypesEnabled: bool with get, set
    abstract SimpleTypesCollapsedByDefault: bool with get, set
    abstract TypeExpressionsEnabled: bool with get, set
    abstract TypeExpressionsCollapsedByDefault: bool with get, set
    abstract MembersEnabled: bool with get, set
    abstract MembersCollapsedByDefault: bool with get, set
    abstract LetOrUseEnabled: bool with get, set
    abstract LetOrUseCollapsedByDefault: bool with get, set
    abstract CollectionsEnabled: bool with get, set
    abstract CollectionsCollapsedByDefault: bool with get, set
    abstract PatternMatchesEnabled: bool with get, set
    abstract PatternMatchesCollapsedByDefault: bool with get, set
    abstract TryWithFinallyEnabled: bool with get, set
    abstract TryWithFinallyCollapsedByDefault: bool with get, set
    abstract IfThenElseEnabled: bool with get, set
    abstract IfThenElseCollapsedByDefault: bool with get, set
    abstract CExpressionMembersEnabled: bool with get, set
    abstract CExpressionMembersCollapsedByDefault: bool with get, set
    abstract LoopsEnabled: bool with get, set
    abstract LoopsCollapsedByDefault: bool with get, set
    abstract AttributesEnabled: bool with get, set
    abstract AttributesCollapsedByDefault: bool with get, set
    abstract TooltipZoomLevel: int with get, set



[<AutoOpen>]
module Utils =
    type System.IServiceProvider with
        /// Use the service provider to get an MEF service via its Interface type
        member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
        ///  Use the service provider to get an MEF Visual Studio service
        ///  and cast it to its Interface type e.g.
        ///  `.GetService<IVsTextManager, SVsTextManager>()`
        member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T



        /// Try to use the service provider to get an MEF service via its Interface type
        member x.TryGetService<'T>() = 
            match x.GetService(typeof<'T>) with
            | null -> None | svc -> svc :?> 'T |> Some


        ///  Try to use the service provider to get an MEF Visual Studio service
        ///  and cast it to its Interface type e.g.
        ///  `.GetService<IVsTextManager, SVsTextManager>()`
        member x.TryGetService<'T, 'S>() = 
            match x.GetService(typeof<'S>) with
            | null -> None | svc -> svc :?> 'T |> Some
            
    type DefaultGeneralOptions() = 
        interface IGeneralOptions with
            member __.XmlDocEnabled                          with get()  = true  and set _ = ()
            member __.FormattingEnabled                      with get()  = true  and set _ = ()
            member __.NavBarEnabled                          with get()  = true  and set _ = ()
            member __.HighlightUsageEnabled                  with get()  = true  and set _ = ()
            member __.RenameRefactoringEnabled               with get()  = true  and set _ = ()
            member __.DepthColorizerEnabled                  with get()  = false and set _ = ()
            member __.NavigateToEnabled                      with get()  = true  and set _ = ()
            member __.SyntaxColoringEnabled                  with get()  = true  and set _ = ()
            member __.InterfaceImplementationEnabled         with get()  = true  and set _ = ()
            member __.FolderOrganizationEnabled              with get()  = true  and set _ = ()
            member __.FindAllReferencesEnabled               with get()  = true  and set _ = ()
            member __.GenerateRecordStubEnabled              with get()  = true  and set _ = ()
            member __.UnionPatternMatchCaseGenerationEnabled with get()  = true  and set _ = ()
            member __.ResolveUnopenedNamespacesEnabled       with get()  = true  and set _ = ()
            member __.UnusedReferencesEnabled                with get()  = true  and set _ = ()
            member __.UnusedOpensEnabled                     with get()  = true  and set _ = ()
            member __.TaskListCommentsEnabled                with get()  = true  and set _ = ()
            member __.GoToMetadataEnabled                    with get()  = true  and set _ = ()
            member __.GenerateReferencesEnabled              with get()  = true  and set _ = ()
            member __.GoToSymbolSourceEnabled                with get()  = true  and set _ = ()
            member __.QuickInfoPanelEnabled                  with get()  = true  and set _ = ()
            member __.LinterEnabled                          with get()  = false and set _ = ()
            member __.OutliningEnabled                       with get()  = true  and set _ = ()



type DefaultCodeGenerationOptions() =
    interface ICodeGenerationOptions with 
        member __.DefaultBody with get () = "Unchecked.defaultOf<_>" and set _ = ()
        member __.CodeGenerationOptions with get() = CodeGenerationKinds.DefaultValue and set _ = ()
        member __.InterfaceMemberIdentifier with get () = "__" and set _ = ()

type DefaultGlobalOptions() =
    interface IGlobalOptions with
        member __.DiagnosticMode with get() = true and  set _ = ()
        member __.BackgroundCompilation with get() = true and set _ = ()
        member __.ProjectCacheSize with get() = 50 and set _ = ()


         

[<RequireQualifiedAccess>]
module Setting =
    open System

    let generalOptions = DefaultGeneralOptions() 

    let codeGenOptions = DefaultCodeGenerationOptions() 
    
    let globalOptions = DefaultGlobalOptions ()
     
    let getGeneralOptions _ = generalOptions :> IGeneralOptions
    let getGlobalOptions _ = globalOptions :> IGlobalOptions
    let getCodeGenerationOptions _ = codeGenOptions :> ICodeGenerationOptions

    let getFormattingOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IFormattingOptions>()    
        


    //    serviceProvider.GetService<ICodeGenerationOptions>()
//        match serviceProvider.GetService(typeof<ICodeGenerationOptions>) with
//        | null -> DefaultCodeGenerationOptions() :> ICodeGenerationOptions
//        | opts -> opts :?> ICodeGenerationOptions



    let getDefaultMemberBody (codeGenOptions: ICodeGenerationOptions) =
        match codeGenOptions.CodeGenerationOptions with
        | CodeGenerationKinds.Failwith -> "failwith \"Not implemented yet\""
        | CodeGenerationKinds.NotImplementedYet -> "raise (System.NotImplementedException())"
        | CodeGenerationKinds.DefaultValue -> "Unchecked.defaultof<_>"
        | _ -> codeGenOptions.DefaultBody

    let getInterfaceMemberIdentifier (codeGenOptions: ICodeGenerationOptions) =
        IdentifierUtils.encapsulateIdentifier SymbolKind.Ident codeGenOptions.InterfaceMemberIdentifier


              

//
//        //serviceProvider.GetService<IGlobalOptions>()
//        match serviceProvider.GetService(typeof<IGlobalOptions>) with
//        | null ->  :> IGlobalOptions
//        | opts -> opts :?> IGlobalOptions


    let getLintOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<ILintOptions>()

    let getOutliningOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IOutliningOptions>()

