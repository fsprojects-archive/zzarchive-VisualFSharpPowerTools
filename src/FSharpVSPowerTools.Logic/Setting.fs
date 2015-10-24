namespace FSharpVSPowerTools

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
    abstract ToplevelEnabled: bool with get, set
    abstract MatchStatementEnabled: bool with get, set
    abstract ToplevelCollapsedByDefault: bool with get, set
    abstract MatchStatementCollapsedByDefault: bool with get, set

[<AutoOpen>]
module Utils =
    type System.IServiceProvider with
        member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
        member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T

[<RequireQualifiedAccess>]
module Setting =
    open System
     
    let getGeneralOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IGeneralOptions>()

    let getFormattingOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IFormattingOptions>()

    let getCodeGenerationOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<ICodeGenerationOptions>()

    let getDefaultMemberBody (codeGenOptions: ICodeGenerationOptions) =
        match codeGenOptions.CodeGenerationOptions with
        | CodeGenerationKinds.Failwith -> "failwith \"Not implemented yet\""
        | CodeGenerationKinds.NotImplementedYet -> "raise (System.NotImplementedException())"
        | CodeGenerationKinds.DefaultValue -> "Unchecked.defaultof<_>"
        | _ -> codeGenOptions.DefaultBody

    let getInterfaceMemberIdentifier (codeGenOptions: ICodeGenerationOptions) =
        IdentifierUtils.encapsulateIdentifier SymbolKind.Ident codeGenOptions.InterfaceMemberIdentifier

    let getGlobalOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IGlobalOptions>()
        
    let getLintOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<ILintOptions>()
