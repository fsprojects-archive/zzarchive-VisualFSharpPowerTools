namespace FSharpVSPowerTools

type IGeneralOptions =
    (* Editor enhancements *)
    /// XML comments generation
    abstract XmlDocEnabled: bool with get, set
    /// Code formatting
    abstract FormattingEnabled: bool with get, set
    /// Navigation Bar
    abstract NavBarEnabled: bool with get, set
    /// Highlight usage
    abstract HighlightUsageEnabled: bool with get, set
    /// Depth colorizer
    abstract DepthColorizerEnabled: bool with get, set
    /// Syntax coloring
    abstract SyntaxColoringEnabled: bool with get, set
    /// Gray out unused references
    abstract UnusedReferencesEnabled: bool with get, set
    /// Gray out unused opens
    abstract UnusedOpensEnabled: bool with get, set
    /// QuickInfo panel
    abstract QuickInfoPanelEnabled: bool with get, set
    /// FSharpLint integration
    abstract LinterEnabled: bool with get, set

    (* Refactorings *)
    /// Rename reference
    abstract RenameRefactoringEnabled: bool with get, set
    /// Resolve unopened namespaces
    abstract ResolveUnopenedNamespacesEnabled: bool with get, set
    /// Generate references for FSI
    abstract GenerateReferencesEnabled: bool with get, set
    /// Implement interface
    abstract InterfaceImplementationEnabled: bool with get, set
    /// Generate record stub
    abstract GenerateRecordStubEnabled: bool with get, set
    /// Generate union pattern match case
    abstract UnionPatternMatchCaseGenerationEnabled: bool with get, set

    (* Navigation *)
    /// NavigateTo
    abstract NavigateToEnabled: bool with get, set
    /// Navigate to source
    abstract GoToSymbolSourceEnabled: bool with get, set
    /// Go to metadata
    abstract GoToMetadataEnabled: bool with get, set
    /// Find all references
    abstract FindAllReferencesEnabled: bool with get, set

    (* Tools *)
    /// Task List comments
    abstract TaskListCommentsEnabled: bool with get, set
    /// Folder organization
    abstract FolderOrganizationEnabled: bool with get, set

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
    abstract StrictMode: bool with get, set
    abstract DiagnosticMode: bool with get, set

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

