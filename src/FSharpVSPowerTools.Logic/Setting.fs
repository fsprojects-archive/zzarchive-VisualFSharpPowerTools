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
    abstract UnusedDeclarationsEnabled: bool with get, set
    abstract TaskListCommentsEnabled: bool with get, set
    abstract GoToMetadataEnabled: bool with get, set

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

[<RequireQualifiedAccess>]
module Setting =
    open System
    open FSharpVSPowerTools.ProjectSystem

    let getGeneralOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IGeneralOptions>()

    let getFormattingOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<IFormattingOptions>()

