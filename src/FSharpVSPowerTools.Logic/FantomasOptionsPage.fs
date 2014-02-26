namespace FSharpVSPowerTools

open System
open System.Collections.Generic
open System.ComponentModel
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.VisualStudio.Shell
open System.Runtime.InteropServices

[<Guid("30829e04-262b-4af7-89e3-d4bf7a1d0c23")>]
type FantomasOptionsPage() as this =
    do
        let config = Fantomas.FormatConfig.FormatConfig.Default

        this.PageWidth <- 120
        this.SemicolonAtEndOfLine <- config.SemicolonAtEndOfLine
        this.SpaceBeforeArgument <- config.SpaceBeforeArgument
        this.SpaceBeforeColon <- config.SpaceBeforeColon
        this.SpaceAfterComma <- config.SpaceAfterComma
        this.SpaceAfterSemicolon <- config.SpaceAfterSemicolon
        this.IndentOnTryWith <- config.IndentOnTryWith
        this.ReorderOpenDeclaration <- config.ReorderOpenDeclaration
        this.SpaceAroundDelimiter <- config.SpaceAroundDelimiter

    [<Category("Layout")>]
    [<DisplayName("Page Width")>]
    [<Description("Set maximum width.")>]
    member val PageWidth: int = 0 with get, set

    [<Category("Syntax")>]
    [<DisplayName("Semicolon at End of Line")>]
    [<Description("Insert ';' at EOL between list, array and record elements.")>]
    member val SemicolonAtEndOfLine: bool = false with get, set

    [<Category("Spacing")>]
    [<DisplayName("Space Before Argument")>]
    [<Description("Insert ' ' before arguments in function applications.")>]
    member val SpaceBeforeArgument: bool = false with get, set

    [<Category("Spacing")>]
    [<DisplayName("Space Before Colon")>]
    [<Description("Insert ' ' before ':' in type annotations.")>]
    member val SpaceBeforeColon: bool = false with get, set

    [<Category("Spacing")>]
    [<DisplayName("Space After Comma")>]
    [<Description("Insert ' ' after ',' in tuples.")>]
    member val SpaceAfterComma: bool = false with get, set

    [<Category("Spacing")>]
    [<DisplayName("Space After Semicolon")>]
    [<Description("Insert ' ' after ';' in list, array and record elements.")>]
    member val SpaceAfterSemicolon: bool = false with get, set

    [<Category("Spacing")>]
    [<DisplayName("Space Around Delimiter")>]
    [<Description("Insert ' ' after starting and before ending of lists, arrays, sequences and records.")>]
    member val SpaceAroundDelimiter: bool = false with get, set

    [<Category("Indentation")>]
    [<DisplayName("Indent on Try...With")>]
    [<Description("Indent one level in all clauses of: bool = false with blocks.")>]
    member val IndentOnTryWith: bool = false with get, set

    [<Category("Refactoring")>]
    [<DisplayName("Reorder open declarations")>]
    [<Description("Reorder and deduplicate open statements while doing formatting.")>]
    member val ReorderOpenDeclaration: bool = false with get, set
