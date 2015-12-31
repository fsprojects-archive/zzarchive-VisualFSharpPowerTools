namespace FSharpVSPowerTools

open System
open System.Globalization
open System.Runtime.CompilerServices
open System.Xml
open System.Xml.Linq
open System.IO
open System.Text
open System.Collections.Generic
open System.ComponentModel.Composition
open EnvDTE
open Microsoft.VisualStudio.Settings
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Shell.Settings
open Microsoft.VisualStudio.Editor
open System.Runtime.CompilerServices
open FSharpVSPowerTools.Pervasive

[<AutoOpen>]
module Utils =
    type System.IServiceProvider with
        member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
        member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T

[<AutoOpen>]
module StoreUtils =

    let [<Literal>] VFPT_SETTINGS = "vfpt_settings.xml"

    let configurePath (filepath:string) =
        let filename, folder =
            if String.IsNullOrEmpty filepath then
                VFPT_SETTINGS, Path.Combine
                    (Environment.GetFolderPath (Environment.SpecialFolder.UserProfile),@".configs/.vfpt")
            else
                Path.GetFileName filepath, Path.GetDirectoryName filepath
        if not (Directory.Exists folder) then Directory.CreateDirectory folder |> ignore

        Path.Combine (folder, filename)

    let defaultSettingsPath = configurePath ""

    let writerSettings =
        XmlWriterSettings (Indent=true, NewLineOnAttributes=true, Async=true, WriteEndDocumentOnClose=true)


type ISettingsStore =
    abstract member Get : name:string -> string option
    abstract member Set : name:string -> value:string -> unit
    abstract member Load : unit -> unit
    abstract member Save : unit -> unit
    abstract member GetContents : unit -> string


[<Sealed>]
type XmlSettingsStore () as self =

    let settings = Dictionary<string,string>()
    do  (self :> ISettingsStore).Load()

    interface ISettingsStore with

        member __.Save ()  =
            if (not<<File.Exists) defaultSettingsPath then
                using (XmlWriter.Create(defaultSettingsPath, writerSettings)) (fun writer ->
                    writer.WriteStartElement "VFPT"
                    settings |> Seq.iter (fun kvp -> writer.WriteElementString (kvp.Key, kvp.Value))
                    writer.WriteEndElement ())
            else
                let xdoc = XDocument.Load defaultSettingsPath
                let vfptNode =  xdoc.GetOrCreateElement "VFPT"
                settings |> Seq.iter (fun kvp -> vfptNode.SetOrCreateElement kvp.Key kvp.Value)
                xdoc.Save defaultSettingsPath


        member __.Load () =
            try if File.Exists defaultSettingsPath then
                    let doc = XDocument.Load defaultSettingsPath
                    (doc.Element "VFPT").Descendants()
                    |> Seq.iter (fun elem -> Dict.addOrUpdate elem.Name.LocalName elem.Value settings)
            with exn -> debug "%s" exn.Message
                        debug "%s" exn.InnerException.Message


        member __.Get name       = Dict.tryFind name settings
        member __.Set name value = Dict.addOrUpdate name value settings |> ignore

        member __.GetContents () =
            let sb = StringBuilder()
            settings |> Seq.iter (fun x -> sb.AppendLine(sprintf "%s | %s" x.Key x.Value) |> ignore)
            string sb


[<AutoOpen>]
module SettingsConverters =

    let settingsStore = XmlSettingsStore () :> ISettingsStore

    let settingsChanged = Event<EventArgs>()
    let settingsEvents  = settingsChanged.Publish

    // when settings have changed repopulate the settings dictionary based on the new values
    settingsEvents.Add (fun _ -> settingsStore.Load ())


    let getBool (name:string) (defaultValue:bool) : bool =
        match settingsStore.Get name with
        | None | Some "" -> defaultValue
        | Some value -> Convert.ToBoolean value


    let getInt (name:string) (defaultValue:int) : int =
        match settingsStore.Get name with
        | None | Some "" -> defaultValue
        | Some value -> Convert.ToInt32 value


    let getString (name:string) (defaultValue:string) : string =
        match settingsStore.Get name with
        | None | Some "" -> defaultValue
        | Some value     -> value


    let setValue  (name:string) (value:obj) : unit =
        if isNull value then settingsStore.Set name "" else
        settingsStore.Set name (Convert.ToString (value, CultureInfo.InvariantCulture))


[<AutoOpen>]
module SettingsStrings =

    // GENERAL OPTIONS
    //----------------------
    let XML_DOC_ENABLED                             = "XmlDocEnabled"
    let FORMATTING_ENABLED                          = "FormattingEnabled"
    let NAVBAR_ENABLED                              = "NavBarEnabled"
    let HIGHLIGHT_USAGE_ENABLED                     = "HighlightUsageEnabled"
    let HIGHLIGHT_PRINTF_USAGE_ENABLED              = "HighlightPrintfUsageEnabled"
    let RENAME_REFACTORING_ENABLED                  = "RenameRefactoringEnabled"
    let DEPTH_COLORIZER_ENABLED                     = "DepthColorizerEnabled"
    let NAVIGATE_TO_ENABLED                         = "NavigateToEnabled"
    let SYNTAX_COLORING_ENABLED                     = "SyntaxColoringEnabled"
    let INTERFACE_IMPLEMENTATION_ENABLED            = "InterfaceImplementationEnabled"
    let FOLDER_ORGANIZATION_ENABLED                 = "FolderOrganizationEnabled"
    let FIND_ALL_REFERENCES_ENABLED                 = "FindAllReferencesEnabled"
    let GENERATE_RECORD_STUB_ENABLED                = "GenerateRecordStubEnabled"
    let UNION_PATTERN_MATCH_CASE_GENERATION_ENABLED = "UnionPatternMatchCaseGenerationEnabled"
    let RESOLVE_UNOPENED_NAMESPACES_ENABLED         = "ResolveUnopenedNamespacesEnabled"
    let UNUSED_REFERENCES_ENABLED                   = "UnusedReferencesEnabled"
    let UNUSED_OPENS_ENABLED                        = "UnusedOpensEnabled"
    let TASK_LIST_COMMENTS_ENABLED                  = "TaskListCommentsEnabled"
    let GO_TO_METADATA_ENABLED                      = "GoToMetadataEnabled"
    let GENERATE_REFERENCES_ENABLED                 = "GenerateReferencesEnabled"
    let GO_TO_SYMBOL_SOURCE_ENABLED                 = "GoToSymbolSourceEnabled"
    let QUICK_INFO_PANEL_ENABLED                    = "QuickInfoPanelEnabled"
    let LINTER_ENABLED                              = "LinterEnabled"
    let OUTLINING_ENABLED                           = "OutliningEnabled"
    let PEEK_DEFINITION_ENABLED                     = "PeekDefinitionEnabled"


    // FORMATTING OPTIONS
    //----------------------
    let PAGE_WIDTH                        =    "PageWidth"
    let SEMICOLON_AT_END_OF_LINE          =    "SemicolonAtEndOfLine"
    let SPACE_BEFORE_ARGUMENT             =    "SpaceBeforeArgument"
    let SPACE_BEFORE_COLON                =    "SpaceBeforeColon"
    let SPACE_AFTER_COMMA                 =    "SpaceAfterComma"
    let SPACE_AFTER_SEMICOLON             =    "SpaceAfterSemicolon"
    let SPACE_AROUND_DELIMITER            =    "SpaceAroundDelimiter"
    let INDENT_ON_TRYWITH                 =    "IndentOnTryWith"
    let REORDER_OPEN_DECLARATION          =    "ReorderOpenDeclaration"

    // GLOBAL OPTIONS
    //----------------------
    let DIAGNOSTIC_MODE         = "DiagnosticMode"
    let BACKGROUND_COMPILATION  = "BackgroundCompilation"
    let PROJECT_CACHE_SIZE      = "ProjectCacheSize"


    // CODE GENERATION OPTIONS
    //------------------------
    let DEFAULT_BODY                  = "DefaultBody"
    let CODE_GENERATION_OPTIONS       = "CodeGenerationOptions"
    let INTERFAC_EMEMBER_IDENTIFIER   = "InterfaceMemberIdentifier"


    // OUTLINING OPTIONS
    //------------------------
    let OPENS_ENABLED                             = "OpensEnabled"
    let OPENS_COLLAPSED_BY_DEFAULT                = "OpensCollapsedByDefault"
    let MODULES_ENABLED                           = "ModulesEnabled"
    let MODULES_COLLAPSED_BY_DEFAULT              = "ModulesCollapsedByDefault"
    let HASH_DIRECTIVES_ENABLED                   = "HashDirectivesEnabled"
    let HASH_DIRECTIVES_COLLAPSED_BY_DEFAULT      = "HashDirectivesCollapsedByDefault"
    let TYPES_ENABLED                             = "TypesEnabled"
    let TYPES_COLLAPSED_BY_DEFAULT                = "TypesCollapsedByDefault"
    let SIMPLE_TYPES_ENABLED                      = "SimpleTypesEnabled"
    let SIMPLE_TYPES_COLLAPSED_BY_DEFAULT         = "SimpleTypesCollapsedByDefault"
    let TYPE_EXPRESSIONS_ENABLED                  = "TypeExpressionsEnabled"
    let TYPE_EXPRESSIONS_COLLAPSED_BY_DEFAULT     = "TypeExpressionsCollapsedByDefault"
    let MEMBERS_ENABLED                           = "MembersEnabled"
    let MEMBERS_COLLAPSED_BY_DEFAULT              = "MembersCollapsedByDefault"
    let LET_OR_USE_ENABLED                        = "LetOrUseEnabled"
    let LET_OR_USE_COLLAPSED_BY_DEFAULT           = "LetOrUseCollapsedByDefault"
    let COLLECTIONS_ENABLED                       = "CollectionsEnabled"
    let COLLECTIONS_COLLAPSED_BY_DEFAULT          = "CollectionsCollapsedByDefault"
    let PATTERN_MATCHES_ENABLED                   = "PatternMatchesEnabled"
    let PATTERN_MATCHES_COLLAPSED_BY_DEFAULT      = "PatternMatchesCollapsedByDefault"
    let TRY_WITH_FINALLY_ENABLED                  = "TryWithFinallyEnabled"
    let TRY_WITH_FINALLY_COLLAPSED_BY_DEFAULT     = "TryWithFinallyCollapsedByDefault"
    let IF_THEN_ELSE_ENABLED                      = "IfThenElseEnabled"
    let IF_THEN_ELSE_COLLAPSED_BY_DEFAULT         = "IfThenElseCollapsedByDefault"
    let CEXPRESSION_MEMBERS_ENABLED               = "CExpressionMembersEnabled"
    let CEXPRESSION_MEMBERS_COLLAPSED_BY_DEFAULT  = "CExpressionMembersCollapsedByDefault"
    let LOOPS_ENABLED                             = "LoopsEnabled"
    let LOOPS_COLLAPSED_BY_DEFAULT                = "LoopsCollapsedByDefault"
    let ATTRIBUTES_ENABLED                        = "AttributesEnabled"
    let ATTRIBUTES_COLLAPSED_BY_DEFAULT           = "AttributesCollapsedByDefault"
    let XML_DOC_COMMENTS_ENABLED                  = "XmlDocCommentsEnabled"
    let XML_DOC_COMMENTS_COLLAPSED_BY_DEFAULT     = "XmlDocCommentsCollapsedByDefault"
    let COMMENTS_ENABLED                          = "CommentsEnabled"
    let COMMENTS_COLLAPSED_BY_DEFAULT             = "CommentsCollapsedByDefault"
    let TOOLTIP_ZOOM_LEVEL                        = "TooltipZoomLevel"



type IGeneralOptions =
    abstract XmlDocEnabled                          : bool with get, set
    abstract FormattingEnabled                      : bool with get, set
    abstract NavBarEnabled                          : bool with get, set
    abstract HighlightUsageEnabled                  : bool with get, set
    abstract HighlightPrintfUsageEnabled            : bool with get, set
    abstract RenameRefactoringEnabled               : bool with get, set
    abstract DepthColorizerEnabled                  : bool with get, set
    abstract NavigateToEnabled                      : bool with get, set
    abstract SyntaxColoringEnabled                  : bool with get, set
    abstract InterfaceImplementationEnabled         : bool with get, set
    abstract FolderOrganizationEnabled              : bool with get, set
    abstract FindAllReferencesEnabled               : bool with get, set
    abstract GenerateRecordStubEnabled              : bool with get, set
    abstract UnionPatternMatchCaseGenerationEnabled : bool with get, set
    abstract ResolveUnopenedNamespacesEnabled       : bool with get, set
    abstract UnusedReferencesEnabled                : bool with get, set
    abstract UnusedOpensEnabled                     : bool with get, set
    abstract TaskListCommentsEnabled                : bool with get, set
    abstract GoToMetadataEnabled                    : bool with get, set
    abstract GenerateReferencesEnabled              : bool with get, set
    abstract GoToSymbolSourceEnabled                : bool with get, set
    abstract QuickInfoPanelEnabled                  : bool with get, set
    abstract LinterEnabled                          : bool with get, set
    abstract OutliningEnabled                       : bool with get, set
    abstract PeekDefinitionEnabled                  : bool with get, set
    abstract Load : unit -> unit
    abstract Save : unit -> unit
    abstract member GetContents : unit -> string


[<Sealed>]
type GeneralOptions () as self =
    do
        settingsStore.Load()
        settingsEvents.Add (fun _ -> (self :> IGeneralOptions).Load ())

    interface IGeneralOptions with
        member __.XmlDocEnabled
            with get () = getBool  XML_DOC_ENABLED true
            and  set v  = setValue XML_DOC_ENABLED v

        member __.FormattingEnabled
            with get () = getBool  FORMATTING_ENABLED true
            and  set v  = setValue FORMATTING_ENABLED v

        member __.NavBarEnabled
            with get () = getBool  NAVBAR_ENABLED false
            and  set v  = setValue NAVBAR_ENABLED v

        member __.HighlightUsageEnabled
            with get () = getBool  HIGHLIGHT_USAGE_ENABLED true
            and  set v  = setValue HIGHLIGHT_USAGE_ENABLED v

        member __.HighlightPrintfUsageEnabled
            with get () = getBool  HIGHLIGHT_PRINTF_USAGE_ENABLED true
            and  set v  = setValue HIGHLIGHT_PRINTF_USAGE_ENABLED v

        member __.RenameRefactoringEnabled
            with get () = getBool  RENAME_REFACTORING_ENABLED true
            and  set v  = setValue RENAME_REFACTORING_ENABLED v

        member __.DepthColorizerEnabled
            with get () = getBool  DEPTH_COLORIZER_ENABLED true
            and  set v  = setValue DEPTH_COLORIZER_ENABLED v

        member __.NavigateToEnabled
            with get () = getBool  NAVIGATE_TO_ENABLED true
            and  set v  = setValue NAVIGATE_TO_ENABLED v

        member __.SyntaxColoringEnabled
            with get () = getBool  SYNTAX_COLORING_ENABLED true
            and  set v  = setValue SYNTAX_COLORING_ENABLED v

        member __.InterfaceImplementationEnabled
            with get () = getBool  INTERFACE_IMPLEMENTATION_ENABLED true
            and  set v  = setValue INTERFACE_IMPLEMENTATION_ENABLED v

        member __.FolderOrganizationEnabled
            with get () = getBool  FOLDER_ORGANIZATION_ENABLED true
            and  set v  = setValue FOLDER_ORGANIZATION_ENABLED v

        member __.FindAllReferencesEnabled
            with get () = getBool  FIND_ALL_REFERENCES_ENABLED true
            and  set v  = setValue FIND_ALL_REFERENCES_ENABLED v

        member __.GenerateRecordStubEnabled
            with get () = getBool  GENERATE_RECORD_STUB_ENABLED true
            and  set v  = setValue GENERATE_RECORD_STUB_ENABLED v

        member __.UnionPatternMatchCaseGenerationEnabled
            with get () = getBool  UNION_PATTERN_MATCH_CASE_GENERATION_ENABLED true
            and  set v  = setValue UNION_PATTERN_MATCH_CASE_GENERATION_ENABLED v

        member __.ResolveUnopenedNamespacesEnabled
            with get () = getBool  RESOLVE_UNOPENED_NAMESPACES_ENABLED true
            and  set v  = setValue RESOLVE_UNOPENED_NAMESPACES_ENABLED v

        member __.UnusedReferencesEnabled
            with get () = getBool  UNUSED_REFERENCES_ENABLED true
            and  set v  = setValue UNUSED_REFERENCES_ENABLED v

        member __.UnusedOpensEnabled
            with get () = getBool  UNUSED_OPENS_ENABLED true
            and  set v  = setValue UNUSED_OPENS_ENABLED v

        member __.TaskListCommentsEnabled
            with get () = getBool  TASK_LIST_COMMENTS_ENABLED true
            and  set v  = setValue TASK_LIST_COMMENTS_ENABLED v

        member __.GoToMetadataEnabled
            with get () = getBool  GO_TO_METADATA_ENABLED true
            and  set v  = setValue GO_TO_METADATA_ENABLED v

        member __.GenerateReferencesEnabled
            with get () = getBool  GENERATE_REFERENCES_ENABLED true
            and  set v  = setValue GENERATE_REFERENCES_ENABLED v

        member __.GoToSymbolSourceEnabled
            with get () = getBool  GO_TO_SYMBOL_SOURCE_ENABLED true
            and  set v  = setValue GO_TO_SYMBOL_SOURCE_ENABLED v

        member __.QuickInfoPanelEnabled
            with get () = getBool  QUICK_INFO_PANEL_ENABLED true
            and  set v  = setValue QUICK_INFO_PANEL_ENABLED v

        member __.LinterEnabled
            with get () = getBool  LINTER_ENABLED true
            and  set v  = setValue LINTER_ENABLED v

        member __.OutliningEnabled
            with get () = getBool  OUTLINING_ENABLED true
            and  set v  = setValue OUTLINING_ENABLED v

        member __.PeekDefinitionEnabled
            with get () = getBool  PEEK_DEFINITION_ENABLED true
            and  set v  = setValue PEEK_DEFINITION_ENABLED v

        member __.GetContents() = settingsStore.GetContents()

        member __.Load ()   = settingsStore.Load ()
        member self.Save () =
            settingsStore.Save ()
            settingsChanged.Trigger EventArgs.Empty


type IFormattingOptions =
    abstract PageWidth             : int  with get, set
    abstract SemicolonAtEndOfLine  : bool with get, set
    abstract SpaceBeforeArgument   : bool with get, set
    abstract SpaceBeforeColon      : bool with get, set
    abstract SpaceAfterComma       : bool with get, set
    abstract SpaceAfterSemicolon   : bool with get, set
    abstract SpaceAroundDelimiter  : bool with get, set
    abstract IndentOnTryWith       : bool with get, set
    abstract ReorderOpenDeclaration: bool with get, set
    abstract Load : unit -> unit
    abstract Save : unit -> unit

[<Sealed>]
type FormattingOptions () as self  =
    do
        settingsStore.Load()
        settingsEvents.Add (fun _ -> (self :> IFormattingOptions).Load ())

    interface IFormattingOptions with
        member __.PageWidth
            with get () = getInt PAGE_WIDTH 120
            and  set v  = setValue PAGE_WIDTH v

        member __.SemicolonAtEndOfLine
            with get () = getBool  SEMICOLON_AT_END_OF_LINE true
            and  set v  = setValue SEMICOLON_AT_END_OF_LINE v

        member __.SpaceBeforeArgument
            with get () = getBool  SPACE_BEFORE_ARGUMENT true
            and  set v  = setValue SPACE_BEFORE_ARGUMENT v

        member __.SpaceBeforeColon
            with get () = getBool  SPACE_BEFORE_COLON true
            and  set v  = setValue SPACE_BEFORE_COLON v

        member __.SpaceAfterComma
            with get () = getBool  SPACE_AFTER_COMMA true
            and  set v  = setValue SPACE_AFTER_COMMA v

        member __.SpaceAfterSemicolon
            with get () = getBool  SPACE_AFTER_SEMICOLON true
            and  set v  = setValue SPACE_AFTER_SEMICOLON v

        member __.SpaceAroundDelimiter
            with get () = getBool  SPACE_AROUND_DELIMITER true
            and  set v  = setValue SPACE_AROUND_DELIMITER v

        member __.IndentOnTryWith
            with get () = getBool  INDENT_ON_TRYWITH true
            and  set v  = setValue INDENT_ON_TRYWITH v

        member __.ReorderOpenDeclaration
            with get () = getBool  REORDER_OPEN_DECLARATION true
            and  set v  = setValue REORDER_OPEN_DECLARATION v

        member __.Load () = settingsStore.Load ()

        member self.Save () =
            settingsStore.Save ()
            settingsChanged.Trigger EventArgs.Empty



type CodeGenerationKinds =
    | Failwith = 0
    | NotImplementedYet = 1
    | DefaultValue = 2
    | Uncompilable = 3


type ICodeGenerationOptions =
    abstract DefaultBody                : string with get, set
    abstract CodeGenerationOptions      : CodeGenerationKinds with get, set
    abstract InterfaceMemberIdentifier  : string with get, set
    abstract Load : unit -> unit
    abstract Save : unit -> unit

[<Sealed>]
type CodeGenerationOptions () as self =
    do
        settingsStore.Load()
        settingsEvents.Add (fun _ -> (self :> ICodeGenerationOptions).Load ())

    interface ICodeGenerationOptions with

        member __.DefaultBody
            with get () = getString DEFAULT_BODY ""
            and  set v  = setValue  DEFAULT_BODY v

        member __.InterfaceMemberIdentifier
            with get () = getString DEFAULT_BODY "x"
            and  set v  = setValue  DEFAULT_BODY v

        member val CodeGenerationOptions =
            CodeGenerationKinds.Failwith with get, set

        member __.Load () = settingsStore.Load ()
        member self.Save () =
            settingsStore.Save ()
            settingsChanged.Trigger EventArgs.Empty


type IGlobalOptions =
    abstract DiagnosticMode             : bool with get, set
    abstract BackgroundCompilation      : bool with get, set
    abstract ProjectCacheSize           : int  with get, set
    abstract Load : unit -> unit
    abstract Save : unit -> unit

[<Sealed>]
type GlobalOptions  () as self =
    do
        settingsStore.Load()
        settingsEvents.Add (fun _ -> (self :> IGlobalOptions).Load ())

    interface IGlobalOptions with
        member __.DiagnosticMode
            with get () = getBool  DIAGNOSTIC_MODE false
            and  set v  = setValue DIAGNOSTIC_MODE v

        member __.BackgroundCompilation
            with get () = getBool  BACKGROUND_COMPILATION true
            and  set v  = setValue BACKGROUND_COMPILATION v

        member __.ProjectCacheSize
            with get () = getInt   PROJECT_CACHE_SIZE 50
            and  set v  = setValue PROJECT_CACHE_SIZE v

        member __.Load () = settingsStore.Load ()
        member self.Save () =
            settingsStore.Save ()
            settingsChanged.Trigger EventArgs.Empty

type ILintOptions =
    abstract UpdateDirectories: unit -> unit
    abstract GetConfigurationForDirectory: string -> FSharpLint.Framework.Configuration.Configuration
    abstract Load : unit -> unit
    abstract Save : unit -> unit


type IOutliningOptions =
    abstract OpensEnabled                           :   bool with get, set
    abstract OpensCollapsedByDefault                :   bool with get, set
    abstract ModulesEnabled                         :   bool with get, set
    abstract ModulesCollapsedByDefault              :   bool with get, set
    abstract HashDirectivesEnabled                  :   bool with get, set
    abstract HashDirectivesCollapsedByDefault       :   bool with get, set
    abstract TypesEnabled                           :   bool with get, set
    abstract TypesCollapsedByDefault                :   bool with get, set
    abstract SimpleTypesEnabled                     :   bool with get, set
    abstract SimpleTypesCollapsedByDefault          :   bool with get, set
    abstract TypeExpressionsEnabled                 :   bool with get, set
    abstract TypeExpressionsCollapsedByDefault      :   bool with get, set
    abstract MembersEnabled                         :   bool with get, set
    abstract MembersCollapsedByDefault              :   bool with get, set
    abstract LetOrUseEnabled                        :   bool with get, set
    abstract LetOrUseCollapsedByDefault             :   bool with get, set
    abstract CollectionsEnabled                     :   bool with get, set
    abstract CollectionsCollapsedByDefault          :   bool with get, set
    abstract PatternMatchesEnabled                  :   bool with get, set
    abstract PatternMatchesCollapsedByDefault       :   bool with get, set
    abstract TryWithFinallyEnabled                  :   bool with get, set
    abstract TryWithFinallyCollapsedByDefault       :   bool with get, set
    abstract IfThenElseEnabled                      :   bool with get, set
    abstract IfThenElseCollapsedByDefault           :   bool with get, set
    abstract CExpressionMembersEnabled              :   bool with get, set
    abstract CExpressionMembersCollapsedByDefault   :   bool with get, set
    abstract LoopsEnabled                           :   bool with get, set
    abstract LoopsCollapsedByDefault                :   bool with get, set
    abstract AttributesEnabled                      :   bool with get, set
    abstract AttributesCollapsedByDefault           :   bool with get, set
    abstract XmlDocCommentsEnabled                  :   bool with get, set
    abstract XmlDocCommentsCollapsedByDefault       :   bool with get, set
    abstract CommentsEnabled                        :   bool with get, set
    abstract CommentsCollapsedByDefault             :   bool with get, set
    abstract TooltipZoomLevel                       :   int  with get, set
    abstract Load : unit -> unit
    abstract Save : unit -> unit


[<Sealed>]
type OutliningOptions () as self =
    do
        settingsStore.Load()
        settingsEvents.Add (fun _ -> (self :> IOutliningOptions).Load ())

    interface IOutliningOptions with
        member __.OpensEnabled
            with get () = getBool  OPENS_ENABLED true
            and  set v  = setValue OPENS_ENABLED v

        member __.OpensCollapsedByDefault
            with get () = getBool  OPENS_COLLAPSED_BY_DEFAULT true
            and  set v  = setValue OPENS_COLLAPSED_BY_DEFAULT v

        member __.ModulesEnabled
            with get () = getBool  MODULES_ENABLED true
            and  set v  = setValue MODULES_ENABLED v

        member __.ModulesCollapsedByDefault
            with get () = getBool  MODULES_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue MODULES_COLLAPSED_BY_DEFAULT v

        member __.HashDirectivesEnabled
            with get () = getBool  HASH_DIRECTIVES_ENABLED true
            and  set v  = setValue HASH_DIRECTIVES_ENABLED v

        member __.HashDirectivesCollapsedByDefault
            with get () = getBool  HASH_DIRECTIVES_COLLAPSED_BY_DEFAULT true
            and  set v  = setValue HASH_DIRECTIVES_COLLAPSED_BY_DEFAULT v

        member __.TypesEnabled
            with get () = getBool  TYPES_ENABLED true
            and  set v  = setValue TYPES_ENABLED v

        member __.TypesCollapsedByDefault
            with get () = getBool  TYPES_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue TYPES_COLLAPSED_BY_DEFAULT v

        member __.SimpleTypesEnabled
            with get () = getBool  SIMPLE_TYPES_ENABLED true
            and  set v  = setValue SIMPLE_TYPES_ENABLED v

        member __.SimpleTypesCollapsedByDefault
            with get () = getBool  SIMPLE_TYPES_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue SIMPLE_TYPES_COLLAPSED_BY_DEFAULT v

        member __.TypeExpressionsEnabled
            with get () = getBool  TYPE_EXPRESSIONS_ENABLED true
            and  set v  = setValue TYPE_EXPRESSIONS_ENABLED v

        member __.TypeExpressionsCollapsedByDefault
            with get () = getBool  TYPE_EXPRESSIONS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue TYPE_EXPRESSIONS_COLLAPSED_BY_DEFAULT v

        member __.MembersEnabled
            with get () = getBool  MEMBERS_ENABLED true
            and  set v  = setValue MEMBERS_ENABLED v

        member __.MembersCollapsedByDefault
            with get () = getBool  MEMBERS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue MEMBERS_COLLAPSED_BY_DEFAULT v

        member __.LetOrUseEnabled
            with get () = getBool  LET_OR_USE_ENABLED true
            and  set v  = setValue LET_OR_USE_ENABLED v

        member __.LetOrUseCollapsedByDefault
            with get () = getBool  LET_OR_USE_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue LET_OR_USE_COLLAPSED_BY_DEFAULT v

        member __.CollectionsEnabled
            with get () = getBool  COLLECTIONS_ENABLED true
            and  set v  = setValue COLLECTIONS_ENABLED v

        member __.CollectionsCollapsedByDefault
            with get () = getBool  COLLECTIONS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue COLLECTIONS_COLLAPSED_BY_DEFAULT v

        member __.PatternMatchesEnabled
            with get () = getBool  PATTERN_MATCHES_ENABLED true
            and  set v  = setValue PATTERN_MATCHES_ENABLED v

        member __.PatternMatchesCollapsedByDefault
            with get () = getBool  PATTERN_MATCHES_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue PATTERN_MATCHES_COLLAPSED_BY_DEFAULT v

        member __.TryWithFinallyEnabled
            with get () = getBool  TRY_WITH_FINALLY_ENABLED true
            and  set v  = setValue TRY_WITH_FINALLY_ENABLED v

        member __.TryWithFinallyCollapsedByDefault
            with get () = getBool  TRY_WITH_FINALLY_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue TRY_WITH_FINALLY_COLLAPSED_BY_DEFAULT v

        member __.IfThenElseEnabled
            with get () = getBool  IF_THEN_ELSE_ENABLED true
            and  set v  = setValue IF_THEN_ELSE_ENABLED v

        member __.IfThenElseCollapsedByDefault
            with get () = getBool  IF_THEN_ELSE_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue IF_THEN_ELSE_COLLAPSED_BY_DEFAULT v

        member __.CExpressionMembersEnabled
            with get () = getBool  CEXPRESSION_MEMBERS_ENABLED true
            and  set v  = setValue CEXPRESSION_MEMBERS_ENABLED v

        member __.CExpressionMembersCollapsedByDefault
            with get () = getBool  CEXPRESSION_MEMBERS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue CEXPRESSION_MEMBERS_COLLAPSED_BY_DEFAULT v

        member __.LoopsEnabled
            with get () = getBool  LOOPS_ENABLED true
            and  set v  = setValue LOOPS_ENABLED v

        member __.LoopsCollapsedByDefault
            with get () = getBool  LOOPS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue LOOPS_COLLAPSED_BY_DEFAULT v

        member __.AttributesEnabled
            with get () = getBool  ATTRIBUTES_ENABLED true
            and  set v  = setValue ATTRIBUTES_ENABLED v

        member __.AttributesCollapsedByDefault
            with get () = getBool  ATTRIBUTES_COLLAPSED_BY_DEFAULT true
            and  set v  = setValue ATTRIBUTES_COLLAPSED_BY_DEFAULT v

        member __.XmlDocCommentsEnabled
            with get () = getBool  XML_DOC_COMMENTS_ENABLED true
            and  set v  = setValue XML_DOC_COMMENTS_ENABLED v

        member __.XmlDocCommentsCollapsedByDefault
            with get () = getBool  XML_DOC_COMMENTS_COLLAPSED_BY_DEFAULT false
            and  set v  = setValue XML_DOC_COMMENTS_COLLAPSED_BY_DEFAULT v

        member __.CommentsEnabled
            with get () = getBool  COMMENTS_ENABLED true
            and  set v  = setValue COMMENTS_ENABLED v

        member __.CommentsCollapsedByDefault
            with get () = getBool  COMMENTS_COLLAPSED_BY_DEFAULT true
            and  set v  = setValue COMMENTS_COLLAPSED_BY_DEFAULT v

        member __.TooltipZoomLevel
            with get () = getInt   TOOLTIP_ZOOM_LEVEL 85
            and  set v  = setValue TOOLTIP_ZOOM_LEVEL v

        member __.Load () = settingsStore.Load ()
        member self.Save () =
            settingsStore.Save ()
            settingsChanged.Trigger EventArgs.Empty


module SettingsContext =

    let triggerSettingsChanged (e) = settingsChanged.Trigger(e)

    let GeneralOptions = GeneralOptions() :> IGeneralOptions
    let GlobalOptions  = GlobalOptions()  :> IGlobalOptions
    let FormattingOptions = FormattingOptions () :> IFormattingOptions
    let OutliningOptions = OutliningOptions() :> IOutliningOptions
    let CodeGenerationOptions = CodeGenerationOptions() :> ICodeGenerationOptions

    let getDefaultMemberBody () =
        match CodeGenerationOptions.CodeGenerationOptions with
        | CodeGenerationKinds.Failwith -> "failwith \"Not implemented yet\""
        | CodeGenerationKinds.NotImplementedYet -> "raise (System.NotImplementedException())"
        | CodeGenerationKinds.DefaultValue -> "Unchecked.defaultof<_>"
        | _ -> CodeGenerationOptions.DefaultBody

    let getInterfaceMemberIdentifier () =
        IdentifierUtils.encapsulateIdentifier SymbolKind.Ident CodeGenerationOptions.InterfaceMemberIdentifier

    let getLintOptions (serviceProvider: IServiceProvider) =
        serviceProvider.GetService<ILintOptions>()
