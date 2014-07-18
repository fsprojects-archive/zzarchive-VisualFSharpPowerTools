namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools

type MockGeneralOptionsPage() = 
    interface IGeneralOptionsPage with
        member val DepthColorizerEnabled = true with get, set        
        member val FindAllReferencesEnabled = true with get, set        
        member val FolderOrganizationEnabled = true with get, set        
        member val FormattingEnabled = true with get, set        
        member val GenerateRecordStubEnabled = true with get, set
        member val HighlightUsageEnabled = true with get, set
        member val InterfaceImplementationEnabled = true with get, set
        member val NavBarEnabled = true with get, set
        member val NavigateToEnabled = true with get, set
        member val RenameRefactoringEnabled = true with get, set
        member val ResolveUnopenedNamespacesEnabled = true with get, set
        member val SyntaxColoringEnabled = true with get, set
        member val UnionPatternMatchCaseGenerationEnabled = true with get, set
        member val UnusedDeclarationsEnabled = true with get, set
        member val XmlDocEnabled = true with get, set

