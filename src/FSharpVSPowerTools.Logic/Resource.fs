namespace FSharpVSPowerTools

[<RequireQualifiedAccess>] 
module Resource =
    let [<Literal>] vsPackageTitle = "F# Power Tools"
    
    let [<Literal>] formattingErrorMessage = "Unable to format. "
    let [<Literal>] formattingValidationMessage = "Validation after formatting failed. The original content is preserved."

    let [<Literal>] renameErrorMessage = "Unable to rename. The symbol hasn't been declared in current solution."
    
    let [<Literal>] validatingEmptyName = "Empty names are not allowed."
    let [<Literal>] validatingUnionCase = "Invalid name for union cases."
    let [<Literal>] validatingActivePattern = "Invalid name for active patterns."
    let [<Literal>] validatingIdentifier = "Invalid name for identifiers."
    let [<Literal>] validatingTypeName = "Invalid name for types."
    let [<Literal>] validatingGenericTypeParameter = "Invalid name for generic type parameters."
    let [<Literal>] validatingStaticallyResolvedTypeParameter = "Invalid name for statically resolved type parameters."
    let [<Literal>] validatingOperator ="Invalid name for operators."
    let [<Literal>] validationFolderWithGivenNameAlreadyExists = "Folder with given name already exists in the project."
    let [<Literal>] validationDestinationFolderDoesNotExist = "Destination folder doesn't exist. Please try to create destination folder before moving files."
    let [<Literal>] validationCannotCreateFolder = "Unable to create folder. Please make sure you have enough privileges."
    let [<Literal>] validationExistingFolderOnDisk = "An existing folder has been added."
    let [<Literal>] validationRenameFolderAlreadyExistsOnDisk = "Unable to rename folder. Folder with given name already exists on disk."

    let [<Literal>] navBarUnauthorizedMessage = "Unauthorized access to navigation bar configuration. Please try again after restarting Visual Studio from Administrator."
    let [<Literal>] navBarErrorMessage = "Error occurs while saving navigation bar configuration."

    let newFolderDialogTitle = vsPackageTitle + " - New Folder"
    let renameFolderDialogTitle = vsPackageTitle + " - Rename Folder"

    let [<Literal>] interfaceEmptyStatusMessage = "This interface has no member."
    let [<Literal>] interfaceFilledStatusMessage = "All members of this interface have been implemented."

    let [<Literal>] findSymbolUseCurrentProject = "Finding symbols in current project..."
    let [<Literal>] findSymbolUseOtherProjects = "Finding symbols in other projects..."
    let [<Literal>] findSymbolUseAllProjects = "Finding symbol usages in all projects..."

    let [<Literal>] findAllReferencesInitializingMessage = "Initializing Find All References..."
    let [<Literal>] findAllReferencesFindInFileMessage = "Finding symbol usages in file..."
    let [<Literal>] findAllReferencesFindInProjectsMessage = "Finding symbol usages in projects..."
    
    let [<Literal>] findAllReferencesInvalidExpressionMessage = "The caret must be on a valid expression to find all references."
    let [<Literal>] findAllReferencesIllformedExpressionMessage = "Can't find a symbol under cursor. Please make sure that the code is typechecked."
 
    let [<Literal>] implementInterfaceCommandName = "Explicitly implement interface"
    let [<Literal>] recordGenerationCommandName = "Generate record stubs"
    let [<Literal>] unionPatternMatchCaseCommandName = "Generate union pattern match cases"

    let [<Literal>] goToDefinitionStatusMessage = "Generated symbol metadata"
    let [<Literal>] goToDefinitionInvalidSymbolMessage = "Can't generate metadata for this symbol."

    let [<Literal>] languageServiceErrorMessage = """
Internal language services have encountered severe errors.
Syntax coloring or other features may stop working.
If you experience such issues, they could be fixed as follows: (1) Clean current solution (2) Restart Visual Studio (3) Rebuild the solution.
We're sorry for any inconvenience caused.
"""