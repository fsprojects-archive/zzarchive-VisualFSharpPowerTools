namespace FSharpVSPowerTools

[<RequireQualifiedAccess>] 
module Resource =
    let [<Literal>] vsPackageTitle = "F# Power Tools"
    
    let [<Literal>] formattingErrorMessage = "Unable to format. "

    let [<Literal>] renameErrorMessage = "Unable to rename. The symbol hasn't been declared in current project."
    let [<Literal>] validatingEmptyName = "Empty names are not allowed."
    let [<Literal>] validatingOriginalName = "New name should not be the same as the original one."
    let [<Literal>] validatingUnionCase = "Invalid name for union cases."
    let [<Literal>] validatingActivePattern = "Invalid name for active patterns."
    let [<Literal>] validatingIdentifier = "Invalid name for identifiers."
    let [<Literal>] validatingGenericTypeParameter = "Invalid name for generic type parameters."
    let [<Literal>] validatingStaticallyResolvedTypeParameter = "Invalid name for statically resolved type parameters."
    let [<Literal>] validatingOperator ="Invalid name for operators."

    let [<Literal>] navBarUnauthorizedMessage = "Unauthorized access to navigation bar configuration. Please try again after restarting Visual Studio from Administrator."
    let [<Literal>] navBarErrorMessage = "Error occurs while saving navigation bar configuration."

    let [<Literal>] newFolderDialogTitle = "F# Power Tools - New Folder"
    let [<Literal>] renameFolderDialogTitle = "F# Power Tools - Rename Folder"