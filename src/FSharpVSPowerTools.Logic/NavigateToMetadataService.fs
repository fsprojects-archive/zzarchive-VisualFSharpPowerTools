namespace FSharpVSPowerTools.Navigation

open System.IO
open System.Threading
open System.Security
open System.Collections.Generic
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.CodeGeneration
open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.Range
open SourceLink.SymbolStore
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.Win32
open System.Text
open System.ComponentModel.Composition
open System

[<Export>]
type NavigateToMetadataService [<ImportingConstructor>] 
    (vsLanguageService: VSLanguageService,
     [<Import(typeof<SVsServiceProvider>)>]serviceProvider: IServiceProvider,
     projectFactory: ProjectFactory,
     editorOptionsFactory: IEditorOptionsFactoryService) =
    
    // Use a single cache across text views
    static let xmlDocCache = Dictionary<string, IVsXMLMemberIndex>()
    let xmlIndexService = serviceProvider.GetService<IVsXMLMemberIndexService, SVsXMLMemberIndexService>()

    /// If the XML comment starts with '<' not counting whitespace then treat it as a literal XML comment.
    /// Otherwise, escape it and surround it with <summary></summary>
    let processXml (xml: string) =
        if String.IsNullOrEmpty(xml) then xml
        else
            let trimmedXml = xml.TrimStart([|' ';'\r';'\n'|])
            if String.IsNullOrEmpty(trimmedXml) then xml
            else
                match trimmedXml.[0] with
                | '<' ->
                    String.Join("", "<root>", xml, "</root>")
                | _ ->
                    let escapedXml = SecurityElement.Escape(xml)
                    String.Join("", "<summary>", escapedXml, "</summary>")
            
    let getXmlDocBySignature (fsSymbol: FSharpSymbol) signature =
        match fsSymbol.Assembly.FileName with
        | Some assemblyName ->
            match xmlDocCache.TryGetValue(assemblyName) with
            | true, xmlIndex -> 
                match xmlIndex.ParseMemberSignature(signature) with
                | _, 0u -> []
                | _, index ->
                    match xmlIndex.GetMemberXML(index) with
                    | VSConstants.S_OK, xml ->
                        let processedXml = processXml xml
                        let xmlDocs = ResizeArray()
                        match xmlIndexService.GetMemberDataFromXML(processedXml) with
                        | VSConstants.S_OK, memberData ->
                            match memberData.GetSummaryText() with
                            | VSConstants.S_OK, xmlSummary ->
                                xmlDocs.Add(xmlSummary)
                            | _ -> ()
                            match memberData.GetParamCount() with
                            | VSConstants.S_OK, count when count > 0 ->
                                for i in 0..count-1 do
                                    match memberData.GetParamTextAt(i) with
                                    | VSConstants.S_OK, name, text ->
                                        let xmlDoc = sprintf "%s: %s" name text
                                        xmlDocs.Add(xmlDoc)
                                    | _ -> ()
                            | _ -> ()
                            xmlDocs |> Seq.toList
                        | _ -> []                            
                    | _ -> []
            | false, _ -> 
                match xmlIndexService.CreateXMLMemberIndex(assemblyName) with
                | VSConstants.S_OK, xmlIndex ->
                    match xmlIndex.BuildMemberIndex() with
                    | VSConstants.S_OK ->
                        xmlDocCache.Add(assemblyName, xmlIndex)
                        []
                    | _ -> []
                | _ -> []
        | None -> []

    // Keep a single window frame for all text views
    static let mutable currentWindow: IVsWindowFrame option = None 

    let tryFindExactLocation signature filePath signatureProject currentSymbol =
        async {
            let! symbolUses = 
                vsLanguageService.GetAllUsesOfAllSymbolsInSourceString(signature, filePath, signatureProject, AllowStaleResults.No, checkForUnusedOpens=false)

            /// Try to reconstruct fully qualified name for the purpose of matching symbols
            let rec tryGetFullyQualifiedName (symbol: FSharpSymbol) = 
                Option.attempt (fun _ -> 
                    match symbol with
                    | TypedAstPatterns.Entity (entity, _, _) ->
                        Some (sprintf "%s.%s" entity.AccessPath entity.DisplayName)
                    | MemberFunctionOrValue mem ->
                        tryGetFullyQualifiedName mem.EnclosingEntity
                        |> Option.map (fun parent -> sprintf "%s.%s" parent mem.DisplayName)
                    | Field(field, _) ->
                        tryGetFullyQualifiedName field.DeclaringEntity
                        |> Option.map (fun parent -> sprintf "%s.%s" parent field.DisplayName)
                    | UnionCase uc ->
                        match uc.ReturnType with
                        | TypeWithDefinition entity ->
                            tryGetFullyQualifiedName entity
                            |> Option.map (fun parent -> sprintf "%s.%s" parent uc.DisplayName)
                        | _ -> 
                            None
                    | ActivePatternCase case ->
                        let group = case.Group
                        group.EnclosingEntity
                        |> Option.bind tryGetFullyQualifiedName
                        |> Option.map (fun parent -> 
                            let sb = StringBuilder()
                            sb.Append("|") |> ignore
                            for name in group.Names do
                                sb.AppendFormat("{0}|", name) |> ignore
                            if not group.IsTotal then
                                sb.Append("_|") |> ignore
                            sprintf "%s.( %O )" parent sb)
                    | _ ->
                        None)
                |> Option.flatten
                |> Option.orTry (fun _ -> Option.attempt (fun _ -> symbol.FullName))

            let isLocalSymbol filePath (symbol: FSharpSymbol) =
                symbol.DeclarationLocation 
                |> Option.map (fun r -> String.Equals(r.FileName, filePath, StringComparison.OrdinalIgnoreCase)) 
                |> Option.getOrElse false

            let currentSymbolFullName = tryGetFullyQualifiedName currentSymbol

            let matchedSymbol = 
                symbolUses 
                |> Seq.groupBy (fun { SymbolUse = symbolUse } -> symbolUse.Symbol)
                |> Seq.collect (fun (_, uses) -> Seq.truncate 1 uses)
                |> Seq.choose (fun { SymbolUse = symbolUse } -> 
                    match symbolUse.Symbol with
                    | TypedAstPatterns.Entity _
                    | MemberFunctionOrValue _
                    | ActivePatternCase _                   
                    | UnionCase _
                    | Field _ as symbol -> 
                        let symbolFullName = tryGetFullyQualifiedName symbol
                        if symbolFullName = currentSymbolFullName && isLocalSymbol filePath symbol then
                            Some symbol
                        else None
                    | _ -> None)
                |> Seq.sortBy (fun symbol -> 
                    symbol.DeclarationLocation |> Option.map (fun r -> r.StartLine, r.StartColumn))
                |> Seq.tryHead

            return matchedSymbol |> Option.bind (fun s -> s.DeclarationLocation)
        }

 

    // Now the input is an entity or a member/value.
    // We always generate the full enclosing entity signature if the symbol is a member/value
    let tryCreateMetadataContext project textBuffer ast (span: SnapshotSpan) (fsSymbolUse: FSharpSymbolUse) = 
        async {
            let fsSymbol = fsSymbolUse.Symbol
            let fileName = SignatureGenerator.getFileNameFromSymbol fsSymbol

            // The file system is case-insensitive so list.fsi and List.fsi can clash
            // Thus, we generate a tmp subfolder based on the hash of the filename
            let subFolder = string (uint32 (hash fileName))

            let filePath = Path.Combine(Path.GetTempPath(), subFolder, fileName)
            let editorOptions = editorOptionsFactory.GetOptions(textBuffer)
            let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)  
            let displayContext = fsSymbolUse.DisplayContext
            match VsShellUtilities.IsDocumentOpen(serviceProvider, filePath, Constants.guidLogicalTextView) with
            | true, _hierarchy, _itemId, _windowFrame ->
                match projectFactory.TryGetSignatureProjectProvider(filePath) with
                | Some (signature, signatureProject) ->
                    let! range = tryFindExactLocation signature filePath signatureProject fsSymbol
                    return Some (filePath, range)
                | None -> 
                    Logging.logInfo (fun _ -> sprintf "Can't find a signature or signature project for '%s'" filePath)               
                    return None
            | false, _, _, _ ->
                let (startLine, startCol, _, _) = span.ToRange()
                let pos = mkPos (startLine+1) startCol
                let openDeclarations = 
                    ast 
                    |> Option.map (OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation pos) 
                    |> Option.getOrElse []
                match SignatureGenerator.formatSymbol 
                        (getXmlDocBySignature fsSymbol) indentSize displayContext openDeclarations fsSymbol 
                        SignatureGenerator.Filterer.NoFilters SignatureGenerator.BlankLines.Default with
                | Some signature ->
                    let directoryPath = Path.GetDirectoryName(filePath)
                    Directory.CreateDirectory(directoryPath) |> ignore
                    File.WriteAllText(filePath, signature)
                    let signatureProject = projectFactory.RegisterSignatureProjectProvider filePath (signature, project)
                    let! range = tryFindExactLocation signature filePath signatureProject fsSymbol
                    return Some (filePath, range)
                | None ->
                    return None
        }

    let viewMetadata context = 
        match context with
        | Some (filePath, range: range option) ->
            match projectFactory.TryGetSignatureProjectProvider(filePath) with
            | Some (signature, _signatureProject) -> 
                let frameAndBuffer =  
                    match VsShellUtilities.IsDocumentOpen(serviceProvider, filePath, Constants.guidLogicalTextView) with
                    | true, _hierarchy, _itemId, windowFrame ->
                        let vsTextView = VsShellUtilities.GetTextView(windowFrame)
                        let mutable vsTextLines = Unchecked.defaultof<_>
                        vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                        let vsTextBuffer = vsTextLines :> IVsTextBuffer         
                        Some (windowFrame, vsTextBuffer)
                    | false, _, _, _ ->
                            let directoryPath = Path.GetDirectoryName(filePath)
                            Directory.CreateDirectory(directoryPath) |> ignore
                            File.WriteAllText(filePath, signature)
                            let mutable hierarchy = Unchecked.defaultof<_>
                            let mutable itemId = Unchecked.defaultof<_>
                            let windowFrame = ref null
                            let canShow = 
                                try
                                    VsShellUtilities.OpenDocument(
                                        serviceProvider, 
                                        filePath, 
                                        Constants.guidLogicalTextView, 
                                        &hierarchy,
                                        &itemId,
                                        windowFrame)
                                    true
                                with _ -> false
                            if canShow then
                                // Ensure that only one signature is opened at a time
                                currentWindow 
                                |> Option.bind Option.ofNull
                                |> Option.iter (fun window -> window.CloseFrame(uint32 __FRAMECLOSE.FRAMECLOSE_NoSave) |> ignore)
                                // Prevent the window being reopened as a part of a solution
                                (!windowFrame).SetProperty(int __VSFPROPID5.VSFPROPID_DontAutoOpen, true) |> ignore
                                currentWindow <- Some !windowFrame
                                let vsTextView = VsShellUtilities.GetTextView(!windowFrame)
                                let mutable vsTextLines = Unchecked.defaultof<_>
                                vsTextView.GetBuffer(&vsTextLines) |> ensureSucceeded
                                let vsTextBuffer = vsTextLines :> IVsTextBuffer
                                match vsTextBuffer.GetStateFlags() with
                                | VSConstants.S_OK, currentFlags ->
                                    // Try to set buffer to read-only mode
                                    vsTextBuffer.SetStateFlags(currentFlags ||| uint32 BUFFERSTATEFLAGS.BSF_USER_READONLY) |> ignore
                                | _ -> ()
                                Some (!windowFrame, vsTextBuffer)
                            else
                                None
                frameAndBuffer
                |> Option.iter (fun (windowFrame, vsTextBuffer) ->         
                    let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
                    let (startRow, startCol) = 
                        range 
                        |> Option.map (fun r -> r.StartLine-1, r.StartColumn)
                        |> Option.getOrElse (0, 0)
                    vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.guidLogicalTextView, startRow, startCol, startRow, startCol) 
                    |> ensureSucceeded
                    // We display the window after putting the project into the project system.
                    // Hopefully syntax coloring on generated signatures will catch up on time.
                    windowFrame.Show() |> ensureSucceeded)
            | None -> ()
        | None ->
            ()

    member __.NavigateToMetadata(project, textBuffer, ast, span, fsSymbolUse) = 
        async {
            let! context = tryCreateMetadataContext project textBuffer ast span fsSymbolUse
            return viewMetadata context
        }
        
    member __.TryFindMetadataRange(project, textBuffer, ast, span, fsSymbolUse) = 
        async {
            let! context = tryCreateMetadataContext project textBuffer ast span fsSymbolUse
            return 
                context
                |> Option.map (fun (filePath, range) -> 
                    let zeroPos = mkPos 1 0
                    range |> Option.getOrTry (fun _ -> mkRange filePath zeroPos zeroPos)) 
        }

    static member ClearXmlDocCache() =
        xmlDocCache.Clear()
          