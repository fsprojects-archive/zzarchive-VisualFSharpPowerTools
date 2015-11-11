namespace FSharpVSPowerTools

open FSharpVSPowerTools.ProjectSystem                       
open FSharpVSPowerTools.SyntaxColoring                      
open Microsoft.VisualStudio.Language.StandardClassification 
open Microsoft.VisualStudio.Shell                           
open Microsoft.VisualStudio.Text                            
open Microsoft.VisualStudio.Text.Classification             
open Microsoft.VisualStudio.Text.Editor                     
open Microsoft.VisualStudio.Text.Formatting                 
open Microsoft.VisualStudio.Text.Tagging                    
open Microsoft.VisualStudio.Utilities                       
open Microsoft.Win32                                        
open System                                                 
open System.Collections.Generic                             
open System.Collections.ObjectModel                         
open System.ComponentModel.Composition                      
open System.Diagnostics                                     
open System.Windows.Media                                   


module ClassificationTypes =
    //namespace FSharpVSPowerTools
    //
    //    static class ClassificationTypes
    //    
    let [<Literal>] FSharpReferenceType = Constants.fsharpReferenceType 
    let [<Literal>] FSharpValueType     = Constants.fsharpValueType     
    let [<Literal>] FSharpPatternCase   = Constants.fsharpPatternCase   
    let [<Literal>] FSharpFunction      = Constants.fsharpFunction      
    let [<Literal>] FSharpMutableVar    = Constants.fsharpMutableVar    
    let [<Literal>] FSharpQuotation     = Constants.fsharpQuotation     
    let [<Literal>] FSharpModule        = Constants.fsharpModule        
    let [<Literal>] FSharpUnused        = Constants.fsharpUnused        
    let [<Literal>] FSharpPrintf        = Constants.fsharpPrintf        
    let [<Literal>] FSharpEscaped       = Constants.fsharpEscaped       
    let [<Literal>] FSharpOperator      = Constants.fsharpOperator      

    [<Export>][<Name (FSharpReferenceType)>][<BaseDefinition "identifier">]
    let internal FSharpReferenceClassificationType = ClassificationTypeDefinition () 

    [<Export>][<Name (FSharpValueType)>][<BaseDefinition "identifier">]
    let internal FSharpValueClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpPatternCase)>][<BaseDefinition "identifier">]
    let internal FSharpPatternCaseClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpFunction)>][<BaseDefinition "identifier">]
    let internal FSharpFunctionClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpMutableVar)>][<BaseDefinition "identifier">]
    let internal FSharpMutableletClassificationType = ClassificationTypeDefinition ()
        
    [<Export>][<Name (FSharpQuotation)>][<BaseDefinition "identifier">]
    let internal FSharpQuotationClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpModule)>][<BaseDefinition "identifier">]
    let internal FSharpModuleClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpUnused)>][<BaseDefinition "identifier">]
    let internal FSharpUnusedClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpPrintf)>][<BaseDefinition "identifier">]
    let internal FSharpPrintfClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpEscaped)>][<BaseDefinition "identifier">]
    let internal FSharpEscapedClassificationType = ClassificationTypeDefinition ()

    [<Export>][<Name (FSharpOperator)>][<BaseDefinition "identifier">]
    let internal FSharpOperatorClassificationType = ClassificationTypeDefinition ()


    type FontColor (?foreground:Color Nullable, ?background:Color Nullable) =
        let foreground = defaultArg foreground (Nullable())
        let background = defaultArg background (Nullable())

        member __.Foreground = foreground
        member __.Background = background
        new (?foreground:Color, ?background:Color) =
            let nullout = function| Some x -> Nullable x | None -> Nullable()
            FontColor (nullout foreground, nullout background)
                
open ClassificationTypes
[<Export>]
type ClassificationColorManager [<ImportingConstructor>] 
    (   themeManager: ThemeManager,
        classificationFormatMapService : IClassificationFormatMapService ,
        classificationTypeRegistry  : IClassificationTypeRegistryService ) =

    let themeColors = Dictionary<VisualStudioTheme, IDictionary<string, FontColor>> ()

    let lastTheme = VisualStudioTheme.Unknown

    let lightAndBlueColors = Dictionary<string, FontColor>() :> IDictionary<_,_> 

    do  lightAndBlueColors {
            add ( ClassificationTypes.FSharpReferenceType, FontColor(Color.FromRgb((43uy, 145uy, 175uy))))
            add ( ClassificationTypes.FSharpReferenceType, FontColor(Color.FromRgb(43uy, 145uy, 175uy))) 
            add ( ClassificationTypes.FSharpValueType,     FontColor(Color.FromRgb(43uy, 145uy, 175uy))) 
            add ( ClassificationTypes.FSharpPatternCase,   FontColor(Colors.Black)) 
            add ( ClassificationTypes.FSharpFunction,      FontColor(Colors.Black)) 
            add ( ClassificationTypes.FSharpMutableVar,    FontColor(Colors.Black)) 
            add ( ClassificationTypes.FSharpQuotation,     FontColor(background=Color.FromRgb(255uy, 242uy, 223uy))) 
            add ( ClassificationTypes.FSharpModule,        FontColor(Color.FromRgb(43uy, 145uy, 175uy))) 
            add ( ClassificationTypes.FSharpUnused,        FontColor(Color.FromRgb(157uy, 157uy, 157uy)))
            add ( ClassificationTypes.FSharpPrintf,        FontColor(Color.FromRgb(43uy, 145uy, 175uy))) 
            add ( ClassificationTypes.FSharpEscaped,       FontColor(Color.FromRgb(255uy, 0uy, 128uy))) 
            add ( ClassificationTypes.FSharpOperator,      FontColor(Colors.Black))
        }
    // Dark theme colors
    let darkColors = Dictionary<string, FontColor>():> IDictionary<_,_>
    
    do darkColors { 
        add (ClassificationTypes.FSharpReferenceType, FontColor(Color.FromRgb(78uy, 201uy, 176uy))) 
        add (ClassificationTypes.FSharpValueType    , FontColor(Color.FromRgb(78uy, 201uy, 176uy))) 
        add (ClassificationTypes.FSharpPatternCase  , FontColor(Color.FromRgb(220uy, 220uy, 220uy)))
        add (ClassificationTypes.FSharpFunction     , FontColor(Color.FromRgb(220uy, 220uy, 220uy)))
        add (ClassificationTypes.FSharpMutableVar   , FontColor(Color.FromRgb(220uy, 220uy, 220uy)))
        add (ClassificationTypes.FSharpQuotation    , FontColor(background=Color.FromRgb(98uy, 58uy, 0uy)))
        add (ClassificationTypes.FSharpModule       , FontColor(Color.FromRgb(78uy, 201uy, 176uy)))
        add (ClassificationTypes.FSharpUnused       , FontColor(Color.FromRgb(155uy, 155uy, 155uy)))
        add (ClassificationTypes.FSharpPrintf       , FontColor(Color.FromRgb(78uy, 220uy, 176uy)))
        add (ClassificationTypes.FSharpEscaped      , FontColor(Color.FromRgb(190uy, 0uy, 94uy)))
        add (ClassificationTypes.FSharpOperator     , FontColor(Color.FromRgb(220uy, 220uy, 220uy)))
    }

    do  themeColors {
        add ( VisualStudioTheme.Blue, lightAndBlueColors )
        add ( VisualStudioTheme.Light, lightAndBlueColors )
        add ( VisualStudioTheme.Unknown, lightAndBlueColors )
        add ( VisualStudioTheme.Dark, darkColors )
    }    

    member __.GetDefaultColors (category:string) =
        let currentTheme = themeManager.GetCurrentTheme()
        match currentTheme with
        | VisualStudioTheme.Dark ->
            let mutable color = FontColor(Color.FromRgb(220uy, 220uy, 220uy), Color.FromRgb(30uy, 30uy, 30uy))
            if not (themeColors.[currentTheme].TryGetValue(category,&color)) then
                Logging.logWarning ( fun () -> sprintf "Theme manager can't read colors correctly from %s theme."  (string currentTheme))
            color  
        | _ -> 
            let mutable color = FontColor (Colors.Black, Colors.White)
            if not (themeColors.[currentTheme].TryGetValue(category,&color)) then
                Logging.logWarning ( fun () -> sprintf "Theme manager can't read colors correctly from %s theme."  (string currentTheme))
            color  

    member __.UpdateColors () =
        let currentTheme = themeManager.GetCurrentTheme()
        if currentTheme <> VisualStudioTheme.Unknown && currentTheme <> lastTheme then
            let lastTheme = currentTheme
            let colors = themeColors.[currentTheme]
            let formatMap = classificationFormatMapService.GetClassificationFormatMap(category="text")
            try
                formatMap.BeginBatchUpdate()
                for pair in colors do
                    let ptype, color = pair.Key, pair.Value
                    let classificationType = classificationTypeRegistry.GetClassificationType ptype
                    let oldProp = formatMap.GetTextProperties( classificationType)
                    let foregroundBrush, backgroundBrush = 
                        (if color.Foreground = Nullable() then null else SolidColorBrush color.Foreground.Value),
                        (if color.Background = Nullable() then null else SolidColorBrush color.Background.Value)
                    let newProp = 
                        TextFormattingRunProperties.CreateTextFormattingRunProperties
                            (   foregroundBrush, backgroundBrush, oldProp.Typeface, Nullable(), Nullable(), oldProp.TextDecorations, 
                                oldProp.TextEffects, oldProp.CultureInfo    )
                    formatMap.SetTextProperties (classificationType, newProp)
            finally
                formatMap.EndBatchUpdate()

module  ClassificationFormats =
    

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType (ClassificationTypeNames = ClassificationTypes.FSharpReferenceType)>]
    [<Name (ClassificationTypes.FSharpReferenceType)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>] 
    type internal FSharpReferenceTypeFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpReferenceType
        member __.DisplayName = "F# Types"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background


    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)>]
    [<Name (ClassificationTypes.FSharpValueType)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>] 
    type internal FSharpValueTypeFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpValueType
        member __.DisplayName = "F# Value Types"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)>]
    [<Name (ClassificationTypes.FSharpPatternCase)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>] 
    type internal FSharpPatternCaseFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpPatternCase
        member __.DisplayName = "F# Patterns"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)>]
    [<Name (ClassificationTypes.FSharpFunction)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>] 
    type internal FSharpFunctionFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpFunction
        member __.DisplayName = "F# Functions / Methods"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpMutableVar)>]
    [<Name (ClassificationTypes.FSharpMutableVar)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>] 
    type internal FSharpMutableletFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpMutableVar
        member __.DisplayName = "F# Mutable letiables / Reference Cells"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpQuotation)>]
    [<Name (ClassificationTypes.FSharpQuotation)>]
    [<UserVisible (true)>]
    [<Order(Before = PredefinedClassificationTypeNames.String)>]
    type internal FSharpQuotationFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpQuotation
        member __.DisplayName = "F# Quotations"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
        member __.ForegroundCustomizable = false
        

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpModule)>]
    [<Name (ClassificationTypes.FSharpModule)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>]
    type internal FSharpModuleFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpModule
        member __.DisplayName = "F# Modules"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            
        
    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpUnused)>]
    [<Name (ClassificationTypes.FSharpUnused)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>]
    type internal FSharpUnusedFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpUnused
        member __.DisplayName = "F# Unused Declarations"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
            

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPrintf)>]
    [<Name (ClassificationTypes.FSharpPrintf)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>]
    type internal FSharpPrintfFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpPrintf
        member __.DisplayName = "F# Printf Format"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
             

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpEscaped)>]
    [<Name (ClassificationTypes.FSharpEscaped)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>]
    type internal FSharpEscapedFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpEscaped
        member __.DisplayName = "F# Escaped Characters"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background
             

    [<Export (typeof<EditorFormatDefinition>)>]
    [<ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpOperator)>]
    [<Name (ClassificationTypes.FSharpOperator)>]
    [<UserVisible (true)>]
    [<Order(After = PredefinedClassificationTypeNames.String)>]
    type internal FSharpOperatorFormat [<ImportingConstructor>] (colorManager:ClassificationColorManager) =
        inherit ClassificationFormatDefinition()
        let colors = colorManager.GetDefaultColors ClassificationTypes.FSharpOperator
        member __.DisplayName = "F# Operators"
        member __.ForegroundColor = colors.Foreground
        member __.BackgroundColor = colors.Background

             
[<Export (typeof<ITaggerProvider>)>]
[<TagType(typeof<UnusedDeclarationTag>)>]
[<Export (typeof<IClassifierProvider>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type SyntaxConstructClassifierProvider [<ImportingConstructor>]
    (   [<Import(typeof<SVsServiceProvider>)>]
        serviceProvider           : IServiceProvider,
        shellEventListener        : ShellEventListener,
        classificationColorManager: ClassificationColorManager,
        classificationRegistry    : IClassificationTypeRegistryService,
        textDocumentFactoryService: ITextDocumentFactoryService,
        fsharpVsLanguageService   : VSLanguageService,
        projectFactory            : ProjectFactory ) as self =

    static let serviceType = typeof<SyntaxConstructClassifier> : Type
    let subscriptions = ResizeArray()
    let themeChanged = shellEventListener.ThemeChanged


    // Receive notification for Visual Studio theme change
    do  themeChanged.Subscribe (fun _ ->
            classificationColorManager.UpdateColors ()
        ) |> subscriptions.Add

    member __.GetClassifier(textBuffer: ITextBuffer)= 
        //let generalOptions = Setting.getGeneralOptions serviceProvider
//            if not generalOptions.SyntaxColoringEnabled then None else
//        let includeUnusedReferences = generalOptions.UnusedReferencesEnabled
//        let includeUnusedOpens = generalOptions.UnusedOpensEnabled
        let includeUnusedReferences = false
        let includeUnusedOpens = false
        //let mutable doc = Unchecked.defaultof<ITextDocument>
        //if not(textDocumentFactoryService.TryGetTextDocument (textBuffer,&doc)) then None else
            //let d = doc
        maybe{
            let! doc = textDocumentFactoryService.TryDocumentFromBuffer textBuffer
            return textBuffer.Properties.GetOrCreateSingletonProperty (serviceType, fun () ->
                new SyntaxConstructClassifier
                    (   doc, textBuffer, classificationRegistry,
                        fsharpVsLanguageService, serviceProvider, projectFactory,
                        includeUnusedReferences, includeUnusedOpens))
                //|> Some
        }

    interface IClassifierProvider with
        member __.GetClassifier textBuffer = 
            match self.GetClassifier textBuffer with
            | None -> null
            | Some cl -> cl :> IClassifier
            

    interface ITaggerProvider with
        member x.CreateTagger(buffer: ITextBuffer)= 
            (self :> IClassifierProvider).GetClassifier buffer :?> ITagger<_>


    interface IDisposable with
        member __.Dispose () = subscriptions.Iter dispose

