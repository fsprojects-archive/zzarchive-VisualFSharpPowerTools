using System.ComponentModel;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell.Interop;

namespace FSharpVSPowerTools
{
    [Guid("30829e04-262b-4af7-89e3-d4bf7a1d0c23")]
    public class FantomasOptionsPage : DialogPage, IFormattingOptions
    {

        IFormattingOptions settings;

        //[ImportingConstructor]
        //public FantomasOptionsPage(IFormattingOptions _settings)
        //{
        //    settings = _settings;
        //}

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            settings = VFPT_Settings.getFormattingOptions();
            PageWidth                = settings.PageWidth                 ;
            SemicolonAtEndOfLine     = settings.SemicolonAtEndOfLine      ;
            SpaceBeforeArgument      = settings.SpaceBeforeArgument       ;
            SpaceBeforeColon         = settings.SpaceBeforeColon          ;
            SpaceAfterComma          = settings.SpaceAfterComma           ;
            SpaceAfterSemicolon      = settings.SpaceAfterSemicolon       ;
            SpaceAroundDelimiter     = settings.SpaceAroundDelimiter      ;
            IndentOnTryWith          = settings.IndentOnTryWith           ;
            ReorderOpenDeclaration   = settings.ReorderOpenDeclaration    ;

        }

        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            settings = VFPT_Settings.getFormattingOptions();
            settings.PageWidth              =   PageWidth               ;
            settings.SemicolonAtEndOfLine   =   SemicolonAtEndOfLine    ;
            settings.SpaceBeforeArgument    =   SpaceBeforeArgument     ;
            settings.SpaceBeforeColon       =   SpaceBeforeColon        ;
            settings.SpaceAfterComma        =   SpaceAfterComma         ;
            settings.SpaceAfterSemicolon    =   SpaceAfterSemicolon     ;
            settings.SpaceAroundDelimiter   =   SpaceAroundDelimiter    ;
            settings.IndentOnTryWith        =   IndentOnTryWith         ;
            settings.ReorderOpenDeclaration = ReorderOpenDeclaration;
            settings.Save();
        }

        [Category("Layout")]
        [DisplayName("Page Width")]
        [Description("Set maximum width.")]
        public int PageWidth { get; set; }

        [Category("Syntax")]
        [DisplayName("Semicolon at End of Line")]
        [Description("Insert ';' at EOL between list, array and record elements.")]
        public bool SemicolonAtEndOfLine { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Argument")]
        [Description("Insert ' ' before arguments in function applications.")]
        public bool SpaceBeforeArgument { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Colon")]
        [Description("Insert ' ' before ':' in type annotations.")]
        public bool SpaceBeforeColon { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Comma")]
        [Description("Insert ' ' after ',' in tuples.")]
        public bool SpaceAfterComma { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Semicolon")]
        [Description("Insert ' ' after ';' in list, array and record elements.")]
        public bool SpaceAfterSemicolon { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Around Delimiter")]
        [Description("Insert ' ' after starting and before ending of lists, arrays, sequences and records.")]
        public bool SpaceAroundDelimiter { get; set; }

        [Category("Indentation")]
        [DisplayName("Indent on Try...With")]
        [Description("Indent one level in all clauses of with blocks.")]
        public bool IndentOnTryWith { get; set; }

        [Category("Refactoring")]
        [DisplayName("Reorder open declarations")]
        [Description("Reorder and deduplicate open statements while doing formatting.")]
        public bool ReorderOpenDeclaration { get; set; }


        public override void SaveSettingsToXml(IVsSettingsWriter writer)
        {
            base.SaveSettingsToXml(writer);
        }
    }


}
