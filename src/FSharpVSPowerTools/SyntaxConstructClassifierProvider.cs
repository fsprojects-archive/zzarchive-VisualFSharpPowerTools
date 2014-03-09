using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Text.Classification;

namespace FSharpVSPowerTools
{
    static class ClassificationTypes
    {
        // Base definition for all other classification types
        [Export]
        [Name("FSharp")]
        internal static ClassificationTypeDefinition FSharpDefinition;

        // Type names
        [Export]
        [Name("FSharp.TypeName")]
        [BaseDefinition("FSharp")]
        internal static ClassificationTypeDefinition FSharpTypeNameDefinition;
    }

    static class ClassificationFormats
    {
        // TypeName
        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.TypeName")]
        [Name("FSharp.TypeName")]
        sealed class FSharpTypeNameFormat : ClassificationFormatDefinition
        {
            public FSharpTypeNameFormat()
            {
                this.ForegroundColor = System.Windows.Media.Colors.Teal;
            }
        }
    }

    [Export(typeof(IClassifierProvider))]
    [ContentType("F#")]
    public class SyntaxConstructClassifierProvider : IClassifierProvider
    {
        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry { get; set; }

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            return buffer.Properties.GetOrCreateSingletonProperty(() => new SyntaxConstructClassifier(buffer, ClassificationRegistry));
        }
    }
}
