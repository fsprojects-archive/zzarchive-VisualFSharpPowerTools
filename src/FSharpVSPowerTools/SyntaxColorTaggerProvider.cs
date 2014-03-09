using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Text.Classification;

namespace FSharpVSPowerTools
{
    //[Export(typeof(IClassifierProvider))]
    //[ContentType("F#")]
    //public class SyntaxConstructClassifierProvider : IClassifierProvider
    //{
    //    [Import]
    //    internal IClassificationTypeRegistryService ClassificationRegistry { get; set; }

    //    public IClassifier GetClassifier(ITextBuffer buffer)
    //    {
    //        return buffer.Properties.GetOrCreateSingletonProperty(() => new SyntaxConstructClassifier(buffer, ClassificationRegistry));
    //    }
    //}

    //internal class SyntaxConstructClassifier : IClassifier
    //{
    //    IClassificationTypeRegistryService classificationRegistry;
    //    ITextBuffer buffer;

    //    public SyntaxConstructClassifier(ITextBuffer buffer, IClassificationTypeRegistryService classificationRegistry)
    //    {
    //        classificationRegistry = classificationRegistry;
    //        buffer = buffer;

    //        buffer.Changed += BufferChanged;
    //    }

    //    /// <summary>
    //    /// When the buffer changes, check to see if any of the edits were in a paragraph with multi-line tokens.
    //    /// If so, we need to send out a classification changed event for those paragraphs.
    //    /// </summary>
    //    void BufferChanged(object sender, TextContentChangedEventArgs e)
    //    {
    //        int eventsSent = 0;

    //        foreach (var change in e.Changes)
    //        {
    //            //SnapshotSpan paragraph = GetEnclosingParagraph(new SnapshotSpan(e.After, change.NewSpan));

    //            //if (MarkdownParser.ParagraphContainsMultilineTokens(paragraph.GetText()))
    //            //{
    //            //    eventsSent++;

    //            //    var temp = this.ClassificationChanged;
    //            //    if (temp != null)
    //            //        temp(this, new ClassificationChangedEventArgs(paragraph));
    //            //}
    //        }
    //    }

    //    public event System.EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    //    public System.Collections.Generic.IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    //    {
    //        throw new System.NotImplementedException();
    //    }
    //}

    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(TypeColorTag))]
    public class SyntaxColorTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal ITextSearchService TextSearchService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            return buffer.Properties.GetOrCreateSingletonProperty(() => new SyntaxColorTagger(textView, buffer) as ITagger<T>);
        }
    }
}
