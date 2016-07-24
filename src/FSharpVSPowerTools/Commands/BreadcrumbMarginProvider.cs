namespace FSharpVSPowerTools
{
    using System.ComponentModel.Composition;

    using FSharp.Editing.VisualStudio.Navigation;

    using Microsoft.VisualStudio.Text.Editor;
    using Microsoft.VisualStudio.Utilities;

    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(FSharp.Editing.VisualStudio.Constants.BreadcrumbBarMarginName)]
    [Order(After = PredefinedMarginNames.Left)]
    [MarginContainer(PredefinedMarginNames.Top)]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal sealed class BreadcrumbMarginProvider : IWpfTextViewMarginProvider
    {
        /// <summary>
        /// Creates an <see cref="IWpfTextViewMargin"/> for the given <see cref="IWpfTextViewHost"/>.
        /// </summary>
        /// <param name="wpfTextViewHost">The <see cref="IWpfTextViewHost"/> for which to create the <see cref="IWpfTextViewMargin"/>.</param>
        /// <param name="marginContainer">The margin that will contain the newly-created margin.</param>
        /// <returns>The <see cref="IWpfTextViewMargin"/>.
        /// The value may be null if this <see cref="IWpfTextViewMarginProvider"/> does not participate for this context.
        /// </returns>
        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost wpfTextViewHost, IWpfTextViewMargin marginContainer)
        {
            return new BreadcrumbBarMargin(wpfTextViewHost.TextView);
        }
    }
}