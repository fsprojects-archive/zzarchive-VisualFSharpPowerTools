using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.Navigation;
using Microsoft.VisualStudio.Text;
using System.Collections.ObjectModel;
using System;
using Microsoft.VisualStudio.Shell.Interop;
using EnvDTE80;
using EnvDTE;

namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [Export(typeof(IWpfTextViewConnectionListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    public class GoToDefinitionFilterProvider : IVsTextViewCreationListener, IWpfTextViewConnectionListener, IDisposable
    {
        [Import]
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import]
        internal IEditorOptionsFactoryService editorOptionsFactory = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        private readonly System.IServiceProvider serviceProvider = null;
        private readonly SolutionEvents solutionEvents = null;
        private static readonly Type serviceType = typeof(GoToDefinitionFilterProvider);
        
        [ImportingConstructor]
        public GoToDefinitionFilterProvider([Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider)
        {
            this.serviceProvider = serviceProvider;

            var dte = serviceProvider.GetService(typeof(SDTE)) as DTE;
            var events = dte.Events as Events2;
            this.solutionEvents = events.SolutionEvents;
            this.solutionEvents.AfterClosing += Cleanup;
        }

        private void Cleanup()
        {
 	        GoToDefinitionFilter.ClearXmlDocCache();
        }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var textView = editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || (!generalOptions.GoToMetadataEnabled && !generalOptions.GoToDownloadedSourceEnabled)) return;
            // Favor Navigate to Source feature over Go to Metadata
            var preference = generalOptions.GoToDownloadedSourceEnabled
                                ? (generalOptions.GoToMetadataEnabled ? NavigationPreference.SymbolSourceOrMetadata : NavigationPreference.SymbolSource) 
                                : NavigationPreference.Metadata;
            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(textView.TextBuffer, out doc))
            {
                Debug.Assert(doc != null, "Text document shouldn't be null.");
                var commandFilter = new GoToDefinitionFilter(doc, textView, editorOptionsFactory,
                                                             fsharpVsLanguageService, serviceProvider, projectFactory,
                                                             preference);
                textView.Properties.AddProperty(serviceType, commandFilter);
                AddCommandFilter(textViewAdapter, commandFilter);
            }
        }

        private static void AddCommandFilter(IVsTextView viewAdapter, GoToDefinitionFilter commandFilter)
        {
            if (!commandFilter.IsAdded)
            {
                // Get the view adapter from the editor factory
                IOleCommandTarget next;
                int hr = viewAdapter.AddCommandFilter(commandFilter, out next);

                if (hr == VSConstants.S_OK)
                {
                    commandFilter.IsAdded = true;
                    // You'll need the next target for Exec and QueryStatus
                    if (next != null) commandFilter.NextTarget = next;
                }
            }
        }

        public void SubjectBuffersConnected(IWpfTextView textView, ConnectionReason reason, Collection<ITextBuffer> subjectBuffers)
        {
        }

        public void SubjectBuffersDisconnected(IWpfTextView textView, ConnectionReason reason, Collection<ITextBuffer> subjectBuffers)
        {
            if (reason != ConnectionReason.TextViewLifetime) return;

            GoToDefinitionFilter commandFilter;
            if (textView.Properties.TryGetProperty(serviceType, out commandFilter))
            {
                var textViewAdapter = editorFactory.GetViewAdapter(textView);
                int hr = textViewAdapter.RemoveCommandFilter(commandFilter);
                Debug.Assert(hr == VSConstants.S_OK, "Should be able to unwire the command.");
                (commandFilter as IDisposable).Dispose();
            }
        }

        public void Dispose()
        {
            this.solutionEvents.AfterClosing -= Cleanup;
        }
    }
}