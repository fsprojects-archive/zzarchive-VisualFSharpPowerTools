using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Windows.Media;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Win32;
using FSharpVSPowerTools.DepthColorizer;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(DepthRegionTag))]
    public class DepthColorizerTaggerProvider : ITaggerProvider
    {
        readonly IServiceProvider _serviceProvider;
        readonly ITextDocumentFactoryService _textDocumentFactoryService;
        readonly ProjectFactory _projectFactory;
        readonly VSLanguageService _vsLanguageService;
        readonly IOpenDocumentsTracker _openDocumentTracker;

        [ImportingConstructor]
        public DepthColorizerTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService,
            IOpenDocumentsTracker openDocumentTracker)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
            _openDocumentTracker = openDocumentTracker;
        }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            ITextDocument doc;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.DepthColorizerEnabled) return null;

            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return new DepthTagger( doc, buffer, _serviceProvider,_projectFactory, 
                    _vsLanguageService, _openDocumentTracker) as ITagger<T>;
            }

            return null;
        }
    }

    [Export(typeof(IWpfTextViewCreationListener))]
    [Export(typeof(IWpfTextViewConnectionListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Structured)]
    public class DepthColorizerAdornmentManager : IWpfTextViewCreationListener, IWpfTextViewConnectionListener
    {
        [Export]
        [Name(Constants.depthAdornmentLayerName)]
        [Order(Before = PredefinedAdornmentLayers.CurrentLineHighlighter)]
        internal AdornmentLayerDefinition AdornmentLayerDefinition { get; set; }

        private readonly ShellEventListener _shellEventListener;
        private readonly ThemeManager _themeManager;
        private readonly IViewTagAggregatorFactoryService _viewTagAggregatorFactoryService;
        private readonly IServiceProvider _serviceProvider;
        
        private static readonly Type serviceType = typeof(DepthColorizerAdornmentManager);

        [ImportingConstructor]
        public DepthColorizerAdornmentManager(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ShellEventListener shellEventListener,
            ThemeManager themeManager,
            IViewTagAggregatorFactoryService viewTagAggregatorFactoryService)
        {
            _serviceProvider = serviceProvider;
            _themeManager = themeManager;
            _viewTagAggregatorFactoryService = viewTagAggregatorFactoryService;
            _shellEventListener = shellEventListener;
        }

        public void TextViewCreated(IWpfTextView textView)
        {
            if (textView == null) return;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.DepthColorizerEnabled) return;
            
            var tagAggregator = _viewTagAggregatorFactoryService.CreateTagAggregator<DepthRegionTag>(textView);
            var adornment = new DepthColorizerAdornment(textView, tagAggregator, _themeManager, _shellEventListener);
            textView.Properties.AddProperty(serviceType, adornment);
        }

        public void SubjectBuffersConnected(IWpfTextView textView, ConnectionReason reason, System.Collections.ObjectModel.Collection<ITextBuffer> subjectBuffers)
        {
        }

        public void SubjectBuffersDisconnected(IWpfTextView textView, ConnectionReason reason, System.Collections.ObjectModel.Collection<ITextBuffer> subjectBuffers)
        {
            if (reason != ConnectionReason.TextViewLifetime) return;

            IDisposable adornment;
            
            if (textView.Properties.TryGetProperty(serviceType, out adornment))
            {
                bool success = textView.Properties.RemoveProperty(serviceType);
                Debug.Assert(success, "Should be able to remove adornment from the text view.");
                adornment.Dispose();
            }
        }
    }
}
