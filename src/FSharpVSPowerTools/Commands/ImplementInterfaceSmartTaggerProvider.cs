using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.Refactoring;
using FSharpVSPowerTools.ProjectSystem;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell.Interop;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(ImplementInterfaceSmartTag))]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class ImplementInterfaceSmartTaggerProvider : IViewTaggerProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly ITextUndoHistoryRegistry _undoHistoryRegistry;
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;
       
        [ImportingConstructor]
        public ImplementInterfaceSmartTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            IEditorOptionsFactoryService editorOptionsFactory,
            ITextUndoHistoryRegistry undoHistoryRegistry,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _editorOptionsFactory = editorOptionsFactory;
            _undoHistoryRegistry = undoHistoryRegistry;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.InterfaceImplementationEnabled) return null;
            var codeGenOptions = Setting.getCodeGenerationOptions(_serviceProvider);
            if (codeGenOptions == null) return null;

            var dte = _serviceProvider.GetService(typeof(SDTE)) as EnvDTE.DTE;
            var vsVersion = VisualStudioVersionModule.fromDTEVersion(dte.Version);
            if (vsVersion >= VisualStudioVersion.VS2015) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                var implementInterface = 
                    new ImplementInterface(doc, textView,
                        _editorOptionsFactory, _undoHistoryRegistry.RegisterHistory(buffer),
                        _fsharpVsLanguageService, _serviceProvider, _projectFactory,
                        Setting.getInterfaceMemberIdentifier(codeGenOptions),
                        Setting.getDefaultMemberBody(codeGenOptions));
                return new ImplementInterfaceSmartTagger(buffer, implementInterface) as ITagger<T>;
            }

            return null;
        }
    }
}