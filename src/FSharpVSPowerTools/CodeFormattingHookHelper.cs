﻿using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text;
using FSharpVSPowerTools.CodeFormatting;
using Microsoft.VisualStudio.Shell;

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [Name("F# Formatting Command Hook")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class CodeFormattingHookHelper : IWpfTextViewCreationListener
    {
        private readonly IVsEditorAdaptersFactoryService _adaptersFactory;
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;
        private readonly IEditorOperationsFactoryService _editorOperationsFactoryService;
        private readonly ITextBufferUndoManagerProvider _textBufferUndoManagerProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly IServiceProvider _serviceProvider;

        [ImportingConstructor]
        public CodeFormattingHookHelper(
            IVsEditorAdaptersFactoryService adaptersFactory, 
            IEditorOptionsFactoryService editorOptionsFactory,
            IEditorOperationsFactoryService editorOperationsFactoryService,
            ITextBufferUndoManagerProvider textBufferUndoManagerProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider)
        {
            _adaptersFactory = adaptersFactory;
            _editorOptionsFactory = editorOptionsFactory;
            _editorOperationsFactoryService = editorOperationsFactoryService;
            _textBufferUndoManagerProvider = textBufferUndoManagerProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _serviceProvider = serviceProvider;
        }

        internal StandardCommandDispatcher RegisterCommandDispatcher(IWpfTextView wpfTextView)
        {
            var view = _adaptersFactory.GetViewAdapter(wpfTextView);
            if (view != null)
            {
                return StandardCommandDispatcher.Register(view, wpfTextView, GetServices());
            }
            return null;
        }

        public void TextViewCreated(IWpfTextView wpfTextView)
        {
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke(new Action(() =>
            {
                RegisterCommandDispatcher(wpfTextView);
            }));
        }

        private CodeFormattingServices GetServices()
        {
            return new CodeFormattingServices(_editorOptionsFactory, _editorOperationsFactoryService, 
                            _textBufferUndoManagerProvider, _textDocumentFactoryService, _serviceProvider);
        }
    }
}
