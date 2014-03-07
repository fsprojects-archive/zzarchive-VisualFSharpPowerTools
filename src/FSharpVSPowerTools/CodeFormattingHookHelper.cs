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

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [Name("F# Dummy Command Hook")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class CodeFormattingHookHelper : IWpfTextViewCreationListener
    {
        private readonly IVsEditorAdaptersFactoryService _adaptersFactory;
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;
        private readonly IEditorOperationsFactoryService _editorOperationsFactorySerivce;
        private readonly ITextBufferUndoManagerProvider _textBufferUndoManagerProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;

        [ImportingConstructor]
        public CodeFormattingHookHelper(
            IVsEditorAdaptersFactoryService adaptersFactory, 
            IEditorOptionsFactoryService editorOptionsFactory,
            IEditorOperationsFactoryService editorOperationsFactoryService,
            ITextBufferUndoManagerProvider textBufferUndoManagerProvider,
            ITextDocumentFactoryService textDocumentFactoryService)
        {
            _adaptersFactory = adaptersFactory;
            _editorOptionsFactory = editorOptionsFactory;
            _editorOperationsFactorySerivce = editorOperationsFactoryService;
            _textBufferUndoManagerProvider = textBufferUndoManagerProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
        }

        public void TextViewCreated(IWpfTextView wpfTextView)
        {
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke(new Action(() =>
            {
                var view = _adaptersFactory.GetViewAdapter(wpfTextView);
                if (view != null)
                {
                    StandardCommandDispatcher.Register(view, wpfTextView, GetServices());
                }
            }));
        }

        private CodeFormattingServices GetServices()
        {
            return new CodeFormattingServices(_editorOptionsFactory, _editorOperationsFactorySerivce, _textBufferUndoManagerProvider, _textDocumentFactoryService);
        }
    }
}
