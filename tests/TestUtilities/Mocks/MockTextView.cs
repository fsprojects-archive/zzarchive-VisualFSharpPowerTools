/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the Apache License, Version 2.0, please send an email to 
 * vspython@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Projection;
using Microsoft.VisualStudio.Utilities;
using System.Linq;
using Microsoft.VisualStudio.Text.Formatting;
using System.Collections.Generic;

namespace TestUtilities.Mocks {

    public class MockViewScroller : IViewScroller
    {
        public void EnsureSpanVisible(VirtualSnapshotSpan span, EnsureSpanVisibleOptions options)
        {
            throw new NotImplementedException();
        }

        public void EnsureSpanVisible(SnapshotSpan span, EnsureSpanVisibleOptions options)
        {
            throw new NotImplementedException();
        }

        public void EnsureSpanVisible(SnapshotSpan span)
        {
            throw new NotImplementedException();
        }

        public void ScrollViewportHorizontallyByPixels(double distanceToScroll)
        {
            throw new NotImplementedException();
        }

        public void ScrollViewportVerticallyByLine(ScrollDirection direction)
        {
            Console.WriteLine("ScrollViewportVerticallyByLines");
        }

        public void ScrollViewportVerticallyByLines(ScrollDirection direction, int count)
        {
            Console.WriteLine("ScrollViewportVerticallyByLines");
        }

        public bool ScrollViewportVerticallyByPage(ScrollDirection direction)
        {
            throw new NotImplementedException();
        }

        public void ScrollViewportVerticallyByPixels(double distanceToScroll)
        {
            throw new NotImplementedException();
        }
    }


    public class MockTextViewLines : IWpfTextViewLineCollection
    {
        private IEnumerable<ITextViewLine> _lines;
        public MockTextViewLines(IEnumerable<ITextViewLine> lines)
        {
            _lines = lines;
        }
        public IWpfTextViewLine FirstVisibleLine
        {
            get { throw new NotImplementedException(); }
        }

        public System.Windows.Media.Geometry GetLineMarkerGeometry(SnapshotSpan bufferSpan, bool clipToViewport, System.Windows.Thickness padding)
        {
            throw new NotImplementedException();
        }

        public System.Windows.Media.Geometry GetLineMarkerGeometry(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public System.Windows.Media.Geometry GetMarkerGeometry(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public System.Windows.Media.Geometry GetMarkerGeometry(SnapshotSpan bufferSpan, bool clipToViewport, System.Windows.Thickness padding)
        {
            throw new NotImplementedException();
        }

        public System.Windows.Media.Geometry GetTextMarkerGeometry(SnapshotSpan bufferSpan, bool clipToViewport, System.Windows.Thickness padding)
        {
            throw new NotImplementedException();
        }

        public System.Windows.Media.Geometry GetTextMarkerGeometry(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public IWpfTextViewLine GetTextViewLineContainingBufferPosition(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public IWpfTextViewLine LastVisibleLine
        {
            get { throw new NotImplementedException(); }
        }

        public System.Collections.ObjectModel.ReadOnlyCollection<IWpfTextViewLine> WpfTextViewLines
        {
            get { throw new NotImplementedException(); }
        }

        public IWpfTextViewLine this[int index]
        {
            get { throw new NotImplementedException(); }
        }

        public bool ContainsBufferPosition(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        ITextViewLine ITextViewLineCollection.FirstVisibleLine
        {
            get { throw new NotImplementedException(); }
        }

        public SnapshotSpan FormattedSpan
        {
            get { throw new NotImplementedException(); }
        }

        public TextBounds GetCharacterBounds(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public int GetIndexOfTextLine(ITextViewLine textLine)
        {
            throw new NotImplementedException();
        }

        public System.Collections.ObjectModel.Collection<TextBounds> GetNormalizedTextBounds(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetTextElementSpan(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        ITextViewLine ITextViewLineCollection.GetTextViewLineContainingBufferPosition(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public ITextViewLine GetTextViewLineContainingYCoordinate(double y)
        {
            throw new NotImplementedException();
        }

        public System.Collections.ObjectModel.Collection<ITextViewLine> GetTextViewLinesIntersectingSpan(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public bool IntersectsBufferSpan(SnapshotSpan bufferSpan)
        {
            throw new NotImplementedException();
        }

        public bool IsValid
        {
            get { throw new NotImplementedException(); }
        }

        ITextViewLine ITextViewLineCollection.LastVisibleLine
        {
            get { throw new NotImplementedException(); }
        }

        public int IndexOf(ITextViewLine item)
        {
            throw new NotImplementedException();
        }

        public void Insert(int index, ITextViewLine item)
        {
            throw new NotImplementedException();
        }

        public void RemoveAt(int index)
        {
            throw new NotImplementedException();
        }

        ITextViewLine System.Collections.Generic.IList<ITextViewLine>.this[int index]
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public void Add(ITextViewLine item)
        {
            throw new NotImplementedException();
        }

        public void Clear()
        {
            throw new NotImplementedException();
        }

        public bool Contains(ITextViewLine item)
        {
            throw new NotImplementedException();
        }

        public void CopyTo(ITextViewLine[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        public int Count
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        public bool Remove(ITextViewLine item)
        {
            throw new NotImplementedException();
        }

        public System.Collections.Generic.IEnumerator<ITextViewLine> GetEnumerator()
        {
            return _lines.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            throw new NotImplementedException();
        }
    }

    public class MockTextView : IWpfTextView, ITextView {
        private readonly ITextBuffer _buffer;
        private readonly PropertyCollection _props = new PropertyCollection();
        private readonly MockTextSelection _selection;
        private readonly MockTextCaret _caret;
        private readonly MockBufferGraph _bufferGraph;

        public MockTextView(ITextBuffer buffer) {
            _buffer = buffer;
            _selection = new MockTextSelection(this);
            _bufferGraph = new MockBufferGraph(this);
            _caret = new MockTextCaret(this);
        }

        public MockBufferGraph BufferGraph {
            get {
                return _bufferGraph;
            }
        }

        IBufferGraph ITextView.BufferGraph {
            get { return _bufferGraph; }
        }

        public ITextCaret Caret {
            get { return _caret;  }
        }

        public void Close() {
            throw new NotImplementedException();
        }

        public event EventHandler Closed {
            add {  }
            remove {  }
        }

        public void DisplayTextLineContainingBufferPosition(Microsoft.VisualStudio.Text.SnapshotPoint bufferPosition, double verticalDistance, ViewRelativePosition relativeTo, double? viewportWidthOverride, double? viewportHeightOverride) {
            throw new NotImplementedException();
        }

        public void DisplayTextLineContainingBufferPosition(Microsoft.VisualStudio.Text.SnapshotPoint bufferPosition, double verticalDistance, ViewRelativePosition relativeTo) {
            throw new NotImplementedException();
        }

        public Microsoft.VisualStudio.Text.SnapshotSpan GetTextElementSpan(Microsoft.VisualStudio.Text.SnapshotPoint point) {
            throw new NotImplementedException();
        }

        public Microsoft.VisualStudio.Text.Formatting.ITextViewLine GetTextViewLineContainingBufferPosition(Microsoft.VisualStudio.Text.SnapshotPoint bufferPosition) {
            throw new NotImplementedException();
        }

        public event EventHandler GotAggregateFocus {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public bool HasAggregateFocus {
            get { throw new NotImplementedException(); }
        }

        public bool InLayout {
            get { throw new NotImplementedException(); }
        }

        public bool IsClosed {
            get { throw new NotImplementedException(); }
        }

        public bool IsMouseOverViewOrAdornments {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler<TextViewLayoutChangedEventArgs> LayoutChanged {
            add { }
            remove { }
        }

        public double LineHeight {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler LostAggregateFocus {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public double MaxTextRightCoordinate {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler<MouseHoverEventArgs> MouseHover {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public IEditorOptions Options {
            get { return new MockTextOptions(); }
        }

        public Microsoft.VisualStudio.Text.ITrackingSpan ProvisionalTextHighlight {
            get {
                throw new NotImplementedException();
            }
            set {
                throw new NotImplementedException();
            }
        }

        public void QueueSpaceReservationStackRefresh() {
            throw new NotImplementedException();
        }

        public ITextViewRoleSet Roles {
            get { throw new NotImplementedException(); }
        }

        public ITextSelection Selection {
            get { return _selection; }
        }

        public ITextBuffer TextBuffer {
            get { return _buffer; }
        }

        public Microsoft.VisualStudio.Text.ITextDataModel TextDataModel {
            get { throw new NotImplementedException(); }
        }

        public Microsoft.VisualStudio.Text.ITextSnapshot TextSnapshot {
            get { return _buffer.CurrentSnapshot; }
        }

        public ITextViewLineCollection TextViewLines {
            get { return new MockTextViewLines(Enumerable.Empty<ITextViewLine>()); }
        }

        public ITextViewModel TextViewModel {
            get { throw new NotImplementedException(); }
        }

        public IViewScroller ViewScroller {
            get { return new MockViewScroller(); }
        }

        public double ViewportBottom {
            get { throw new NotImplementedException(); }
        }

        public double ViewportHeight {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler ViewportHeightChanged {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public double ViewportLeft {
            get {
                throw new NotImplementedException();
            }
            set {
                throw new NotImplementedException();
            }
        }

        public event EventHandler ViewportLeftChanged {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public double ViewportRight {
            get { throw new NotImplementedException(); }
        }

        public double ViewportTop {
            get { throw new NotImplementedException(); }
        }

        public double ViewportWidth {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler ViewportWidthChanged {
            add { throw new NotImplementedException(); }
            remove { throw new NotImplementedException(); }
        }

        public Microsoft.VisualStudio.Text.ITextSnapshot VisualSnapshot {
            get { throw new NotImplementedException(); }
        }

        public Microsoft.VisualStudio.Utilities.PropertyCollection Properties {
            get { return _props; }
        }

        #region IWpfTextView Members

        public System.Windows.Media.Brush Background {
            get {
                throw new NotImplementedException();
            }
            set {
                throw new NotImplementedException();
            }
        }

        public event EventHandler<BackgroundBrushChangedEventArgs> BackgroundBrushChanged {
            add { }
            remove { }
        }

        public Microsoft.VisualStudio.Text.Formatting.IFormattedLineSource FormattedLineSource {
            get { throw new NotImplementedException(); }
        }

        public IAdornmentLayer GetAdornmentLayer(string name) {
            throw new NotImplementedException();
        }

        public ISpaceReservationManager GetSpaceReservationManager(string name) {
            throw new NotImplementedException();
        }

        Microsoft.VisualStudio.Text.Formatting.IWpfTextViewLine IWpfTextView.GetTextViewLineContainingBufferPosition(SnapshotPoint bufferPosition) {
            throw new NotImplementedException();
        }

        public Microsoft.VisualStudio.Text.Formatting.ILineTransformSource LineTransformSource {
            get { throw new NotImplementedException(); }
        }

        IWpfTextViewLineCollection IWpfTextView.TextViewLines {
            get { return new MockTextViewLines(Enumerable.Empty<ITextViewLine>()); }
        }

        public System.Windows.FrameworkElement VisualElement {
            get { throw new NotImplementedException(); }
        }

        public double ZoomLevel {
            get {
                throw new NotImplementedException();
            }
            set {
                throw new NotImplementedException();
            }
        }

        public event EventHandler<ZoomLevelChangedEventArgs> ZoomLevelChanged {
            add { }
            remove { }
        }

        #endregion
    }
}
