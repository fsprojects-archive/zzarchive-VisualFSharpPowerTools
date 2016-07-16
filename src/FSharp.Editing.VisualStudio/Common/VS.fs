namespace FSharp.Editing.VisualStudio

module VS =
  module Snapshot =
    open Microsoft.VisualStudio.Text
    let inline mkPoint position textSnapshot = SnapshotPoint(textSnapshot, position)
    let inline mkSpan (b: SnapshotPoint) (e: SnapshotPoint) = SnapshotSpan(b, e)
    let inline mkSpanFromPositions (b: int) (e: int) textSnapshot =
      let b = mkPoint b textSnapshot
      let e = mkPoint e textSnapshot
      mkSpan b e
