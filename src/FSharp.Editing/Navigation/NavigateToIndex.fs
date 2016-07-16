namespace FSharp.Editing

open System.Collections.Generic
open System.Globalization
open Microsoft.FSharp.Compiler

module Index =
    type MatchKind = 
        | Exact = 0
        | Prefix = 1
        | Substring = 2
        | Regular = 3
        | None = 4

    [<System.Diagnostics.DebuggerDisplay("{DebugString()}")>]
    type private IndexEntry(str: string, offset: int, item: NavigableItem, isOperator: bool) =
        member __.String = str
        member __.Offset = offset
        member __.Length = str.Length - offset
        member __.Item = item
        member __.IsOperator = isOperator
        member x.StartsWith (s: string) = 
            if s.Length > x.Length then 
                false
            else
                CultureInfo.CurrentCulture.CompareInfo.IndexOf(str, s, offset, s.Length, CompareOptions.IgnoreCase) = offset
        member private __.DebugString() = sprintf "%s (offset %d) (%s)" (str.Substring offset) offset str

    let private indexEntryComparer =
        {
            new IComparer<IndexEntry> with
                member __.Compare(a, b) = 
                    let res = CultureInfo.CurrentCulture.CompareInfo.Compare(a.String, a.Offset, b.String, b.Offset, CompareOptions.IgnoreCase)
                    if res = 0 then a.Offset.CompareTo(b.Offset) else res
        }

    type NavigableItemProcessor = NavigableItem * string * bool * MatchKind -> unit
        
    type IIndexedNavigableItems =
        abstract Find: searchValue: string * itemProcessor: NavigableItemProcessor -> unit

    type Builder() =
        let entries = ResizeArray()

        member __.Add(items: seq<NavigableItem>) =
            for item in items do
                let isOperator, name = 
                    if PrettyNaming.IsMangledOpName item.Name then 
                        true, PrettyNaming.DecompileOpName item.Name 
                    else 
                        false, item.Name
                for i = 0 to name.Length - 1 do
                    entries.Add(IndexEntry(name, i, item, isOperator))

        member __.BuildIndex() =
            entries.Sort(indexEntryComparer)
            {
                new IIndexedNavigableItems with
                    member __.Find(searchValue, processor) =
                        if entries.Count > 0 then 
                            let entryToFind = IndexEntry(searchValue, 0, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
                            let initial = 
                                let p = entries.BinarySearch(entryToFind, indexEntryComparer)
                                if p < 0 then ~~~p else p
                            let handle index = 
                                let entry = entries.[index]
                                let matchKind = 
                                    if entry.Offset = 0 then
                                        if entry.Length = searchValue.Length then MatchKind.Exact
                                        else MatchKind.Prefix
                                    else MatchKind.Substring
                                processor(entry.Item, entry.String, entry.IsOperator, matchKind)
                        
                            // in case if there are multiple matching items binary search might return not the first one.
                            // in this case we'll walk backwards searching for the applicable answers
                            let mutable pos = initial
                            while pos >= 0 && pos < entries.Count && entries.[pos].StartsWith searchValue do
                                handle pos
                                pos <- pos - 1

                            // value of 'initial' position was already handled on the previous step so here we'll bump it
                            let mutable pos = initial + 1
                            while pos < entries.Count && entries.[pos].StartsWith searchValue do
                                handle pos
                                pos <- pos + 1
            }