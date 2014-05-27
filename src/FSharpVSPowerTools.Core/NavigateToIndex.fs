namespace FSharpVSPowerTools.ProjectSystem.Navigation

open System
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
        member x.String = str
        member x.Offset = offset
        member x.Length = str.Length - offset
        member x.Item = item
        member x.IsOperator = isOperator
        member x.StartsWith (s: string) = 
            if s.Length > x.Length then 
                false
            else
                CultureInfo.CurrentCulture.CompareInfo.IndexOf(str, s, offset, s.Length, CompareOptions.IgnoreCase) = offset
        member private x.DebugString() = sprintf "%s (offset %d) (%s)" (str.Substring offset) offset str

    let private IndexEntryComparer =
        {
            new IComparer<IndexEntry> with
                member x.Compare(a, b) = 
                    let res = CultureInfo.CurrentCulture.CompareInfo.Compare(a.String, a.Offset, b.String, b.Offset, CompareOptions.IgnoreCase)
                    if res = 0 then a.Offset.CompareTo(b.Offset) else res
        }
        
    type IIndexedNavigableItems =
        abstract Find: searchValue: string * itemProcessor: (NavigableItem * string * bool * MatchKind-> unit) -> unit

    type Builder() =
        let entries = ResizeArray()

        member x.Add(items: seq<NavigableItem>) =
            for item in items do
                let isOperator, name = 
                    if PrettyNaming.IsMangledOpName item.Name then 
                        true, PrettyNaming.DecompileOpName item.Name 
                    else 
                        false, item.Name
                for i = 0 to name.Length - 1 do
                    entries.Add(IndexEntry(name, i, item, isOperator))

        member x.BuildIndex() =
            entries.Sort(IndexEntryComparer)
            {
                new IIndexedNavigableItems with
                    member x.Find(searchValue, processor) = 
                        let entryToFind = IndexEntry(searchValue, 0, Unchecked.defaultof<_>, Unchecked.defaultof<_>)
                        let initial = 
                            let p = entries.BinarySearch(entryToFind, IndexEntryComparer)
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
                        while pos >= 0  && entries.[pos].StartsWith searchValue do
                            handle pos
                            pos <- pos - 1

                        // value of 'initial' position was already handled on the previous step so here we'll bump it
                        let mutable pos = initial + 1
                        while pos < entries.Count && entries.[pos].StartsWith searchValue do
                            handle pos
                            pos <- pos + 1
            }