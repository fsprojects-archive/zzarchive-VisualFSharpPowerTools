/// Basic operations on options.
[<CompilationRepresentation(enum<CompilationRepresentationFlags> (4))>]
module Microsoft.FSharp.Core.Option

/// Returns true if the option is not None.
val isSome : option:'T option -> bool
/// Returns true if the option is None.
val isNone : option:'T option -> bool
/// Gets the value associated with the option.
val get : option:'T option -> 'T
/// count inp evaluates to match inp with None -> 0 | Some _ -> 1.
val count : option:'T option -> int
/// fold f s inp evaluates to match inp with None -> s | Some x -> f s x.
val fold : folder:('State -> 'T -> 'State) -> state:'State -> option:'T option -> 'State
/// fold f inp s evaluates to match inp with None -> s | Some x -> f x s.
val foldBack : folder:('T -> 'State -> 'State) -> option:'T option -> state:'State -> 'State
/// exists p inp evaluates to match inp with None -> false | Some x -> p x.
val exists : predicate:('T -> bool) -> option:'T option -> bool
/// forall p inp evaluates to match inp with None -> true | Some x -> p x.
val forall : predicate:('T -> bool) -> option:'T option -> bool
/// iter f inp executes match inp with None -> () | Some x -> f x.
val iter : action:('T -> unit) -> option:'T option -> unit
/// map f inp evaluates to match inp with None -> None | Some x -> Some (f x).
val map : mapping:('T -> 'U) -> option:'T option -> 'U option
/// bind f inp evaluates to match inp with None -> None | Some x -> f x
val bind : binder:('T -> 'U option) -> option:'T option -> 'U option
/// filter f inp evaluates to match inp with None -> None | Some x -> if f x then Some x else None.
val filter : predicate:('T -> bool) -> option:'T option -> 'T option
/// Convert the option to an array of length 0 or 1.
val toArray : option:'T option -> 'T []
/// Convert the option to a list of length 0 or 1.
val toList : option:'T option -> 'T list
/// Convert the option to a Nullable value.
val toNullable : option:'T option -> System.Nullable<'T> when 'T : (new : unit -> 'T) and 'T : struct and 'T :> System.ValueType
/// Convert a Nullable value to an option.
val ofNullable : value:System.Nullable<'T> -> 'T option when 'T : (new : unit -> 'T) and 'T : struct and 'T :> System.ValueType
/// Convert a potentially null value to an option.
val ofObj : value:'T -> 'T option when 'T : null
/// Convert an option to a potentially null value.
val toObj : value:'T option -> 'T when 'T : null
