
module MyParsing

/// Get the "nested ranges" of source code structures, along with the minimum-number-of-indent spaces inside that span (ignoring comments and #commands).
/// Note: The 'filename' is only used e.g. to look at the filename extension (e.g. ".fs" versus ".fsi"), this does not try to load the file off disk.  
///       Instead, 'sourceCodeOfTheFile' should contain the entire file as a giant string.
val GetRanges : string * string // sourceCodeOfTheFile, filename
                   -> (int * int * int * int * int) []  // start line, column, end line, column, minIndent

/// Get non-overlapping ranges, where each range spans at most a single line, and has info about its "semantic depth".
/// Note: The 'filename' is only used e.g. to look at the filename extension (e.g. ".fs" versus ".fsi"), this does not try to load the file off disk.  
///       Instead, 'sourceCodeOfTheFile' should contain the entire file as a giant string.
val GetNonoverlappingDepthRanges : string *string // sourceCodeOfTheFile, filename
                                     -> (int * int * int * int) []  // line, start column, end column, depth