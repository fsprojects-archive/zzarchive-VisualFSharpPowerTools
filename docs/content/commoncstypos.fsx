(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**

Auto-correct common C#-/JavaScript-developer typos in F#-files
-----------------------

The idea is to correct most common polyglot- / C#- / JavaScript-developer typing errors while they write F#:

 - Writing only ``"var "`` in the beginning of the line, it will be replaced to ``"let "``
 - Writing C# / ES6 (JS) type lambda ``"(... => "`` it will be replaced to ``"(fun ... ->"``, e.g. in ``".Select(x => x + 1)"`` (only once per code line, and not in commented code)
 - Writing only C#-style ``"using "`` in the beginning of the line, with no bracket, it will be replaced to "open "

*)