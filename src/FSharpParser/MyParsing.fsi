
module MyParsing

type XmlDocable =
    | XmlDocable of (*line:*) int * (*indent:*) int * (*paramNames:*) string list

val GetXmlDocables : sourceCodeOfTheFile:string * filename:string -> XmlDocable list
