import type { Example } from "../useExamples";

export const jsonExample: Example = {
    name: "JSON",
    grammar: `// JSON Grammar
null = "null" ;
bool = "true" | "false" ;
number = /-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?/ ;

comma = "," ?w ;
colon = ":" ?w ;

string = /"(?:[^"\\\\]|\\\\(?:["\\\\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;

array = "[" >> (( value << comma ? ) *)?w << "]" ;
@pretty array group indent sep(", ");

pair = string, colon >> value ;
@pretty pair sep(": ");

object = "{" >> (( pair << comma ? ) *)?w << "}" ;
@pretty object group indent sep(", ");

value = object | array | string | number | bool | null ;
@pretty value group;`,
    input: `{"name": "BBNF", "version": 1, "items": [1, 2, 3], "nested": {"a": true, "b": null}}`,
};
