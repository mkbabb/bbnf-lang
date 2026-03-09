import type { Example } from "../useExamples";

export const jsonExample: Example = {
    name: "JSON",
    grammar: `// JSON Grammar
value = object | array | string | number | "true" | "false" | "null";

@pretty value group;

object = "{" , members? , "}";
@pretty object group indent sep(", ");

members = member , ("," , member)*;
member = string , ":" , value;
@pretty member sep(": ");

array = "[" , elements? , "]";
@pretty array group indent sep(", ");

elements = value , ("," , value)*;

string = /"[^"]*"/;
number = /-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?/;`,
    input: `{"name": "BBNF", "version": 1, "items": [1, 2, 3], "nested": {"a": true, "b": null}}`,
};
