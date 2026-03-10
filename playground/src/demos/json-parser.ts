import type { Demo } from "./types";

export const jsonParserDemo: Demo = {
    id: "json-parser",
    title: "Build a JSON parser",
    description: "Define a JSON grammar from scratch, step by step.",
    steps: [
        {
            grammar: `// Step 1: Start with simple values
string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;
value = string | number | "true" | "false" | "null" ;`,
            input: `"hello"`,
            annotation: "We start with the simplest JSON values: strings, numbers, and the literals `true`, `false`, `null`. The `value` rule uses alternation (`|`) to try each option.",
            highlightRegion: "grammar",
        },
        {
            input: `42`,
            annotation: "Try parsing a number. The regex `/- ?\\d+(\\.\\d+)?/` matches integers and decimals. Change the input to test different values.",
            highlightRegion: "input",
        },
        {
            grammar: `string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;

array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;

value = array | string | number | "true" | "false" | "null" ;`,
            input: `[1, 2, "three"]`,
            annotation: "Now we add arrays. The `elements` rule uses concatenation (`,`) and repetition (`*`) to match comma-separated values. The `?` makes elements optional for empty arrays.",
            highlightRegion: "grammar",
        },
        {
            grammar: `string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;

array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;

object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;

value = object | array | string | number | "true" | "false" | "null" ;`,
            input: `{"name": "BBNF", "version": 1}`,
            annotation: "Objects follow the same pattern: key-value pairs separated by commas. Note that `value` references `object` which references `value` — BBNF handles recursive grammars naturally.",
            highlightRegion: "grammar",
        },
        {
            input: `{"name": "BBNF", "items": [1, 2, 3], "nested": {"a": true}}`,
            annotation: "The parser handles arbitrarily nested structures. Check the AST tab to see the parse tree.",
            highlightRegion: "ast",
        },
        {
            grammar: `string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;

array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;

object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;

value = object | array | string | number | "true" | "false" | "null" ;

@pretty value group ;
@pretty object group indent sep(", ") ;
@pretty array group indent sep(", ") ;
@pretty member sep(": ") ;`,
            annotation: "Finally, add `@pretty` directives to format the output. `group indent` creates indented blocks, `sep(\", \")` controls separators. Switch to the Formatted tab to see the result.",
            highlightRegion: "format",
        },
    ],
};
