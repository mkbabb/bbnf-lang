import type { Demo } from "./types";

export const prettyPrintingDemo: Demo = {
    id: "pretty-printing",
    title: "Format with @pretty",
    description: "Build a grammar-driven formatter using @pretty directives.",
    steps: [
        {
            grammar: `// Simple list grammar
item = /[a-zA-Z]+/ ;
list = "[" , items? , "]" ;
items = item , ("," , item)* ;`,
            input: `[alpha, beta, gamma, delta, epsilon, zeta, eta, theta]`,
            annotation: "Start with a grammar that parses bracketed lists. Without `@pretty`, there's no formatting — the output just echoes the input.",
            highlightRegion: "format",
        },
        {
            grammar: `item = /[a-zA-Z]+/ ;
list = "[" , items? , "]" ;
items = item , ("," , item)* ;

@pretty list group ;`,
            annotation: "`group` wraps the list content so it stays on one line if it fits, or breaks as a unit if it doesn't. Try adjusting the Width slider in the controls bar.",
            highlightRegion: "format",
        },
        {
            grammar: `item = /[a-zA-Z]+/ ;
list = "[" , items? , "]" ;
items = item , ("," , item)* ;

@pretty list group indent ;`,
            annotation: "Adding `indent` increases the indentation level when the group breaks. Now items are indented inside the brackets.",
            highlightRegion: "format",
        },
        {
            grammar: `item = /[a-zA-Z]+/ ;
list = "[" , items? , "]" ;
items = item , ("," , item)* ;

@pretty list group indent sep(", ") ;`,
            annotation: "`sep(\", \")` controls the separator between repeated elements. When the group fits on one line, items are separated by `, `. When it breaks, each item gets its own line with `\",\"` (trailing space trimmed).",
            highlightRegion: "format",
        },
        {
            grammar: `// Nested list grammar
item = /[a-zA-Z]+/ ;
value = list | item ;
list = "[" , values? , "]" ;
values = value , ("," , value)* ;

@pretty list group indent sep(", ") ;
@pretty value group ;`,
            input: `[alpha, [beta, gamma], delta, [epsilon, [zeta, eta]], theta]`,
            annotation: "Formatting composes naturally with nesting. Each nested list gets its own group, and the pretty-printer decides independently whether each group should break.",
            highlightRegion: "format",
        },
    ],
};
