import type { Demo } from "./types";

export const bbnfGrammarDemo: Demo = {
    id: "bbnf-grammar",
    title: "Write BBNF in BBNF",
    description: "Explore the self-hosting grammar — BBNF defined in its own notation.",
    steps: [
        {
            grammar: `// BBNF is self-describing — this grammar defines BBNF itself.
// Step 1: Start with identifiers and literals.

identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;

literal = "\\"" , /(\\\\.|[^"\\\\])*/ , "\\""
        | "'"  , /(\\\\.|[^'\\\\])*/ , "'"
        | "\`"  , /(\\\\.|[^\`\\\\])*/ , "\`" ;`,
            input: `greeting = "Hello" ;`,
            annotation: "BBNF is self-hosting — its syntax is defined in BBNF itself. We start with the two most basic building blocks: identifiers (rule names) and string literals.",
            highlightRegion: "grammar",
        },
        {
            grammar: `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;
literal = "\\"" , /(\\\\.|[^"\\\\])*/ , "\\"" ;
regex = "/" , /(\\\\.|[^\\/])+/ , "/" ;

// A term is any atomic expression
term = identifier | literal | regex
     | "(" , rhs , ")"
     | "[" , rhs , "]"
     | "{" , rhs , "}" ;`,
            annotation: "Terms are the atoms of BBNF: identifiers, literals, regex patterns, and grouped expressions. `[expr]` means optional, `{expr}` means repetition — these are EBNF shortcuts.",
            highlightRegion: "grammar",
        },
        {
            grammar: `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;
literal = "\\"" , /(\\\\.|[^"\\\\])*/ , "\\"" ;
regex = "/" , /(\\\\.|[^\\/])+/ , "/" ;

term = identifier | literal | regex
     | "(" , rhs , ")"
     | "[" , rhs , "]"
     | "{" , rhs , "}" ;

// Postfix quantifiers and operators
factor = term , ("?w" | "?" | "*" | "+") ? ;
binary_factor = factor , (("<<" | ">>" | "-") , factor) * ;
concatenation = (binary_factor , "," ?) + ;
alternation = (concatenation , "|" ?) + ;
rhs = alternation ;`,
            input: `value = object | array | string | number ;
array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;`,
            annotation: "Now we add the full operator hierarchy: quantifiers (`*`, `+`, `?`, `?w`), skip/next (`<<`, `>>`), minus (`-`), concatenation (`,`), and alternation (`|`). This mirrors the precedence table.",
            highlightRegion: "grammar",
        },
        {
            grammar: `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;
literal = "\\"" , /(\\\\.|[^"\\\\])*/ , "\\"" ;
regex = "/" , /(\\\\.|[^\\/])+/ , "/" ;

comment = "//" , /.*/ ;

term = identifier | literal | regex
     | "(" , rhs , ")"
     | "[" , rhs , "]"
     | "{" , rhs , "}" ;

factor = term , ("?w" | "?" | "*" | "+") ? ;
binary_factor = factor , (("<<" | ">>" | "-") , factor) * ;
concatenation = (binary_factor , "," ?) + ;
alternation = (concatenation , "|" ?) + ;
rhs = alternation ;

lhs = identifier ;
rule = lhs , "=" , rhs , (";" | ".") ;
grammar = (comment ? , rule , comment ?) * ;`,
            input: `// JSON Grammar
value = object | array | string | number ;
object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;
array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;
string = /"[^"]*"/ ;
number = /-?\\d+/ ;`,
            annotation: "The complete BBNF grammar: production rules (`lhs = rhs ;`), comments (`//`), and a top-level `grammar` rule that matches zero or more rules. This grammar can parse itself!",
            highlightRegion: "ast",
        },
    ],
};
