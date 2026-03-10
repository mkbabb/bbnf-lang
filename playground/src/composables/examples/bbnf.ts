import type { Example } from "../useExamples";

export const bbnfExample: Example = {
    name: "BBNF",
    grammar: `// BBNF self-hosting grammar (simplified)
grammar = (rule | prettyDirective | importDirective)* ;
rule = identifier , "=" , expression , ";" ;
expression = alternation ;
alternation = sequence , ("|" , sequence)* ;
sequence = unary , ("," , unary)* ;
unary = atom , ("*" | "+" | "?")? ;
atom = identifier | stringLiteral | regexLiteral | "(" , expression , ")" ;
identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;
stringLiteral = /"[^"]*"/ ;
regexLiteral = /\\/[^\\/]+\\// ;
prettyDirective = "@pretty" , identifier , prettyArg* , ";" ;
prettyArg = identifier | stringLiteral ;
importDirective = "@import" , stringLiteral , ";" ;

@pretty grammar ;
@pretty rule sep(" = ") ;
@pretty alternation group sep(" | ") ;
@pretty sequence sep(", ") ;`,
    input: `grammar = rule* ;
rule = identifier , "=" , expression , ";" ;
expression = term , ("|" , term)* ;
term = factor , ("," , factor)* ;
factor = /[_a-zA-Z][_a-zA-Z0-9]*/ | /"[^"]*"/ ;`,
};
