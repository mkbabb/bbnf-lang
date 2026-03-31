import type { Example } from "../useExamples";
import grammar from "@grammars/bbnf/bbnf.bbnf?raw";

export const bbnfExample: Example = {
    name: "BBNF",
    grammar,
    input: `grammar = rule* ;
rule = identifier , "=" , expression , ";" ;
expression = term , ("|" , term)* ;
term = factor , ("," , factor)* ;
factor = /[_a-zA-Z][_a-zA-Z0-9]*/ | /"[^"]*"/ ;`,
    entryRule: "grammar",
};
