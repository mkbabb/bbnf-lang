import type { Example } from "../useExamples";

export const mathExample: Example = {
    name: "Math",
    grammar: `// Math Expression Grammar — operator precedence via recursive descent
expr = term , (("+" | "-") ?w , term) * ;
term = factor , (("*" | "/") ?w , factor) * ;
factor = number | "(" >> expr << ")" ?w ;
number = /\\d+(\\.\\d+)?/ ?w ;`,
    input: `2 + 3 * (4 - 1)`,
};
