import type { Example } from "../useExamples";

export const mathExample: Example = {
    name: "Math",
    grammar: `// Math Expression Grammar
expr = term , (("+" | "-") , term)*;
term = factor , (("*" | "/") , factor)*;
factor = number | "(" , expr , ")";
number = /\\d+(\\.\\d+)?/;`,
    input: `2 + 3 * (4 - 1)`,
};
