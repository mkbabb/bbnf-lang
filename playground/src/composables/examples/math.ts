import type { Example } from "../useExamples";

export const mathExample: Example = {
    name: "Math",
    grammar: `number = /\\d+(\\.\\d+)?/ ?w ;
factor = number | "(" >> expr << ")" ?w ;
term = factor , (("*" | "/") ?w , factor) * ;
expr = term , (("+" | "-") ?w , term) * ;`,
    input: `2 + 3 * (4 - 1)`,
};
