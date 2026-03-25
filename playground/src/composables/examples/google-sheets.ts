import type { Example } from "../useExamples";
import grammar from "@grammars/lang/google-sheets.bbnf?raw";

export const googleSheetsExample: Example = {
    name: "Google Sheets",
    grammar,
    input: `=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))`,
    entryRule: "formula",
    description:
        "Google Sheets formula parser with `LET`, `LAMBDA`, `IF`, `MAKEARRAY`, and full operator precedence. Pretty-prints with paired LET bindings and proper nesting.",
    tags: ["@pretty", "recursive", "operators"],
};
