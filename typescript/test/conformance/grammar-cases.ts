// SERVED MODEL: claude-opus-5-5
import { readFileSync } from "node:fs";

/** JSON has no `undefined`: a positional hole is written `{ "$undefined": true }`. */
export const encode = (v: unknown): unknown =>
    v === undefined
        ? { $undefined: true }
        : Array.isArray(v)
          ? v.map(encode)
          : v && typeof v === "object"
            ? Object.fromEntries(Object.entries(v).map(([k, x]) => [k, encode(x)]))
            : v;

const fixture = (p: string) => readFileSync(new URL(`../fixtures/${p}`, import.meta.url), "utf8");

/** bbnf-lang's own grammars, each on inputs it accepts and at least one it refuses. */
export const CASES: { grammar: string; rule: string; input: string }[] = [
    ...[fixture("data/json/data.json"), '{"a": [1, 2.5e3, true, null, "x\\n"]}', "[]", "[1,]"].map((input) => ({ grammar: "lang/json.bbnf", rule: "value", input })),
    ...["1 + 2 * 3", "(1+2)*3", "2 ^ 3 ^ 2", "1 +"].map((input) => ({ grammar: "lang/math.bbnf", rule: "expr", input })),
    ...['a,b,"c,d"\n1,2,3\n', "x\n", '"unterminated'].map((input) => ({ grammar: "lang/csv.bbnf", rule: "csv", input })),
    ...[String.raw`/^(?:a|b)+[\d\w]{2,3}$/gi`, String.raw`/(?<=x)y/`, "/(/"].map((input) => ({ grammar: "lang/regex.bbnf", rule: "regex", input })),
    ...["rgb(1 2 3 / 50%)", "#fff", "hsl(120deg 50% 50%)", "color-mix(in oklch, red, blue)", "RGB(1 2 3)", "rgb("].map((input) => ({ grammar: "css/css-color.bbnf", rule: "color", input })),
    ...["10px", "1.5e2deg", "50%", "3", "10 px"].map((input) => ({ grammar: "css/css-value-unit.bbnf", rule: "valueUnit", input })),
    ...["@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }", "@keyframes x { 50% { opacity: .5 } }", "@keyframes { }"].map((input) => ({ grammar: "css/css-keyframes.bbnf", rule: "KEYFRAMES_RULE", input })),
    ...["div.a > p#b:hover, ul li:nth-child(2n+1)", "a[href^='x' i]::before", ">"].map((input) => ({ grammar: "css/css-selectors.bbnf", rule: "selectorList", input })),
    ...['=SUM(A1:B2, 3) * 2 & "x"', "=IF(A1>0, TRUE, #N/A)", "=("].map((input) => ({ grammar: "lang/google-sheets.bbnf", rule: "formula", input })),
    ...[fixture("grammar/lang/bbnf.bbnf")].map((input) => ({ grammar: "lang/bbnf.bbnf", rule: "grammar", input })),
];
