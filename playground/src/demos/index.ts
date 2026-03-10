import { jsonParserDemo } from "./json-parser";
import { bbnfGrammarDemo } from "./bbnf-grammar";
import { errorRecoveryDemo } from "./error-recovery";
import { prettyPrintingDemo } from "./pretty-printing";
import type { Demo } from "./types";

export const demos: Demo[] = [jsonParserDemo, bbnfGrammarDemo, errorRecoveryDemo, prettyPrintingDemo];

export function getDemoById(id: string): Demo | undefined {
    return demos.find((d) => d.id === id);
}

export type { Demo, DemoStep } from "./types";
