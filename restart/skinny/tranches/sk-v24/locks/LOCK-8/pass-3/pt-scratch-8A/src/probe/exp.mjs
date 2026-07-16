import { all, string, regex, ParserState } from "./_core.bundle.mjs";
const A = all(string('('), regex(/\d+/), string(')'));
const fnA = A.parser;
console.error("FNA_NAME=", JSON.stringify(fnA.name));
for (let i=0;i<2e6;i++){ const st=new ParserState("(42)"); fnA(st); }
console.error("STATUS_A=", %GetOptimizationStatus(fnA));
