// LG-b baseline probe (sk-v27 LW1 lockstep gate): TS half. Uses the
// published @mkbabb/bbnf-lang package's BBNFToASTWithImports (grammars may
// carry @import). One line per file: path \t ACCEPT|REJECT [\t furthest offset].
import { readFileSync } from "node:fs";
import { BBNFToASTWithImports } from "@mkbabb/bbnf-lang";
for (const path of process.argv.slice(2)) {
  const src = readFileSync(path, "utf8");
  let line;
  try {
    const [parser, parsed] = BBNFToASTWithImports(src);
    if (parsed) line = "ACCEPT";
    else line = `REJECT\tTS no parse (state ${JSON.stringify(parser?.state?.offset ?? null)})`;
  } catch (e) {
    line = `REJECT\tthrow ${String(e).slice(0, 120).replace(/\s+/g, " ")}`;
  }
  console.log(`${path}\t${line}`);
}
