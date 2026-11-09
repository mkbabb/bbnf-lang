// LOCK-8 pass-3 8C PROTO-2 — JS RegExp oracle (column 1).
// Anchored leftmost-first match, the ECMAScript semantics the Rust DFA must mirror.
import { readFileSync } from "node:fs";
const url = new URL("./hazard-triples.json", import.meta.url);
const { triples } = JSON.parse(readFileSync(url, "utf8"));
console.log("hazard  pattern      input   JS_RegExp");
for (const t of triples) {
  const m = new RegExp("^(?:" + t.pattern + ")").exec(t.input);
  const got = m ? m[0] : null;
  const ok = got === t.js_expected ? "" : "  <<MISMATCH vs pinned expected";
  console.log(
    `${t.hazard}      ${t.pattern.padEnd(11)}  ${JSON.stringify(t.input).padEnd(6)}  ${JSON.stringify(got)}${ok}`,
  );
}
