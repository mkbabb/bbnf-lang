// LW1.USE-1 driver: `node use1-census.mjs` writes CONSUMER-CENSUS.tsv and
// CONSUMER-CENSUS.summary.tsv; `--check` regenerates and compares byte-for-byte.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { census, CENSUS_HEADER } from "./census.mjs";
import { HERE, tsv, parseTsv, readHeads, show } from "./lib.mjs";

const { rows, entries } = census();
const counts = new Map();
for (const r of rows) {
    const k = `${r.repo}\t${r.scope}\t${r.kind}`;
    counts.set(k, (counts.get(k) ?? 0) + 1);
}
const summary =
    `# value.js parse entries (read at its pin): ${[...entries.parse].sort().join(" ")}\n` +
    `# value.js serialize entries: ${[...entries.serialize].sort().join(" ")}\n` +
    tsv(["repo", "scope", "kind", "count"], [...counts].sort(([a], [b]) => a.localeCompare(b)).map(([k, count]) => {
        const [repo, scope, kind] = k.split("\t");
        return { repo, scope, kind, count };
    }));
const out = { "CONSUMER-CENSUS.tsv": tsv(CENSUS_HEADER, rows), "CONSUMER-CENSUS.summary.tsv": summary };

// Oracle sites, each found by an independent `git grep` at the pin (2026-11-05),
// not by this scanner. `absent` rows are comment lines the scanner must not count.
const ORACLE = [
    ["fourier-analysis", "web/src/lib/colors.ts", 24, "IMPORT-VJ"],
    ["fourier-analysis", "web/src/lib/colors.ts", 93, "CALL-PARSE", "parseCssColor"],
    ["fourier-analysis", "web/src/lib/colors.ts", 23, "CALL-PARSE", "parseCssColor", "absent"],
    ["fourier-analysis", "web/src/lib/colors.ts", 86, "CALL-PARSE", "parseCssColor", "absent"],
    ["daraksha", "src/palette/derive.ts", 23, "CALL-PARSE", "parseCssColor"],
    ["glass-ui", "src/composables/color/value.ts", 42, "CALL-PARSE", "parseCssColor"],
    ["keyframes.js", "src/animation/compile/emit/serialize-value.ts", 2, "IMPORT-VJ"],
    ["value.js", "scripts/gen-grammar.mjs", 47, "REF-BBNF"],
    ["value.js", "src/css/bbnf/load.ts", 12, "GENERATED"],
    ["value.js", "src/css/bbnf/color-parse.ts", 13, "GENERATED"],
    ["value.js", "src/css/grammar/css.bbnf", 1, "GRAMMAR"],
    ["sci-report", "dashboards/sci/features/rainbow/RainbowStack.vue", 23, "IMPORT-VJ"],
];

if (process.argv.includes("--check")) {
    let bad = 0;
    const onDisk = existsSync(join(HERE, "CONSUMER-CENSUS.tsv")) ? parseTsv(readFileSync(join(HERE, "CONSUMER-CENSUS.tsv"), "utf8")) : [];
    for (const [repo, file, line, kind, binding, absent] of ORACLE) {
        const hit = onDisk.some((r) => r.repo === repo && r.file === file && Number(r.line) === line && r.kind === kind && (!binding || r.binding === binding));
        if (hit === Boolean(absent)) {
            console.log(`RED oracle ${repo}:${file}:${line} ${kind}${binding ? " " + binding : ""} ${absent ? "counted but is a comment" : "not in the census"}`);
            bad++;
        }
    }
    // every census row's cited line holds what the row says it holds
    const heads = new Map(readHeads().map((h) => [h.repo, h]));
    let untrue = 0;
    for (const r of onDisk) {
        if (!r.binding || !r.kind.startsWith("CALL-")) continue;
        const h = heads.get(r.repo);
        const text = show(h.path, h.sha, r.file).split("\n")[Number(r.line) - 1] ?? "";
        if (!text.includes(r.binding)) untrue++;
    }
    if (untrue) {
        console.log(`RED ${untrue} CALL rows whose cited line lacks the binding`);
        bad++;
    }
    if (!bad) console.log(`GREEN oracle ${ORACLE.length}/${ORACLE.length}; every CALL row's line holds its binding`);
    for (const [f, text] of Object.entries(out)) {
        const p = join(HERE, f);
        if (!existsSync(p)) {
            console.log(`RED ${f}: missing`);
            bad++;
        } else if (readFileSync(p, "utf8") !== text) {
            console.log(`RED ${f}: differs from the regeneration at HEADS.tsv`);
            bad++;
        } else console.log(`GREEN ${f}: ${text.split("\n").length - 2} rows, regenerated identically`);
    }
    process.exit(bad ? 1 : 0);
}
for (const [f, text] of Object.entries(out)) writeFileSync(join(HERE, f), text);
console.log(`wrote ${rows.length} census rows`);
