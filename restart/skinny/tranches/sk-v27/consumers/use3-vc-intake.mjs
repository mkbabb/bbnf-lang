// LW1.USE-3: intake of the 66 unruled V-C css-color misses (value.js COHESION
// §0hy) as lane rows. Source: value.js `bench/records/W10close/gates/vc-1.txt.gz`
// at commit 5a3ba9707 (local tranche-u; the push is held by §0hz's push law).
// Each MISS is classed by a rule over its input, never by hand, and each class
// carries its lane and its unit. The cures are value.js's own (the programme
// never edits a consumer); the units observe them.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { execFileSync } from "node:child_process";
import { gunzipSync } from "node:zlib";
import { createHash } from "node:crypto";
import { homedir } from "node:os";
import { HERE, tsv, parseTsv } from "./lib.mjs";

const VJ = join(homedir(), "Programming/value.js");
const COMMIT = "5a3ba9707b544cd8dce7d93c2f3f86514193b30c";
const RECORD = "bench/records/W10close/gates/vc-1.txt.gz";

// first matching rule wins; order is most specific first
const CLASSES = [
    { id: "VC-VAR", test: (m) => /\bvar\(--/.test(m.input), lane: "USE", unit: "LW3.USE-4e", owner: "value.js X·P (RULING candidate: computed-value var() needs a cascade, which value.js does not model)", reason: "var() substitution at computed-value time" },
    { id: "VC-CTX", test: (m) => /\d(em|rem|vw|vh)\b/.test(m.input), lane: "USE", unit: "LW3.USE-4e", owner: "value.js X·P (RULING candidate: needs a font-size/viewport context)", reason: "context-dependent length inside a colour function" },
    { id: "VC-CONTRAST", test: (m) => /contrast-color\(/i.test(m.input), lane: "USE", unit: "LW3.USE-4a", owner: "value.js X·P (css-color-5 contrast-color() is absent from color.bbnf)", reason: "contrast-color() refused" },
    { id: "VC-REL-MATH", test: (m) => m.kind === "refused" && /\bfrom\b/.test(m.input) && /\b(sin|cos|tan|asin|acos|atan2?|pow|sqrt|hypot|log|exp|clamp|pi)\(?/.test(m.input), lane: "USE", unit: "LW3.USE-4c", owner: "value.js X·P (math functions over relative-colour channel keywords)", reason: "math function over a channel keyword refused" },
    { id: "VC-POWERLESS", test: (m) => m.kind === "serialization" && /^(ok)?lch\(from color\(/.test(m.input), lane: "USE", unit: "LW3.USE-4b", owner: "value.js X·P (powerless hue after conversion serializes as none)", reason: "powerless hue not carried as none" },
    { id: "VC-NONE", test: (m) => m.kind === "serialization" && /\bnone\b/.test(m.input), lane: "USE", unit: "LW3.USE-4b", owner: "value.js X·P (relative colour drops a `none` origin channel)", reason: "`none` channel lost in relative-colour serialization" },
];

function readRecord() {
    const gz = execFileSync("git", ["-C", VJ, "show", `${COMMIT}:${RECORD}`], { maxBuffer: 1 << 28 });
    const text = gunzipSync(gz).toString("utf8");
    return { text, sha256: createHash("sha256").update(gz).digest("hex") };
}

function intake() {
    const { text, sha256 } = readRecord();
    const rows = [];
    let n = 0;
    for (const line of text.split("\n")) {
        const m = line.match(/^MISS (\w+) (\S+) ([\w-]+): "(.*?)"(?: actual="(.*)")?$/);
        if (!m) continue;
        const miss = { kind: m[1], file: m[2], property: m[3], input: m[4], actual: m[5] ?? "" };
        const c = CLASSES.find((k) => k.test(miss));
        rows.push({ n: ++n, wpt_file: miss.file, property: miss.property, input: miss.input, miss: miss.kind, actual: miss.actual, class: c?.id ?? "UNCLASSED", lane: c?.lane ?? "", unit: c?.unit ?? "", owner: c?.owner ?? "", reason: c?.reason ?? "" });
    }
    const total = text.match(/^css-color\s+(?:\d+\s+){7}(\d+)$/m)?.[1];
    return { rows, sha256, total: Number(total) };
}

const HEADER = ["n", "wpt_file", "property", "input", "miss", "actual", "class", "lane", "unit", "owner", "reason"];
const OUT = join(HERE, "VC-INTAKE.tsv");
const { rows, sha256, total } = intake();
const text = `# source: value.js ${COMMIT.slice(0, 12)}:${RECORD} sha256 ${sha256}; css-color MISS column = ${total}\n` + tsv(HEADER, rows);

if (process.argv.includes("--check")) {
    let bad = 0;
    if (total !== 66) (console.log(`RED the record's css-color MISS column reads ${total}, not 66`), bad++);
    const onDisk = existsSync(OUT) ? parseTsv(readFileSync(OUT, "utf8").split("\n").slice(1).join("\n")) : [];
    if (onDisk.length !== total) (console.log(`RED VC-INTAKE.tsv holds ${onDisk.length} rows; the record has ${total} misses`), bad++);
    const unrouted = onDisk.filter((r) => !r.lane || !r.unit || r.class === "UNCLASSED");
    if (unrouted.length) (console.log(`RED ${unrouted.length} rows without a lane and a unit`), bad++);
    if (existsSync(OUT) && readFileSync(OUT, "utf8") !== text) (console.log("RED VC-INTAKE.tsv differs from its regeneration"), bad++);
    if (!bad) {
        const by = {};
        for (const r of onDisk) by[`${r.class}→${r.unit}`] = (by[`${r.class}→${r.unit}`] ?? 0) + 1;
        console.log(`GREEN VC-INTAKE.tsv: ${onDisk.length}/66 rows, each with a lane and a unit; ${Object.entries(by).map(([k, v]) => `${k} ${v}`).join(", ")}`);
    }
    process.exit(bad ? 1 : 0);
}
writeFileSync(OUT, text);
console.log(`wrote ${rows.length} rows; unclassed ${rows.filter((r) => r.class === "UNCLASSED").length}`);
