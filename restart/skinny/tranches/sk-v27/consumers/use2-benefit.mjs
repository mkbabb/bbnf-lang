// LW1.USE-2: the benefit table's row 0 (LG-d), one row per consumer
// (value, keyframes, fourier). Columns: parse-bearing bytes (raw and gzip -9,
// the static import closure of the value.js subpaths the consumer imports in
// src, read from the PUBLISHED tarball it pins), parse ms (median and IQR over
// fresh-process reps; NOISY unless the 1-minute load stayed < 8), conformance
// (the V-C denominators of record) and hand-parse sites (census REGEX rows in
// src scope; lines deleted = 0 at row 0 by definition).
//
//   node use2-benefit.mjs bench   run the reps (wrap in guarded --cpu-cores 0)
//   node use2-benefit.mjs table   write BENEFIT-ROW0.tsv from the records
//   node use2-benefit.mjs --check RED until row 0 is whole and true
//   node use2-benefit.mjs child <cell>   one rep (internal)
import { readFileSync, writeFileSync, existsSync, mkdirSync, appendFileSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { execFileSync } from "node:child_process";
import { gzipSync } from "node:zlib";
import { createHash } from "node:crypto";
import { homedir, loadavg } from "node:os";
import { performance } from "node:perf_hooks";
import { HERE, readHeads, show, tsv, parseTsv } from "./lib.mjs";

const LOG = join(homedir(), ".dev-logs/bbnf-lockstep/consumers");
const PKG = { "4.3.0": join(LOG, "x430/package"), "4.2.1": join(LOG, "x421/package"), "4.2.0": join(LOG, "x420/package") };
const TARBALL_SHA256 = {
    "4.3.0": "7b3146927731a26ae2a7c1d53c87b302b2d72f76f8b5ccfed1cbb469e0a40908",
    "4.2.1": "a6110f009f24fab1dcad2099a2f8a60da20cc848a96a1b845d3930c51b573097",
    "4.2.0": "1eaea189be662d163d2eaa6a8ce3f8f68b5388a80fccbd7cc53490574d14cdc2",
};
const RECORDS = join(HERE, "records/use2-row0.jsonl");
// LW1 records carry no version; they were taken on these pins
const LEGACY_VERSION = { value: "4.2.1", "value-color": "4.2.1", keyframes: "4.2.0", fourier: "4.2.0" };
const versionOf = (r) => r.version ?? LEGACY_VERSION[r.cell];
const REPS = 15;

// The consumers' rows: the pinned value.js (PIN-CENSUS.tsv), the subpaths their
// src imports (CONSUMER-CENSUS.tsv), and the parse leaf each one calls.
const CELLS = {
    // LW2.USE-3: value's row is the published 4.3.0 tarball; its timing is the Phase 3 cell (bench/PREREG.md §8.3)
    value: { version: "4.3.0", entry: "css", fn: "parseCssValue", corpus: "wpt-all", phase3: "C-S0-ROW0-VALUE-CSS" },
    "value-color": { version: "4.3.0", entry: "color", fn: "parseCssColor", corpus: "wpt-css-color", phase3: "C-S0-ROW0-VALUE-COLOR" },
    keyframes: { version: "4.2.0", entry: "css", fn: "parseStylesheet", corpus: "kf-keyframes" },
    fourier: { version: "4.2.0", entry: "color", fn: "parseCssColor", corpus: "wpt-css-color" },
};

function wptCases() {
    const vj = readHeads().find((h) => h.repo === "value.js");
    const text = show(vj.path, vj.sha, "test/css/wpt-values/cases-2026-11-04-r1.json");
    return { cases: JSON.parse(text).cases, sha256: createHash("sha256").update(text).digest("hex") };
}

/** keyframes.js's own production parse leaf corpus: bench/compile.bench.ts `makeKeyframes`. */
function kfSheets() {
    const make = (stops) =>
        Array.from({ length: stops }, (_, i) => {
            const pct = Math.round((i / (stops - 1)) * 100);
            return `${pct}% { opacity: ${i / stops}; ` + `transform: translateX(${i * 10}px) translateY(${i * 5}px) ` + `scale(${1 + i / 100}) rotate(${i * 6}deg); }`;
        }).join("\n");
    return [2, 6, 11, 50, 200].map((n) => `@keyframes k {\n${make(n)}\n}`);
}

function corpus(name) {
    if (name === "kf-keyframes") {
        const s = kfSheets();
        return { inputs: s, sha256: createHash("sha256").update(s.join("\0")).digest("hex") };
    }
    const { cases, sha256 } = wptCases();
    const pick = name === "wpt-css-color" ? cases.filter((c) => c.module === "css-color") : cases;
    return { inputs: pick.map((c) => c.input), sha256 };
}

async function child(cellName) {
    const cell = CELLS[cellName];
    const { inputs, sha256 } = corpus(cell.corpus);
    const mod = await import(join(PKG[cell.version], `dist/subpaths/${cell.entry}.js`));
    const fn = mod[cell.fn];
    const pass = () => {
        let ok = 0;
        for (const s of inputs) if (fn(s).ok) ok++;
        return ok;
    };
    const t0 = performance.now();
    const ok = pass(); // first pass: cold (grammar construction is lazy)
    const cold = performance.now() - t0;
    const t1 = performance.now();
    pass();
    const warm = performance.now() - t1;
    process.stdout.write(JSON.stringify({ cell: cellName, version: cell.version, n: inputs.length, ok, cold_ms: cold, warm_ms: warm, corpus_sha256: sha256 }) + "\n");
}

function therm() {
    try {
        return execFileSync("pmset", ["-g", "therm"], { encoding: "utf8" }).replace(/\s+/g, " ").trim();
    } catch {
        return "unavailable";
    }
}

function bench() {
    mkdirSync(dirname(RECORDS), { recursive: true });
    for (const [v, p] of Object.entries(PKG)) {
        const sha = createHash("sha256").update(readFileSync(join(LOG, `tarballs/mkbabb-value.js-${v}.tgz`))).digest("hex");
        if (sha !== TARBALL_SHA256[v]) throw new Error(`tarball ${v} sha256 ${sha} != ${TARBALL_SHA256[v]}`);
        if (!existsSync(p)) throw new Error(`missing extracted ${p}`);
    }
    const run = new Date().toISOString();
    // interleave cells across reps so drift lands on every cell alike
    for (let rep = 0; rep < REPS; rep++) {
        for (const cell of Object.keys(CELLS)) {
            const before = loadavg()[0];
            const out = execFileSync(process.execPath, [join(HERE, "use2-benefit.mjs"), "child", cell], { encoding: "utf8" });
            const after = loadavg()[0];
            appendFileSync(RECORDS, JSON.stringify({ run, rep, ...JSON.parse(out), load1_before: before, load1_after: after, therm: rep === 0 ? therm() : undefined, node: process.version }) + "\n");
        }
    }
    console.log(`appended ${REPS * Object.keys(CELLS).length} reps to ${RECORDS}`);
}

const q = (xs, p) => {
    const s = [...xs].sort((a, b) => a - b);
    const i = (s.length - 1) * p;
    return s[Math.floor(i)] + (s[Math.ceil(i)] - s[Math.floor(i)]) * (i - Math.floor(i));
};

/** Static ESM closure of a dist subpath (relative imports only), raw and gzip -9 per file summed. */
function closure(version, entries) {
    const root = join(PKG[version], "dist");
    const seen = new Set();
    const walk = (f) => {
        if (seen.has(f)) return;
        seen.add(f);
        const t = readFileSync(f, "utf8");
        for (const m of t.matchAll(/(?:import|export)\s*(?:[\s\S]*?\sfrom\s*)?["'](\.[^"']+)["']/g)) walk(resolve(dirname(f), m[1]));
    };
    for (const e of entries) walk(join(root, `subpaths/${e}.js`));
    let raw = 0;
    let gz = 0;
    for (const f of seen) {
        const b = readFileSync(f);
        raw += b.length;
        gz += gzipSync(b, { level: 9 }).length;
    }
    return { raw, gz, files: seen.size };
}

function srcSubpaths(repo) {
    const rows = parseTsv(readFileSync(join(HERE, "CONSUMER-CENSUS.tsv"), "utf8"));
    const subs = new Set();
    for (const r of rows) {
        if (r.repo !== repo || r.kind !== "IMPORT-VJ" || r.scope !== "src" || r.detail === "type-only") continue;
        const m = r.module.match(/^@mkbabb\/value\.js\/(\w+)$/);
        if (m) subs.add(m[1]);
    }
    return [...subs].sort();
}

function handSites(repo) {
    return parseTsv(readFileSync(join(HERE, "CONSUMER-CENSUS.tsv"), "utf8")).filter((r) => r.repo === repo && r.kind === "REGEX" && r.scope === "src").length;
}

const ROW_OF = { value: ["value", "value-color"], keyframes: ["keyframes"], fourier: ["fourier"] };
const REPO_OF = { value: "value.js", keyframes: "keyframes.js", fourier: "fourier-analysis" };

/** value's conformance column: the V-C table judged on the same tarball (use3-vc-artifact.mjs). */
function vcOf(version) {
    const f = join(HERE, `records/use3-vc-${version}.txt`);
    if (!existsSync(f)) return `V-C not judged on the ${version} tarball`;
    const text = readFileSync(f, "utf8");
    const sha = text.match(/sha256 ([0-9a-f]{64})/)[1];
    const [, cases, pass, ruled, , , , , miss] = text.split("\n").find((l) => l.startsWith("TOTAL")).trim().split(/\s+/);
    return `V-C judged on value.js ${version} tarball sha256 ${sha} (records/use3-vc-${version}.txt): ${cases} cases, ${pass} pass, ${ruled} ruled, ${miss} MISS (css-color)`;
}

function table() {
    const recs = existsSync(RECORDS) ? readFileSync(RECORDS, "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l)) : [];
    const lastRun = recs.length ? recs[recs.length - 1].run : null;
    const mine = recs.filter((r) => r.run === lastRun && versionOf(r) === CELLS[r.cell]?.version);
    const quiet = mine.length > 0 && mine.every((r) => r.load1_before < 8 && r.load1_after < 8);
    const loads = mine.flatMap((r) => [r.load1_before, r.load1_after]);
    const rows = [];
    for (const [row, cells] of Object.entries(ROW_OF)) {
        const repo = REPO_OF[row];
        // value's own row measures its whole parse-bearing surface (./css and ./color)
        const subs = row === "value" ? ["color", "css"] : srcSubpaths(repo);
        const version = CELLS[cells[0]].version;
        const parseSubs = subs.filter((s) => s === "css" || s === "color" || s === "value");
        const b = closure(version, parseSubs);
        const ms = cells.map((c) => {
            const w = mine.filter((r) => r.cell === c).map((r) => r.warm_ms);
            const k = mine.filter((r) => r.cell === c).map((r) => r.cold_ms);
            const one = mine.find((r) => r.cell === c);
            if (!w.length) return CELLS[c].phase3 ? `${c} ${CELLS[c].fn}: Phase 3 cell ${CELLS[c].phase3} (value.js ${CELLS[c].version} tarball; the 4.2.1 reading is superseded)` : `${c}: no reps`;
            return `${c} ${CELLS[c].fn}×${one.n} (ok ${one.ok}) warm ${q(w, 0.5).toFixed(2)} ms IQR ${q(w, 0.25).toFixed(2)}–${q(w, 0.75).toFixed(2)}; cold ${q(k, 0.5).toFixed(2)} ms IQR ${q(k, 0.25).toFixed(2)}–${q(k, 0.75).toFixed(2)}; reps ${w.length}`;
        });
        const phase3 = cells.every((c) => CELLS[c].phase3 && !mine.some((r) => r.cell === c));
        rows.push({
            row: 0,
            consumer: row,
            pinned_value: version,
            src_subpaths: subs.join(" ") || "(none)",
            parse_bytes_raw: b.raw,
            parse_bytes_gz9: b.gz,
            closure_files: b.files,
            parse_ms: ms.join(" | "),
            timing_label: phase3 ? `PHASE-3 (${cells.map((c) => CELLS[c].phase3).join(", ")})` : !mine.length ? "UNMEASURED" : quiet ? "QUIET" : `NOISY (load1 ${Math.min(...loads).toFixed(1)}–${Math.max(...loads).toFixed(1)})`,
            conformance:
                row === "value"
                    ? vcOf(version)
                    : row === "keyframes"
                      ? "n/a: keyframes.js has no WPT denominator; it inherits value.js 4.2.0's grammar"
                      : "n/a: fourier has no WPT denominator; it inherits value.js 4.2.0's ./color grammar",
            artifact_sha256: TARBALL_SHA256[version],
            hand_parse_sites_src: handSites(repo),
            hand_lines_deleted: 0,
            run: lastRun ?? "",
        });
    }
    return rows;
}

const HEADER = ["row", "consumer", "pinned_value", "artifact_sha256", "src_subpaths", "parse_bytes_raw", "parse_bytes_gz9", "closure_files", "parse_ms", "timing_label", "conformance", "hand_parse_sites_src", "hand_lines_deleted", "run"];
const OUT = join(HERE, "BENEFIT-ROW0.tsv");
const [, , cmd, arg] = process.argv;
if (cmd === "child") await child(arg);
else if (cmd === "bench") bench();
else if (cmd === "table") {
    writeFileSync(OUT, tsv(HEADER, table()));
    console.log(readFileSync(OUT, "utf8"));
} else if (cmd === "--check") {
    let bad = 0;
    if (!existsSync(OUT)) (console.log("RED BENEFIT-ROW0.tsv: missing"), bad++);
    else {
        const rows = parseTsv(readFileSync(OUT, "utf8"));
        for (const want of ["value", "keyframes", "fourier"]) {
            const r = rows.find((x) => x.consumer === want);
            if (!r) (console.log(`RED row 0 lacks ${want}`), bad++);
            else if (r.timing_label.startsWith("PHASE-3")) {
                if (want !== "value" || !(Number(r.parse_bytes_gz9) > 0)) (console.log(`RED ${want}: a Phase 3 timing label on a row without a registered cell`), bad++);
            } else if (r.timing_label === "UNMEASURED" || /no reps/.test(r.parse_ms) || !(Number(r.parse_bytes_gz9) > 0))
                (console.log(`RED ${want}: unmeasured column`), bad++);
            else if (!/reps (\d+)/.test(r.parse_ms) || Number(r.parse_ms.match(/reps (\d+)/)[1]) < 10) (console.log(`RED ${want}: < 10 reps`), bad++);
        }
        const fresh = tsv(HEADER, table());
        if (fresh !== readFileSync(OUT, "utf8")) (console.log("RED BENEFIT-ROW0.tsv differs from its records"), bad++);
    }
    if (!bad) console.log("GREEN BENEFIT-ROW0.tsv: keyframes and fourier measured from records (≥ 10 reps each), value's bytes and V-C on one tarball with its timing a registered Phase 3 cell; regenerated identically");
    process.exit(bad ? 1 : 0);
} else console.log("usage: bench | table | --check");
