// LW2.USE-1 / LW2.USE-2: G-S1, value.js parse speed under the sk-v27 TS emitter.
//
// Two arms over ONE value.js tree, value.js `4038dce5` (read through git, never a working tree):
//   base  the tree as committed: src/css/bbnf/generated/{grammar,color}.js as @mkbabb/bbnf-lang 0.2.0 emitted them
//   cand  the same tree with those modules re-emitted by bbnf-lang `typescript/` (this checkout, built), through
//         value.js's own scripts/gen-grammar.mjs (same grammars, same action tables, same entries)
// Nothing else differs: `build` refuses unless the two source trees differ in exactly the generated files.
//
// Every timed pass calls the entry on every input of the class and folds the result into a checksum (the walk)
// INSIDE the timed region, so an arm that skips work or skips the walk changes the checksum (USE-2).
//
//   node gs1.mjs build                     arms -> $GS1_DIR/_build/{base,cand}.mjs, provenance.json
//   node gs1.mjs proof [--mutant M]        equal-work proof over the 17,316 WPT inputs x the 7 entries
//                                          (M = skip-work | skip-walk: the falsifier's mutants); exit 0 iff 100 % equal
//   node gs1.mjs vector <arm> [M]          one arm's per-(entry, case) checksum vector (internal: a fresh process)
//   node gs1.mjs pass <cell> <arm>         one timed fresh-process pass of a registered PREREG §8 cell (S11, under cbb0)
//   node gs1.mjs block <cell> <b>          ABBA block b of a cell: four fresh processes, two pairs, order by the seed
import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, symlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const BBNF = resolve(HERE, "../../../../..");
const TS = join(BBNF, "typescript");
const VJ = join(homedir(), "Programming/value.js");
const DIR = process.env.GS1_DIR ?? join(homedir(), ".dev-logs/bbnf-lockstep/lw2/S10-USE/gs1");
const BUILD = join(DIR, "_build");
const PREREG = join(BBNF, "bench/PREREG.md");

export const BASE_SHA = "4038dce5ac3278eb10441daca1e55d5c4f83cc70";
// The WPT value cases of record (value.js test/css/wpt-values, identical at origin/master 12fe04a75 and tag v4.3.0).
export const WPT = { rev: "v4.3.0", file: "test/css/wpt-values/cases-2026-11-04-r1.json", sha256: "2639ac5779ca711beca73e7127c477cbb866d729bc31d692c2f1e2095552fc76", cases: 17316 };
export const ENTRIES = ["parseCssColor", "parseCssScalar", "parseCssValue", "parseCssValues", "parseKeyframeSelector", "parseTimingFunction", "parseStylesheet"];
export const SEED = 20261109;
const GENERATED = ["src/css/bbnf/generated/color.d.ts", "src/css/bbnf/generated/color.js", "src/css/bbnf/generated/grammar.d.ts", "src/css/bbnf/generated/grammar.js"];

const git = (repo, ...a) => execFileSync("git", ["-C", repo, ...a], { encoding: "utf8", maxBuffer: 1 << 28 });
const sha256 = (b) => createHash("sha256").update(b).digest("hex");

// ── the corpus ────────────────────────────────────────────────────────────────

export function wptInputs() {
    const text = git(VJ, "show", `${WPT.rev}:${WPT.file}`);
    if (sha256(text) !== WPT.sha256) throw new Error(`WPT corpus sha256 drift at ${WPT.rev}:${WPT.file}`);
    const cases = JSON.parse(text).cases;
    if (cases.length !== WPT.cases) throw new Error(`WPT corpus has ${cases.length} cases, not ${WPT.cases}`);
    return cases.map((c) => c.input);
}

/** mulberry32: the registered PRNG; one stream per seed. */
export function prng(seed) {
    let a = seed >>> 0;
    return () => {
        a = (a + 0x6d2b79f5) >>> 0;
        let t = a;
        t = Math.imul(t ^ (t >>> 15), t | 1);
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
}

/** Fisher–Yates under the seed: the pass order, identical for both arms. */
export function shuffled(xs, seed) {
    const r = prng(seed), a = [...xs];
    for (let i = a.length - 1; i > 0; i--) {
        const j = Math.floor(r() * (i + 1));
        [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
}

// ── the walk: a structural checksum of a result (FNV-1a, two lanes) ────────────
// isDeepStrictEqual's view: constructor name, own enumerable keys (sorted), array order, Map/Set entries,
// -0 apart from 0, NaN equal to NaN. A thrown error folds as its constructor name.

export function walk(x, h) {
    const seen = new Set();
    const str = (s) => {
        for (let i = 0; i < s.length; i++) {
            const c = s.charCodeAt(i);
            h[0] = Math.imul(h[0] ^ c, 0x01000193);
            h[1] = Math.imul(h[1] ^ c, 0x5bd1e995) ^ (h[1] >>> 13);
        }
        h[0] = Math.imul(h[0] ^ 0xff, 0x01000193);
    };
    const go = (v) => {
        switch (typeof v) {
            case "number": return str(Object.is(v, -0) ? "n-0" : `n${v}`);
            case "string": return str(`s${v.length}:`), str(v);
            case "boolean": return str(v ? "T" : "F");
            case "undefined": return str("u");
            case "bigint": return str(`b${v}`);
            case "symbol": return str(`y${v.description}`);
            case "function": return str(`f${v.name}`);
        }
        if (v === null) return str("z");
        if (seen.has(v)) return str("cyc");
        seen.add(v);
        str(`o${v.constructor?.name ?? "null"}`);
        if (Array.isArray(v)) {
            str(`a${v.length}`);
            for (const e of v) go(e);
        } else if (v instanceof Map) {
            str(`m${v.size}`);
            for (const [k, e] of v) go(k), go(e);
        } else if (v instanceof Set) {
            str(`t${v.size}`);
            for (const e of v) go(e);
        } else {
            const ks = Object.keys(v).sort();
            str(`k${ks.length}`);
            for (const k of ks) str(k), go(v[k]);
        }
        seen.delete(v);
    };
    go(x);
    return h;
}

const callFold = (fn, s, h) => {
    let r;
    try {
        r = fn(s);
    } catch (e) {
        r = { threw: e?.constructor?.name ?? typeof e };
    }
    walk(r, h);
    return r;
};

// ── build ─────────────────────────────────────────────────────────────────────

function treeFiles(root) {
    const out = new Map();
    const rec = (d) => {
        for (const n of readdirSync(join(root, d), { withFileTypes: true })) {
            const p = d ? `${d}/${n.name}` : n.name;
            if (n.name === "node_modules") continue;
            if (n.isDirectory()) rec(p);
            else out.set(p, sha256(readFileSync(join(root, p))));
        }
    };
    rec("");
    return out;
}

async function build() {
    const sha = git(VJ, "rev-parse", `${BASE_SHA}^{commit}`).trim();
    if (sha !== BASE_SHA) throw new Error(`base ${BASE_SHA} resolves to ${sha}`);
    const cli = join(TS, "dist/cli.js");
    if (!existsSync(cli)) throw new Error(`${cli} missing: build bbnf-lang typescript first (npm ci && npm run build)`);
    rmSync(DIR, { recursive: true, force: true });
    mkdirSync(BUILD, { recursive: true });
    const src = { base: join(DIR, "base-src"), cand: join(DIR, "cand-src") };
    mkdirSync(src.base, { recursive: true });
    execFileSync("sh", ["-c", `git -C "${VJ}" archive ${BASE_SHA} src scripts/gen-grammar.mjs package.json | tar -x -C "${src.base}"`]);
    cpSync(src.base, src.cand, { recursive: true });
    // value.js's own generator, with @mkbabb/bbnf-lang resolved to this checkout's TS package
    mkdirSync(join(src.cand, "node_modules/@mkbabb"), { recursive: true });
    symlinkSync(TS, join(src.cand, "node_modules/@mkbabb/bbnf-lang"));
    symlinkSync(join(TS, "node_modules/esbuild"), join(src.cand, "node_modules/esbuild"));
    const gen = spawnSync(process.execPath, [join(src.cand, "scripts/gen-grammar.mjs")], { cwd: src.cand, encoding: "utf8" });
    if (gen.status !== 0) throw new Error(`gen-grammar under bbnf-lang HEAD failed (${gen.status}):\n${gen.stdout}\n${gen.stderr}`);
    rmSync(join(src.cand, "node_modules"), { recursive: true, force: true });
    // the only difference between the arms is the emitted modules
    const a = treeFiles(src.base), b = treeFiles(src.cand);
    const diff = [...new Set([...a.keys(), ...b.keys()])].filter((p) => a.get(p) !== b.get(p)).sort();
    const stray = diff.filter((p) => !GENERATED.includes(p));
    if (stray.length) throw new Error(`arms differ outside the generated modules: ${stray.join(" ")}`);
    const { build: esbuild } = await import(join(TS, "node_modules/esbuild/lib/main.js"));
    const prov = { base: BASE_SHA, bbnf_head: git(BBNF, "rev-parse", "HEAD").trim(), bbnf_ts_dirty: git(BBNF, "status", "--porcelain", "--", "typescript/src").trim(),
        node: process.version, esbuild: JSON.parse(readFileSync(join(TS, "node_modules/esbuild/package.json"), "utf8")).version, differ: diff, arms: {} };
    for (const arm of ["base", "cand"]) {
        const entry = join(BUILD, `${arm}-entry.ts`);
        writeFileSync(entry, `export * as css from ${JSON.stringify(join(src[arm], "src/css/index.ts"))};\n`);
        const out = join(BUILD, `${arm}.mjs`);
        await esbuild({ entryPoints: [entry], outfile: out, bundle: true, platform: "node", format: "esm", target: "node22", minify: false,
            sourcemap: false, logLevel: "warning", absWorkingDir: src[arm] });
        prov.arms[arm] = { bundle_sha256: sha256(readFileSync(out)), generated: Object.fromEntries(GENERATED.map((p) => [p, (arm === "base" ? a : b).get(p)])) };
    }
    writeFileSync(join(DIR, "provenance.json"), JSON.stringify(prov, null, 1) + "\n");
    console.log(JSON.stringify({ differ: diff, base: prov.arms.base.bundle_sha256.slice(0, 16), cand: prov.arms.cand.bundle_sha256.slice(0, 16), bbnf_head: prov.bbnf_head }));
}

async function loadArm(arm) {
    const m = await import(join(BUILD, `${arm}.mjs`));
    m.css.parseCssColor("red"); // grammar construction outside any timing
    return Object.fromEntries(ENTRIES.map((e) => [e, m.css[e]]));
}

// ── USE-2: the equal-work proof ───────────────────────────────────────────────

/** One arm's checksum per (entry, case), computed exactly as a timed pass computes it. */
async function vector(arm, mutant) {
    const F = await loadArm(arm);
    const xs = wptInputs();
    const out = {};
    for (const e of ENTRIES) {
        const fn = F[e];
        const v = new Array(xs.length);
        const ok = new Array(xs.length);
        let last;
        for (let i = 0; i < xs.length; i++) {
            const h = [0x811c9dc5, 0x9747b28c];
            if (mutant === "skip-work" && i % 2 === 1 && last !== undefined) walk(last, h); // reuse, no parse
            else if (mutant === "skip-walk") (last = fn(xs[i])), (h[0] = 0), (h[1] = 0);
            else last = callFold(fn, xs[i], h);
            v[i] = (h[0] >>> 0).toString(16).padStart(8, "0") + (h[1] >>> 0).toString(16).padStart(8, "0");
            ok[i] = last?.ok === true ? 1 : 0;
        }
        out[e] = { v, ok };
    }
    process.stdout.write(JSON.stringify(out));
}

function proof(mutant) {
    const prov = JSON.parse(readFileSync(join(DIR, "provenance.json"), "utf8"));
    const read = (arm, m) => {
        const r = spawnSync(process.execPath, [fileURLToPath(import.meta.url), "vector", arm, ...(m ? [m] : [])], { encoding: "utf8", maxBuffer: 1 << 28 });
        if (r.status !== 0) throw new Error(`vector ${arm} exited ${r.status}: ${r.stderr.slice(0, 400)}`);
        return JSON.parse(r.stdout);
    };
    const A = read("base"), B = read("cand", mutant);
    const rows = [];
    let bad = 0, total = 0;
    for (const e of ENTRIES) {
        const n = A[e].v.length;
        let eq = 0;
        const first = [];
        for (let i = 0; i < n; i++) {
            if (A[e].v[i] === B[e].v[i]) eq++;
            else if (first.length < 3) first.push(i);
        }
        const acc = A[e].ok.reduce((s, x) => s + x, 0);
        total += n;
        bad += n - eq;
        rows.push({ entry: e, cases: n, equal: eq, unequal: n - eq, acc_base: acc, rej_base: n - acc,
            acc_cand: B[e].ok.reduce((s, x) => s + x, 0), first_unequal: first.join(",") || "-" });
    }
    const verdict = bad === 0 ? "EQUAL-WORK" : "REFUSED";
    const head = ["entry", "cases", "equal", "unequal", "acc_base", "rej_base", "acc_cand", "first_unequal"];
    const tsvText = `# LW2.USE-2 equal-work proof: value.js ${BASE_SHA.slice(0, 9)} base vs cand (bbnf-lang ${prov.bbnf_head.slice(0, 9)} TS emitter);` +
        ` WPT ${WPT.rev}:${WPT.file} sha256 ${WPT.sha256.slice(0, 16)}… (${WPT.cases} cases); bundles base ${prov.arms.base.bundle_sha256.slice(0, 16)}… cand ${prov.arms.cand.bundle_sha256.slice(0, 16)}…;` +
        ` mutant ${mutant ?? "none"}; verdict ${verdict} (${total - bad}/${total})\n` +
        [head.join("\t"), ...rows.map((r) => head.map((k) => r[k]).join("\t"))].join("\n") + "\n";
    console.log(tsvText.trimEnd());
    if (!mutant) writeFileSync(join(HERE, "EQUAL-WORK.tsv"), tsvText);
    process.exit(bad === 0 ? 0 : 1);
}

// ── USE-1: registered cells (PREREG §8) and the timed pass S11 runs ───────────

export function registeredCells() {
    const text = readFileSync(PREREG, "utf8");
    const want = readFileSync(`${PREREG}.sha256`, "utf8").split(/\s+/)[0];
    if (sha256(text) !== want) throw new Error("PREREG.md does not match PREREG.md.sha256 (an edit after the hash is an addendum)");
    const cells = {};
    for (const m of text.matchAll(/^\| (C-S1-(ACC|REJ)-[A-Z]+) \| F-VALUE-S1 \| (\w+) \| (acc|rej) \|/gm)) cells[m[1]] = { entry: m[3], klass: m[4] };
    return cells;
}

/** A pass is k sweeps of the class in the seeded order; k = ceil(4096 / n), so every pass makes >= 4,096 calls. */
async function pass(cellId, arm) {
    const cell = registeredCells()[cellId];
    if (!cell) throw new Error(`cell ${cellId} is not registered in PREREG.md §8`);
    if (arm !== "base" && arm !== "cand") throw new Error(`arm ${arm}`);
    // both bundles are loaded in every process (the class is split by base's verdict), so the arms' heaps match
    const B = await loadArm("base"), C = await loadArm("cand"), F = arm === "base" ? B : C;
    const xs0 = wptInputs().filter((s) => {
        try {
            return (B[cell.entry](s)?.ok === true) === (cell.klass === "acc");
        } catch {
            return cell.klass === "rej";
        }
    });
    const xs = shuffled(xs0, SEED), fn = F[cell.entry], k = Math.ceil(4096 / xs.length);
    const sweep = (h) => { for (let j = 0; j < k; j++) for (const s of xs) callFold(fn, s, h); };
    for (let w = 0; w < 3; w++) sweep([0, 0]);
    const h = [0x811c9dc5, 0x9747b28c];
    const t0 = performance.now();
    sweep(h);
    const ms = performance.now() - t0;
    const bytes = xs.reduce((s, x) => s + Buffer.byteLength(x), 0) * k;
    console.log(`cell=${cellId} arm=${arm} n=${xs.length} k=${k} ms=${ms.toFixed(4)} ns_per_byte=${((ms * 1e6) / bytes).toFixed(4)} checksum=${(h[0] >>> 0).toString(16)}${(h[1] >>> 0).toString(16)}`);
}

/** Block b: two pairs in four fresh processes, ABBA when the seed's b-th draw is < 0.5, else BAAB. */
function block(cellId, b) {
    if (!registeredCells()[cellId]) throw new Error(`cell ${cellId} is not registered in PREREG.md §8`);
    const r = prng(SEED);
    let d = 0;
    for (let i = 0; i <= Number(b); i++) d = r();
    const order = d < 0.5 ? ["base", "cand", "cand", "base"] : ["cand", "base", "base", "cand"];
    const got = order.map((arm) => {
        const o = execFileSync(process.execPath, [fileURLToPath(import.meta.url), "pass", cellId, arm], { encoding: "utf8" });
        const m = Object.fromEntries(o.trim().split(" ").map((kv) => kv.split("=")));
        return { arm, ns: Number(m.ns_per_byte), checksum: m.checksum };
    });
    for (const [p, [x, y]] of [[1, [got[0], got[1]]], [2, [got[2], got[3]]]]) {
        const base = x.arm === "base" ? x : y, cand = x.arm === "cand" ? x : y;
        const same = base.checksum === cand.checksum ? 1 : 0;
        console.log(`pair${p}_order=${order.slice(2 * p - 2, 2 * p).join("")} pair${p}_r=${same ? Math.log(cand.ns / base.ns).toFixed(6) : "DISCARDED"} pair${p}_checksum_equal=${same}`);
    }
}

const [, , verb, a1, a2] = process.argv;
if (verb === "build") await build();
else if (verb === "proof") proof(a1 === "--mutant" ? a2 : undefined);
else if (verb === "vector") await vector(a1, a2);
else if (verb === "pass") await pass(a1, a2);
else if (verb === "block") block(a1, a2);
else if (verb === "cells") console.log(JSON.stringify(registeredCells(), null, 1));
else console.log("usage: build | proof [--mutant skip-work|skip-walk] | pass <cell> <arm> | block <cell> <b> | cells");
