/**
 * TS <-> Rust front-end parity over every grammar in `grammar/MANIFEST`
 * (sk-v27 LW2.PAR-1; ruling R-b). Promotes the LW1 gate's two readers
 * (`lgb_ts_read.mjs`, `lgb_rust_read.rs`) to one test.
 *
 * - TS arm: this package's front end at HEAD (`BBNFToASTWithImports`).
 * - Rust arm: `BbnfBootstrap::parse` through the `debug_parse` binary of
 *   crates/bootstrap (`cargo build -p bbnf-bootstrap --bin debug_parse`),
 *   found at `$BBNF_RUST_READER` or `target/debug/debug_parse`. A missing
 *   binary fails the test; it never skips.
 *
 * The test passes only when the arms disagree on exactly the grammars that
 * `restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv` carries as
 * Rust-side rows, every row's spec anchor is a sentence of `docs/bbnf/`, and
 * the case set is the whole MANIFEST (at least the 36 grammars of the LW1
 * baseline, and never fewer than the count the table was ruled at).
 */
import { describe, expect, it } from "vitest";
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import path from "node:path";

import { BBNFToASTWithImports } from "../src/parse";

const ROOT = path.resolve(__dirname, "../..");
const MANIFEST = path.join(ROOT, "grammar/MANIFEST");
const DIVERGENCE = path.join(ROOT, "restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv");
const SPEC = ["grammar-syntax.md", "operators.md", "pretty-directives.md", "recover-directives.md"].map((f) =>
    path.join(ROOT, "docs/bbnf", f),
);
const RUST = process.env.BBNF_RUST_READER ?? path.join(ROOT, "target/debug/debug_parse");
const LW1_BASELINE_CASES = 36;

type Row = { id: string; files: string[]; anchors: string[]; fate: string };

function manifest(): { sha256: string; files: string[] } {
    const text = readFileSync(MANIFEST, "utf8");
    const files = text
        .split("\n")
        .filter((l) => l.trim() !== "")
        .map((l) => {
            const [hash, file] = l.split(/\s+/, 2);
            const bytes = readFileSync(path.join(ROOT, file));
            expect(createHash("sha256").update(bytes).digest("hex"), file).toBe(hash);
            return file;
        });
    return { sha256: createHash("sha256").update(text).digest("hex"), files };
}

function divergence(): { header: Map<string, string>; rows: Row[] } {
    const header = new Map<string, string>();
    const lines = readFileSync(DIVERGENCE, "utf8").split("\n").filter((l) => l !== "");
    for (const l of lines.filter((l) => l.startsWith("#"))) {
        const [k, v] = l.slice(1).trim().split("\t");
        if (v !== undefined) header.set(k, v);
    }
    const [head, ...body] = lines.filter((l) => !l.startsWith("#"));
    const cols = head.split("\t");
    const rows = body.map((l) => {
        const r = Object.fromEntries(l.split("\t").map((v, i) => [cols[i], v]));
        return { id: r.id, files: r.files.split(","), anchors: r.anchor.split(" ‖ "), fate: r.fate };
    });
    return { header, rows };
}

function tsAccepts(file: string): boolean {
    try {
        const result = BBNFToASTWithImports(readFileSync(path.join(ROOT, file), "utf8"));
        return result.length === 2 && result[1] !== undefined;
    } catch {
        return false;
    }
}

function rustAccepts(file: string): boolean {
    const out = spawnSync(RUST, [path.join(ROOT, file)], { stdio: ["ignore", "ignore", "pipe"] });
    if (out.error) throw out.error;
    return out.status === 0;
}

describe("TS <-> Rust front-end parity (LW2.PAR-1)", () => {
    it("reads the whole MANIFEST that DIVERGENCE.tsv was ruled at", () => {
        const { sha256, files } = manifest();
        const { header } = divergence();
        expect(sha256, "grammar/MANIFEST changed: re-read and re-rule DIVERGENCE.tsv").toBe(header.get("manifest-sha256"));
        expect(files.length).toBeGreaterThanOrEqual(LW1_BASELINE_CASES);
        expect(files.length).toBeGreaterThanOrEqual(Number(header.get("cases")));
    });

    it("cites a spec sentence for every ruled divergence", () => {
        const spec = SPEC.map((f) => readFileSync(f, "utf8")).join("\n");
        for (const row of divergence().rows) {
            for (const anchor of row.anchors) {
                expect(spec.includes(anchor), `${row.id}: anchor not in docs/bbnf: ${anchor}`).toBe(true);
            }
        }
    });

    it("disagrees exactly on the carried Rust-side rows", () => {
        expect(existsSync(RUST), `build the Rust arm: cargo build -p bbnf-bootstrap --bin debug_parse (${RUST})`).toBe(true);
        const { files } = manifest();
        const observed = files
            .map((f) => ({ f, ts: tsAccepts(f), rust: rustAccepts(f) }))
            .filter((v) => v.ts !== v.rust)
            .map((v) => `${v.f}\tts=${v.ts ? "ACCEPT" : "REJECT"}\trust=${v.rust ? "ACCEPT" : "REJECT"}`);
        const carried = divergence()
            .rows.filter((r) => r.fate.startsWith("CARRIED"))
            .flatMap((r) => r.files);
        expect(observed.map((o) => o.split("\t")[0]).sort()).toEqual([...carried].sort());
    }, 120_000);
});
