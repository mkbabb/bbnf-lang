// PROTO-2 (LOCK-6 6A) — behavioral field-for-field comparator.
//
// Drives the wasm cdylib through ts/path.ts (makePathTag) in Node and
// compares each path's error tuple against the canonical bbnf::path
// rows emitted by `examples/canonical_dump.rs`. Prints a per-path
// AGREE/DIVERGE verdict.
//
// NB: ts/path.ts makePathTag has a rebox defect — its inner try/catch
// swallows the `new PathError(payload)` it constructs and rethrows the
// raw wasm JsError. The PathErrorPayload is nonetheless intact in the
// error .message; we parse it out to recover the true cdylib payload.

import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const wasm = require("./pkg/bbnf_path_ts.js");
const { makePathTag, PathError } = await import("./tslib/path.js");

const path = makePathTag(wasm.compile_path, "Json");
const tag = (s) => Object.assign([s], { raw: [s] });

// Canonical rows from cargo run --example canonical_dump (bbnf::path).
const CANON = {
    "$.statuses[0].text": { ok: false, segment_index: 2, segment_str: "text", struct_name: "Document", alternatives: ["Document","Status"], reason: "UnregisteredRule" },
    "$.nope":             { ok: false, segment_index: 0, segment_str: "nope", struct_name: "Document", alternatives: ["id","statuses"], reason: "UnknownField" },
    "$.id[0]":            { ok: false, segment_index: 1, segment_str: "0", struct_name: "Document", alternatives: [], reason: "FieldOnScalar" },
    "":                   { ok: false, segment_index: 0, segment_str: "<empty>", struct_name: "Document", alternatives: ["Document","Status"], reason: "EmptyPath" },
    "$.statuses[0].nope": { ok: false, segment_index: 2, segment_str: "nope", struct_name: "Document", alternatives: ["Document","Status"], reason: "UnregisteredRule" },
};

function tsRow(p) {
    try {
        const tp = path(tag(p));
        return { ok: true, segments: (tp.segments ?? []).map(s => s.kind === "Field" ? s.value : s.kind === "Index" ? String(s.value) : s.kind === "Wildcard" ? "*" : "@"+s.value) };
    } catch (e) {
        let payload = null;
        if (e instanceof PathError) {
            payload = { segment_index: e.segment_index, segment_str: e.segment_str, struct_name: e.struct_name, alternatives: [...e.alternatives], reason: e.reason };
        } else {
            const msg = e instanceof Error ? e.message : String(e);
            try { payload = JSON.parse(msg); } catch { payload = { raw: msg }; }
        }
        return { ok: false, ...payload };
    }
}

const FIELDS = ["segment_index","segment_str","struct_name","alternatives","reason"];
const eq = (a,b) => JSON.stringify(a) === JSON.stringify(b);

let agree = 0, diverge = 0;
console.log("path                     | verdict  | detail");
console.log("-------------------------|----------|--------------------------------");
for (const p of Object.keys(CANON)) {
    const ts = tsRow(p);
    const rs = CANON[p];
    let ok = ts.ok === rs.ok;
    const diffs = [];
    if (rs.ok === false && ts.ok === false) {
        for (const f of FIELDS) if (!eq(ts[f], rs[f])) { ok = false; diffs.push(`${f}: TS=${JSON.stringify(ts[f])} RS=${JSON.stringify(rs[f])}`); }
    } else if (ts.ok !== rs.ok) {
        diffs.push(`TS ok=${ts.ok} vs RS ok=${rs.ok}`);
    }
    if (ok) agree++; else diverge++;
    const label = (p === "" ? "(empty)" : p).padEnd(24);
    console.log(`${label} | ${ok ? "AGREE   " : "DIVERGE "} | ${diffs.join("; ") || "field-for-field equal"}`);
}
console.log("-------------------------|----------|--------------------------------");
console.log(`AGREE=${agree}  DIVERGE=${diverge}  (of ${Object.keys(CANON).length})`);
