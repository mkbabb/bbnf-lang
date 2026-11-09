// PROTO-2 (LOCK-6 6A) — REAL Rust->TS(Node) behavioral drive.
//
// Loads the bbnf-path-ts cdylib compiled to wasm32 (nodejs target) and
// drives it THROUGH the actual ts/path.ts `makePathTag` + `PathError`
// (compiled to ESM JS), so every call crosses the wasm boundary via
// serde-wasm-bindgen — NOT the intra-Rust compile_path_native proxy.
//
// For each of the five drive paths it captures the field tuple
// (segment_index, segment_str, struct_name, alternatives, reason) on
// failure, or {ok, segments} on success, and prints a JSON array on
// stdout. `compare.mjs` diffs this against the canonical bbnf::path rows.

import { createRequire } from "node:module";
const require = createRequire(import.meta.url);

// nodejs-target wasm-bindgen glue loads the .wasm synchronously at require.
const wasm = require("./pkg/bbnf_path_ts.js");
const { makePathTag, PathError } = await import("./tslib/path.js");

const path = makePathTag(wasm.compile_path, "Json");

// Tagged-template shim: makePathTag returns fn(strings, ...values).
const tag = (s) => Object.assign([s], { raw: [s] });

const PATHS = ["$.statuses[0].text", "$.nope", "$.id[0]", "", "$.statuses[0].nope"];

const rows = [];
for (const p of PATHS) {
    try {
        const tp = path(tag(p));
        const segs = (tp.segments ?? []).map((seg) => {
            switch (seg.kind) {
                case "Field": return seg.value;
                case "Index": return String(seg.value);
                case "Wildcard": return "*";
                case "VariantName": return "@" + seg.value;
                default: return JSON.stringify(seg);
            }
        });
        rows.push({ path: p, ok: true, segments: segs });
    } catch (e) {
        if (e instanceof PathError) {
            rows.push({
                path: p,
                ok: false,
                segment_index: e.segment_index,
                segment_str: e.segment_str,
                struct_name: e.struct_name,
                alternatives: [...e.alternatives],
                reason: e.reason,
            });
        } else {
            rows.push({ path: p, ok: false, raw_error: String(e) });
        }
    }
}

console.log(JSON.stringify(rows));
