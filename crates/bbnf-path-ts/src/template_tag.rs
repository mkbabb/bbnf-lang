//! JS-side template-literal tag bridge.
//!
//! Exposes the JavaScript shim string consumed by the TS frontend at
//! `ts/path.ts`. Bundlers (Vite, esbuild, SWC) inline this constant at
//! build time so the runtime cost is one wasm-bindgen call per
//! `path` `` invocation — not a per-character JS-side parse.
//!
//! The shim is intentionally minimal: it joins the literal-template
//! parts into a path string (ignoring the interpolated values; the
//! surface is a tagged template literal whose payload is a single
//! string), then delegates to the wasm-bindgen `compile_path` export.
//! The TS-side wrapper at `ts/path.ts` adds the typing surface, the
//! constant-folding helper, and the `PathError` rendering.

/// JavaScript shim for the `path` `` template-literal tag.
///
/// Consumers import the wasm-bindgen output, then attach this shim as
/// the `path` symbol. Bundlers can also inline the shim at build time
/// when `compile_path` is statically resolvable.
pub const TEMPLATE_TAG_JS: &str = r#"
// path-tag.js — TS template-literal tag bridge for bbnf-path-ts.
// Auto-generated; do not hand-edit.
//
// Usage:
//   import init, { compile_path } from "bbnf-path-ts";
//   import { makePathTag } from "./path-tag.js";
//   await init();
//   const path = makePathTag(compile_path, "Json");
//   const p = path`$.statuses[0].text`;
//
// The tag joins the template strings into a single path string; the
// interpolation slots are ignored (a path tag's payload is the literal
// template). Pass interpolations through `String.raw` if dynamic
// segments are required at runtime.
export function makePathTag(compileFn, grammar) {
    return function pathTag(strings, ...values) {
        const pathStr = strings.raw.reduce((acc, str, i) => {
            const interp = i < values.length ? String(values[i]) : "";
            return acc + str + interp;
        }, "");
        return compileFn(pathStr, grammar);
    };
}
"#;
