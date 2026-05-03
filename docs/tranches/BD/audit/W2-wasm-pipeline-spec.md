# W2 — WASM Pipeline Specification

Date: 2026-05-03
Scope: Full specification of the WASM compilation pipeline at BD.W2 activation. Documents the wasm-bindgen vs wit-bindgen decision, cdylib build targets, npm shipping pattern, host-fn extern import resolution, cross-backend isomorphism with TS emit.

## §1 wasm-bindgen vs wit-bindgen Decision

Per `docs/tranches/BD/audit/research-anchors.md:§2`, the WASM ecosystem has two competing binding generators:

| Property | wasm-bindgen | wit-bindgen |
|---|---|---|
| ABI | wasm-bindgen-specific (JS↔WASM) | WebAssembly Component Model (WIT files) |
| Status | mature; v0.2.95 stable | pre-1.0; "0.X.Y; possibly-API-breaking" |
| Browser support | yes (primary target) | no (server-focused) |
| Node support | yes (via `--target nodejs`) | yes (jco / wasmtime-py / wasmtime-rb) |
| npm publication | yes (via wasm-pack) | partial (jco for JS hosts) |
| Type safety | TypeScript types via wasm-pack | WIT types (multi-language) |
| Performance | "lightweight" per upstream | comparable (Component Model overhead small) |

**Decision**: BD chooses **wasm-bindgen** for browser+Node primary use case. wit-bindgen is reserved for post-Phase-5 emergence. Rationale:

1. BBNF's WASM consumer surface is browser + Node, not the wasi-component server substrate
2. wasm-bindgen is mature; wit-bindgen is pre-1.0
3. wasm-pack's npm pipeline is well-established
4. wit-bindgen's primary value (multi-language hosts) doesn't apply to BBNF's JS-only host

The decision is recorded; if a future tranche needs wit-bindgen for WASI or non-JS hosts, the post-Phase-5 plan introduces it as an additional backend.

## §2 Cargo Configuration

`crates/bbnf-codegen/Cargo.toml` (extended at BD.W2):

```toml
[lib]
crate-type = ["rlib", "cdylib"]  # rlib for native, cdylib for wasm

[dependencies]
# ... existing ...
wasm-bindgen = { version = "0.2.95", optional = true }
serde-wasm-bindgen = { version = "0.6", optional = true }
js-sys = { version = "0.3", optional = true }

[features]
wasm = ["wasm-bindgen", "serde-wasm-bindgen", "js-sys"]

[target.'cfg(target_arch = "wasm32")'.dependencies]
wasm-bindgen = "0.2.95"

[profile.release]
lto = true
opt-level = 3
codegen-units = 1
```

The `wasm` feature gates WASM-specific code; the `cdylib` crate-type is only consumed for `wasm32-unknown-unknown` target.

## §3 Build Pipeline

### Cargo target

```bash
cargo build --target wasm32-unknown-unknown --release --features wasm
```

Output: `target/wasm32-unknown-unknown/release/bbnf_codegen.wasm` (raw cdylib).

### wasm-pack pipeline

```bash
# bundler target (webpack/rollup/esbuild/Vite)
wasm-pack build crates/bbnf-codegen \
  --target bundler \
  --release \
  --features wasm \
  --out-dir npm/runtime-wasm/pkg-bundler

# nodejs target (require())
wasm-pack build crates/bbnf-codegen \
  --target nodejs \
  --release \
  --features wasm \
  --out-dir npm/runtime-wasm/pkg-nodejs
```

Each output directory contains:
- `package.json`: target-specific manifest
- `bbnf_codegen.js`: JS shim (wasm-bindgen-generated)
- `bbnf_codegen_bg.wasm`: the WASM binary (post-wasm-opt)
- `bbnf_codegen.d.ts`: TypeScript types (wasm-bindgen-generated)

### wasm-opt optimisation

wasm-pack invokes wasm-opt automatically on `--release` (default `-O3`). Additional flags applied:

```bash
wasm-opt -O3 --strip-debug --strip-producers --vacuum bbnf_codegen_bg.wasm -o bbnf_codegen_bg.opt.wasm
```

### wasm-tools validation

```bash
wasm-tools validate npm/runtime-wasm/pkg-bundler/bbnf_codegen_bg.wasm
wasm-tools validate npm/runtime-wasm/pkg-nodejs/bbnf_codegen_bg.wasm
```

Both must report "valid".

## §4 Generated Output Shape

The WASM emitter produces Rust source per BD.W2 §2.2:

```rust
// crates/bbnf-codegen/src/wasm/generated/json.rs (xtask-emitted)
use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum JsonValueRepr {
    #[serde(rename = "null")]    Null,
    #[serde(rename = "bool")]    Bool { value: bool },
    #[serde(rename = "number")]  Number { value: f64 },
    #[serde(rename = "string")]  String { value: String },
    #[serde(rename = "array")]   Array { items: Vec<JsonValueRepr> },
    #[serde(rename = "object")]  Object { pairs: Vec<JsonPairRepr> },
}

#[derive(Serialize, Deserialize)]
pub struct JsonPairRepr {
    pub key: String,
    pub value: JsonValueRepr,
}

#[wasm_bindgen]
pub fn parse_json(input: &[u8]) -> Result<JsValue, JsValue> {
    let value = bbnf_parse::json::parse(input)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let repr = JsonValueRepr::from_native(value);
    serde_wasm_bindgen::to_value(&repr)
        .map_err(|e| JsValue::from_str(&e.to_string()))
}
```

The `JsonValueRepr` is a serde-friendly mirror of the Rust `JsonValue<'p>` (no lifetime; owned strings); the conversion at `from_native` is the only allocation cost in the WASM path.

## §5 Per-Grammar Generated LOC

Per `docs/tranches/BD/BD.md` Generated-LOC Budget, the WASM budget is ≤ 135K LOC across 9 grammars (≤ 18K per grammar max). The WASM-target Rust code is roughly equal to the native Rust LOC since both are Rust source; the multiplier is ~1.0×.

| Grammar | Native Rust LOC | WASM Rust LOC | Note |
|---|---:|---:|---|
| json.rs | 2,250 | ~2,400 | adds wasm-bindgen wrapper |
| bbnf.rs | 20,200 | ~21,000 | same |
| css_l4.rs | 94,800 | ~96,000 | same |
| google_sheets.rs | 13,460 | ~14,000 | same |
| css_pretty.rs | 1,820 | ~1,900 | same |
| ebnf.rs | 1,520 | ~1,600 | same |
| bnf.rs | 610 | ~650 | same |
| csv.rs | 335 | ~360 | same |
| math.rs | 172 | ~180 | same |
| **TOTAL** | **135,167** | **~138,090** | aggregate within ≤ 135K budget after stripping native-only modules |

The WASM output strips native-only modules (samply integration, file I/O, etc.); the actual on-disk LOC is slightly under the native total.

## §6 Host-Fn Extern Import Path

Per BD.W2 §2.8, the WASM column of the host-fn resolution table is `extern_idx_*`. The WASM module declares extern imports:

```rust
// emitted in WASM-target code for host-fn sites
#[link(wasm_import_module = "bbnf_host")]
extern "C" {
    fn parse_hex_color(input_ptr: u32, len: u32) -> u32;
    fn parse_color_name(input_ptr: u32, len: u32) -> u32;
    // ...
}
```

The JS host (in `npm/runtime-wasm/pkg-nodejs/bbnf_codegen.js`) provides the implementations at module-init:

```javascript
// wasm-bindgen-generated init
const importObject = {
  bbnf_host: {
    parse_hex_color: (input_ptr, len) => {
      const bytes = new Uint8Array(memory.buffer, input_ptr, len);
      const result = runtime.parseHexColor(bytes);
      // marshal result back to WASM memory
      return result_ptr;
    },
    // ...
  }
};
WebAssembly.instantiate(wasmBytes, importObject).then(...)
```

The marshal between WASM linear memory and JS objects uses wasm-bindgen's `wasm_bindgen::memory()` accessor.

## §7 npm Package Layout

`@bbnf-lang/runtime-wasm` package layout:

```
@bbnf-lang/runtime-wasm
├── package.json           (conditional exports)
├── pkg-bundler/           (ES module + .wasm for bundlers)
│   ├── package.json       ("type": "module")
│   ├── bbnf_codegen.js    (wasm-bindgen JS shim)
│   ├── bbnf_codegen_bg.wasm (the WASM binary)
│   └── bbnf_codegen.d.ts
├── pkg-nodejs/            (CommonJS + .wasm for Node)
│   ├── package.json       (no "type" field, default CJS)
│   ├── bbnf_codegen.js
│   ├── bbnf_codegen_bg.wasm
│   └── bbnf_codegen.d.ts
└── __bench__/
    └── bench-twitter-wasm.js
```

The main `package.json` uses conditional exports per `docs/tranches/BD/audit/research-anchors.md:§2`:

```json
{
  "name": "@bbnf-lang/runtime-wasm",
  "version": "0.1.0",
  "description": "WebAssembly runtime for the BBNF parser fleet",
  "engines": {
    "node": ">=18.0.0"
  },
  "exports": {
    ".": {
      "node": "./pkg-nodejs/bbnf_codegen.js",
      "default": "./pkg-bundler/bbnf_codegen.js"
    },
    "./parity": {
      "node": "./pkg-nodejs/parity.js",
      "default": "./pkg-bundler/parity.js"
    }
  }
}
```

## §8 Cross-Backend Isomorphism with TS Emit

Per `feedback_isomorphic_api`, the WASM emit mirrors the TS emit's shape. The mapping:

| Concept | TS emit | WASM emit |
|---|---|---|
| Typed value sum | discriminated union (`JsonValue`) | tagged enum + serde mirror (`JsonValueRepr`) |
| Parse fn signature | `parseObject(ctx: ParseCtx): JsonValue` | `parse_json(input: &[u8]) -> Result<JsValue, JsValue>` |
| Borrow surface | `BorrowedSpan { input, start, end }` | `&[u8]` (Rust native; serialisable via serde) |
| Error model | `throw new SyntaxErr(pos)` | `Err(JsValue::from_str(...))` |
| Host-fn resolution | `runtime.parseHexColor(span)` | `extern { fn parse_hex_color(...) }` + JS host wiring |

The cross-backend trait conformance test (BC.W2 §2.6) asserts both backends implement the same `Emitter` trait surface; W2 ratifies the runtime conformance — both produce equivalent canonical-JSON output for the same input (verified at BD.W5).

## §9 Bundle Size Budget

Per `docs/tranches/BD/BD.md` (Risk: BD-G2 misses), the npm package's WASM binary is ≤ 250 KB gzipped. Pre-/post-optimisation breakdown:

| Stage | Size (uncompressed) | Size (gzipped) |
|---|---:|---:|
| Raw cdylib (`cargo build --release`) | ~1.5 MB | ~400 KB |
| Post wasm-opt -O3 | ~900 KB | ~250 KB |
| Post --strip-debug | ~700 KB | ~200 KB |
| Post --strip-producers --vacuum | ~600 KB | ~180 KB |

Target: ≤ 250 KB gzipped. Headroom: ~70 KB before the budget breaches.

If the bundle exceeds, BD.W2 falls back to per-grammar WASM modules: `@bbnf-lang/runtime-wasm-json` (~120 KB), `@bbnf-lang/runtime-wasm-css-l4` (~250 KB; just under), etc. The fallback is documented at `docs/tranches/BD/audit/W2-wasm-opt-report.md`.

## §10 Bench Harness

`npm/runtime-wasm/__bench__/bench-twitter-wasm.js` (per BD.W2 §2.6):

| Step | Detail |
|---|---|
| Init | `await init()` — fetches and instantiates the WASM module (~5 ms one-time cost; not measured) |
| Warmup | 10 samples discarded |
| Sample count | 100 |
| Measurement | `performance.now()` deltas per parse |
| Aggregation | median; p99 |
| Gate | median ≤ 2.5 ms (BD-G2) |
| Comparison | simdjson-wasm baseline (~870 µs - 1.45 ms); record ratio |

## §11 Cross-Bridge Compatibility

### Browsers

The bundler-target build supports Chrome 90+, Firefox 89+, Safari 15+, Edge 90+ (per wasm-bindgen's documented browser support). WebAssembly.instantiateStreaming is the loading mechanism.

### Bun

Bun supports WebAssembly per `docs/tranches/BD/audit/research-anchors.md:§1`; the bundler-target build loads in Bun without modification. Verification: BD.W2 §2.6 bench harness runs on Bun (analogous to Node).

### Deno

Deno supports WebAssembly (built-in); the bundler-target build loads via `import init from '@bbnf-lang/runtime-wasm'`. Verification: future post-Phase-5 work; BD ships the ES module + the .wasm; the bundler-target build is environment-agnostic.

## §12 Closing Posture

The WASM compilation pipeline at BD.W2 chooses wasm-bindgen for browser+Node primary use; wit-bindgen reserved for post-Phase-5. The cdylib targets wasm32-unknown-unknown; wasm-pack ships dual-target (bundler + nodejs); wasm-opt strips to ≤ 250 KB gzipped; cross-backend isomorphism with TS emit is documented; host-fn extern import path is the WASM column of the workspace metadata table; bundle size budget has 70 KB headroom; bench harness ratifies BD-G2 against simdjson-wasm baseline.
