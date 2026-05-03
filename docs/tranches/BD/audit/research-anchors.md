# BD Research Anchors

Date: 2026-05-03
Scope: Primary-source research for BD's TS native bindings, WASM compilation, sister-crate publication, worktree fixtures, and cross-backend parity. Cited at path:line where loaded from local repo; URL otherwise.

## §1 — TS Native Bindings (NAPI-RS)

### NAPI-RS architecture (https://github.com/napi-rs/napi-rs, https://napi.rs/docs/introduction/getting-started)

NAPI-RS is a "framework for building compiled Node.js add-ons in Rust via Node-API"; it eliminates node-gyp. Architecture: cargo crate with `crate-type = ["cdylib"]`, deps on `napi` + `napi-derive`, `build.rs` invoking `napi_build::setup()`. The N-API ABI is version-stable across Node 12-22 — same compiled binary runs on every Node release in the supported window without recompile. Macros: `#[napi]` exports; `#[napi(constructor)]`, `#[napi(getter)]`, `#[napi(setter)]`, async via `#[napi]` returning `napi::Result<T>` for any `T: ToNapiValue`.

### npm packaging via @napi-rs/cli

`napi new` scaffolds. `napi build` produces `.node` files per platform. The publishing pattern is **per-platform sub-package** under a scope:

- **Main package**: `@scope/pkg` ships JS loader + TS types
- **Platform packages**: `@scope/pkg-darwin-arm64`, `@scope/pkg-darwin-x64`, `@scope/pkg-linux-x64-gnu`, `@scope/pkg-linux-arm64-gnu`, `@scope/pkg-linux-x64-musl`, `@scope/pkg-win32-x64-msvc`, `@scope/pkg-win32-arm64-msvc`, `@scope/pkg-freebsd-x64` — each ships only its `.node` binary
- **Loader logic**: main `index.js` introspects `process.platform` + `process.arch` + libc family (gnu/musl) and `require`s the matching sub-package, which is listed as `optionalDependencies` on the main package

The platform matrix is "macOS x64+aarch64, Linux x64/aarch64/arm/powerpc64le/s390x/loong64/riscv64 (gnu+musl variants), Windows x64/x86/arm64, FreeBSD x64, Android variants".

### Bun + Deno N-API support

- **Bun**: implements N-API; per Bun docs at https://bun.sh/docs/runtime/nodejs-apis, native addon loading works for the napi-rs ecosystem (the bun runtime exposes the Node-API ABI). This is the same loading path as Node 20+.
- **Deno**: runtime FFI is `Deno.dlopen` per https://docs.deno.com/runtime/manual/runtime/ffi_api — accepts dynamic libraries with C-compatible signatures (i8/u8/i16/u16/i32/u32/i64/u64/f32/f64/pointer/buffer/struct), `--allow-ffi --unstable` permission required. NAPI-RS's `.node` files do NOT load via `Deno.dlopen` directly because NAPI-RS produces N-API-shaped exports, not bare C-ABI exports. For Deno, BD ships either a separate cdylib with C-ABI exports OR uses Deno's N-API compatibility layer (Deno 2.x adds N-API; pre-2.x Deno cannot load napi-rs binaries). **Decision**: BD ships NAPI-RS binaries primarily; Deno consumers use Deno 2.x N-API compat OR the WASM build.

### swc napi pattern

swc/crates/binding_napi_* is the canonical large-codebase NAPI-RS reference. Pattern: per-binding-target crate (`binding_core_node`, `binding_core_wasm`, etc.), each with its own `crate-type = ["cdylib"]`, each consuming the shared swc compiler crate as a path-dep. The proc-macro `#[napi]` wraps Rust fns; the cdylib exports those fns via N-API. The npm package `@swc/core` ships per-platform sub-packages.

### Workspace cdylib pattern

The cargo-workspace pattern for path-ts (BC.W2 → BD.W0):

```toml
# crates/path-ts/Cargo.toml
[lib]
crate-type = ["cdylib"]

[dependencies]
path-core = { path = "../path-core" }
napi = { version = "2", default-features = false, features = ["napi6"] }
napi-derive = "2"

[build-dependencies]
napi-build = "2"
```

The cdylib is npm-packaged; `path-core` is the Rust path AST + compile logic shared with the path crate (Rust-side proc-macro). This isolation per Lock 7 (`crates/path-ts/` separate from `crates/path/` because Rust toolchain forbids proc-macro path-dep sharing).

## §2 — WASM Compilation

### wasm-bindgen (https://github.com/rustwasm/wasm-bindgen)

wasm-bindgen "facilitates interactions between WebAssembly modules and JavaScript" via the `#[wasm_bindgen]` macro. Three packages: `wasm-bindgen` (core), `js-sys` (JS standard library bindings), `web-sys` (Web API bindings). Performance posture: "lightweight" — generates bindings only for "JavaScript imports you actually use and Rust functionality that you export". The crate-type for browser/Node is `cdylib` targeting `wasm32-unknown-unknown`. Output: `.wasm` binary + JS shim file pair.

### wasm-pack (https://github.com/rustwasm/wasm-pack)

Build pipeline cdylib → npm:

1. `cargo build --target wasm32-unknown-unknown --release` produces the `.wasm` cdylib
2. wasm-bindgen post-processor generates JS shims + TS type definitions
3. wasm-pack assembles the npm package layout: `pkg/` directory with `package.json`, `<name>.js`, `<name>_bg.wasm`, `<name>.d.ts`

Target options:
- `--target bundler` (default): emits ES modules consumable by webpack/rollup/esbuild
- `--target web`: standalone ES module loadable via `<script type="module">` directly in browser
- `--target nodejs`: CommonJS module loadable via `require()`
- `--target no-modules`: classic-script global, no bundler

### wit-bindgen + Component Model (https://github.com/bytecodealliance/wit-bindgen)

wit-bindgen targets the WebAssembly Component Model. Status: pre-1.0; "0.X.Y; possibly-API-breaking changes". Supported guests: Rust (via `wasm32-wasip2`), C/C++, C#, Go, MoonBit. Hosts: wasmtime (Rust/Python/Ruby), jco (JS), Java. **Not browser-focused**: targets server/embedded WASM.

**Decision**: BD chooses wasm-bindgen (mature; browser-targeted; npm-publishable). wit-bindgen is reserved for a future post-Phase-5 Component Model emergence; BD does not use it. Rationale: bbnf's WASM consumer surface is browser+Node, not the wasi-component substrate.

### wasm-tools

The optimisation toolchain: `wasm-opt` (binaryen) post-processes the cdylib; `wasm-tools validate` verifies the output. wasm-pack invokes wasm-opt automatically on `--release`.

### simdjson-wasm + simd-json benchmarks

For BD-G2 ratification, the WASM JSON parser baseline is V8's WebAssembly engine running compiled JSON parsers. Public benchmarks on twitter.json:

- simdjson-wasm: ~3-5x slower than simdjson native (no SIMD in baseline WASM; SIMD128 helps)
- simd-json compiled to wasm32: ~2.5-4x slower than native simd-json (wasm32 lacks AVX2)

So the BD-G2 floor for `parse(twitter.json)` is bounded by 4× the BC-G1 native target (380 µs × 4 ≈ 1.52 ms) — reasonable upper bound. With wasm-bindgen overhead (~100 ns per JS↔WASM transition × handful of transitions per parse) the practical floor is ~1.5-2.5 ms. **BD-G2 gate**: ≤ 2.5 ms on M1 Pro Node 20.x with `--release` + wasm-opt.

## §3 — Sister-Crate Publication

### cargo-release (https://github.com/crate-ci/cargo-release)

Extends `cargo publish` with: validation, version management, tagging, pushing. Workspace support: `--workspace`, `--exclude`, `--package`. "Updates dependent crates in workspace when changing version" — the publish ordering is computed automatically from path-dep graph (leaves first, roots last). Pre-release hooks: changelog generation, version-bump propagation. CHANGELOG style is consumer-customised.

### cargo-semver-checks (https://github.com/obi1kenobi/cargo-semver-checks)

Detects semver violations via rustdoc JSON analysis. Catches:
- Removed pub items (functions, types, traits)
- Added `#[must_use]` on existing fns
- Function signature changes (params, return type)
- Trait def/impl modifications
- Visibility reductions

**Limitations**: doesn't catch every semver break (field type changes, generic param changes, lifetime changes are gaps). Version-pinned to rustdoc JSON format. Invocation: `cargo install cargo-semver-checks --locked`; `cargo semver-checks --baseline-version <v>` against published version.

### docs.rs build pipeline

Required Cargo.toml metadata for clean docs.rs build:
- `description` (required for crates.io + docs.rs)
- `license` (required)
- `repository`, `homepage`, `documentation`
- `keywords` (≤ 5), `categories` (must match crates.io's category list)
- `readme = "README.md"` (or path)
- `[package.metadata.docs.rs]` block: `all-features = true`, `targets = [...]` for cross-platform docs
- `package.exclude` directive removes git directories, build artefacts from the published tarball

Doc-test surface: `cargo doc --no-deps -p <crate>` produces the rustdoc HTML; doc-tests run via `cargo test --doc`. docs.rs builds with `--all-features` by default; doc-tests in private modules don't run.

### Publication ordering for BD.W3

Sister-crate path-dep graph (per `audit/MODULES-2026-05-03.md` + Lock 11):

```
egraph-derive  → (none external)            depth 0
egraph         → egraph-derive              depth 1
csp-solver     → (none external)            depth 0
bbnf-regex     → (none external)            depth 0
parse-that     → bbnf-regex (via path-dep)  depth 1   [BC.W5c gap-I]
```

Publication order (cargo-release auto-computes): egraph-derive, egraph, csp-solver, bbnf-regex; parse-that disposition per BC.W5c gap-I (likely permanent path-dep per surgery 33 default, or option (ii) deferred publication with named gate).

## §4 — Worktree Fixture Infrastructure

### Carry context

BC.W5 §2.7 (`docs/tranches/BC/waves/W5.md:79-86`) lands `xtask worktree-init` to materialise `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` per grammar. BC→BD.C3 carries the parallel-agent dispatch infrastructure. BD's role: extend the fixture infrastructure to support CI matrix expansion (Rust + TS + WASM per grammar) and per-grammar test fixtures evolving with grammar source.

### Patterns surveyed

- **git submodule**: heavyweight; per-fixture commit pinning; nested `.git` directories; difficult to atomic-update
- **git subtree**: easier than submodule; merge-only; difficult to keep in sync without losing local edits
- **path-dep cargo workspace**: chosen — per-grammar fixture crate at `crates/<grammar>/tests/fixtures/`, `[dev-dependencies] <grammar>-fixtures = { path = "tests/fixtures" }`. Fixture data lives alongside grammar source; CI matrix runs `cargo test -p <grammar>` against fixture inputs

**Decision for BD.W4**: path-dep workspace pattern. Per-grammar `tests/fixtures/` directory ships canonical inputs (e.g., `data/json/twitter.json` becomes `crates/bbnf-parse/tests/fixtures/json/twitter.json` referenced via `include_bytes!`). xtask materialises fleet-wide via symlinks where appropriate. The CI matrix per BD.W5 runs Rust + TS + WASM parity per grammar.

## §5 — Cross-Backend Parity

### Parity verification matrix

For each grammar (9 grammars per `audit/MODULES-2026-05-03.md:625-628`):

| Backend | Inputs | Output | Equivalence |
|---|---|---|---|
| Rust | `<grammar>::parse(&input) -> <Grammar>Document<'_>` | typed AST in arena | bit-equal across runs |
| TS | `parse<Grammar>(input: Uint8Array) -> <Grammar>Value` | typed JS value sum | byte-equal serialisation modulo float-repr |
| WASM | `<grammar>_parse(ptr, len) -> ptr` | typed struct in linear memory | byte-equal at marshal boundary |

**Equivalence relation**: serialise each backend's output to canonical JSON; assert byte-equal except for float representation (which differs between Rust's `f64` Display and JS's `Number.prototype.toString`). Float comparisons use `Math.abs(a - b) < f64::EPSILON * max(|a|, |b|, 1.0)`.

### Gate ratification

BD-G4 (cross-backend parity): for each of 9 grammars × 3-5 fixtures per grammar = 27-45 parity tests, all pass. CI matrix: `cargo test -p bbnf-parse --test parity_<grammar>` on Rust; `npm test -w @bbnf-lang/runtime` on TS; `wasmtime run parity-test.wasm` on WASM.

## §6 — Performance Anchors

| Anchor | Source | Number |
|---|---|---|
| sonic-rs M1 Pro twitter | `audit/SOTA-2026-05-03.md:50-58` | 436 µs (parse-to-typed-struct) |
| simdjson On-Demand twitter | `audit/SOTA-2026-05-03.md:88` | ~2.2 GB/s (~290 µs for 631KB twitter.json) |
| simdjson-wasm twitter | research §2 | ~3-5× simdjson native ⇒ ~870 µs - 1.45 ms |
| lightningcss bootstrap.css | `audit/SOTA-2026-05-03.md:131-136` | 4.16 ms |
| BC native target (BD baseline) | `docs/tranches/BC/BC.md:15` | 380 µs JSON twitter |

## §7 — Closing posture

BD's research load: the largest of any Phase-4 agent. The above primary sources fix the deliverable boundary:

1. **TS bindings**: NAPI-RS for the `path-ts` proc-macro shell + `@bbnf-lang/runtime` per-platform npm bundle
2. **WASM bindings**: wasm-bindgen + wasm-pack pipeline; npm publication via `--target bundler` + `--target nodejs` dual-build
3. **Sister-crate publication**: cargo-release for ordering; cargo-semver-checks for breakage detection; docs.rs metadata enrichment
4. **Worktree fixtures**: path-dep workspace per-grammar fixture pattern; xtask materialises symlinks where needed
5. **Cross-backend parity**: serialise-and-compare; CI matrix per grammar × backend

Every BD deliverable derives from a primary-source mechanism. Zero "investigate later"; zero "TBD". The plan is decided in-document.
