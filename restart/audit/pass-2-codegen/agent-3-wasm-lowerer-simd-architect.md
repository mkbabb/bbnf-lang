# PASS-2 Agent 3: WASM Lowerer + SIMD Architect

## §1 Scope + Framing

Lens: define WASM V1 lowering and SIMD scanner integration under PASS-2. PASS-2 owns WASM V1 and SIMD scanner kernels while TS is deferred (`restart/prompts/PASS-2-CODEGEN.md:3`). The current backend has a WAT scaffold, not a production WASM path: `crates/core/src/backend/wasm/mod.rs` describes WebAssembly text format modules (`crates/core/src/backend/wasm/mod.rs:1-5`), and the WASM emitter implements the broad `Emitter` trait with string output (`crates/core/src/backend/wasm/emitter/mod.rs:19-22`).

The greenfield position is a two-layer WASM lowerer. Backend IR lowers to Rust parser core in the same shape as native Rust, then PASS-2 emits a wasm32 binding layer that exposes stable pointer/length or wasm-bindgen APIs. Raw WAT remains a smoke/ABI fixture only. BD.W2's planned production path also says the scaffold graduates to wasm-bindgen-bound Rust source and raw WAT is not the production path (`docs/tranches/BD/waves/W2.md:38-62`).

SIMD is first-class. README names NEON, AVX2, AVX512, WASM-SIMD, and scalar targets (`restart/README.md:324-340`). The existing `simd-scan` crate already has a clean dispatch shell with NEON, AVX2, AVX512, WASM, and scalar modules (`crates/simd-scan/src/lib.rs:19-29`) and a runtime dispatch path (`crates/simd-scan/src/lib.rs:70-114`). This report keeps that crate, but makes Backend IR the sole way grammars configure it.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| WASM via wasm32 Rust core | BD.W2 names wasm-bindgen source as production path (`docs/tranches/BD/waves/W2.md:38-62`). | Current WASM emitter emits WAT strings (`crates/core/src/backend/wasm/emitter/mod.rs:19-218`). | Reuse Rust lowerer's BIR-to-parser core, with WASM-specific ABI, exports, imports, and memory ownership. | Do not duplicate parser logic in raw WAT. | REINVENT. |
| Raw WAT scaffold | Useful for tiny ABI smoke and snapshot debugging. | Not suitable for Tape-backed typed parser emission. | Keep as `wasm/smoke_wat.rs`, not as production lowerer. | No PASS-2 gate may depend solely on WAT. | KEEP-MODIFY. |
| WASM SIMD scanner | `simd-scan` has a WASM module and SIMD128 baseline notes (`crates/simd-scan/src/wasm.rs:1-14`, `crates/simd-scan/src/wasm.rs:32-34`). | Browser support and Node flags can differ. | Compile `simd-scan` for wasm32 with scalar fallback and feature-gated SIMD128. | Parity before speed; scalar fallback must be byte-identical. | KEEP. |
| Structural alphabet | Existing alphabet is data-driven, with no per-grammar code (`crates/simd-scan/src/alphabet.rs:1-18`). | Backend currently can configure scan directly from grammar-facing code. | Backend IR emits `StructuralAlphabet` from PASS-1 shape analysis. | No grammar names in `simd-scan`. | KEEP-MODIFY. |
| Host functions in WASM | BD.W2 sketches extern imports for host fns (`docs/tranches/BD/waves/W2.md:165-183`). | Per-grammar host crates are disallowed by default (`restart/README.md:13-25`). | BIR host table lowers to import table entries and generated JS glue only when external `@host fn` is required. | Generic host primitives must compile in wasm32 when possible. | REINVENT. |
| WASM perf gate | README sets WASM in scope and BD.W2 names twitter ≤2.5ms (`restart/README.md:324-340`, `docs/tranches/BD/waves/W2.md:5`). | PASS-2 cannot claim full BD parity because TS production is deferred. | PASS-2 gate is JSON smoke plus scanner throughput; BD parity gate waits. | Do not overclaim 81-cell matrix. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **WASM lowerer consumes Backend IR.** The current WASM emitter's trait implementation must be replaced with a BIR consumer. This matches Lock 5 and prevents a second grammar-walking backend (`restart/locks/LOCKS.md:42`).

2. **Linear-memory ABI mirrors Tape.** WASM parser exports accept `(ptr, len)` or `&[u8]` through wasm-bindgen and return an owned result handle or serialized result depending on PASS-3 API. Internally the parser builds Tape and typed views exactly like native Rust. BD.W5 describes WASM output as typed struct in linear memory for parity purposes (`docs/tranches/BD/waves/W5.md:14-18`); PASS-2 gives that memory layout a Tape-backed core.

3. **SIMD scanner is a sibling substrate, not per-grammar code.** `StructuralAlphabet` is emitted from BIR and consumed by `simd-scan`; the crate remains generic. Lock 14 forbids grammar match arms in generic crates (`restart/locks/LOCKS.md:60`).

4. **WASM SIMD and scalar are byte-identical.** `scan_structural` already centralizes dispatch with scalar fallback (`crates/simd-scan/src/lib.rs:70-114`). PASS-2 adds cross-kernel snapshot fixtures for each emitted alphabet.

5. **TS is a scaffold-only consumer.** The BIR is designed for Rust, TS, and WASM, per README (`restart/README.md:104-117`), but PASS-2 marks TS production as deferred by the PASS-2 prompt (`restart/prompts/PASS-2-CODEGEN.md:3`).

## §4 New Facilities Proposed

| Facility | Explication | Gate |
|---|---|---|
| `codegen/src/lower/wasm/mod.rs` | BIR-to-wasm32 binding planner. Emits wrapper source, import table, export table, memory ownership glue. | `cargo check --target wasm32-unknown-unknown -p runtime` for JSON smoke. |
| `codegen/src/lower/wasm/abi.rs` | Stable pointer/length/result-handle ABI. | ABI snapshot includes exported function names and import names. |
| `codegen/src/lower/wasm/host.rs` | Maps BIR `HostCall` chains to generic primitives or extern imports. | Host table length equals BIR host table length. |
| `codegen/src/lower/wasm/simd.rs` | Emits `StructuralAlphabet` constants and feature gates. | WASM SIMD and scalar scans return identical structural indexes on corpus samples. |
| `simd-scan` kernel parity fixtures | Per-alphabet byte fixtures and expected index lists. | `cargo test -p simd-scan` across scalar plus native available kernels. |

The SIMD alphabet shape already has clear kernel categories: empty, nibble LUT, wide LUT, and multi-compare (`crates/simd-scan/src/alphabet.rs:98-125`). PASS-2 should use these categories as BIR cost outputs, not duplicate kernel selection in lowerers.

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1 must compute structural alphabets and cost thresholds. README places shape mining and cost extraction before Backend IR (`restart/README.md:188-217`). PASS-2 receives `SimdPlan { alphabet, kernel_shape, expected_hits, fallback }` and emits scanner constants.

PASS-3 receives WASM API choices: package surface, JS init flow, result serialization, and parity runner. BD.W2's package layout and npm details belong downstream (`docs/tranches/BD/waves/W2.md:87-123`), while PASS-2 must leave enough binding metadata for PASS-3 to package without rewriting parser internals.

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| WASM lowerer becomes a raw WAT parser generator | Duplicates Rust logic and diverges | Make wasm32 Rust source the production path; WAT limited to smoke snapshots. |
| SIMD kernel differs from scalar | Incorrect parse offsets | Per-alphabet parity tests compare index vectors across scalar and active SIMD kernels. |
| Host import ABI becomes per-grammar | Violates Lock 14 | Import names come from BIR host table and metadata, not handwritten grammar branches. |
| WASM memory ownership leaks | Browser/Node consumers see unstable handles | Emit ownership helpers and PASS-3-facing result handles from one ABI module. |
| PASS-2 claims BD.W5 parity early | False gate closure | PASS-2 closes WASM smoke and scanner parity only; 81-cell parity is BD.W5 (`docs/tranches/BD/waves/W5.md:181-217`). |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| Current WAT backend | Tiny smoke WAT generation as diagnostic fixture (`crates/core/src/backend/wasm/mod.rs:1-5`). | Production WASM source generation through wasm32 Rust binding planner. | Broad WAT string emitter as main backend. |
| BD.W2 | wasm-bindgen and pointer/length direction (`docs/tranches/BD/waves/W2.md:38-62`, `docs/tranches/BD/waves/W2.md:165-183`). | Re-anchor crate names and host model to current README. | Per-grammar parser crates. |
| `simd-scan` | Existing architecture and arch dispatch (`crates/simd-scan/src/lib.rs:19-29`). | Configure only via BIR structural alphabets. | Grammar-specific scanner code. |
| SOTA | simdjson structural scan throughput target (`restart/corpora/SOTA.md:73-89`). | Use for grammar-specific alphabets and Tape offsets. | Separate DOM-like tape with no typed views. |

## Wave 2 correction note

This agent's perf trajectory (agent-3 §1, line 20 cited by HARDENING-PASS-2 punch item 5) is superseded by PASS-2.md §7's row-complete SOTA table whose columns are `Competitor / Dataset / Platform / bbnf target / PASS-2 mechanism / Evidence artefact`. Mechanism-only rows (OpenFrame deletion, Pratt auto-detection, WASM parity) are demoted to a sister mechanism-gate table per HARDENING-CONSOLIDATED §4.29. The wasm32 binding-path obligation this agent carries remains intact; the SOTA gate routing is corrected.
