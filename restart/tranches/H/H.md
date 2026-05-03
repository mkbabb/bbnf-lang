# Tranche H — TS + WASM Emitters

## Gestalt

Tranche H activates the TS + WASM backends per Lock 5's IR-and-per-backend-lower contract. The reshaped Emitter trait from tranche D (8-10 per-shape methods) enables both backends to walk the same per-shape pattern as Rust — `emit_alt`, `emit_seq`, `emit_repeat`, `emit_ref`, `emit_lit`, `emit_regex`, `emit_map`, plus auxiliaries. The `bbnf-codegen::ts` and `bbnf-codegen::wasm` modules activate; per-grammar TS source emits at `<output>/<grammar>.ts`; per-grammar WASM cdylib builds at `<output>/<grammar>.wasm`. Cross-backend smoke: at least one grammar (BBNF + JSON, ideally also CSS L4) emits valid TS + WASM on top of the proven Rust path from tranche D.

The TS emitter consumes the same `LayoutSink` trait that Rust does (Pass A facility #8); the WASM emitter consumes the same. Per-backend divergence — TS expresses arena via TypedArray + offset; WASM expresses arena via linear memory + Index — is encoded in the per-backend `LayoutSink` impl, NOT in the codegen IR.

The `crates/path-ts/src/` cdylib activates per Lock 7 — TS callers receive `pointer![...]`-equivalent template tag plus `bbnf-runtime`-equivalent `parse / parse_in / parse_owned` (via JS lifetime conventions: arena via `BumpArena` JS class; owned via plain object).

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| TS emitter substantive activation | H.W0 | `crates/bbnf-codegen/src/ts/` produces TS source for at least 3 grammars; smoke-tests in `vitest` |
| WASM emitter substantive activation | H.W1 | `crates/bbnf-codegen/src/wasm/` produces WASM cdylib for at least 3 grammars; smoke-tests in `wasm-pack test` |
| Cross-backend parity: Rust ↔ TS ↔ WASM | H.W2 | one input → three backends → identical typed-tree (tested per grammar; specialised cohort tested explicitly; trivial cohort tested in batch) |
| `path-ts` cdylib activates | H.W3 | TS callers consume `pointer["a", "b", 1]` template tag; round-trips through `bbnf-runtime` equivalent |
| Lock 5 verification | H.W3 | `rg 'fn emit_for_rust\|fn emit_for_ts\|fn emit_for_wasm' crates/bbnf-codegen/src/{rust, ts, wasm}/` produces matching trait method counts; per-backend lower shares Emitter trait |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| H.W0 — TS emitter activation | `bbnf-codegen::ts::lower` produces TS source for BBNF + JSON + CSS L4 | 2 parallel | TS source compiles + smoke-tests in vitest |
| H.W1 — WASM emitter activation | `bbnf-codegen::wasm::lower` produces WASM cdylib for BBNF + JSON + CSS L4 | 2 parallel | WASM compiles + smoke-tests in wasm-pack test |
| H.W2 — Cross-backend parity | one input → three backends → identical typed-tree per grammar | 3 parallel (per-grammar batches) | parity matrix passes |
| H.W3 — `path-ts` cdylib + Lock 5 verification | TS template tag + cdylib activate; Lock 5 audit | 2 parallel | TS pointer round-trips; Lock 5 trait method count match |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| 22-variant codegen IR + Emitter trait | D | D.W2 |
| 9 per-grammar declaration crates | E | E.W3 |
| `parse / parse_in / parse_owned` API | G | G.W2 |
| `pointer![...]` macro + path-core | G | G.W2 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| TS + WASM emitters integrated | I (publication may include TS pkg), J (cross-backend parity matrix) | J.W3 |
| Cross-backend parity proven | J | J.W3 |
| `path-ts` cdylib | J | J.W3 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | honoured | (continuous) |
| 2 — Layout canon | honoured | (continuous) |
| 3 — Cursor + byte-skip | honoured | (continuous) |
| 4 — Per-domain orthogonal | honoured | (continuous) |
| 5 — IR + per-backend | substantively-honoured | H.W3 (TS + WASM consume same Emitter trait) |
| 6 — xtask source emit | honoured | (continuous) |
| 7 — `crates/path/` consolidated | substantively-honoured | H.W3 (path-ts cdylib activates; Lock 7 triplet operational across backends) |
| 8 — Surpass SOTA | partial | (continuous; full set at J) |
| 9 — Slice-borrow primary | honoured | (continuous from G; TS + WASM honour via JS lifetime conventions) |
| 10 — Pratt + SIMD auto-detected | honoured | (continuous from F; per-backend SIMD: WASM uses simd128 where eligible) |
| 11 — Path-deps for sister crates | honoured | (continuous) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | honoured | (continuous) |
| 14 — Full grammar generalisation | honoured | (continuous; TS + WASM emitters carry zero grammar-named module) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| TS + WASM emitters drift from Rust emit due to backend-specific divergence | H.W2 cross-backend parity matrix; per-shape walking pattern enforced via Emitter trait constraint; per master plan §13 R14 |
| WASM cdylib build complexity | H.W1 staged: wasm-pack toolchain confirmed; minimal test grammar (BBNF) builds first; expand to JSON + CSS L4 |
| TS emit output bloat (CSS L4 may emit ~50K LOC TS) | H.W0 generated-LOC budget per master plan §12.2: per-grammar TS LOC bounded by declared budget |
| `path-ts` cdylib JS interop complexity | H.W3 staged: path-ts skeleton compiles; minimal pointer round-trip; expand to full visitor surface |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Per-grammar TS emit time | ≤ 30s per grammar | H.W0 |
| Per-grammar WASM build time | ≤ 60s per grammar | H.W1 |
| Cross-backend parity test | ≤ 5min total | H.W2 |
| Generated-LOC budget | H.exit: 173,750 LOC (+30K TS vs. G.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14.

## Closing posture

Tranche H closes with all three backends operational. Rust + TS + WASM share the Emitter trait, the `LayoutSink` consumer, the per-shape walking pattern. Cross-backend parity passes per grammar. The `path-ts` cdylib activates; the Lock 7 triplet is operational across backends.

The greenfield mandate carries: no per-backend bespoke walking (one Emitter trait); no per-backend grammar-named code (Lock 14 honoured across backends); the IR contract is the substrate (Lock 5).
