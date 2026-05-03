# Tranche E — Per-grammar Declaration Crates + Runtime Template

## Gestalt

Tranche E is the convergent pivot. Per Pass B §6, three locks retire as one architectural movement: Lock 1 (tape dead — through direct-projection emit), Lock 13 (no god directories — through dissolution of the 17-child `runtime/` god directory), Lock 14 (full grammar generalisation — through per-grammar declaration crates). None lands without the others; all three land with one substrate identity: per-grammar declaration crates carrying template-emitted runtimes consuming the reshaped Emitter trait from tranche D, with direct-projection emit retiring the OpenFrame heap-stack across all 9 grammars.

`bbnf-runtime-template` lands at `crates/bbnf-runtime-template/` per Pass B §3 facility. The template consumes (grammar source + workspace metadata + per-grammar registry) and emits typed Rust per grammar — `<G>Value` enum, `<G>Document`, `<G>View`, `<G>Kind`, `<G>Arena`, `<G>StructBuilder` impl. The trivial cohort (bnf, csv, ebnf, css-pretty, math) emits 100% from template; the specialised cohort (bbnf-meta, json, css-l4, google-sheets) emits canonical surface from template + extension via per-grammar declaration crate's `specialised/` module.

The 9 per-grammar declaration crates scaffold and adopt: `crates/bbnf-meta/`, `crates/json/`, `crates/css-l4/`, `crates/google-sheets/`, `crates/bnf/`, `crates/csv/`, `crates/ebnf/`, `crates/css-pretty/`, `crates/math/`. Each carries `src/generated.rs` (relocated from `crates/core/src/grammar/generated/`), `src/runtime/` (template-emitted), `src/host.rs` (relocated host fns; CSS L4 takes `crates/core/src/css_types.rs`), `src/specialised/` (specialised cohort only), `tests/`, `benches/`, `Cargo.toml` per master plan §6.2 schema.

Direct-projection emit retires OpenFrame: parse fns hold partial state on the call stack; `SmallVec` carries element collections; arena owns interned compound IDs. The 86.07% samply share (per RESTART-SKETCH §A.7) collapses by mechanism, not by patching the Vec-clone. Per-grammar runtime hand-written files (~13K LOC across 5 trivial cohort grammars) retire entire; the specialised cohort retains `specialised/` for hand-written extensions only (CSS L4 colour functions, BBNF aggregator overrides, Sheets path-query canonical wiring).

This tranche is the substrate centerpiece. The largest single-tranche surface in the restart. Lock 14's verification command — adding a new grammar requires only metadata + grammar source + declaration crate, with ZERO code change in any other crate — passes here.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| `bbnf-runtime-template` substantive impl | E.W0 | `crates/bbnf-runtime-template/src/emit/{value, document, view, kind, arena, builder}.rs` populated; smoke-emit one grammar's runtime |
| 9 per-grammar declaration crates scaffold | E.W1 | `find crates/{bbnf-meta, json, css-l4, google-sheets, bnf, csv, ebnf, css-pretty, math}/Cargo.toml` returns 9 files; each compiles |
| Per-grammar `src/generated.rs` migrates | E.W2 | `find crates/core/src/grammar/generated/` returns nothing; `find crates/<g>/src/generated.rs` returns 9 files |
| Per-grammar `src/runtime/` template-emit | E.W3 | `cargo xtask regen` produces per-grammar `runtime/{value, document, view, kind, arena, builder}.rs`; trivial cohort emits 100% from template |
| Direct-projection emit retires OpenFrame | E.W4 | `rg -nE '\b(OpenFrame\|<\w+>StructCheckpoint\|JsonStructCheckpoint)\b' crates/{bbnf-meta, json, css-l4, google-sheets, bnf, csv, ebnf, css-pretty, math}/src/` returns 0; per-grammar parse benches show OpenFrame-departure regression |
| Per-grammar host.rs migrates | E.W2 | `find crates/core/src/css_types.rs` returns nothing; `find crates/css-l4/src/host.rs` matches; CSS L4 `parse_hex_color` resolves via the new path |
| Specialised cohort `specialised/` impl | E.W5 | `find crates/{bbnf-meta, json, css-l4, google-sheets}/src/specialised/` returns directories; trivial cohort has no `specialised/` |
| Cohort-template generator (Pass A facility #5) | E.W3 | `xtask/src/template/cohort/` populated; trivial cohort runtime emits from one template |
| Per-grammar parse smoke | E.W6 | `cargo nextest run -p <each grammar declaration crate>` passes; per-grammar `tests/parse.rs` round-trips the test fixture |
| Lock 14 verification gate | E.W7 | adding a new grammar to `[workspace.metadata.bbnf.grammars]` + creating its declaration crate + running `cargo xtask regen` produces a working parser with ZERO code change in any other crate; verified by integration test fixture |
| `runtime_invariants.rs` per-grammar contract test | E.W6 | `tests/runtime_invariants.rs` per declaration crate verifies: trait conformance + structural alphabet emission + path cursor presence + marker struct + parse entry + host-fn resolution |
| BBNF aggregator `pub use bbnf::*` retires | E.W4 | `rg 'pub use bbnf::\*' crates/bbnf-meta/src/` returns 0; BBNF accesses uniformly via namespaced path |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| E.W0 — `bbnf-runtime-template` substantive impl | template emit per master plan §4.10; consumes (grammar source + metadata + registry) and emits typed Rust per-grammar runtime modules | 3 parallel | template emits one grammar's runtime smoke |
| E.W1 — 9 per-grammar declaration crates scaffold | each declaration crate has Cargo.toml + src/lib.rs + tests skeleton; workspace metadata populates | 4 parallel | 9 crates compile (skeletal); workspace check green |
| E.W2 — Per-grammar `generated.rs` + `host.rs` migration | move generated.rs from `crates/core/src/grammar/generated/` to per-grammar declaration crate; move host fns (CSS L4 css_types.rs, etc.) | 4 parallel (per cohort batch) | generated.rs in 9 declaration crates; host.rs in specialised cohort |
| E.W3 — Per-grammar `runtime/` template-emit (cohort generator) | xtask emits per-grammar runtime modules from `bbnf-runtime-template`; cohort-template generator (xtask/src/template/cohort/) emits trivial cohort | 5 parallel (one per cohort grammar) | trivial cohort emits 100% from template; specialised cohort emits canonical surface |
| E.W4 — Direct-projection emit (OpenFrame retires) | parse fns hold partial state on call stack; SmallVec carries element collections; OpenFrame heap-stack retires entire | 4 parallel (per backend / per-grammar) | OpenFrame grep returns 0 across declaration crates; per-grammar parse smoke passes |
| E.W5 — Specialised cohort `specialised/` impl | bbnf-meta + json + css-l4 + sheets carry hand-written extensions only (CSS L4 colour funcs, BBNF aggregator, Sheets path-query, JSON specialised) | 4 parallel (one per specialised grammar) | specialised cohort tests pass; canonical surface from template + extension from `specialised/` |
| E.W6 — Per-grammar parse smoke + runtime invariants | per-grammar `tests/parse.rs` round-trips fixtures; `tests/runtime_invariants.rs` per Pass B Agent B.6 §6.1 | 3 parallel | all 9 declaration crates' tests pass |
| E.W7 — Lock 14 verification (new-grammar onboarding test) | integration test: add a synthetic 10th grammar via metadata + declaration crate + regen; ZERO code change required in other crates | 1 | onboarding gate passes |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| `bbnf-grammar`, `bbnf-parse`, `bbnf-ir`, `bbnf-passes` | C | C.W4 |
| 22-variant typed codegen IR + reshaped Emitter trait | D | D.W2 |
| `LayoutSink` impl per Rust backend | D | D.W3 |
| Rust lowerer smoke (BBNF round-trips) | D | D.W4 |
| Skeletal per-grammar declaration crates | A | A.W2 |
| `bbnf-runtime`, `bbnf-runtime-template`, `bbnf-host` skeletal | A | A.W2 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| 9 per-grammar declaration crates with template-emitted runtimes | F (optimiser pipeline benches per grammar), G (slice-borrow API per grammar), H (TS + WASM emit per grammar), J (cross-backend parity) | (continuous) |
| Direct-projection emit (OpenFrame-free) | F, G, J | F.W4, G.W2, J.W2 |
| Per-grammar `runtime_invariants.rs` contract | J | J.W2 |
| Lock 14 onboarding test gate | J | J.W4 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | substantively-honoured | E.W4 (direct-projection emit retires OpenFrame entire) |
| 2 — Layout canon | honoured | (continuous from C) |
| 3 — Cursor + byte-skip | substantive | E.W4 (cursor consult on EMPTY_PATH binding constant-folds) |
| 4 — Per-domain orthogonal | n/a | (deferred to F) |
| 5 — IR + per-backend | honoured | (continuous from D; bbnf-runtime-template consumes the typed IR) |
| 6 — xtask source emit | substantively-honoured | E.W3 (xtask emits per-grammar runtime; runtime-template subcommand) |
| 7 — `crates/path/` consolidated | honoured | (continuous from C) |
| 8 — Surpass SOTA | partial | E.W4 (direct-projection emit substantively retires the 86.07% samply share; full SOTA gates land at F + G + J) |
| 9 — Slice-borrow primary | partial | E.W4 (per-grammar runtime consumes slice-borrow primary; full API at G) |
| 10 — Pratt + SIMD auto-detected | n/a | (deferred to F) |
| 11 — Path-deps for sister crates | honoured | (continuous) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | substantively-honoured | E.W3 (`runtime/` god directory dissolves; 9 per-grammar declaration crates each cohesive) |
| 14 — Full grammar generalisation | substantively-honoured | E.W3 + E.W7 (per-grammar declaration crates + onboarding test) |

The convergent pivot — Lock 1 + Lock 13 + Lock 14 retire together at E.W3-W4. None lands without the others.

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| `bbnf-runtime-template` emits incorrect typed Rust for one grammar | E.W2 smoke gate per grammar: emitted runtime parses test fixture per `bbnf-test-fixtures::fixture::<grammar>`; per-grammar parity matrix; per master plan §13 R7 |
| Direct-projection emit retires OpenFrame but introduces correctness regression | E.W4 per-grammar regression suite vs prior tape-era output; nested-depth fixtures (citm_catalog), array-heavy fixtures (canada); per §13 R8 |
| Per-grammar declaration crate Cargo.toml missing `[workspace.metadata.bbnf]` reference | E.W3 `cargo xtask validate-metadata --check` CI gate; per §13 R9 |
| Cohort-template generator regression: trivial cohort emits non-equivalent code | E.W3 generator regression budget per master plan §12.3: ±15% per grammar; overflow blocks wave |
| Specialised cohort `specialised/` divergence between grammars | E.W5 per-grammar audit: each specialised grammar's `specialised/` carries one cohesive concern (CSS L4 colour funcs, BBNF aggregator, etc.); cross-grammar consistency check |
| Lock 14 onboarding test fails because hidden grammar coupling persists | E.W7 staged: synthetic 10th grammar uses simplest possible source; if onboarding fails, the test surface point of remaining coupling fires triumvirate |
| BBNF aggregator `pub use bbnf::*` retire breaks downstream | E.W4 staged: namespace audit + downstream rename + `cargo check --workspace` |
| Generated-LOC budget regression in E (large reduction expected) | E budgets -13K LOC reduction; if budget exceeded (more reduction than expected), audit confirms substantive vs accidental | per master plan §12.3: ±15% per grammar; verified per wave |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| `cargo xtask regen` for one grammar | ≤ 30s | E.W3 per-grammar |
| Per-grammar runtime template emit | ≤ 5s incremental | E.W3 |
| Per-grammar parse smoke `cargo test -p <g>` | ≤ 60s | E.W6 |
| Generated-LOC budget | E.exit: 145,750 LOC (-13K vs. D.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14. Tranche E's prose register: unpretentious-academic with mild lilt at ~5%; the convergent pivot invites narrative weight ("hereupon the substrate's bone structure tightens"; "the OpenFrame machinery retires entire"); domain verbiage from compiler theory + Romantic-era musical idiom (the convergent pivot has a cadential character).

## Closing posture

Tranche E closes with the substrate centerpiece in place. The 9 per-grammar declaration crates carry their generated parsers + template-emitted runtimes; the Lock 14 onboarding test passes; OpenFrame retires; the 86.07% samply share collapses by mechanism. Tranches F, G, H land in compatible parallel thereafter — the optimiser pipeline (F), the slice-borrow API (G), the TS+WASM emitters (H) — each consuming the substrate E settled.

The greenfield mandate carries: no quick solutions in template emission (the template is grammar-agnostic by construction); no workarounds in OpenFrame retirement (direct-projection emit is the architectural pivot, not a checkpoint optimisation); the hand-written per-grammar runtime files retire entire (no carry-forward).

Hereupon the convergent pivot completes. Lock 1 + Lock 13 + Lock 14 honour together.
