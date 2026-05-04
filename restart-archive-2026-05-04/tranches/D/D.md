# Tranche D — Codegen IR Contract

## Gestalt

Tranche D lands the codegen IR contract — the substrate every backend lowerer consumes per Lock 5. The 22-variant typed codegen IR derives from Phase-4 BC.W0's substrate (the Phase-4 directive's typed-IR specification) and lives at `crates/bbnf-codegen-ir/`. The `Emitter` trait reshapes from 30 methods to 8-10 per Pass B §2.c — Rust + TS + WASM all walk the same per-shape pattern (`emit_alt`, `emit_seq`, `emit_repeat`, `emit_ref`, `emit_lit`, `emit_regex`, `emit_map`, plus auxiliary `emit_grammar_header`, `emit_rule_signature`, `emit_dispatch`). The `LayoutSink` impl bridges per-backend, consuming the `Layout` from `bbnf-ir::registry::sink` and producing per-backend output. The Rust lowerer smoke test passes — at least one grammar (BBNF or JSON) round-trips through `bbnf-codegen-ir` → `bbnf-codegen::rust::lower` → emitted source compiling. The struct_direct sub-modules (4 files, ~2500 LOC) retire per Pass B §1.a — the orthogonal-codepath fault dissolves; per-shape methods replace the sub-naming. The substrate.rs decision file retires per Pass B §1.a — substrate selection is monocular post-Lock-1.

This is the contract tranche. Every backend tranche thereafter (H for TS+WASM activation) consumes this contract. Per Lock 5 the IR contract document at `docs/spec/codegen.md` lands here as the first cross-backend reference.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| 22-variant typed codegen IR lands | D.W0 | `crates/bbnf-codegen-ir/src/ir/{mod, variants, lower}.rs` populated; 22 variants exhaustive per Phase-4 BC.W0; type-system audit passes |
| Emitter trait reshape (30 → 8-10 methods) | D.W2 | `bbnf-codegen-ir/src/emitter.rs::Emitter` carries ≤ 10 methods; sub-naming `struct_direct` retires; per-shape methods replace |
| `LayoutSink` impl per backend | D.W3 | `bbnf-codegen/src/rust/emitter/sink.rs` impl `LayoutSink for RustEmitter`; smoke-test consumes one grammar's `Layout` and produces typed-Rust source |
| Rust lowerer smoke | D.W4 | `bbnf-codegen::rust::lower(grammar) -> TokenStream` produces source for at least one grammar (BBNF) that compiles when re-fed to `cargo check` |
| struct_direct sub-modules retire | D.W2 | `find crates/bbnf-codegen/src/rust/emitter/shapes/struct_direct/` returns nothing; per-shape methods carry the prior content |
| substrate.rs retires | D.W2 | `find crates/bbnf-codegen/src/rust/emitter/shapes/substrate.rs` returns nothing |
| `docs/spec/codegen.md` lands | D.W4 | document at `docs/spec/codegen.md` cites IR contract + per-backend lowering + invariants |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| D.W0 — 22-variant typed codegen IR | `crates/bbnf-codegen-ir/src/ir/{mod, variants, lower}.rs` lands; type-system audit passes | 1 | exhaustive variant coverage; lower function signature stable |
| D.W1 — `bbnf-codegen` content migration | Move `crates/core/src/{backend, generate}` to bbnf-codegen; preserve module structure but flatten god directory per master plan §4.9 | 4 parallel | bbnf-codegen compiles; per-backend trees populate (`rust/`, `ts/`, `wasm/`) |
| D.W2 — Emitter trait reshape | 30-method trait collapses to 8-10 per-shape methods; struct_direct sub-modules retire; substrate.rs retires; per-shape methods replace | 2 parallel | Emitter trait method count ≤ 10; struct_direct grep returns 0 |
| D.W3 — Per-backend LayoutSink wiring | RustEmitter impl LayoutSink; TSEmitter impl LayoutSink stub; WasmEmitter impl LayoutSink stub | 3 parallel (per backend) | per-backend stubs compile; Rust impl substantive; TS + WASM stubs |
| D.W4 — Rust lowerer smoke + codegen.md | At least one grammar (BBNF or JSON) round-trips through bbnf-codegen-ir → bbnf-codegen::rust::lower → emitted source compiling; `docs/spec/codegen.md` lands | 2 parallel | smoke test passes; doc lands; cross-references to architecture.md |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| `bbnf-grammar`, `bbnf-parse` content migrated | C | C.W1 |
| `bbnf-ir` Layout vocabulary + `LayoutSink` trait | C | C.W3 |
| `bbnf-passes` (every transformation pass) | C | C.W4 |
| Skeletal `bbnf-codegen-ir/`, `bbnf-codegen/` | A | A.W2 |
| Naming canon (Lock 2 fold complete) | C | C.W2 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| 22-variant typed codegen IR | E (consumed by runtime template), F (consumed by optimisers), H (consumed by TS+WASM emitters) | E.W0, F.W0, H.W0 |
| Reshaped `Emitter` trait | E, F, H | (continuous) |
| `LayoutSink` impl per backend | E, H | E.W2 (Rust); H.W2 (TS+WASM) |
| Rust lowerer smoke (proven path through grammar → IR → Rust source) | E | E.W2 (per-grammar declaration crate generated.rs) |
| `docs/spec/codegen.md` | E, H, J | (continuous reference) |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | substantive-honoured | D.W2 (struct_direct + substrate.rs retire; orthogonal-codepath fault dissolves) |
| 2 — Layout canon | honoured | (continuous from C) |
| 3 — Cursor + byte-skip | n/a | — |
| 4 — Per-domain orthogonal | n/a | — |
| 5 — IR + per-backend | honoured | D.W0 (typed IR contract); D.W3 (per-backend lower) |
| 6 — xtask source emit | substrate-honoured | (xtask consumes bbnf-codegen post-D) |
| 7 — `crates/path/` consolidated | honoured | (continuous from C) |
| 8 — Surpass SOTA | n/a (pre-perf-tranche) | — |
| 9 — Slice-borrow primary | n/a | — |
| 10 — Pratt + SIMD auto-detected | n/a | — |
| 11 — Path-deps for sister crates | honoured | (continuous) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | honoured | (continuous from C) |
| 14 — Full grammar generalisation | honoured | (continuous; bbnf-codegen carries ZERO grammar-named module) |
| `feedback_no-orthogonal-codepaths` | honoured | D.W2 |
| `feedback_one-codegen-path` | honoured | D.W2 (one Emitter trait, one walking pattern) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| 22-variant typed IR is incomplete (not exhaustive over typed-grammar shapes) | D.W0 type-system audit: `inverse-layout-audit` from C.W4 fires over the new IR; missing variants surface as build-fail |
| Emitter trait reshape introduces backend-specific divergence | D.W2 per-backend method-count audit: Rust + TS + WASM all impl the same trait method count; cross-backend equivalence test (smoke) |
| Rust lowerer smoke passes for one grammar but fails for another | D.W4 smoke runs across all 9 grammars (BBNF + JSON + ...); at least 3 (BBNF + JSON + CSS Pretty) must round-trip; the rest may carry temporary stub but must be tracked |
| struct_direct sub-modules' content lost in retire | D.W2 staged: per-shape method receives the substantive logic; sub-module files delete after method-level smoke passes |
| substrate.rs retire breaks substrate-selection logic | D.W2 audit: substrate selection is monocular post-Lock-1; the file's logic is dead per Pass B §1.a; retire is delete-only |
| Generated-LOC regression in D | D's per-grammar regen produces less code (Emitter trait collapse + struct_direct retire reduce emit by ~5-10%); per master plan §12.2: D.exit = 158,750 LOC (-8K) |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| `cargo check -p bbnf-codegen-ir` | ≤ 5s incremental | D.W0 |
| Per-grammar regen smoke | ≤ 30s per grammar | D.W4 |
| Generated-LOC budget | D.exit: 158,750 LOC (-8K vs. C.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14.

## Closing posture

Tranche D closes with the codegen IR contract settled. Every backend (Rust now; TS + WASM at H) consumes the same typed IR + the same Emitter trait. The 30-method trait dissolved into 8-10 per-shape methods; the struct_direct + substrate.rs orthogonal-codepath fault retired; the Rust lowerer smoke proves the contract.

Tranche E receives the proven IR contract and lifts the per-grammar substrate.
