# Tranche G — Slice-Borrow API + Pointer Macro + Visitor Surface

## Gestalt

Tranche G lands the bbnf-runtime user-facing API per Lock 9 — slice-borrow primary, with bumpalo + owned escape hatches. Three surfaces over one parse implementation, lifetime-discriminated. The default `parse(input)` returns `&'i str` slices + `Cow<'i, str>` for transformations (lightning-css model). The opt-in `parse_in(input, &bump)` returns bumpalo-arena allocations (sonic-rs model). The opt-in `parse_owned(input)` returns owned strings (serde-json escape). The lifetime parameter is the discriminant; the underlying parse implementation is singular.

The `pointer![...]` macro per Lock 7 honours the sonic-rs convention: `pointer!["a", "b", 1]` builds a compile-time path AST consumed by `bbnf-runtime::get(input, &path)`. The macro lives at `crates/path/src/lib.rs` (Rust proc-macro shell); the TS equivalent lives at `crates/path-ts/src/lib.rs` (cdylib shell); both share `crates/path-core/` for lex/lower/validate.

The `Visitor<'i, T>` + `VisitTypes` per the lightning-css convention. Per-grammar visitor traits emit from `bbnf-runtime-template` (Tranche E carry); per-grammar visitor impls land in per-grammar declaration crates' `src/specialised/visit.rs` (specialised cohort) or auto-emit (trivial cohort).

The `Box::leak` at `crates/core/src/grammar/mod.rs:57` (Pass A §5.7 Lock 9 violation) retires here — the synthesizer's adjudication: introduce `parse_grammar_in(input, &bump)` arena variant per Lock 9. The legacy `Box::leak` deletes; the slice-borrow primary surface stands.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| `parse(input)` slice-borrow primary | G.W0 | `crates/bbnf/src/lib.rs` exports `parse<'i>(input: &'i str) -> Result<<G>Document<'i>, BbnfError>`; per-grammar declaration crates implement |
| `parse_in(input, &bump)` arena variant | G.W1 | exports `parse_in<'i>(input: &'i str, bump: &'i Bump) -> Result<<G>Document<'i>, BbnfError>`; smoke-tests against bumpalo arena |
| `parse_owned(input)` owned variant | G.W1 | exports `parse_owned(input: &str) -> Result<<G>OwnedDocument, BbnfError>`; smoke-tests against owned escape |
| Three surfaces share underlying parse impl | G.W2 | code audit: each grammar's `parse_*` wraps a single `parse_inner` per generated.rs; verified via `rg 'fn parse_inner' crates/<g>/src/generated.rs` per grammar |
| `pointer![...]` macro lands | G.W2 | `crates/path/src/lib.rs` exports `pointer!` macro; `bbnf-runtime::get(input, &pointer!["a", "b", 1])` returns the leaf |
| `Visitor<'i, T>` + `VisitTypes` per grammar | G.W3 | per-grammar declaration crate exports `Visitor<'i, T>`; lightning-css-style visit methods emit from template |
| `Box::leak` retires | G.W4 | `rg 'Box::leak' crates/{bbnf-grammar, bbnf-parse}/src/` returns 0; `parse_grammar_in(input, &bump)` arena variant exists |
| Lock 9 verification | G.W4 | API surface per Lock 9: slice-borrow primary; bumpalo + owned escape hatches; lifetime-discriminant honoured |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| G.W0 — `parse(input)` slice-borrow primary | per-grammar declaration crates expose default slice-borrow parse | 3 parallel (per-cohort batches) | parse(input) compiles + smoke-tests per grammar |
| G.W1 — `parse_in` + `parse_owned` escape hatches | bumpalo arena variant + owned variant emit per template | 3 parallel | both variants smoke-test per grammar |
| G.W2 — Three surfaces share parse impl + pointer macro | parse_inner audit; `pointer![...]` macro lands at `crates/path/src/lib.rs` | 2 parallel | one `parse_inner` per grammar; pointer macro round-trips |
| G.W3 — Visitor + VisitTypes per grammar | per-grammar `Visitor<'i, T>` emits from template; specialised cohort `visit.rs` extensions | 4 parallel (per-grammar batches) | visitor traits emit; specialised visitors integrate |
| G.W4 — `Box::leak` retire + Lock 9 verification | parse_grammar_in arena variant lands; Box::leak retires; Lock 9 audit | 1 | Box::leak grep returns 0; Lock 9 API surface verified |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| 9 per-grammar declaration crates | E | E.W3 |
| Direct-projection emit | E | E.W4 |
| `path-core` consolidated | C | C.W6 |
| Optimiser pipeline integrated | F | F.W4 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| `parse / parse_in / parse_owned` API | H (TS + WASM emitters consume same surface), J (cross-backend parity tests three surfaces) | H.W2, J.W2 |
| `pointer![...]` macro | H, J | H.W2, J.W3 |
| `Visitor<'i, T>` per grammar | J | J.W3 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | honoured | (continuous) |
| 2 — Layout canon | honoured | (continuous) |
| 3 — Cursor + byte-skip | honoured | (continuous) |
| 4 — Per-domain orthogonal | honoured | (continuous from F) |
| 5 — IR + per-backend | honoured | (continuous from D) |
| 6 — xtask source emit | honoured | (continuous) |
| 7 — `crates/path/` consolidated | honoured | G.W2 (pointer macro at consolidated path crate) |
| 8 — Surpass SOTA | partial | (continuous from F; full set at J) |
| 9 — Slice-borrow primary | substantively-honoured | G.W0-W2 (three surfaces, lifetime-discriminant); G.W4 (Box::leak retires) |
| 10 — Pratt + SIMD auto-detected | honoured | (continuous from F) |
| 11 — Path-deps for sister crates | honoured | (continuous) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | honoured | (continuous) |
| 14 — Full grammar generalisation | honoured | (continuous; per-grammar Visitor traits emit from template, no hand-written per-grammar API) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Slice-borrow + arena + owned (Lock 9) surface drift between backends | G.W2 unified API surface; H.W3 cross-backend equivalence test (one input, three backends, identical typed-tree); per master plan §13 R13 |
| `parse_in` arena variant introduces hidden eager allocation | G.W1 audit: each `parse_in` impl bumps the bumpalo arena per allocation; no hidden Vec/Box outside the arena scope |
| `pointer![...]` macro compile-time validation incomplete | G.W2 macro test fixtures: invalid path AST (out-of-bounds index, type-mismatched key) must fail at compile; valid paths round-trip through type_check |
| Visitor surface diverges between specialised and trivial cohort | G.W3 per-cohort audit: trivial cohort visitor emits 100% from template; specialised cohort visitor wraps template with extensions |
| `Box::leak` retire breaks `parse_grammar` API for callers expecting `'static` | G.W4 staged: introduce `parse_grammar_in(input, &bump)` first; existing `parse_grammar` retires after callers migrate |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Per-grammar `parse / parse_in / parse_owned` emit time | ≤ 5s incremental | G.W0-W1 |
| `pointer!` macro expansion time | ≤ 100ms per call | G.W2 |
| Generated-LOC budget | G.exit: 143,750 LOC (+500 vs. F.exit; parse_in/owned variants per grammar) | per master plan §12.2 |

## Voice locks

Per master plan §14.

## Closing posture

Tranche G closes with the user-facing API in place per Lock 9. Three surfaces, one parse impl, lifetime-discriminated. The pointer macro consolidates per Lock 7; the Visitor surface honours the lightning-css convention; Box::leak retires.

The greenfield mandate carries: no eager allocation in slice-borrow primary (Cow + slices only); no per-grammar API divergence (template emit honours uniformity); the `Box::leak` substrate workaround retires entire.
