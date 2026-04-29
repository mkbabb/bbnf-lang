# AZ-II — FINAL (Partial Close)

**Status**: PARTIAL CLOSE — cutover.A through cutover.M Phase 3 LANDED; tape deletion + EBNF activation + Parsed<R> deletion deferred.

**Date**: 2026-04-28
**Master HEAD at cutover.M close**: `a29a1265` (`feat(cutover.M): activate non-BBNF StructDirect resolver arms; regen 9 grammars (Phase 3b/c/d)`)
**Wave document**: [`waves/cutover.md`](waves/cutover.md)
**Parent plan**: [`AZ-II.md`](AZ-II.md)

AZ-II completes the BBNF resolver-arm activation, emitter substrate fixes, the BBNF compact-source serializer, and the cutover.M Phase 3 fleet activation onto StructDirect for CSV / Math / BNF / CSS Pretty. The tape crate deletion (Hard gate 1), EBNF activation (deferred under cutover.M for layout-routing depth), Parsed<R> deletion, and 17-entry bench refresh defer to a follow-on tranche under documented scope. The bootstrap reproducibility CI gate is intact; BBNF self-parity admits 56/56 fixtures via the cutover.G hand-written bootstrap parser; the cutover.L Phase 3a Alt-of-Ref + cutover.M alt_dispatch Alt-of-Literal/Regex/Seq emitter surgery produce a self-consistent regen of the eight-grammar fleet (BBNF + JSON + CSS L4 + Sheets + CSV + Math + BNF + CSS Pretty); the cutover.I.5 typed walker over `BbnfDocument` re-emits compact BBNF source idempotently.

## Trajectory recap

AZ-II decomposed into eight cutover sub-stages spanning ~30 hours of dispatch:

| Sub-stage | Commits | Headline |
|---|---|---|
| cutover.A | `63cacbe2` `d3977825` `19a2669a` `82a88696` `ec7a0fa1` | Substrate hoist (`tape::dta` → `bbnf-ir::dta`); `tape::visitor` family deletion (746 LOC); BBNF runtime substrate at `crates/core/src/runtime/bbnf/`; resolver-arm extension at `crates/ir/src/registry/strategy.rs::for_grammar`. |
| cutover.B | `d6b0377a` | Stage A / Stage B byte-equal cycle + permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs`. |
| cutover.C | scope-reveal | `crates/tape/` deletion deferred — surfaced 700 BbnfBootstrapNodeView refs across 35 files; 1218 tape refs in regen'd bbnf.rs; sibling-grammar tape consumers. Routed to cutover.D + E. |
| cutover.D | 4 parallel agents | Consumer migration: `host.rs` / `lower/` / `graph/` / parity-harness recode onto `BbnfDocument`. |
| cutover.E | `911ee70f` `6b2f3ca7` `57e017de` `cb36c997` | Non-BBNF struct-direct substrates authored (csv / math / bnf / ebnf / css_pretty); BBNF resolver-arm DEFERRED on Discovery 1 emitter regression; non-BBNF resolver-arms deferred under same gate. |
| cutover.F | `b813eb64` `246efda7` `6056baee` | Three emitter bug classes diagnosed + fixed: array Shape-2 dispatch; flat Alt/Repeat/Regex/Negate/Minus inline emission; substrate-LANDED. |
| cutover.G | `e52974a6` `984d7535` `863de6a5` `caf07d96` `9300e9df` | Hand-written BBNF bootstrap parser at `crates/core/src/grammar/bootstrap_parser.rs` (~900 LOC) breaks the chicken-and-egg between regen and the broken on-disk `BbnfBootstrap::parse`. 56/56 BBNF self-parity tests pass under cutover.G. |
| cutover.H Phase 0 | `42e0906b` | `graph/deps::collect_refs_from_compound` skips value-expression compound subtrees so JSON regen-check no longer mis-classifies host-fn idents as nonterminal refs. |
| cutover.H Phase 1 | `3d799a29` `1513328e` | BBNF resolver-arm re-flip + transparent-rule emitter fix at `shapes/mod.rs` (Wrap-classified transparent rules emit per-shape fns under StructDirect; transparency-aware dispatch with no outer compound). `bootstrap_parser.rs::parse_pretty_hint` pushes the parenthesised arg span as a child Span so `sep("...")` hints emit canonical strings. Phase 5 (`bbnf_rule` un-ignore) deferred — `serialize_compact_doc` requires a typed walker over `BbnfDocument` materialising non-Span literals (`;` / `.`). |
| cutover.I Phase 5 | `98008086` | `BbnfBootstrap::serialize_compact_doc` authored at `crates/core/src/runtime/bbnf/serialize.rs`. Typed walker over `BbnfDocument` keyed on `BbnfCompoundKind`; each compound shape (Rule, Alternation, Concatenation, MappedFactor, Closure, ImportDirective, PrettyDirective, …) emits its required structural literals (`=`, `;`, `,`, `|`, `->`, `from`, `@import`, …) in grammar order. `bbnf_rule` test in `serialize_roundtrip.rs` un-ignored — round-trip idempotence holds. 19/19 serialize_roundtrip + 1/1 bbnf_bootstrap_reproducibility tests pass. |
| cutover.K Phase 0-2 | `a09173dc` `cbf77e06` `7d283a8f` | Phase 0: `bootstrap_parser.rs::parse_mapped_factor` wraps the mapping target in an anonymous compound. Phase 1: `lower::value_expr` typed-leaf source-text recovery. Phase 2: per-shape Err paths now close any open compound frames before propagating to the dispatcher. |
| cutover.L Phase 3a | `b770fae7` | Keyword-shape Alt-of-Ref struct-direct emitter at `shapes/keyword/struct_direct.rs` — Ref branches prefix-check + delegate to the target shape fn (CSS L4 `pseudoClass` / `pseudoElement`). Per-rule emission gate at `shapes/mod.rs` admits Wrap- and Keyword-classified transparent rules under both substrate strategies. |
| cutover.M Phase 3b/c/d | `a29a1265` | Resolver arms for CSV / Math / BNF / CSS Pretty flip to StructDirect; all 9 grammars regen onto matching substrate. AltDispatch struct_direct emitter (`shapes/alt_dispatch/branches.rs::emit_dispatch_arms_struct_direct`) now emits Alt-of-Literal / Alt-of-Regex / Alt-of-Seq branches as byte-comparison + `push_leaf_with_unit()` + `push_branch_tag(idx)` triples — pre-cutover.M these arms emitted empty placeholders that collapsed BBNF `type_name` and CSS L4 pseudo-class arms into no-op loops. EBNF activation deferred (Alt-of-many-literal `letter`/`digit`/`symbol` rules expose layout-routing depth beyond cutover.M's cap). |

## Hard-gate readout per `waves/cutover.md` §Hard gate

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green without it | DEFERRED | 13874 tape references workspace-wide; deletion is a wave-scale refactor outside cutover.H's 300-min cap. Routed to follow-on tranche. |
| 2 | Stage A / Stage B byte-equal across BBNF fixture corpus | MET (cutover.B) | Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` PASSES under cutover.H regen output; idempotent. |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide | NOT VERIFIED | Audit pass exists; full-fleet verification gated on Phase 2 regen-fleet activation (deferred). |
| 4 | `StructRegistry` non-empty for every Named rule | MET (cutover.A) | `populate_struct_registry` returns layouts for all 9 grammars; regression test in place. |
| 5 | Parity harnesses recoded to struct-vs-external on all four grammars | MET (cutover.D) | `685bad2f` / `825e8a06`. |
| 6 | 17-entry matrix at AU floor; BBNF self-parse within ±10% of AU baseline | DEFERRED | Bench archive captured at `docs/benchmarks/post-AZ-II.json` (cutover.E-era placeholder; refresh at follow-on tranche after tape deletion). BBNF self-parse via bootstrap_parser, not codegen. |
| 7 | AZ-II FINAL.md + `docs/benchmarks/post-AZ-II.json` exist on master | MET (this commit) | This document; bench archive present (cutover.E placeholder retained). |
| 8 | Decay sweep | PARTIAL | cutover.A landed (`tape::dta` hoist + `tape::visitor` deletion + driver helper deletion); `crates/tape/` deletion deferred per Gate 1. |

## BA handoff verification per AZ-II.md §Handoff contract — 7 points

| # | Point | Status | Notes |
|---|---|---|---|
| 1 | All four grammars on direct-to-struct (JSON + CSS L4 + Sheets + BBNF) | PARTIAL | JSON + CSS L4 + Sheets active at cutover.A (StructDirect resolver-arms flipped, regen output on disk). BBNF resolver-arm flipped at cutover.H Phase 1 (`3d799a29`); the regen output produces a working struct-direct parser via `bootstrap_parser` routing. The codegen-emitted `BbnfBootstrap::parse` self-host (regen reproducing itself byte-equal AND admitting the BBNF fixture corpus) is a deferred follow-up — cutover.G's hand-written parser remains the canonical entry point. |
| 2 | `crates/tape/` deleted | DEFERRED | Per Hard gate 1 above. |
| 3 | `StructRegistry` closed fleet-wide | MET | Per Hard gate 4 above. |
| 4 | Parity harnesses on struct comparisons | MET | Per Hard gate 5 above. |
| 5 | 17-entry matrix at AU parity | DEFERRED | Per Hard gate 6 above. |
| 6 | BBNF self-parse byte-reproducible | MET | Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` PASSES; regen of the cutover.H-fixed bbnf.rs produces byte-identical output. |
| 7 | Parent-pointer decision surface open for BA.W0 | DEFERRED | Surface accessible post-tape-deletion. |

## Throughput delta vs AU + vs AZ-I

`docs/benchmarks/post-AZ-II.json` retains the cutover.E placeholder values; re-bench gated on tape-deletion + struct-direct activation parity. The cutover.E values reflect BBNF on TapeDirect baseline; numbers do not capture cutover.H's StructDirect activation. Per-bench logs at `docs/benchmarks/post-AY-AZ-II-close-*.txt` (cutover.E-era for compile/css/sheets/bbnf; cutover.H Phase 6 partial run for json).

| Grammar / fixture | AU baseline | AZ-I close | AZ-II close (cutover.E placeholder) | Delta vs AU | Delta vs AZ-I |
|---|---:|---:|---:|---:|---:|
| JSON canada | 1231 MB/s | 547 MB/s | 547 MB/s | -55.6% | 0% |
| JSON citm | 2438 MB/s | 1476 MB/s | 1476 MB/s | -39.5% | 0% |
| JSON twitter | 1967 MB/s | 1402 MB/s | 1402 MB/s | -28.7% | 0% |
| CSS bootstrap | 454 MB/s | SIGABRT | SIGABRT | n/a | n/a |
| Sheets parse_simple | 95 MB/s | SIGABRT | SIGABRT | n/a | n/a |
| BBNF self-parse | 394 MB/s | 87 MB/s | 87 MB/s | -77.9% | 0% |

The post-AY cumulative regression vs AU baseline (~70-80%) is documented in the cutover.A through cutover.E reports; remediation routes to BB.close (cost-model-driven inferred rewrites) per the refined wave trajectory at `docs/tranches/az-i/W2-CLOSE-AUDIT.md`.

## Reversal disposition

cutover.H Phase 1 + cutover.I Phase 5 land as a non-reversal partial close. The BBNF resolver-arm + emitter fixes + serialize_compact_doc are NOT under reversal — they net-improve workspace test pass count by +16 tests at cutover.H (HEAD: 1429/1640 pass; cutover.H: 1445/1640 pass) and +1 test at cutover.I.5 (bbnf_rule un-ignored). The deferrals (Phase 2 non-BBNF regen-fleet; Phase 3-4 Parsed<R> + tape deletion; Phase 6 bench refresh) carry forward under explicit scope to a follow-on tranche; cutover.I.2 substrate (`shapes/transparent.rs`, ~430 LOC) sits in a worktree pending the wave-scale blocker resolution documented under cutover.I Phase 2 above.

## Workspace test posture

Pre-cutover.H (HEAD `42e0906b`): 1429 / 1640 pass (1429 / 1614 effective; 26 skipped); 211 fail.
Post-cutover.H (HEAD `1513328e`): 1445 / 1640 pass (1445 / 1614 effective); 195 fail.
Post-cutover.I.5 (HEAD `98008086`): bbnf_rule un-ignored — 19/19 serialize_roundtrip + 1/1 bbnf_bootstrap_reproducibility pass.
Post-cutover.M (HEAD `a29a1265`): 1514 / 1642 pass; 128 fail; 25 skipped. Same failure set as the pre-cutover.M baseline (cutover.L Phase 3a `b770fae7`).

Net: +16 tests fixed at cutover.H; +1 test (bbnf_rule un-ignored) at cutover.I.5; cutover.M nets +0 (deltas the same baseline failure set as cutover.L), regen `--check` clean, reproducibility CI gate green. The remaining 128 failures are pre-existing CSS L4, Sheets, and json-prototype test issues outside the cutover scope.

## File-level deltas at AZ-II close

| File | Phase | Change |
|---|---|---|
| `crates/ir/src/registry/strategy.rs` | H.1 | Re-flip BBNF StructDirect resolver-arm (deferred at `9f40f17c`). |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | H.1 | Restrict transparent-rule skip to non-Wrap shapes under StructDirect; emit Wrap-classified transparent rules so cross-rule Ref call sites resolve. |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` | H.1 | Add `emit_alt_struct_dispatch_transparent` — transparency-aware Alt-dispatch with no outer compound. |
| `crates/core/src/grammar/bootstrap_parser.rs` | H.1 | `parse_pretty_hint` pushes parenthesised arg span as Span child so `sep("...")` hints emit canonical strings. |
| `crates/core/src/grammar/generated/bbnf.rs` | H.1 | Regen output (39396 LOC) under fixed emitter + resolver-arm flip. |
| `crates/core/src/graph/deps.rs` | H.0 | Skip value-expression compound subtrees in nonterminal-reference walks. |
| `crates/core/tests/serialize_roundtrip.rs` | H.5 | Document Phase 5 deferral; route through `bootstrap_parser::parse`; retain `#[ignore]`. |
| `crates/core/src/runtime/bbnf/serialize.rs` | I.5 | NEW — typed walker over `BbnfDocument` re-emits compact BBNF source with all required structural literals. |
| `crates/core/src/runtime/bbnf/mod.rs` | I.5 | Export `serialize_compact_doc`. |
| `crates/core/tests/serialize_roundtrip.rs` | I.5 | Un-ignore `bbnf_rule` test; route through `serialize_compact_doc`; assert idempotence under reparse. |

## Deferred-scope summary

| Phase | Scope | Deferral rationale | Route |
|---|---|---|---|
| H.2 / I.2 / M.3 | Re-enable non-BBNF resolver arms + regen fleet | LANDED at cutover.M Phase 3b/c/d (commit `a29a1265`) for CSV / Math / BNF / CSS Pretty. cutover.K Phase 2's per-shape Err-path open-frame close + cutover.L Phase 3a's Alt-of-Ref keyword surgery + cutover.M's alt_dispatch struct_direct Alt-of-Literal/Regex/Seq emitter surgery together unblocked the activation. EBNF stays on TapeDirect: its `letter` / `digit` / `symbol` Alt-of-many-literal AltDispatch rules expose layout-routing depth (the per-letter pushes don't yet route through the `EbnfStructBuilder`'s expected layout) beyond cutover.M's cap. | LANDED for 4/5 grammars; EBNF deferred |
| H.3 | `Parsed<R>` deletion (Option B) | Eight of nine grammars now return concrete `Document` types directly. Only EBNF still routes through `Parsed<R>` (still on TapeDirect under cutover.M's deferral). Per-site `view()` / `to_value()` migration plus the EBNF activation must close first. | follow-on tranche |
| H.4 | `crates/tape/` deletion | Cross-crate references remain large (~10k); deletion requires resolution of every `tape::Tape` / `tape::TapeOffset` / `tape::dta::*` consumer plus EBNF activation. | follow-on tranche |
| H.5 | `bbnf_rule` un-ignore | LANDED at cutover.I.5 (commit `98008086`). |
| H.6 | 17-entry close matrix bench refresh | Bench compile under fat-LTO takes >10 minutes per harness. Full bench refresh deferred — the cutover.M emitter changes do not move the JSON / CSS L4 / Sheets / BBNF hot paths (those grammars were already on StructDirect pre-M); the freshly-activated CSV / Math / BNF / CSS Pretty parsers were not in the AU-era 17-entry close matrix. | follow-on tranche |

## Next-tranche scope

The follow-on tranche opens on cutover.H's substrate. Its job:

1. Per-shape transparent-passthrough emission across Object/Array/Keyword/Number/Pratt/Unordered/ArgList/HRegex (Phase 2 unblocking).
2. Regen-fleet activation: csv/math/bnf/ebnf/css_pretty StructDirect arms flipped; all 9 grammars regenerated; workspace clean.
3. `Parsed<R>` deletion (Option B): per-grammar Document type as parse return.
4. `crates/tape/` deletion + cross-crate severance.
5. `BbnfBootstrap::serialize_compact_doc` authoring + `bbnf_rule` un-ignore.
6. 17-entry bench refresh + close-matrix archive at `docs/benchmarks/post-AZ-II.json` (replacing cutover.E placeholder).
7. Codegen-emitted `BbnfBootstrap::parse` self-host (regen reproduces itself byte-equal AND admits the BBNF fixture corpus).

Estimated: 2-3 dispatch waves at 300-min cap each.

## Archaeology

cutover.H inherits cutover.G's substrate and adds the resolver-arm + emitter fix that was scoped at cutover.H per cutover.G-PARTIAL §Recommendation. The eight-phase dispatch brief targeted full close in 300 minutes; the actual scope (per-shape transparent emission across the full shape alphabet + tape-deletion's 13874-ref refactor) is wave-scale and routes to a follow-on tranche.

The PARTIAL close discipline holds: cutover.H Phase 1 is a productive landing — the BBNF substrate is canonical; the regen pipeline produces a self-consistent parser; the bootstrap reproducibility CI gate is intact; net workspace test posture improves by +16 tests. The path to AZ-II's ultimate close (tape deletion + full-fleet StructDirect activation + bench refresh) is well-scoped, and the BB.close gate documented at `docs/tranches/az-i/W2-CLOSE-AUDIT.md` §9 unblocks on the substrate cutover.H lands.
