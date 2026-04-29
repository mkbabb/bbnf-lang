# AZ-II — Interim Manifest (Routed to cutover.O)

**Status**: INTERIM MANIFEST — cutover.A through cutover.M Phase 3 LANDED at master; cutover.N dispatched + halted at organizational usage limit; cutover.O.0 tooling preflight, O1 builder transactions, O2 EBNF direct projection, O3a failure-baseline routing, and O3 generated view purge LANDED; Parsed<R> deletion + TapeDirect deletion + tape deletion + 17-entry bench refresh remain in cutover.O terminal hardening.

**Date authored**: 2026-04-28 (cutover.H Phase 7); refreshed 2026-04-29 (cutover.N halt addendum).
**Master HEAD at FINAL.md update**: `1d9a80bb` (`docs(az-ii): comprehensive PROGRESS-SNAPSHOT-2026-04-29 — 14 substage trajectory`).
**Last cutover commit landed**: `6a7e0f06`
(`fix(az-ii): purge StructDirect generated view residue`).
**Wave document**: [`waves/cutover/README.md`](waves/cutover/README.md)
**O-wave specs**: [`O0`](waves/cutover/O0.md) /
[`O1`](waves/cutover/O1.md) / [`O2`](waves/cutover/O2.md) /
[`O3a`](waves/cutover/O3a.md) / [`O3`](waves/cutover/O3.md) /
[`O4`](waves/cutover/O4.md) /
[`O5`](waves/cutover/O5.md) / [`O6`](waves/cutover/O6.md) /
[`O7`](waves/cutover/O7.md)
**O3a child specs**: [`J1`](waves/cutover/O3a-J1.md) /
[`C1`](waves/cutover/O3a-C1.md) /
[`S1`](waves/cutover/O3a-S1.md) /
[`P1`](waves/cutover/O3a-P1.md) /
[`A1`](waves/cutover/O3a-A1.md)
**Parent plan**: [`AZ-II.md`](AZ-II.md)
**Trajectory snapshot**: [`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md)
**Implemented-state record**: the snapshot is canonical for cutover.A through cutover.N. Later hardening notes refine cutover.O ordering and record O0/O1/O2/O3a/O3 landing; they do not claim cutover.N landed code.

AZ-II's cutover wave decomposed into 14 sequential sub-stages (cutover.A through cutover.N) over multiple sessions. cutover.A through cutover.M LANDED at master, completing the BBNF resolver-arm activation, emitter substrate fixes, the BBNF compact-source serializer, and the cutover.M Phase 3 fleet activation onto StructDirect for CSV / Math / BNF / CSS Pretty. cutover.N (EBNF activation + Phases 4/5/6 close) was dispatched at master `43f0795b` and halted at organizational usage limit before any commits landed. cutover.O.0 repaired proof-command surfaces, cutover.O.1 landed grammar-general StructDirect builder transactions, cutover.O.2 flipped EBNF to StructDirect by wiring structural `Seq` AltDispatch branches through the shared StructDirect branch walker, O3a routed the post-O2 failure baseline, and O3 purged generated view residue. The remaining terminal blockers are Parsed<R> deletion, TapeDirect deletion, tape crate deletion (Hard gate 1), semantic parity, and 17-entry bench refresh.

The bootstrap reproducibility CI gate is intact (BBNF + JSON regen idempotence both verified). BBNF self-parity admits 56/56 fixtures via the cutover.G hand-written bootstrap parser. The cutover.L Phase 3a Alt-of-Ref + cutover.M alt_dispatch Alt-of-Literal/Regex/Seq emitter surgery produce a self-consistent regen of the eight-grammar fleet (BBNF + JSON + CSS L4 + Sheets + CSV + Math + BNF + CSS Pretty); the cutover.I.5 typed walker over `BbnfDocument` re-emits compact BBNF source idempotently.

## Trajectory recap

AZ-II decomposed into 14 cutover sub-stages spanning multiple dispatch
sessions. This table is a compact recap; the authoritative
commit-by-commit record is
[`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md).

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
| cutover.M Phase 3b/c/d | `a29a1265` | Resolver arms for CSV / Math / BNF / CSS Pretty flip to StructDirect; all 9 grammars regen onto matching substrate. AltDispatch struct_direct emitter (`shapes/alt_dispatch/branches.rs::emit_dispatch_arms_struct_direct`) now emits Alt-of-Literal / Alt-of-Regex / Alt-of-Seq branches as byte-comparison + `push_leaf_with_unit()` + `push_branch_tag(idx)` triples — pre-cutover.M these arms emitted empty placeholders that collapsed BBNF `type_name` and CSS L4 pseudo-class arms into no-op loops. EBNF activation was deferred at M and superseded by O2, which flips EBNF to `EbnfDocument`. |
| cutover.O2 | `60561ba3` | EBNF resolver arm flips to StructDirect; shared StructDirect structural-Seq branch emission preserves nested children for grouped terms and commits branch tags transactionally. `EbnfParser::parse` now returns `EbnfDocument`; focused EBNF parse/serialize/accessor tests pass and full regen is idempotent. |
| cutover.O3 | `6a7e0f06` | StructDirect generated files no longer emit tape-backed generated views, `ValueRoot`, projection materializer/consumer tables, `TapeCursor` scan-policy doc residue, or node-view serializer APIs. The O3 scan artifact records zero O3 residue hits; remaining generated `crate::runtime::tape` references route to O4/O5. |

## Hard-gate readout per `waves/cutover/README.md` §Hard gate

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green without it | DEFERRED | Large live tape surface remains (~10k core refs; 13,874 workspace-wide at an earlier scan). Deletion is a wave-scale refactor routed to cutover.O, not a separate tranche. |
| 2 | Stage A / Stage B byte-equal across BBNF fixture corpus | MET (cutover.B) | Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` PASSES under cutover.H regen output; idempotent. |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide | NOT VERIFIED | Audit pass exists; full-fleet verification is part of cutover.O semantic/perf close after EBNF activation. |
| 4 | `StructRegistry` non-empty for every Named rule | MET (cutover.A) | `populate_struct_registry` returns layouts for all 9 grammars; regression test in place. |
| 5 | Parity harnesses recoded to struct-vs-external on all four grammars | MET (cutover.D) | `685bad2f` / `825e8a06`. |
| 6 | 17-entry matrix at AU floor; BBNF self-parse within ±10% of AU baseline | PARTIAL | Bench archive captured at `docs/benchmarks/post-AZ-II.json` (cutover.E-era placeholder plus later notes). Full refresh belongs to cutover.O after builder transactions, EBNF projection, and tape deletion. BBNF self-parse still routes through `bootstrap_parser`, not generated self-host parse. |
| 7 | AZ-II interim manifest + `docs/benchmarks/post-AZ-II.json` exist on master | MET (interim form) | Convert this manifest at cutover.O.7 after terminal gates pass. |
| 8 | Decay sweep | PARTIAL | cutover.A landed (`tape::dta` hoist + `tape::visitor` deletion + driver helper deletion); `crates/tape/` deletion deferred per Gate 1. |

## BA handoff verification per AZ-II.md §Handoff contract — 7 points

| # | Point | Status | Notes |
|---|---|---|---|
| 1 | All four grammars on direct-to-struct (JSON + CSS L4 + Sheets + BBNF) | MET for the named four; terminal surface still partial | JSON + CSS L4 + Sheets active at cutover.A (StructDirect resolver-arms flipped, regen output on disk). BBNF resolver-arm flipped at cutover.H Phase 1 (`3d799a29`); the regen output produces a working struct-direct parser via `bootstrap_parser` routing. CSV / Math / BNF / CSS Pretty also flipped at cutover.M, and EBNF flipped at cutover.O2. The codegen-emitted `BbnfBootstrap::parse` self-host is still bridged by cutover.G's hand-written parser. |
| 2 | `crates/tape/` deleted | DEFERRED | Per Hard gate 1 above. |
| 3 | `StructRegistry` closed fleet-wide | MET | Per Hard gate 4 above. |
| 4 | Parity harnesses on struct comparisons | MET | Per Hard gate 5 above. |
| 5 | 17-entry matrix at AU parity | DEFERRED | Per Hard gate 6 above. |
| 6 | BBNF self-parse byte-reproducible | MET | Permanent CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` PASSES; regen of the cutover.H-fixed bbnf.rs produces byte-identical output. |
| 7 | Parent-pointer decision surface open for BA.W0 | DEFERRED | Surface accessible post-tape-deletion. |

## Throughput delta vs AU + vs AZ-I

`docs/benchmarks/post-AZ-II.json` retains the cutover.E placeholder values plus later explanatory notes; re-bench is gated on cutover.O terminal hardening. The cutover.E values reflect BBNF on the old baseline and do not capture the terminal StructDirect/tape-deleted state. Per-bench logs at `docs/benchmarks/post-AY-AZ-II-close-*.txt` remain historical until O6 refreshes JSON sonic-rs parity, CSS lightningcss typed parity, and the 17-entry close matrix.

| Grammar / fixture | AU baseline | AZ-I close | AZ-II close (cutover.E placeholder) | Delta vs AU | Delta vs AZ-I |
|---|---:|---:|---:|---:|---:|
| JSON canada | 1231 MB/s | 547 MB/s | 547 MB/s | -55.6% | 0% |
| JSON citm | 2438 MB/s | 1476 MB/s | 1476 MB/s | -39.5% | 0% |
| JSON twitter | 1967 MB/s | 1402 MB/s | 1402 MB/s | -28.7% | 0% |
| CSS bootstrap | 454 MB/s | SIGABRT | SIGABRT | n/a | n/a |
| Sheets parse_simple | 95 MB/s | SIGABRT | SIGABRT | n/a | n/a |
| BBNF self-parse | 394 MB/s | 87 MB/s | 87 MB/s | -77.9% | 0% |

The post-AY cumulative regression vs AU baseline (~70-80%) is documented in the cutover.A through cutover.E reports. No optimization tranche may consume these stale values as truth; cutover.O.6 must publish the next baseline before BB.close consumes cost-model or inferred-rewrite work.

## Reversal disposition

cutover.H Phase 1 + cutover.I Phase 5 landed as non-reversal interim
state. The BBNF resolver-arm + emitter fixes + `serialize_compact_doc`
are not under reversal: they improve workspace test posture and
preserve the reproducibility gate. The old Phase 2 non-BBNF regen-fleet
deferral is now mostly closed by cutover.K/L/M; the remaining blockers
carry forward under explicit scope to cutover.O terminal hardening.

## Workspace test posture

Pre-cutover.H (HEAD `42e0906b`): 1429 / 1640 pass (1429 / 1614 effective; 26 skipped); 211 fail.
Post-cutover.H (HEAD `1513328e`): 1445 / 1640 pass (1445 / 1614 effective); 195 fail.
Post-cutover.I.5 (HEAD `98008086`): bbnf_rule un-ignored — 19/19 serialize_roundtrip + 1/1 bbnf_bootstrap_reproducibility pass.
Post-cutover.M (HEAD `43f0795b`): 1514 / 1642 pass; 128 fail; 25 skipped.
Post-snapshot (HEAD `1d9a80bb`): unchanged from cutover.M (snapshot is docs-only).

Net: +16 tests fixed at cutover.H; +1 test (bbnf_rule un-ignored) at cutover.I.5; cutover.K Phase 0/1/2 + cutover.L Phase 3a + cutover.M Phase 3b/c/d cumulative +69 tests; regen `--check` clean across all 9 grammars; reproducibility CI gate green for both BBNF + JSON. The remaining 128 failures are pre-existing CSS L4, Sheets, EBNF, and json-prototype test issues — EBNF subset routes to cutover.O, the rest are out-of-scope for AZ-II.

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
| H.2 / I.2 / M.3 | Re-enable non-BBNF resolver arms + regen fleet | LANDED at cutover.M Phase 3b/c/d (commit `a29a1265`) for CSV / Math / BNF / CSS Pretty. cutover.K Phase 2's per-shape Err-path open-frame close + cutover.L Phase 3a's Alt-of-Ref keyword surgery + cutover.M's alt_dispatch struct_direct Alt-of-Literal/Regex/Seq emitter surgery together unblocked the activation. EBNF stayed on TapeDirect at cutover.M, but that miss is superseded by O2's shared structural-Seq AltDispatch repair and `EbnfDocument` resolver flip. | LANDED for 4/5 at M; superseded to 5/5 by O2 |
| H.3 | `Parsed<R>` deletion (Option B) | Nine of nine grammars now return concrete `Document` types directly. `Parsed<R>` is production-dead residue, but per-site generated view / `to_value()` migration and `TapeDirect` fallback deletion still need to close. | cutover.O.4 |
| H.4 | `crates/tape/` deletion | Cross-crate references remain large (~10k); deletion requires resolution of every `tape::Tape` / `tape::TapeOffset` / `tape::dta::*` consumer after generated view purge and `Parsed<R>` / `TapeDirect` deletion. | cutover.O.5 |
| H.5 | `bbnf_rule` un-ignore | LANDED at cutover.I.5 (commit `98008086`). |
| H.6 | 17-entry close matrix bench refresh | Bench compile under fat-LTO takes >10 minutes per harness. Full bench refresh deferred — the cutover.M emitter changes do not move the JSON / CSS L4 / Sheets / BBNF hot paths (those grammars were already on StructDirect pre-M); the freshly-activated CSV / Math / BNF / CSS Pretty parsers were not in the AU-era 17-entry close matrix. | cutover.O.6 |

## cutover.O Terminal Hardening Scope

The terminal substage opens on cutover.M's substrate and the
cutover.N no-code halt recorded by the progress snapshot. Items 1, 2
(4/5 grammars), 5, 7 LANDED in cutover.K/L/M; remaining work for
cutover.O:

1. **[O0 tooling preflight](waves/cutover/O0.md)** — LANDED: stale bench, profiling, IAI, and
   release-pin surfaces were repaired or de-canonicalized before close
   evidence collection.
2. **[O1 builder transactions](waves/cutover/O1.md)** — LANDED: grammar-general
   checkpoint/rollback/commit support exists on StructDirect builders and
   is wired through every speculative alternate/repeat/minus/negate
   emitter path.
3. **[O2 EBNF activation](waves/cutover/O2.md)** — LANDED: high-branch literal alternates
   and structural `Seq` AltDispatch branches project through
   StructDirect; `EbnfParser::parse -> EbnfDocument`.
4. **[O3a failure baseline and triumvirate redress](waves/cutover/O3a.md)** —
   LANDED as routed evidence: all post-O2 failures and the failed JSON
   bench baseline are assigned to research/plan/redress cohorts.
5. **[O3 generated view purge](waves/cutover/O3.md)** — LANDED: remove tape-backed `TapeCursor`,
   node-view, and `ValueRoot` residue from StructDirect generated output
   unless consumed through a document API.
6. **[O4 `Parsed<R>` / `TapeDirect` deletion](waves/cutover/O4.md)** — next active: delete production
   `Parsed<R>` and remove fallback `TapeDirect` semantics.
7. **[O5 `crates/tape` deletion](waves/cutover/O5.md)** — delete the standalone crate after
   relocating only non-tape scan/index primitives.
8. **[O6 semantic/perf close](waves/cutover/O6.md)** — refresh JSON sonic-rs parity, CSS
   lightningcss typed parity, and the 17-entry close matrix.
9. **[O7 FINAL conversion](waves/cutover/O7.md)** — convert this manifest from interim form to
   terminal close.

Remaining estimate after O3: O4-O7 remain sequential under fan-out. Open
AZ-III only if a later gate proves new grammar-general inference/layout
substrate is required; do not move tape deletion, `Parsed<R>` deletion,
stale benches, or parity gaps into AZ-III.

## Trajectory snapshot pointer

For per-substage commit-by-commit detail across the 14 sub-stages
(cutover.A through cutover.N), agent dispatch history with caps and
outcomes per dispatch, hard-gate readout, BA handoff verification,
substrate inventory (NEW / MODIFIED / DELETED files), worktree state,
and the trajectory progress estimate, see
[`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md).

## Archaeology

cutover.H inherits cutover.G's substrate and adds the resolver-arm + emitter fix that was scoped at cutover.H per cutover.G-PARTIAL §Recommendation. The eight-phase dispatch brief targeted full close in 300 minutes; the actual scope grew into cutover.A through cutover.N and now routes to cutover.O terminal hardening.

The interim-manifest discipline holds: cutover.H Phase 1 is a productive landing — the BBNF substrate is canonical; the regen pipeline produces a self-consistent parser; the bootstrap reproducibility CI gate is intact; net workspace test posture improves by +16 tests. The path to AZ-II's ultimate close is now cutover.O: O0/O1/O2/O3a/O3 are landed, and the remaining gates are `Parsed<R>` / `TapeDirect` deletion, tape deletion, semantic/perf refresh, and FINAL conversion.
