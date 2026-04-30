# AZ-II — Progress Log

**Status**: interim manifest routed through cutover.O; `cutover.O.0`,
`cutover.O.1`, `cutover.O.2`, `cutover.O.3`, and `cutover.O.4`
landed; O3a J1/C1/S1/P1/A1 triads integrated as routed evidence;
O5 tape-crate deletion is the next active substage.
Implemented-state record:
[`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md).
Live terminal sequence: `cutover.O.0` through `cutover.O.7`, specified
for dispatch at [`waves/cutover/O0.md`](waves/cutover/O0.md) through
[`waves/cutover/O7.md`](waves/cutover/O7.md).
`cutover.O3a` is the closed failure-baseline and triumvirate redress
prelude inserted before generated-view purge. O3 is closed at
`6a7e0f06`; P1 required no O3b, and A1's
archive/delete/live-product/bootstrap disposition remains routed into
O5/O6/O7.

**Date**: 2026-04-23

Dated execution log for tranche AZ-II. AZ-II opened after AZ-I close
(seven-point handoff contract verified) and is now open only for
terminal hardening.

AZ-II completes the direct-to-struct migration. BBNF's own grammar
moves to the `project_types`-derived struct path via a two-stage
bootstrap cutover (Stage A: tape-compiler builds struct-compiler
candidate; Stage B: candidate rebuilds itself; byte-equal output
is the close gate). Once BBNF parses into a derived struct, the
tape crate has no remaining consumers and is deleted.

Original wave plan (three waves + FINAL): W0 bootstrap-cutover research +
classifier extension + AZ-II baseline → W1 Stage A (tape-compiler
builds struct-compiler candidate) → W2 Stage B (candidate rebuilds
itself + byte-equal close gate) → W3 FINAL — `crates/tape/`
deletion + parity recode + BA handoff.

Parent plan: `docs/tranches/AZ-II/AZ-II.md`.
Research: `docs/tranches/AZ-II/RESEARCH.md`.
Cutover design: `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md` (lands W0).

## Gate summary

- **Byte-equal reproducibility**: Stage A output = Stage B output
  across every `.bbnf` fixture in the tree; zero byte differences.
- **Throughput**: Full 17-entry matrix at AU baseline or better on
  the struct-only path across JSON, CSS L4, Sheets, BBNF.
- **Coverage**: 100% `->` coverage fleet-wide; `StructRegistry`
  closed on every Named rule in every production grammar.
- **Tape-deleted**: `rg '^crates/tape/'` and `rg 'use bbnf_tape'`
  return zero matches; `cargo build -p bbnf --no-default-features`
  succeeds without `crates/tape/` existing.
- **Workspace**: ≥ 967 pass / ≤ 33 fail / ≤ 30 ignored.
- **Parity harnesses**: struct-vs-external-native on all four
  grammars; `tests/bbnf_bootstrap_reproducibility.rs` as
  permanent CI gate.

## Escape clause

Byte-equal failure at W2 close is a re-plan trigger, not a
partial-close. Full tape abrogation is binding repo policy; there
is no pre-declared "shrunken-tape" floor. On W2 failure, the wave
reverts its substrate, records drift evidence, and authors a
re-plan brief against that evidence. `feedback_no-workarounds-arch`
and `feedback_no-orthogonal-codepaths` forbid retaining a
tape-bearing substrate for BBNF alongside struct-only data
grammars, even under W2 pressure.

## 2026-04-28 — wave plan refined per W2-CLOSE-AUDIT

The original W0 / W1 / W2 / W3 four-wave shape collapses into a
single **AZ-II.cutover** wave per `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`
§9. The W2-act activation pattern is reusable for BBNF without
further substrate work; Stage A / Stage B is two regen invocations
rather than a wave's worth of ceremony; tape deletion is mechanical
once `crates/tape/` has zero remaining consumers. Wave doc lands at
`docs/tranches/AZ-II/waves/cutover/README.md`. The W0 / W1 / W2 wave docs
carry supersede notices at `docs/tranches/AZ-II/waves/{W0,W1,W2}.md`
and stay on disk as historical record. Cutover substage specs live in
`docs/tranches/AZ-II/waves/cutover/`.

The cutover wave runs in three sequential sub-stages:

- **AZ-II.cutover.A** (cap 120 min) — `tape::dta` hoist to
  `bbnf-ir::dta` per `audit/AUDIT-6` §8.2; `tape::visitor` family
  deletion (746 LOC) per §8.3; tape driver dead-helper deletion per
  `audit/AUDIT-3` §6; BBNF typed-leaf authoring closes
  `StructRegistry` for BBNF; `crates/core/src/runtime/bbnf/`
  authored; resolver-arm extension for `BbnfBootstrap`. IR-side
  decay: `crates/ir/src/passes/recognizers/dta.rs` ~900 LOC
  amputation per `audit/AUDIT-3` §1.
- **AZ-II.cutover.B** (cap 60 min) — Stage A regen + Stage B
  byte-equal cycle. Permanent CI gate at
  `crates/core/tests/bbnf_bootstrap_reproducibility.rs`.
- **AZ-II.cutover.C** (cap 120 min) — `crates/tape/` deletion;
  cross-crate severance; view / pprint / @debug recode; parity
  harness recode; AZ-II FINAL.md; `docs/benchmarks/post-AZ-II.json`
  archive.

## Wave status

| Wave | Status | Headline |
|---|---|---|
| W0 | superseded (2026-04-28) | Folded into cutover.A (substrate hoist + BBNF runtime + decay sweep) |
| W1 | superseded (2026-04-28) | Folded into cutover.B (Stage A + Stage B byte-equal cycle) |
| W2 | superseded (2026-04-28) | Folded into cutover.C (`crates/tape/` deletion + recode + FINAL) |
| cutover | interim-manifest (cutover.A through cutover.M LANDED; cutover.N halted at usage limit; [O0](waves/cutover/O0.md) tooling preflight, [O1](waves/cutover/O1.md) builder transactions, [O2](waves/cutover/O2.md) EBNF direct projection, [O3a](waves/cutover/O3a.md) failure routing, [O3](waves/cutover/O3.md) generated-view purge, and [O4](waves/cutover/O4.md) return-model deletion landed) | 9/9 grammars StructDirect; generated view residue is zero; `Parsed<R>` / `TapeDirect` are gone from production Rust; terminal hardening now routes through [cutover.O5-O7](waves/cutover/README.md) for tape deletion, semantic/perf truth, and close conversion |

## 2026-04-28 — cutover.G partial close

cutover.G HEAD `2060dd8d` lands the chicken-and-egg break: a
hand-written BBNF bootstrap parser at
`crates/core/src/grammar/bootstrap_parser.rs` (~900 LOC)
consumes BBNF source and emits `BbnfDocument<'_>` directly via
`BbnfStructBuilder`. The consumer entry points (`crate::grammar::parse`,
`crate::pipeline::directives::parse_to_pipeline_inputs`) route
through it; all 56 BBNF self-parity tests pass under cutover.G.

`cargo xtask regen --grammar bbnf` runs to completion under
cutover.G — `compile_paths_request` 10ms; `generate_all` 35ms;
`prettyplease` 252ms; on-disk bbnf.rs 34230 LOC produced. The
regen output exposes a follow-up emitter codegen inconsistency
(12 unresolved `parse_wrap_BbnfBootstrap_value_expr` /
`parse_wrap_BbnfBootstrap_rhs` references — call sites reference
the per-shape fn but the wrap emitter no longer defines those
fns when the IR pass marks alias rules transparent). Fix
deferred to cutover.H per the dispatch brief's emitter-scope
boundary.

cutover.G commits:
- `47ba1256` chore(az-ii): cutover.{C,E,F}-PARTIAL move to audit/
  (Phase H cleanup)
- `cc5b2877` feat(cutover.G): hand-written BBNF bootstrap parser
  breaks chicken-and-egg
- `2060dd8d` fix(cutover.G): leaf-shape rules emit Span directly
  + type-keyword filter

PARTIAL close report at `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md`
(393 LOC, archived at cutover.H close) details the strategy
analysis, Phase 1.b regen success, Phase 1.c emitter fix scope,
and the cutover.H sub-phase plan.

## 2026-04-28 — cutover.H Phase 0 + Phase 1 + AZ-II PARTIAL close

cutover.H lands the BBNF resolver-arm re-flip + transparent-rule
emitter fix that cutover.E deferred at `9f40f17c`. Phase 0
(`42e0906b`) lands the validator value-expression-subtree skip in
`graph::deps::collect_refs_from_compound` so the JSON regen-check
no longer mis-classifies host-fn idents as nonterminal refs.
Phase 1 (`3d799a29`) re-flips the BBNF resolver-arm at
`crates/ir/src/registry/strategy.rs::for_grammar` and lands the
transparent-rule wrap-fn fix at `shapes/mod.rs:202` (gate on
`!is_struct_direct()`-AND-non-Wrap retains the legacy skip;
Wrap-classified transparent rules under StructDirect emit a
transparency-aware body via the new `emit_alt_struct_dispatch_transparent`
in `shapes/wrap/struct_direct.rs`). The bootstrap_parser's
`parse_pretty_hint` now pushes the parenthesised arg span as a
child Span so `sep("...")` hints emit canonical strings.

cutover.H Phase 1 commits:
- `3d799a29` feat(cutover.H): re-flip BBNF resolver-arm +
  transparent-rule emitter fix
- `1513328e` docs(cutover.H): document Phase 5 deferral —
  bbnf_rule serialize_roundtrip

Test posture vs HEAD: +16 tests fixed (1429 → 1445 pass);
zero new failures introduced.

Phases 2-7 partial / deferred:
- **Phase 2** (non-BBNF resolver-arm fleet activation) — DEFERRED.
  The cutover.H emitter changes alter regen output for css_pretty /
  css_l4 / google_sheets in ways that surface previously-latent
  shape-classification issues (transparent rules under non-Wrap
  shapes). Resolution requires per-shape transparent-passthrough
  emission across Object/Array/Keyword/Number/Pratt/Unordered/
  ArgList/HRegex emitters — wave-scale refactor.
- **Phase 3** (`Parsed<R>` deletion) — DEFERRED. 126 cross-crate
  references; per-crate site replacement is wave-scale.
- **Phase 4** (`crates/tape/` deletion) — DEFERRED. 13874
  cross-crate references; deletion is wave-scale.
- **Phase 5** (`bbnf_rule` un-ignore) — PARTIAL (`1513328e`).
  Documented deferral; the naive `span_range`-based serializer
  drops mandatory terminators (`;`); requires authoring
  `BbnfBootstrap::serialize_compact_doc` (typed walker over
  BbnfDocument materialising non-Span literals).
- **Phase 6** (17-entry close matrix bench) — PARTIAL. JSON bench
  refresh captured; bbnf bench fails because divan harness uses
  `BbnfBootstrap::parse` directly which doesn't yet self-host;
  CSS L4 + Sheets parse_* SIGABRT pre-existing; compile_pipeline
  bench did not complete within cap. `docs/benchmarks/post-AZ-II.json`
  refreshed with cutover.H Phase 6 JSON values + cutover.E
  placeholder retention for non-BBNF deferred entries.
- **Phase 7** (FINAL.md + PROGRESS close) — MET (this commit
  series). `docs/tranches/AZ-II/FINAL.md` authored as PARTIAL CLOSE
  manifest. `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md`
  archived alongside C/E/F partials.

AZ-II closes as **PARTIAL** per `docs/instructions/README.md`
§"Substrate-with-consumer is one unit of work". The substrate
(cutover.A through cutover.H Phase 1) is canonical; the consumer
half (full regen-fleet activation, tape deletion, bench refresh,
codegen self-host) now routes to cutover.O under explicit scope. Later
cutover.K/L/M closed most of the regen-fleet activation; the remaining
terminal blockers are listed in the trajectory follow-on table below.

Master HEAD at cutover.H Phase 1 close: cherry-picked to master
at `1513328e` then `ee568213` (PARTIAL CLOSE FINAL.md authored).

## 2026-04-28 — cutover.I.5 + K Phases 0/1/2 + L Phase 3a + M Phase 3b/c/d

Five additional substages landed, advancing the trajectory toward
full close:

### cutover.I Phase 5 (`98008086` `c7e5999b`)

`BbnfBootstrap::serialize_compact_doc(doc: &BbnfDocument<'_>) -> String`
authored at `crates/core/src/runtime/bbnf/serialize.rs` (~430 LOC).
Typed walker over `BbnfDocument` keyed on `BbnfCompoundKind`; each
compound shape (Rule, Alternation, Concatenation, MappedFactor,
Closure, ImportDirective, PrettyDirective, …) emits its required
structural literals (`=`, `;`, `,`, `|`, `->`, `from`, `@import`,
…) in grammar order. `bbnf_rule` test in `serialize_roundtrip.rs`
un-ignored; idempotence verified — 19/19 serialize_roundtrip +
1/1 bbnf_bootstrap_reproducibility tests pass.

Phase 2 (non-BBNF resolver-arm fleet activation) DEFERRED on two
blockers diagnosed mid-dispatch:
1. JSON regen reproducibility — `bool` rule resolves to
   `TypeDesc::Span` instead of `TypeDesc::Bool`; cutover.K Phase 1
   surfaced this is a downstream symptom of cutover.G's
   bootstrap_parser dropping typed-leaf source text.
2. Per-shape Err open-compound frame leak — `flat`/`array`/`object`
   struct_direct emitters return `Err(...)` without closing open
   `begin_compound` frames; cutover.K Phase 2 fixed structurally.

PARTIAL close report at `docs/tranches/AZ-II/audit/cutover.I-PARTIAL.md`.

### cutover.K Phase 0 (`a09173dc`)

`bootstrap_parser.rs::parse_mapped_factor` wraps the mapping target
in an anonymous compound starting at `->`. `lower_mapped_factor`
discriminates the mapping by `c.span_text().starts_with("->")`;
without the wrapper, the arrow leaf and the value_expr are
siblings of `factor` and neither child satisfies the prefix test,
so the Map IR node was silently skipped (regression introduced in
cutover.G when the hand-written bootstrap parser replaced the
codegen-emitted parse_that-shaped tape).

### cutover.K Phase 1 (`cbf77e06`)

Typed-leaf source-text recovery in value-expression chain via
structural compound kinds + new `fold_typed_leaf_descendant`
walker. The reproducibility CI gate at
`crates/core/tests/bbnf_bootstrap_reproducibility.rs` extended
to `json_regen_is_idempotent` — closes the gap that hid the
TypeDesc::Bool regression (Blocker 1 from cutover.I PARTIAL).

### cutover.K Phase 2 (`7d283a8f`)

Per-shape Err paths close open compound frames via uniform IIFE
wrappers across `shapes/{flat,wrap,pratt,arglist}/struct_direct.rs`
— uniform fix per `feedback_no-orthogonal-codepaths`. Resolves
Blocker 2 from cutover.I PARTIAL.

### cutover.L Phase 3a (`b770fae7`)

Keyword-shape struct_direct emitter at
`crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`
handles Alt-of-Ref branches under StructDirect — Ref branches
prefix-check + delegate to the target shape fn (CSS L4
`pseudoClass` / `pseudoElement`). Per-rule emission gate at
`shapes/mod.rs` admits Wrap- and Keyword-classified transparent
rules under both substrate strategies.

### cutover.M Phase 3b/c/d (`a29a1265` `43f0795b`)

Resolver arms for CSV / Math / BNF / CSS Pretty flip to StructDirect
at `crates/ir/src/registry/strategy.rs::for_grammar`. AltDispatch
struct_direct emitter at
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs::emit_dispatch_arms_struct_direct`
extended: Alt-of-Literal / Alt-of-Regex / Alt-of-Seq branches now
emit byte-comparison + `push_leaf_with_unit()` + `push_branch_tag(idx)`
triples (pre-cutover.M these arms emitted empty placeholders that
collapsed BBNF `type_name` and CSS L4 pseudo-class arms into no-op
loops). All 9 grammars regen onto matching substrate; 8 of 9
returned concrete `Document` types directly at cutover.M close.

EBNF activation deferred — `letter`/`digit`/`symbol` Alt-of-many-
literal AltDispatch rules expose layout-routing depth (the
per-letter pushes don't yet route through `EbnfStructBuilder`'s
expected layout) beyond cutover.M's 300-min cap. Superseded by
cutover.O2, which flips EBNF to `EbnfDocument`.

`docs/tranches/AZ-II/FINAL.md` updated — PARTIAL CLOSE manifest
reflected 8/9 fleet activation at cutover.M close.
`docs/benchmarks/post-AZ-II.json` description updated to recap
cutover.A through cutover.M trajectory.

Workspace test posture at cutover.M close: 1514 / 1642 pass.
Net +85 tests fixed since cutover.D close.

## 2026-04-28 — cutover.N halt + comprehensive PROGRESS-SNAPSHOT

cutover.N dispatched at master `43f0795b` to close the remaining
phases — EBNF activation diagnosis + repair, Parsed<R> deletion,
tape migration / deletion, bench refresh + AZ-II FINAL terminal
close. Halted at organizational usage limit; no commits landed.

A comprehensive trajectory-state document was authored at
`docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` (master
`1d9a80bb`) capturing the full 14-substage trajectory
(cutover.A through cutover.N), agent dispatch history, hard-gate
readout, BA handoff verification, substrate inventory, and the
remaining work routed through cutover.O.

That snapshot is the implemented-state record: cutover.N landed no code
commits. The 2026-04-29 hardening audit does not change the implemented
state; it aligns the next cutover.O sequence so tooling preflight and
StructDirect builder transactions precede EBNF activation.

The trajectory's ~85% complete by structural milestones; ~70%
complete by LOC churn (tape migration is the long pole at ~10k
cross-crate refs).

## Handoff

- Opens on: AZ-I close (seven-point handoff contract verified).
- Closes into: BA (pointer queries on struct tree); BA opens on
  AZ-II FINAL CLOSE (currently PARTIAL pending cutover.O).
- BB opens on AZ-II close independently of BA's progress.
- BB.scaffold.A/B/C already landed at master `26f95469`/`4bb49ef2`/
  `9b20ded1`/`a4ca2b2f` (e-graph ruler + IR rewrites substrate);
  BB.close gates on AZ-II FINAL CLOSE.

## Trajectory follow-on

| Substage | Scope | Estimated cap |
|---|---|---|
| [cutover.O.0](waves/cutover/O0.md) | Tooling preflight: stale bench aliases, IAI CI, profiling scripts, release pin | LANDED |
| [cutover.O.1](waves/cutover/O1.md) | StructDirect builder transaction ABI across speculative branches | LANDED |
| [cutover.O.2](waves/cutover/O2.md) | EBNF diagnosis + generic AltDispatch structural-Seq repair | LANDED |
| [cutover.O.3a](waves/cutover/O3a.md) | Failure baseline + research/plan/redress cohort dispatch | complete_with_misses — all cohorts routed |
| [cutover.O.3a-J1](waves/cutover/O3a-J1.md) | JSON materialization, parity, and throughput cohort | complete_with_misses — bool payload to O4, projection accounting to O3, parity/`data_xl` to O6 |
| [cutover.O.3a-C1](waves/cutover/O3a-C1.md) | CSS admission, payloads, and lightningcss cohort | complete_with_misses — C1 source owners block O6 CSS truth until green |
| [cutover.O.3a-S1](waves/cutover/O3a-S1.md) | Sheets branch payload and serialization cohort | complete_with_misses — S1-E1/S1-R1/S1-SER1 block O6 Sheets truth until green |
| [cutover.O.3a-P1](waves/cutover/O3a-P1.md) | Projection totality and generated-view residue cohort | complete_with_misses — closes inside O3, no O3b required |
| [cutover.O.3a-A1](waves/cutover/O3a-A1.md) | Analysis/LSP/prototype/bootstrap disposition cohort | complete_with_misses — live repairs and archive/delete/bootstrap gates routed to O5/O6/O7 |
| [cutover.O.3](waves/cutover/O3.md) | Generated tape-view / `ValueRoot` residue purge for StructDirect | LANDED — `6a7e0f06`, zero-hit O3 scan, 32-test focused gate green |
| [cutover.O.4](waves/cutover/O4.md) | `Parsed<R>` deletion and `TapeDirect` fallback removal | LANDED — `c51f9742`, `815fbcea`, `3165e52f`, `58ea61a6`, `97061c41`, `8040bd69` |
| [cutover.O.5](waves/cutover/O5.md) | `crates/tape` deletion after relocating non-tape scan/index primitives | next active — 120 min |
| [cutover.O.6](waves/cutover/O6.md) | 17-entry close matrix + JSON sonic-rs / CSS lightningcss parity refresh | 90 min |
| [cutover.O.7](waves/cutover/O7.md) | AZ-II FINAL.md interim manifest -> terminal close conversion | 30 min |

Remaining estimate after O3: O4-O7 remain the terminal path. If a later gate
requires a new grammar-general inference/layout substrate, author
AZ-III for that substrate only; do not move tape deletion,
`Parsed<R>` deletion, stale benches, or parity gaps into AZ-III.

## 2026-04-29 — Parallel hardening audit

Parallel audit lanes reviewed B0-B7, AY-AZ-II, instructions,
gestalt, remaining trajectory, risk/perf, meta-audit, codegen paths,
the last 1200 commits, and current implementation wiring.

Integrated findings:

- AZ-II is interim, not terminal: 9/9 grammars are StructDirect after
  O2; O3 closed generated tape-view residue, while `Parsed<R>`,
  `TapeDirect`, and `crates/tape` remain.
- StructDirect speculative branches needed grammar-general builder
  transactions before EBNF/CSS/BBNF correctness claims were reliable;
  O1 landed that prerequisite.
- `Parsed<R>`, `TapeDirect`, and `crates/tape` remain live blockers.
- BBNF's hand-written `bootstrap_parser.rs` is a bridge, not the final
  canonical self-hosting parser.
- CSP/egraph/type-inference facts are underwired: egraph facts are not
  persisted as the shared semantic database; documented CSP tier/parent
  constraints are not fully installed; type inference still falls back to
  `BoxedEnum`.
- B0-B7 tooling mostly landed, but bench aliases, IAI CI, profiling
  scripts, release workflow pinning, and bench docs have drifted.
- `docs/tranches/meta-audit/` and
  `docs/tranches/next-tranche-research/` are historical provenance, not
  live planning canon. They should be archived after inbound links are
  rewritten.

Canonical follow-up ledger:
`docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md`.

## 2026-04-29 — cutover.O.0 tooling preflight

O0 repaired the command surfaces that would otherwise poison terminal
close evidence:

- Bench aliases now activate the feature tiers required by
  `json_parse_that`, `json_value`, competitor, stress, VM, and
  workspace bench surfaces.
- IAI CI now invokes `json_callgrind` with the `callgrind` feature and
  consumes a tracked `scripts/iai-compare.sh` instead of a missing
  helper.
- Profiling prep now targets `json_value`, carries the required
  `competitor` feature for that bench, and invalidates expand artifacts
  against `crates/core/src/grammar/generated/*.rs`.
- Release CI now installs the repository `rust-toolchain.toml` channel
  instead of floating `dtolnay/rust-toolchain@nightly`.
- The obsolete `scripts/bench_regression.sh` wrapper was deleted rather
  than retained as a second benchmark authority.

No performance or testing baseline was collected in O0 per user
instruction. O6 still owns JSON sonic-rs parity, CSS lightningcss typed
parity, and the 17-entry post-AZ-II close matrix.

## 2026-04-29 — cutover.O.1 StructDirect builder transactions

O1 landed the grammar-general speculative builder ABI required before
EBNF activation:

- `StructBuilder` now exposes `checkpoint`, `rollback`, and `commit`
  through an associated checkpoint type.
- All grammar-specific StructDirect builders capture open-frame stack,
  root, arena cursors, next-handle state, and pending CSS value state
  where applicable; rollback truncates arenas and restores the builder
  to the exact speculative entry point.
- StructDirect emitters now wrap speculative alternate, repeat,
  minus/negate, ref-led keyword, AltDispatch, array, arglist, unordered,
  flat, and wrap paths in checkpoint/commit/rollback transactions.
- The generated fleet was refreshed through canonical `cargo xtask
  regen`; `cargo xtask regen --check` is clean across all 9 grammars.
- Focused wire-contract tests cover JSON rollback of completed roots
  and open frames plus CSS rollback of nested rule attempts.

Validation: `cargo check -p bbnf --lib --profile ax-iter`; focused
JSON and CSS StructDirect checkpoint tests; `cargo xtask regen --check`;
`git diff --check`. No performance or close-matrix baseline was
collected in O1 per instruction; O6 still owns semantic/parity
throughput truth.

## 2026-04-29 — cutover.O.2 EBNF direct projection

O2 landed the last production grammar resolver-arm flip:

- `EmitStrategy::for_grammar` now routes `EbnfParser` /
  `EbnfGrammar` through `EbnfStructBuilder` and `EbnfDocument`.
- StructDirect structural `Seq` branch emission is shared through the
  inline branch walker and consumed by both Keyword and AltDispatch
  shapes. AltDispatch now preserves nested structural children and
  commits the owning branch tag transactionally, which admits EBNF
  grouped terms such as `{ digit }`, `[ rhs ]`, and `( rhs )`.
- Full canonical regen refreshed the affected generated fleet
  (`bbnf`, `css_l4`, `google_sheets`, `ebnf`); `cargo xtask regen
  --check` is clean across all 9 grammars.
- `EbnfParser::parse` now returns `EbnfDocument<'_>`.

Validation: `cargo check -p bbnf --lib --profile ax-iter`;
`cargo test -p bbnf --profile ax-iter --test ebnf_prettify -- --nocapture`;
`cargo test -p bbnf --profile ax-iter --test serialize_roundtrip ebnf_rule -- --nocapture`;
`cargo test -p bbnf --profile ax-iter --test typed_accessor_surface ebnf_compile_time_accessors -- --nocapture`;
`cargo test -p bbnf --profile ax-iter --test emit_strategy -- --nocapture`;
`cargo test -p bbnf --profile ax-iter --test bbnf_bootstrap_reproducibility -- --nocapture`;
affected-consumer smokes for CSS L4, BBNF self-parity, and Sheets
self-parity; `cargo xtask regen --check`; `git diff --check`.
No performance or close-matrix baseline was collected in O2 per user
instruction; O6 still owns semantic/parity throughput truth.

## 2026-04-29 — cutover.O3a failure baseline and triumvirate redress

Post-documentation baseline:

- `scripts/test-tier.sh workspace --profile ax-iter --no-fail-fast`
  ran 1645 tests: 1561 passed, 84 failed, 25 skipped. Failure list:
  `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`.
- `make ay-bench-close WAVE=az-ii-doc-baseline` failed in the JSON
  lane. `json_monolithic` measured canada/citm/data_s, then
  `data_xl` exceeded the bench timeout: 2.478697958s against a 1s
  cap. Transcript:
  `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`.

O3a routes every failure through five cohorts before source redress
continues:

| Cohort | Scope |
|---|---|
| J1 | JSON materialization, typed accessors, canonical/sonic parity, structural/wrap tests, and JSON bench timeout |
| C1 | CSS comment admission, hex/named-color payloads, pseudo/selector payloads, and lightningcss parity |
| S1 | Sheets branch payloads, error literals, operator/range/unary parsing, and serialize self-parity |
| P1 | projection totality and generated view / `ValueRoot` residue |
| A1 | analysis/LSP regressions and historical `json-prototype` disposition |

Round 1 dispatch used research + plan + redress triads for J1, C1, and
S1. Round 2 used the same triad shape for P1 and A1. Plan agents
created or amended wave specs before source redress was permitted.

Six-agent audit synthesis:
`docs/tranches/AZ-II/audit/O3a-six-agent-audit-synthesis-2026-04-29.md`.
Round 2 synthesis:
`docs/tranches/AZ-II/audit/O3a-round2-synthesis-2026-04-29.md`.
The audit confirms the five cohorts exhaust the 84 failed tests and the
JSON `data_xl` bench timeout, but J1/C1/S1 required finer-grained child
wave specs before any source redress. Those deployable specs now exist:

| Child spec | Owned surface | Blocks |
|---|---|---|
| [O3a-J1](waves/cutover/O3a-J1.md) | 24 JSON tests plus `json_monolithic::data_xl` timeout | O3/O4/O6: `O3a-J1-{research,plan,redress}.md` integrated |
| [O3a-C1](waves/cutover/O3a-C1.md) | 17 CSS/lightningcss tests | O6 CSS parity: `O3a-C1-{research,plan,redress}.md` integrated |
| [O3a-S1](waves/cutover/O3a-S1.md) | 33 Sheets tests | S1-E1/S1-R1/S1-SER1 then O6: `O3a-S1-{research,plan,redress}.md` integrated |
| [O3a-P1](waves/cutover/O3a-P1.md) | 1 projection-totality test plus generated-view residue | O3 close: `O3a-P1-{research,plan,redress}.md` integrated; no O3b required |
| [O3a-A1](waves/cutover/O3a-A1.md) | 2 analysis/LSP tests, 7 json-prototype tests, bootstrap/JIT disposition | O5/O6/O7: `O3a-A1-{research,plan,redress}.md` integrated |

Round 1 integration:

- J1 research proves JSON bool branches materialize as null, numeric
  parity has oracle-witness drift, JSON accessor/structural tests need
  document-owned projection accounting, and `data_xl` is a full
  materialization timeout. The J1 plan routes bool payload repair to
  O4, projection accounting to O3, and corpus parity plus `data_xl`
  measurement to O6.
- C1 research/probes separate CSS corpus admission from payload loss.
  Hex host-function payloads, named-color literal payloads, and
  `:dir(...)` selector materialization are source-owned before O6.
  Bootstrap/tailwind admission and lightningcss parity block O6 CSS
  truth while red.
- S1 research/probes assign all 33 Sheets failures outside O4's return
  model: S1-E1 owns mapped branch payloads, S1-R1 owns sheet-prefix and
  range admission, and S1-SER1 owns compact serializer fixed points.
  O6 verifies Sheets health after those source owners land.

Round 2 integration:

- P1 research/plan/redress reproduced
  `projection_totality_runtime_call_count` as a stale runtime witness
  over the old generated `ValueRoot` / `<Grammar>Value` materializer
  path. Structural totality still passes (`JSON=3 CSS_L4=53 Sheets=10
  BBNF=15 -> total=81`). O3 owns the fix: generated view/value
  residue purge plus document-owned projection/accessor test rewrite.
  No O3b is required unless O3 redress proves an out-of-bounds owner.
- A1 research/plan/redress split live product failures from historical
  prototype surface. `bbnf-analysis::directives
  import_directive_has_semantic_tokens` and `bbnf-lsp::integration
  test_hover_recover_keyword` are live directive-span repairs.
  `json-prototype` is archive/delete-owned, not fixture-seed repair;
  the retired Gorgeous derive-backed JIT is deletion-owned inside
  AZ-II; `bootstrap_parser.rs` remains only as a bounded bridge until
  O6 proves generated self-host or blocks O7.

## 2026-04-29 — cutover.O3 generated view purge

O3 closed the StructDirect generated-view residue surface without
creating a compatibility shim or an O3b child wave.

Implementation:

- `6a7e0f06` gates the tape-first serializer generator to
  `EmitStrategy::TapeDirect`; StructDirect grammars no longer emit
  `serialize_*`, `serialize_compact`, or `__dispatch_serialize` APIs
  over `<Grammar>NodeView<'_>`.
- Generated scan-policy documentation now uses substrate-neutral
  wording, so generated StructDirect files do not retain `TapeCursor`
  links as documentation residue.
- `regen_shape_goldens` now asserts the O3-owned residue class:
  `TapeCursor`, generated `NodeView`, `ValueRoot`,
  `materialize_projection_*`, `PROJECTION_MATERIALIZERS`, and
  `PROJECTION_CONSUMERS`. General `crate::runtime::tape` references
  are classified under O4/O5.
- `structural_object_two_pairs` now asserts the document-owned JSON
  object shape: object root plus two scalar values; object keys remain
  `JsonPair::key` and are verified through the pair slice.

Evidence:

- `docs/benchmarks/AZ-II/cutover/O3-generated-view-scan.txt` records
  zero O3 residue hits and zero node-view serializer hits across the
  nine generated grammars.
- The same artifact records 9511 generated `crate::runtime::tape`
  references as O4/O5 return-model/tape-substrate work and zero
  generated `Parsed<` hits.
- `cargo xtask regen --check` is clean across all 9 grammars.
- Focused O3 gate:
  `cargo nextest run -p bbnf --test projection_totality --test named_type_preservation --test typed_accessor_surface --test runtime_root --test regen_shape_goldens --cargo-profile ax-iter --no-fail-fast -- --nocapture`
  ran 32 tests: 32 passed, 1 skipped.
- O3.J1-P1 sub-gate:
  `cargo nextest run -p bbnf --test typed_accessor_surface --test structural --cargo-profile ax-iter --no-fail-fast json_accessor_surface structural_object_two_pairs -- --nocapture`
  ran 2 tests: 2 passed.

Next instruction at O3 close was O4: delete `Parsed<R>` and
`TapeDirect`, then route remaining tape-substrate deletion to O5.

## 2026-04-30 — cutover.O4 Parsed/TapeDirect deletion

O4 closed the live production return-model surface without adding a
compatibility wrapper.

Implementation commits:

- `c51f9742` repairs JSON bool branch payload deposition through the
  concrete document builder.
- `815fbcea` makes unknown production grammar strategy selection fail
  closed instead of falling back.
- `3165e52f`, `58ea61a6`, and `97061c41` delete the generated
  `Parsed` parse return surface, purge generated imports, and remove
  the runtime `Parsed` module/export.
- `8040bd69` removes StructDirect tape-offset success/control payloads,
  renames the direct Ref-call helper to `emit_ref_call_shape`, deletes
  obsolete tape-only shape emitter helpers, regenerates all nine
  grammars, refreshes shape-dispatch goldens, and archives the O4 scan.

Evidence:

- `docs/benchmarks/AZ-II/cutover/O4-parsed-tapedirect-scan.txt`
  records zero production hits for `Parsed`, `Parsed::new`,
  `TapeDirect`, `EmitStrategy::TapeDirect`, generated `TapeOffset`,
  `emit_ref_call_tape`, `TapeOffset::NONE`, and `Option<TapeOffset>`
  return payloads in the O4-owned source scopes.
- `cargo check -p bbnf --lib --profile ax-iter` passed.
- `cargo xtask regen --check` passed across all 9 grammars.
- `cargo nextest run -p bbnf --test emit_strategy --test
  typed_accessor_surface --test keyword_ref_branch_wire_contract
  --cargo-profile ax-iter --no-fail-fast -- --nocapture` ran 30
  tests: 30 passed.
- `cargo test -p bbnf --test shape_dispatch_emission --profile
  ax-iter -- --nocapture` ran 29 tests: 29 passed.

O3a disposition:

- J1's JSON bool payload failure is O4-owned and closed by
  `c51f9742`; no adapter return path was introduced.
- S1 remains non-return-model-owned per
  `docs/tranches/AZ-II/audit/O3a-S1-plan.md`; O4's Sheets return-model
  scan has zero `Parsed` / `TapeDirect` hits.

Next instruction: open O5 under `waves/cutover/O5.md`. O5 owns
`crates/tape`, remaining `runtime::tape` primitives, simd-scan index
relocation, tape-only tests/benches/examples, `json-prototype`,
Gorgeous JIT, manifest severance, and the no-shim deletion scan. O6/O7
must not claim semantic close or terminal close until O5 is green.
