# AZ-II — Progress Log

**Status**: partial close. Implemented-state record:
[`PROGRESS-SNAPSHOT-2026-04-29.md`](PROGRESS-SNAPSHOT-2026-04-29.md).
Live terminal sequence: `cutover.O.0` through `cutover.O.7`.

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
`docs/tranches/AZ-II/waves/cutover.md`. The W0 / W1 / W2 wave docs
carry supersede notices and stay on disk as historical record.

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
| cutover | partial-close (cutover.A through cutover.M LANDED; cutover.N halted at usage limit; EBNF activation + Parsed<R> deletion + tape deletion + bench refresh deferred) | 8/9 grammars StructDirect; terminal hardening routes through cutover.O.0-O.7 with builder transactions before EBNF activation ([waves/cutover.md](waves/cutover.md)) |

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
return concrete `Document` types directly.

EBNF activation deferred — `letter`/`digit`/`symbol` Alt-of-many-
literal AltDispatch rules expose layout-routing depth (the
per-letter pushes don't yet route through `EbnfStructBuilder`'s
expected layout) beyond cutover.M's 300-min cap.

`docs/tranches/AZ-II/FINAL.md` updated — PARTIAL CLOSE manifest
reflects 8/9 fleet activation.
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
| cutover.O.0 | Tooling preflight: stale bench aliases, IAI CI, profiling scripts, release pin | 60 min |
| cutover.O.1 | StructDirect builder transaction ABI across speculative branches | 120 min |
| cutover.O.2 | EBNF diagnosis + generic AltFacts/layout-routing repair | 120 min |
| cutover.O.3 | Generated tape-view / `ValueRoot` residue purge for StructDirect | 90 min |
| cutover.O.4 | `Parsed<R>` deletion and `TapeDirect` fallback removal | 90 min |
| cutover.O.5 | `crates/tape` deletion after relocating non-tape scan/index primitives | 120 min |
| cutover.O.6 | 17-entry close matrix + JSON sonic-rs / CSS lightningcss parity refresh | 90 min |
| cutover.O.7 | AZ-II FINAL.md PARTIAL → FINAL CLOSE conversion | 30 min |

Total estimated: ~12 hours sequential under fan-out. If EBNF requires a
new grammar-general inference/layout substrate beyond `AltFacts` +
transactional builders, author AZ-III for that substrate only; do not
move tape deletion, `Parsed<R>` deletion, stale benches, or parity gaps
into AZ-III.

## 2026-04-29 — Six-lane hardening audit

Six parallel audit lanes reviewed B0-B7, AY-AZ-II, instructions,
gestalt, remaining trajectory, risk/perf, meta-audit, codegen paths,
the last 1200 commits, and current implementation wiring.

Integrated findings:

- AZ-II is partial, not terminal: 8/9 grammars are StructDirect; EBNF
  remains TapeDirect.
- StructDirect speculative branches need grammar-general builder
  transactions before EBNF/CSS/BBNF correctness claims are reliable.
- `Parsed<R>`, `TapeDirect`, generated tape views, and `crates/tape`
  remain live blockers.
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
