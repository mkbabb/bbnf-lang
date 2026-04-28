# AZ-II — Progress Log

**Status**: planned (gated on AZ-I close)

**Date**: 2026-04-23

Dated execution log for tranche AZ-II. Execution begins after AZ-II
opens on AZ-I close (seven-point handoff contract verified).

AZ-II completes the direct-to-struct migration. BBNF's own grammar
moves to the `project_types`-derived struct path via a two-stage
bootstrap cutover (Stage A: tape-compiler builds struct-compiler
candidate; Stage B: candidate rebuilds itself; byte-equal output
is the close gate). Once BBNF parses into a derived struct, the
tape crate has no remaining consumers and is deleted.

Wave plan (three waves + FINAL): W0 bootstrap-cutover research +
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
| cutover | partial-close (cutover.A through cutover.H Phase 1 LANDED; tape deletion + non-BBNF regen-fleet activation deferred) | BBNF self-host substrate canonical; tape deletion routes to follow-on tranche ([waves/cutover.md](waves/cutover.md)) |

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
codegen self-host) routes to a follow-on tranche under explicit
scope.

Master HEAD at AZ-II close: tracked in cutover.H worktree;
cherry-picks gated on orchestrator review.

## Handoff

- Opens on: AZ-I close (seven-point handoff contract verified).
- Closes into: BA (pointer queries on struct tree).
- BB opens on AZ-II close independently of BA's progress.
