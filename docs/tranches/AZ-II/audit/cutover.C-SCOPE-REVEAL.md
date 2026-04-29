# AZ-II.cutover.C — Scope-reveal report

**Date**: 2026-04-28
**Worktree**: `/tmp/bbnf-worktrees/cutover-C` (detached HEAD `a4ca2b2f`)
**Cap**: 120 min, halted with relinquishment under
`docs/instructions/README.md` §"Relinquish when stuck" + §"No
deferrals" + "Substrate-with-consumer is one unit of work".

## Trigger

The cutover.C dispatch brief framed five-to-seven mechanical
commits (regen + entry-site fixes + parity recode + tape deletion +
bench archive + FINAL.md) within a 120-min cap. On contact, the
substrate state diverged from the brief's assumption in two ways
that compound into a tranche-rescoping signal rather than an
implementation-level escalation.

## Discovery 1 — consumer migration is structurally larger than the cap

The cutover.A baseline `crates/core/src/grammar/generated/bbnf.rs`
ships a TapeDirect parser. Running `cargo xtask regen --grammar
bbnf` against the post-cutover.A compiler with the resolver-arm at
`crates/ir/src/registry/strategy.rs::EmitStrategy::for_grammar`'s
`BbnfBootstrap` arm produces a struct-direct parser whose entry
point returns `crate::runtime::bbnf::BbnfDocument<'_>` instead of
`crate::runtime::Parsed<'_, BbnfBootstrap>`.

The codebase carries **700 references to `BbnfBootstrapNodeView`**
across `crates/core/`, `crates/gorgeous/`, `crates/lsp/`, and
`crates/analysis/`. The consumer surface includes:

| Module | Surface | LOC affected |
|---|---|---|
| `crates/core/src/grammar/host.rs` | `extract_observational` / `extract_for_pipeline` walk `&Parsed<'_, BbnfBootstrap>` and dispatch on `BbnfBootstrapRuleKind` per child node | 619 |
| `crates/core/src/lower/tape_walk.rs` | `find_descendant_by_kind`, `find_rhs_expression_descendant`, `peel_alt_wrapper` — recursive cursor walks over `BbnfBootstrapNodeView` | 339 |
| `crates/core/src/lower/mod.rs` + sub-modules | `lower_rhs`, `lower_term`, `lower_factor`, `lower_pratt`, etc. — every IR lowering pass consumes `BbnfBootstrapNodeView` | ~1100 across 10 files |
| `crates/core/src/lower/value_expr/` | Pratt + atom + closure lowering over view types | ~600 across 6 files |
| `crates/core/src/graph/{deps,metadata}.rs` | Module dependency tracking + recover-binding metadata over view types | ~250 |
| `crates/core/src/types.rs` | `RuleEntry { rhs: BbnfBootstrapNodeView<'a> }` — RHS storage type pervades the AST surface | ~50 |
| `crates/core/src/pipeline/{compile,directives}.rs` | Parse entry sites + module loader signatures | ~200 |
| `crates/analysis/src/state/ast_utils/` + features | LSP semantic tokens, hover, formatting, span queries — every ast-walk consumer | ~400 across 8 files |
| `crates/core/tests/bbnf_*_parity.rs` | Parity harness compares cursor walks | 745 across 3 files |

Migration is not 1:1 because `BbnfBootstrapNodeView` exposes
tape-cursor semantics (`kind() -> TapeKind`, `variant_idx() -> u8`,
`span() -> (u32, u32)`, `child(i)`, `children()`) that differ from
`BbnfView`'s `kind() -> BbnfKind`, `focus() -> BbnfValue<'p>`,
`children() -> impl Iterator<Item = BbnfView>`. Lower passes
matching on tape kinds (`TapeKind::Repeat`, `TapeKind::Alt`,
`TapeKind::Seq`) need re-architected dispatch through
`BbnfCompoundKind` enum branches.

Authentic migration of all 700 sites — without stubs, without
`BbnfBootstrapNodeView` compatibility shims, without orthogonal
codepaths — is a multi-day effort, not a 120-min sub-stage.

## Discovery 2 — codegen template still emits tape substrate

The post-regen `crates/core/src/grammar/generated/bbnf.rs`
contains **1218 references to `tape::*` symbols** even after the
StructDirect resolver-arm activates. The struct-direct `parse()`
function at line 28396 internally:

- Allocates a `crate::runtime::tape::Tape::<()>::with_capacity(...)` (no — corrected: actually allocates `BbnfStructBuilder::new()` per the regen output line 28404; the tape allocation is gone from the parse body)
- Maps `crate::runtime::tape::DtaError` variants to `ParseErr` (line 28414, 28420, 28426)
- Emits `BbnfBootstrapNodeView` and `BbnfBootstrapRuleKind` types alongside the StructDirect surface (lines 17902, 17914 in regen output) — these are dead code in the StructDirect path but still reach the build

`crates/tape/` is therefore not deletable on cutover.C alone:
- `crates/core/src/grammar/generated/{json,css_l4,google_sheets,csv,math,ebnf,bnf,css_pretty}.rs` (the 8 non-BBNF grammars) all carry `use crate::runtime::tape::*` and consume `Tape<()>`, `TapeBuilder`, `TapeCursor`, `TapeKind` directly — these grammars stayed on TapeDirect through AZ-I.W2-act
- The 8 grammars' `Root for <Grammar>` impls (e.g. line 18131 in bbnf.rs's regen output) require `&Tape<()>` parameters per `crate::runtime::Root::make_view` — the trait surface itself binds `Tape<()>` into the view-construction protocol
- `crates/core/src/runtime/parsed.rs::Parsed<'p, R>` carries `tape: Tape<R>` as a field (line 84) — `Parsed` is referenced at hundreds of sites; deleting tape requires re-architecting `Parsed`

These cross-grammar couplings were not visible in the cutover/README.md
plan's "structural cutover deletes tape on Commit 4" framing.
Per `feedback_no-orthogonal-codepaths` no shrunken-tape-retained-
for-some-grammars floor is permitted; per `feedback_no-workarounds`
no temporary tape-keeps-but-renames-itself shim is permitted.

## Sub-gate readout

cutover/README.md's cutover.C sub-gate enumerates 7 closure conditions:

| # | Condition | Status | Evidence |
|---|---|---|---|
| 1 | `crates/tape/` does not exist on disk | NOT MET | `find crates/tape -type d` returns the directory; deletion blocked by Discovery 2 |
| 2 | `cargo build -p bbnf --no-default-features` succeeds | NOT MET (premise unmet) | `crates/tape` not deleted |
| 3 | Live tape-symbol scan returns zero | NOT MET | Generated bbnf.rs alone carries 1218 tape refs post-regen; the 8 sibling grammars carry their full TapeDirect substrate |
| 4 | `docs/benchmarks/post-AZ-II.json` exists | DEFERRED | Bench in flight; no post-tape-delete numbers possible since tape stayed |
| 5 | AZ-II FINAL.md committed | LANDED via this scope-reveal report's accompanying FINAL.md | (see `docs/tranches/AZ-II/FINAL.md`) |
| 6 | Workspace nextest 0 failures | NOT VERIFIED | Cap consumed by scope analysis; bench in-flight |
| 7 | 17-entry matrix at AZ-I baseline | DEFERRED | Bench in flight; substrate same as cutover.A baseline so numbers expected to match AZ-I close |

## Hard-gate readout (cutover/README.md §"Hard gate")

| # | Gate | Status |
|---|---|---|
| 1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green | NOT MET (Discovery 2) |
| 2 | Stage A / Stage B byte-equal across BBNF fixture corpus; permanent CI gate green | MET (cutover.B; `crates/core/tests/bbnf_bootstrap_reproducibility.rs` exists; stage-a-bbnf.rs vs stage-b-bbnf.rs `diff -q` empty) |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide | NOT VERIFIED in this dispatch |
| 4 | `StructRegistry` non-empty for every Named rule in the four grammars including BBNF | MET (cutover.A: `populate_struct_registry` returns layouts for BBNF per cutover.A's `bbnf-ir::registry::structs` regression test) |
| 5 | Parity harnesses recoded to struct-vs-external on all four grammars | NOT MET (Discovery 1) |
| 6 | 17-entry matrix at AU floor on every entry; BBNF self-parse within ±10% of AU baseline | IN FLIGHT (bench started but not landed) |
| 7 | AZ-II FINAL.md + `docs/benchmarks/post-AZ-II.json` exist on master | PARTIAL (FINAL.md authored as scope-reveal close; bench JSON deferred) |
| 8 | Decay sweep: `dta.rs` ≤ 720 LOC; `tape::dta` and `tape::visitor` gone; pattern_alphabet decay items gone | MET in cutover.A (commits `63cacbe2`, `19a2669a`, `d3977825`) |

## BA handoff verification (AZ-II.md §"Handoff contract to BA")

| # | Contract item | Status |
|---|---|---|
| 1 | All four grammars running direct-to-struct | PARTIAL (3 of 4: JSON / CSS L4 / Sheets via AZ-I; BBNF resolver-arm landed but consumer migration not complete) |
| 2 | `crates/tape/` deleted | NOT MET (Discovery 2) |
| 3 | `StructRegistry` closed fleet-wide | MET (cutover.A) |
| 4 | Parity harnesses rewired to struct comparisons | PARTIAL (3 of 4: JSON / CSS L4 / Sheets at AZ-I close; BBNF unmigrated) |
| 5 | Full 17-entry matrix at AU parity on struct-only path | NOT MET (BBNF stays on tape; matrix expected to match AZ-I close) |
| 6 | BBNF self-parse byte-reproducible | MET (cutover.B byte-equal CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs`) |
| 7 | Parent-pointer decision surface open | MET (no blocker to BA.W0's pointer-vs-traversal study; data-grammar trees are closed structurally) |

## Recommended re-plan

The cutover wave's third sub-stage requires re-tranching against
the discovered scope. Two paths align with repository discipline:

### Path A — cutover.D (consumer migration sub-stage)

Open `docs/tranches/AZ-II/waves/cutover/D.md` as a new sub-stage
between cutover.B and the original cutover.C, scoped to:

1. **Migrate `host.rs`** (619 LOC) from `&Parsed<BbnfBootstrap>` +
   `BbnfBootstrapNodeView` to `&BbnfDocument<'_>` + `BbnfView<'_,
   '_>`. Author equivalent walkers (`peel_iter_wrapper`,
   `peel_wrappers`, `absorb_item`, `decode_*` family) over
   `BbnfCompoundKind` dispatches.
2. **Migrate `lower/`** (~1100 LOC across 10 files) — every
   `lower_rhs`, `lower_term`, etc. consumes `BbnfBootstrapNodeView`;
   re-target each at `BbnfView`. Tape-kind dispatches
   (`TapeKind::Repeat`, etc.) re-target at `BbnfCompoundKind` arms.
3. **Migrate `lower/value_expr/`** (~600 LOC) — Pratt + closures +
   atom lowering.
4. **Migrate `graph/`** (~250 LOC) — module dependency traversal +
   recover binding extraction.
5. **Migrate `pipeline/{compile,directives}`** entry sites + AST
   carrier types (`types.rs::RuleEntry::rhs`).
6. **Migrate `analysis/`** (~400 LOC) — LSP features.
7. **Migrate parity harnesses** (745 LOC) — struct-vs-existing-ast
   comparisons.

Cap estimate: ~480 min spread across 3-4 parallel sub-agents in
their own worktrees per `docs/instructions/README.md` §"Worktree
isolation". Sub-agents own non-overlapping file bounds; grammar
emitter is frozen during execution.

### Path B — codegen template carve-out

Re-scope cutover.C such that the StructDirect codegen template at
`crates/core/src/backend/rust/emitter/grammar.rs` ELIDES the tape
view types (`BbnfBootstrapNodeView`, `BbnfBootstrapRuleKind`,
`grammarView`, etc.) when `EmitStrategy::is_struct_direct()` —
forcing consumer migration as a compile-error consequence rather
than a planned sweep. This makes cutover.D unavoidable and is
shorter to author (~50-100 LOC in the emitter), but moves the
entire scope-reveal forward to compile time without addressing it.

Path A is the canonical path per `feedback_no-deferrals`.

## Decay reclaim totals (this dispatch)

Zero LOC reclaimed. `crates/tape/` retained on disk (3578 LOC across
14 files). The dispatch's substrate work (regen attempts) was
reverted to maintain a clean working tree for the orchestrator.

## Recommendation to orchestrator

1. **Cherry-pick this scope-reveal report** + the accompanying
   `FINAL.md` + `PROGRESS.md` close entry as the AZ-II.cutover.C
   close commits.
2. **Open `cutover/D.md`** authoring follow-on Path A above.
3. **Land bench JSON when available** (in-flight at relinquishment;
   captures cutover.A baseline state — same as AZ-I close since
   neither cutover.B nor cutover.C lands runtime substrate that
   touches the parse hot path).
4. **Block BA.W0** pending cutover.D close — handoff contract
   items 1, 2, 4, 5 not yet met.
5. **Consider re-evaluating cutover/README.md's tape deletion sub-gate**
   — Discovery 2 reveals `crates/tape/` is consumed by 8 non-BBNF
   grammars' generated parsers and by `Parsed<R>` itself. Tape
   deletion requires either (a) migrating all 9 grammars off
   TapeDirect (well outside the AZ-II scope), or (b) re-architecting
   `crate::runtime::Parsed` to not require tape (out-of-scope).

The discipline here is `docs/instructions/README.md` §"Substrate-
with-consumer is one unit of work" — landing a struct-direct BBNF
parser without verified runtime consumers does not close the wave.
The honest action is relinquishment with concrete redispatch
guidance, not a substrate-only landing dressed as completion.
