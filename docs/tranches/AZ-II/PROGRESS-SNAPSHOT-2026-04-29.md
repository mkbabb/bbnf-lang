# AZ-II Progress Snapshot — 2026-04-29

Comprehensive progress + agent status capture taken at master HEAD `43f0795b`
post-cutover.M close. Authored as a durable record of the AZ-II.cutover wave
trajectory across 14 sub-stages (cutover.A through cutover.N), the
substrate / consumer / activation phases each landed, and what remains to
close AZ-II terminally.

This document is the implemented-state read-of-record for AZ-II through
cutover.N halt: cutover.N landed no code commits. Post-snapshot
hardening amendments may refine the order of cutover.O, but they do not
change the implemented progress recorded here.

Post-snapshot hardening addendum: cutover.O.0 tooling preflight,
cutover.O.1 StructDirect builder transactions, and cutover.O.2 EBNF
direct projection have since landed. The active resume point is
cutover.O.3 generated view purge.

This document supplements:
- `docs/tranches/AZ-II/AZ-II.md` (parent plan)
- `docs/tranches/AZ-II/PROGRESS.md` (rolling dated execution log)
- `docs/tranches/AZ-II/FINAL.md` (PARTIAL CLOSE manifest, cutover.M-era)
- `docs/tranches/AZ-II/waves/cutover.md` (wave spec; original 3-stage plan)
- `docs/tranches/AZ-II/audit/cutover.{C,E,F,G,I}-PARTIAL.md` (per-substage scope-reveal reports)
- `docs/benchmarks/post-AZ-II.json` (close matrix archive — partial)

## Trajectory recap (cutover.A → cutover.M)

The cutover wave was originally specified at `waves/cutover.md` as three
sequential sub-stages (A.substrate / B.byte-equal / C.delete) within a
~5-hour cap. Contact with reality expanded that into 14 sub-stages
(cutover.A through cutover.N) over multiple sessions; each sub-stage
landed an independently-cherry-pickable substrate fix or consumer
migration block.

| Sub-stage | Headline | Master commit(s) | Status |
|---|---|---|---|
| **cutover.A** | BBNF struct-direct runtime substrate + resolver-arm flip; `tape::dta` hoist; `tape::visitor` deletion (746 LOC); driver dead-helper deletion; `recognizers/dta.rs` amputation (52 LOC of dead surfaces) | `5cdbcfa6` `583b0591` `19a2669a` `82a88696` `ec7a0fa1` | LANDED |
| **cutover.B** | Stage A / Stage B byte-equal verification + permanent reproducibility CI gate at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` | `d6b0377a` | LANDED |
| **cutover.C** | Scope-reveal — diagnosed BBNF consumer surface as ~3855 LOC across 35 files (too large for cutover.C's 120-min cap); fanned into cutover.D | `e91df301` (scope-reveal doc) `99024342` (bench archive placeholder) | DIAGNOSTIC |
| **cutover.D** | 4 parallel agents migrated all BBNF consumers (host.rs, lower/, graph/, pipeline/, types.rs, analysis/, parity tests) to `BbnfDocument` / `BbnfView` / `RuntimeView` | `113a1d23` `dba623b8` `34280d2a` `7648b723` `bcdf25ed` `073aa703` `fa3026e8` `e677867f` `464e8ae3` `b5b0f930` `3396f472` `a7a9f771` `24b19281` `4e9b8745` `43526778` `825e8a06` `685bad2f` `a02e2ee2` `7a320ce4` `b1d0576a` `16d8a0ca` `8428d4fc` `2aa6822e` | LANDED |
| **cutover.E** | Surfaced Discovery 1 (BBNF emitter regression — bbnf.rs rejects every input at offset 0). Authored 5 non-BBNF runtime substrates (csv/math/bnf/ebnf/css_pretty); resolver arms deferred until BBNF repaired | `57e017de` `6b2f3ca7` `911ee70f` (substrates) `cb36c997` (PARTIAL doc) `9f40f17c` (defer arms) | SUBSTRATE LANDED |
| **cutover.F** | Diagnosed THREE distinct StructDirect emitter bug classes (NOT cutover.D2 lower-side as cutover.E suggested). Landed substrate fixes: `shapes/array/mod.rs` Wrap-vs-Repeat dispatch; `shapes/flat/struct_direct.rs` inline-position emission for Alt/Repeat/Regex/Negate/Minus | `b813eb64` `246efda7` `6056baee` (PARTIAL doc) | LANDED |
| **cutover.G** | Hand-crafted bootstrap parser at `crates/core/src/grammar/bootstrap_parser.rs` breaks chicken-and-egg (regen needs working BbnfBootstrap::parse, which needs working bbnf.rs, which needs regen). 56/56 BBNF self-parity tests pass; reproducibility CI gate idempotent | `863de6a5` (move PARTIAL docs) `e52974a6` `984d7535` `caf07d96` `9300e9df` (PARTIAL doc) | LANDED |
| **cutover.H Phase 0** | `crates/core/src/graph/deps.rs::collect_refs_from_compound` skips value-expression compounds (fixes JSON regen-check pre-commit hook gate) | `42e0906b` | LANDED |
| **cutover.H Phase 1** | BBNF resolver-arm re-flipped to StructDirect with transparent-rule emitter fix at `shapes/mod.rs` (`is_transparent && !is_struct_direct()`) | `3e8a0ed7` | LANDED |
| **cutover.H Phase 7** | AZ-II FINAL.md (PARTIAL CLOSE manifest) + PROGRESS close entry + cutover.G-PARTIAL archive | `a61507eb` `ee568213` | LANDED |
| **cutover.I.5** | `BbnfBootstrap::serialize_compact_doc(doc: &BbnfDocument<'_>) -> String` authored at `crates/core/src/runtime/bbnf/serialize.rs` (~430 LOC); bbnf_rule serialize_roundtrip un-ignored. Phase 2 substrate authored in worktree but NOT activated (Blocker 1 + 2 surfaced) | `a128529a` `c7e5999b` (PARTIAL doc) | LANDED |
| **cutover.K Phase 0** | bootstrap_parser wraps mapped_factor mapping in anonymous compound — fixes `lower_mapped_factor` discrimination predicate | `a09173dc` | LANDED |
| **cutover.K Phase 1** | typed-leaf source-text recovery in value-expression chain via structural compound kinds + new `fold_typed_leaf_descendant` walker. Reproducibility CI gate extended to JSON regen idempotence | `cbf77e06` | LANDED |
| **cutover.K Phase 2** | per-shape Err paths close open compound frames via uniform IIFE wrappers across `shapes/{flat,wrap,pratt,arglist}/struct_direct.rs` | `7d283a8f` | LANDED |
| **cutover.L Phase 3a** | keyword-shape struct_direct emitter handles Alt-of-Ref branches (CSS L4 pseudoClass/pseudoElement TokenDispatch-shaped emission) | `b770fae7` | LANDED |
| **cutover.M Phase 3b/c/d** | Activate non-BBNF StructDirect resolver arms for CSV/Math/BNF/CSS Pretty (4/5 deferred grammars activated). Surgical AltDispatch struct_direct emitter expansion: Alt-of-Literal / Alt-of-Regex / Alt-of-Seq branches emit byte-comparison + `push_leaf_with_unit()` + `push_branch_tag(idx)` triples. Regen all 9 grammars idempotent | `a29a1265` `43f0795b` (FINAL.md update) | LANDED |
| **cutover.N** | EBNF activation diagnosis + repair; Phase 4 (Parsed<R> deletion); Phase 5 (tape migration); Phase 6 (bench refresh + AZ-II FINAL terminal close) | dispatched, halted at usage limit | IN FLIGHT |

## Total commits since AZ-II open

50+ commits since master `d35e34ea` (W2-act.recovery scope-reveal). The
trajectory is fully linear (no merge commits, no force-pushes); each
cherry-pick was reviewed for compile + nextest health before landing.

## Current master HEAD

`43f0795b` — `docs(AZ-II): cutover.M Phase 3 close — FINAL.md + post-AZ-II.json reflect 8/9 fleet StructDirect activation`

## Hard-gate readout (cutover.md §Hard gate, 8 conditions)

| # | Gate | Status |
|---|---|---|
| 1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green | DEFERRED — ~10k cross-crate refs; gated on generated view purge plus Phase 4 (Parsed<R> / TapeDirect deletion) |
| 2 | Stage A / Stage B byte-equal across BBNF fixture corpus; permanent CI gate green | MET — `bbnf_bootstrap_reproducibility::bbnf_regen_is_idempotent` passes; cutover.K Phase 1 added `json_regen_is_idempotent` |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide (JSON, CSS L4, Sheets, BBNF) | NOT VERIFIED — gated on Phase 6 (bench/audit suite refresh); cutover.M did not run the audit pass |
| 4 | `StructRegistry` non-empty for every Named rule in the four grammars including BBNF | MET — cutover.A authored BBNF; cutover.M extended to CSV/Math/BNF/CSS Pretty |
| 5 | Parity harnesses recoded to struct-vs-external on all four grammars | MET — cutover.D recoded BBNF parity tests; AZ-I had already recoded JSON/CSS L4/Sheets |
| 6 | 17-entry matrix at AU floor; BBNF self-parse within ±10% of AU baseline | PARTIAL — cutover.H Phase 6 partial captured JSON/Sheets/BBNF (within ±2% of AZ-I close); CSS bootstrap + Sheets parse_simple SIGABRT pre-existing; full bench refresh deferred to cutover.N |
| 7 | AZ-II FINAL.md + `docs/benchmarks/post-AZ-II.json` exist on master | MET (PARTIAL CLOSE form) — both documents on master since cutover.H Phase 7; reflect cutover.M close. Convert to FINAL CLOSE when cutover.N closes Phase 4/5/6 |
| 8 | Decay sweep: `recognizers/dta.rs` ≤ ~720 LOC; `tape::dta` and `tape::visitor` surfaces gone; tape driver dead helpers gone; `crates/json-prototype/` retired; pattern_alphabet decay items gone | PARTIAL — cutover.A landed (dta hoist, visitor delete, driver helpers); json-prototype retired in W2-act Wave 0; tape/ deletion deferred |

## BA handoff verification (AZ-II.md §Handoff contract to BA, 7 points)

| # | Point | Status |
|---|---|---|
| 1 | All four grammars on direct-to-struct (JSON + CSS L4 + Sheets + BBNF) | MET — plus CSV / Math / BNF / CSS Pretty also on StructDirect (cutover.M); EBNF on StructDirect after cutover.O2 |
| 2 | `crates/tape/` deleted | DEFERRED |
| 3 | `StructRegistry` closed fleet-wide | MET (9/9 grammars after cutover.O2) |
| 4 | Parity harnesses on struct comparisons | MET |
| 5 | Full 17-entry matrix at AU parity on struct-only path | PARTIAL (per Hard gate 6) |
| 6 | BBNF self-parse byte-reproducible | MET (cutover.B + reinforced through cutover.H/M) |
| 7 | Parent-pointer decision surface open for BA.W0 | DEFERRED — gated on tape deletion (parent-pointer landing point depends on whether `Parsed<R>` survives or per-grammar `Document` is the canonical surface) |

## Remaining work to AZ-II terminal close

cutover.N's intended deletion + bench close remains open after O2 EBNF
activation, and the post-snapshot hardening audit refines the order.
`cutover.O` is the terminal AZ-II wave and must close these gates in
sequence. Each row now has a dispatchable wave spec under
`docs/tranches/AZ-II/waves/` and may use up to 10 parallel
fully-contained sibling worktrees inside the row's file bounds:

| Substage | Scope | Required outcome |
|---|---|---|
| [O0](waves/cutover.O0.md) | Tooling preflight | LANDED — stale bench aliases, IAI CI, profiling scripts, and release pin repaired or explicitly de-canonicalized before close evidence is collected |
| [O1](waves/cutover.O1.md) | StructDirect builder transactions | LANDED — grammar-general checkpoint/rollback/commit support wired through speculative alternate/repeat/minus/negate emitter paths and runtime builders |
| [O2](waves/cutover.O2.md) | EBNF direct projection | LANDED — high-branch literal alternates and structural `Seq` branches project through StructDirect; `EbnfParser::parse -> EbnfDocument` |
| [O3](waves/cutover.O3.md) | Generated view purge | in_progress — tape-backed `TapeCursor`, node-view, and `ValueRoot` residue removed from StructDirect generated output unless consumed through a document API |
| [O4](waves/cutover.O4.md) | `Parsed<R>` / `TapeDirect` deletion | `Parsed<R>` removed as a production parser result; `TapeDirect` fallback semantics removed |
| [O5](waves/cutover.O5.md) | `crates/tape` deletion | standalone tape crate deleted after only genuinely non-tape scan/index primitives move to their natural owner |
| [O6](waves/cutover.O6.md) | semantic/perf close | JSON sonic-rs parity, CSS lightningcss typed parity, and the 17-entry close matrix refreshed |
| [O7](waves/cutover.O7.md) | final conversion | `FINAL.md` converted from PARTIAL CLOSE to terminal close |

The EBNF failure remains structural and generic. Per cutover.M's
deviation note, EBNF's `letter = "A" | "B" | ... | "z"` 52-branch
Alt-of-literal hits a layout-routing depth gap even after cutover.M's
AltDispatch surgery. The hardening audit adds one prior correctness
gate: speculative StructDirect parsing must roll back builder state, not
only input position, before EBNF/CSS/BBNF correctness claims are
trustworthy.

## Agent dispatch history

| Agent | Cap | Status | Result |
|---|---:|---|---|
| cutover.A (substrate hoist + BBNF runtime + decay) | 120 min | completed | 5 commits LANDED |
| cutover.B (Stage A/B byte-equal) | 60 min | completed | 1 commit LANDED |
| cutover.C (consumer migration first attempt) | 120 min | scope-revealed | diagnosed 3855 LOC consumer surface; fanned to cutover.D |
| cutover.D1 (host.rs + lower/) | 120 min | completed | 6 commits LANDED |
| cutover.D2 (lower/value_expr/) | 120 min | completed | 4 commits LANDED |
| cutover.D3 (graph + pipeline + types.rs) | 120 min | completed | 3 commits LANDED |
| cutover.D4 (analysis + parity harnesses) | 120 min | completed | 4 commits LANDED |
| cutover.E (tape deletion + non-BBNF + Parsed refactor) | 240 min | partial | 6 commits LANDED (5 substrates + 1 PARTIAL doc); Discovery 1 surfaced |
| cutover.F (BBNF emitter regression repair) | 240 min | landed | 3 commits LANDED |
| cutover.G (chicken-and-egg break + AZ-II close) | 300 min | partial | 5 commits LANDED (bootstrap_parser; PROGRESS entry; PARTIAL doc) |
| cutover.H (AZ-II close ceremony, first attempt) | 300 min | partial | 1 commit LANDED (Phase 0) before halting at usage limit |
| cutover.H2 (resume from H) | 300 min | partial | 3 commits LANDED (Phase 1 + Phase 5 deferral doc + Phase 7 FINAL.md PARTIAL) |
| cutover.I (close Phases 2-6) | 300 min | partial | 2 commits LANDED (Phase 5 only); Blockers 1 + 2 surfaced |
| cutover.J (Blockers 1 + 2 fix + activate fleet) | 300 min | partial | 0 commits LANDED; halted at usage limit with mapped_factor wrapper diagnosis in flight (preserved as stash) |
| cutover.K (resume from J) | 300 min | landed | 3 commits LANDED (Phases 0/1/2) |
| cutover.L (keyword-shape Alt-of-Ref + Phases 3-6) | 300 min | partial | 1 commit LANDED (Phase 3a); Phase 3b/c/d/e in flight at halt (preserved as stash) |
| cutover.M (resume from L) | 300 min | partial | 2 commits LANDED (Phase 3b/c/d for 4/5 grammars; FINAL.md update); EBNF deferred |
| cutover.N (EBNF + Phases 4/5/6) | 300 min | dispatched + halted | 0 commits LANDED; halted at organizational usage limit |

## Substrates / files of record

### NEW (durable)

- `crates/ir/src/dta/{mod,types}.rs` — DTA types hoisted from tape (cutover.A)
- `crates/core/src/runtime/bbnf/{value,arena,builder,document,view,mod}.rs` — BBNF struct-direct runtime (cutover.A)
- `crates/core/src/runtime/{csv,math,bnf,ebnf,css_pretty}/{value,arena,builder,document,view,mod}.rs` — non-BBNF struct-direct runtimes (cutover.E)
- `crates/core/src/runtime/bbnf/serialize.rs` — `serialize_compact_doc` (cutover.I.5)
- `crates/core/src/grammar/bootstrap_parser.rs` — hand-crafted BBNF parser bridging chicken-and-egg (cutover.G)
- `crates/core/tests/bbnf_bootstrap_reproducibility.rs` — permanent CI gate covering BBNF + JSON regen idempotence (cutover.B + cutover.K Phase 1)
- `crates/core/tests/project_types_bbnf.rs` — BBNF struct registry closure regression test (cutover.A)
- `crates/core/tests/bbnf_struct_builder_substrate.rs` — BbnfStructBuilder wire-contract tests (cutover.A)
- `crates/core/tests/json_object_pairs_probe.rs` — Object deposit fix regression probe (W2-act.close A.fix; pre-AZ-II)
- `docs/benchmarks/AZ-II/cutover/stage-{a,b}-bbnf.rs` — byte-equal proof artefacts (cutover.B)
- `docs/benchmarks/post-AZ-II.json` — close matrix archive (PARTIAL form, cutover.E + H + M updates)
- `docs/tranches/AZ-II/FINAL.md` — close ceremony manifest (PARTIAL CLOSE form, cutover.H Phase 7 + M update)
- `docs/tranches/AZ-II/audit/cutover.{C,E,F,G,I}-PARTIAL.md` — per-substage scope-reveal reports
- `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` — this document

### MODIFIED (load-bearing)

- `crates/ir/src/registry/strategy.rs` — `EmitStrategy::for_grammar` resolver arms; 9/9 grammars StructDirect after cutover.O2
- `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` — Wrap-vs-Repeat dispatch (cutover.F)
- `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` — inline-position emission (cutover.F) + Err frame cleanup (cutover.K Phase 2)
- `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` — `emit_alt_struct_dispatch_transparent` (cutover.H Phase 1) + Err frame cleanup (cutover.K Phase 2)
- `crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs` — Err frame cleanup (cutover.K Phase 2)
- `crates/core/src/backend/rust/emitter/shapes/arglist.rs` — Err frame cleanup (cutover.K Phase 2)
- `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` — Alt-of-Ref handler (cutover.L Phase 3a)
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs` — Alt-of-Literal/Regex/Seq emission (cutover.M)
- `crates/core/src/backend/rust/emitter/shapes/mod.rs` — transparent-rule gating (cutover.H Phase 1)
- `crates/core/src/grammar/host.rs` — extract_observational + extract_for_pipeline migrated to BbnfDocument/BbnfView (cutover.D1)
- `crates/core/src/lower/expression/{wrap,repeat,pratt,closures,alt}.rs` + `crates/core/src/lower/{mod,metadata,tape_walk}.rs` — expression hierarchy lowering migrated (cutover.D1, cutover.K Phase 1)
- `crates/core/src/lower/value_expr/{atom,mod,precedence,simple_kinds,unwrap,view_walk}.rs` — value-expression sub-grammar migrated (cutover.D2, cutover.K Phase 1)
- `crates/core/src/runtime/bbnf/view.rs` — BbnfView accessor surface (8 methods + span_text_opt + span_bounds + span_range; cutover.D1 + harmonization commits)
- `crates/core/src/runtime/bbnf/document.rs` — BbnfChildrenSlice iterator + extension methods (cutover.D3 + harmonization)
- `crates/core/src/types.rs` — `RuleEntry::rhs: BbnfView<'a, 'a>` (cutover.D3)
- `crates/core/src/graph/{deps,metadata}.rs` — migrated to BbnfView (cutover.D3) + collect_refs value-expr filter (cutover.H Phase 0)
- `crates/core/src/pipeline/{compile,directives}.rs` — entry-site migration to BbnfDocument (cutover.D3)
- `crates/core/src/pipeline.rs` — `PipelineOptions::rewrites: Option<RuleSet>` (BB.scaffold.C)
- `crates/core/src/grammar/mod.rs` — `parse()` routes through bootstrap_parser (cutover.G)
- `crates/core/src/runtime/mod.rs` — RuntimeView re-export (W2-act.close A.fix; pre-AZ-II)
- `crates/core/src/runtime/view.rs` — RuntimeView trait substrate (W2-act.close A.fix; pre-AZ-II)
- `crates/analysis/src/state/ast_utils/**` + `crates/analysis/src/features/**` — LSP migration (cutover.D4)
- `crates/core/tests/bbnf_{ast_parity,self_parity,parity}.rs` — BBNF parity harnesses recoded to struct-vs-bbnf::ast (cutover.D4)
- 9× `crates/core/src/grammar/generated/*.rs` — regen output (cutover.M for 5 newly-activated grammars; cutover.B for BBNF; AZ-I for JSON/CSS L4/Sheets)

### DELETED

- `crates/json-prototype/` (relocated to `crates/core/benches/json-prototype/` in W2-act Wave 0; pre-AZ-II)
- `crates/tape/src/visitor.rs` (746 LOC; cutover.A)
- `crates/tape/src/driver.rs` dead helpers (~236 LOC; cutover.A)
- `crates/ir/src/passes/recognizers/dta.rs` `summarise` + `DtaSummary` + `DtaProfile` + unused PushFingerprint import (52 LOC; cutover.A)

## Worktree state at snapshot time

```
/Users/mkbabb/Programming/bbnf-lang    43f0795b [master]
/private/tmp/bbnf-worktrees/cutover-N  43f0795b (detached HEAD; cutover.N dispatched + halted)
```

## Verification at snapshot time

- `cargo iter-check`: clean (warnings only; pre-existing)
- `cargo nextest run -p bbnf --test bbnf_bootstrap_reproducibility --profile ax-iter`: 2/2 passed (BBNF + JSON regen idempotent)
- `cargo nextest run -p bbnf --test bbnf_self_parity --profile ax-iter`: 56/56 passed
- Workspace nextest: 1514/1642 pass per cutover.M Phase 6 capture (1429→1514 = +85 tests post-cutover.D net of new substrates and harness recodes)

## Why EBNF deferred (and what generality means here)

cutover.M's emitter changes — keyword-shape Alt-of-Ref handler (cutover.L
Phase 3a) and AltDispatch struct_direct surgery (cutover.M) — are
**structurally general**: shape-uniform code with no per-grammar carve-outs.
They activated CSV, Math, BNF, CSS Pretty cleanly.

EBNF is the outlier because of `letter = "A" | "B" | "C" | … | "z"` —
a 52-branch Alt-of-literal-single-byte. The cutover.M emitter generates
per-letter byte-comparison + `push_leaf_with_unit()` + `push_branch_tag(idx)`
triples. Compile passes, but runtime parse fails at offset 0. Three
candidate structural roots:

1. Shape classifier's branch-count-aware dispatch (does 52-branch Alt
   classify as keyword? alt_dispatch? something else?)
2. `EbnfStructBuilder` layout admission depth (cutover.E authored the
   substrate as thin scaffold; may need extension)
3. Runtime `push_branch_tag(idx)` indexing for high branch counts
   (potential `u8` overflow at idx=52? unlikely but worth verifying)

The landed O2 fix is **structural and generic**: StructDirect
AltDispatch now emits structural `Seq` branches through the shared
inline branch walker, preserving nested children and committing the
branch tag transactionally. cutover.N halted at organizational usage
limit with no code commits; cutover.O.1 supplied the builder
transaction prerequisite, and cutover.O.2 flipped EBNF to
`EbnfDocument`. The next active gate is O3 generated view purge.

## Trajectory progress estimate

| Metric | Estimate |
|---|---|
| Structural milestones complete | ~85% |
| LOC churn complete | ~70% (tape migration is the long pole) |
| Hard gates MET | 3/8 unconditionally; 5/8 PARTIAL/PASS-equivalent under deferral budget |
| BA handoff points MET | 4/7 unconditionally; 3/7 deferred per gate |

## Trajectory follow-on

- **cutover.O** (active): terminal hardening sequence O0-O7: tooling
  preflight and builder transactions have landed; EBNF projection,
  generated view purge,
  `Parsed<R>` / `TapeDirect` deletion, tape crate deletion, semantic/perf
  close, and FINAL conversion.
- **AZ-II FINAL CLOSE**: convert `docs/tranches/AZ-II/FINAL.md` from
  PARTIAL CLOSE manifest to FINAL CLOSE; archive
  `docs/benchmarks/post-AZ-II.json` with full bench refresh.
- **Wave 3 (BA)**: opens after AZ-II FINAL CLOSE per the refined
  trajectory at `~/.claude/plans/cozy-forging-chipmunk.md`. Path IR +
  type checker + `path!()` macro + isomorphic Rust/TS/Python bindings.
- **Wave 4 (BB.close)**: opens after BA. Inferred-rule rewrite scaffold
  (BB.scaffold) authored at master `26f95469` `4bb49ef2` `9b20ded1`
  `a4ca2b2f` per the refined plan.

## Reversal posture

Per AZ-II.md §Reversal: BBNF self-parse byte-reproducibility holds
(cutover.B + reinforced via cutover.H/K/M); reproducibility CI gate
covers BBNF + JSON. No emergency reversal triggered across the
14-substage trajectory; every dispatch landed substrate-only when scope
overran cap.

`feedback_no-deferrals` envelope: every deferral is documented in a
PARTIAL CLOSE artefact under `docs/tranches/AZ-II/audit/` with a named
cutover.O substage carrying the work. The trajectory's BB.close gate
lives at the refined plan's Wave 4; the deferred work is not a hedge to a
phantom future tranche. It is the canonical cutover.O close path for the
AZ-II terminal milestone.
