# DEEPX-8 — Full BA / BB / BC Tranche Specification

**Date**: 2026-05-02
**Agent**: DEEPX-8 (codename: KNUTH-SPEC)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-deepX-8`
**Branch**: `deepX-fullspec`
**Base**: master `40e1835d` (post-Phase 1 plan-surgery)
**Mandate (verbatim)**: *"We need to have these fully formed with no ambiguity, one at a time, with maximal wave-based clarity alongside maximal tranche.md clarity and specification, alongside triumvariate dispatch for scope increase/change as we implement."*

## What Landed

### Tranche BA — Direct-Projection Codegen

- **`docs/tranches/BA/BA.md`** rewritten to AZ-IV.md depth (~280 lines):
  - 13 active contradictions / architectural defect (DEEP-A + DEEP-B + DEEP-C convergence)
  - 9 invariants (BA-scoped, extending AZ-IV's 14)
  - Carry ledger (13 AZ-IV-routed items: F2 sonic-rs, AF AU floor, F8 32 zero-caller substrates, F4 Tailwind regex_scan, F10 watchdog, F5 TS Node-execute, plus DEEP-A/B/C-derived rows)
  - 18 non-routable carries (every Audit-C MASKED-DEFERRAL bound to a wave)
  - 7-row wave table
  - Critical files and ownership map (per-surface owner-wave)
  - Orchestration rules (max 6 agents; sibling worktrees; HARD CAP; triumvirate auto-triggers)
  - **24 hard gates** (target was ≥ 22)
  - Deletion bias (forbidden patterns — explicit list)
  - Cross-tranche debt + TS/WASM punt position

- **7 wave specs at `docs/tranches/BA/waves/W{0..6}.md`**:
  - **W0 Truth, Regen, Cleanup Absorption** (5 agents): regen 9/9; substrate-audit denominator refresh; 18 zero-caller substrates DELETED or whitelisted; 3 module clusters retired; `merge_path_seed` decision; worktree fixture symlink contract codified
  - **W1 Inverse-Layout-Audit IR Pass** (5 agents): every compound-typed rule has a non-empty StructLayout; `cargo build` fails on un-inferred compound rule; named test fixtures cover Seq → Tuple, Alt → HeterogeneousAltJoin, Repeat → Vec(inner), Option(struct)
  - **W2 Direct-Projection Codegen** (6 agents — max under ceiling): per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum emitted from StructRegistry; `arena_template` + `builder_template` retired; AU floor 18/19 BELOW closes; F4 Tailwind regex_scan disposition documented
  - **W3 Speculative Checkpoint Redesign** (5 agents): `Vec<OpenFrame>::clone` not in samply top-3; `Checkpoint = (stack_depth, arena_count)` value type; predictive first-byte dispatch in JSON; ≥ 80% inclusive-samples reduction per DEEP-B
  - **W4 `parse_with` As Value-API Hot Path** (5 agents): `Document::get<T>(path)` reroutes through `parse_with`; eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`; sonic-class `<Grammar>Parser::get<T>(input, path)` API lands; `bbnf_get_twitter ≤ 5× sonic_get_twitter`; F2 MASKED-DEFERRAL closes
  - **W5 Cursor Consult + LegacyPath Retirement** (5 agents): `cursor.consult(&ParsedSegment)` unifies `match_field` / `match_index` / `decide`; `LegacyPath` / `LegacySegment` shim retires; per-grammar `__path_plan` re-exports retire
  - **W6 Measurement And Close** (3 agents): `post-BA.json` per SPEC.md; AU floor 19/19 at-or-above; zero watchdog rows; samply 7-artefact contract canonicalised as standing close discipline; FINAL.md cites resolving artefact for every Hard Gate

### Tranche BB — Egraph Rule Inference + Ruler + VM Oracle + Ranker

- **`docs/tranches/BB/BB.md`** rewritten to AZ-IV.md depth (~230 lines):
  - 5 active contradictions
  - 7 invariants
  - 8-row BA opening contract (hard gate)
  - Carry ledger (2 BA-routed items: F4 Tailwind regex_scan, `merge_path_seed` seed bag)
  - 18 non-routable carries
  - 7-row wave table
  - Critical files and ownership map
  - Orchestration rules
  - **18 hard gates**
  - Deletion bias
  - Cross-tranche debt naming BC

- **7 wave specs at `docs/tranches/BB/waves/W{0..6}.md`**:
  - **W0 Substrate Preflight** (5 agents): `crates/ir/src/rewrites/` recreated clean; schema validator; base RON rules land; cost extractor path-aware extension
  - **W1 Ruler CVC Enumerator** (5 agents): `enumerate.rs` over IrNode alphabet; e-graph residue wrapper; budget N=50 per pass; node-count ceiling 100k
  - **W2 VM Oracle On Residue** (5 agents): `oracle.rs` validates candidates; per-candidate budget 100ms wall + 256MB memory; **Tranche H rediscovery ≥ 80%**; no walker resurrection
  - **W3 Ranker + Tiering** (5 agents): `rank.rs` with 6 signals; Class-1/2/3 classifier > 90% Class 1+2; **≥ 5 accepted rules per primary grammar**; corpus hit-rate ≥ 0.1 per parse
  - **W4 Grammar-Colocated Rewrite Dirs** (5 agents): `grammar/<name>/rewrites/*.ron` schema; xtask discovery; rule admission chain end-to-end; ≥ 10 LOC shrink in one grammar
  - **W5 Review Ledger + CI** (5 agents): `rules-ci` workflow; Class-3 rationales; rejected-rule docs; throughput gain on `post-BA.json` matrix; no regression
  - **W6 Measurement And Close** (3 agents): `post-BB.json` per SPEC.md; samply 7-artefact contract; FINAL.md; close-honesty checklist; BC cross-tranche debt named

### Tranche BC — Cleanup Pass + Discipline Codification

- **`docs/tranches/BC/BC.md`** rewritten to AZ-IV.md depth (~210 lines):
  - 5 active contradictions
  - 6 invariants
  - 5-row BB opening contract (hard gate)
  - Carry ledger (9 BA + BB routed items: AUDIT-B splits, fixture contract, samply CI, Audit-A residue, csp-solver split, bbnf-regex relocation, F5 routes to BD)
  - 15 non-routable carries
  - 7-row wave table
  - Critical files and ownership map
  - Orchestration rules
  - **15 hard gates**
  - Deletion bias
  - Cross-tranche debt naming BD

- **7 wave specs at `docs/tranches/BC/waves/W{0..6}.md`**:
  - **W0 Truth + Cleanup-Substrate Inventory** (5 agents): post-BB regen 9/9 green; substrate-audit denominator refresh; AUDIT-B target file LOC verified; Audit-A residue inventoried with per-item disposition draft
  - **W1 AUDIT-B Routed Splits** (5 agents): `runtime/css_l4/builder.rs` (1014 LOC), `passes/types/mod.rs` (786 LOC), `csp_strategy/mod.rs` split into directory-modules ≤ 500 LOC each; cross-crate isomorphism per `feedback_directory_modules`
  - **W2 Worktree Fixture Symlink Contract** (5 agents): `xtask worktree-init` materialises every grammar's data + `rewrites/*.ron`; W6.2 known-miss fully closes
  - **W3 Samply 7-Artefact Contract Canonicalization** (5 agents): `.github/workflows/perf-claim.yml` gates every PR with a perf claim; environmental-gating retires; standing close discipline documented
  - **W4 Audit-A TRANSPOSE Residue Absorption** (5 agents; W0-derived per-item dispatch): each of 12 Audit-A items has named close criterion + evidence
  - **W5 Cross-Repo Discipline** (5 agents): csp-solver canonical-source split refreshed (bbnf-lang in-tree vs csc411 sibling diff-clean); bbnf-regex relocated to `parse-that/rust/bbnf-regex/`; cross-repo bench parity
  - **W6 Measurement And Close + FINAL.md** (3 agents): `post-BC.json` per SPEC.md; zero regressions vs `post-BB.json`; samply 7-artefact contract; FINAL.md; close-honesty checklist; cross-tranche debt names BD

## Triumvirate Discipline (Baked Into Every Wave)

Per `ORCHESTRATION.md` §Triumvirate Auto-Triggers, every wave names:

1. **The file bounds whose expansion would invalidate the wave.** Explicit per-wave list of which scope-reveal conditions trigger triumvirate (e.g., BA.W2's "the emitter cannot produce a typed struct/enum for one of the 9 grammars").
2. **The hard-gate failures that would not be local-edit-recoverable.** Explicit perf-miss thresholds (e.g., BA.W3's "after the regen, `Vec<OpenFrame>::clone` is still in samply top-3").
3. **The diagnostic-loop iteration cap.** Three iterations is the hard cap per ORCHESTRATION.md.

Auto-triggers (no user prompt required):
- JSONL transcript quiet > 15 minutes
- First-pass return with no commit and no evidence (after one verbatim redispatch)
- Three diagnostic-loop iterations without isolating root cause
- Scope reveal that invalidates file bounds / hard gates / substrate-with-consumer wiring

Mandatory artefact paths per ORCHESTRATION.md §Triumvirate:
- `docs/tranches/{LETTER}/audit/{COHORT}-research.md`
- `docs/tranches/{LETTER}/audit/{COHORT}-plan.md` (with `## Exact Wave-Amendment Text` section)
- `docs/tranches/{LETTER}/audit/{COHORT}-redress.md`

Tranche-specific triumvirate triggers per the user mandate:
- **BA**: scope-reveal (W2 misses a grammar shape; W3 finds non-disjoint alphabet); perf miss (W4 doesn't close ≤ 5× sonic); test regression (W5 breaks 19/19 AU floor)
- **BB**: rule contradiction (a candidate L=R fails oracle); enumeration explosion (CVC budget exceeds N=50); ranker misclassification
- **BC**: residue surfaces a NEW deferral (cleanup that itself has chronic carries)

## Disjointness, Worktree Hygiene, Lint Cadence

Every wave names sibling-worktree absolute paths and per-agent `CARGO_TARGET_DIR`. The orchestrator runs `git worktree list` and `git worktree add` before dispatch (per ORCHESTRATION.md §Integration). At most one cargo invocation in flight per `CARGO_TARGET_DIR`.

Lint cadence per WAVE_SPEC.md §7: broad implementation waves run `cargo fmt --all -- --check`, focused `cargo clippy --profile ax-iter`, focused nextest, and `git diff --check` after each integration batch and before close. Docs-only waves run `git diff --check` plus available document checks.

## Hard Gate Counts

| Tranche | Hard Gate Count | Target |
|---|---|---|
| BA | 24 | ≥ 22 |
| BB | 18 | ≥ 18 |
| BC | 15 | ≥ 15 |

## Wave Count Per Letter

Each tranche has exactly 7 waves (W0..W6) per the user mandate. Total: 21 wave specs.

## Commit Cadence

- BA: `81676bb5 docs(tranches/BA): full direct-projection spec — 7 waves, 24 hard gates`
- BB: `d465e7d4 docs(tranches/BB): full rule-discovery spec — 7 waves, 18 hard gates`
- BC: this commit (forthcoming)

## What's Not In Scope For This Dispatch

- No source code modified (DEEPX-8 is read-only for source per the dispatch packet).
- No precepts submodule bump (the tranche specs reference the canonical SPEC.md / WAVE_SPEC.md / ORCHESTRATION.md as they stand at master `40e1835d`).
- No FINAL.md or PROGRESS.md for any of the three letters (they will be authored when the tranche actually opens; this dispatch produces the *plan*, not the *execution log*).
- No archive of the previous BA / BB / BC content (the historical/ subdirectories were already in place at master `40e1835d` per the Phase 1 plan-surgery commit).

## Status

All three letters land COMPLETE per the dispatch packet's acceptance criterion: top-level `<LETTER>.md` matching AZ-IV.md depth + 7 wave specs matching WAVE_SPEC.md. No partials. The user mandate "fully formed with no ambiguity" is met.
