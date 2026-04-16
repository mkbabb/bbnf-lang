# Retrospective Synthesis — AK through AV

Twelve parallel retrospective agents analysed the twelve tranches
preceding AW. This document agglomerates their findings into a
chronic-deferral ledger, a recurring-anti-patterns catalog, a
what-worked template, and a corrective map to
`docs/instructions/TRANCHE_SPEC.md`.

Sources: `docs/tranches/AW/audit/{AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV}-retro.md`.

## Chronic deferrals ledger

| Item | Origin | Chain | Status |
|------|--------|-------|--------|
| Cost-model grid sweep | AM.6 | AM → AO.4.1 → AP.6.4 → AQ.9.4 → AW ledger | AX scope |
| Global CSP solve | AL prototype | AL → AO.4.2 → AP.6.5 → AQ.9.5 → AW ledger | AX scope |
| Scanner-architecture cluster | AR.6.x / AS.5.x | AR → AS → AT → AU → AV → AW ledger | AX scope |
| CSS L4 tailwind offset 387594 | AN.3 | AN → AO → AP → AQ → AR | healed at AR |
| StructRegistry | AS.2.3 | AS → AT → deleted AU.4.2 | path re-opened as backend-type-tables; Color view in AW-I.W0.5 |
| Bug 2b residuals | AU/AV | AU → AV → AW.2.5 → AW-II.W1.3 | AW-II.W1.3 |
| Fuse/inline activation | AU PROGRESS (`scc_id.is_none()` guard no-op) | AU → AV → AW.0.10 reverted → AW-I.W2.3 + W4.5 | AW-I.W4.5 |
| Structural-dispatch infrastructure | AM.5 / AO / AP | AM → AO → AP → AQ.5 deleted | deleted; AW-II.W2.3 selector classifier replaces |
| Visitor 6× SIMD-packed gate | AV.2.5 | AV.2.5 partial (3.3× scalar) → AW.6.3 → AW-II.W5.1 | AW-II.W5.1 |

## Recurring anti-patterns

### 1. Substrate-without-activation — the #1 chronic pattern

Emission lands; consumer doesn't; hard gate closes on "code exists";
runtime never fires.

Evidence:
- **AK** EmissionTier scaffolding left standing after `__branch_idx`
  obsoleted it; AM demolished ~2000 LOC.
- **AM** AM.2 payload buffer + AM.5 structural bitmap landed in
  parse-that with no codegen consumers.
- **AO** full structural-dispatch chain (IR pass + cursor + quote-
  parity + pre-scan codegen + WS elision) landed; derive path never
  called it. AP's recap marks AO's parse impact **0%**.
- **AP.1** flagship structural-dispatch committed with `structural_
  mode = false`. AQ.5 deleted ~400 LOC of the infrastructure.
- **AQ** Phase-6 typed-payload system (PayloadKind deletion,
  TypeDesc expansion, layout planner, Alt typed enum view) shipped
  end-to-end source; zero payload writes in six production grammars.
- **AS.2.3** StructRegistry met "scaffold exists" gate; never
  populated; AT catalogued as dead infrastructure.
- **AT.1** `resolve_branch_type` emitted `push_leaf_with_{f64,bool,u8}`
  correctly; `driver/alt.rs::branch_pushes_children` mis-classified
  inlined-Ref leaf branches as compound — every typed capture a
  dead store. AU.1.1 (`83357e4`) is the real activation patch.
- **AV** AV.2.5 reordered-unrolling kernels landed; consumer API
  (`Tape::reduce_column`) never written.

Corrective (TRANCHE_SPEC §"Activation-gate rule"): substrate
addition requires same-wave consumer that calls it plus a hard gate
verifying the call fires at runtime (bench delta, `cargo expand`
citation, samply attribution, tape-walk test). Source-grep gates
are supplementary.

### 2. Silent deferrals

Plan-time promises vanish without escape clause.

Evidence:
- **AN** five items (AN.0.5, AN.2, AN.3, AN.5, AN.6) → AO with no
  rationale. Started a four-tranche chronic on CSS L4 tailwind
  offset 387594.
- **AO** four of six planned phases disappeared, only re-surfacing
  as retroactive "STATUS: OPEN" block.
- **AP** ~10 sub-phases (AP.1 proper, AP.3.2, 3.4, 4.2, 4.3, 4.4,
  5.4, 5.5, 6.4, 6.5) silently deferred despite plan's self-promise
  "No deferrals."
- **AS** 4 sub-items silently dropped under "PARTIAL" phase label.
- **AV** V6-V9 scope-cut at V5 close "per user direction" —
  tranche-boundary cut, not plan-time deferral; three within-wave
  items also slipped (AV.3.6 fn-per-rule delete, AV.0.5 Color
  admission inert through V5, AV.2 post-order contradicted plan's
  "pre-order from idx+1").

Corrective (TRANCHE_SPEC §"Closing ceremony" item 5): every plan-
enumerated item not landed in FINAL appears in FINAL's deferred
ledger with named destination tranche + rationale. Silent drop is
a violation.

### 3. Plan-only / ceremonial tranche docs

PROGRESS.md / FINAL.md missing; plan edited post-hoc to match
what landed.

Evidence:
- **AK** plan authored at `c62ad389` AFTER implementation commits
  (AK.0 at 05:04, AK.1 at 05:12, AK.md at 05:15). Post-hoc
  close-out as plan.
- **AL** no plan doc; four competing research prototypes
  unreconciled.
- **AM** plan-only; no PROGRESS / FINAL / research. AM.md edited
  retroactively to annotate AM.4/.5 and silently drop AM.6. AM.0+
  (CSP soft-index, 269× compile speedup) was undeclared scope
  folded post-hoc.
- **AO**, **AP** no PROGRESS, no FINAL, no bench.
- **AQ** ceremonial 4-wave × 6-agent wave structure; commit trail
  is single linear master history with no worktree cherry-picks.
- **AS** 5-phase 9-gate ~11-day plan executed solo in ~1.5h; no
  worktrees, no research/, no FINAL.

Corrective (TRANCHE_SPEC §"Document set"): PROGRESS.md + FINAL.md
are required. Tranche directory structure is non-optional.

### 4. Label / document discipline violations

New work lands under old letters; plans edit in-place; `{LETTER}.md`
doesn't exist before `{LETTER}.N` commits.

Evidence:
- **AL** label collision — AN plan (`acaa1898`) + three AN.0 fix
  commits landed BEFORE AL.1 committed; AN Phase-0 doc-fold
  (`17728fd7`) retroactively absorbs AL.1 as "AN Phase 1.1".
- **AR** audit-driven replan (correct methodology) kept under AR/
  instead of promoted to a new letter per `new-tranche-new-doc`.
- **AS** itself was mid-stream re-plan of AR-audit leftovers — no
  new letter opened.

Corrective (TRANCHE_SPEC §"Document set" + §"Scope-reveal protocol"):
`{LETTER}.md` is a hard gate before any `{LETTER}.N` commit. Mid-
tranche scope pivots open a new letter.

### 5. Gate-off commits

Feature shipped behind `false` default flag with no same-tranche
consumer flipping it.

Evidence:
- **AP.1** structural-dispatch committed with `structural_mode =
  false` in `generate/mod.rs:61`. Three specific bugs surfaced
  only in AQ's post-hoc audit.

Corrective (TRANCHE_SPEC §"Gate-off commits"): no commit lands
with its activation gate disabled unless plan-declared with a
named restoration wave.

### 6. Hard-gate-via-grep

Gate closes on source pattern match while runtime never exercises
the code.

Evidence:
- **AT.1** Phase-1 passed its grep gate; `branch_pushes_children`
  in `driver/alt.rs` (file absent from AT's critical-files table)
  still mis-classified branches. Every typed payload capture was a
  dead store until AU.1.1.

Corrective (TRANCHE_SPEC §"Hard gates"): gate closes on runtime
evidence; critical-files table must be audited against data flow,
not phase narrative.

### 7. Ceremonial wave structure

Plan declares multi-wave × multi-agent; execution is linear solo
master history.

Evidence:
- **AQ** 4-wave × 6-agent plan; linear master commit trail; no
  worktree cherry-picks.
- **AS** 5-phase solo ~1.5h execution despite ~11-day plan.

Corrective (TRANCHE_SPEC §"Wave stipulation"): declared wave
structure executes, or plan amends before execution.

### 8. Commit-before-parallelise violation

Master not clean before spawning parallel agents; duplicate /
conflicting commits surface.

Evidence:
- **AR** duplicate `b0e4534` / `6c889d5` (AR.7.1); duplicate
  `677a801` / `6074a4b` (AR.3.1).

Corrective (TRANCHE_SPEC §"Commit discipline"): master clean before
every wave dispatch; sub-agents commit in worktrees only.

### 9. Bench omission

No intra-tranche bench; regressions invisible until close.

Evidence:
- **AV** V10 was the first bench; 2.5-4.5× regression across every
  entry invisible until tranche close.
- **AN** functional gate passed while shipping -39% canada /
  -20% data_xl throughput.
- **AR** AR.1.1 functional gate passed while shipping canada
  -39% / data_xl -20% regression.

Corrective (TRANCHE_SPEC §"Bench contract"): per-wave or
aggregated; silent omission is a violation.

### 10. Research edict skipped

RESEARCH.md six-agent fan-out aspirational; tranches inherit
predecessor's.

Evidence:
- **AV** `research/` holds six "April 2026"-dated deliverables
  whose headers identify them as AU-era research.

Corrective (TRANCHE_SPEC §"Before authoring the plan"): research
wave runs per-tranche or plan explicitly waives with rationale.

## What worked — AU as template

AU is the bright spot. What AU did right that TRANCHE_SPEC
codifies:

1. **Re-plan on scope reveal, not defer.** AU Session 2 rewrote
   Phase 2 from CSS-scanner-activation to CSS-typed-AST-parity-
   with-lightningcss mid-tranche without breaking wave cadence.
2. **Samply trio.** Shared `CARGO_TARGET_DIR` + `wave.tsv`
   contract + `profile-bench-headless.sh` unlocked a 27-entry
   profiling fan-out every downstream doc cites.
3. **Pre-wave friction fixes.** Three setup fixes (entry
   enumeration, ripgrep→grep, bencher substring filter) landed
   before the wave so sub-agents inherited working setup.
4. **Dated PROGRESS at wave boundaries.** AU's PROGRESS is the
   ground-truth execution log subsequent tranches cite.
5. **Profiling docs co-located.** `AU/profiling-1.md` +
   `profiling-2.md` carry the wave's samply evidence as structural
   artefacts, not hidden state.

## Direct corrective → TRANCHE_SPEC mapping

| Anti-pattern | TRANCHE_SPEC section |
|--------------|----------------------|
| Substrate-without-activation | §"Hard gates" — Activation-gate rule |
| Silent deferrals | §"Closing ceremony" + §"Document set" — FINAL deferred ledger |
| Plan-only / ceremonial docs | §"Document set" — PROGRESS + FINAL required |
| Label / document violations | §"Document set" + §"Scope-reveal protocol" — new letter rule |
| Gate-off commits | §"Hard gates" — Gate-off commit rule |
| Hard-gate-via-grep | §"Hard gates" — runtime evidence requirement |
| Ceremonial wave structure | §"Wave stipulation" — executed-as-declared |
| Commit-before-parallelise | §"Commit discipline" — master-clean rule |
| Bench omission | §"Bench contract" — per-wave or aggregated |
| Research edict skipped | §"Before authoring the plan" — research wave required |

## AW disposition check

Against the ten anti-patterns above, AW-I + AW-II currently
stand:

- Substrate-without-activation: **cleared.** AW-I.W3 swaps
  `parse()` to dispatch through `dta_run`; AW-I.W4 deletes the
  legacy. No substrate lands without same-wave consumer.
- Silent deferrals: **cleared.** AW-I.md / AW-II.md enumerate
  routed items with destinations; `parse_dta` additive-shadow
  retires in AW-I.W3.
- Plan-only docs: **cleared.** PROGRESS.md rolling; FINAL-I.md +
  FINAL.md scheduled at W5 / W6 closes respectively.
- Label discipline: **cleared.** AW-I / AW-II split authored
  with AW.md preserved as historical; no label collision.
- Gate-off commits: **watchable.** AW-I.W0 `has_inline_frame_
  depth: bool = false` is correct-default (legacy path), flipped
  by AW-I.W3 parse swap — same tranche, named restoration wave.
  Not a gate-off violation.
- Hard-gate-via-grep: **partial.** AW-I.W4.5 "CSS L4 DTA state
  count < 2000" closes on a count measurement but the count is
  proxied via `grep -c 'DtaState::'` — needs direct access to
  `DtaBuilder::summarise` for honest verification.
- Ceremonial wave structure: **cleared.** W0 / W1-substrate
  already executed with worktree cherry-picks; W2-W5 to follow
  suit.
- Commit-before-parallelise: **cleared** for W0; to maintain
  through W2-W4.
- Bench omission: **declared.** AW-I one bench at W5 close;
  AW-II per-wave through W5 aggregating at W6. Plan-declared.
- Research edict: **cleared.** AW ran its own 5-agent research
  wave (commits `6917125`–`8846ee2`).

One residual (hard-gate-via-grep on DTA state count) —
AW-I.md refinement: replace the grep proxy with a call to
`bbnf_ir::passes::recognizers::dta::summarise` or a dedicated
test that asserts the state-count bound. Folded into AW-I.W4.5
hard-gate phrasing.
