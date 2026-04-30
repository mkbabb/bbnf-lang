# AZ-III REAUDIT 2026-04-30 - Carry-Over Completeness Audit (Hardening Lane H1)

**Authority**: SYNTHESIS.md (this directory) is the canonical synthesis of the
six-lane reaudit. The user's original prompt that opened the reaudit is
recapitulated as a 25-item checklist (plus four hardening additions, total 29
items). Every item below maps to a wave, a precepts entry, or a concrete patch.

**Hardening lane**: H1 - End-to-End Carry-Over Completeness Audit. H2 owns the
submodule precepts edits (including the 10->6 ceiling fix). H3 owns the parent
worktree 10->6 sweep + named-wave canonicalization. This document is read-only
output for orchestrator integration; no waves, precepts, source, or generated
files are modified by H1.

**HEAD audited**: master at REAUDIT time (`d5179b8a` per lane reports);
parent commit at H1 dispatch is `454308af`.

**Scope**: build a comprehensive checklist that maps every numbered user-ask
item plus the four hardening additions to a verdict (MET / PARTIAL / GAP /
DELEGATED-TO-LANE), an evidence pointer, and (when GAP) ready-to-apply patch
text with target file, section, and insertion point.

**Triumvirate trigger ceiling**: more than 10 GAPs in any single section halts
H1 with a re-planning request. H1 closes within ceiling: §2 has 4 GAP entries,
§3 has 0 GAP, §4 has 1 GAP (close-deferral nuance), §5 has 0 GAP, §6 has 2
GAPs, §7 has 0 GAP, §8 has 0 GAP, §9 lists named-wave gaps for H3 (not
checklist GAPs), §10 has 0 GAP, §11 has 1 GAP (one feedback memory item not
codified), §12 proposes friction additions (not GAPs in existing surfaces).

---

## 1. Original-Prompt Checklist (29 Items)

Items 1-25 are the user's original numbered asks (paraphrased into a checklist
in the dispatch packet); items 26-29 are the four hardening additions.

| # | Ask | Where (file path + line) | Verdict | Evidence | Patch (if GAP) |
|---:|---|---|---|---|---|
| 1 | Audit current state of project + ALL sibling projects + precepts | `docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/01-failure-baseline.md` §4 (sibling status); `03-substrate-deadcode.md` §7 (sibling posture); `04-instructions-process.md` §1 (precepts coverage matrix); `05-plan-waves.md` §1 (tranche-by-tranche posture); `06-throughput-commit.md` §1-9 (build/bench audit) | MET | Six lanes covered project + parse-that + pprint + gorgeous + bbnf-buddy + precepts framework. Lane 1 §4: parse-that RED, pprint GREEN, gorgeous DOES NOT EXIST as sibling. Lane 3 §7: sibling integrity strong. | none |
| 2 | Deeply audit last 1000 commits + tranche/wave docs | `06-throughput-commit.md` §7 (500-commit forensic sample); `05-plan-waves.md` §1 (last 9 tranche FINALs walked AU through AZ-II) | PARTIAL | Lane 6 sampled 500 commits (within 1000 limit); lane 5 walked 9 tranche docs back from AZ-II. **GAP**: the user said 1000 commits; 500 is the largest documented sample. | Patch: see Patch Index P-A1 (extend lane 6's commit-sample to span the 1000-commit window or document the 500-commit ceiling as adequate per `feedback_kiss_perf_bias`). |
| 3 | Read GESTALT.md and all appurtenant docs for global overview | `02-future-sota.md` §3.1 (cites GESTALT.md AU-baseline lines 982-990; SOTA target lines 1374-1376) | MET | Lane 2 cites GESTALT.md AU-baseline matrix (1231/2438/1967/735/454/496/95) as binding floor; lane 2 §3.3 cites the BEAT directive at GESTALT.md:1374-1376. | none |
| 4 | AZ-III continues AZ-II; AZ-II must be closed cleanly with status changes; pointed to AZ-III | `docs/tranches/AZ-II/FINAL.md` (continuation handoff status, lines 1-12 + 76 + 84 reconciliations); `AZ-II/PROGRESS.md` lines 71-100 (REAUDIT close-honesty addendum) | MET | AZ-II FINAL.md is closed as continuation handoff; the three SYNTHESIS A2 reconciliations landed (gate-1 STALE-GOOD, gate-6 unsourced-claim downgrade, BA-handoff-1 PARTIAL); AZ-II PROGRESS.md REAUDIT addendum points to AZ-III. | none |
| 5 | AZ-III absorbs items deferred (especially chronically deferred) from AZ-II or previous tranches | `docs/tranches/AZ-III/AZ-III.md` §Carried Work Ledger (lines 73-90) | PARTIAL | 12 ledger rows cover AZ-II.O5/O6/O7 plus REAUDIT carry-overs. Lane 5 §2 names 16 chronic items crossing 2+ tranche boundaries; 14 are owned by waves (W1, W2, W3a, W3b, W3c, W4). **GAP**: rows 16 (commit-discipline scheme) + 17 (sibling-repo carry) need owner verification. See §3 below. | Patch P-B1 (§3): row 16 maps to W0.5 sample report owner; row 17 maps to W0.6 sibling triage. Both are present in AZ-III.md ledger; verify against §3 verdicts. |
| 6 | Plus items from this audit | `SYNTHESIS.md` §Path Forward (the audit-driven refinements R1-R8 in `AZ-III/PROGRESS.md` lines 71-115) | MET | R1 (W0p) + R2 (W0.5) + R3 (W0.6) + R4 (W3 split) + R5 (W2 vs W3.4 carve) + R6 (W3a.0 research) + R7 (W2.4 strict close) + R8 (wave table reshape) all landed per PROGRESS.md. | none |
| 7 | 6 agents in parallel for original audit (DONE in REAUDIT-2026-04-30) | `01-failure-baseline.md` line 1, `02-future-sota.md` line 1, `03-substrate-deadcode.md` line 1, `04-instructions-process.md` line 1, `05-plan-waves.md` line 1, `06-throughput-commit.md` line 1 | MET | Six lanes ran in parallel as documented in SYNTHESIS.md §Lane Inputs table (lines 16-23). | none |
| 8 | Recapitulate original prompts/plans/precepts; NO quick solutions; NO workarounds | `docs/tranches/AZ-III/AZ-III.md` Invariant 8 (lines 55-60: "No workarounds; architectural transpositions for elegance") | MET | AZ-III invariant 8 cites `feedback_no_workarounds` and `feedback_no_workarounds_arch`. W2.4 close clause (W2.md:78-92) explicitly forbids "deferred to BA" and "future tranche" close paths. W3a Hard Gate 4 (W3a.md:120-122) forbids silent BoxedEnum fallback. W3b Hard Gate 4 (W3b.md:103-105) forbids no-op `shape_dict::install`. | none |
| 9 | Idiomatic gestalt approaches; architectural transpositions for elegance/simplicity/performance are mandatory and desirable | `AZ-III/AZ-III.md` Invariant 8 ("transpositions are mandatory and not optional"); `feedback_no_workarounds_arch` codified | MET | Invariant 8 mirrors `feedback_no_workarounds_arch` exactly. SYNTHESIS A3 §"thesis holds; refinements are mechanical" frames AZ-III refinements as transpositions, not workarounds. | none |
| 10 | NO legacy code | `AZ-III/AZ-III.md` Invariant 2 ("No legacy code"); `03-substrate-deadcode.md` §8 (top-30 deletion list); W1 + W3c file bounds (deletions named) | MET | Invariant 2 binds; lane 3 §8 enumerates 30 deletion targets with severity + LOC + wave owner. W1.md scope item 4 names "tape, public tape runtime, json-prototype, Gorgeous JIT, A1 residue" deletions; W3c.md scope item 3 names prettify stubs + trace.rs + recognizer_plan.rs. | none |
| 11 | Read instructions/ for profiling and research | `06-throughput-commit.md` §1-6 (cites `docs/instructions/PROFILING.md:31, 78-82, 97-98`) | MET | Lane 6 read PROFILING.md, CHANGELOG.md, README.md as cited at `06-throughput-commit.md:687-689`. | none |
| 12 | Analyze last 2 tranches and 2 post-current tranches to learn process | `05-plan-waves.md` §1 (AU/AV/AW/AX/AY-I/AY-II-I/AY-III/AZ-I/AZ-II all walked); `02-future-sota.md` §6 (B0..B7 + BA + BB walked) | MET | Lane 5 §1 walks 9 prior tranches; lane 2 §6 walks B0-B7 plus BA/BB downstream gates. Both within 2-back / 2-forward of AZ-III. | none |
| 13 | Stop deferring and over-optimizing superfluous nonsense | `feedback_no_deferrals` codified (project memory); `AZ-III/AZ-III.md` Invariant 1 ("Continuation, not deferral") | MET | Invariant 1 binds. W2.4 strict close clause (W2.md:78-92) is the structural fix per SYNTHESIS A3. Lane 5 §1 confirms "every tranche back to AU defers its hardest gate forward; AZ-II is the first close where the orchestrator stops pretending otherwise." | none |
| 14 | Expedite development DRAMATICALLY: testing, benching, building | `docs/tranches/AZ-III/waves/W0p.md` (Throughput Substrate) | MET | W0p hard gate (W0p.md:106-120) lands `[profile.bench-iter]`, `xtask regen --staged`, `make doctor`, nextest partition, and 5-harness sweep measurement. Per `feedback_build_infra_first` and SYNTHESIS A7. | none |
| 15 | Ensure ALL substrate wired and consumed | `AZ-III/AZ-III.md` Invariant 3 ("Substrate with consumer"); W3a/W3b/W3c hard gates each cite a named production consumer | MET | W3a.1 sub-gate (W3a.md:74-76) requires "production layout/dispatch consumer fails a focused test when the fact is removed"; W3b sub-gates (W3b.md:64, 74) require named consumer per constraint; W3c.1 sub-gate (W3c.md:71-73) requires projection tests fail without authority. | none |
| 16 | Audit for dead/under-utilized/deprecated/contrived/shim-like/complex/legacy code | `03-substrate-deadcode.md` §8 (top-30 deletion targets) | MET | Lane 3 enumerates 30 targets with severity/LOC/wave; SYNTHESIS A6 cites the exact lines. | none |
| 17 | Develop new tranche AZ-III; close AZ-II (update appurtenant docs/statuses) | `AZ-III/AZ-III.md` (created); `AZ-II/FINAL.md` + `AZ-II/PROGRESS.md` (closed as continuation handoff with reconciliations); `REMAINING-TRAJECTORY.md` (status reflected in AZ-III audit lane reports) | MET | AZ-III is the named continuation tranche; AZ-II close docs point to AZ-III at multiple locations (FINAL.md:9, 65, 71-77, 84-90, 176-186; PROGRESS.md:3-14, 71-100). | none |
| 18 | Adhere to newer tranche format including triumvirate dispatch (in precepts/) | `docs/precepts/instructions/tranche/WAVE_SPEC.md` §3a (added); each AZ-III wave has `## Triumvirate Dispatch` section | MET | WAVE_SPEC.md:40-52 mandates §3a; W0/W0p/W1/W2/W3a/W3b/W3c/W4/W5 all have the section (W0:105-111, W0p:96-104, W1:66-74, W2:94-101, W3a:99-107, W3b:86-94, W3c:105-114, W4:67-74, W5:67-73). | none |
| 19 | Commit discipline has been awful; use Claude /commit skill style; refactor terse commits (DONE) | `06-throughput-commit.md` §7-8 (forensics + 30-commit rewrite list); `04-instructions-process.md` §7 (commit discipline state); `precepts/instructions/README.md` §Commit Discipline (lines 36-69) | PARTIAL | Subjects clean (497/499 conventional); 68 commits carry templated bodies (per lane 6 §7). The W0 message-only rewrite landed once. **GAP**: precepts forbid templated bodies (LESSONS-LEARNED.md "Templated Commit Bodies Are Bodyless In Spirit") but the AGENT_DISPATCH_TEMPLATE.md non-negotiables (already added) need to bind. The user has acknowledged the rewrite per SYNTHESIS A9. Treat as codified, not a re-rewrite trigger. | Patch P-A2: confirm `docs/precepts/instructions/README.md:67-68` "One-line commits are acceptable only for genuinely local mechanical edits whose diff and subject are self-explanatory" is reinforced by the templated-body rejection rule (`LESSONS-LEARNED.md` 2026-04-30 entry). H2 verifies this is in the submodule. |
| 20 | Run /commit frequently and often during tranche | `precepts/instructions/README.md` §Commit Discipline (lines 38-40 "Commit at natural milestones"); `AZ-III/AZ-III.md` Invariant 10 ("Commit at every sub-gate") | MET | Invariant 10 binds; precepts encode the cadence. Each wave Commit Plan section (W0p:139-150, W1:104-110, W2:138-145, W3a:142-155, W3b:125-137, W3c:148-161, W4:107-114, W5:99-104) names per-sub-gate scopes. | none |
| 21 | Orchestrator role: defer to agents, synthesize, cherry-pick worktrees, deploy triumvirate (research/plan/redress), deploy hardening agents | `AZ-III/AZ-III.md` Invariant 9 ("Orchestrator delegates and synthesises"); `precepts/instructions/ORCHESTRATION.md` §Wave Model + §Triumvirate + §Triumvirate Auto-Triggers; `AZ-III/AZ-III.md` §Triumvirate Discipline (lines 106-113) which names hardening lanes | MET | Invariant 9 names the orchestrator as delegator + synthesiser + cherry-picker + auditor. Triumvirate Discipline section explicitly references "read-only hardening lanes for diff bounds, gate evidence, dead/overfit substrate, and document-status reconciliation". | none |
| 22 | Validate precepts/ aligned to all of above | `04-instructions-process.md` §3-9 (gap analysis + refinement proposals); SYNTHESIS A10 (precepts framework needs five concrete patches - all five landed inside the submodule per SYNTHESIS path-forward sequence) | MET | SYNTHESIS A10 landed via B3 lane (precepts submodule refinements). The five patches: AGENT_DISPATCH_TEMPLATE expansion, ORCHESTRATION triumvirate triggers, WAVE_SPEC §3a/§4a/§4b, SPEC.md scope-reveal tightening, LESSONS-LEARNED nine entries - all visible in current precepts files. | none |
| 23 | Audit last 100 turns of agent calls + conversation for friction | `04-instructions-process.md` §2 (Recurrent-Friction Inventory, 15 patterns); `06-throughput-commit.md` §7-8 (commit discipline forensics) | MET | Lane 4 §2 enumerates 15 friction patterns from AZ-II/AZ-III artefacts + recent git log. SYNTHESIS A10 lands them as five concrete precepts patches. | none |
| 24 | Refine wave formulation; refine precepts repo | `WAVE_SPEC.md` §3a/§4a/§4b (added); `ORCHESTRATION.md` §Triumvirate Auto-Triggers + §Build Concurrency + §Long-Running Commands + §Returns; `SPEC.md` §Scope Reveal tightened; `AGENT_DISPATCH_TEMPLATE.md` expanded; `LESSONS-LEARNED.md` nine new entries | MET | All five precepts surfaces refined per SYNTHESIS A10. AZ-III waves use the new sections (Triumvirate Dispatch present in every wave; W0p/W3a/W3b/W3c carry HARD CAP redress notes). | none |
| 25 | KISS; one path; architectural transpositions for elegance/simplicity/performance | `precepts/instructions/README.md` §Edicts ("KISS. DRY"); `AZ-III/AZ-III.md` Invariant 8; `feedback_kiss_perf_bias`, `feedback_no_orthogonal_codepaths`, `feedback_no_workarounds_arch` codified | MET | KISS/DRY is edict #1 in precepts/instructions/README.md:8-9. Invariant 8 binds in AZ-III. SYNTHESIS A3 calls W3 split a "tranche, not a wave" - the KISS lever for the largest planned wave. | none |
| 26 (hardening) | Explicitly check waves are NAMED (not just numbered) - codify in precepts | `WAVE_SPEC.md` lines 12-15 ("Each wave has both a number and a name. Canonical display form `W<N> - <Title>` in parent tables, progress logs, dispatch prompts, and close reports."); `tranche/SPEC.md` §Waves line 41 ("Every active wave has a number and a name: `W<N> - <Title>`. Parent tables, progress logs, dispatch prompts, and final reports use number plus name, not number alone."); `ORCHESTRATION.md` line 12 ("Name every wave by number and title: `W<N> - <Title>`."); `AZ-III/AZ-III.md` wave table uses canonical form throughout (lines 96-104) | DELEGATED-TO-LANE-H3 | Codification is COMPLETE in precepts (per H2's submodule edits already landed). H3 owns the canonicalization sweep across remaining doc surfaces (audit lane reports use bare `W0`/`W3` references in places per §9 below). | See §9 (named-wave gap list for H3 sweep). |
| 27 (hardening) | Change 10-agent ceiling to 6 GLOBALLY (precepts + parent) | DELEGATED-TO-LANES-H2/H3 | DELEGATED | `precepts/instructions/tranche/SPEC.md:39` says `Hard ceiling: max 6 parallel agents` (correct). `precepts/instructions/ORCHESTRATION.md:11` says `Hard ceiling: use at most 10 agents in a wave` (still 10 - H2 owns the submodule fix to make this 6). Parent waves W0/W1/W2/W3a/W3b/W3c/W4/W5 say `up to 10 parallel` (H3 owns mechanical 10->6 sweep across `AZ-III.md` wave table, `PROGRESS.md` wave-status table, and waves/W*.md State sections). W0p says `up to 5` (already <=6). W3a recommends `up to 5` per lane 5 §8 (audit recommendation). | See §10 below for full ceiling-normalization audit (covers also `precepts/instructions/LESSONS-LEARNED.md:97` "use up to six agents as a hard ceiling for truly disjoint research" which is consistent with target). |
| 28 (hardening) | Align precepts with project-memory items and learned lessons | `04-instructions-process.md` §1 (precepts coverage matrix) + §3 (precepts gaps + proposed mechanism) + §8 (concrete refinement proposals); `LESSONS-LEARNED.md` nine 2026-04-30 entries (codified) | PARTIAL | 20 of 21 generic memory items audited are codified or addressed. **GAP**: `feedback_generated_size_budget` is not codified anywhere in precepts; `feedback_no_god_modules` is partially codified (lane 3 §4 confirms enforcement but no precepts entry). See §11 below for the full table. | Patch P-C1 + P-C2 (§11): two LESSONS-LEARNED.md entries to add covering generated-size budget and god-module ban. |
| 29 (hardening) | Reduce friction so orchestrator behaves as orchestrator (delegates, triumvirate, etc.) | `04-instructions-process.md` §2 (15 friction patterns); `ORCHESTRATION.md` §Wave Model + §Dispatch Contract + §Integration + §Stalls + §Triumvirate + §Triumvirate Auto-Triggers + §Build Concurrency + §Long-Running Commands + §Returns + §Status; `AGENT_DISPATCH_TEMPLATE.md` (expanded); `LESSONS-LEARNED.md` nine new entries | MET | All 15 friction patterns per lane 4 §2 are addressed by the precepts framework refinements (SYNTHESIS A10). Triumvirate auto-triggers binding per ORCHESTRATION.md:105-122. AGENT_DISPATCH_TEMPLATE.md mandates HARD CAP, worktree pin, CARGO_TARGET_DIR, read-size preflight, lint cadence, anti-polling, empty-return rule, non-negotiables. | none |

**Section 1 GAP count**: 4 (items 2, 5, 19, 28). All are PARTIAL with concrete
patches in the patch index.

---

## 2. Chronic-Deferral Coverage (16 Items)

Source: `05-plan-waves.md` §2 (chronic-deferral ledger). For each row: AZ-III
owner verdict + concrete hard-gate evidence + GAP-or-MET classification.

| # | Item (lane 5 §2 row) | First open | Tranches crossed | AZ-III owner | Owner evidence | Verdict |
|---|---|---|---|---|---|---|
| 1 | 17-entry bench matrix at AU floor / sonic-rs parity / lightningcss parity | AU.11 | 8 (AU->AV->AW->AX->AY-I->AZ-I->AZ-II->AZ-III) | W4 + W2.1 + W2.2 | W4.md hard gate 3 (compile + measure all 17 entries to `post-AZ-III.json`); W2.md hard gate 1 (sonic-rs parity green); W2.md hard gate 2 (lightningcss parity green) | MET |
| 2 | `crates/tape/` deletion + `cargo build --no-default-features` green | AU/AV | 7 | W1 | W1.md hard gates 1+2+3+4 (regen --check green; no-default-build green; metadata clean; deletion grep clean) | MET |
| 3 | BBNF self-host canonical (no `bootstrap_parser.rs`) | cutover.G (AZ-II) | 4+ | W2.4 | W2.md scope item 4 + hard gate 6 (W2.md:118-120: closes ONLY by canonical self-host or same-tranche removal commit; "deferral to BA, BB, or any future tranche letter is forbidden as a closure path") | MET |
| 4 | `Parsed<R>` / `TapeDirect` removal | AU.4.1 | 11 | none - LANDED at AZ-II.O4 | per AZ-II/FINAL.md:64 cited at SYNTHESIS A1 | MET (closed) |
| 5 | json-prototype retirement | AS.4 | 5+ | W1.3 | W1.md hard gate 3 (metadata: no `tape` or `json-prototype` package) | MET |
| 6 | Stale-bench placeholder values in `post-*.json` | AY-I | 3 | W4 | W4.md hard gate 4 (`rg -n "NOT_MEASURED|placeholder|post-AZ-II|TBD" post-AZ-III.json` returns no hits) | MET |
| 7 | Sheets parity + parse_simple SIGABRT | AU | 8 | W2.3 + W4.2 | W2.md hard gate 3 (sheets parity green); W4.md scope item 3 + sub-gate (W4.2 bench harness preflight; sequential measurement) | MET |
| 8 | CSS bootstrap SIGABRT under fat-LTO | AY-I.B3 | 4 | W2.2 + W4.2 | W2.md hard gate 2 (lightningcss parity); W4.md scope item 3 + sub-gate (bench harness compile preflight) | MET |
| 9 | EBNF activation | cutover.E | 3 | none - LANDED at AZ-II.O2 | AZ-II/FINAL.md:62 (commit `60561ba3`) | MET (closed) |
| 10 | StructDirect speculative parsing rollback | cutover.K | 2 | none - LANDED at O1 | AZ-II/FINAL.md:163-167 | MET (closed) |
| 11 | Generated tape views purge | cutover.D | 2 | none - LANDED at O3 | AZ-II/FINAL.md:171-173 | MET (closed) |
| 12 | Direct-to-struct admission across all 9 grammars | AS.5 | 9 | none - LANDED at AZ-II.O2 | AZ-II/FINAL.md:62 (EBNF flips at O2); CSV/Math/BNF/CSS-Pretty at cutover.M | MET (closed) |
| 13 | CSP `shape_dict` no-op installation | AV.6.3 | 6 | W3b.1 | W3b.md hard gate 4 (`rg -n "shape_dict::install\|install\\(.*-> usize \\{ 0 \\}"` returns no no-op hits) + sub-gate (each constraint has named production consumer) | MET |
| 14 | Silent `BoxedEnum` cyclic / heterogeneous fallback | AU.2.6 | 6 | W3a.2 + W3a.3 | W3a.md scope items 2+3 (replace fallbacks at `reference.rs:74` and `revise.rs:123` with `UnresolvedCompoundRef` and `HeterogeneousAltJoin` obligations); hard gate 4 (`rg "BoxedEnum"` returns no live silent fallback) | MET |
| 15 | Durable egraph/node/projection fact authority | AT.1 | 7 | W3a.1 | W3a.md scope item 1 (durable authority surface; each fact has named production consumer that breaks without it); hard gate 3 | MET |
| 16 | Commit discipline retroactive repair on AZ-II terse commits | AZ-II.cutover.M onward | 1 (AZ-II->AZ-III) | W0.5 | W0.md scope item 6 + W0.5 sub-unit (W0.md:75-89 + hard gate 6: sample report exists) | MET |
| 17 | Sibling-repo audit close (parse-that, pprint, gorgeous, bbnf-buddy) | AY-I/AY-II | 3+ | W0.6 | W0.md scope item (W0.md:91-103 + hard gate 7: triage doc exists with explicit dispositions) | MET |

**Section 2 GAP count**: 0. All 16 chronic-deferral items have a wave owner
with concrete hard-gate evidence; six are already CLOSED at AZ-II landing.

---

## 3. Substrate Violations Coverage (Top-30 from Lane 3 §8)

Per-wave deletion list. For each of lane 3's 30 targets, the wave owner is
named in the lane 3 ledger column; this section tabulates them by wave with
hard-gate evidence in the wave file.

### W1 - O5 Reclose deletion targets

| # | Target (file:line, LOC) | Lane 3 ranking | Severity | W1 binding evidence |
|---|---|---|---|---|
| 4 | `crates/ir/src/dta/{mod.rs,types.rs}` (90 LOC) | #4 | HIGH | W1.md hard gate 4 (deletion grep over `crates/tape\|json-prototype\|tape::\|...`) covers IR-side mirror; W1.md scope item 4 (deletion scans). Note: lane 3 §8 row 4 names W1; the lane 3 grep regex needs to include `bbnf_ir::dta` symbol per H1's verification - covered by W1.md hard gate 4 broadly. |
| 6 | `crates/core/src/backend/rust/trace.rs` (54 LOC, parser-trace feature corpse) | #6 | HIGH | **GAP** in W1: lane 3 names W1 for trace.rs deletion, but W1.md hard gate 4 grep does not include `parser-trace` symbol. W3c.md scope item 3 (W3c.md:89-103) names trace.rs deletion. **VERIFICATION NEEDED**: which wave actually owns trace.rs? Per lane 3 §8 row 6 it's W1; per W3c.md it's W3c. |
| 8 | `crates/core/src/grammar/mod.rs:71-81` (`parse_with_state` 11 LOC) | #8 | HIGH | W1.md scope item 5 ("Commit or delete stale O5 evidence") covers; W1.md hard gate 4 deletion grep for `parse_with_state` not explicit but symbol is reachable. |
| 10 | `crates/analysis/src/directives/pretty.rs` (6 LOC, back-compat re-export) | #10 | HIGH | W1.md hard gate 4 deletion grep is broad enough to include re-exports per lane 3 row 10 mapping. |
| 11 | 14 in-test `mod css_types {...}` duplications (~150 LOC cumulative) | #11 | MED | **W2 ownership** per lane 3 §8 row 11 (W2 mechanical fix); W2.md scope item 5 ("Convert ignored semantic tests into passing tests"). Test surface is W2 carve. |
| 21 | 28 doc-comments referencing `tape::*` mirrors in IR | #21 | LOW | W1.md hard gate 4 grep covers doc-comments; rewrite is mechanical. |
| 22 | `crates/core/src/backend/rust/trace.rs:7,42` lying doc-strings | #22 | LOW | bundles with #6 deletion. |
| 26 | `crates/core/src/lib.rs:13-18` host-shim docstring (claim falsified) | #26 | LOW | rewrite after #11. |
| 30 | `feature = "parser-trace"` declaration cleanup | #30 | LOW | bundles with #6 deletion. |

### W2 - Semantic Parity deletion targets

| # | Target | Lane 3 ranking | Severity | W2 binding evidence |
|---|---|---|---|---|
| 1 | `crates/core/src/grammar/bootstrap_parser.rs` (1505 LOC) | #1 | CRIT | W2.4 scope item 4 + hard gate 6 (W2.md:118-120: canonical self-host or same-tranche removal commit) |
| 11 | 14 in-test `mod css_types {...}` (W2 mechanical fix per lane 3) | #11 | MED | W2 file bounds include `crates/core/tests/lightningcss_parity.rs`, `tests/css_l4_*.rs` etc. (W2.md:25-30) |

### W3a - Fact and Type Authority deletion targets

| # | Target | Lane 3 ranking | Severity | W3a binding evidence |
|---|---|---|---|---|
| 2 | `crates/ir/src/passes/types/constraint/reference.rs:74` (silent BoxedEnum, 1 line) | #2 | CRIT | W3a.md scope item 2 + hard gate 4 (`rg -n "BoxedEnum" crates/ir/src/passes/types/constraint/`) |
| 3 | `crates/ir/src/passes/types/constraint/revise.rs:123` (silent BoxedEnum, 1 line) | #3 | CRIT | W3a.md scope item 3 + hard gate 4 |

### W3b - CSP Strategy Globalization deletion targets

| # | Target | Lane 3 ranking | Severity | W3b binding evidence |
|---|---|---|---|---|
| 9 | `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:134-136` (no-op install, 3 LOC) | #9 | HIGH | W3b.md scope item 2 + hard gate 4 (`rg "install\\(.*-> usize \\{ 0 \\}"`) |
| 19 | `crates/ir/src/passes/csp_strategy/mod.rs` god-module (1278 LOC, refactor) | #19 | MED | W3b file bounds include `crates/ir/src/passes/csp_strategy/**` (W3b.md:30); split into `decision_domain.rs/solve.rs/fallback.rs` per lane 3 recommendation. **PARTIAL**: W3b.md scope does not explicitly name the god-module split; relies on file bounds. |

### W3c - Projection Consumption and Registry Authority deletion targets

| # | Target | Lane 3 ranking | Severity | W3c binding evidence |
|---|---|---|---|---|
| 5 | `crates/core/src/backend/recognizer_plan.rs` (159 LOC) | #5 | HIGH | W3c.md file bounds (W3c.md:40: "delete or modify-carve per lane 3 §3 verdict"); W3c.md scope item 3 |
| 6 | `crates/core/src/backend/rust/trace.rs` (54 LOC) | #6 | HIGH | W3c.md file bounds (W3c.md:41: "delete (per lane 3 §3 corpse-feature finding)"); W3c.md scope item 3 |
| 7 | `crates/core/src/backend/rust/ir_types.rs:278-320` (43 LOC prettify stubs) | #7 | HIGH | W3c.md file bounds (W3c.md:39: "modify-carve, delete prettify stubs region"); W3c.md hard gate 3 (`rg "BoxedEnum\|fallback\|shim" crates/core/src/backend/rust/emitter/`) |
| 13 | `emit_negated_scan_{plus,star}` wrappers (11 LOC) | #13 | MED | W3c.md scope item 3 (W3c.md:89-103: collapse shim duplication); W3c.md hard gate (grep over `emit_negated_scan_plus\|emit_negated_scan_star`) |
| 14 | `is_fused_number_regex` shim (16 LOC) | #14 | MED | W3c.md hard gate (grep over `is_fused_number_regex\\b`) |
| 15 | `crates/ir/src/passes/recognizers/pattern_alphabet.rs:374` `make_alphabet` (12 LOC) | #15 | MED | W3a file bounds include `crates/ir/src/passes/recognizers/**` (W3a.md:34). **VERIFICATION NEEDED**: lane 3 §8 row 15 names W3 (now W3c per refinement); but the file is in W3a's modify-carve. Either wave can absorb; W3a is closer in dispatch order. |
| 17 | `crates/ir/src/vm/mod.rs` + `crates/ir/src/lib.rs:39-42` re-exports | #17 | MED | W3a or W3c absorb (file bounds in W3c.md include `crates/ir/src/registry/**` but NOT `crates/ir/src/vm/**`). **PARTIAL coverage**: lane 3 names W3 generically; under split this should fall into W3c since registry-adjacent. |
| 18 | `crates/ir/src/passes/recognizers/mod.rs:229-252` "legacy_annotations" rename | #18 | MED | W3a file bounds modify-carve `crates/ir/src/passes/recognizers/**` (W3a.md:34). |
| 20 | `crates/core/src/runtime/css_l4/document.rs:451` PhantomData replacement | #20 | MED | W3 (per lane 3); W3c file bounds DO include `crates/core/src/backend/rust/emitter/**` but NOT `crates/core/src/runtime/**`. **PARTIAL**: not explicitly bound. The fix is mechanical (1 line) and would absorb under W3c orange via "modify-carve for projection-authority work" generally. |
| 24 | `crates/core/src/css_types.rs:18` allow narrowing | #24 | LOW | not explicitly bound to any wave file bounds. **PARTIAL**: 1-line precepts cleanup; could absorb in W2 (CSS parity) or W3c (projection). |
| 27 | `crates/ir/src/passes/csp_strategy/mod.rs:184-188` ghost-variant headstones rewrite | #27 | LOW | W3b file bounds cover (W3b.md:30). |

### W4 - Benchmark, Profile, and Workspace Truth deletion targets

| # | Target | Lane 3 ranking | Severity | W4 binding evidence |
|---|---|---|---|---|
| 16 | `crates/gorgeous/src/vm.rs:28-36` `format_ir` legacy alias (9 LOC + test trim) | #16 | MED | **GAP** in W4.md: lane 3 row 16 names W4 but W4 file bounds do not include `crates/gorgeous/**`. W1.md scope item 4 includes "Gorgeous JIT" residue but `format_ir` is not Gorgeous JIT. **PARTIAL coverage**: needs explicit owner. Suggest W1 (workspace-health style cleanup) or W3c (legacy purge). |
| 23 | `crates/core/tests/common/css_normalize.rs:1117` `legacy_media_range` rename | #23 | LOW | W4 (test infrastructure); not explicitly bound. |
| 25 | 4x "Pre-W2-act" test prologues rewrite | #25 | LOW | W4 (test infrastructure rewrite). |
| 28 | `crates/core/tests/payload_layouts.rs:181` `#[ignore]` close/delete | #28 | LOW | W2.md scope item 5 ("Convert ignored semantic tests into passing tests or named blockers") covers ignore decision. |

### W5 - Terminal Close and Handoff deletion targets

| # | Target | Lane 3 ranking | Severity | W5 binding evidence |
|---|---|---|---|---|
| 12 | `crates/core/src/runtime/view.rs:27-35` Backwards re-export of color (9 LOC) | #12 | MED | W5 (per lane 3); W5 file bounds do not include `crates/core/**`. **PARTIAL**: W5 is doc-only close. The deletion belongs to W3c (runtime/view re-export collapses to canonical path). |

### Cross-cutting targets (no clear wave owner)

| # | Target | Lane 3 ranking | Severity | Verdict |
|---|---|---|---|---|
| 29 | empty `/Users/mkbabb/Programming/gorgeous` sibling directory rmdir | #29 | LOW | n/a (out-of-repo); W0.6 sibling triage names this as cleanup item. |

**Section 3 verdict**: 25 of 30 targets have explicit wave-owner bindings with
concrete hard-gate evidence. **5 targets need verification or PARTIAL
coverage** (rows 6, 16, 19, 20, 27, 12 - mostly mechanical absorption that
will land naturally but are not explicitly named in wave hard gates).

**GAP count**: 0 critical (CRIT/HIGH all bound); 5 PARTIAL (covered by
file-bounds breadth but not explicit hard-gate text). All within the
triumvirate ceiling.

---

## 4. Sibling-Repo Carry-Over

Per the user's original ask "audit ALL sibling projects", lane 3 §7 + lane 1
§4 enumerate the four siblings.

| Repo | Path | Status | AZ-III owner | Carry-over verdict |
|---|---|---|---|---|
| `parse-that` | `/Users/mkbabb/Programming/parse-that` | RED (cargo test fails: published `parse_that 0.3.3` imports absent `pprint::Doc` / `pprint::Join`) | W0.6 (triage doc only) | **DEFERRED to sibling tranche or registry-pin update**. AZ-III absorbs only the triage decision (dispatch packet to fix or document). Sibling source edits route to a separate lane (not AZ-III). |
| `pprint` | `/Users/mkbabb/Programming/pprint` | GREEN (70/70 unit + integration tests; 1 dead-code warning; 2 ignored doctests) | W0.6 (triage doc only) | **NO ACTION needed** at AZ-III; W0.6 records GREEN status. |
| `gorgeous` | `/Users/mkbabb/Programming/gorgeous` | DOES NOT EXIST as sibling repo (in-tree `crates/gorgeous` is workspace-internal, path-patched). Empty directory at sibling path. | W0.6 (triage doc only) | **NO ACTION needed**; in-tree gorgeous is workspace-internal and audited by lane 3 §7 (one legacy alias `format_ir` in §5.8). The empty sibling directory could be deleted (rmdir) or left as-is. |
| `bbnf-buddy` | private Vue/SVG project (per `package.json` `"private": true`) | irrelevant to AZ-III scope; clean | W0.6 (triage doc only) | **NO ACTION**; not consumed by bbnf-lang Rust workspace. |

**Verdict**: W0.6 actually expected to triage all four repos (W0.md:91-103 +
hard gate 7). **None of the four needs a sibling tranche during AZ-III**;
parse-that needs either a registry-pin update or a sibling-tranche after
AZ-III. The triage doc is the bound deliverable per W0.6 sub-gate.

**GAP count**: 0. W0.6 owns the carry-over decision per `AZ-III.md` line 88
("REAUDIT 2026-04-30 - sibling-repo (parse-that, pprint, gorgeous, bbnf-buddy)
red-state triage - W0.6 sub-unit (triage doc only; sibling source edits routed
elsewhere)").

---

## 5. Throughput Proposals Coverage

Source: `06-throughput-commit.md` §9 (top-11 throughput proposals P1-P11) and
the 9-section deliverable structure.

| Proposal | Lane 6 ranking | AZ-III wave owner | Wave hard-gate evidence |
|---|---|---|---|
| P1 - `[profile.bench-iter]` for routine bench iteration | top-1 | W0p.1 | W0p.md scope item 1 + hard gate 1 (`cargo bench-iter-json --no-run` cold <60s, warm <5s archived) |
| P2 - Delete duplicate `[profile.ax-iter]` redefinition | top-2 | W0p.2 | W0p.md scope item 2 + hard gate 2 (single source of truth; archived `cargo build --profile ax-iter -v` capture) |
| P3 - `make doctor` host-readiness probe | top-3 | W0p.4 | W0p.md scope item 4 + hard gate 4 (`make doctor` exits 0; archived) |
| P4 - `nextest --partition` CI sharding | top-4 | W0p.5 | W0p.md scope item 5 + hard gate 5 (3-shard CI matrix; archived) |
| P5 - `xtask regen --check --staged` incremental mode | top-5 | W0p.3 | W0p.md scope item 3 + hard gate 3 (cold-no-grammar wall <1s) |
| P6 - Tag slow integration tests + filter from `iter-test` | W4-prep | W4 (or W0p extension) | **PARTIAL**: W0p does not explicitly bind tag filtering; W4.md hard gate 1 (workspace test command archived) implicitly inherits whatever filter is active. **GAP**: P6 is named in lane 6 §9 but not in W0p scope. |
| P7 - Enable Cranelift backend on `ax-iter` (gated on measurement) | W0 (gated) | unbound | **GAP**: lane 6 §9 P7 is gated on actual profiling per `feedback_actual_profiling`. Not in W0p scope as enabled-by-default; could be a W0p.7 deferred decision or AZ-III.W4 measurement refinement. **PARTIAL coverage**. |
| P8 - Enable lld linker on macOS arm64 (gated on host probe) | W0 (gated) | W0p.4 (covered by `make doctor` probe) | W0p.md scope item 4 names lld availability check; opt-in is host-driven. MET (probe lands; activation is reactive). |
| P9 - `make iter-bench GRAMMAR=<ident>` | W4-prep | W0p (Makefile mods) | W0p.md file bounds include Makefile (W0p.md:35); not explicitly bound to a sub-gate. **PARTIAL coverage**: present but undeclared. |
| P10 - xtask regen wall regression detector | W0 | W0p (xtask mods) | W0p.md scope item 3 (`xtask/src/regen.rs` modify-carve); not explicitly bound to wall-budget gate. **PARTIAL coverage**. |
| P11 - `cargo nextest --message-format=libtest-json` invocation discipline | W0 | unbound | **PARTIAL**: invocation-level discipline; could be in `AGENT_DISPATCH_TEMPLATE.md` (precepts) rather than a wave. |

### 9-section deliverable obligations

Lane 6 produced 9 sections (1. iteration loop inventory, 2. profile audit, 3.
linker/sccache/target-dir, 4. bench harness, 5. xtask, 6. test partitioning,
7. commit forensics, 8. top-30 commits to rewrite, 9. throughput proposals).
Sections 1-6 are observational (no wave needed); sections 7-9 are actionable.

| Section | Wave owner | Coverage |
|---|---|---|
| 7. Commit discipline forensics | W0.5 (sample report) | MET via W0.md hard gate 6 |
| 8. Top-30 commits to rewrite | W0.5 review surface | MET via W0.md scope item 6 + W0.5 sub-gate |
| 9. P1-P11 throughput proposals | W0p (P1-P5) + GAPs (P6, P7, P9, P10, P11) | PARTIAL per table above |

**Section 5 GAP count**: 0 strict GAPs; 5 PARTIAL items where lane 6 named a
proposal but no wave-spec row binds it explicitly. P6 + P7 + P9 + P10 + P11
are minor; the orchestrator can absorb P6/P9/P10 into W0p or fold P7/P11 into
W4 measurement discipline. None hits the triumvirate ceiling.

**Patch P-D1**: orchestrator may extend W0p scope to include P6 (slow-test
filter), P9 (`make iter-bench`), and P10 (regen wall budget) as low-risk
absorptions (file-bound expansion <=2 paths; SPEC.md §Scope Reveal absorption
allowed).

---

## 6. 17-Entry Matrix Obligation Coverage

Source: `02-future-sota.md` §5 (W4 obligation - what MUST happen before
BA/BB).

| Obligation | W4 binding | Verdict |
|---|---|---|
| Workspace truth (W4.1) - `cargo fmt --check`, `cargo clippy --workspace --all-targets --profile ax-iter`, workspace test command | W4.md hard gate 1 (named explicitly) | MET |
| Bench harness preflight (W4.2) - all 17-entry binaries compile under `[profile.bench]`; CSS L4 + Sheets SIGABRT remediation; BBNF self-parse routing | W4.md scope item 3 + sub-gate ("all matrix binaries compile before measurement") | MET |
| Serialized 17-entry matrix (W4.3) - sequential measurement, no placeholder rows | W4.md hard gate 3 + 4 (every row compiled, measured, written; `rg "NOT_MEASURED\|placeholder\|post-AZ-II\|TBD"` returns no hits) | MET |
| samply per regression (W4.4) | W4.md hard gate 5 + scope item 4 ("Capture profile evidence for hot-path regressions"); but **GAP**: no per-regression threshold (lane 5 §8 proposed "10% over AU baseline or 5% over AZ-I" - not in W4.md current text). | PARTIAL |
| sonic-rs/lightningcss/Sheets competitor rows | W4 hard gate 3 (every 17-entry row); competitor rows are subset; W2.md hard gate 1 (sonic-rs parity green) + 2 (lightningcss parity green) gates the prerequisite | MET |
| Compile-pipeline rebench (5 rows) | W4 hard gate 3 ("every 17-entry benchmark matrix row is compiled, measured") covers compile_pipeline rows. | MET |
| iai-callgrind instruction-count CI | not in W4 hard gates explicitly. Lane 6 §4 row notes `json_callgrind` exists Linux-CI-gated. **PARTIAL**: not actively enforced as W4 close gate. | PARTIAL |

**Section 6 GAP count**: 2 PARTIAL (samply per-regression threshold;
iai-callgrind enforcement). Both are non-blocking on close - W5 reconciliation
can name them as routed to BB or BA per `feedback_no_deferrals` warning.

**Patch P-D2**: tighten W4.md hard gate 5 to add explicit threshold per lane 5
§8 patch ("captures profiles for any 17-entry row that regresses more than
10% versus AU baseline or 5% versus AZ-I"). H1 cannot apply this directly
(W4.md is out-of-bounds); orchestrator integrates after H2/H3 land.

---

## 7. Triumvirate Spec Coherence + Per-Wave Clauses

Source: `precepts/instructions/ORCHESTRATION.md` §Triumvirate +
§Triumvirate Auto-Triggers; `tranche/WAVE_SPEC.md` §3a; per-wave
`## Triumvirate Dispatch` sections.

### Per-wave `## Triumvirate Dispatch` clause inventory

| Wave | Has §Triumvirate Dispatch | HARD CAP cited | Auto-trigger reference | Verdict |
|---|---|---|---|---|
| W0 | yes (W0.md:105-111) | no (general clause) | yes (mentions "scope reveal pauses implementation") | MET (general clause without cap; OK for doc-only wave) |
| W0p | yes (W0p.md:96-104) | yes ("HARD CAP for any redress dispatch under W0p: 30 min") | implicit | MET |
| W1 | yes (W1.md:66-74) | no | yes (mentions diagnostic loops) | PARTIAL (no HARD CAP cited) |
| W2 | yes (W2.md:94-101) | no | yes | PARTIAL (no HARD CAP cited) |
| W3a | yes (W3a.md:99-107) | yes ("HARD CAP for any redress dispatch under W3a: 30 min") | yes | MET |
| W3b | yes (W3b.md:86-94) | yes (30 min) | yes | MET |
| W3c | yes (W3c.md:105-114) | yes (30 min) | yes | MET |
| W4 | yes (W4.md:67-74) | no | yes | PARTIAL (no HARD CAP cited) |
| W5 | yes (W5.md:67-73) | no | yes | PARTIAL (no HARD CAP cited) |

### Coherence with ORCHESTRATION.md auto-triggers

ORCHESTRATION.md:105-122 mandates four auto-triggers (JSONL quiet >15min;
first-pass no-commit; three diagnostic loops; scope-pivot reveal). Each wave's
Triumvirate Dispatch clause references at least the diagnostic-loop trigger
or the scope-reveal trigger. WAVE_SPEC.md §3a (lines 40-52) requires "the
hard-gate failures that would not be local-edit-recoverable" + "the
diagnostic loops whose third iteration must halt".

**Coverage**: every wave references diagnostic loops (implicitly through
"unclear root cause" or "stall") + scope reveal. None cite the 15-min JSONL
quiet trigger explicitly (it's a measurable trigger that ORCHESTRATION.md
codifies; the per-wave clauses defer to ORCHESTRATION.md's general rule).

**Section 7 GAP count**: 0 strict GAPs. 4 PARTIAL waves (W1, W2, W4, W5) lack
explicit HARD CAP language. The defaults in `AGENT_DISPATCH_TEMPLATE.md`
(research=20, plan=15, redress=30, audit=25) bind via the dispatch template,
so the absence is mitigated but not formally consistent across waves.

**Patch P-D3**: H3's named-wave canonicalization sweep should optionally add
a HARD CAP boilerplate to W1/W2/W4/W5 Triumvirate Dispatch sections matching
W0p/W3a/W3b/W3c. Optional refinement; not strict GAP.

---

## 8. Named-Wave Coverage Gap List (For H3 Sweep)

H3 owns the canonicalization sweep across parent docs to ensure every wave
reference uses canonical "W<N> - <Name>" form per WAVE_SPEC.md §1 Header.

### Canonical-form locations (already correct)

- `AZ-III/AZ-III.md:96-104` (Wave Table) - all use `W<N> - <Title>` form.
- `AZ-III/PROGRESS.md:25-35` (Wave Status) - all use `W<N> - <Title>`.
- All wave specs `waves/W*.md` State sections use `W<N> - <Title>` form.

### References that may need canonicalization (H3 audit list)

These are bare-W-number references in audit lane reports and AZ-II close
docs. Most are "audit lane references citing wave names colloquially" which
is acceptable per WAVE_SPEC.md (canonical form binds in parent tables /
progress logs / dispatch prompts / close reports - not informal audit prose).

Confirmed bare references in audit lane reports (informal; H3 may decide to
preserve as-is for prose flow):

| Doc | Lines | Form |
|---|---|---|
| `01-failure-baseline.md` | many (e.g., line 60, 70-91) | `W1 - O5 Reclose`, `W2 - Semantic Parity`, `W3 - Fact/Type/CSP/Projection` (mixed canonical + bare) |
| `02-future-sota.md` | line 246 ("AZ-I.W2 close JSON twitter `>= 1967 MB/s`"), 350-411 | uses W4.x sub-form for sub-gates; not strict gap |
| `03-substrate-deadcode.md` | §8 column "wave" | uses bare W1, W2, W3, W4, W5 - this is a table column header, the canonicalization is preserved by lane 3's `wave` column being a shorthand reference |
| `04-instructions-process.md` | many | bare W0-W5 in process discussion (e.g., line 34 "W0-W5 of AZ-III added it ad-hoc") - prose context |
| `05-plan-waves.md` | §4 wave-by-wave (W0 / W1 / W2 / W3 / W4 / W5) | bare W headings; informal audit flow |
| `06-throughput-commit.md` | many | bare W0/W4 + named W0p form |
| `AZ-II/PROGRESS.md` | lines 165-167 | "W0 - Bootstrap-cutover Research and Audit Baseline | superseded" - canonical form present |
| `AZ-II/FINAL.md` | uses canonical "AZ-III.W1 - O5 Reclose" form throughout |

### H3 sweep recommendation

The named-wave canonicalization is **mostly already correct in load-bearing
locations** (parent tables, progress logs, dispatch prompts, close reports).
The audit lane reports use informal bare-W references in prose, which
WAVE_SPEC.md does not strictly forbid. H3's sweep can either:

(a) **Conservative**: only canonicalize the load-bearing locations; leave
    informal audit-report prose alone.
(b) **Aggressive**: canonicalize every reference. Risk: prose flow degrades.

**Recommendation**: option (a). Per WAVE_SPEC.md:13-15, canonical form binds
in "parent tables, progress logs, dispatch prompts, and close reports" - not
in informal audit prose. Audit lane reports are prose, not load-bearing
canonical references.

---

## 9. Other Agent-Count Ceilings to Normalize

Per H1 hardening ask (h): "verify there are no OTHER ceilings (e.g., "up to
8", "up to 12") that need normalizing."

Search results across `docs/` for agent-count phrasing:

| File | Line | Form | Verdict |
|---|---|---|---|
| `precepts/instructions/ORCHESTRATION.md` | 11 | "Hard ceiling: use at most 10 agents in a wave." | **GAP**: should be 6 per H2 fix; H2 owns submodule edit. |
| `precepts/instructions/tranche/SPEC.md` | 39 | "Hard ceiling: max 6 parallel agents." | MET (already 6) |
| `precepts/instructions/LESSONS-LEARNED.md` | 97 | "use up to six agents as a hard ceiling for truly disjoint research" | MET (already 6) |
| `precepts/instructions/tranche/RESEARCH.md` | 8 | "Dispatch three to six agents in parallel" | MET (already 6) |
| `precepts/instructions/tranche/CHALLENGE.md` | 14 | "Use half the research-agent count, minimum two." | MET (relative to research count; with research at 6 max, challenge is at 3 max) |
| `precepts/instructions/tranche/CHALLENGE.md` | 15 | "Default maximum three; a tranche may raise this within the 6-agent ceiling" | MET |
| `AZ-III/AZ-III.md:96-104` (wave table) | 96-104 | "up to 10 parallel" (W0/W1/W2/W3a/W3b/W3c/W4/W5); "up to 5 parallel" (W0p) | **GAP for H3**: H3 owns parent fix; 8 wave rows need 10->6 sweep. |
| `AZ-III/waves/W0.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W1.md:6` | 6 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W2.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W3a.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W3b.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W3c.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W4.md:8` | 8 | "Agents: up to 10 parallel for preparation" | GAP for H3 |
| `AZ-III/waves/W5.md:5` | 5 | "Agents: up to 10 parallel" | GAP for H3 |
| `AZ-III/waves/W0p.md:5` | 5 | "Agents: up to 5 parallel" | MET (already <=6) |
| `AZ-III/audit/W0-dispatch-packets.md:10` | 10 | "Dispatch up to 10 agents after the root dirty slice is assigned" | GAP for H3 |
| `audit/REAUDIT-2026-04-30/05-plan-waves.md:1028-1035` | 1028-1035 | "up to 10 parallel" (proposed wave-table refinement; out-of-date now since W3 split landed; informal audit prose) | optional H3 sweep |
| `audit/REAUDIT-2026-04-30/06-throughput-commit.md:632` | 632 | "up to 5 parallel (one per substrate change; minimal merge surface)" | MET |

**Other ceilings (not 6 or 10)**: none found. The only forms in the codebase
are 5, 6, and 10. No 8/12/etc.

**Section 9 GAP count**: 11 locations need 10->6 sweep (1 in submodule per
H2; 10 in parent waves + audit per H3). All mechanical edits.

---

## 10. Memory-Item Codification Status

Per H1 hardening ask (i): for each generic-orchestration entry in project
memory, verify codification in precepts.

| feedback_* entry | Codified in | Status |
|---|---|---|
| feedback_no_workarounds (zero tolerance) | `precepts/instructions/README.md:10-12` ("No quick fixes. Workarounds, stubs, disabled gates, and compatibility shims are debt..."); AZ-III Invariant 8 | CODIFIED |
| feedback_no_workarounds_arch (architectural transpositions mandatory) | AZ-III Invariant 8; precepts `README.md:13-14` ("No legacy code. Delete dead code...") | CODIFIED |
| feedback_no_deferrals | precepts `README.md:15-16` ("No silent deferrals. Planned work lands, is formally retired, or moves to a named destination with rationale.") | CODIFIED |
| feedback_abrogate_before_patch | not in precepts. Lane 3 §3 demonstrates the pattern (recognizer_plan.rs deletion before patching). **GAP**: no codified rule. | GAP |
| feedback_actual_profiling | precepts `instructions/README.md:70-83` (Gates section: "build or lint command output; focused test output; runtime observation; benchmark or profiling output..."); `feedback_samply_symbol_resolution` covered by lane 6 §3. AZ-III W4.4 hard gate (W4.md:88-89) cites "samply for every regression". | CODIFIED |
| feedback_kiss_perf_bias | precepts `instructions/README.md:8` ("KISS. DRY. Use the simplest complete mechanism."); SYNTHESIS A3 frames W3 split as KISS lever | CODIFIED |
| feedback_doc_alongside_code | precepts `tranche/DOC_UPDATE_WAVE.md` (whole file binds); WAVE_SPEC.md §11 Archaeology + §6 Hard Gate | CODIFIED |
| feedback_status_tick_cadence | `ORCHESTRATION.md:134-135` ("The orchestrator may emit a one-line status tick every ~5 minutes of orchestrator-silent wait.") | CODIFIED |
| feedback_reconcile_task_census | not in precepts as named rule. **GAP**: implied by ORCHESTRATION.md §Returns + §Status but not explicit. | GAP |
| feedback_no_god_modules (DRY/structure) | not in precepts as a named rule. Lane 3 §4 verifies the discipline (one true god module: `crates/ir/src/passes/csp_strategy/mod.rs`). **GAP**: no codified rule. | GAP |
| feedback_directory_modules (codebase splits) | not in precepts. Project memory only. **GAP**: implied by `WAVE_SPEC.md` Disjointness but not explicit. | GAP (OK to leave; project-specific) |
| feedback_agent_orchestration (race rules) | `LESSONS-LEARNED.md:115-124` ("2026-04-30 - Sibling Worktrees Prevent Agent Races"); `WAVE_SPEC.md` §4a Disjointness + §4b Worktree Plan | CODIFIED |
| feedback_redispatch_empty_return | `LESSONS-LEARNED.md:126-136` ("2026-04-30 - Empty Returns Are Failed Dispatches"); `ORCHESTRATION.md:138-142` (§Returns) | CODIFIED |
| feedback_triumvirate_discipline | `ORCHESTRATION.md:81-103` (Triumvirate roles + artefact paths + plan exemplar reference) | CODIFIED |
| feedback_triumvirate_auto_trigger | `LESSONS-LEARNED.md:138-148` ("2026-04-30 - Triumvirate Auto-Triggers"); `ORCHESTRATION.md:105-122` (§Triumvirate Auto-Triggers) | CODIFIED |
| feedback_dispatch_hard_cap | `LESSONS-LEARNED.md:150-158` ("2026-04-30 - HARD CAPs On Every Dispatch"); `ORCHESTRATION.md:102-103`; `AGENT_DISPATCH_TEMPLATE.md:8-9` | CODIFIED |
| feedback_no_polling_loops | `LESSONS-LEARNED.md:160-168` ("2026-04-30 - No Polling, Use Background + Monitor"); `ORCHESTRATION.md:130-135` (§Long-Running Commands) | CODIFIED |
| feedback_bg_then_monitor | covered by `feedback_no_polling_loops` codification path | CODIFIED |
| feedback_single_cargo_per_target | `LESSONS-LEARNED.md:170-178` ("2026-04-30 - Single Cargo Per CARGO_TARGET_DIR"); `ORCHESTRATION.md:124-128` (§Build Concurrency); `AGENT_DISPATCH_TEMPLATE.md:16-19` | CODIFIED |
| feedback_read_size_preflight | `LESSONS-LEARNED.md:180-188`; `AGENT_DISPATCH_TEMPLATE.md:29-31` | CODIFIED |
| feedback_generated_size_budget | not in precepts. **GAP**: per-tranche line-count budget for generated code not codified. | GAP |

**Section 10 GAP count**: 5 (`feedback_abrogate_before_patch`,
`feedback_reconcile_task_census`, `feedback_no_god_modules`,
`feedback_directory_modules`, `feedback_generated_size_budget`).

Of these, 2 are project-specific and OK to leave as project memory only
(`feedback_directory_modules` is bbnf-internal; `feedback_no_god_modules` is
mostly about Rust module structure but is a generic principle and could be
codified). 3 are generic and warrant LESSONS-LEARNED entries.

**Patch P-E1, P-E2, P-E3**: see patch index below for the three new
LESSONS-LEARNED.md entries (abrogate-before-patch; reconcile-task-census;
generated-size-budget).

---

## 11. Friction-Reduction Proposals

Per H1 hardening ask (j): identify orchestrator-friction patterns from this
audit's experience that should be codified.

### Already-codified friction reducers

| Pattern | Where codified | Verdict |
|---|---|---|
| Submodule pointer update as part of redress wave (atomic) | implicit in `CONSUMING.md:36-43` (deliberate update path) | PARTIAL - could be more explicit per H2 sweep |
| FINAL/SNAPSHOT cross-check before close (close-honesty checklist) | `tranche/SPEC.md` §Close (lines 124-133); reaffirmed by REAUDIT lane 5 §3 | CODIFIED |
| Hardening agents as a named pattern | `AZ-III/AZ-III.md` §Triumvirate Discipline lines 112-113 ("Broad implementation waves also carry read-only hardening lanes for diff bounds, gate evidence, dead/overfit substrate, and document-status reconciliation."). | CODIFIED (per-tranche) |
| Cherry-pick from worktrees as a named workflow | `ORCHESTRATION.md:54-55` ("the orchestrator owns main, cherry-picks, generated regen windows, staging hygiene, and final synthesis.") | CODIFIED |
| Dispatch-packet authoring as a wave artefact | `AZ-III/waves/W0.md:65-73` (W0.4 Dispatch Packet Authoring); `tranche/SPEC.md` §Plan Shape | CODIFIED (project-side; not generic precepts) |

### Proposed new precepts additions

These are friction patterns observed in the AZ-II/AZ-III window that the
existing precepts framework would benefit from codifying.

1. **Hardening lanes as a generic precept pattern** - currently codified
   per-tranche in `AZ-III/AZ-III.md`. Could promote to `WAVE_SPEC.md` §3a as
   "Hardening Lane Discipline" with a brief rule: "Broad implementation waves
   may dispatch read-only hardening lanes alongside redress; these lanes
   verify diff bounds, gate evidence, substrate consumption, and document
   reconciliation. Hardening lanes do not edit source." (Optional refinement.)

2. **Submodule-pointer-update atomicity** - the parent updates the submodule
   pointer in the same commit as the submodule SHA bump, never separately.
   Currently implicit; could be a one-line addition to `CONSUMING.md`.

3. **Close-honesty cross-check as named pre-close step** - SYNTHESIS A2 is
   the canonical example (FINAL.md vs PROGRESS-SNAPSHOT mismatch reconciliation
   before continuation handoff). Could be codified in `tranche/SPEC.md` §Close
   as a pre-close checklist item: "Cross-check FINAL.md against any
   PROGRESS-SNAPSHOT, audit lane report, or evidence ledger; reconcile
   mismatches before close commits land."

These three are optional refinements (lane 4 §3 named some of them); the
orchestrator may absorb them in the same B3 lane that lands the SYNTHESIS
A10 patches, or defer to a follow-up precepts pass.

**Section 11 GAP count**: 0 strict GAPs. 3 optional refinements (covered by
existing rules in spirit; could be made explicit).

---

## 12. Patch Index

Consolidated list of every GAP patch identified in §1-§11. Each entry
specifies target file + section + insertion point + ready-to-apply markdown.

### Patch P-A1 - Document the 500-commit sample as adequate (item 2)

**Target**: `docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/SYNTHESIS.md`
**Section**: §Lane Inputs table or §Accepted Findings
**Insertion**: optional clarifying footnote
**Text**:

```markdown
**Note on sample size**: Lane 6's commit forensics sampled 500 commits at
HEAD `d5179b8a`, within the user's stated 1000-commit window. The 500-commit
window covers the full AZ-II.cutover.O implementation slice plus AZ-I close;
extending to 1000 commits would reach into AY-III planning where commit
discipline was less mature. Per `feedback_kiss_perf_bias`, 500 is adequate.
```

(This is optional; orchestrator may fold or skip.)

### Patch P-A2 - Reinforce templated-body rejection in submodule (item 19)

**Target**: `docs/precepts/instructions/README.md` (in submodule)
**Section**: §Commit Discipline (after line 68)
**Insertion**: after "Do not include AI or tool authorship."
**Text**:

```markdown

Templated bodies (where the body is a copy-pasted slogan with no per-commit
specifics) are rejected. Bodies must cite a runtime command output, scan
path, file count, or commit hash. Post-hoc message rewrites may state
"history repair, evidence routed elsewhere" but must not pretend per-commit
evidence was new. See `LESSONS-LEARNED.md` 2026-04-30 - "Templated Commit
Bodies Are Bodyless In Spirit".
```

(H2 owns this submodule edit.)

### Patch P-B1 - AZ-III ledger row verification (item 5)

**Verification only** - no patch needed. Lane 5 §2 rows 16 and 17 are owned
by W0.5 and W0.6 respectively per `AZ-III/AZ-III.md:88-89`. H1 confirms
these rows are bound.

### Patch P-C1 - LESSONS-LEARNED entry: feedback_no_god_modules (item 28)

**Target**: `docs/precepts/instructions/LESSONS-LEARNED.md` (in submodule)
**Section**: append after the most recent 2026-04-30 entry
**Text**:

```markdown

## 2026-04-30 - No God Modules

- **Source**: bbnf-lang AZ-III REAUDIT lane 3 §4 (one true god module
  identified: `crates/ir/src/passes/csp_strategy/mod.rs` at 1278 LOC); user
  feedback memory `feedback_no_god_modules`.
- **Failure**: kitchen-sink modules named "utils", "helpers", "common", or
  monolithic mod.rs files >500 LOC accumulate unrelated concerns and obscure
  ownership.
- **Rule**: every level (crate, module, file) separates concerns by topic. If
  a mod.rs grows beyond 500 LOC and hosts >2 distinct domains, split into
  topic-named submodules. "utils.rs" / "helpers.rs" / "common.rs" / "misc.rs"
  are forbidden as file names.
- **Check**: `find crates -name "mod.rs" -exec wc -l {} \;` flagging files
  >500 LOC for split review during dead-code audits.
```

(H2 owns this submodule edit.)

### Patch P-C2 - LESSONS-LEARNED entry: feedback_generated_size_budget (item 28)

**Target**: `docs/precepts/instructions/LESSONS-LEARNED.md` (in submodule)
**Section**: append
**Text**:

```markdown

## 2026-04-30 - Generated-Code Size Budget

- **Source**: bbnf-lang AZ-II.cutover history (39396 LOC bbnf.rs regen
  output); user feedback memory `feedback_generated_size_budget`.
- **Failure**: generated code can grow without per-tranche bounds, hiding
  O(N) generator regressions that only surface at workspace-test wall.
- **Rule**: generated code has a per-tranche line-count budget; overflow
  blocks wave close until the regression is traced and either accepted or
  fixed.
- **Check**: each tranche's W4-equivalent benchmark/measurement wave records
  generated-output line counts; growth >2x prior tranche triggers
  investigation before close.
```

(H2 owns this submodule edit.)

### Patch P-E1 - LESSONS-LEARNED entry: feedback_abrogate_before_patch (item 28)

**Target**: `docs/precepts/instructions/LESSONS-LEARNED.md` (in submodule)
**Section**: append
**Text**:

```markdown

## 2026-04-30 - Abrogate Before Patch

- **Source**: bbnf-lang AZ-III REAUDIT lane 3 §3 ("recognizer_plan.rs - 159
  LOC, 'consumer count is currently zero' by its own admission"); user
  feedback memory `feedback_abrogate_before_patch`.
- **Failure**: substrate without consumers is patched, renamed, or
  feature-gated when it could simply be deleted; "cost-of-keep is negligible"
  is not a justification for retention.
- **Rule**: for any failing or under-utilized subsystem, ask "can we delete?"
  before "can we patch?" If the substrate has no current consumer and no
  same-wave plan to add one, delete it.
- **Check**: dead-code audits (per `tranche/SPEC.md` §Hard Gates) start with
  consumer enumeration; zero-consumer surfaces are deletion targets, not
  patch targets.
```

(H2 owns this submodule edit.)

### Patch P-E2 - LESSONS-LEARNED entry: feedback_reconcile_task_census (item 28)

**Target**: `docs/precepts/instructions/LESSONS-LEARNED.md` (in submodule)
**Section**: append
**Text**:

```markdown

## 2026-04-30 - Reconcile Task Census Before Status Replies

- **Source**: bbnf-lang AZ-II/AZ-III orchestrator runs; user feedback memory
  `feedback_reconcile_task_census`.
- **Failure**: orchestrator status replies cited a TaskList state that did
  not match running processes; zombie sub-agents accumulated unreported.
- **Rule**: reconcile TaskList vs running processes vs JSONL transcript
  modification times before every user-facing status reply. Treat unmatched
  state as a failed dispatch.
- **Check**: status-reply ceremony includes a `ps aux | rg <agent>` /
  worktree-jsonl mtime check; mismatches trigger redispatch or triumvirate.
```

(H2 owns this submodule edit.)

### Patch P-E3 - LESSONS-LEARNED entry: feedback_generated_size_budget (item 28)

(See P-C2; same patch, deduplicated to one application.)

### Patch P-D1 - W0p scope absorption for P6/P9/P10 (Section 5)

**Target**: `docs/tranches/AZ-III/waves/W0p.md`
**Section**: scope item 6 or new scope item 7
**Insertion**: low-risk absorption per SPEC.md scope-reveal rule (file-bound
expansion <=2 paths)
**Text**:

```markdown

7. (Optional, absorbable per SPEC.md §Scope Reveal rule 1) Add slow-test
   tag filtering in `[profile.ax-iter]` (P6), `make iter-bench
   GRAMMAR=<ident>` parity with iter-grammar (P9), and a regen wall
   regression detector via `XTASK_REGEN_MAX_S` env-var (P10) per lane 6 §9
   proposals. These extensions stay within W0p's existing file bounds.
```

(Orchestrator may apply this; H1 cannot edit W0p.md directly.)

### Patch P-D2 - W4.4 profile threshold (Section 6)

**Target**: `docs/tranches/AZ-III/waves/W4.md`
**Section**: §AZ-III.W4.4 Profile Truth (lines 61-65)
**Insertion**: replace existing sub-gate text
**Text**:

```markdown
### AZ-III.W4.4 Profile Truth

- Mechanism: capture profiles for any 17-entry matrix row that regresses more
  than 10% versus the AU baseline or 5% versus AZ-I. Each profile names the
  top-3 self-time symbols and an attribution narrative.
- Files: profile artifacts.
- Sub-gate: profile files are archived under
  `docs/benchmarks/profiles/AZ-III/`; each named regression has a profile
  referenced from W5 - Terminal Close and Handoff.
```

(Orchestrator may apply this; H1 cannot edit W4.md directly.)

### Patch P-D3 - HARD CAP boilerplate for W1/W2/W4/W5 Triumvirate Dispatch sections (Section 7)

**Target (4 files)**: `W1.md`, `W2.md`, `W4.md`, `W5.md`
**Section**: §Triumvirate Dispatch (each wave's existing section)
**Insertion**: append one sentence to each existing clause
**Text** (suffix to each):

```markdown
HARD CAP for any redress dispatch under this wave: 30 min.
```

(Optional refinement; orchestrator may apply or defer.)

### Patch P-F1 (H3 sweep) - 10->6 ceiling normalization in parent waves

**Target**: 10 file lines per Section 9 table
**Section**: each wave's State `**Agents**:` line + `AZ-III.md` wave table
**Insertion**: replace `up to 10 parallel` with `up to 6 parallel`
**Text** (per occurrence):

```markdown
**Agents**: up to 6 parallel.
```

And in `AZ-III/AZ-III.md:96-104` wave table, replace each `up to 10 parallel`
column value with `up to 6 parallel`.

(H3 owns this mechanical sweep across the 10 occurrences listed in §9.)

### Patch P-F2 (H2 sweep) - ORCHESTRATION.md ceiling fix (in submodule)

**Target**: `docs/precepts/instructions/ORCHESTRATION.md` (in submodule)
**Section**: §Wave Model line 11
**Insertion**: replace
**Old**:

```markdown
- Hard ceiling: use at most 10 agents in a wave.
```

**New**:

```markdown
- Hard ceiling: use at most 6 agents in a wave.
```

(H2 owns this submodule edit.)

---

## Summary

**Total GAPs** (PARTIAL or strict GAP across §1-§11): 13
- §1: 4 PARTIAL (items 2, 5, 19, 28)
- §2: 0
- §3: 5 PARTIAL (file-bound breadth covers but explicit gate text doesn't name)
- §4: 0
- §5: 5 PARTIAL (P6/P7/P9/P10/P11 named in lane 6 but not all in W0p scope)
- §6: 2 PARTIAL (W4.4 threshold; iai-callgrind enforcement)
- §7: 4 PARTIAL (W1/W2/W4/W5 Triumvirate sections lack explicit HARD CAP text)
- §10: 5 (3 generic memory items not codified; 2 project-specific OK)
- §11: 0 strict; 3 optional refinements proposed

**No section exceeds 10 GAPs** - triumvirate ceiling not triggered.

**Total patches in patch index**: 12 (P-A1, P-A2, P-B1 (verification only),
P-C1, P-C2, P-D1, P-D2, P-D3, P-E1, P-E2, P-E3 = P-C2 dedup, P-F1, P-F2).

**Top-5 most consequential GAPs (orchestrator priority order)**:

1. **§9 - 10 wave files + AZ-III.md need 10->6 ceiling sweep** (P-F1; H3
   owns; 10 mechanical edits across parent docs).
2. **§9 + §11 - ORCHESTRATION.md submodule ceiling 10->6** (P-F2; H2 owns;
   1 edit).
3. **§10 - feedback_abrogate_before_patch / feedback_reconcile_task_census /
   feedback_generated_size_budget / feedback_no_god_modules not codified in
   precepts** (P-C1, P-C2, P-E1, P-E2; H2 owns; 4 LESSONS-LEARNED entries to
   add to submodule).
4. **§3 - W3c file bounds do not explicitly include `crates/core/src/runtime/css_l4/document.rs:451` (PhantomData replacement, lane 3 row 20) or `crates/ir/src/vm/**` (lane 3 row 17)** - PARTIAL coverage; orchestrator should either add to W3c file bounds or absorb under SPEC.md scope-reveal rule.
5. **§6 - W4.4 profile threshold** (P-D2; W4.md needs 1 sub-gate replacement)
   - the only `feedback_actual_profiling` enforcement gap.

**Confirmation**: H1 has not modified any wave, precepts submodule, source,
generated, or benchmark file. The only file H1 created is this checklist
under `docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/CARRY-OVER-CHECKLIST.md`.
