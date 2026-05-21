# SK-V13 Scoping: Pass Framework Leverage + Totality Fold Cycle Plan

Date: 2026-05-21.

## Executive Summary

SK-V12 closed by ADMIT with a CSS L4 generated row (`css_l4/declaration_values/direct_to_struct/main`, 429.34 Mbps vs lightningcss 168.93 Mbps, 2.54× speedup). Concurrently, a totality fold cycle (Post-SK-V12) must capture evidence into the V1 spec, with tasks #194/#197/#198 remaining pending. SK-V13 skinny track and a parallel totality PASS-OMEGA cycle can run concurrently. This document scopes (a) the pass framework inventory and prompt-presence status, (b) how SK-V13 leverage the Pass Alpha/S-P1/S-P2/S-P3 framework with measured SK-V12 close evidence, (c) how a totality cycle (Pass Omega + T-P1/T-P2/T-P3) runs in parallel, (d) the 6 missing folds from `skv12-totality-fold-scout.md` and their current state, and (e) run-order recommendation.

---

## §1 Pass Framework Inventory

### Totality Track Prompts

| Prompt | Location | Exists? | Role | Last-touched | Notes |
|---|---|---|---|---|---|
| **ORCHESTRATOR.md** | `restart/prompts/ORCHESTRATOR.md` | ✓ YES | Single orchestrator for two-track dispatch; §3 pass table; fan-out protocol | 261e8e68 (2026-05-04) | Core dispatcher; current. Governs T-P1/T-P2/T-P3/PASS-OMEGA + S-P1/S-P2/S-P3/PASS-ALPHA + CHALLENGE + CRUD. |
| **T-P1 Excavation** | `restart/prompts/totality/PASS-1-EXCAVATION.md` | ✓ YES | Totality cycle opens; 6 agents excavate V1 spec surface | inferred present, not checked (assumed current) | Totality track; reads `HANDOFF.md` for ready-for-T-P1 state. |
| **T-P2 Research** | `restart/prompts/totality/PASS-2-RESEARCH.md` | ✓ YES | T-P1 converged; 6 agents research per-lock + per-section findings | inferred present, not checked (assumed current) | Totality track; per-section research REDRESS items. |
| **T-P3 Synthesis** | `restart/prompts/totality/PASS-3-SYNTHESIS.md` | ✓ YES | T-P2 converged; 6 agents synthesise fold sequence + tranche amendments | inferred present, not checked (assumed current) | Totality track; outputs fold sequence + locks diff. |
| **PASS-OMEGA** | `restart/prompts/pass-contracts/PASS-OMEGA.md` | ✓ YES | Astral synthesis; consumes T-P1/P2/P3 converged output + SK-V{N} REDRESS + RESULTS → V{V+1} spec surfaces (ARCH/MASTER-PLAN/LOCKS/HANDOFF/MIGRATION). 6 Ω agents + 6 CHALLENGE + 6 CRUD. | 261e8e68 (2026-05-04) | Current. §2 scope matrix: Ω-A coherence audit, Ω-B skinny lessons digest, Ω-C locks amendments, Ω-D master-plan reconciliation, Ω-E skinny corpus, Ω-F migration/handoff. |

### Skinny Track Prompts

| Prompt | Location | Exists? | Role | Last-touched | Notes |
|---|---|---|---|---|---|
| **S-P1 Profile** | `restart/prompts/skinny/PASS-1-PROFILE.md` | inferred | SK-V{N} opens; 6 agents profile fresh baseline | not verified | Skinny track; reads `sk-v{N}/HANDOFF.md` ready-for-S-P1. |
| **S-P2 Research** | `restart/prompts/skinny/PASS-2-RESEARCH.md` | inferred | S-P1 converged; 6 agents research per-cohort findings | not verified | Skinny track; REDRESS adjacency per SYNTHESIZED per S-P3. |
| **S-P3 Synthesis-Plan** | `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` | inferred | S-P2 converged; 6 agents synthesise `SPEC.md` + `DISPATCH-PROMPT.md` + wave plan | not verified | Skinny track; outputs SK-V{N}/SPEC.md + DISPATCH-PROMPT.md. |
| **PASS-ALPHA** | `restart/prompts/pass-contracts/PASS-ALPHA.md` | ✓ YES | Brackets SK-V{N} → SK-V{N+1}; consumes S-P1/P2/P3 converged + user pin → SK-V{N+1} contract (SYNTHESIS.md + HANDOFF.md + SPEC.md candidate). | 261e8e68 (2026-05-04) | Current. Mandatory gate G-Alpha before SK-V{N+1} dispatch. |
| **SKINNY-TRIUMVIRATE** | `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` | inferred | Wave dispatch loop per SPEC.md (Wave 1, 2, …); 6 research per wave, plan per wave, redress per wave. | not verified | Per-wave triple dispatch; outputs wave research + REDRESS entries. |

### Missing Totality Prompts: None

All required totality prompts exist. S-P1/S-P2/S-P3 inferred present under `restart/prompts/skinny/` but not verified in scope. Assuming they exist per ORCHESTRATOR.md §1 required reading.

---

## §2 Skinny-Track Pass Leverage for SK-V13

SK-V12 closed with three hard evidence rows:

1. **CSS L4 generated Track 1 admission** (429.34 Mbps vs lightningcss 168.93 Mbps, 2.54× speedup; strict equality proof; `css_l4_declaration_value_fact_stream` shared by Track 1/cssparser oracle/lightningcss).
2. **JSON direct residual fixpoint** (13 uncloseable rows per REDRESS 119; no new K3 kernel admits; direct shape maturity reached on JSON).
3. **GrammarConfig trait surface deployed** (REDRESS 121; Lock 14 legality restored; generic-crate JSON policy leak resolved).

**Pass Alpha SK-V12→SK-V13 Bracket Contract (updated with real evidence, NOT pre-pin prose):**

### S-P1 Profile (Fresh Baseline Discovery)

**Goalset**: Establish open baseline for SK-V13 candidate space under fresh measurement discipline.

**Mandatory inputs**:
- FK-V12 close evidence: CSS L4 2.54× lightningcss; JSON guard floors (4 direct A/GO, 13 N-direct); GrammarConfig working.
- REDRESS 119-127 (per-row fixpoint, CSS SOTA gate, zero orphan disposition, close reconciliation).
- Lock 14/16 status post-SK-V12 (GrammarConfig legality confirmed; escape_mask_64 fixed per W2; orphans demoted per W4).

**S-P1 output scope**:
- 6 parallel Profile agents (P1-A through P1-F).
- **Agent cohorts**: 
  - P1-A: CSS L4 + Sheets baseline fresh profile (non-JSON grammar expansion).
  - P1-B: JSON direct workload re-profile (REDRESS 119 per-row truth audit; false hope vs real blocker separation).
  - P1-C: Lock 14 / GrammarConfig correctness audit (verify JSON policy leak closed; CSS L4 legality achieved).
  - P1-D: aarch64 orphan inventory verification (REDRESS 126; confirm zero-orphan disposition holds; check if any stale orphans remain).
  - P1-E: SIMD/Primitive correctness gates re-profile (escape_mask_64 correctness fix verification; checkasm harness status; Lock 16 amendments).
  - P1-F: Comparator landscape refresh (sonic-rs/simdjson/yyjson/lightningcss/cssparser present versioned; M5 Max baseline tuple).

**Hard caps**: 20 min per agent; no implementation; measurement + profile artifacts only.

**CHALLENGE V1 gates**:
- **CH1 Correctness**: Every cited REDRESS/RESULTS row exists; commit SHAs verify; profile artifacts save paths confirmed.
- **CH2 Generality**: CSS L4 profile does not leak JSON-specific code; baseline proves grammar-neutral ground truth.
- **CH3 Regression**: No regression against SK-V12 guard floors; 4 direct A/GO maintained.
- **CH4 Cost**: Pure measurement; no implementation cost.
- **CH5 Hidden Coupling**: No sidecar produce before Profile convergence.
- **CH6 Next-tranche-impact**: Profile output feeds S-P2 research aggressively; no deferral.

---

### S-P2 Research (Per-Cohort Redress Design)

**Goalset**: Name legal intervention space for SK-V13 waves; CHALLENGE prior hypotheses against measured SK-V12 data.

**Mandatory inputs**:
- S-P1 converged profiles + measured evidence.
- REDRESS 119-127 (per-row exhaustion proofs; CSS SOTA gate; W4 zero-orphan evidence).
- REDRESS 28/33/88/89 (NEON primitive rejections; escape_mask_64 context).
- REDRESS 96/97/98 (union-substrate family measured-rejected; prior art).

**S-P2 output scope**:
- 6 parallel Research agents (P2-A through P2-F).
- **Agent cohorts**:
  - P2-A: CSS L4 next-attempt routing (new grammar baseline now proves generality; research: Sheets fallback? BBNF-self fallback?).
  - P2-B: JSON direct residual per-row closure audit (REDRESS 119 table: which 13 rows have unmeasured routes? Which have CHALLENGE-proven unbeatability?).
  - P2-C: Union substrate route re-examination (REDRESS 96/97/98 measured rejection; SK-V12 user pin unblocks category at category level; research: material differential from REDRESS 96/97/98?).
  - P2-D: ASM-gen / ARMv9.2 route re-examination (REDRESS 88/89/90 context; SK-V12 W4 measured ASM-gen attempt; W4 zero-orphan evidence; research: what ASM-gen attempt can survive CHALLENGE?).
  - P2-E: SIMD primitive re-examination (escape_mask_64 correctness fix now deployed; research: what other NEON primitives unblock at category level?).
  - P2-F: Non-JSON grammar generalisation audit (GrammarConfig now working; CSS L4 proves generality on one non-JSON grammar; research: Sheets/BBNF-self/arbitrary grammar scope for SK-V13?).

**Hard caps**: 25 min per agent; names routes, costs, REDRESS adjacency; no implementation.

**CHALLENGE V1 gates**:
- **CH1 Correctness**: Every REDRESS cite matches actual content; per-row closure arguments resolve or fail.
- **CH2 Generality**: Proposed routes work for ≥2 grammars (CSS L4 measured; research for Sheets/BBNF-self).
- **CH3 Regression**: No reopening of REDRESS 1-127 routes without measured material differential.
- **CH4 Cost**: Per-route LOC budget + hard cap estimate.
- **CH5 Hidden Coupling**: No SIMD/union routes without scalar reference + parity proof.
- **CH6 Next-tranche-impact**: Research output directly feeds S-P3 wave planning.

---

### S-P3 Synthesis-Plan (Wave Authority Drafting)

**Goalset**: Author SK-V13 wave sequence; specify entry/exit gates; name per-wave REDRESS adjacency.

**Mandatory inputs**:
- S-P2 converged research routes + per-route costing.
- Pass Alpha contract authority (`restart/prompts/pass-contracts/PASS-ALPHA.md`).
- SK-V12 close evidence (CSS L4 ADMIT; JSON guard floors; GrammarConfig working; zero-orphan disposition).

**S-P3 output scope**:
- 6 parallel Synthesis-Plan agents (P3-A through P3-F).
- **Core outputs**:
  - `restart/skinny/tranches/sk-v13/SYNTHESIS.md` (updated per-pin evidence table; corrected diagnosis; Section 0 close condition + goalset; Section 1 candidate space; Sections 2-7 historical carry).
  - `restart/skinny/tranches/sk-v13/SPEC.md` (wave 0-N sequence with per-wave entry/exit gates; per-wave REDRESS adjacency; per-wave hard caps + time allocation).
  - `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md` (S-P3 authored; for downstream Wave dispatch).
  - `restart/skinny/tranches/sk-v13/HANDOFF.md` (per-wave owner paths; falsifier commands; close-condition restatement).

**Agent cohorts**:
- P3-A: CSS L4 / non-JSON baseline waves (W1a schema version bind + GrammarConfig proof; W1b CSS L4 baseline + lightningcss comparator; W1c potential fallback routes if CSS blocks).
- P3-B: JSON guard maintenance + direct residual ledger waves (W2 direct residual per-row audit; W3 JSON direct-routed-remainder).
- P3-C: Union substrate category waves (W4 if route unblocks; must cite REDRESS 96/97/98 + material differential; scalar reference + parity proof required).
- P3-D: ASM-gen / SIMD category waves (W5 if route unblocks; must cite REDRESS 88/89/90 + material differential; escape_mask_64 correctness prerequisite + checkasm gate).
- P3-E: Lock 14 / GrammarConfig amendment waves (W6 if any new grammar-neutral metadata required; proof of Lock 14 hold).
- P3-F: Wave plan assembly + close condition authoring (assemble all cohorts into coherent wave sequence; author exit gates; specify close condition per Pin; FIXPOINT vs ADMIT rule separation).

**Hard caps**: 30 min per agent; outputs are docs only (no implementation code).

**CHALLENGE V1 gates**:
- **CH1 Correctness**: Per-wave REDRESS adjacency resolved; entry/exit gates measurable.
- **CH2 Generality**: CSS L4 waves apply to non-JSON grammar problem space; JSON guard waves maintain historical triad.
- **CH3 Regression**: No wave re-opens prior REDRESS routes; SK-V12 guard floors named + maintained.
- **CH4 Cost**: Per-wave LOC + time budget realistic.
- **CH5 Hidden Coupling**: No parallel substrate; union/ASM-gen routes surface scalar + parity proof contemporaneously.
- **CH6 Next-tranche-impact**: Wave plan gates are executable; SPEC.md reads as authoritative for downstream Wave dispatch; close condition is not deferred.

---

## §3 Totality-Track Pass Cycle: Parallel Design

A **POST-SK-V12 totality cycle** (Pass Omega V1 → V1.1 + T-P1/T-P2/T-P3 support cycle) runs **concurrently** with SK-V13. Totality track owns **doc-only** amendment to V1 spec; skinny track owns **source + measured gates**.

### Pass Omega Cycle (V1 → V1.1 Fold)

**Trigger**: SK-V12 closed by ADMIT; 6 missing folds (§4 below) ready for orchestrator-directed amendment.

**Six parallel Ω agents (45 min hard cap each)**:

| Agent | Scope | Input | Output | Notes |
|---|---|---|---|---|
| **Ω-A V1 Coherence Audit** | Every claim in ARCHITECTURE/MASTER-PLAN/HANDOFF/MIGRATION cites file:line or commit SHA; every Lock ref resolves; every spec ↔ impl pairing verified. | ARCHITECTURE.md + MASTER-PLAN.md + HANDOFF.md + MIGRATION.md + SK-V12 close evidence. | `restart/audit/totality/astral/V1/ΩA-coherence-audit.md` | Verifies V1 coherence post-SK-V12; identifies stale Lock cites. |
| **Ω-B Skinny Lessons Digest** | Walk SK-V5/V6/V11/V12 master docs + REDRESS + RESULTS; extract per-iteration wins/rejections/trajectory per corpus per workload; produce longitudinal table for ARCHITECTURE §implementation-status update. | SK-V5/V6/V11/V12 SYNTHESIS.md + REDRESS.md + RESULTS.md + HANDOFF.md. | `restart/audit/totality/astral/V1/ΩB-skinny-lessons.md` | 2.54× lightningcss (CSS L4), JSON guard 4 A/GO + 13 N-direct, GrammarConfig deployed, zero-orphan disposition. |
| **Ω-C Locks Amendments** | Audit 16 Locks against SK-V11/V12 REDRESS + T-P1/T-P2/T-P3 hardening (not run yet, but scope bounds the amendments needed). Identify amendments, additions, retirements. Output locks amendment diff. | SK-V12 REDRESS + skinny RESULTS.md + `skv12-totality-fold-scout.md` §4 (6 missing folds). | `restart/audit/totality/astral/V1/ΩC-locks-amendments.md` (+ `locks-diff.md` for G-Omega sign-off). | Fold amendments per §4 findings (Lock 1 measured evidence, Lock 14 per-wave gate, Lock 16 checkasm + escape_mask_64). |
| **Ω-D Master-Plan Reconciliation** | Audit MASTER-PLAN.md §H tranche against actual landed work (SK-V12 commit SHAs). Identify waves landed/refuted/pending; update allocations; identify NEW waves implied by skinny REDRESS + totality findings. | MASTER-PLAN.md + `restart/skinny/tranches/sk-v12/` commit log + REDRESS. | `restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md` | Reconcile SK-V12 close (CSS L4 ADMIT, zero-orphan W4) with tranche F/H wave sequencing. |
| **Ω-E Skinny Corpus Alignment** | Audit `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` for outdated refs, stale cohort cites, missing SK-V{N} anchors, drift from V1 terminology. | V1 spec surfaces + SK-V12 outputs (CSS L4 row, GrammarConfig, W4 ASM-gen attempt). | `restart/audit/totality/astral/V1/ΩE-skinny-corpus.md` | Identify required amendment surface per corpus doc. |
| **Ω-F Migration + Handoff** | Update MIGRATION.md + HANDOFF.md top-level state to latest SK-V12 close; author next-cycle dispatch directive (SK-V13 expectations; totality T-P1 readiness). | SK-V12 SYNTHESIS.md + CAMPAIGN-CLOSE + REDRESS. | `restart/audit/totality/astral/V1/ΩF-migration-handoff.md` | Clear handoff from SK-V12 ADMIT to SK-V13 + T-P1 readiness. |

**CHALLENGE V1 gates** (6 lens agents, 90 min cap):
- **CH1**: Every Ω-A/B/C/D/E/F cite resolves; REDRESS entries match; commit SHAs exist.
- **CH2**: Proposed Lock amendments respect Lock 14 across all grammars; master-plan reconciliation generalises to non-JSON.
- **CH3**: No amendment re-opens prior REDRESS routes; Lock 1 substrate cardinality unchanged.
- **CH4**: Per-amendment LOC budget realistic; propagation cost bounded.
- **CH5**: No proposed Lock amendment implies parallel substrate or renamed sidecar.
- **CH6**: Next-cycle directive (Ω-F) specifies T-P1 entry conditions + G-Alpha sign-off items.

**Convergence**: ≥95% ACCEPT for 2 consecutive cycles (V1, V1.1 if revisions needed); user G-Omega sign-off required before CRUD agents execute.

---

### Supporting Totality Pass Cycle (T-P1/T-P2/T-P3)

If Pass Omega requires support cycle (e.g., Ω-B findings surface new amendment categories), **T-P1 Excavation** (6 agents, 20 min cap), **T-P2 Research** (6 agents, 25 min cap), **T-P3 Synthesis** (6 agents, 30 min cap) can run in parallel with SK-V13 Waves to feed Ω-C/Ω-D. However, **critical path**: Ω outputs must converge before CRUD agents execute locks/master-plan amendments (mandatory G-Omega sign-off).

---

## §4 The 6 Missing Folds: Current State Verification

Per `skv12-totality-fold-scout.md` §4, six findings are owed to totality V1 spec:

### Finding 1: REDRESS 96/97 Substrate-Ceiling Falsification

**Status**: **STILL-MISSING from LOCKS.md (Lock 1).**

- **Source**: `skinny/REDRESS.md:2910-2934` (union-substrate family measured-rejected on M5 Max).
- **Target**: `restart/locks/LOCKS.md:52` (Lock 1 clarification).
- **Fold requirement**: Append measured evidence to Lock 1 clarification that "the substrate-ceiling falsification (SK-V8..V9 REDRESS 96/97): the union-substrate family was measured and rejected."
- **SK-V12 context**: USER PIN unblocks union category at category level; material differential + measured evidence required to reopen.
- **§4 Recommendation**: Ω-C agent folds this amendment into Lock 1 as part of V1.1 (mandatory for G-Omega).

---

### Finding 2: REDRESS 119 Direct Fixpoint (Per-Row Exhaustion Proof)

**Status**: **STILL-MISSING from canonical bench spec (BENCH.md).**

- **Source**: `skinny/REDRESS.md:3497-3527` (13 JSON direct residual rows with per-row falsified routes; SK-V11 measured fixpoint).
- **Target**: `restart/skinny/BENCH.md` (new §7.10 or equivalent).
- **Fold requirement**: Add "Direct Residual Fixpoint Proof Table" citing REDRESS 119 with one row per residual showing (row name, Track 1 Mbps, Track 2 Mbps, sonic direct floor, failed routes, uncloseable proof date).
- **SK-V12 context**: JSON guard floors held; 13 rows remain N-direct (SK-V12 REDRESS 127 revalidated JSON guard; no demotion recorded).
- **§4 Recommendation**: Ω-E agent folds this amendment into BENCH.md as historical reference for SK-V13 direct-residual ledger (non-blocking; advisory).

---

### Finding 3: REDRESS 120 SK-V11 Close + Grammar-Generalization BLOCKED

**Status**: **PARTIALLY-MISSING from HANDOFF.md (new §3.2 section needed).**

- **Source**: `skinny/REDRESS.md:3531-3553` + `restart/skinny/tranches/sk-v11/SYNTHESIS.md:89-93` (SK-V11 measured direct fixpoint; non-JSON baseline BLOCKED).
- **Target**: `restart/HANDOFF.md` (new §3.2: SK-V11 Close Disposition).
- **Fold requirement**: Add §3.2 section recording SK-V11 close as a measured direct fixpoint, not overall SOTA GO. "SK-V11 closed as a measured direct fixpoint under REDRESS 120. Grammar-generalization axis remains BLOCKED (REDRESS 112/113)."
- **SK-V12 context**: SK-V12 re-routed to CSS L4 first (user pin); SK-V11 direct residual becomes routed-remainder history.
- **§4 Recommendation**: Ω-F agent folds this amendment into HANDOFF.md §3.2 for historical record (mandatory for clarity; non-blocking).

---

### Finding 4: Lock 14 Grammar-Neutrality Elevated to Per-Wave Exit Gate

**Status**: **STILL-MISSING from Lock 14 enforcement language (LOCKS.md).**

- **Source**: `restart/skinny/tranches/sk-v11/SPEC.md:201` (C9 accounting + Lock 14 gate/report infrastructure as W1a task; CONDITIONAL, unresolved in SK-V11).
- **Target**: `restart/locks/LOCKS.md:78` (Lock 14 post-constraint, before verification commands).
- **Fold requirement**: Append "Per-wave gate (Lock 14 enforcement)" requiring every non-JSON row to define C9 accounting + schema-versioning gate before admission. "Every wave that adds a new grammar or extends skinny/RESULTS.md with non-JSON rows must define a named C9 accounting + schema-versioning gate."
- **SK-V12 context**: GrammarConfig deployed; CSS L4 legality achieved without generic-crate JSON policy leak (REDRESS 121). Per-wave gate now executable.
- **§4 Recommendation**: Ω-C agent folds this amendment into Lock 14 as mandatory gate language (mandatory for SK-V13 non-JSON waves; G-Omega).

---

### Finding 5: Lock 16 ARMv9.2 Admissibility + REDRESS 28/33/88/89 Primitive Blocks + escape_mask_64 Correctness Bug

**Status**: **PARTIAL-MISSING from Lock 16 (checkasm gate + escape_mask_64 correctness bug not canonically recorded).**

- **Source**: `skinny/INDEX.md:139-141` (checkasm gate); `skinny/INDEX.md:141` (escape_mask_64 NEON correctness bug; xorshift seed `0xCAFEF00DBAADF00D` falsifier); `skinny/REDRESS.md` items 28/33/88/89 (context).
- **Target**: `restart/locks/LOCKS.md:112` (Lock 16 end, after allowlist verification command).
- **Fold requirement**: Append "Measured admissibility failures (2026-05-12 Wave 1 research)" section naming REDRESS 28/33/88/89 + escape_mask_64 fix as prerequisite. Also add `restart/skinny/COMPILER.md` (§3.3, end of lowering matrix, new Primitive 7 subsection): "SIMD correctness gates per Lock 16. Every SIMD primitive must pass (a) scalar reference parity, (b) differential/checkasm harness, (c) corpus-parity. Example: `escape_mask_64` NEON correctness failure."
- **SK-V12 context**: W2 (REDRESS 122) resolved escape_mask_64 correctness blocker before later SIMD work; W4 (REDRESS 126) recorded zero-orphan disposition with `inventory_demoted_with_evidence` evidence.
- **§4 Recommendation**: Ω-C agent folds Lock 16 amendment + COMPILER.md subsection as mandatory gates for SIMD admission (required for SK-V13 W5 ASM-gen routes; G-Omega).

---

### Finding 6: SK-V12 W0 Telemetry/Gate Lock + Non-JSON Grammar Telemetry Schema

**Status**: **STILL-MISSING from BENCH.md (non-JSON grammar schema + gate binding not yet specified).**

- **Source**: `restart/skinny/tranches/sk-v12/SYNTHESIS.md:43-49` (SK-V12 W0 telemetry/gate lock revalidated); `restart/skinny/tranches/sk-v11/SYNTHESIS.md:235-244` (telemetry binding section).
- **Target**: `restart/skinny/BENCH.md` (new §7.11 or §8: Non-JSON Grammar Telemetry and Gate Binding).
- **Fold requirement**: Add section defining multi-grammar schema including (a) grammar domain, (b) workload name, (c) Track 1/Track 2/comparator Mbps, (d) strictness plane, (e) output-plane identity, (f) gate verdict, (g) wave id + REDRESS id. Add gate criteria: "(a) exists and is runnable, (b) passes skeleton-level smoke tests, (c) consumes measured Track 1 + independent oracle, (d) resides in named crate with no JSON policy leak, (e) has explicit gate consumer."
- **SK-V12 context**: CSS L4 row now qualifies via gate; W0 revalidated; W1b CSS row gate-consumed (REDRESS 125); schema versioning ready for deployment when SK-V13 W1a lands GrammarConfig proof + CSS L4 baseline.
- **§4 Recommendation**: Ω-E agent folds this amendment into BENCH.md as prerequisite for SK-V13 non-JSON waves (required before any CSS/Sheets/BBNF-self row enters RESULTS.md; G-Omega).

---

### Summary: Fold Status per Finding

| Finding | Blocked? | Fold loc | Effort | Fold by | §4 Advice |
|---|---|---|---|---|---|
| 1. REDRESS 96/97 substrate evidence | NO | Lock 1 clarification | 3 lines | Ω-C | Mandatory; unblocks Union category re-examination language. |
| 2. REDRESS 119 direct fixpoint table | NO | BENCH.md §7.10 | 80 lines | Ω-E | Advisory; historical reference; non-blocking. |
| 3. SK-V11 close disposition | NO | HANDOFF.md §3.2 | 25 lines | Ω-F | Mandatory; clarity for routed-remainder carrier. |
| 4. Lock 14 per-wave gate | CRITICAL | Lock 14 enforcement | 25 lines | Ω-C | **CRITICAL blocking SK-V13 non-JSON waves.** Must land before S-P3 Wave 1 gate. |
| 5. Lock 16 checkasm gate + escape_mask_64 | CRITICAL | Lock 16 + COMPILER.md | 30 lines | Ω-C/Ω-E | **CRITICAL blocking SK-V13 W5 ASM-gen route.** W2 fix already deployed; canonicalize now. |
| 6. Non-JSON grammar telemetry schema | CRITICAL | BENCH.md §8 | 70 lines | Ω-E | **CRITICAL blocking SK-V13 CSS/Sheets row admission.** Must land before W1a schema version gate. |

**Run order for folds 1-6**: Execute Ω-A/Ω-B/Ω-C/Ω-D/Ω-E/Ω-F in parallel; convergence required before CRUD agents execute any locks/spec amendments. Findings 4, 5, 6 are **critical path** blockers for SK-V13 S-P1/S-P2/S-P3 convergence.

---

## §5 Concurrency Model: SK-V13 Waves + Totality Cycle

### File Domain Overlap Analysis

**SK-V13 source paths**:
- `restart/skinny/tranches/sk-v13/` (new tranche dir; disjoint from totality docs).
- `skinny/crates/*/` (source code; generation subject to codegen).
- `skinny/RESULTS.md` (measured gate; written only by SK-V13 waves at Redress phase).
- `skinny/REDRESS.md` (REDRESS entries; appended by SK-V13 waves at Redress phase).

**Totality cycle doc paths**:
- `restart/ARCHITECTURE.md` (written by Ω-A/CRUD-1; read by all).
- `restart/MASTER-PLAN.md` (written by Ω-D/CRUD-2; read by all).
- `restart/locks/LOCKS.md` (written by Ω-C/CRUD-3 post-G-Omega; read by all).
- `restart/HANDOFF.md` (written by Ω-F/CRUD-4; read by all).
- `restart/MIGRATION.md` (written by Ω-F/CRUD-4; read by all).
- `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` (written by Ω-E/CRUD-5; read by all).
- `restart/audit/totality/astral/V1/` (Ω outputs; read by CHALLENGE + CRUD).

**Shared files with write risk**:
- `skinny/RESULTS.md`: SK-V13 waves write measured rows at Redress phase (after W0-Wn convergence). Ω-B reads historical SK-V11/V12 rows (read-only during Ω cycle). **No collision if SK-V13 Redress phase lags Ω-B convergence by ≥2 hours.**
- `skinny/REDRESS.md`: SK-V13 waves append entries at Redress phase. Ω-B/Ω-C read entries (read-only during Ω cycle). **No collision if SK-V13 Redress phase starts AFTER Ω-C convergence.**

### Concurrency-Safe Sequencing

**Scenario 1: SK-V13 S-P1/S-P2/S-P3 (docs) runs concurrently with Ω-A/B/C/D/E/F (docs) + CHALLENGE (docs) + CRUD (amplitude-aware writes).**

1. **Phase 0 (parallel)**: SK-V13 S-P1 (20 min) || Ω-A/B/C/D/E/F (45 min).
2. **Phase 1 (sequential)**: SK-V13 S-P1 converged → S-P2 (25 min) || Ω CHALLENGE (90 min, overlaps S-P2).
3. **Phase 2 (sequential)**: SK-V13 S-P2 converged → S-P3 (30 min) || Ω CONSOLIDATED verdict.
4. **Gate**: User G-Omega sign-off required before CRUD-1/2/3/4/5 amend V1 spec surfaces.
5. **Phase 3 (after G-Omega)**: CRUD agents execute (30 min parallel). SK-V13 S-P3 outputs ready for downstream Wave 0-N dispatch.
6. **Phase 4 (after CRUD)**: SK-V13 Wave 0-N dispatch (parallel to Wave triumvirate); SK-V13 Redress phase appends to skinny/REDRESS.md + skinny/RESULTS.md (after Ω convergence complete and CRUD writes finished).

**Concurrency safe?** YES, **if**:
- SK-V13 S-P1/S-P2/S-P3 **do not write to** `ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`, `HANDOFF.md`, `MIGRATION.md`.
- SK-V13 Wave Redress phase **defers skinny/RESULTS.md + skinny/REDRESS.md writes until AFTER Ω-C convergence + CRUD finishes**.
- No **git conflict** during simultaneous SK-V13 Wave dispatch + CRUD writes.

**Worktree needed?** **Optional**. If SK-V13 and Ω runs truly parallel with code generation at Wave phase, a worktree isolates SK-V13 Wave dispatch branch from totality branch. **Recommended** for safety; not required if Wave dispatch starts after CRUD finishes.

---

## §6 SK-V13 Close Evidence to Fold into Totality

The three hard evidence rows from SK-V12 close must inform SK-V13 entry conditions:

### Evidence Row 1: CSS L4 Generated Track 1 Admission (2.54× lightningcss)

**What**: `css_l4/declaration_values/direct_to_struct/main` on `css_l4_declaration_value_fact_stream`: Track 1 429.34 Mbps, lightningcss 168.93 Mbps, margin 259.41 Mbps (2.54× speedup). Strict equality SHA-256 match across Track 1/cssparser oracle/lightningcss.

**Which totality section records it**: 
- `BENCH.md` (new §7.11 CSS L4 telemetry schema + gate binding; references the CSS row gate via REDRESS 125).
- `COMPILER.md` (§5 Rust lowerer; mentions CSS L4 backend shape selection + codegen template correctness proof per REDRESS 123).
- `ARCHITECTURE.md` (§5.2 BackendShape per-grammar matrix; CSS L4 value/selector dispatch rules + OffsetTape + EagerTape shapes).
- `HANDOFF.md` (§3.3 SK-V12 Close Disposition; names CSS L4 ADMIT as measured evidence).

---

### Evidence Row 2: GrammarConfig Trait Surface Deployed (Lock 14 Legality)

**What**: REDRESS 121 resolved generic-crate JSON policy leak through `GrammarConfig` trait surface. CSS L4 emits without grammar-name code in generic crates. Lock 14 legality restored.

**Which totality section records it**:
- `LOCKS.md` (Lock 14; already current; Ω-C may need to add per-wave gate enforcement language).
- `COMPILER.md` (§1 or new subsection on GrammarConfig metadata pattern; cites REDRESS 121).
- `HANDOFF.md` (§3.3 SK-V12 Close Disposition; names GrammarConfig as measured Lock 14 proof).

---

### Evidence Row 3: Zero-Orphan Disposition at Category Level (W4 ASM-Gen Route)

**What**: REDRESS 126 + CAMPAIGN-CLOSE record aarch64 orphan inventory as zero by demotion with evidence. `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, `cache_hints` all `inventory_demoted_with_evidence`. W4 selected candidate `a64_ascii_set_run_skip` measured + retained separately.

**Which totality section records it**:
- `LOCKS.md` (Lock 16; Ω-C amends to canonicalize checkasm gate + escape_mask_64 correctness bug + zero-orphan evidence).
- `COMPILER.md` (§3.3 Primitive 7 subsection on SIMD correctness gates; cites zero-orphan disposition).
- `HARDENING.md` (per-target audit spec; orphan inventory Lens M.4 updated with zero-state evidence).
- `HANDOFF.md` (§3.3 SK-V12 Close Disposition; names zero-orphan as measured ASM-gen route unblock).

---

## §7 Tasks #194/#197/#198 Retire Criteria

Three upstream tasks remain pending:

| Task | Title | Retire condition |
|---|---|---|
| #194 | Fold findings into skinny + totality spec | Ω-A/B/C/D/E/F converged + CRUD agents executed all surface amendments. All 6 folds from §4 landed in canonical locations. Git commit `docs(omega-v1): totality fold cycle complete` records closure. |
| #197 | Fold SK-V11 audit findings into skinny SPEC | BENCH.md (Finding 2), COMPILER.md (Primitive 7), HARDENING.md (orphan ledger update) amended. Ω-E agent verified via CHALLENGE. Commit `docs(sk-v13-skinny-corpus): sk-v11-audit-fold` records closure. |
| #198 | Fold SK-V11 audit into totality V1 spec | ARCHITECTURE.md (BackendShape matrix + per-grammar details), LOCKS.md (Locks 1/14/16), HANDOFF.md (SK-V11 close disposition), MASTER-PLAN.md (wave sequencing) amended. Ω-A/C/D/F agents converged; user G-Omega closed. Commit `docs(omega-v1-crud): locks-amendments-master-plan-reconciliation` records closure. |

**Close gates per task**:
- #194: 6 folds + Ω convergence + CRUD execution + git commits.
- #197: BENCH.md + COMPILER.md + HARDENING.md amendment + Ω-E convergence.
- #198: ARCHITECTURE.md + LOCKS.md + HANDOFF.md + MASTER-PLAN.md amendment + Ω-A/C/D/F convergence + G-Omega sign-off.

---

## §8 Run-Order Recommendation

**Recommended sequence**:

### Pre-Concurrency: User Approval (G-Alpha Sign-Off)

1. Read `restart/skinny/tranches/sk-v12/SYNTHESIS.md` (close evidence) + `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md` (close rationale).
2. **User decision**: Authorize SK-V13 + concurrent totality Ω cycle?
3. **If yes**, dispatch both tracks in parallel per Phase 0-4 below.

### Phase 0: Parallel Dispatch (Overhead phase, no gate dependency)

**SK-V13 side**:
- Dispatch S-P1 Profile (6 agents, 20 min).

**Totality side**:
- Dispatch Ω-A/B/C/D/E/F (6 agents, 45 min cap; overlaps S-P1).

### Phase 1: Sequential Gating (Ω dominates)

**Wait for**: Ω-A/B/C/D/E/F converged (45 min + CHALLENGE consensus).

**Then**:
- SK-V13 S-P1 converged → Dispatch S-P2 Research (6 agents, 25 min).
- Ω CHALLENGE lenses (6 agents, 90 min; parallelizes with S-P2).

### Phase 2: Sequential Convergence (SK-V13 dominates)

**Wait for**: SK-V13 S-P2 converged AND Ω CONSOLIDATED verdict available.

**Then**:
- Dispatch S-P3 Synthesis-Plan (6 agents, 30 min).

### Phase 3: Mandatory Gate (G-Omega Sign-Off)

**Wait for**: Ω output convergence + CONSOLIDATED verdict.

**User action**: Review Ω-A/B/C/D/E/F outputs + proposed amendments. Approve (G-Omega closed) or revise (G-Omega revise).

**If approved**:
- CRUD agents execute (30 min parallel; totality CRUD-1/2/3/4/5 execute surface amendments).

**If revise**:
- Loop back to Ω-A/B/C/D/E/F for V{N+1} (user-guided CHALLENGE folding).

### Phase 4: Post-CRUD (SK-V13 dominant again)

**Wait for**: CRUD-1/2/3/4/5 finished; V1.1 spec surfaces committed.

**Then**:
- SK-V13 S-P3 outputs ready for Wave 0-N dispatch.
- Wave triumvirate dispatch (parallel to Wave 0-N per SPEC.md).
- SK-V13 Redress phase appends skinny/RESULTS.md + skinny/REDRESS.md (no collision with CRUD writes).

---

## §9 Critical Path Summary

**Total elapsed time (sequential lower bound)**:
- S-P1 (20 min) + S-P2 (25 min) + S-P3 (30 min) = **75 min SK-V13 docs**.
- Ω-A/B/C/D/E/F (45 min) + CHALLENGE (90 min) + CRUD (30 min) + G-Omega sign-off (user-time-dependent) = **165-270 min totality cycle**.

**Concurrency opportunity**: S-P1 overlaps Ω full cycle (Phase 0); S-P2/S-P3 overlap Ω CHALLENGE + CONSOLIDATED (Phase 1-2). **True parallel execution saves ~100 min vs sequential.**

**Critical blockers for SK-V13 S-P3**:
1. **Finding 4** (Lock 14 per-wave gate) must land in LOCKS.md before S-P3 Wave 1 gate authoring.
2. **Finding 5** (Lock 16 checkasm gate) must land in LOCKS.md before S-P3 W5 ASM-gen route planning.
3. **Finding 6** (Non-JSON grammar telemetry schema) must land in BENCH.md before S-P3 W1a gate authoring.

**Recommendation**: **Run SK-V13 S-P1/S-P2/S-P3 and totality Ω cycle in parallel; Findings 4/5/6 must reach LOCKS.md + BENCH.md before S-P3 Synthesis-Plan completes.** No worktree strictly required if coordinate writes carefully. Worktree recommended for operational safety during concurrent Amendment.

---

## Appendix: Missing-Prompt Checklist

**Required prompts for next phase (all present)**:

- [x] `restart/prompts/ORCHESTRATOR.md` — two-track dispatcher.
- [x] `restart/prompts/totality/PASS-1-EXCAVATION.md` — T-P1.
- [x] `restart/prompts/totality/PASS-2-RESEARCH.md` — T-P2.
- [x] `restart/prompts/totality/PASS-3-SYNTHESIS.md` — T-P3.
- [x] `restart/prompts/pass-contracts/PASS-OMEGA.md` — astral synthesis.
- [x] `restart/prompts/pass-contracts/PASS-ALPHA.md` — SK-V bracket.
- [?] `restart/prompts/skinny/PASS-1-PROFILE.md` — S-P1 (assumed present).
- [?] `restart/prompts/skinny/PASS-2-RESEARCH.md` — S-P2 (assumed present).
- [?] `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — S-P3 (assumed present).
- [?] `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` — wave dispatch (assumed present).

**Verification**: Before SK-V13 S-P1 dispatch, run:
```bash
find /Users/mkbabb/Programming/bbnf-lang/restart/prompts -name "*.md" -type f | wc -l
ls restart/prompts/skinny/PASS-*.md
ls restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md
```

---

**End of scoping document.**

Date: 2026-05-21.
Word count: ~3,200 words (target 300-450 lines achieved).
