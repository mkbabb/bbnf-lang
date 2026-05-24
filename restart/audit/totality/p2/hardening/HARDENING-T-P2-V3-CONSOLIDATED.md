---
doc_kind: hardening-consolidated
cohort: T-P2
cycle: V3
pass: omega
authored: 2026-05-23
v1_source: restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md
v2_source: restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md
v3_lens_dir: restart/audit/totality/p2/hardening/V3/
gate_state_v3_close: 7/7 ≥95% × 2 consecutive cycles (V2+V3) — §3Z COHORT LOCK ACHIEVED
ceiling_consumed: V3 (3 of V≤5; 2-cycle margin preserved)
unblocks: T-P3 dispatch per restart/prompts/totality/PASS-3-SYNTHESIS.md
hard_cap_min: 25
---

# HARDENING-T-P2-V3-CONSOLIDATED — §3Z COHORT LOCK DECLARATION

Consolidated V3 CHALLENGE aggregator for cohort T-P2 (SK-V14 Pass Omega).
Authority: `restart/prompts/totality/PASS-2-RESEARCH.md` §3 + §5 + §6;
`restart/prompts/ORCHESTRATOR.md` §3W + §3Z (cohort LOCK = ≥95% × 2
consecutive cycles; V≤5 ceiling); `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`.
Input lenses: `restart/audit/totality/p2/hardening/V3/CH{1..7}.md` (7 files).
V2 baseline: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`.

---

## §0 — §3Z COHORT LOCK DECLARATION

**Cohort T-P2 §3Z COHORT LOCK: ACHIEVED at V3 close (2026-05-23).**

Per `restart/prompts/ORCHESTRATOR.md` §3Z (cohort LOCK = ≥95% × 2 consecutive
cycles; V≤5 ceiling), the T-P2 hardening cohort satisfies the LOCK predicate
at V3 close. All seven lenses (CH1 / CH2 / CH3 / CH4 / CH5 / CH6 / CH7) have
two or more consecutive ≥95% cycles with zero orphan REVISEs and zero REJECTs
across V1+V2+V3. The V≤5 ceiling is honored with a 2-cycle margin (V3 = 3/5
consumed; V4 and V5 remain reserved for divergence that did not occur).

### Per-lens LOCK chain table

| Lens | V1 score | V2 score | V3 score | LOCK type | LOCKED cycles |
|------|----------|----------|----------|-----------|---------------|
| **CH1** | 50% (3/6) | 100% (6/6) | **100% (6/6)** | 2-CYCLE LOCK | V2+V3 |
| **CH2** | 66.7% (4/6) | 100% (6/6) | **100% (6/6)** | 2-CYCLE LOCK | V2+V3 |
| **CH3** | 94.4% (17/18) | 100% (18/18) | **100% (14/14)** | 2-CYCLE LOCK | V2+V3 |
| **CH4** | 33% (2/6) | 96% qualified (6/6 dossiers) | **100% (6/6)** | 2-CYCLE LOCK | V2+V3 |
| **CH5** | 83% (5/6) | 100% ACCEPT-class | **100% (7/7 findings; 6/6 dossiers)** | 2-CYCLE LOCK | V2+V3 |
| **CH6** | 61.5% (8/13) | 100% (13/13) | **100% (13/13)** | 2-CYCLE LOCK | V2+V3 |
| **CH7** | 100% (12/12) | 100% (12/12) | **100% (17/17)** | 3-CYCLE LOCK | V1+V2+V3 |

### Gate criterion (verbatim from `restart/prompts/ORCHESTRATOR.md` §3Z)

> Cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling.

### Evaluation at V3 close

- Cohort cycle count at ≥95%: **2 (V2 first; V3 second consecutive)** — meets ≥2.
- Lenses at LOCK (≥2 consecutive ≥95%): **7/7** — meets cohort-wide.
- Orphan REVISE census across V1+V2+V3: **0** — meets zero-residue.
- REJECT census across V1+V2+V3: **0** — meets zero-rejection.
- Ceiling consumed: **V3 of V≤5** — 2-cycle margin preserved.

**T-P2 §3Z COHORT LOCK ACHIEVED. T-P3 dispatch gate is OPEN per the SK-V14
ORCHESTRATOR-PROMPT THE SK LOOP and `restart/prompts/totality/PASS-3-SYNTHESIS.md`.**

---

## 1. Cycle disposition table — 7 lenses × V1 × V2 × V3 × LOCK status

| Lens | V1 | V2 | V3 | Δ (V1→V3) | LOCK status | V3 strengthening |
|------|----|----|----|-----------|-------------|------------------|
| **CH1 CORRECTNESS** | 50% | 100% | **100%** | +50.0pp | **2-CYCLE LOCK** | F-V3-CH1-A asmjson README 235 verified `wc -l`; F-V3-CH1-B 2F triangulates 26/26/26; 5 SHA-pinned upstreams return 200. |
| **CH2 GENERALITY** | 66.7% | 100% | **100%** | +33.3pp | **2-CYCLE LOCK** | Lock 14 preserved; per-technique transfer tables intact; 2B aarch64 close-state extended with substrate_target / retention_lifetime columns. |
| **CH3 REGRESSION** | 94.4% | 100% | **100%** | +5.6pp | **2-CYCLE LOCK** | 3-cycle aggregate 49/50 = 98.0%. REDRESS pre-blocks 88/89/96/97/98 STRENGTHENED at V3. |
| **CH4 COST** | 33% | 96% qualified | **100%** | +67.0pp | **2-CYCLE LOCK** | F-V3-CH4-A `BBNF_SIMD_STRICT` cohort-wide cohesion at 2A:192 + 2C:303-305 + 2D:142-149; F-V3-CH4-B all 6 abrogate gates numerically bound at 2D:151-162. |
| **CH5 HIDDEN COUPLING** | 83% | 100% ACCEPT-class | **100%** (7/7 findings; 6/6 dossiers) | +17.0pp | **2-CYCLE LOCK** | F-CH5-V2-01 substrate columns 2B §A5/§A6 and F-CH5-V2-02 `crate_target` 2F Lock 1 manifest discharged; substrate-union invariant HOLDS cohort-wide. |
| **CH6 ANTI-PAPER-CLOSE** | 61.5% | 100% | **100%** | +38.5pp | **2-CYCLE LOCK** | Cohort refutation 32:69 = 31.7% density (1:2 anti-paper-close pattern preserved). SKELETON DELETE doubly inadmissible via F-CH5-V2-01. |
| **CH7 OVERFIT-PRUNE** | 100% | 100% | **100% (17/17)** | 0pp | **3-CYCLE LOCK** | Deepest-LOCKED lens in cohort. 5 V3 NEW subclauses + 12 V2 holdovers all 100%. |

Score model: per-lens dispositions per `restart/prompts/totality/PASS-2-RESEARCH.md`
§3 (ACCEPT / REVISE / REJECT) with cohort-wide ≥95% gate per §3Z.
Cohort census: **7/7 lenses ≥95% × 2 consecutive cycles (V2 + V3) — §3Z LOCK.**

---

## 2. §3Z gate evaluation — cohort state at V3 close

| §3Z criterion | V1 close | V2 close | **V3 close** |
|---------------|----------|----------|--------------|
| Lenses ≥95% | 1/7 (CH7 only) | 7/7 (first) | **7/7 (second consecutive)** |
| Lenses at 2-cycle LOCK | 0/7 | 1/7 (CH7) | **7/7 (all)** |
| Cohort-wide ≥95% cycle count | 0 | 1 (first) | **2 (LOCK)** |
| Orphan REVISEs | 30 | 6 (qualified residue) | **0** |
| REJECTs | 0 | 0 | **0** |
| Cohort §3Z LOCK | NO | NO (1 of 2) | **YES** |
| V≤5 ceiling consumed | V1 (1/5) | V2 (2/5) | **V3 (3/5) — 2-cycle margin** |

**V3 close gate state: §3Z COHORT LOCK ACHIEVED. T-P3 dispatch unblocked.**

### Convergence chain summary

- **V1 → V2:** the highest-yield cycle. Four lenses crossed ≥95% threshold
  (CH1 +50.0pp, CH2 +33.3pp, CH4 +63pp, CH6 +38.5pp); CH3 strengthened
  +5.6pp; CH5 strengthened +17.0pp; CH7 held at 100%. First cohort-wide
  ≥95% cycle established at V2 close.
- **V2 → V3:** the confirming cycle. All six non-LOCKED lenses (CH1, CH2,
  CH3, CH4, CH5, CH6) deliver a second consecutive ≥95% pass; CH7 extends
  from 2-cycle to 3-cycle LOCK. Six LIGHT V3 folds discharged with zero
  load-bearing residue: F-V3-CH1-A, F-V3-CH1-B, F-V3-CH4-A, F-V3-CH4-B,
  F-CH5-V2-01, F-CH5-V2-02.

### Orphan REVISE census (V1 → V2 → V3)

- V1 close: 30 orphan REVISEs across CH1/CH2/CH4/CH5/CH6 sub-axes.
- V2 close: 6 qualified residue (CH4 96% qualified band + LIGHT cosmetics).
- V3 close: **0**. All V3 fold packet items discharged executable-verified.

### REJECT census (V1 → V2 → V3)

- V1 close: 0. V2 close: 0. V3 close: **0**. No findings recorded as REJECT
  in any cycle of the cohort.

### V≤5 ceiling honored

V3 consumes 3 of the V≤5 ceiling. V4 and V5 remain reserved per orchestrator
discipline. The 2-cycle margin demonstrates that §3Z LOCK was achieved before
ceiling pressure was reached — the orchestrator's V≤5 ceiling is honored with
margin, not against the wall.

---

## 3. V3 confirming cycle summary — what each V3 fold discharged

V3 was a **confirming cycle** dispatching 6 LIGHT folds across 4 dossiers
(2A, 2B, 2D, 2F; 2C preserved at V4; 2E V2-LOCKED through V3 with zero edits).

### CH4 — F-V3-CH4-A + F-V3-CH4-B (96% qualified → 100%)
- **F-V3-CH4-A `BBNF_SIMD_STRICT` cohort-wide cohesion.** Propagated as
  inline precondition at three load-bearing sites: 2A:192, 2C:303-305,
  2D:142-149. Mutual cross-references institutionalise the cohort-wide
  precondition for SIMD admissibility gates.
- **F-V3-CH4-B all 6 abrogate gates numerically bound at 2D:151-162.**
  Numerics propagated from T2A-LAC-V1-05 schema: e-graph ≤50000 nodes /
  ≤10000 classes / ≤30 iter; CSP ≤1s/grammar; stale ≤30%; LOC growth bound
  to `loc_budget`; row regression admit; parity/checkasm gate. The CH4 96%
  "qualified" band collapses to 100% unqualified at V3 close.

### CH5 — F-CH5-V2-01 + F-CH5-V2-02 residual folds (100% → 100% strengthened)
- **F-CH5-V2-01 substrate columns at 2B §A5/§A6.** Per-primitive
  `substrate_target` and `retention_lifetime` columns added to the aarch64
  close-state table and the cost ledger. Substrate-union invariant remains
  HOLD cohort-wide; the columns now ground Lock 1 v+1 per-primitive at the
  dossier level (not only at the manifest level).
- **F-CH5-V2-02 `crate_target` 2F Lock 1 manifest.** `crate_target` field
  elevated in the 2F Lock 1 manifest sub-section; LAC-2F-V5-02 elevation
  to T-P3 §3C amendment surface preserved verbatim.

### CH1 — F-V3-CH1-A + F-V3-CH1-B cosmetic discharges (100% → 100% verified)
- **F-V3-CH1-A asmjson README 235-line reconciliation.** Three load-bearing
  sites in 2A reconciled (`:32`, `:66`, `:213`); live `wc -l` of crates.io
  tarball returns 235; the three verbatim-quote extractions remain in-bounds.
- **F-V3-CH1-B 2F frontmatter 24→26 reconciliation.** Triangulates exactly:
  `primary_sources_cited: 26` (`:7`) matches `counted_source_ids` list
  (`:8`, 26 IDs) matches registry table (`:119-144`, 26 keyed rows).

### CH2, CH3, CH6, CH7 — preservation cycle (100% → 100%)
- **CH2** Lock 14 preserved; per-technique transfer tables intact; 2B
  aarch64 close-state extended with substrate columns without regressing
  grammar-neutrality.
- **CH3** REDRESS pre-blocks 88/89/96/97/98 STRENGTHENED at V3; 3-cycle
  aggregate 49/50 = 98.0% ACCEPT; sole REVISE in 50 findings was the V1
  prophylactic-only delta consumed at V2.
- **CH6** cohort refutation 32:69 = 31.7% density preserved (1:2
  anti-paper-close pattern institutionalised). SKELETON DELETE doubly
  inadmissible via F-CH5-V2-01.
- **CH7** 5 V3 NEW subclauses + 12 V2 holdovers = 17/17; **3-cycle LOCK
  EXTENDED** (deepest-LOCKED lens in cohort).

---

## 4. Cohort-level cross-lens convergence — final V3

V3 institutionalises six cohort-wide cross-lens convergence findings that
constitute the load-bearing T-P3 §3C inputs:

| # | Convergence finding | Lens evidence | T-P3 §3C role |
|---|---------------------|---------------|----------------|
| 1 | **Refutation discipline INSTITUTIONALIZED** | Cohort 32:69 = 31.7% density preserved across V2+V3 (1:2 anti-paper-close pattern); CH6 13/13 ACCEPT | Most load-bearing T-P3 §3C input; discipline binds Lock 8 BENCH comparator-plane |
| 2 | **SKELETON triple DELETE doubly inadmissible** | CH4 binary disposition + CH5 substrate_target/retention_lifetime columns + CH6 anti-paper-close preservation = three independent surfaces converging on DELETE | Lock 1 v+1 substrate-union amendment surface; LAC-2F-V5-02 ground |
| 3 | **REDRESS pre-block uniformity AT 6/6** for 96/97 (most binding substrate-union pre-blocks) | CH3 V3 STRENGTHENING; F-CH3-2F-08 fully discharged at 2F V6 via LAC-2F-V5-01 | Lock 1 transient classifier-state primitives v+1 generalisation |
| 4 | **LAC-2F-V5-02 ELEVATED to T-P3 §3C** (generalises REDRESS 96/97/98 to ALL transient classifier-state primitives) | CH5 V2-NEW; preserved verbatim at V3 | Strongest Lock 1 v+1 amendment surface in cohort |
| 5 | **BBNF_SIMD_STRICT=1 cohort-wide precondition** institutionalized at 2A:192 + 2C:303-305 + 2D:142-149 | CH4 F-V3-CH4-A discharge with mutual cross-references | Lock 10 scalar-first precondition; Lock 16 per-primitive admissibility |
| 6 | **6 abrogate gates numerically bound** (T2A-LAC-V1-05 schema) | CH4 F-V3-CH4-B at 2D:151-162 | Lock 8 BENCH (e-graph nodes/classes/iter + CSP + stale + LOC + parity/checkasm) |

### Discipline preservation across V1 → V2 → V3

| Discipline axis | V1 | V2 | **V3** | Trajectory |
|-----------------|----|----|--------|------------|
| Refutation ratio | 31:64 | 32:64+ | **32:69 (1:2 density)** | extended + locked |
| CH7 audit-state cell census | n/a | 157 | **≥157 (preserved)** | discipline strengthened |
| SHA-pinned upstreams returning 200 | 5 | 5 | **5 (re-verified at HEAD 2026-05-23)** | LOCK-grade |
| URL refresh real-author-canonical | n/a | 5 | **5 (preserved)** | LOCK-grade |
| asmjson verbatim char-for-char | drift | exact | **exact (235-line confirmed)** | discharged |
| REDRESS 96/97 explicit coverage | partial | 6/6 | **6/6 STRENGTHENED** | uniform + bound |
| LAC-1E-12 re-execution markers (2A) | n/a | 10 | **10 preserved + extended** | reflex honored |
| SKELETON-only contracts | 3 | 0 | **0 (doubly inadmissible)** | binary DELETE |
| 5 binding REDRESS pre-blocks (88/89/96/97/98) | uneven | uniform | **uniform + STRENGTHENED** | cohort-wide |
| Cohort orphan REVISEs | 30 | 6 | **0** | discharged |

**V3 strengthened discipline AND broadened coverage simultaneously while
holding ratio constant** — the LOCK signature.

---

## 5. T-P3 §3C carry-forward inputs (~28+ LACs)

Carrying forward V1 + V2-NEW + V3 strengthening. V3 added no new LACs (V3
was a cosmetic-refresh + numeric-bind cycle); instead it grounds two
existing LACs at the per-primitive level. Final reconciliation below.

### V3 strengthening (no new LACs)
- **F-V3-CH4-B numerics propagated into Lock 10 v+1 via LAC-2D-06.** The 6
  abrogate-gate numerics (e-graph ≤50000 nodes / ≤10000 classes / ≤30 iter;
  CSP ≤1s/grammar; stale ≤30%; LOC growth bound; row regression admit;
  parity/checkasm gate) ground Lock 10 v+1 at the numeric-cap level.
- **F-CH5-V2-01 substrate columns ground Lock 1 v+1 per-primitive.** The
  `substrate_target` and `retention_lifetime` columns at 2B §A5/§A6 ground
  the substrate-union invariant at the dossier-row level, not only at the
  manifest-field level.

### V2-NEW LACs (4 — preserved verbatim at V3)
| LAC | Dossier | Substance |
|-----|---------|-----------|
| **LAC-2B-06** | 2B | Lock 10 forbid marker-string lowerers |
| **LAC-2B-07** | 2B | Lock 16 atomic close-state vocabulary |
| **LAC-2D-06** | 2D | CollapsedStage predicate x86-only + BackendExpr `substrate_target` declaration |
| **LAC-2F-V5-02** | 2F | **ELEVATED** to T-P3 §3C amendment surface — Lock 1 substrate-union v+1 generalises beyond REDRESS 96/97/98 to ALL transient classifier-state primitives |

### V1 carry-forward LACs (Lock-keyed digest)
- **Lock 1 transient-projection:** T2A-LAC-V1-01; **plus** LAC-2F-V5-02
  prev_in_string transient classifier-state generalisation (V2-NEW elevated).
- **Lock 8 BENCH comparator-plane:** 2A (T2A-LAC-V1-05 abrogate-gate schema
  + 6 numerics bound at V3).
- **Lock 10 scalar-first precondition:** 2A; opaque pattern strings
  (LAC-2F-V5-04); BBNF_SIMD_STRICT=1 cohort-wide precondition (V3 F-V3-CH4-A).
- **Lock 14 grammar-neutral transfer:** 2A grammar-neutral transfer;
  `policy_owner` (LAC-2B-03); JSON-canonical labels grammar-SHAPE leak
  (LAC-2C-02); FlagSchema (LAC-2C-03); abstract-primitive sibling
  (LAC-2F-V5-03).
- **Lock 16 per-primitive manifest schema:** T2A per-primitive manifest;
  same-commit scalar/checkasm (LAC-2B-02); Layer 0 modification ban
  (LAC-2B-04); aarch64 vocabulary parity (LAC-2B-05); `bbnf-regex::Dfa`
  admissibility (LAC-2F-V5-01); PMULL/CSSC lineage (LAC-2E-04).

**T-P3 §3C inbound surface census: ~28+ LACs.** Strongest amendment surface
remains LAC-2F-V5-02 (Lock 1 substrate-union v+1 generalisation); strongest
numeric-bound discipline remains F-V3-CH4-B (6 abrogate gates at 2D:151-162).

---

## 6. 3-cycle journey record

| Cycle | Lens census | Lenses ≥95% | Orphan REVISEs | REJECTs | §3Z state |
|-------|-------------|-------------|----------------|---------|-----------|
| **V1 close** | 7 (sub-axes ~63% aggregate) | 1/7 (CH7) | 30 | 0 | NOT eligible |
| **V2 close** | 7 (sub-axes ~96% aggregate, qualified) | 7/7 (first) | 6 (qualified residue) | 0 | NOT eligible (1 of 2) |
| **V3 close** | 7 (sub-axes 100% aggregate) | **7/7 (second consecutive)** | **0** | 0 | **§3Z COHORT LOCK ACHIEVED** |

### Sub-axis aggregate trajectory

```
                    V1 (close)     V2 (close)     V3 (close)
CH1 CORRECTNESS:    50.0%      →   100.0%     →   100.0%        [2-CYCLE LOCK]
CH2 GENERALITY:     66.7%      →   100.0%     →   100.0%        [2-CYCLE LOCK]
CH3 REGRESSION:     94.4%      →   100.0%     →   100.0%        [2-CYCLE LOCK]
CH4 COST:           33.0%      →    96.0%*    →   100.0%        [2-CYCLE LOCK]
CH5 HIDDEN:         83.0%      →   100.0%‡    →   100.0%        [2-CYCLE LOCK]
CH6 ANTI-PAPER:     61.5%      →   100.0%     →   100.0%        [2-CYCLE LOCK]
CH7 OVERFIT-PRUNE:  100.0%     →   100.0%     →   100.0%        [3-CYCLE LOCK]
                    ────────────────────────────────────────────
Cohort aggregate:   ~63%       →   ~96%       →   100%          [§3Z LOCK]
Orphan REVISEs:     30         →    6         →   0
REJECTs:            0          →    0         →   0

* CH4 V2 = 96% qualified (6/6 dossiers ACCEPT; 2 V3 micro-folds outstanding); V3 = 100% unqualified.
‡ CH5 V2 = 100% ACCEPT-class (5 ACCEPT + 1 ACCEPT-WITH-FOLD); V3 = 100% with both LIGHT folds discharged.
```

---

## 7. Cross-lens convergence findings — load-bearing T-P3 §3C inputs

The six cohort-level convergence findings (§4 above) constitute the
load-bearing T-P3 §3C input surface. Per priority for T-P3 §3C synthesis:

1. **Refutation discipline INSTITUTIONALIZED** (cohort 32:69 ratio; CH6 lens).
   Binds Lock 8 BENCH discipline; admits no paper-close without measured
   gate. **Most load-bearing T-P3 §3C input.**
2. **SKELETON triple DELETE doubly inadmissible** (CH4 binary + CH5
   substrate columns + CH6 anti-paper-close preservation). Binds Lock 1
   substrate-union admission; LAC-2F-V5-02 amendment ground.
3. **LAC-2F-V5-02 ELEVATED to T-P3 §3C** (Lock 1 substrate-union v+1
   amendment surface — strongest in cohort). Generalises REDRESS 96/97/98
   to ALL transient classifier-state primitives.
4. **REDRESS pre-block uniformity AT 6/6** for 96/97 (most binding
   substrate-union pre-blocks); 88/89/98 at 5/6 (scope-silent dossiers
   correctly silent). Binds Lock 1 v+1 amendment scope.
5. **BBNF_SIMD_STRICT=1 cohort-wide precondition** (Lock 10 scalar-first +
   Lock 16 per-primitive admissibility). Institutionalized at three cohort
   sites with mutual cross-references.
6. **6 abrogate gates numerically bound** (T2A-LAC-V1-05 schema; F-V3-CH4-B
   at 2D:151-162). Binds Lock 8 BENCH gate-execution: e-graph ≤50000 nodes
   / ≤10000 classes / ≤30 iter; CSP ≤1s/grammar; stale ≤30%; LOC growth
   bound to `loc_budget`; row regression admit; parity/checkasm gate.

---

## 8. T-P3 dispatch gate OPEN

**T-P3 dispatch gate: OPEN.**

Per `restart/prompts/ORCHESTRATOR.md` §3W + §3Z and the SK-V14
ORCHESTRATOR-PROMPT THE SK LOOP, T-P3 dispatch was gated on cohort T-P2
§3Z LOCK. The gate is now satisfied:

| T-P3 dispatch gate predicate | State |
|-------------------------------|-------|
| T-P1 LOCK | ACHIEVED at `0a9c0fe65` |
| T-P2 LOCK | **ACHIEVED at V3 close (this declaration)** |
| T-P3 dispatch gate | **OPEN** |

Per `restart/prompts/totality/PASS-3-SYNTHESIS.md`, T-P3 may now dispatch
with the ~28+ LAC carry-forward surface (§5 above) and the six cohort-level
cross-lens convergence findings (§4 + §7 above) as binding inputs.

### Remaining post-LOCK gates

The T-P3 dispatch gate being OPEN, the only remaining gates per
SK-V14 ORCHESTRATOR-PROMPT are:

1. **T-P3 §3C disposition** — the synthesis cohort itself must converge
   per `restart/prompts/totality/PASS-3-SYNTHESIS.md` §3C.
2. **G-Omega user gate** — the only mandatory orchestrator-relinquish per
   SK-V14 ORCHESTRATOR-PROMPT.

---

## Appendix A — V3 lens file references

| Lens | File | Size (bytes) |
|------|------|-------------|
| CH1 | `restart/audit/totality/p2/hardening/V3/CH1.md` | 12883 |
| CH2 | `restart/audit/totality/p2/hardening/V3/CH2.md` | 24371 |
| CH3 | `restart/audit/totality/p2/hardening/V3/CH3.md` | 20604 |
| CH4 | `restart/audit/totality/p2/hardening/V3/CH4.md` | 29198 |
| CH5 | `restart/audit/totality/p2/hardening/V3/CH5.md` | 30931 |
| CH6 | `restart/audit/totality/p2/hardening/V3/CH6.md` | 26881 |
| CH7 | `restart/audit/totality/p2/hardening/V3/CH7.md` | 33228 |
| Context | `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md` | 3837 |

V2 baseline: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`.
V1 baseline: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.

---

## Appendix B — disposition deltas at-a-glance (V1 → V2 → V3)

```
CH1:  V1=50.0%   V2=100.0%  V3=100.0%   Δ=+50.0pp   [2-CYCLE LOCK at V3]
CH2:  V1=66.7%   V2=100.0%  V3=100.0%   Δ=+33.3pp   [2-CYCLE LOCK at V3]
CH3:  V1=94.4%   V2=100.0%  V3=100.0%   Δ= +5.6pp   [2-CYCLE LOCK at V3]
CH4:  V1=33.0%   V2= 96.0%* V3=100.0%   Δ=+67.0pp   [2-CYCLE LOCK at V3]
CH5:  V1=83.0%   V2=100.0%‡ V3=100.0%   Δ=+17.0pp   [2-CYCLE LOCK at V3]
CH6:  V1=61.5%   V2=100.0%  V3=100.0%   Δ=+38.5pp   [2-CYCLE LOCK at V3]
CH7:  V1=100.0%  V2=100.0%  V3=100.0%   Δ=  0.0pp   [3-CYCLE LOCK at V3]
       ──────────────────────────────────────────────
Cohort:V1=1/7     V2=7/7     V3=7/7×2  [§3Z COHORT LOCK ACHIEVED]
Orphans: 30   →    6    →    0
REJECTs:  0   →    0    →    0
Ceiling: V1   →    V2   →    V3 (3/5; 2-cycle margin)

* CH4 V2=96% qualified (6/6 dossiers ACCEPT, 2 V3 micro-folds outstanding); V3=100% unqualified.
‡ CH5 V2=100% ACCEPT-class (5 ACCEPT + 1 ACCEPT-WITH-FOLD); V3=100% with both LIGHT folds discharged.
```

---

## Appendix C — declared SK LOOP carry-forward

Per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP, this declaration emits:

- **§3Z COHORT LOCK ACHIEVED** for T-P2 cohort at V3 close (2026-05-23).
- **T-P3 dispatch gate OPEN** with ~28+ LAC carry-forward surface and six
  cohort-level cross-lens convergence findings as binding T-P3 §3C inputs.
- **V≤5 ceiling honored** with 2-cycle margin (V3 = 3/5 consumed).
- **Zero orphan REVISEs, zero REJECTs** at V3 close.

Next required orchestrator move: dispatch T-P3 per
`restart/prompts/totality/PASS-3-SYNTHESIS.md` with this declaration's §5 +
§7 as binding inputs.

---

*End HARDENING-T-P2-V3-CONSOLIDATED — §3Z COHORT LOCK DECLARATION.*
