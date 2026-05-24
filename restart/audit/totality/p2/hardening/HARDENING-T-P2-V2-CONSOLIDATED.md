---
doc_kind: hardening-consolidated
cohort: T-P2
cycle: V2
pass: omega
authored: 2026-05-23
v1_source: restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md
v2_lens_dir: restart/audit/totality/p2/hardening/V2/
gate_state_v2_close: 7/7 ≥95% (first cohort-wide ≥95% cycle); 1/7 at 2-cycle LOCK (CH7)
predicted_v3_cohort_lock: TRUE — confirming cycle + ~6 LIGHT folds → all 7 ≥95% second consecutive → §3Z COHORT LOCK
hard_cap_min: 25
---

# HARDENING-T-P2-V2-CONSOLIDATED

Consolidated V2 CHALLENGE aggregator for cohort T-P2 (SK-V14 Pass Omega).
Authority: `restart/prompts/totality/PASS-2-RESEARCH.md` §3 + §5; `restart/prompts/ORCHESTRATOR.md` §3W + §3Z; `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md`.
Input lenses: `restart/audit/totality/p2/hardening/V2/CH{1..7}.md` (7 files).
V1 baseline: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.

---

## 1. Cycle disposition table — 7 lenses × V1 × V2 × LOCK status

| Lens | V1 score | V2 score | Δ | Cycle LOCK status | Notes |
|------|----------|----------|---|-------------------|-------|
| **CH1 CORRECTNESS** | 50% (3/6) | **100% (6/6)** | +50.0pp | V2 first ≥95% — V3 confirming required | 5 SHA-pinned upstream + 3 BLK discharged with executable verification (Sneller Wayback + 5 URL refreshes + asmjson verbatim char-for-char). 2 V3 cosmetic micro-folds outstanding (2A README 235 vs 236; 2F counted_source_ids 24 vs 26). |
| **CH2 GENERALITY** | 66.7% (4/6) | **100% (6/6)** | +33.3pp | V2 first ≥95% — V3 confirming required | Lock 14 holds cohort-wide. 2A + 2D per-technique transfer tables + 2B aarch64 close-state classification (1+2+2+1+3-SKELETON-DELETE) + 2C F9 2-cell split + F10 BBNF-self FIRMED. |
| **CH3 REGRESSION** | 94.4% (17/18) | **100% (18/18)** | +5.6pp | V2 first ≥95% — V3 confirming required; LOCK eligibility recommended | F-CH3-2F-08 fully discharged at 2F V6 (LAC-2F-V5-01 CH3 pre-flight reflex with REDRESS 96/97/98 path:line). REDRESS 96/97 reach 6/6 explicit coverage. V1 0.6pp prophylactic-only delta consumed. |
| **CH4 COST** | 33% (2/6) | **96% (qualified; 6/6 dossiers ACCEPT)** | +63pp | V2 first ≥95% — V3 confirming required | All 4 V1 load-bearing REVISEs DISCHARGED (CH4-F1 SKELETON DELETE + CH4-F2 marker-string + CH4-F4 ~37-row cost ledger + CH4-F7 LOC/risk realism). 2 V3 micro-folds: F-V3-CH4-A `BBNF_SIMD_STRICT` cohesion in 2A/2C/2D; F-V3-CH4-B abrogate-gate e-graph cap + LOC growth bind in 2D :136-146. |
| **CH5 HIDDEN COUPLING** | 83% (5/6) | **100% ACCEPT-class (5 ACCEPT + 1 ACCEPT-WITH-FOLD)** | +17pp | V2 first ≥95% — V3 confirming required | Substrate-union invariant HOLDS cohort-wide. V1 single REVISE (2D) FULLY DISCHARGED via LAC-2D-06. **LAC-2F-V5-02 ELEVATED** to T-P3 §3C amendment surface (generalises REDRESS 96/97/98 to ALL transient classifier-state primitives). 2 V3 folds: F-CH5-V2-01 substrate columns 2B §A5/§A6; F-CH5-V2-02 `crate_target` Lock 1 manifest in 2F. |
| **CH6 ANTI-PAPER-CLOSE** | 61.5% (8/13) | **100% (13/13)** | +38.5pp | V2 first ≥95% — V3 confirming required | All 5 V1 REVISE discharged verbatim (F7/F8/F9/F10/F11). SKELETON-only count collapses 3→0 via 2B binary DELETE — most honest CH6 disposition shift. Refutation count rises 31→32. |
| **CH7 OVERFIT-PRUNE** | 100% (12/12) | **100% (12/12)** | 0pp | **V2 = 2-cycle LOCK ACHIEVED** | First lens in cohort at LOCK. Audit-state cell census grew 89→157 (+76% discipline strengthening). SKELETON DELETE honest. 5 URL refreshes real-author-canonical. 10 LAC-1E-12 re-execution markers in 2A. |

Score model: per-lens dispositions per `restart/prompts/totality/PASS-2-RESEARCH.md` §3 (ACCEPT / REVISE / REJECT) with cohort-wide ≥95% gate per §3Z.
Cohort census: **7/7 lenses ≥95% at V2 close — FIRST cohort-wide ≥95% cycle.**

---

## 2. §3Z gate evaluation — cohort state at V2 close

Per `restart/prompts/ORCHESTRATOR.md` §3Z, COHORT LOCK requires **two consecutive cycles** with every lens at ≥95% disposition. V2 close state:

| §3Z criterion | V1 close | V2 close | V3 prediction |
|---------------|----------|----------|---------------|
| Lenses ≥95% | 1/7 (CH7 only) | **7/7** | 7/7 (predicted) |
| Lenses at 2-cycle LOCK | 0/7 | **1/7 (CH7)** | 7/7 (predicted — all consecutive) |
| Cohort-wide ≥95% cycle count | 0 | **1 (first)** | 2 (LOCK) |
| Cohort §3Z LOCK eligible | NO | NO (one cycle only) | **YES (predicted)** |

**V2 close gate state: FIRST cohort-wide ≥95% cycle; one consecutive of two required for LOCK. CH7 is the lone 2-cycle LOCK lens (V1+V2 both 100%). The remaining 6 lenses need V3 confirming at ≥95% to clear §3Z.**

The orchestrator's V≤5 ceiling holds with margin (V3 close projects to LOCK; V4/V5 reserved for divergence).

---

## 3. V2 strengthening packet — what each V2 fold discharged

V2 was the highest-yield cycle in the T-P2 hardening sequence: V1 had 4 lenses below 95% (CH1 50%, CH2 66.7%, CH4 33%, CH6 61.5%). All four crossed at V2 with discharged V1 REVISEs and no carry-over to V3 beyond LIGHT cosmetics.

### CH1 CORRECTNESS — V1 50% → V2 100% (+50.0pp)
V1 had 3 BLOCKED inadmissibles (CH1-F1 SKELETON dossier 2B; CH1-F2 asmjson exact-text drift; CH1-F3 Sneller cite 404). V2 discharge:
- **CH1-F1 SKELETON dossier 2B DELETED** as part of cohort-wide SKELETON triple-DELETE (binary removal with zero-consumer proof; cited at 5 honest fold sites).
- **CH1-F2 asmjson verbatim char-for-char** verified against upstream HEAD; 5 SHA pins survive.
- **CH1-F3 Sneller cite refreshed** via Wayback canonical URL + 5 sibling URL refreshes (all real-author-canonical, zero fabricated).

V3 remainder: 2 LIGHT cosmetic micro-folds (`F-V3-CH1-A` 2A README 235 vs 236; `F-V3-CH1-B` 2F counted_source_ids 24 vs 26).

### CH2 GENERALITY — V1 66.7% → V2 100% (+33.3pp)
V1 had 2 REVISE (2A and 2D per-technique transfer tables; 2C F9 cell split; F10 BBNF-self soft-deferral). V2 discharge:
- **2A + 2D per-technique transfer tables** added cohort-wide; Lock 14 grammar-neutral transfer holds.
- **2B aarch64 close-state classification 1+2+2+1+3** lands with SKELETON DELETE consuming the 3-element cell.
- **2C F9 2-cell split** lands; **F10 BBNF-self FIRMED ADMITTED-VIA-C4-W10** via `parse_that_regex::unescape_string` same-commit binding (soft-deferral eliminated).

V3 remainder: zero load-bearing; cohesion folds only.

### CH3 REGRESSION — V1 94.4% → V2 100% (+5.6pp)
V1 had one finding short of 100% (F-CH3-2F-08, prophylactic-only 0.6pp delta). V2 discharge:
- **F-CH3-2F-08 fully discharged at 2F V6** via LAC-2F-V5-01 CH3 pre-flight reflex with REDRESS 96/97/98 path:line citation.
- **REDRESS 96/97 reach 6/6 explicit coverage** (5 binding REDRESS pre-blocks 88/89/96/97/98 honored uniformly).
- CH3 LOCK eligibility **recommended at V2 close**; V3 confirming required for cohort §3Z.

### CH4 COST — V1 33% → V2 96% (qualified; +63pp)
Largest single-cycle delta in the cohort. V1 had 4 load-bearing REVISEs (CH4-F1 SKELETON; CH4-F2 marker-string drift; CH4-F4 cost ledger sparsity; CH4-F7 LOC/risk realism). V2 discharge:
- **CH4-F1 SKELETON DELETE** (cohort-wide consumption).
- **CH4-F2 marker-string** lower-bound discharged with executable verification.
- **CH4-F4 ~37-row cost ledger** fully populated (up from V1 sparse).
- **CH4-F7 LOC/risk realism** lands with abrogate-gate bind.

V3 remainder: 2 micro-folds (`F-V3-CH4-A` `BBNF_SIMD_STRICT=1` cohort-wide precondition propagation in 2A/2C/2D; `F-V3-CH4-B` 2D :136-146 abrogate-gate e-graph node cap + LOC growth numeric bind, copy from T2A-LAC-V1-05).

### CH5 HIDDEN COUPLING — V1 83% → V2 100% ACCEPT-class (+17pp)
V1 had 1 REVISE (2D substrate-union coverage). V2 discharge:
- **LAC-2D-06** discharges 2D substrate-union REVISE (CollapsedStage predicate x86-only + BackendExpr substrate_target declaration).
- **LAC-2F-V5-02 ELEVATED** to T-P3 §3C amendment surface — generalises REDRESS 96/97/98 to ALL transient classifier-state primitives. **Strongest Lock 1 v+1 amendment surface in the cohort.**
- Substrate-union invariant HOLDS cohort-wide (5 ACCEPT + 1 ACCEPT-WITH-FOLD).

V3 remainder: 2 LIGHT folds (`F-CH5-V2-01` substrate columns 2B §A5/§A6; `F-CH5-V2-02` `crate_target` Lock 1 manifest in 2F).

### CH6 ANTI-PAPER-CLOSE — V1 61.5% → V2 100% (+38.5pp)
V1 had 5 REVISE (F7/F8/F9/F10/F11). V2 discharge:
- **All 5 V1 REVISE discharged verbatim** (no scope drift; literal point-for-point).
- **SKELETON-only count collapses 3→0 via 2B binary DELETE** — most honest CH6+CH7 disposition shift in the cohort (three inadmissible contracts removed with zero-consumer proof; cited at 5 honest fold sites).
- Refutation count rises 31→32 (continuing CH6 trajectory).

### CH7 OVERFIT-PRUNE — V1 100% + V2 100% = 2-cycle LOCK
First lens in cohort at LOCK. V2 strengthening:
- **Audit-state cell census grew 89→157** (+76% discipline strengthening).
- **5 URL refreshes real-author-canonical** verified at HEAD.
- **10 LAC-1E-12 re-execution markers in 2A** confirm reflex honored.
- SKELETON DELETE honest (zero overfit residue from binary removal).

### Cross-cutting V2 motifs (single source-of-truth)
- **SKELETON triple-DELETE** is the cohort's most honest CH6+CH7 disposition shift; cited at 5 honest fold sites (2B, CH1-F1, CH4-F1, CH6 collapse, CH7 census reduction).
- **LAC-2F-V5-02 elevation to T-P3 §3C** is the strongest Lock 1 v+1 amendment surface.
- **2C V4 BBNF-self FIRMED ADMITTED-VIA-C4-W10** via same-commit `parse_that_regex::unescape_string` binding eliminates the V1 soft-deferral.

---

## 4. Cross-lens convergence — refutation discipline preservation

V2 preserves and extends the refutation discipline established at V1:

| Discipline axis | V1 measure | V2 measure | Trajectory |
|-----------------|------------|------------|------------|
| Refutation ratio (refutations : claims) | 31:64 | **32:64+** | preserved + extended |
| CH7 audit-state cell census | 89 | **157** | +76% strengthening |
| SHA-pinned upstream cites | 5 | **5** (verified) | LOCK-grade |
| URL refresh real-author-canonical | n/a | **5** | new V2 discipline |
| asmjson verbatim char-for-char | drift | **exact** | discharged |
| REDRESS 96/97 explicit coverage | partial | **6/6** | uniform |
| LAC-1E-12 re-execution markers (2A) | n/a | **10** | reflex honored |
| SKELETON-only contracts | 3 | **0** | binary DELETE |
| 5 binding REDRESS pre-blocks (88/89/96/97/98) | uneven | **uniform** | cohort-wide |

**The refutation ratio held while the audit-state cell census grew by 76%** — V2 strengthened discipline AND broadened coverage simultaneously.

**5 SHA-pinned upstream cites + 5 URL refreshes all real-author-canonical, zero fabricated** — verified at HEAD.

---

## 5. V3 fold packet (~6 LIGHT items across 4 dossiers)

All V3 folds are LIGHT (no load-bearing remaining; no scope expansion). Predicted ≤30 min author time across the cohort.

| # | ID | Dossier | Path:line / target | Disposition | Risk |
|---|----|---------|--------------------|-------------|------|
| 1 | **F-V3-CH4-A** | 2A + 2C + 2D | inline cohesion (all three) | Propagate `BBNF_SIMD_STRICT=1` as cohort-wide precondition inline (already gate-consumable per V2; prose cohesion only) | LIGHT — wording |
| 2 | **F-V3-CH4-B** | 2D | `:136-146` | Abrogate-gate e-graph node cap + LOC growth numeric bind (4/6 thresholds bound; copy numerics from T2A-LAC-V1-05) | LIGHT — copy numerics |
| 3 | **F-CH5-V2-01** | 2B | `§A5/§A6` | Add per-primitive `substrate_target` / `retention_lifetime` columns to aarch64 close-state table + cost ledger | LIGHT — column add |
| 4 | **F-CH5-V2-02** | 2F | Lock 1 manifest | Elevate `crate_target` Lock 1 manifest field (LAC-2F-V5-02 already elevated to T-P3 §3C amendment surface) | LIGHT — schema field |
| 5 | **F-V3-CH1-A** | 2A | README | Line count 235 vs claimed 236 cosmetic refresh | LIGHT — cosmetic |
| 6 | **F-V3-CH1-B** | 2F | frontmatter | `counted_source_ids` 24 vs 26 reconciliation | LIGHT — cosmetic |

**Packet shape:** 6 LIGHT items, 4 dossiers touched (2A, 2B, 2D, 2F), 0 load-bearing, 0 scope expansion.

---

## 6. V3 dispatch shape — confirming-cycle + 6 LIGHT folds

V3 is a **confirming cycle** for §3Z LOCK eligibility. Dispatch parameters:

- **Cycle role:** confirming — second consecutive ≥95% across all 7 lenses required for LOCK
- **Folds in scope:** 6 LIGHT items (§5 above)
- **Out-of-scope:** any scope expansion; any new V3 finding raises orchestrator escalation per §3W
- **Cohort hard cap:** standard (LIGHT folds + confirming verification)
- **Per-lens dispatch:** 7 parallel V3 cycles; aggregator at V3 close

**Predicted V3 close scoring (per lens):**

| Lens | V2 | V3 predicted | Confidence |
|------|----|--------------| -----------|
| CH1 | 100% | **100%** | HIGH (2 cosmetic folds discharge fully) |
| CH2 | 100% | **100%** | HIGH (zero V3 load-bearing) |
| CH3 | 100% | **100%** | HIGH (REDRESS 96/97 uniform) |
| CH4 | 96% qualified | **≥95%** (likely 100%) | HIGH (2 micro-folds discharge qualifications) |
| CH5 | 100% ACCEPT-class | **100%** | HIGH (2 LIGHT folds; LAC-2F-V5-02 already elevated) |
| CH6 | 100% | **100%** | HIGH (SKELETON DELETE locked in) |
| CH7 | 100% (2-cycle LOCK) | **100% (3-cycle LOCK)** | LOCK-GRADE |

**All 7 lenses predicted ≥95% at V3 close → §3Z COHORT LOCK eligible.**

---

## 7. T-P3 §3C carry-forward — ~28+ LACs (+4 V2-NEW)

Carrying forward V1 LACs plus V2-NEW additions; final reconciliation at V3 close.

### V2-NEW LACs (4)
| LAC | Dossier | Substance |
|-----|---------|-----------|
| **LAC-2B-06** | 2B | Lock 10 forbid marker-string lowerers |
| **LAC-2B-07** | 2B | Lock 16 atomic close-state vocabulary |
| **LAC-2D-06** | 2D | CollapsedStage predicate x86-only + BackendExpr `substrate_target` declaration |
| **LAC-2F-V5-02** | 2F | **ELEVATED** to T-P3 §3C amendment surface — Lock 1 substrate-union v+1 generalises beyond REDRESS 96/97/98 to ALL transient classifier-state primitives |

### V1 carry-forward LACs (Lock-keyed digest)
- **Lock 1 transient-projection:** T2A-LAC-V1-01
- **Lock 8 BENCH comparator-plane:** 2A
- **Lock 10 scalar-first precondition:** 2A; opaque pattern strings (LAC-2F-V5-04)
- **Lock 14 grammar-neutral transfer:** policy_owner (LAC-2B-03); JSON-canonical labels grammar-SHAPE leak (LAC-2C-02); FlagSchema (LAC-2C-03); abstract-primitive sibling (LAC-2F-V5-03)
- **Lock 16 per-primitive manifest schema:** same-commit scalar/checkasm (LAC-2B-02); Layer 0 modification ban (LAC-2B-04); aarch64 vocabulary parity (LAC-2B-05); `bbnf-regex::Dfa` admissibility (LAC-2F-V5-01); PMULL/CSSC lineage (LAC-2E-04)

**T-P3 §3C inbound surface census: ~28+ LACs (+4 V2-NEW), with LAC-2F-V5-02 as the strongest Lock 1 v+1 amendment surface.**

---

## 8. Predicted §3Z COHORT LOCK at V3 close

| Predicate | State at V2 close | State at V3 close (predicted) |
|-----------|-------------------|-------------------------------|
| All 7 lenses ≥95% | TRUE (first cycle) | **TRUE (second consecutive)** |
| Two consecutive cohort-wide ≥95% cycles | FALSE (1/2) | **TRUE (2/2)** |
| CH7 LOCK | TRUE (2-cycle) | TRUE (3-cycle) |
| CH3 LOCK eligibility | recommended | **eligible** |
| CH1/CH2/CH4/CH5/CH6 LOCK | not yet (1-cycle ≥95%) | **eligible (2-cycle ≥95%)** |
| §3Z COHORT LOCK | NO | **YES (predicted)** |
| V≤5 ceiling consumed | V2 (2/5) | V3 (3/5) — margin preserved |

**V2 close gate state:** 7/7 ≥95% (FIRST cohort-wide ≥95% cycle); 1/7 at 2-cycle LOCK (CH7).
**Trajectory:** V3 fold (~6 LIGHT items across 4 dossiers) → V3 CHALLENGE confirming cycle → cohort §3Z LOCK at V3 close.
**V≤5 orchestrator ceiling honored with 2-cycle margin.**

---

## Appendix A — V2 lens file references

| Lens | File | Size |
|------|------|------|
| CH1 | `restart/audit/totality/p2/hardening/V2/CH1.md` | 16025 B |
| CH2 | `restart/audit/totality/p2/hardening/V2/CH2.md` | 22418 B |
| CH3 | `restart/audit/totality/p2/hardening/V2/CH3.md` | 25497 B |
| CH4 | `restart/audit/totality/p2/hardening/V2/CH4.md` | 34764 B |
| CH5 | `restart/audit/totality/p2/hardening/V2/CH5.md` | 25367 B |
| CH6 | `restart/audit/totality/p2/hardening/V2/CH6.md` | 25312 B |
| CH7 | `restart/audit/totality/p2/hardening/V2/CH7.md` | 29821 B |
| Context | `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md` | 3578 B |

V1 baseline: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md` (61102 B).

---

## Appendix B — disposition deltas at-a-glance

```
CH1:  V1=50.0%   V2=100.0%  Δ=+50.0pp   [V2 first ≥95%; V3 confirming required]
CH2:  V1=66.7%   V2=100.0%  Δ=+33.3pp   [V2 first ≥95%; V3 confirming required]
CH3:  V1=94.4%   V2=100.0%  Δ= +5.6pp   [V2 first ≥95%; LOCK eligibility recommended]
CH4:  V1=33.0%   V2= 96.0%* Δ=+63.0pp   [V2 first ≥95% qualified; V3 confirming required]
CH5:  V1=83.0%   V2=100.0%‡ Δ=+17.0pp   [V2 first ≥95%; V3 confirming required]
CH6:  V1=61.5%   V2=100.0%  Δ=+38.5pp   [V2 first ≥95%; V3 confirming required]
CH7:  V1=100.0%  V2=100.0%  Δ=  0.0pp   [2-CYCLE LOCK ACHIEVED]
       ──────────────────────────────────
Cohort: V1=1/7    V2=7/7              [FIRST cohort-wide ≥95% cycle]

* CH4 96% qualified (6/6 dossiers ACCEPT; 2 V3 micro-folds outstanding)
‡ CH5 100% ACCEPT-class (5 ACCEPT + 1 ACCEPT-WITH-FOLD)
```

---

*End HARDENING-T-P2-V2-CONSOLIDATED.*
