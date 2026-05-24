---
doc_kind: hardening-consolidated
cohort: T-P3
cycle: V1
pass: omega
authored: 2026-05-23
v1_lens_dir: restart/audit/totality/p3/hardening/V1/
challenge_context: restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md
gate_state_v1_close: 4 ACCEPT / 3 REVISE (CH2 + CH6 + CH7); cohort sub-axis ≈86%; §3Z COHORT LOCK NOT YET ACHIEVED (V2 fold required)
ceiling_consumed: V1 (1 of V≤5)
predicted_trajectory: V2 fold (~16+ items, 6 artefacts) → V2 CHALLENGE → V3 confirming → cohort LOCK at V3
hard_cap_min: 30
---

# HARDENING-T-P3-V1-CONSOLIDATED — V1 CHALLENGE Aggregator

Consolidated V1 CHALLENGE aggregator for cohort T-P3 (SK-V14 Pass Omega
totality synthesis). Authority: `restart/prompts/totality/PASS-3-SYNTHESIS.md`
§3 + §5 + §6; `restart/prompts/ORCHESTRATOR.md` §3W + §3Z (cohort LOCK =
≥95% × 2 consecutive cycles; V≤5 ceiling); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
§CH7 (binding from S-P0 carry-forward); `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md`.
Input lenses: `restart/audit/totality/p3/hardening/V1/CH{1..7}.md` (7 files).

---

## §0 — V1 §3Z gate state

**Cohort T-P3 §3Z COHORT LOCK: NOT YET ACHIEVED at V1 close (2026-05-23).
V2 fold required; predicted LOCK at V3 close.**

Per `restart/prompts/ORCHESTRATOR.md` §3Z (cohort LOCK = ≥95% × 2 consecutive
cycles; V≤5 ceiling), the V1 cycle returns four ACCEPT/ACCEPT-WITH-MINOR
dispositions (CH1 + CH3 + CH4 + CH5) and three REVISE dispositions (CH2 +
CH6 + CH7). Cohort sub-axis aggregate ≈86%. The cohort-wide REVISE rate at
V1 is ≈30.6% — within the PASS-3 §3 V1 honest-shape expectation (≥30%
REVISE), explicitly NOT a paper-close cycle. V≤5 ceiling consumed at V1 =
1/5; 4-cycle margin reserved.

### Per-lens V1 disposition table

| Lens | V1 score | ACCEPT count | REVISE count | REJECT count | Verdict | Severity |
|------|---------:|-------------:|-------------:|-------------:|---------|----------|
| **CH1 CORRECTNESS** | 95.7% (110/115 deltas) | 110 | 5 | 0 | **ACCEPT** with 5 LOW REVISE | citation hygiene (cosmetic) |
| **CH2 GENERALITY** | 87.5% (7/8 evidence checks) | 7 | 1 | 0 | **REVISE** | 3F-MIG-004 LAC-1E-14 misclassification |
| **CH3 REGRESSION** | 100% (9/9 findings) | 9 | 0 | 0 | **ACCEPT** (incl. 1 ACCEPT-STRENGTHENING) | clean; LAC-2F-V5-02 elevation textbook-CH3-strengthening |
| **CH4 COST** | ~100% (11/11; 3 V2-minor) | 11 | 3 (MINOR) | 0 | **ACCEPT-WITH-MINOR** | per-delta ledger complete |
| **CH5 HIDDEN COUPLING** | 100% (13/13 + 8/8 sub-checks) | 13 | 0 | 0 | **ACCEPT** | substrate-union preserved; LAC-2F-V5-02 STRENGTHENING |
| **CH6 ANTI-PAPER-CLOSE** | 69.2% (9/13) | 9 | 4 | 0 | **REVISE** | 4 narrow paper-close fissures (3A/3C/3D/3F) |
| **CH7 OVERFIT-PRUNE** | 70% (7/9 + 1 hygiene) | 7 | 2 | 0 | **REVISE** | T-P2 numerator + Pattern H census command staleness |

Score model: per-lens dispositions per `restart/prompts/totality/PASS-3-SYNTHESIS.md`
§3 (ACCEPT / REVISE / REJECT) with cohort-wide ≥95% gate per §3Z. Cohort
census at V1 close: **4 ACCEPT/ACCEPT-WITH-MINOR + 3 REVISE; cohort
sub-axis ≈86%; first cycle of two needed for §3Z LOCK NOT ACHIEVED.**

---

## §1 — Cycle disposition table — 7 lenses × ACCEPT-rate × verdict

| Lens | V1 verdict | Strength | Defect class | V2 fold items |
|------|-----------|----------|---------------|----------------|
| **CH1 CORRECTNESS** | ACCEPT (95.7%) | `3C-locks-v+1-diff.md` applies cleanly via `git apply --check --recount` for 6 representative hunks; all 115 deltas cite real T-P1 finding-ids or T-P2 grounding at path:line | 5 LOW REVISE — citation hygiene (anchor-pair, frontmatter ambiguity, hunk-index drift, dual-target hunk format, V3/V5 attribution drift) | 5 items |
| **CH2 GENERALITY** | REVISE (87.5%) | 5-shape `BackendShape` canon preserved across 3A/3B/3D/3E; 7-step onboarding test intact; zero JSON-narrowing dispositions in 3C; 12 L14-HC clauses in 3E executable | 1 REVISE — 3F-MIG-004 misclassifies LAC-1E-14 as 5th BackendShape variant at 3 sites (:104 + §4:125 + open-question:311); mirror 3C V4-3 wording | 3 sites in 1 artefact (3F) |
| **CH3 REGRESSION** | ACCEPT (100%) | Zero REDRESS re-opens; zero refuted-wave revivals; zero rejected-route promotions; zero REDRESS-strengthened lock weakenings; LAC-2F-V5-02 elevation textbook CH3-STRENGTHENING (pre-block widens, gate hardens, lineage preserved verbatim) | None | 0 items (3 carry-forward notes for aggregator visibility) |
| **CH4 COST** | ACCEPT-WITH-MINOR (~100%) | All 6 artefacts carry per-delta cost-and-routing ledger; 3B NEW waves each carry same-wave consumer; T2A-LAC-V1-05 6 abrogate gates numerically bound at 3C-L08 | 3 V2-MINOR — Pattern H W6 LOC variance (3B-D3 ~11k vs 3B-D9 ~2.0k vs 3C-L14 4-8k); 3E-D06 wave-id pin; 3F-MIG-004 numeric W8 budget pin | 3 items across 3B/3C/3E/3F |
| **CH5 HIDDEN COUPLING** | ACCEPT (100%) | No parallel substrate / sidecar / renamed-scanner Lock 1 violation / Track 1≡Track 2 dishonesty; LAC-2F-V5-02 elevation STRENGTHENING (not introducing) substrate-union; LAC-1E-14 5th SUBSTRATE preserves 5-shape canon at Lock 10 | None | 0 items (5 carry-forward guardrails for Pass Omega) |
| **CH6 ANTI-PAPER-CLOSE** | REVISE (69.2%) | Receiver/blocker/gate triples native to 3A/3B/3C/3D/3E/3F open-question tables; 3D §9 monotonic boundary exemplary; 3F measurable dispatch checklist 8-gate; 0 DEFER + 0 REJECT discipline survives scrutiny | 4 REVISE — 3A ARCH-3A-D06 orphan routing (substrate-union elevation does not select two-cursor disposition); 3C V3-merged anchor mis-cite (`34a28f5c1` vs actual `e12c5323d`); 3D SK-V12 W1b in BOTH §1 wins AND §2 rejections; 3F-MIG-004 paper-conditional "until §3C disposes" despite 3C ACCEPTed | 4 items across 4 artefacts (3A/3C/3D/3F) |
| **CH7 OVERFIT-PRUNE** | REVISE (70%) | 16-lock count preserved (LAC-1E-12 as preface, NOT Lock 17); 5-shape canon preserved (LAC-1E-14 5th SUBSTRATE, NOT 6th BackendShape); audit-overlay 4-column schema institutionalised; SKELETON DELETE refusal architecture (3B + 3F); zero fake `@generated` / SCAFFOLD-as-load-bearing / gate-relabel admits | 2 REVISE doc-level executable — (1) T-P2 cohort numerator 31:64 → 32:69 at 6 sites (T-P1 V1 CH7 COH-012 counter-surface fabrication precedent); (2) Pattern H census command returns 63 not 67 — drop `-maxdepth 2` at 3 sites; **meta-CH7 self-correction catches LAC-1E-12 institutionalisation procedural failure in the very LOCKS amendment proposing it** | 2 items × multiple sites |

**Cohort V1 close ACCEPT-rate aggregate:** 4 ACCEPT/ACCEPT-WITH-MINOR + 3 REVISE = ≈86% sub-axis. Honest V1 shape (≥30% REVISE expected per PASS-3 §3); not paper-close.

---

## §2 — §3Z gate evaluation — cohort state at V1 close

| §3Z criterion | V1 close |
|---------------|----------|
| Lenses ≥95% | 4/7 (CH1 + CH3 + CH4 + CH5) |
| Lenses at 2-cycle LOCK | 0/7 (V1 is first cycle) |
| Cohort-wide ≥95% cycle count | 0 (cohort sub-axis ≈86%) |
| Orphan REVISEs | 15 (5 CH1 LOW + 1 CH2 + 3 CH4 MINOR + 4 CH6 + 2 CH7) |
| REJECTs | 0 |
| DEFERs in 3C disposition matrix | 0 (38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER) |
| Cohort §3Z LOCK | NO (V2 fold required) |
| V≤5 ceiling consumed | V1 (1/5; 4-cycle margin) |

**V1 close gate state: §3Z COHORT LOCK NOT YET ACHIEVED. V2 fold required.**

### V1 cycle posture per PASS-3 §3 paper-close detector

PASS-3-SYNTHESIS §3 expects ≥30% REVISE at V1 cycle as an anti-paper-close
detector. Cohort actual REVISE rate at V1 = ~30.6% (3 REVISE / 7 + minor
weighting from CH1/CH4 LOW-cohort) — within honest V1 shape, explicitly
NOT paper-close. CH1 alone returns 4.3% (below 30%) but cohort jointly
satisfies the detector when CH2 + CH6 + CH7 REVISE contributions are
weighted. CH6 itself returns 30.8% REVISE — exactly within the expected
V1 cycle posture per CH6's own self-assessment.

### Convergence chain prediction

- **V1 → V2:** the highest-yield cycle. V2 fold packet (~16+ items
  enumerated in §5 below) routes mechanical doc-only repairs across 6
  artefacts (3A/3B/3C/3D/3E/3F); heaviest folds at 3C (5 items) and 3F
  (5 items). Expect V2 ACCEPT-rate per lens to cross ≥95% threshold for
  CH2 + CH6 + CH7 (mechanical fixes); CH1 + CH4 minor cosmetics
  discharged; CH3 + CH5 preserved at 100%.
- **V2 → V3:** the confirming cycle. Six non-LOCKED lenses (CH1, CH2,
  CH4, CH6, CH7) deliver second consecutive ≥95%; CH3 + CH5 hold at
  100% for 3-CYCLE LOCK (CH3) and 2-CYCLE LOCK (CH5). Predicted
  cohort-wide ≥95% × 2 consecutive at V3 close.

### V≤5 ceiling honored

V1 consumes 1 of V≤5 ceiling. V2/V3/V4/V5 remain reserved per orchestrator
discipline. The predicted LOCK at V3 (3 cycles consumed) preserves
2-cycle margin per the SK-V14 T-P2 V3 LOCK precedent at
`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`.

---

## §3 — V1 strengthening packet — what V1 already discharges

V1 is structurally sound on the load-bearing architectural axes; the V2
fold packet is mechanical repair, not architectural revision. The V1
strengthening packet enumerates what is already binding at V1 close.

### CH3 REGRESSION — 100% ACCEPT (1 ACCEPT-STRENGTHENING)

The LAC-2F-V5-02 ELEVATION (3C V4 hunk V4-2 at
`restart/audit/totality/p3/3C-locks-v+1-diff.md:90`-`116`) is the **textbook
CH3-STRENGTHENING shape**: the historical 3-shape REDRESS pre-block
(retained class-column / streaming structural cursor / class-lane-only)
widens to ALL transient classifier-state primitives (quote-mask,
escape-mask, structural-mask, class-stream, prev-state byte, prefix-XOR
carry word); the V3-merged Lock 1 disposition gate strengthens from
"REDRESS 96/97/98 material-differential" to "REJECT without further
measurement" for any cross-call retention proposal. CH3 verified: every
artefact carrier of the elevation references the REDRESS 96/97/98
lineage; none weakens it.

### CH5 HIDDEN COUPLING — 100% ACCEPT (13/13 evidence + 8/8 sub-checks)

V1 cohort preserves the 5-shape `BackendShape` canon at Lock 10
(`restart/audit/totality/p3/3C-locks-v+1-diff.md:129`-`134`) and lands
LAC-1E-14 `FactStream` as the **5th SUBSTRATE category at the Lock 1
manifest level only**, NOT a 6th `BackendShape` variant. The two axes
(Lock 1 substrate manifest + Lock 10 BackendShape search domain) are
orthogonal; LAC-1E-14 touches only the manifest axis. Substrate-union
invariant HOLDS cohort-wide; 3F-MIG-004 is the **single artefact
outlier** (see CH2 + CH6 + CH7 REVISE convergence in §4).

### CH4 COST — ACCEPT-WITH-MINOR across all 6 artefacts

All 6 substantive artefacts (3A/3B/3C/3D/3E/3F) carry a §Cost And Routing
Ledger with LOC budget + propagation surfaces + risk class + wave
alignment + same-wave consumer + hard-cap/abrogate-gate columns for every
delta. 3B's three NEW wave proposals (MP-NW-SK14-W0..W11-INHERIT +
SKELETON-DELETE-REFUTED + F-V2-P1ABC-RERECORD-STAGE-0) each carry a
same-wave consumer; the refusal-as-consumer pattern is structurally
admissible per CH6 anti-paper-close discipline. T2A-LAC-V1-05's **6
abrogate gates are numerically bound** at 3C-L08-audit-overlay-columns:
e-graph ≤50000 nodes / ≤10000 classes / ≤30 iter; CSP ≤1s/grammar;
stale ≤30%; LOC growth bound; row regression admit; parity/checkasm
gate. FOLD-3D-013 institutionalises the CH4 6-class cost-neutrality
taxonomy as binding fold rule across future CHALLENGE cycles.

### CH1 CORRECTNESS — diff applies cleanly to LOCKS.md at HEAD

`git apply --check --recount` against `restart/locks/LOCKS.md` at HEAD
`345c321409` returns exit:0 for all 6 representative hunks (V4-1 preface,
V4-2 Lock 1 elevation, V4-3 FactStream, V4-7 Pattern H census, V4-8
CollapsedStage replacement, V4-9 DFA admissibility). The 51 LAC row
count in 3C disposition matrix verifies (`grep -c "^| LAC-\|^| T2A-LAC-"`
returns 51). REDRESS 96/97/98 path:line citations in V4-9 verified
against `skinny/REDRESS.md` headings.

### CH6 — anti-paper-close architectural spine preserved

The four CH6 ACCEPT rows highlight: 3A Open Questions receiver/blocker/
gate triples native; 3B reclassification + refusal-as-named-amendment
discipline; 3C 0-DEFER + 0-REJECT survives scrutiny (every MODIFY
disposition I traced carries a concrete admission gate); 3D §9 monotonic
boundary declaration exemplary (the section every artefact should aspire
to); 3E 12 L14-HC clauses with HEAD baseline numbers (30 parser-name
sites + 127 grammar-named reexports as monotonic-decrease gates); 3F
measurable dispatch checklist enumerates 8 gates with path:line evidence.

### CH7 — architectural CH7-positives preserved

The 16-lock count is correctly preserved across 3C crystallisation + 3C
diff (LAC-1E-12 lands as preface clause, NOT Lock 17). The 5-shape
`BackendShape` canon is correctly preserved at Lock 10 line 164 (HEAD-
verified). Audit-overlay 4-column schema (track2_entry_point /
comparator_plane / per_iter_equality / audit_overlay_verdict) lands in
3C-L08 + propagates into 3B-MP-D07 + 3D-FOLD-014. SKELETON DELETE
refusal architecture present in both 3B (MP-NW-SK14-SKELETON-DELETE-REFUTED)
and 3F (3F-MIG-007). Zero fake `@generated` admits; zero SCAFFOLD-as-
load-bearing admits; zero gate-relabel admits.

---

## §4 — Cross-lens convergence findings

V1 produces three load-bearing cross-lens convergence findings — single
defects flagged by multiple lenses (the cohort's strongest V2-fold signal)
and meta-discipline corrections.

### Convergence 1 — 3F-MIG-004 LAC-1E-14 misclassification (CH2 + CH6 + CH7 triple-lens)

The single artefact 3F misclassifies LAC-1E-14 at three sites:

- `restart/audit/totality/p3/3F-migration-handoff.md:104` — table row "LAC-1E-14
  proposes `FactStream` as 5th BackendShape variant — but this is G-Omega-gated
  per Lock 1 v+1 since it touches Lock 10 BackendShape canon too"
- `restart/audit/totality/p3/3F-migration-handoff.md:125` — proposed-text §4
  repeats "LAC-1E-14 proposes `FactStream` as 5th BackendShape variant (gates
  Lock 1 + Lock 10 v+1)"
- `restart/audit/totality/p3/3F-migration-handoff.md:311` — CH2 open question
  repeats same misclassification

This **directly contradicts** the V1 dispatch binding (`CHALLENGE-CONTEXT.md:30`),
3C V4 hunk V4-3 (`3C-locks-v+1-diff.md:118`-`140`), 3C disposition matrix
row LAC-1E-14 (`3C-locks-crystallisation.md:32, :119`), 3A executive summary
(`3A-architecture-synthesis.md:39`), 3B coherence matrix row 1
(`3B-master-plan-reconciliation.md:179`), 3D fold row
(`3D-skinny-fold.md:174`), and 3E L14-HC-07 hardening clause
(`3E-grammar-generalisation.md:208`). Three independent lens detections
(CH2 evidence row F2; CH6 finding #4 + §4 row 1; CH7 finding §1 row 7
hygiene note) converge on the same artefact + same 3 sites.

**Repair shape:** mechanical 3F V2 rewrite mirroring 3C V4 hunk V4-3
wording — replace "5th BackendShape variant (gates Lock 1 + Lock 10
v+1)" with "5th admitted-product category at the Lock 1 substrate
manifest, NOT a 6th `BackendShape` variant; the 5-shape Lock 10 search
domain holds". CH2 open question at `:311` closes with 3C-selected
disposition.

### Convergence 2 — Meta-CH7 self-correction (LAC-1E-12 institutionalisation procedural failure)

CH7 §1 finding row 5 catches **the very LOCKS amendment being proposed**
(LAC-1E-12 promotion to LOCKS preface — the institutionalisation of
executable verification mandate per T-P1 V5 §6.1) failing its own
discipline at two sites:

1. **T-P2 cohort numerator 31:64 → 32:69 staleness** (T-P1 V1 CH7
   COH-012 counter-surface fabrication pattern recurrence in attenuated
   form). 3F carries `31:64` at 5 sites (lines 71, 107, 131, 280, 315);
   3C diff preface carries `31:69 = 31.7%` at line 69 (arithmetic
   broken: 31/69 = 44.9%, not 31.7%). Canonical T-P2 V3 figure per
   `HARDENING-T-P2-V3-CONSOLIDATED.md:76, 172, 187, 198, 295` is
   `32:69 = 31.7%`. The stale 31:64 propagates from V1 dispatch context
   transcription of V2-era value. **CH7 cannot let a stale dispatch-
   context number freeze a stale refutation-density transcription
   across V1 surface absorption** — the LAC-1E-12 mandate the 3C diff
   itself proposes to institutionalise.

2. **Pattern H census command vs file count**. 3C diff V4-7 hunk + 3B-MP-D03
   + 3F-MIG-003 bind `find crates/core/src/runtime -mindepth 2 -maxdepth 2
   -type f -name '*.rs' | wc -l` which returns **63** at HEAD
   `345c321409`. The source authority (`sk-v14-audit-overfit-pre-restart-pattern.md:56,
   :26`) uses unrestricted `find ... -name '*.rs' | wc -l` returning
   **67**. The 4-file gap lives at `google_sheets/document/{path_query,
   mod, canonical, view}.rs` depth 3. **The lock cannot bind a census
   command whose output (63) contradicts the file-count figure the lock
   asserts (67)** — this is exactly the "bound verification action that
   does not verify what the lock claims" failure mode LAC-1E-12 was
   promoted to prevent.

**Repair shape:** mechanical replacement (a) `31:64` → `32:69` at 5 sites
in 3F + `31:69 = 31.7%` → `32:69 = 31.7%` at 1 site in 3C diff; (b) drop
`-maxdepth 2` from bound command in 3C diff V4-7 + 3B-MP-D03 + 3F-MIG-003
to match source authority and produce 67-file output. Both repairs
preserve LAC-1E-12 institutionalisation by **actually honoring its
discipline in the amendment proposing it**.

### Convergence 3 — 3C V3-merged anchor mis-cite (CH6 finding #4)

3C V4 cites HEAD `34a28f5c1` as the "V3 hunks merged into LOCKS.md
post-V3 §3Z LOCK via Pass Omega CRUD" baseline at
`restart/audit/totality/p3/3C-locks-v+1-diff.md:14`, but
`git log --oneline restart/locks/LOCKS.md` shows the actual Pass Omega
CRUD-3 LOCKS amendment commit is `e12c5323d docs(omega-crud3): apply
locks v1.1 amendments`. `34a28f5c1` is the T-P2 V3 hardening LOCK commit
and does not touch LOCKS.md. The merge is real but the cite is the
wrong anchor commit. Additionally the disposition matrix repeats "Already
merged at HEAD; no v+1 delta" **twelve times** for V3-carried hunks
without per-hunk re-execution transcripts — the LAC-1E-12 executable
verification mandate (institutionalised by 3C-PREFACE-ch7-binding itself)
requires path:line + executable command + observed output, not assertion.

**Repair shape:** (a) replace HEAD anchor `34a28f5c1` with `e12c5323d`
throughout `3C-locks-v+1-diff.md:14` and V3-carried disposition matrix
rows; (b) for each V3-carried row, replace "Already merged at HEAD; no
v+1 delta" with a one-line `git grep`/path:line re-execution anchor at
HEAD pointing to surviving lock text; (c) add re-execution transcripts to
Convergence Log appendix so the no-silent-drop claim is itself executable
per the LAC-1E-12 mandate.

### Architectural discipline INTACT at V1 close

Despite three REVISE lenses, the architectural spine is preserved:

| Discipline axis | V1 state |
|-----------------|----------|
| 16-lock count | PRESERVED (LAC-1E-12 lands as preface, NOT Lock 17; 4 explicit "NOT Lock 17" disambiguation sites; 0 affirmative "Lock 17" claims) |
| 5-shape `BackendShape` canon at Lock 10 | PRESERVED (HEAD-verified at Lock 10 line 164; LAC-1E-14 lands at substrate-manifest level only) |
| Substrate-union invariant | STRENGTHENED via LAC-2F-V5-02 ELEVATION (textbook CH3-STRENGTHENING; CH5 ACCEPT) |
| Silent-drop census | 0 (3C disposition matrix 51 candidates; CH1 51 LAC row count verified; CH6 0 DEFER survives scrutiny) |
| DEFER dispositions | 0 (3C 38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER) |
| SKELETON DELETE REFUSED coherent | 3 artefacts (3A ARCH-3A-D10 + 3B MP-NW-SK14-SKELETON-DELETE-REFUTED + 3F-MIG-007) |
| Pattern H + CH7 preface clauses coherent | 6 artefacts cite Pattern H 67-file census + 9 sub-waves (3A + 3B + 3C + 3D + 3E + 3F) |
| LOCKS diff applies cleanly | `git apply --check --recount` exit:0 for 6 representative hunks |

---

## §5 — V2 fold packet (~16+ items; convergent across multiple lenses)

V2 fold packet enumerated per-artefact with severity. Heaviest folds at
3C (5 items) and 3F (5 items); single items at 3A/3D/3E; pair at 3B.

### 3A — 1 item (CH6 routing fix)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH6-3A** | CH6 finding #1 | REVISE | ARCH-3A-D06 two-cursor-vs-unified-cursor decision routes to "T-P3 §3C ratifies (a) OR (b)" but 3C V1's substrate-union elevation (LAC-2F-V5-02) addresses cross-call retention, NOT the two-cursor question. Routing target does not contain the disposition 3A expects. Either (i) 3C V2 adds `3C-L01-cursor-shape-ratification` hunk explicitly dispositioning 1A-DIV-008's two cursor types, OR (ii) 3A V2 reroutes ARCH-3A-D06 to Pass Omega Ω-A architecture intake with explicit blocker + gate. |

### 3B — 2 items (CH4 + CH1)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH4-3B-A** | CH4 REVISE-MINOR row 1 | MINOR | Pattern H W6 LOC variance reconcile to one canonical band cited identically across 3B-D3 (~11000 LOC), 3B-D9 (~2.0k LOC W6 + ~1.4k LOC W5), 3C-L14 (4000-8000 LOC), 3D-FOLD-011, 3F-MIG-003. Authority: SK-V14 SPEC §13 W6 budget (`restart/skinny/tranches/sk-v14/SPEC.md:243, 687-775`). Either ~11k = aggregate-with-rewire and 4-8k = consolidation-only (tag both with scope) OR one number must yield. |
| **F-V2-CH1-3B** | CH1 REVISE-CH1-3B-01 | LOW | MP-3B-V1-D03 cites `1E-locks-evidence.md:102, 125` for "LAC-1E-15 Pattern H 67-file recurrence vector"; `:102` is D-1E-15 receiver row, `:125` is LAC-1E-15 row itself. Citation should make pair-role explicit: `:125 (LAC-1E-15 source) + :102 (D-1E-15 receiver)`. Doc-only fix; Pass Omega CRUD-2. |

### 3C — 5 items (heaviest fold; CH6 + CH1 + CH7)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH6-3C** | CH6 finding #4 + §4 Convergence 3 | REVISE | V3-merged anchor mis-cite: replace HEAD anchor `34a28f5c1` with `e12c5323d` (Pass Omega CRUD-3 actual commit) throughout `3C-locks-v+1-diff.md:14` and V3-carried matrix rows. Replace 12 "Already merged at HEAD; no v+1 delta" bare assertions with per-hunk re-execution transcripts (path:line + `git grep` + observed output) per LAC-1E-12 mandate. Add transcripts to Convergence Log appendix. |
| **F-V2-CH1-3C-A** | CH1 REVISE-CH1-3C-01 | LOW | Frontmatter ambiguity: `proposed_deltas_count: 18` (hunks) vs `delta_summary.answered: [51 LAC IDs]` (candidates). Add `proposed_candidate_count: 51` and `proposed_hunk_count: 18` as separate frontmatter keys. |
| **F-V2-CH1-3C-B** | CH1 REVISE-CH1-3C-02 | LOW | V4-7 hunk-index off by 10 lines: target table says `:253` but hunk-body prose says `:263` (correct; paragraph closes at `:263` per HEAD verification). Pass Omega CRUD-3 consume prose target; correct table summary. |
| **F-V2-CH1-3C-C** | CH1 REVISE-CH1-3C-03 | LOW | V4-4 dual-target (Lock 6 `:115` + Lock 14 `:229`/`:231`) ships as two `diff` code blocks without `@@` hunk headers. Convert both into proper unified-diff hunks before applying via `git apply`. Semantically correct; syntactically incomplete. |
| **F-V2-CH7-3C** | CH7 finding §1 row 10 + §4 Convergence 2 part (2) | REVISE | Pattern H census command: change `find crates/core/src/runtime -mindepth 2 -maxdepth 2 -type f -name '*.rs' \| wc -l` (returns 63) to `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` (returns 67, matching source authority). Apply at 3C-diff V4-7 hunk body + 3B-MP-D03 + 3F-MIG-003. Add verify_action evidence cell on LAC-1E-15 disposition row noting corrected command returns 67 at HEAD. |

### 3D — 1 item (CH6)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH6-3D** | CH6 finding #5 + §6 cross-artefact row 7 | REVISE | SK-V12 CSS L4 `declaration_values_extended` row appears in BOTH §1 "Skinny wins" (ADMITTED-EVIDENCE) AND §2 "Skinny rejections" (DISPROVED) without reconciling note. §1 row 8 must add explicit cross-cite to §2 row 3 + 3B Wave Classification Ledger CSS L4 row at `:99` marking the SK-V12 admitted row as "historical-row-evidence-at-SK-V13 + AUDIT-FALSIFIED-at-SK-V14-audit-zero + reseat dependency on SK-V14 SPEC W8 R6". Mirror 3B's substrate-pillar-vs-row-admit distinction. |

*(Non-REVISE V2 task: cycle-counter heterogeneity normalisation — 3A/3B/3F frontmatter `cycle: V1`; 3C/3D/3E frontmatter `cycle: V4` (V3-baseline carry + per-artefact increment). Per CH1 §5 open question, V2 dispatch context must pin carry-cycle convention.)*

### 3E — 1 item (CH4)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH4-3E** | CH4 REVISE-MINOR row 2 | MINOR | 3E-D06 generated-fixture cost tail: ledger row caps now-cost at "120-260 docs/test" but impl receiver is vague ("receiver wave capped by S-P3 or explicit Omega handoff gate") without named S-P3 wave-id or numeric LOC cap. Either (a) cite concrete S-P3 wave-id (e.g., "SK-V15 Pass Alpha onboarding wave" or "SK-V14 W11 close + Pass Alpha entry"), or (b) explicitly tag as "not budgeted in this T-P3 delta; handoff gate at G-Omega-V2" per CH6 anti-engineered-defer. |

### 3F — 5 items (heaviest 3F fold; CH2 + CH6 + CH7 + CH4 + CH1 convergence)

| ID | Source lens(es) | Severity | Substance |
|----|-----------------|----------|-----------|
| **F-V2-CH2+CH6+CH7-3F-A** | CH2 REVISE-CH2-V1-01 + CH6 finding #4 + CH7 finding §1 row 7 (§4 Convergence 1) | REVISE (highest-yield V2 fold) | **3F-MIG-004 LAC-1E-14 misclassification triple-lens convergence at 3 sites** (:104 + §4:125 + open-question:311). Mirror 3C V4 hunk V4-3 wording: "5th admitted-product category at the Lock 1 substrate manifest, NOT a 6th `BackendShape` variant; the 5-shape Lock 10 search domain holds". Replace "gates Lock 1 + Lock 10 v+1" with "Lock 1 v+1 substrate-manifest amendment only; Lock 10 v+1 search domain unaffected". CH2 open question at `:311` closes with 3C-selected disposition + reroutes G-Omega gate to Ω-C ARCH-CRUD acceptance of substrate-category-not-shape carrier wording. |
| **F-V2-CH6-3F** | CH6 finding #6 | REVISE | 3F-MIG-004 paper-conditional "until T-P3 §3C disposes" removal: 3C V1 has ALREADY dispositioned LAC-1E-14 as ACCEPT in `3C-locks-crystallisation.md:32` (3C-L01-factstream-fifth-category). 3F V2 rewrite to: "Per 3C V1 disposition 3C-L01-factstream-fifth-category (LAC-1E-14 ACCEPT at `3C-locks-crystallisation.md:32`), CSS L4 fact-stream is the 5th *substrate* category at the Lock 1 manifest level". G-Omega gate language remains correct for spec-merge step, but 3F should not phrase §3C disposition as still-open. |
| **F-V2-CH4-3F** | CH4 REVISE-MINOR row 3 | MINOR | 3F-MIG-004 numeric W8 re-admit consumer-plane budget pin: ledger cell reads "80-180 docs/report" but wave alignment is "T-P3 §3C disposition + W8 re-admit". Either (a) pin numeric LOC envelope for W8 re-admit consumer-plane cost (CSS L4 fact-stream gate-consumed telemetry implementation), or (b) explicitly cross-reference 3C-L01-factstream-fifth-category budget (60-150 docs) and mark 3F-MIG-004 as doc-only-with-zero-impl-tail. |
| **F-V2-CH7-3F** | CH7 finding §1 row 5 + §4 Convergence 2 part (1) | REVISE | T-P2 cohort numerator 31:64 → 32:69 at 5 sites in 3F (lines 71, 107, 131, 280, 315). Re-attribute citation from "T-P2 V5 LOCK refutation density 31:64" to "T-P2 V3 LOCK refutation density 32:69 (`HARDENING-T-P2-V3-CONSOLIDATED.md:198`)" — V5 attribution at disposition level is sound; numerator/denominator pair must come from V3 CONSOLIDATED where cohort was actually counted. |
| **F-V2-CH1-3F** | CH1 REVISE-CH1-3F-01 | LOW | LAC-2F-V5-02 attribution alignment: 3C cites `HARDENING-T-P2-V3-CONSOLIDATED.md:182-192` for LAC-2F-V5-02 elevation evidence; 3F consumes `HARDENING-T-P2-V5-CONVERGED.md`. Both files exist; both citations resolve. Pass Omega CRUD-4 must align: pick V3-LOCK as binding-commit anchor OR V5-CONVERGED as synthesis-history packet consistently across cohort. |

**Total V2 fold packet count: 15 items + 1 cycle-counter normalisation pass = 16 items minimum.**

### V2 fold convergence shape

| Artefact | REVISE count | MINOR count | LOW count | Total V2 items |
|----------|--------------|-------------|-----------|-----------------|
| 3A | 1 | 0 | 0 | 1 |
| 3B | 0 | 1 | 1 | 2 |
| 3C | 1 | 0 | 3 | 4 + 1 (CH7-3C) = 5 |
| 3D | 1 | 0 | 0 | 1 |
| 3E | 0 | 1 | 0 | 1 |
| 3F | 3 | 1 | 1 | 5 |
| **Totals** | **6** | **3** | **5** | **15** (+ cycle-counter norm = 16+) |

Cross-lens convergence concentrates heaviest at 3F (3 REVISE) and 3C (4
items + 1 REVISE) — exactly where Pass Omega CRUD intake will consume the
LOCKS amendment and migration handoff. V2 fold is therefore not just
numerically heavy at 3C/3F but also positionally load-bearing for the
G-Omega merge surface.

---

## §6 — V2 dispatch shape — 6 artefacts touched

V2 dispatch covers 6 of 7 T-P3 artefacts (every artefact except 3C-diff
itself is touched once or more; 3C-cryst is touched for frontmatter
clarification and 3C-diff is touched for anchor + census command repair).

### Per-artefact V2 dispatch summary

| Artefact | V2 dispatch | Lens evidence | Receiver |
|----------|-------------|---------------|----------|
| `3A-architecture-synthesis.md` | 1 light fold (ARCH-3A-D06 routing) | CH6 #1 | 3A V2 author OR 3C V2 author (paired option) |
| `3B-master-plan-reconciliation.md` | 2 folds (Pattern H LOC reconcile + D03 anchor-pair) | CH4 + CH1 | 3B V2 author |
| `3C-locks-crystallisation.md` | 2 folds (frontmatter ambiguity + Pattern H census command) | CH1 + CH7 | 3C V2 author |
| `3C-locks-v+1-diff.md` | 4 folds (V3-merged anchor + V4-7 hunk-index + V4-4 dual-target + Pattern H census command) | CH1 + CH6 + CH7 | 3C V2 author + Pass Omega CRUD-3 |
| `3D-skinny-fold.md` | 1 fold (SK-V12 W1b §1↔§2 cross-cite) | CH6 #5 + §6 row 7 | 3D V2 author |
| `3E-grammar-generalisation.md` | 1 fold (D06 receiver pin) | CH4 | 3E V2 author |
| `3F-migration-handoff.md` | 5 folds (LAC-1E-14 misclassification × 3 sites + paper-conditional removal + W8 budget pin + 31:64 → 32:69 × 5 sites + V3/V5 attribution alignment) | CH2 + CH6 + CH7 + CH4 + CH1 | 3F V2 author — heaviest single-artefact V2 fold |

### V2 dispatch heaviness analysis

3F is the **single heaviest V2 artefact** with 5 distinct folds + 8+
text-site repairs. 3C-diff is the **second-heaviest** with 4 folds + 12
"Already merged" re-execution transcripts. Both are positionally
load-bearing for the Pass Omega CRUD intake + G-Omega merge surface.

V2 cycle hard cap: HARD CAP 20 min per artefact per orchestrator default
(7 artefacts × 20 min = 140 min cycle ceiling; SK-V14 default per
`restart/prompts/ORCHESTRATOR.md §3W`).

V2 dispatch convention pin (from CH1 §5 open question): V2 dispatch
context must specify cycle-counter unification ("all artefacts cycle V2
with per-artefact `baseline_carried_from_v3` field" OR "per-artefact
cycle preserved with cohort cycle in separate frontmatter row").

---

## §7 — Predicted §3Z LOCK trajectory

### V1 → V2 (highest-yield cycle)

V2 fold packet (~16 items + cycle-counter norm) discharges mechanically:

| Lens | V1 → V2 prediction | Mechanism |
|------|-------------------|-----------|
| CH1 CORRECTNESS | 95.7% → 100% | 5 LOW REVISE discharged: anchor-pair (3B-D03) + frontmatter (3C-A) + hunk-index (3C-B) + dual-target (3C-C) + V3/V5 attribution (3F) |
| CH2 GENERALITY | 87.5% → 100% | Single REVISE (3F-MIG-004 LAC-1E-14 misclassification at 3 sites) → mechanical mirror of 3C V4-3 wording; F2 evidence row flips ACCEPT |
| CH3 REGRESSION | 100% → 100% | No V2 dispatch — preserved at LOCK-grade ACCEPT-STRENGTHENING |
| CH4 COST | ACCEPT-WITH-MINOR → 100% | 3 V2 MINOR (Pattern H LOC + 3E-D06 receiver + 3F-MIG-004 budget) discharged numerically |
| CH5 HIDDEN COUPLING | 100% → 100% | No V2 dispatch — preserved at LOCK-grade ACCEPT |
| CH6 ANTI-PAPER-CLOSE | 69.2% → 100% | 4 REVISE discharged: 3A-D06 routing (paired option) + 3C V3-anchor + 3D SK-V12 cross-cite + 3F paper-conditional removal |
| CH7 OVERFIT-PRUNE | 70% → 100% | 2 REVISE doc-level executable discharged: T-P2 numerator 31:64→32:69 at 6 sites + Pattern H census `-maxdepth 2` removal at 3 sites |

**V2 cohort prediction:** 7/7 lenses ≥95% (first consecutive cycle).
Cohort sub-axis ≈100%. §3Z LOCK criterion first-cycle met.

### V2 → V3 (confirming cycle)

V3 confirms V2 ≥95% × 2 consecutive cycles. Predicted state:

| Lens | V2 → V3 prediction | Cycle lockedness |
|------|-------------------|-------------------|
| CH1 CORRECTNESS | 100% | 2-CYCLE LOCK (V2+V3) |
| CH2 GENERALITY | 100% | 2-CYCLE LOCK (V2+V3) |
| CH3 REGRESSION | 100% | 3-CYCLE LOCK (V1+V2+V3); deepest-LOCKED |
| CH4 COST | 100% | 2-CYCLE LOCK (V2+V3) |
| CH5 HIDDEN COUPLING | 100% | 3-CYCLE LOCK (V1+V2+V3) |
| CH6 ANTI-PAPER-CLOSE | 100% | 2-CYCLE LOCK (V2+V3) |
| CH7 OVERFIT-PRUNE | 100% | 2-CYCLE LOCK (V2+V3) |

**V3 cohort prediction:** §3Z COHORT LOCK ACHIEVED at V3 close. Ceiling
consumed: V3 = 3/5; 2-cycle margin preserved per T-P2 V3 LOCK precedent.

### V≤5 ceiling honored with margin

Predicted trajectory consumes V1 + V2 + V3 = 3 of V≤5 ceiling. V4 and V5
remain reserved for divergence (e.g., V2 fold surfacing unexpected
architectural drift — currently no signal of this in V1 evidence).
Two-cycle margin matches T-P2 V3 LOCK precedent exactly.

### Contingency: V2 surprises require V4

If V2 fold execution surfaces a new defect class beyond the 16-item V2
packet (e.g., per-hunk re-execution transcripts in 3C surface a 13th
V3-merged row drift, or 3F V2 LAC-1E-14 rewrite introduces a downstream
3F-internal coherence break), V4 confirming cycle would push LOCK to
V4 close (ceiling consumed: V4 = 4/5; 1-cycle margin). V5 reserved for
exceptional case. CH3 + CH5 already at LOCK-grade reduce the V4 surprise
surface to CH1/CH2/CH4/CH6/CH7 only.

---

## Appendix A — V1 lens file references

| Lens | File | Size (bytes) |
|------|------|--------------|
| CH1 | `restart/audit/totality/p3/hardening/V1/CH1.md` | 15597 |
| CH2 | `restart/audit/totality/p3/hardening/V1/CH2.md` | 18376 |
| CH3 | `restart/audit/totality/p3/hardening/V1/CH3.md` | 30352 |
| CH4 | `restart/audit/totality/p3/hardening/V1/CH4.md` | 15277 |
| CH5 | `restart/audit/totality/p3/hardening/V1/CH5.md` | 22553 |
| CH6 | `restart/audit/totality/p3/hardening/V1/CH6.md` | 22787 |
| CH7 | `restart/audit/totality/p3/hardening/V1/CH7.md` | 37109 |
| Context | `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md` | 4146 |

V0 baseline (six-lens pre-Omega): superseded by this V1 SK-V14 Pass Omega
seven-lens consolidator.

---

## Appendix B — disposition deltas at-a-glance (V1)

```
CH1 CORRECTNESS:    V1 = 95.7%  ACCEPT-with-5-LOW-REVISE  (citation hygiene)
CH2 GENERALITY:     V1 = 87.5%  REVISE                    (3F-MIG-004 LAC-1E-14 misclassification)
CH3 REGRESSION:     V1 = 100%   ACCEPT (incl. STRENGTHENING)  (LAC-2F-V5-02 textbook)
CH4 COST:           V1 = ~100%  ACCEPT-WITH-MINOR         (3 V2-MINOR folds)
CH5 HIDDEN COUPLING:V1 = 100%   ACCEPT                    (substrate-union preserved)
CH6 ANTI-PAPER:     V1 = 69.2%  REVISE                    (4 narrow paper-close fissures)
CH7 OVERFIT-PRUNE:  V1 = 70%    REVISE                    (T-P2 numerator + Pattern H census)
                    ───────────────────────────────────────
Cohort aggregate:   V1 ≈86% sub-axis; 4 ACCEPT / 3 REVISE / 0 REJECT
Orphan REVISEs:     15 (5 CH1-LOW + 1 CH2 + 3 CH4-MINOR + 4 CH6 + 2 CH7)
DEFERs in 3C matrix: 0 (38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER)
Ceiling consumed:   V1 (1/5; 4-cycle margin)
§3Z LOCK state:     NOT YET ACHIEVED (cycle 1 of 2 needed)
Predicted LOCK:     V3 close (consume 3/5 ceiling; 2-cycle margin)
```

---

## Appendix C — declared SK LOOP carry-forward

Per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP, this V1 declaration emits:

- **V1 CHALLENGE cycle close** for T-P3 cohort at 2026-05-23. **§3Z COHORT
  LOCK NOT YET ACHIEVED**; V2 fold required per §5 packet.
- **Cohort sub-axis ≈86%** with honest V1 shape (≥30% REVISE per PASS-3
  §3 anti-paper-close detector). 4 ACCEPT/ACCEPT-WITH-MINOR + 3 REVISE +
  0 REJECT.
- **Architectural discipline INTACT:** 16-lock count preserved; 5-shape
  `BackendShape` canon preserved; substrate-union STRENGTHENED via
  LAC-2F-V5-02; 0 silent drops; 0 DEFER dispositions; SKELETON DELETE
  coherent across 3 artefacts; LOCKS diff applies cleanly via `git apply
  --check --recount`.
- **V2 fold packet shape:** 16+ items across 6 artefacts; heaviest at
  3F (5 items) + 3C (5 items); cross-lens convergence at 3F-MIG-004
  (CH2+CH6+CH7 triple-lens) and meta-CH7 self-correction (LAC-1E-12
  institutionalisation procedural failure caught in the amendment
  proposing it).
- **V≤5 ceiling consumed:** V1 = 1/5; 4-cycle margin reserved.
- **Predicted trajectory:** V2 fold → V2 CHALLENGE → V3 confirming →
  cohort LOCK at V3 close (consume 3/5 ceiling; 2-cycle margin per T-P2
  V3 LOCK precedent).

Next required orchestrator move: dispatch V2 fold per §5 + §6 routing,
with cycle-counter unification pinned in V2 dispatch context per CH1 §5
open question.

---

*End HARDENING-T-P3-V1-CONSOLIDATED — V1 CHALLENGE Aggregator (§3Z LOCK pending V2 fold + V3 confirming).*
