# SK-V18 S-P0 Overfit Audit — HARDENING CONSOLIDATED (§3Z verdict)

Date: 2026-05-31. Composing agent: A3.
Pass: S-P0 audit-overfit, seven-lens CHALLENGE (CH1 Correctness · CH2 Generality · CH3
Regression · CH4 Cost · CH5 Hidden-Coupling · CH6 Next-Tranche-Impact · CH7 Overfit-Prune)
over the S-P0 artefacts `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`. Per `PASS-0-OVERFIT-AUDIT.md`
§Procedure + `ORCHESTRATOR.md` §3W/§3Z (≥95% across CH1–CH7 for TWO consecutive cycles where
CH7 is the new lens, zero orphan REVISE, V≤5).

Live audit HEAD: `83b66db42`. Bracket contract HEAD: `318d9c046`.

## §1 — Cycle trajectory

| Cycle | Per-lens (A/R) | Aggregate | Posture |
|---|---|---|---|
| **V1** | CH1 9/1 · CH2 7/1 · CH3 5/1 · CH4 5/1 · CH5 5/1 · CH6 5/2 · CH7 6/0 | **85.7%** (42A/7R/0) | First pass; 7 single-edit tightening REVISEs / 0 REJECT; all folded |
| **V2** | CH1 10/0 · CH2 6/0 · CH3 6/0 · CH4 5/0 · CH5 6/0 · CH6 7/0 · CH7 7/0 | **100%** (47A/0R/0) | Post-fold confirm; all 7 V1 REVISEs DISCHARGED; zero orphan |
| **V3** | CH1 10/0 · CH2 6/0 · CH3 6/0 · CH4 5/0 · CH5 6/0 · CH6 7/0 · CH7 7/0 | **100%** (47A/0R/0) | 2nd confirm (independent re-grep); both `RuntimeOutputLabels` + `RuntimeFrontendRequirements` distinct structs corroborate the broadened R16; zero orphan |

Zero REJECT every cycle. The 7 V1 REVISEs (R1-CH1, R1-CH2, R1-CH3, R1-CH4, R1-CH5, R1-CH6,
R2-CH6 — CH6 carried two; CH7 raised zero) were single-edit, mechanism-correct,
non-architectural sharpenings, each folded into a0–a3 / SYNTHESIS, each DISCHARGED at V2 and
re-confirmed at V3:
- **R1-CH1** — a1 §L1 CSS-courier LOC: the disk-measured 910-line body span (`runtime_generator.rs:701`→`:1611`) supersedes the V3-seed "646–910" estimate; non-gate, descriptive.
- **R1-CH2** — a1 §L1 honest-finding escape (b) predicate restated as a per-primitive MACHINE mutate-falsifier (the primitive's emitted output must VARY under a `.bbnf` mutation), not "accepts a grammar-derived argument."
- **R1-CH3** — SYNTHESIS §5 fact 3 G2 dual entry-gate annotation (G2 entry-gates on BOTH G1 AND P3) + a2 §4 title/body directional fix ("G1/G3 co-derive; G3-failure blocks PROVE", never a backward "gates G1/G2").
- **R1-CH4** — the G1 ±5% line-count tripwire stated as SOFT; the binding cost-control is the `json_templates/` byte-for-byte oracle diff-match.
- **R1-CH5** — the R16 recipe pin broadened to inline EVERY nested-struct field (`frontend_requirements` field #11 AND `output_labels` field #12), with the one-line `RuntimeTarget: PartialEq` derive cost stated (`RuntimeTarget` derives only `Clone, Copy, Debug`).
- **R1-CH6** — SYNTHESIS R-A0-1 row carries the explicit REJECT clause (an unqualified "beats CSSOM"/"equal-work" close-report claim behind a re-label is a REJECT, per a0 §4).
- **R2-CH6** — SYNTHESIS R-A0-2 row carries the collapse-to-one disk answer (`generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets; manufacturing 7 fake roots is the forbidden overfit).

## §2 — §3Z VERDICT: CONVERGED = TRUE

The §3Z conjunction is MET:
1. **≥95% × 2 consecutive:** V2 100% + V3 100% — two consecutive ≥95% readings. ✓
2. **Zero orphan REVISE:** all 7 V1 REVISEs folded + DISCHARGED at V2, re-confirmed V3; V2/V3
   carry zero REVISE. ✓
3. **V≤5:** converged at V3. ✓
4. **CH7 (the new Overfit-Prune lens) is the spine, not a rider:** the 6 addenda ARE the CH7
   enforcement; CH7 100% at V2/V3. ✓

The S-P0 audit-overfit synthesis + the a0–a3 per-axis artefacts + the 6-addenda registry + the
PRUNE-list + the sequencing constraints are CONVERGED and binding. They are the input
S-P1/S-P2/S-P3 consume.

## §3 — What converged (the binding outputs)

- **The 6 addenda formalized as machine-checkable S-P0 lenses** (a1 L1–L6 registry, with a0 §1
  the goalset-residual restatement), each verified LIVE at HEAD `83b66db42`, each bound three
  ways (close-gate + §0.4 pre-block + telemetry the gate consumer REJECTs on), each carried as a
  forward REJECT trigger into every wave CHALLENGE.
- **The residual census R1–R16** (SYNTHESIS §2), every finding LIVE, mapped to a named PRUNE /
  GENERALIZE wave with a machine-checkable gate. ZERO CRITICAL/HIGH NEW residual; one NEW
  MEDIUM (R16, the nested-`output_labels` gate-recipe pin to S-P3).
- **The PRUNE-list** (SYNTHESIS §4): P1 x86 crate-wide · P2 warm-bench · P3 7-replica collapse ·
  P4 Lock-14 meaningful (BEFORE rebuild) · P5 metalang purge. Net ≈ −10800 LOC.
- **The sequencing constraints** (SYNTHESIS §5): PRUNE → GENERALIZE → PROVE → HONESTY; P4-before-
  G2/G3; S-P1-profile-before-G5/G6; the revert dependency graph (G1 failure blocks
  G2/G3/G4/PROVE; G2 dual-gates on G1 AND P3; G3 failure blocks PROVE); R16 recipe pin to S-P3.

## §4 — S-P0 hardening contributions (beyond confirming the goalset)

1. **Addendum 2 hardened to a 3-co-gate conjunction** — md5-distinctness is
   necessary-not-sufficient; the relocated-seam is caught only structurally.
2. **Addendum 4 axis-precision + test-exclusion + rich-nav preservation** — the phantom lens
   points at `G` not `K`; the ≥2 impl-count is necessary-not-sufficient.
3. **Addendum 6 retire-gated-on-measurement** — the G6 retire branch is an S-P1 samply
   obligation, not an assertion.
4. **NEW finding R16** — the `runtime_target_rows_collapsed` close-gate is SOUND by-exclusion,
   but its implementation recipe must recurse into EVERY nested-struct field — BOTH
   `frontend_requirements` (field #11, `RuntimeFrontendRequirements` at `grammar_provider.rs:46`)
   AND `output_labels` (field #12, `RuntimeOutputLabels` at `grammar_provider.rs:92`) — both
   distinct structs deriving `PartialEq, Eq` (the 3 prose-named `output_labels` fields are nested,
   not top-level). A recipe recursing into `output_labels` only would slip a future seam riding
   `frontend_requirements`. Pinned mechanism-agnostically to S-P3 at the full-expanded-row
   altitude; the cleanest mechanism (`RuntimeTarget: PartialEq`) costs one derive line
   (`RuntimeTarget` derives only `Clone, Copy, Debug`) and covers both nested structs
   automatically — so the gate consumer cannot author a shallow-compare false-green of EITHER.
5. **The P4-before-rebuild + S-P1-before-G5/G6 sequencing facts** made binding entry-gate
   dependencies, not advisory order.

## §5 — Forward posture

S-P0 converged CLEAN with no CRITICAL finding → forward motion proceeds (the PASS-0 failure-mode
halt does not trigger). The prune list IS the goalset's own PRUNE cluster (already CHALLENGE-
survived through Alpha V5), so S-P0 confirms cleanliness + hardens the addenda + pins R16 rather
than blocking. Next: S-P1 (profile the JSON+CSS hot leaves on the benched path, samply-gate the
G6 retire branch) → S-P2 (grammar-neutral projection candidate classes) → S-P3 (wave manifest +
executable `--skv18-generalization-report` gate consumer with the R16 full-row-collapse recipe +
the revert dependency graph + hard-cap defaults + the CH7 lens carried into every wave).

---

**TALLY: V1 85.7% (42A/7R/0) → V2 100% (47A/0R/0) → V3 100% (47A/0R/0). Converged = TRUE.**
§3Z met: ≥95% × 2 consecutive (V2+V3), zero orphan REVISE, V≤5, CH7 the spine. Zero REJECT all
cycles. The 6 addenda are the load-bearing output, formalized + live-verified + forward-binding.
