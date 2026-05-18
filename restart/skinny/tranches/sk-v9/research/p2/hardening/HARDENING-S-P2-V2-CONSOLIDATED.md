# SK-V9 S-P2 V2 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V2 (post-fold of V1 CHALLENGE dispositions).
Inputs: `restart/skinny/tranches/sk-v9/research/p2/hardening/V2/CH{1..6}.md`.

## Verdict — Two lenses converged; four need V3

| Lens | V1 | V2 | 2-consecutive ≥95%? |
|---|---:|---:|---|
| CH1 CORRECTNESS | 96.7% | 98.2% | **✓ converged (V1+V2)** — 1 LOW REVISE carried |
| CH2 GENERALITY | 80.6% | 100% | ✗ 1 of 2 (V1 failed) |
| CH3 REGRESSION | 67.4% | 93.0% | ✗ below bar (3 RESIDUAL-REVISE) |
| CH4 COST | 22.7% | 100% | ✗ 1 of 2 (V1 failed) |
| CH5 HIDDEN COUPLING | ACCEPT | ACCEPT | **✓ converged (V1+V2)** |
| CH6 ANTI-PAPER-CLOSE | 68% | 90.6% | ✗ below bar (3 residuals) |

CH1 + CH5 converged. CH2 + CH4 cleared ≥95% at V2 but need a second
consecutive qualifying cycle (V1 failed). CH3 + CH6 are still below the
95% bar. A thin V3 surgical fold lifts CH3 + CH6 over the bar and the
V3 CHALLENGE re-verifies CH2 + CH4.

## V2 fold success

The two V1 load-bearing REJECTs were corrected honestly:
- **F1 (P2-D wiring)**: §2.1 now correctly states `unescape_uxxxx_x4_neon`
  IS wired at `parse-that-regex/src/lib.rs:402`. REDRESS 82 differential
  reframed to "broaden x4 batcher", not "wire it".
- **F2 (P2-E PMU)**: §6.1 rederived from `pmu_rows.tsv`; the fabricated
  baseline is gone. The honest downgrade is prominent — unicode_escapes
  fell from "PASS 100.5%" to "NEAR-FAIL 94.5%"; zero of four uncloseable
  rows admit on the codec alone. CH6 V2 called this "exemplary
  anti-paper-close discipline — the report surrenders its own headline."

## V3 fold requirements (surgical — all single-sentence)

### CH3 — 3 RESIDUAL-REVISE

1. **P2-D §5.3.1 EOR3 slice** — lacks an explicit six-row no-regression
   gate (the §4.4 CSSC CTZ slice has one; EOR3 omits it). Add the
   same W10b six-row maintain gate.
2. **P2-F §5.2** — names a REDRESS-33-pre-blocked pattern as an
   architecture lesson without an inline citation. Add the citation.
3. Cascade note — P2-D's four "block on P2-A OR fail CH5" slices create
   a wave-sequencing constraint S-P3 must honour. Already captured;
   ensure §0 footer states it.

### CH6 — 3 non-blocking residuals

4. **P2-D EOR3 latency cite** — the SHA3 `veor3q_u8` latency claim needs
   an ARM-ARM or Apple-doc citation.
5. **P2-F ContainerNext code cite** — the §5 ContainerNext architecture
   reference needs a file:line.
6. **P2-D §6.3 wording** — distinguish infrastructure-vs-per-primitive
   deferral (the load-bearing half folded into §6.2.1; the prose
   sentence survives stale).

### CH1 — 1 LOW REVISE

7. **P2-D REDRESS 28+33 line ranges** — cite the `REDRESS.md` line
   ranges (1241-1278 / 1314-1343), not just entry numbers.

### CH2 — 1 hygiene (non-blocking)

8. **P2-F** — anchor P1-V3-B §1.5 canonical vocabulary by path.

All eight folds are surgical, two files (P2-D, P2-F). No re-authoring,
no re-measurement.

## V3 path

1. Apply the 8 surgical edits to P2-D + P2-F.
2. Commit `docs(sk-v9-p2-v3): fold V2 CHALLENGE residuals`.
3. Re-dispatch CHALLENGE V3 (all six lenses).
4. Expected: CH3 + CH6 clear ≥95%; CH2 + CH4 re-verify ≥95% (second
   consecutive); CH1 + CH5 re-verify. S-P2 converges.

## Convergence forecast

V2 cleared 4 of 6 lenses ≥95% (CH1, CH2, CH4 — and CH3 at 93% is one
gate from clearing; CH6 at 90.6% likewise). The V3 fold is the smallest
of the SK-V9 cycle: eight single-sentence edits. V3 CHALLENGE expected
to converge S-P2 fully. S-P3 Synthesis-Plan dispatches after.
