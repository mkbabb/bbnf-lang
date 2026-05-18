# SK-V9 S-P1 V1 Hardening Consolidated

Date: 2026-05-18.
Pass: S-P1 Profile.
Cycle: V1.
Input cohort: `restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md` through `p1f-results-delta.md`.
Challenge cohort: `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH1.md` through `CH6.md`.
Disposition: REVISE.
ACCEPT rate: 2/6 = 33.3%.
Convergence: not converged.

## Verdict

S-P1 V1 is an honest opening ledger, not a completed SK-V9 profile.

The cohort correctly uses the Alpha-closed current HEAD as orchestration
authority while preserving `skinny/RESULTS.md` as the measured `SK-V8-open`
telemetry authority. It does not invent SK-V9-open samply artifacts, PMU
counters, hot-leaf percentages, cycles-per-byte rows, Apache/CITM measured typed
rows, sidecar freshness, or direct-product proof.

That honesty prevents rejection, but it does not satisfy S-P1. The challenge
found pass-blocking evidence gaps: no SK-V9-open manifest, no fresh 17-corpus
samply captures, no resolved top-symbol self-time tables, no PMU/cycles rows,
no rendered masking-probe telemetry, and no fresh delta against a SK-V9 run id.

## Lens Dispositions

| Lens | Disposition | Confidence | Load-bearing finding |
|---|---|---:|---|
| CH1 Correctness | REVISE | 94% | Fresh SK-V9-open samply/PMU evidence is absent; current files are correct as gap reporting only. |
| CH2 Generality / Lock 14 | ACCEPT | 94% | No JSON-only primitive, directive, BIR, substrate, or generic policy leak is introduced. |
| CH3 Regression / REDRESS | ACCEPT | 94% | No REDRESS route is reopened if the packet stays an opening/gap ledger. |
| CH4 Cost / Reproducibility | REVISE | 88% | Method replay must split opening authority from fresh profile evidence and add a packet replay manifest. |
| CH5 Hidden Coupling | REVISE | 87% | Structural-scan, PMU, masking probes, and typed Track 2 require explicit diagnostic non-producer fences. |
| CH6 Anti-paper-close | REVISE | 96% | Normal S-P2 cannot consume absence-coded rows as primitive antecedents; W0 is mandatory before behavior waves. |

## Folded Requirements

1. S-P1 V1 must be cited only as an opening gap ledger. It is not a converged
   S-P1 profile and does not authorize primitive design.
2. The next executable action is a recovery W0 telemetry-lock wave: produce and
   consume a SK-V9-open report/gate manifest with `gate-json` as the same-wave
   consumer, behavior frozen.
3. W0 may update run identity, report labels, manifest validation, replay
   metadata, and diagnostic fences. It may not move parser/scanner/SIMD/codegen
   behavior, throughput cells, Apache/CITM measured row admission, direct
   product claims, or strict admission from deferred/view-boundary rows.
4. After W0, S-P1 must rerun against SK-V9-open evidence before behavior S-P2
   candidates are eligible: P1-A/P1-B/P1-C need fresh symbol-resolving samply,
   P1-D needs same-run PMU/cycles, P1-E needs actual hot-leaf attribution, and
   P1-F needs a fresh SK-V9 delta.
5. Until that rerun exists, S-P2 may only research the W0 telemetry-lock and
   diagnostic-fence problem. It must reject candidate primitives whose
   antecedent is `absent:*`, Criterion-slope-only, source-eligible-only,
   sidecar-historical-only, or stale fused SK-V4 profile evidence.
6. S-P3 must put W0 first in the wave manifest and hard-block behavior waves
   until W0 plus a revised S-P1 data cycle converge.
7. Structural-scan-only, masking probes, and cycles-per-byte are diagnostic
   non-producers. They cannot populate Track 1, Track 2, strict admission,
   product proof, Apache/CITM measured-row evidence, retained cursor state, or a
   parser-owned fact slot.
8. Typed direct Track 2 is `typed_oracle_independent`: an independent oracle
   lane, not a sidecar producer, row-moving comparator, or substitute for
   generated Track 1 DirectBuild.
9. The REDRESS pre-block ledger remains unchanged: no Apache/CITM measured-row
   overclaim, no Canada typed shortcut, no retained structural implementation
   before class/event grammar plus cursor proof, no scalar-parent direct fold,
   no sidecar substrate/public substrate API/new directive/BIR, and no generic
   JSON policy leak.

## Evidence Run During V1

- `cargo xtask check-json` passed.
- `cargo xtask check-real-typed` passed.
- `cargo xtask check-conformance` passed: 21 valid fixtures accepted, 7 invalid
  fixtures rejected.
- `cargo test -p bbnf-bench --lib --bins` passed: 69 tests.
- `cargo xtask lint-loc` failed on pre-existing budget debt:
  `crates/bbnf-bench` 9462/3300 LOC and `xtask` 1215/650 LOC.
- `cargo xtask gate-json --advisory --check-results` failed on existing cache
  coherence: `twitter SIMD metadata invalid: SIMD metadata is from a different
  capture`. This reinforces the W0 recovery requirement.

## Cycle Result

V1 is deliberately not closed. The fold target for V2 is not "paper-accept the
profile"; it is to make W0 telemetry-lock explicit, executable, and
same-wave-consumed, then rerun S-P1 on the SK-V9-open baseline before any
behavior wave can claim empirical ancestry.
