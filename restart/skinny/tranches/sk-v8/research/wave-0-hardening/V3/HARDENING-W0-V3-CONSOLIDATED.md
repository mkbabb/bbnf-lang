# SK-V8 W0 Hardening V3 Consolidated

Date: 2026-05-18.

Reviewed target: `61d5d304 fix(sk-v8-wave0): fold hardening V2 gate blockers`.

## Verdict

REJECT.

The V3 cohort returned three ACCEPT reviews and three REJECT reviews. This is
not a qualifying convergence cycle under ORCHESTRATOR §3Z. W1-W6 remain
blocked until W0 folds the V3 blockers and then achieves two consecutive
qualifying ACCEPT challenge cycles at >=95% with no unresolved blockers.

| Reviewer | Verdict | Confidence | Primary lens |
|---|---|---:|---|
| CH1 | ACCEPT | 96% | Strict admission, outcomes, row identity, Lock 14 grammar-neutrality |
| CH2 | REJECT | 88% | Comparator-id allowlist and strict-admission freshness |
| CH3 | ACCEPT | 96% | Lock 14 frozen roots and directive/asm coverage |
| CH4 | REJECT | 93% | Capture/run-id stability, update semantics, SIMD metadata |
| CH5 | REJECT | 91% | SPEC/RESULTS consistency, row-set run id, sidecar manifest contract |
| CH6 | ACCEPT | 96% | Anti-paper-close and stale-evidence failure behavior |

## Accepted Folds

- V2 row identity is folded: W0 binds `sk_v8.row_id` to the rendered
  corpus/workload before report acceptance.
- Known native strict comparator evidence is folded: `sonic_rs_strict` and
  `serde_json` require workload-specific Criterion source paths, expected output
  planes, `strict`, `same-run-native`, `sidecar_freshness=n/a`, and finite Mbps.
- Known sidecar source/freshness shapes are folded for historical populated and
  explicit absence rows.
- Lock 14 frozen roots now cover directive/parser, runtime/tape, codegen, IR,
  passes, SIMD source/build/ext, parse-that-regex, and typed/direct bench
  surfaces; CH3 accepted this coverage.
- Main Criterion metadata coherence, check-only/update ordering, and volatile
  probe write rejection are materially improved.

## Blocking Findings To Fold

1. Comparator-id admission is not closed before strict-admission evaluation.
   `validate_w0_admission_boundary` can evaluate a populated extra comparator
   before comparator-id allowlisting. Because `StrictAdmissionEvidence` lacks a
   comparator id and still accepts `sidecar_freshness=sidecar-same-run`, an
   unrecognized comparator can create a paper strict-admission shape. Fold by
   rejecting unknown comparator ids before admission or by carrying id-aware
   strict evidence and allowing only accepted anchors.

2. `run_id` fingerprints the mutable Criterion root rather than the validated
   W0 row set. De-rendered volatile probes, unrelated Criterion groups, or later
   wave files can change every committed W0 run id even when no rendered main
   row changes. Fold by deriving `run_id` from validated W0 row inputs only.

3. SIMD scan metadata is appended after the capture-coherence validator and
   malformed SIMD metadata is silently optional. Fold by reading SIMD metadata
   fallibly and validating fixture hash/bytes, capture identity, workload,
   track/materialisation, strictness/output plane, and parity-hash semantics
   before any update path.

4. The SPEC/HANDOFF opening row facts and guard floors no longer match rendered
   `skinny/RESULTS.md`. Fold by reconciling the packet to the W0-rendered row
   state, or by explicitly archiving stale pre-W0 floors and making later waves
   consume only `SK-V8-open` derived rows/floors.

5. W0 still claims malformed sidecar-manifest rejection while populated
   sidecars are hard-coded historical strings and same-run sidecars are rejected
   because no structured manifest is admitted. Fold by either adding a structured
   sidecar-manifest parser plus malformed-manifest negative path, or by removing
   the manifest-rejection claim from W0 and declaring populated sidecars
   historical/non-manifest planning signals.

## Required V4 Entry

The next fold must include source or packet changes that address all five
blockers, plus focused negative tests or scripted checks for:

- unknown populated comparator strict-admission refusal;
- de-rendered volatile probe churn not changing the committed W0 `run_id`;
- malformed or mismatched SIMD metadata rejected before `--update-results`;
- packet row facts matching the current W0 `RESULTS.md`;
- the resolved sidecar manifest contract.
