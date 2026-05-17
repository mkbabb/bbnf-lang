# CH3 - Regression Review, S-P2 Substrate-Ceiling V2

Role: CH3 REGRESSION.

Verdict: REVISE.

Score: 84/100.

## Blocking Findings

1. **`parse_only` demotion is not folded into the packet gate surface.**
   SC-5 correctly says `parse_only` must stop contributing to the SOTA
   scoreboard, become substrate-guard telemetry, keep all strict deltas and named
   residuals, and add an executable plane-mismatch refusal
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:170`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:216`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:231`).
   SPEC still freezes the outcome enum to `{A,C,G,K,L,N-direct}` and keeps the
   17 `parse_only` rows as W0 targets with later "candidate parse residual" posture
   instead of a substrate-guard non-SOTA class
   (`restart/skinny/tranches/sk-v8/SPEC.md:57`,
   `restart/skinny/tranches/sk-v8/SPEC.md:119`). SC-1's W3 table still labels
   candidate rows as strict same-run `parse_only` gates
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:301`),
   while SPEC's W3 exit gate only says selected parse rows cross a threshold and
   all 38 rows maintain budget (`restart/skinny/tranches/sk-v8/SPEC.md:423`).
   That leaves the V1 failure mode open: a W3 plan can treat `parse_only` as an
   admission row even though SC-5 says it is not a plane-honest SOTA gate.

2. **Strict comparator enforcement is described, but not executable enough to
   block deferred or mismatched rows.** The orchestrator requires strict-vs-strict
   admission and treats permissive rows as flaw probes only
   (`restart/prompts/ORCHESTRATOR.md:208`); the S-P2 prompt says lossy/permissive
   rows are never SOTA-beat anchors
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:214`). SPEC names comparator
   classes and required telemetry fields
   (`restart/skinny/tranches/sk-v8/SPEC.md:44`,
   `restart/skinny/tranches/sk-v8/SPEC.md:75`), but W0's gate only rejects missing
   fields, unsupported outcomes, and malformed sidecar manifests
   (`restart/skinny/tranches/sk-v8/SPEC.md:264`,
   `restart/skinny/tranches/sk-v8/SPEC.md:273`). It does not say
   `gate-json` must refuse strict admission when `comparator_plane` mismatches,
   `comparator_strictness` is not strict, or `sidecar_freshness` is not same-run.
   The current RESULTS rows are all `Strictness=deferred` and
   `parse_utf8=view-boundary` (`skinny/RESULTS.md:5`,
   `skinny/RESULTS.md:42`), and SC-5 explicitly acknowledges that deferred
   strictness is the present state (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:98`).
   Without an executable refusal, a candidate can pass by leaving validation or
   comparator work outside the measured path.

3. **Benchmark residual evidence is still mislabeled in SC-5, which weakens
   comparability.** SC-5 says bbnf is "faster than sonic-rs strict" on seven
   `parse_only` rows, but that list includes `unicode_escapes` by citing the
   simdjson DOM delta (`+113.6%`) even though the same row is `-34.6%` versus
   sonic strict (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:42`,
   `skinny/RESULTS.md:35`). The same paragraph also misnames citm's delta
   columns: current RESULTS shows citm `+24.6%` versus sonic strict and `-11.3%`
   versus simdjson DOM (`skinny/RESULTS.md:8`), while SC-5 describes those
   figures against the wrong anchors
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:43`).
   This conflicts with SC-4's own caution that only same-run sonic-strict rows may
   support strict admission and sidecar/SK-V6 rows are planning signals
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:178`);
   RESULTS likewise says C++ comparator columns are sidecar-only and do not count
   as same-run strict anchors (`skinny/RESULTS.md:219`). The residuals are
   visible, but the evidence taxonomy is still not regression-safe.

4. **The measured-path guard for the union candidate is incomplete.** SC-2 says
   strictness is preserved only if stage-1 validation remains strict in the same
   path (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:318`),
   and SC-5 says `tape_vs_tape` is telemetry only until same-run structural-index
   competitors exist
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:178`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:295`).
   SPEC correctly says `tape_vs_tape` is not the W3 production consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:405`), but it does not require W3
   selected rows to prove that UTF-8/control/escape validation, sparse tape facts,
   and structural-index competitor work are in the measured row rather than in a
   view-boundary, sidecar, or post-parse path (`restart/skinny/tranches/sk-v8/SPEC.md:423`).
   This is the remaining route for a wave candidate to pass by moving work out of
   the measured path.

## Non-Blocking Notes

- V2 substantially fixes the V1 sidecar regression. SC-3 now states a
  cardinality invariant, move-consumed `StructuralIndex`, mandatory scan-written
  class identity, and a narrowed facts column
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:115`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:178`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:190`).
- The REDRESS pre-blocks for parser-side aux tables, EventCursor sidecars, PMULL,
  and CTZ/bulk are correctly preserved in the cohort and packet
  (`skinny/REDRESS.md:715`, `skinny/REDRESS.md:756`, `skinny/REDRESS.md:2535`,
  `skinny/REDRESS.md:2573`; `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:297`).
- No-dispatch posture is preserved: G-Alpha authorizes only W0, and W1-W6 remain
  conditional on W0 closure and plan augmentation
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:67`,
  `restart/skinny/tranches/sk-v8/SPEC.md:536`).

## Required Fold Actions

1. Fold SC-5 into SPEC/HANDOFF/SYNTHESIS: make `parse_only` a substrate-guard,
   non-SOTA class or equivalent explicit non-admission state; keep all deltas and
   named residuals; remove "candidate parse residual" wording from `parse_only`
   rows unless the row is plane-matched by a later accepted gate.
2. Add an executable `gate-json` rule to SPEC: strict admission requires
   same-run strict comparator, matching `comparator_plane`, accepted
   `comparator_strictness`, and same-run or explicitly non-admission sidecar
   freshness. Rows with `Strictness=deferred` or view-boundary validation may be
   guard telemetry only.
3. Rewrite SC-1's W3 table so `parse_only` rows are maintain/substrate guards,
   not strict admission rows. `tape_vs_tape` may appear only as gate-binding
   telemetry until same-run structural-index competitor rows exist.
4. Correct SC-5 and any packet summaries that mix delta anchors. Separate
   same-run sonic/serde strict evidence from C++ sidecar evidence and historical
   SK-V6 deltas; remove `unicode_escapes` from the "faster than sonic strict"
   list unless the sonic-strict loss is shown separately.
5. Add W3 measured-path proof language: selected rows must include strict
   validation and tape-fact work in the measured path, not in view-boundary,
   post-parse, sidecar, or comparator-only code. No W3 admission may rely on
   moving work out of the row being measured.
