# S-P2 V4 CH3 - Regression / Benchmark Comparability Review

Role: CH3 (Regression).

Verdict: ACCEPT.

Score: 94/100.

## Blocking Findings With Refs

None.

## Notes

1. **The V3 stale-comparator regression is folded.** V3 blocked because SC-1,
   SC-2, SC-3, and SYNTHESIS still shifted simdjson DOM sidecar values into
   sonic-strict evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:39-72`).
   V4 now reads the authoritative `Delta vs sonic-strict` column and keeps
   simdjson DOM magnitudes parenthesized as sidecar/planning signals:
   SC-1 lists canada `+27.9%`, mesh `+21.4%`, citm_catalog `+24.6%`,
   update_center `-43.1%`, apache_builds `-28.2%`, github_events `-34.0%`,
   distinct_values `-61.2%`, and y_string_unicode `-54.1%`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:95-120`);
   SC-2 repeats the corrected strict-sonic values and explicitly labels the
   old shifted values as sidecar
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:248-259`);
   SC-3 uses the strict-sonic column for its substrate diagnosis and excludes
   larger simdjson DOM values from admission evidence
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:75-83`);
   SYNTHESIS preserves the same split
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:101-108`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:142-151`).

2. **`parse_only` can no longer be silently admitted as SOTA.** SPEC adds the
   `S` substrate-guard/non-SOTA spelling, keeps current `K` parse rows as
   substrate-guard non-admission by policy, and states that neither `K` nor `S`
   may support strict SOTA admission
   (`restart/skinny/tranches/sk-v8/SPEC.md:57-77`). The W0 goalset carries all
   17 `parse_only` rows as profile-bound substrate guards, not close
   contributors (`restart/skinny/tranches/sk-v8/SPEC.md:139-157`,
   `restart/skinny/tranches/sk-v8/SPEC.md:300-312`). SC-5 keeps the important
   regression property: demotion does not delete the losses or wins; every row
   retains same-run sonic deltas, sidecar/historical deltas, and named residuals
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:39-61`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:183-193`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:352-362`).

3. **The strict-vs-strict gate is executable, not advisory prose.** PASS-2
   requires strict-plane comparator discipline and treats permissive/lossy rows
   as flaw probes, never SOTA-beat anchors
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:214-219`). SPEC encodes the V4
   refusal rule: `gate-json` rejects strict admission unless comparator plane
   matches the row output plane, comparator strictness is strict, sidecar
   freshness is same-run or the comparator is a native same-run strict anchor,
   and validation happened inside the measured row
   (`restart/skinny/tranches/sk-v8/SPEC.md:44-55`,
   `restart/skinny/tranches/sk-v8/SPEC.md:117-123`). W0 must add the required
   telemetry, sidecar freshness validation, malformed-manifest rejection, and
   strict-admission refusal tests before later behavior waves can claim row
   quality (`restart/skinny/tranches/sk-v8/SPEC.md:287-312`).

4. **Measured-row proof now gates W3 benchmark comparability.** A selected W3
   parse candidate must prove strict validation, comparator evidence,
   structural cursor work, and admitted tape facts occurred in the measured row,
   not in a view-boundary, post-parse, sidecar, or comparator-only path
   (`restart/skinny/tranches/sk-v8/SPEC.md:473-489`). SC-2 requires measured-path
   proof and recomputes any copied threshold from post-W0 same-run strict rows,
   excluding stale sidecar magnitudes
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:362-378`).
   SC-3's Tier A table preserves the same requirement and distinguishes
   pre-W0 advisory commands from the enforcing post-W0 `gate-json` path
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:469-501`).

5. **No stale sidecar admission route remains.** HANDOFF says C++ comparator
   values are sidecar planning signals unless refreshed under a later same-run
   freshness gate (`restart/skinny/tranches/sk-v8/HANDOFF.md:33-40`). SPEC
   requires populated sidecar cells to have manifest coverage and missing
   sidecar values to be explicit non-admission values
   (`restart/skinny/tranches/sk-v8/SPEC.md:303-305`). `tape_vs_tape` is routed
   residual telemetry, not default W0/W1 scope, current SOTA evidence, or a W3
   production same-wave consumer
   (`restart/skinny/tranches/sk-v8/SPEC.md:125-131`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:79-85`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326-346`).

6. **The pre-blocked REDRESS surface is protected for regression.** PASS-2's CH3
   lens pre-blocks retained-parse/sidecar producers, digest cap-16 routes, stale
   canada mantissa-widen, Unicode/tiny-string/object-pair routes, PMULL, CSSC
   CTZ, and historical blocked routes unless fresh evidence and framing exist
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:109-117`). SPEC carries those as
   hard pre-blocked routes
   (`restart/skinny/tranches/sk-v8/SPEC.md:566-585`). SC-3 requires the union to
   prove it is not a renamed REDRESS 50 or 60-72 route by showing one retained
   tape, scan-written mandatory class identity, no surviving `StructuralIndex`,
   no parser-owned aux slots/fact cursors, scalar/checkasm parity, same-run
   no-regression rows, and no Tier A string-plane overclaim
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:528-543`).

7. **CH6's remaining wording blocker is not a CH3 regression blocker.** SC-1
   still has the "rejected or routed to a separate S-P3 proof" phrase in the
   grammar-class caveat (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:366-377`),
   and CH6 is right to require tightening. From this regression lens, that
   sentence does not reopen stale benchmark admission because it also forbids
   keeping the old `OffsetTape` recursive-descent producer beside the
   retained-index design, while SC-1 R2 rejects a number-heavy maintain miss for
   the current cycle and bars carry-forward promises
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:393-394`).
   SPEC independently requires W0/W1 closure, fresh W3 planning, exact owners,
   thresholds, measured-row proof, Lock 1 proof, and challenge acceptance before
   implementation (`restart/skinny/tranches/sk-v8/SPEC.md:417-489`).

8. **Minor cleanup candidates, not blockers.** SC-5 still contains pre-fold
   wording saying SPEC "currently freezes" `{A,C,G,K,L,N-direct}` and that
   adding `S` requires an amendment
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:220-232`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:347-351`).
   SPEC has already folded `S`, so this is stale explanatory prose rather than a
   regression aperture. SC-1 also has one conservative phrase that calls
   numbers/marine_ik/instruments "historical/no-anchor" planning rows despite
   the corrected table showing same-run sonic anchors
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:126-132`).
   The surrounding tables and packet gates preserve the actual comparator values
   and no-admission status, so this does not block CH3.

## Required Folds If REVISE

N/A. Verdict is ACCEPT.
