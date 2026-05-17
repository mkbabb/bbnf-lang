# S-P2 V3 CH3 Regression Review

Role: CH3 - Regression.

Verdict: REVISE.

Score: 86/100.

## Blocking Findings

1. SC-1 still reopens the V2 strict-comparator blocker by quoting sidecar deltas as same-run sonic-strict evidence.

   The governing rule is strict-vs-strict only: P2-A must ground comparator deltas on the strict plane, and a permissive/lossy row is never a SOTA-beat anchor (`restart/prompts/skinny/PASS-2-RESEARCH.md:214`-`restart/prompts/skinny/PASS-2-RESEARCH.md:219`). V2 required correcting every comparator-delta claim and separating same-run sonic/serde strict anchors from simdjson/yyjson/RapidJSON sidecars (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:207`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:212`).

   SC-1 says canada and mesh are "same-run sonic strict" wins of `+54.6%` and `+51.5%` (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:101`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:104`), then summarizes them as strict wins (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:118`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:120`). The authoritative row has canada at `+27.9%` versus sonic-strict and `+54.6%` versus simdjson DOM (`skinny/RESULTS.md:10`); mesh is `+21.4%` versus sonic-strict and `+51.5%` versus simdjson DOM (`skinny/RESULTS.md:19`). SC-1 also carries the same column drift on citm_catalog (`-11.3%` is simdjson DOM, not sonic), update_center (`-63.4%` is simdjson DOM, not sonic), apache_builds (`-65.3%` is simdjson DOM, not sonic), github_events (`-61.7%` is simdjson DOM, not sonic), distinct_values (`-70.8%` is simdjson DOM, not sonic), and y_string_unicode (`-54.4%` is simdjson DOM, not sonic) (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:107`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:116`; `skinny/RESULTS.md:8`, `skinny/RESULTS.md:12`, `skinny/RESULTS.md:14`, `skinny/RESULTS.md:16`, `skinny/RESULTS.md:39`, `skinny/RESULTS.md:41`).

   Regression impact: V3 correctly demotes `parse_only` to substrate-guard/non-SOTA in SPEC (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`), but SC-1 leaves stale sidecar magnitudes wearing strict evidence labels. That preserves exactly the stale-comparator admission route CH3 was asked to close.

2. SC-2 still seeds the Tier A/W3 narrative with the same stale sidecar magnitudes while claiming a same-run strict parse plane.

   SC-2 says number-heavy corpora win as "canada (+54.6% vs sonic strict, 12 string quotes...), mesh (+51.5%...)" and says string-heavy corpora lose as "twitter (-35.8%), update_center (-63.4%), distinct_values (-70.8%), apache_builds (-65.3%)" (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:248`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:253`). Those are simdjson DOM deltas for canada, mesh, twitter, update_center, distinct_values, and apache_builds, not the sonic-strict deltas shown in `skinny/RESULTS.md:5`, `skinny/RESULTS.md:10`, `skinny/RESULTS.md:12`, `skinny/RESULTS.md:16`, `skinny/RESULTS.md:19`, and `skinny/RESULTS.md:39`.

   The same SC-2 file then states Tier A's row/plane targets are "Same-run strict JSON parse plane only" and must not rely on sidecar, view-boundary, post-parse, or stale comparator evidence (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:333`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:339`). That split-brain is blocking for benchmark comparability: the proof contract says strict-only, but the nearby candidate rationale still uses stale sidecar magnitudes. A future S-P3/W3 threshold can be copied from the wrong paragraph and move work out of the measured strict row.

## Non-Blocking Notes

- SPEC now has the right `parse_only` posture: `S` is an explicit substrate-guard/non-SOTA spelling, current `K` parse rows are treated as non-admission, and neither `K` nor `S` may support strict SOTA admission (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`; `restart/skinny/tranches/sk-v8/SPEC.md:298`-`restart/skinny/tranches/sk-v8/SPEC.md:301`).
- The packet has the needed executable refusal language: `gate-json` rejects strict admission unless comparator plane, strictness, freshness, and measured-row UTF-8/control/escape validation all hold (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`; `restart/skinny/tranches/sk-v8/SPEC.md:285`-`restart/skinny/tranches/sk-v8/SPEC.md:289`).
- The measured-path W3 guard is materially improved: selected rows must prove validation, comparator evidence, structural cursor work, and admitted tape facts occurred in the measured row, not in a view-boundary, post-parse, sidecar, or comparator-only path (`restart/skinny/tranches/sk-v8/SPEC.md:462`-`restart/skinny/tranches/sk-v8/SPEC.md:467`).
- Lock 1 and Lock 14 are mostly folded: V3 removes any `UnionTape` node/new substrate surface in the packet summary (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:147`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:157`), SC-3 confines facts to opaque generated ids interpreted by generated grammar modules (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:189`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:214`), and SPEC's Lock 14 gate enforces the same boundary (`restart/skinny/tranches/sk-v8/SPEC.md:239`-`restart/skinny/tranches/sk-v8/SPEC.md:261`).
- The convergence guard is correctly stated in HANDOFF: V1/V2 did not converge, a future single ACCEPT cycle is not enough, and SC-1..SC-6 authorize no W3 plan by themselves (`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:83`).

## Required Fold Actions

1. Correct SC-1 Section 1.2 to use the authoritative `Delta vs sonic-strict` column for every row. Keep simdjson DOM, yyjson, RapidJSON, asmjson, lossy/permissive, and historical SK-V6 values only as sidecar/planning signals. The corrected strict-sonic examples must include canada `+27.9%`, mesh `+21.4%`, citm_catalog `+24.6%`, twitter `-25.1%`, update_center `-43.1%`, apache_builds `-28.2%`, github_events `-34.0%`, distinct_values `-61.2%`, and y_string_unicode `-54.1%`.
2. Correct SC-2 Section 2 so the string-density/stage-2 rationale either uses strict-sonic deltas exclusively or labels sidecar deltas as diagnostic sidecar magnitudes. Do not write "vs sonic strict" beside simdjson DOM values.
3. Add one explicit guard near the SC-2 Tier A table: any S-P3/W3 row threshold copied from SC-1/SC-2 must be recomputed from post-W0 same-run strict rows and must not use stale sidecar magnitudes.
4. In the S-P3-ready proof tables, clarify that `gate-json --advisory` is acceptable only for pre-W0 research telemetry. A W3 admission proof must run the enforcing post-W0 `gate-json` path required by SPEC Section 0.4/Section 6.
