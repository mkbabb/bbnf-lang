# S-P2 V4 CH1 Correctness Review

Role: CH1 (Correctness)

Verdict: ACCEPT

Score: 95/100

## Blocking Findings

None.

## Notes

1. **V3 comparator-column drift is folded.** The authoritative `skinny/RESULTS.md`
   header orders deltas as `Delta vs SK-V6`, `Delta vs sonic-strict`, `Delta vs
   simdjson DOM`, then `Delta vs yyjson`
   (`skinny/RESULTS.md:3`). Current SC-1 now reports canada `+27.9%`,
   mesh `+21.4%`, citm_catalog `+24.6%`, update_center `-43.1%`,
   apache_builds `-28.2%`, github_events `-34.0%`, distinct_values `-61.2%`,
   and y_string_unicode `-54.1%` as same-run sonic-strict values, with the larger
   simdjson DOM sidecar deltas parenthesized as sidecar signals
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:95`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:120`).
   SC-2 repeats the corrected strict-sonic values and marks the old shifted
   magnitudes as sidecar (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:248`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:259`).
   SC-3 uses the authoritative `Delta vs sonic-strict` column for its substrate
   diagnosis and states that larger simdjson DOM magnitudes are not strict
   admission evidence (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:75`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:83`).
   SYNTHESIS likewise names canada/mesh/marine_ik as `+27.9%`, `+21.4%`,
   `+37.0%` sonic-strict substrate-guard signals and keeps canada `+54.6%` /
   mesh `+51.5%` as simdjson DOM sidecar planning signals
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:101`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:108`).

2. **The packet no longer treats simdjson sidecars as sonic-strict anchors.**
   SPEC classifies sonic-rs strict and serde_json as same-run strict anchors,
   while simdjson, yyjson, RapidJSON, and asmjson are sidecar planning signals
   unless refreshed under same-run rules (`restart/skinny/tranches/sk-v8/SPEC.md:44`-`restart/skinny/tranches/sk-v8/SPEC.md:55`).
   SYNTHESIS repeats the same comparator posture
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:213`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:224`).
   This satisfies the CH1 strictness-plane requirement in
   `restart/prompts/ORCHESTRATOR.md:81`-`restart/prompts/ORCHESTRATOR.md:88`
   and the S-P2 comparator-source requirement in
   `restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`restart/prompts/skinny/PASS-2-RESEARCH.md:100`.

3. **`parse_only` non-admission is folded packet-wide.** SPEC adds `S` as the
   post-W0 substrate-guard spelling and states that neither `K` nor `S` may
   support strict SOTA admission (`restart/skinny/tranches/sk-v8/SPEC.md:57`-`restart/skinny/tranches/sk-v8/SPEC.md:77`).
   The executable refusal rule rejects strict admission on plane mismatch,
   non-strict comparator evidence, stale sidecars, deferred strictness,
   view-boundary UTF-8, C++ sidecar-only evidence, or validation outside the
   measured row (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   W0 exit repeats that every current `parse_only` row remains substrate-guard
   non-admission telemetry and cannot count toward strict SOTA admission while
   `Strictness=deferred` or `parse_utf8=view-boundary`
   (`restart/skinny/tranches/sk-v8/SPEC.md:300`-`restart/skinny/tranches/sk-v8/SPEC.md:309`).
   SC-5 preserves the same distinction: `parse_only` is not a current strict
   SOTA gate, but its residual deltas remain visible as guard telemetry
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:179`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:210`).

4. **The two-cycle S-P2 guard is now packet-wide enough for correctness.**
   ORCHESTRATOR §3Z requires `>=95% ACCEPT` for two consecutive cycles with no
   unresolved critical defects, or an explicit user pin, before a pass advances
   (`restart/prompts/ORCHESTRATOR.md:104`-`restart/prompts/ORCHESTRATOR.md:123`);
   S-P2 repeats the same rule (`restart/prompts/skinny/PASS-2-RESEARCH.md:144`-`restart/prompts/skinny/PASS-2-RESEARCH.md:158`).
   SYNTHESIS states that V1, V2, and V3 did not converge and that a future V4
   ACCEPT would be only the first qualifying cycle after REVISE
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:91`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:99`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:188`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:191`).
   SPEC and HANDOFF repeat that one V4 ACCEPT would not automatically dispatch
   S-P3 (`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:457`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`).

5. **`tape_vs_tape` ownership is no longer hidden gate work.** SC-5 still prices
   the row as a possible 120-180 LOC comparator-plane augmentation with owner
   files, focused refusal tests, and one gate refresh
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:346`),
   but SPEC explicitly routes it as a residual outside default W0/W1 scope unless
   a later accepted plan adds owners, tests, LOC, and rerun budget
   (`restart/skinny/tranches/sk-v8/SPEC.md:125`-`restart/skinny/tranches/sk-v8/SPEC.md:131`).
   HANDOFF repeats that it is not default W0/W1 scope and cannot be W3's
   production consumer (`restart/skinny/tranches/sk-v8/HANDOFF.md:79`-`restart/skinny/tranches/sk-v8/HANDOFF.md:85`).

6. **No hidden deferral remains at CH1 severity.** SC-1's prior carry-forward
   wording is corrected in R2: if the number-heavy maintain budget misses, the
   current Tier A union candidate is rejected for this cycle; later
   reconsideration requires fresh W0 evidence plus a newly accepted S-P3/W3 plan
   and is not a carry-forward promise from S-P2
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:389`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:398`).
   SC-2 states both tiers remain unselected until S-P3/W3 supplies owner paths,
   revert protocol, numeric thresholds, strict same-run planes, and accepted
   challenge evidence (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:362`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:378`).
   SPEC says W3 is not selected by the nomination and requires a fresh plan plus
   challenge acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:452`).
   These fences satisfy ORCHESTRATOR CH6's no-deferral rule for this CH1 read
   (`restart/prompts/ORCHESTRATOR.md:83`-`restart/prompts/ORCHESTRATOR.md:88`).

7. **The one-substrate invariant remains coherent with PASS-2.** PASS-2 states
   that tape/direct-to-struct is one materialisation plan and avoids a second
   authoritative tree or parallel substrate
   (`restart/audit/pass-2-codegen/PASS-2.md:36`). SC-3 consumes
   `StructuralIndex` by move into a single retained `Tape`, with no query,
   clone, cache, parser-owned cursor, or post-build API surface
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:108`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:129`).
   SPEC's W3 exit gate preserves the same rule: a retained projection passes
   only if it replaces the offset-tape outright; a projection added beside the
   existing tape is a sidecar and fails
   (`restart/skinny/tranches/sk-v8/SPEC.md:483`-`restart/skinny/tranches/sk-v8/SPEC.md:489`).
