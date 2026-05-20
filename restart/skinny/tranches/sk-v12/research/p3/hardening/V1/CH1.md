# SK-V12 S-P3 CHALLENGE V1 - CH1 CORRECTNESS

Disposition: REVISE.

## Basis

CH1 checks traceability, measurability, strict-plane comparator use, and
cross-file consistency. The S-P3 contract requires every candidate to trace to
S-P2 and S-P1, every gate to name measurable rows and concrete thresholds, and
every exit gate to compare against the SK-V12-open authority
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110`-`:115`). The
orchestrator requires every CH1 claim to resolve to file:line, RESULTS row, or
REDRESS entry, and requires measurable falsifiability gates
(`restart/prompts/ORCHESTRATOR.md:81`-`:84`).

The packet is directionally correct. C1-C3 trace to the generated non-JSON
baseline blocker from S-P1 and S-P2 (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:301`-`:320`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:14`-`:19`).
C4-C8 trace to S-P2 parser/support families and accepted S-P1 hot leaves
(`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`-`:38`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:31`-`:40`;
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47`-`:51`).
The JSON guard and residual floors match the SK-V12 goalset and REDRESS 119
tables (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:127`-`:147`,
`skinny/REDRESS.md:3508`-`:3522`). The revision items below block ACCEPT.

## Findings

1. Gate names drift between P3-C, P3-F, SPEC, and DISPATCH.

P3-C names W2 `G-W2-GENERALIZED-INTERVENTION` and W3
`G-W3-JSON-DIRECT-COMPANION` (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:50`-`:56`).
P3-F, SPEC, and DISPATCH instead name W2
`G-W2-SELECTED-NONJSON-INTERVENTION` and W3
`G-W3-CONDITIONAL-JSON-COMPANION` (`restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:103`-`:115`,
`restart/skinny/tranches/sk-v12/SPEC.md:462`-`:517`,
`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:97`-`:103`). This is a
contract conflict: an implementation agent cannot know which gate id is binding.

Required fold: make the packet use one gate-name set everywhere. Prefer the
SPEC/DISPATCH spellings:
`G-W2-SELECTED-NONJSON-INTERVENTION` and
`G-W3-CONDITIONAL-JSON-COMPANION`; update P3-C Section 2.1/2.5/2.6 and any P3-F
summary text accordingly.

2. W1 non-JSON row ids use incompatible workload vocabularies.

P3-A and P3-B name rows as `.../direct/main` or `.../typed/main`
(`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:75`-`:79`,
`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:292`-`:299`,
`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:114`-`:118`).
P3-C names the same target as
`{direct_to_struct|real_typed_struct}/main`
(`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:132`-`:137`).
P3-D's schema requires `row_id = {grammar_id}/{corpus_or_workload}/{workload}/main`
and `workload = direct_to_struct` or `real_typed_struct`
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:121`-`:128`).
SPEC currently carries the `direct/main` or typed-equivalent wording
(`restart/skinny/tranches/sk-v12/SPEC.md:171`-`:180`).

Required fold: choose one canonical W1/W2 row id grammar and apply it to P3-A,
P3-B, P3-C, P3-D, P3-F, SPEC, and DISPATCH. For measurability against the
existing workload vocabulary, use P3-D's form:
`css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`,
`sheets/formula/{direct_to_struct|real_typed_struct}/main`, and
`bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main`.

3. The W1 baseline throughput threshold is weakened in SPEC/DISPATCH.

P3-A and P3-C set concrete W1 baseline floors: generated Track 1 >= 1 Mbps,
independent Track 2/oracle >= 1 Mbps, and P3-C also requires sample count >= 30
(`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:78`-`:80`,
`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:147`-`:149`).
P3-F and SPEC/DISPATCH weaken this to "finite and positive" or `> 0`
(`restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md:95`-`:101`,
`restart/skinny/tranches/sk-v12/SPEC.md:399`-`:405`,
`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:107`-`:110`). That creates two
different W1 pass conditions.

Required fold: make SPEC Section 0.5 and Section 4, P3-F, and the DISPATCH
load-bearing facts state the concrete W1 floor: generated Track 1 >= 1 Mbps,
independent Track 2/oracle >= 1 Mbps, strict equality PASS, and sample count >=
30 unless S-P3 deliberately chooses a stricter floor.

4. Required telemetry field names drift between P3-D and SPEC.

P3-D defines the SK-V12 companion schema id and required fields, including
`schema_id`, `generated_track1_source_path`, `track1_artifact`,
`oracle_status`, `baseline_mbps`, `threshold_mbps`,
`benchmark_artifact_path`, and `gate_status`
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:105`-`:150`).
SPEC Section 0.4 uses a different required-evidence list, including
`track1_source_path`, `baseline_track1_mbps`,
`intervention_threshold_mbps`, `benchmark_artifact`, and
`fail_closed_gate_status`, while omitting `schema_id`, `track1_artifact`, and
`oracle_status` (`restart/skinny/tranches/sk-v12/SPEC.md:107`-`:151`). Since
P3-D says companion evidence must be gate-consumed and reject unknown schemas
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:152`-`:165`),
this naming drift makes the required telemetry non-canonical.

Required fold: make SPEC Section 0.4 either quote P3-D's
`sk-v12-nonjson-generated-v1` field set as the canonical companion schema, or
update P3-D and SPEC together to one exact field vocabulary. The gate command in
DISPATCH must name that same schema vocabulary and reject producer-only aliases.

## Accepted Checks

- The JSON guard floors are consistent across P3-A, P3-B, P3-C, SPEC, and the
  SK-V12 synthesis (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:308`-`:322`,
  `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:132`-`:157`,
  `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:73`-`:92`,
  `restart/skinny/tranches/sk-v12/SPEC.md:182`-`:201`).
- The conditional JSON direct residual floors match REDRESS 119 and remain
  correctly pre-blocked by default (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:207`-`:224`,
  `skinny/REDRESS.md:3508`-`:3524`).
- The outcome enum is consistent: the packet uses only `A C G I J K L M
  N-direct S` (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:93`-`:103`,
  `restart/skinny/tranches/sk-v12/SPEC.md:85`-`:105`,
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:147`-`:150`).
- Strict-plane discipline is present: P3-C requires strict same-run sonic direct
  evidence for any W3 direct residual reopen, and SPEC forbids permissive,
  lossy, stale, or output-plane-mismatched comparator evidence
  (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:225`-`:234`,
  `restart/skinny/tranches/sk-v12/SPEC.md:71`-`:83`,
  `restart/skinny/tranches/sk-v12/SPEC.md:233`-`:235`).

## Required Fold Summary

1. Canonicalize W2/W3 gate names across P3-C, P3-F, SPEC, and DISPATCH.
2. Canonicalize W1/W2 non-JSON row ids and workload names across the whole
   packet, preferably to P3-D's `direct_to_struct` / `real_typed_struct` form.
3. Restore the W1 concrete threshold in SPEC/DISPATCH: Track 1 >= 1 Mbps,
   Track 2/oracle >= 1 Mbps, strict equality PASS, sample count >= 30.
4. Canonicalize the companion telemetry schema field names between P3-D,
   SPEC Section 0.4, and DISPATCH's gate instructions.
