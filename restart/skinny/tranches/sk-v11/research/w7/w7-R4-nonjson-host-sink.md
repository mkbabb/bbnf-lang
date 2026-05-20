# SK-V11 W7 R4: Non-JSON Host-Sink Viability

Date: 2026-05-20.
Phase: W7 Phase 1 research.
Scope: determine whether W7 can legally use selected non-JSON oracle/report
files or a non-JSON host sink after the W1b rejection, what baseline is
missing, and what would be required for non-JSON W7 admission.
Output: this file only.

## Finding

W7 cannot currently admit a non-JSON host-sink row under SPEC Section 11.

The W7 owner list permits "selected non-JSON oracle/report files if W1b uses
digest output" (`restart/skinny/tranches/sk-v11/SPEC.md:652-660`). That
condition is false in the live tranche:

- W1b selected `css_l4/declaration_values/direct/main` on
  `css_l4_declaration_value_fact_bytes`, not digest output
  (`restart/skinny/tranches/sk-v11/research/w1b/redress/w1b-redress-rejection.md:8-15`).
- W1b rejected before admitting any generated non-JSON Track 1, report, oracle,
  benchmark row, or `RESULTS.md` row
  (`restart/skinny/tranches/sk-v11/research/w1b/redress/w1b-redress-rejection.md:17-35`,
  `skinny/REDRESS.md:3311-3327`).
- W2 then blocked because the missing W1b baseline leaves
  `W1b_css_baseline_mbps` undefined and makes
  `G-W2-CSS-GENERATED-INTERVENTION` unmeasurable
  (`skinny/REDRESS.md:3340-3355`).
- W6 carried the block forward and admitted no non-JSON proof or reusable
  escaped-segment scalar oracle (`skinny/REDRESS.md:3434-3460`;
  `restart/skinny/tranches/sk-v11/HANDOFF.md:119-130`).

Therefore W7 may cite W1a/W1b non-JSON artifacts as research or preservation
evidence, and it may keep the W1a fixture gate green as a regression check, but
it cannot treat W1a fixtures or rejected W1b plans as W7 admission evidence.
The non-JSON host-sink route is not dispatchable unless a later accepted plan
creates explicit generated non-JSON baseline authority before W7 redress, or a
superseding Alpha/Pass-Omega contract reopens the owner surface.

## Legal Use Of Existing Files

### W1a report lane

W1a established only a schema/report validator. It explicitly did not create a
parser, baseline, generated output, row movement, or generated non-JSON
authority (`restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md:7-15`,
`restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md:41-57`).

The live W1a fixture is schema-only:

- `schema_version = "sk-v11-w1a-nonjson-v1"` and `wave_id = "SK-V11-W1a"`.
- `outcome_id = "S"` and `verdict = "NO-GO"`.
- `measured_validation_path = "schema-only"`.
- `same_wave_consumer_class = "non_json_gate_schema_only"`.
- oracle source sentinel `oracle:w1a:...`, not a measured generated parser
  source (`restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json:1-73`;
  `skinny/crates/bbnf-bench/src/report.rs:129-155`,
  `skinny/crates/bbnf-bench/src/report.rs:1796-1837`).

W7 may legally use this lane only as a preservation check, for example keeping
`bbnf-bench --bin gate -- --w1a-non-json-report .../nonjson-pass-css-l4.json`
green. It must not mutate W1a fixtures, relax the W1a validator, or convert a
W1a schema-only row into W7 admission.

### W1b artifacts

W1b research specified the report shape W7 would have wanted: a generated
non-JSON Track 1, independent same-plane Track 2/oracle, strict fact-byte
equality, source artifacts, and a sibling W1b gate mode
(`restart/skinny/tranches/sk-v11/research/w1b/w1b-R4-bench-gate-report.md:39-76`,
`restart/skinny/tranches/sk-v11/research/w1b/w1b-R5-independent-oracle.md:18-36`).
The W1b plan then hard-stopped because skinny could not produce the selected
generated non-JSON Track 1 inside the accepted owner surface
(`restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-implementation.md:17-35`,
`restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-implementation.md:120-152`).

Because no W1b report was admitted, W7 has no "selected non-JSON oracle/report
files" to consume for admission. W1b files are research authority, not live
benchmark authority.

### Host sink

SPEC Section 11 permits W7 to refactor or specialize only the output
digest/host sink, not parser semantics, and requires either selected direct rows
or a selected non-JSON host sink with strict output equality and at least a 1.0%
improvement (`restart/skinny/tranches/sk-v11/SPEC.md:663-685`).

For JSON, the W7 owner paths include the product sinks and JSON bench/report
surfaces. For non-JSON, W7 lacks the necessary Track 1 row and source authority.
The current skinny codegen and runtime state is still JSON-profiled:

- `json_provider::ensure_runtime_profile` accepts only
  `backend.grammar_name == "json"` (`skinny/crates/codegen/src/json_provider.rs:4-13`).
- `emit_with_layout` still calls that guard and emits JSON provider templates
  (`skinny/crates/codegen/src/lib.rs:102-136`).
- skinny runtime exports generated JSON plus the proof-gated
  `sheets_witness`, not generated `css_l4`, `sheets`, or `bbnf_self` runtime
  modules (`skinny/crates/runtime/src/lib.rs:1-16`;
  `skinny/crates/runtime/src/grammars/` inventory from REDRESS 112).

So a W7 "host sink" may target JSON direct/typed output sinks under the Section
11 owner set, but a non-JSON host sink cannot be the first generated non-JSON
baseline. Doing so would bypass REDRESS 112/113 and turn W7 into the W1b/W2
baseline/intervention wave it is not authorized to be.

## Missing Baseline

The missing baseline is not a report-file formatting problem. It is generated
non-JSON Track 1 authority.

Required missing pieces:

1. A generated non-JSON direct or typed parser row in skinny, preferably
   `css_l4/declaration_values/direct/main` unless a CHALLENGE-approved fallback
   switches to Sheets or BBNF-self.
2. Generated Track 1 source under an accepted non-JSON runtime/codegen owner
   surface, not the old `crates/core` runtime, not `sheets_witness`, not a
   benchmark-private parser, and not a JSON provider emission path.
3. An independent same-plane Track 2/oracle source that does not call generated
   Track 1, generated SinkOnly helpers, generated typed helpers, generated JSON,
   old hand-only non-JSON runtimes, or stale sidecars.
4. Strict output equality artifact on the selected output plane, with same-run
   Track 1 and oracle throughput, host/build/feature/sample metadata, run id,
   profile artifacts, and source artifacts.
5. Gate/report consumption for the W1b-style generated baseline or a
   superseding schema that explicitly replaces W1b while preserving W1a's
   schema-only validator.

Until these exist, W7 cannot name a non-JSON before/after floor, cannot prove
that a host sink is on the generated non-JSON hot path, and cannot close the
SK-V11 grammar-generalization axis.

## Requirements For Non-JSON W7 Admission

Under the current SPEC, non-JSON W7 admission is legal only if all of the
following are true before W7 redress:

- W3-W6 dispositions remain recorded and W7 CHALLENGE accepts a narrow C8
  output digest/hash or per-product host-sink route.
- A generated non-JSON Track 1 baseline already exists, or the SPEC is amended by
  an accepted plan with explicit owner authority to create that baseline.
- The selected row has a same-plane independent oracle/Track 2 and strict output
  equality. W1a schema-only fixture evidence is insufficient.
- The host-sink change touches output sink/report/gate files only; digest/hash
  state must not enter generic parser crates or parser semantics.
- The wave proves the host sink is the limiting hot leaf for the selected row
  with fresh profile evidence, then measures at least a 1.0% improvement for the
  non-JSON host sink with strict equality and guard preservation.
- The gate consumes every produced field in the same wave. Producer-only
  non-JSON telemetry, `gate_only`, W1a schema sentinels, and rejected W1b report
  shapes remain invalid.

If these cannot be satisfied, W7 should either stay on JSON output-sink rows or
record the non-JSON host-sink route as blocked and route the missing generated
non-JSON baseline to a future Alpha/Pass-Omega contract.

## Dispatch Implication

W7 R4 recommendation: do not plan a non-JSON W7 admission against the current
tranche state. The legal W7 choices are:

- JSON C8 output digest/hash host-sink work inside Section 11 owner paths, with
  REDRESS 54/55/66/69, 64, 82, 107, 108, 113, 116, and 117 carried forward; or
- a W7 blocked/rejected record for the non-JSON host-sink path, naming the
  absent generated Track 1 baseline and the false W1b-digest-output condition.

The non-JSON close axis remains unresolved until an accepted wave creates and
benchmarks a generated non-JSON direct/typed parser intervention with an
independent same-plane oracle.
