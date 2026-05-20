# SK-V11 W1a CH5 Challenge: Hidden Coupling And Lock 1

Scope: mandatory CHALLENGE lens CH5 for W1a after S-P3 V4 convergence.
Reviewed artifacts: `SPEC.md` Section 4, `DISPATCH-PROMPT.md` W1a references,
and W1a research/plan artifacts under
`restart/skinny/tranches/sk-v11/research/w1a/`.

## Verdict

W1a is correctly framed as a companion non-JSON gate/report evidence lane. The
plan preserves the JSON W0 validator, keeps `skinny/RESULTS.md` out of scope,
does not depend on `skinny/xtask`, and avoids codegen/runtime/parser/asm/grammar
behavior paths. Those choices materially reduce hidden coupling risk.

CH5 does not fully accept the plan as written. The remaining risk is Track
2/oracle independence: the plan requires `track2_independence_status =
independent_verified` and a nonempty source artifact, and it includes a coupled
status failure fixture, but it does not yet require the validator to prove that
the named source is outside generated Track 1, generated JSON, generated
SinkOnly/typed helpers, hidden benchmark-private parsers, or runtime witness
paths. A status string alone can become a self-attestation and would not satisfy
Lock 1 separation.

## Hidden Coupling Checks

| Check | Assessment | Required adjustment |
|---|---|---|
| Row id/domain mapping | Mostly safe. The plan binds `css_l4`, `sheets`, and `bbnf_self` to exact `_bench` domains and requires `<grammar_id>/<corpus>/<workload>/main`. It also chooses canonical `sheets`, not `google_sheets`. | Keep the exact mapping in one validator helper and reject alias spellings unless SPEC is updated in the same wave. |
| Track 2/oracle independence | Not yet sufficient. The plan rejects `track2_independence_status = coupled_to_track1`, but that only catches honest labels. It must also reject source artifacts classified as generated Track 1, `generated_json`, SinkOnly/typed helper reuse, benchmark-private parser reuse, runtime witness proof, stale sidecar, or prose-only oracle. | Add a gate-consumed source classification or path allow/deny predicate plus a failing fixture where status says independent but source points at a shared/generated parser path. |
| Report fields consumed by same-wave gate | Safe direction. `serde(deny_unknown_fields)`, required non-JSON structs, unknown-key rejection, duplicate row rejection, and report/gate tests address producer-only fields. | Preserve a fixture that adds an extra producer field and proves the new CLI fails. |
| No generated_json/runtime coupling | Safe boundary. The plan forbids generated output and excludes codegen/runtime/parser/grammar behavior edits. R4 explicitly blocks JSON provider emission, `generated_json`, and `sheets_witness` as non-JSON authority. | Add an explicit failing fixture or validator predicate for `source_artifact` values naming `generated_json`, `json_provider`, runtime witness tests, or old hand non-JSON runtime proof. |
| No `xtask` dependency | Safe. The plan moves the W1a CLI hook into `bbnf-bench --bin gate -- --w1a-non-json-report` and keeps `skinny/xtask/src/main.rs` outside the owner set. | Keep verification through the new bench gate path; `xtask gate-json` remains JSON preservation only. |
| No leaked JSON-only assumptions | Mostly safe. The companion report avoids relaxing schema-v3 JSON comparators, JSON row count, JSON run-id snapshot checks, and `RESULTS.md` rendering. | Ensure the non-JSON validator does not call JSON-only constructors such as W0 telemetry defaults or JSON comparator rules except as negative cases. |

## Required Changes Before Redress

1. Add one explicit CH5 failure fixture, for example
   `nonjson-track2-shared-source.json`, where
   `track2_independence_status = "independent_verified"` but the oracle/source
   artifact names a generated Track 1, `generated_json`, SinkOnly/typed helper,
   benchmark-private parser, runtime witness, or old hand-runtime path. The W1a
   gate must fail it.
2. Add a validator predicate that consumes source provenance separately from the
   independence status string. Accept only a narrow W1a schema-only oracle source
   class or path pattern; reject generated/runtime/shared/parser-owned sources
   and stale sidecar/prose-only sources.
3. Keep the implementation inside the planned owner paths:
   `skinny/crates/bbnf-bench/src/report.rs`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs`, and
   `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`. Do not route this
   through `skinny/xtask`, `skinny/RESULTS.md`, codegen, runtime, parser, asm,
   grammar behavior, or generated output.

After these changes, CH5 would accept the companion-lane shape because it would
fail closed on both explicit coupling labels and hidden source reuse.

DISPOSITION: REVISE
