# SK-V11 W1a CH5 Challenge V2: Hidden Coupling And Lock 1

Scope: recheck CH5 after W1a plan revision.

Reviewed artifacts:

- `restart/skinny/tranches/sk-v11/research/w1a/challenge/CH5-hidden-coupling.md`
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4 and the Track 2/oracle
  independence language in Sections 0.3 and 2.3.

## Verdict

The revised W1a plan now satisfies the prior CH5 REVISE.

The previous blocker was that `track2_independence_status =
independent_verified` could have acted as self-attestation unless the gate also
consumed and classified the oracle source. The revised implementation plan adds
`validate_w1a_oracle_source`, requires accepted oracle/source artifacts to be
gate-owned W1a schema-only sentinels such as
`oracle:w1a:<grammar_id>:<corpus>:<workload>:<output_plane>`, and requires
rejection of generated Track 1, `generated_json`, SinkOnly/typed helper reuse,
benchmark-private parser reuse, runtime witness tests, JSON providers, stale
sidecars, old hand-runtime proof, and prose-only oracle evidence.

The revised gate matrix also makes the missing fixture explicit:
`nonjson-track2-shared-source` must fail even when
`track2_independence_status = independent_verified` if the source names
generated/shared/runtime/parser evidence. That is the source-provenance
predicate CH5 required, and it is now gate-consumed rather than report-only
metadata.

## CH5 Checks

| Check | Assessment |
|---|---|
| Gate-consumed source provenance | Satisfied. The implementation plan names `validate_w1a_oracle_source`, and the gate matrix requires the comparator/oracle source validator to read id, strictness, freshness, value, plane, source artifact, and source-provenance class. |
| Independent status cannot self-attest | Satisfied. `independent_verified` is accepted only with the allowed W1a schema-owned source sentinel; hidden coupling source classes remain rejected. |
| Required hidden-coupling fixture | Satisfied. Both the plan and matrix require `nonjson-track2-shared-source`, with the exact failure class from the prior CH5 REVISE. |
| Generated/runtime/shared parser exclusion | Satisfied as a plan contract. The rejection list covers generated Track 1, generated JSON, SinkOnly/typed helpers, benchmark-private parser reuse, runtime witness tests, JSON providers, stale sidecars, old hand-runtime proof, and prose-only oracles. |
| Same-wave gate consumption | Satisfied. The direct `bbnf-bench --bin gate -- --w1a-non-json-report` path must consume the fixture, and the pass/fail commands are listed as W1a verification. |
| Lock 1 separation | Satisfied for W1a's schema-only lane. W1a still claims no generated non-JSON baseline, parser authority, behavior row movement, or `RESULTS.md` row movement. |

## Acceptance Conditions To Preserve During Redress

- Keep `source_artifact` validation separate from
  `track2_independence_status`.
- Keep accepted source artifacts narrow; do not broaden them to arbitrary paths
  or prose without returning to CHALLENGE.
- Keep `nonjson-track2-shared-source` as a required failing gate fixture.
- Keep the W1a lane schema-only: no generated baseline authority, no behavior
  admission, no parser/runtime/codegen edits, and no `skinny/RESULTS.md` row
  movement.

DISPOSITION: ACCEPT
