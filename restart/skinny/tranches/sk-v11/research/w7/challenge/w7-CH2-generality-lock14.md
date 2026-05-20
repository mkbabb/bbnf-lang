# SK-V11 W7 CH2 Challenge: Generality / Lock 14

Date: 2026-05-20.
Scope: W7 CH2 generality and Lock 14 challenge for SPEC Section 11.
Output: this read-only artifact only.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 11.
- `restart/skinny/tranches/sk-v11/research/w7/w7-plan-output-digest-entry-block.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R4-nonjson-host-sink.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R3-redress-preblocks.md`.
- `restart/skinny/tranches/sk-v11/research/w7/w7-R5-oracle-independence.md`.
- REDRESS 112 and 113 in `skinny/REDRESS.md`.
- W1a non-JSON gate/report context under `restart/skinny/tranches/sk-v11/research/w1a/`.
- W1b non-JSON baseline context under `restart/skinny/tranches/sk-v11/research/w1b/`.
- W7 owner paths named by SPEC Section 11, inspected read-only.

## CH2 Question

Should W7 block because there is no generated non-JSON host-sink baseline, or
does a grammar-neutral host-sink candidate already exist inside W7 owner paths?

## Adjudication

BLOCK.

No grammar-neutral non-JSON host-sink candidate exists inside the W7 owner
surface. W7 may preserve the W1a non-JSON schema-only gate as a regression
check, but it may not convert W1a fixtures, rejected W1b plans, or JSON digest
host-sink code into Lock 14 non-JSON admission evidence.

The governing reason is narrower than "there is no non-JSON report file." SPEC
Section 11 permits selected non-JSON oracle/report files only if W1b uses digest
output. That condition is false. W1b selected
`css_l4/declaration_values/direct/main` on
`css_l4_declaration_value_fact_bytes`, not digest output, and REDRESS 112
rejected W1b before any generated non-JSON Track 1 baseline, independent oracle,
benchmark row, report, gate schema, or `RESULTS.md` row was admitted. REDRESS
113 then blocks W2 because W2 may not create the first measurable non-JSON row
without W1b's baseline.

Therefore W7 cannot be the wave that creates the first generated non-JSON
baseline under a C8 host-sink label. Doing so would bypass the W1b/W2 authority
split and turn output-sink work into parser/baseline generalization.

## Owner-Path Findings

The W7 owner paths do not contain a live non-JSON host-sink candidate:

- `skinny/crates/bbnf-bench/src/report.rs` and
  `skinny/crates/bbnf-bench/src/bin/gate.rs` contain the W1a
  `--w1a-non-json-report` lane. That lane accepts only schema-only
  `S / NO-GO` evidence with `same_wave_consumer_class =
  non_json_gate_schema_only`; it rejects admission and baseline claims.
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`
  is a W1a fixture, not a generated parser baseline. Its oracle source is a
  W1a sentinel, not a same-plane generated Track 1 versus independent Track 2
  proof.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` and
  `skinny/crates/bbnf-bench/benches/json_parity.rs` are JSON direct digest
  surfaces. They can support a JSON C8 output-sink route only if W7 clears the
  hot-leaf and oracle gates; they do not supply non-JSON grammar generality.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` is JSON typed host
  product code, not a non-JSON generated direct/typed baseline.
- The live runtime inventory contains generated JSON plus `sheets_witness`, not
  generated `css_l4`, `sheets`, or `bbnf_self` parser modules.
- Codegen still routes runtime emission through
  `json_provider::ensure_runtime_profile`, which accepts only
  `backend.grammar_name == "json"`.

These facts leave no row/oracle pair that W7 can select as a grammar-neutral
non-JSON host sink.

## Lock 14 Analysis

W1a's Lock 14 contribution is gate-consumable schema generality only. That was
valid for W1a because W1a's job was to make non-JSON evidence machine-readable
and fail-closed. It was not generated parser authority.

W7 has a different burden. A W7 non-JSON host-sink admission would need an
already-existing generated non-JSON Track 1 row, an independent same-plane
oracle, strict output equality, same-run throughput, and a host-sink change
inside Section 11 owner paths. W1a does not provide those facts, and W1b/W2
explicitly failed to establish them.

Treating the W1a report lane as W7 host-sink evidence would be a Lock 14 failure
because it would count schema presence, gate-only consumption, JSON-provider
infrastructure, or hand/witness non-JSON artifacts as generated non-JSON
generality.

## Decision

Accept the W7 plan's non-JSON BLOCK rationale for CH2.

There is no REVISE candidate to name. A valid REVISE would have to identify an
exact generated non-JSON row, generated Track 1 source artifact, independent
same-plane oracle source, strict equality artifact, and W7 owner-file host-sink
consumer. The current tranche has none.

The blocked row remains:

- Row: no admissible W7 non-JSON row.
- Oracle: no admissible W7 non-JSON oracle.
- Files: no W7 owner file contains a generated non-JSON host-sink baseline.

W7 may continue only as a JSON C8 output digest/hash host-sink challenge if it
can independently clear SPEC Section 11's fresh hot-leaf, scalar source,
output-plane, oracle, floor, and guard requirements. Otherwise W7 should record
BLOCKED before source redress.
