# SK-V12 W4 PLAN-V4 - CSS Delimiter ASM Microbench Reject Branch

Date: 2026-05-21.
Phase: W4 Plan, V4 after CHALLENGE V3 REVISE.
Status: PLAN-V4 for CHALLENGE.

## V3 CHALLENGE Changes

PLAN-V4 folds every `challenge-v3/CONSOLIDATED.md` blocker:

- The default branch is explicitly **pre-production microbench-only**. It can
  record a measured ASM-gen route attempt for REDRESS/FIXPOINT evidence, but it
  does not claim a same-wave production consumer, strict fact-stream equality,
  CSS ADMIT, or SIMD/ASM production admission.
- Same-wave production consumer, strict equality, W4-current report/gate, and
  Lock 14 parent authorization move entirely to the rare production split after
  a passing microbench.
- Orphan final dispositions use SPEC close vocabulary only:
  `consumed`, `removed`, or `inventory_demoted_with_evidence`.
  Implementation facts such as `production_reachable_scalar_delegate` are
  evidence fields.
- Default verification is root-executable and touched-path scoped. It runs the
  new caller checkasm/microbench and a no-touch proof for JSON/report/gate
  roots. It does not run Lock 14, full JSON gate, or unrelated release
  checkasm unless those roots move.
- The microbench JSON producer is named by command and output environment
  variable.
- The default source/test slice is capped at 220 physical lines in the new test
  file. No production source file is owned by the default branch.

## Entry State

- W1b-2b REDRESS-125 provides the measured CSS ADMIT candidate: Track 1
  `429.34420791225705 Mbps`, lightningcss threshold `169.92962215656692 Mbps`,
  strict fact-stream equality PASS.
- W2 REDRESS-122 satisfies the `escape_mask_64` prerequisite. The default W4
  branch cites W2 but does not rerun W2 because it ships no production SIMD
  admission and no string/escape consumer.
- W4 CHALLENGE V3 accepted the semantic validity, Lock 14 legality, hidden
  coupling, and anti-paper-close shape of delimiter member-find.
- W3 is not required for the current CSS ADMIT path; W3 remains mandatory only
  for campaign FIXPOINT.

## Selected Candidate And Exact Caller

Selected SPEC Section 9 ASM-gen row:

`a64_ascii_set_run_skip`

Selected concrete caller:

`find_ascii_set_member64(bytes, cursor, end, set) -> usize`

Selected delimiter set:

```text
7b7d3b  # b"{};"
```

The candidate consumes the existing grammar-neutral
`bbnf_simd::prim::byte_class_from_eq_set_64` dispatch surface. On aarch64 this
is the real NEON body in `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`.
No public substrate API, directive, BIR variant, `BackendShape`, decoded-byte
sidecar, parser-owned sidecar, JSON template, generic runtime expansion, or x86
work is selected.

The default W4 branch evaluates the caller as a CSS-hot-leaf microbench
consumer, not as a production generated parser consumer. A production consumer
is legal only in the routed split after a microbench pass.

## Scalar Reference

```rust
fn find_ascii_set_member_scalar(
    bytes: &[u8],
    mut cursor: usize,
    end: usize,
    set: &[u8],
) -> usize {
    debug_assert!(cursor <= end && end <= bytes.len());
    debug_assert!(set.len() <= 8);
    while cursor < end && !set.contains(&bytes[cursor]) {
        cursor += 1;
    }
    cursor
}
```

Candidate equivalence requires:

1. `cursor <= end <= bytes.len()`.
2. Full 64-byte windows call `byte_class_from_eq_set_64`.
3. Zero mask advances by 64.
4. Nonzero mask advances by `mask.trailing_zeros()`.
5. Tails shorter than 64 bytes fall back to the scalar reference.

## Default Branch - Measured Microbench Reject

This is the expected W4 redress branch and the only in-cap default work. It
stays inside the 30-minute redress cap by not editing frozen CSS
template/runtime roots, JSON roots, `report.rs`, `gate.rs`,
`lock14_baseline.rs`, or `RESULTS.md`.

Allowed source/evidence surface:

- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
- `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `skinny/REDRESS.md`

Default branch hard cap:

- New caller test and microbench writer: `<= 220` physical lines.
- Production source edits: `0`.
- Generated source edits: `0`.
- Gate/report/RESULTS edits: `0`.

Outcome:

- If caller parity passes and the microbench records `decision=reject`, W4
  closes `MEASURED-REJECT` with ASM-gen attempt evidence.
- The branch does **not** claim:
  - same-wave production consumer;
  - strict CSS fact-stream equality;
  - CSS Track 1 movement;
  - SIMD/ASM production admission;
  - `RESULTS.md` movement.
- No production consumer is shipped, no new orphan is introduced, and REDRESS
  records that existing `byte_class_from_eq_set_64` production consumers remain
  unchanged.

This branch is a measured REDRESS attempt, not a behavior admission. If a later
close bracket needs a production consumer for FIXPOINT, W5/Pass Alpha must
route that explicitly instead of reinterpreting this branch.

## Rare Branch - Microbench Pass

If the microbench records `decision=pass` with
`candidate_speedup_ratio >= 1.01`, W4 halts the default redress at 0.9x cap or
earlier and records `ROUTE-PRODUCTION-SPLIT` with the microbench artifact. It
does not silently continue into production/gate work.

The production split must be separately planned and must own:

- CSS template/runtime production wiring for `scan_block`;
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` narrow
  `sk-v12-waveW4` parent-diff authorization and tests;
- W4-current report/gate schema/CLI/tests;
- fresh post-production Criterion lanes and equality artifacts;
- W2 prerequisite rerun if any string/escape/SIMD correctness surface moves.

No production PASS is allowed in the default W4 redress.

## Microbench Artifact

Required artifact:

`restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`

The artifact is emitted by the caller checkasm test when
`SKV12_W4_MICROBENCH_OUT` is set.

Required root-executable producer command:

```text
BBNF_SIMD_STRICT=1 \
SKV12_W4_MICROBENCH_OUT=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json \
RUSTFLAGS="-C target-cpu=native" \
cargo --manifest-path skinny/Cargo.toml test -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture
```

Required fields:

```text
schema_id=sk-v12-w4-delimiter-find-microbench-v1
wave_id=SK-V12-W4
selected_candidate=a64_ascii_set_run_skip
caller_api=find_ascii_set_member64
delimiter_set_hex=7b7d3b
fixture_sha256
synthetic_windows_sha256
sample_count
scalar_ns_per_iter
candidate_ns_per_iter
candidate_speedup_ratio
threshold_speedup_ratio=1.01
parity_status
decision=pass|reject
```

The test may use the existing `test_fixtures::sha256_hex` dev dependency to
populate `fixture_sha256` and `synthetic_windows_sha256`. No `Cargo.toml` edit
is authorized for the default branch.

Synthetic windows must include no-hit windows, first-hit-at-each-lane windows,
delimiter-heavy windows, high-bit bytes, every tail length 0..63, and the
frozen CSS fixture repeated into longer scan regions.

## Caller Checkasm / Parity

Required new test:

`skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`

Required cases:

- cursor positions, end bounds, and tails 0..63;
- no-hit windows and first-hit-at-each-lane windows;
- duplicate delimiter-set entries;
- high-bit bytes;
- frozen CSS fixture bytes;
- adversarial seeds `0xCAFEF00DBAADF00D`, `0x5441424c455f3634`, and
  `0xDEADBEEF12345678`;
- source immutability before/after candidate call;
- fallback behavior on non-aarch64 or unavailable target feature.

The test compares caller output against `find_ascii_set_member_scalar`, not
only the raw primitive mask.

Required default-branch verification:

```text
BBNF_SIMD_STRICT=1 \
SKV12_W4_MICROBENCH_OUT=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json \
RUSTFLAGS="-C target-cpu=native" \
cargo --manifest-path skinny/Cargo.toml test -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture

git status --short -- \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/codegen/src/json_templates \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/RESULTS.md
```

The second command must print nothing. If it prints a path, default-branch
redress stops and returns to CHALLENGE because the branch has left its touched
surface.

`cargo test -p bbnf-bench lock14`, the JSON gate, AWK guard proof,
`checkasm_byte_class_from_eq_set_64`, and `checkasm_escape_mask_64` are not
default-branch requirements. They become required only if production source,
JSON/report/gate roots, W2 surfaces, or the primitive implementation move.

## Production Split Requirements

These are not default W4 redress work. They are the mandatory contract if the
microbench passes and a follow-up production split is routed.

### Lock 14

The split must add `skinny/crates/bbnf-bench/src/lock14_baseline.rs` to the
owner path and authorize only this parent-diff subject:

```text
sk-v12-waveW4
```

Allowed W4 frozen-root paths must be no broader than:

```text
crates/codegen/src/css_l4_declaration_values_templates/generated.rs
crates/runtime/src/grammars/css_l4_declaration_values/generated.rs
```

Tests must prove `sk-v12-waveW4` rejects `json_templates`, generic runtime,
generic codegen, IR, passes, directives, BIR, `BackendShape`, and public
substrate paths.

### Strict Equality Artifacts

The split's W4-current report/gate must consume:

```text
schema_id=sk-v12-w4-asm-css-v1
wave_id=SK-V12-W4
run_id
redress_entry=REDRESS-126
selected_candidate=a64_ascii_set_run_skip
caller_api=find_ascii_set_member64
input_checksum
grammar_checksum
generated_track1_source_path
generated_runtime_path
track1_fact_artifact_path
cssparser_fact_artifact_path
lightningcss_fact_artifact_path
strict_equality_artifact_path
lightningcss_equality_artifact_path
fact_stream_sha256
track1_mbps
track2_or_oracle_mbps
lightningcss_mbps
threshold_mbps=lightningcss_mbps+1
admission_margin_mbps
strict_output_equality=pass
three_way_equality=pass:track1=cssparser=lightningcss
same_wave_consumer_class=generated_css_scan_block_delimiter_find
lock16_status=pass:scalar+checkasm+caller_parity+microbench+consumer
scalar_reference_status=pass:find_ascii_set_member_scalar
checkasm_or_parity_status=pass:byte_class+caller_find+escape_mask
microbench_artifact=restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json
json_guard_state
orphan_disposition_path=restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md
```

It may compare against REDRESS-125 as baseline evidence, but it must consume
fresh post-production Criterion lanes and equality artifacts.

## Orphan Disposition Output

Redress must write:

`restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`

Required per-row fields:

```text
orphan_name
evidence_status
consumer_path | no-production-consumer
lock16_status
redress_entry
source_grep_evidence
test_or_checkasm_evidence
redress_adjacency
material_differential
selected_by_w4
final_disposition=consumed|removed|inventory_demoted_with_evidence
```

Required final fields:

```text
selected_candidate=a64_ascii_set_run_skip
selected_candidate_orphan_accounting=separate_from_five_row_orphan_set
orphan_count=0
json_guard_state
css_gate_state
```

Planned dispositions:

| Candidate | Final disposition | Required evidence |
|---|---|---|
| `bitmap_prefix_xor_64` | `inventory_demoted_with_evidence` | `evidence_status=production_reachable_scalar_delegate`; `consumer_path=runtime/src/grammars/json/scan.rs -> bbnf_simd::prefix_xor_64`; `lock16_status=scalar_delegate_no_new_admission`; REDRESS adjacency `88`; no new production admission. |
| `bitmap_next_set_bit` | `inventory_demoted_with_evidence` | `evidence_status=no_non_test_consumer_found`; `no-production-consumer`; source grep evidence; checkasm evidence if present; REDRESS adjacency `89`; no new production admission. |
| `bulk_emit_positions_64` | `inventory_demoted_with_evidence` | `evidence_status=production_reachable_scalar_delegate`; `consumer_path=bbnf_simd::compact_mask -> prim::bulk_emit_positions_64`; `lock16_status=scalar_delegate_no_new_admission`; REDRESS adjacency `89`; no new production admission. |
| `byte_context` | `inventory_demoted_with_evidence` | `evidence_status=support_test_only`; `no-production-consumer`; source grep evidence; test/smoke evidence; no production admission. |
| `cache_hints` | `inventory_demoted_with_evidence` | `evidence_status=support_test_only`; `no-production-consumer`; source grep evidence; test/smoke evidence; no production admission. |

`production_reachable_scalar_delegate`, `no_non_test_consumer_found`, and
`support_test_only` are evidence details only. They are not final dispositions.

## Owner Paths

Default microbench-reject branch may edit only:

- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
- `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `skinny/REDRESS.md`

The default branch must not edit:

- `skinny/crates/bbnf-simd/Cargo.toml`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`

Those paths are production-split ownership only after a microbench pass and
separate CHALLENGE acceptance.

## Exit Mapping

- `MEASURED-REJECT`: caller checkasm/parity PASS; microbench
  `decision=reject`; orphan-disposition evidence records `orphan_count=0`;
  JSON/report/gate no-touch proof is empty; no production source remains. This
  is the expected W4 close and records a measured ASM-gen route attempt only.
- `ROUTE-PRODUCTION-SPLIT`: caller checkasm/parity PASS; microbench
  `decision=pass`; W4 halts before production wiring and records the routed
  production/gate split.
- `BLOCKED`: caller parity fails, the microbench artifact cannot be produced,
  no-touch proof fails, or orphan disposition cannot honestly reach
  `orphan_count=0`.

No `BEHAVIOR-PASS-CSS-ADMIT` is claimed by the default W4 branch. The existing
CSS ADMIT candidate remains REDRESS-125 and is promoted only by W5 if all close
conditions hold.

## Revert Protocol

If a source attempt is rejected:

1. Save `/tmp/skv12-waveW4-rejected.patch`.
2. Revert behavior/source edits if any production source moved.
3. Retain the caller test only if it is parity-green and CHALLENGE accepts it as
   evidence; otherwise revert it and keep the rejected patch.
4. Retain pure evidence artifacts and REDRESS documentation.
5. Commit measured REDRESS evidence as:

`docs(sk-v12-waveW4-redress): reject CSS delimiter ASM microbench`
