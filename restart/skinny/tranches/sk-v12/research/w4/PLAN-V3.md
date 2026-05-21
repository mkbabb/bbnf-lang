# SK-V12 W4 PLAN-V3 - CSS Delimiter ASM Microbench And Orphan Disposition

Date: 2026-05-20.
Phase: W4 Plan, V3 after CHALLENGE V2 REVISE.
Status: PLAN-V3 for CHALLENGE.

## V2 CHALLENGE Changes

PLAN-V3 folds every `challenge-v2/CONSOLIDATED.md` blocker:

- W4 is now **microbench-reject-first**. The in-cap redress objective is caller
  parity, isolated microbench, orphan disposition, JSON no-touch guard, and
  REDRESS evidence. Production wiring is not attempted unless the microbench
  passes early enough to route a separately budgeted production/gate split.
- A5's layout `skip_ws_and_comments` run-skip framing is superseded for W4
  redress. W4 selects CSS `scan_block` delimiter member-find only.
- The caller-level checkasm command is named explicitly:
  `checkasm_ascii_set_member_find_64`.
- The production PASS branch, if ever routed, must include W4-current
  W1b-style equality artifacts: Track 1, cssparser, lightningcss fact files,
  fact-stream SHA-256, run id, input/source checksums, and gate-consumed
  equality status.
- Lock 14 owner authorization is split: microbench-reject redress does not edit
  frozen CSS template/runtime roots and needs no W4 parent-diff authorization;
  any production split must own a narrow `lock14_baseline.rs` authorization for
  `sk-v12-waveW4`.
- `orphan-disposition.md` now requires explicit per-row `consumer_path`,
  `lock16_status`, and `redress_entry` accounting.

## Entry State

- W1b-2b REDRESS-125 provides the baseline CSS ADMIT candidate:
  Track 1 `429.34420791225705 Mbps`; lightningcss threshold
  `169.92962215656692 Mbps`; strict fact-stream equality PASS.
- W2 REDRESS-122 satisfies the `escape_mask_64` prerequisite.
- W4 CHALLENGE V2 accepted the semantic validity of delimiter member-find for
  CSS `scan_block`; remaining concerns are evidence/cost/ownership.
- W3 is not running concurrently with W4 and is not required for the current
  ADMIT path; W3 remains mandatory only for FIXPOINT.

## Selected Candidate And Exact Caller

Selected SPEC Section 9 ASM-gen row:

`a64_ascii_set_run_skip`

Selected concrete caller:

`find_ascii_set_member64(bytes, cursor, end, set) -> usize`

This is the delimiter member-find member of the ASCII set-scan family. It is
not the A5 layout/trivia `skip while member` API. For W4 redress the selected
set is the generated CSS `scan_block` delimiter set:

```text
7b7d3b  # b"{};"
```

The candidate consumes the existing grammar-neutral
`bbnf_simd::prim::byte_class_from_eq_set_64` dispatch surface. On aarch64 this
is the real NEON body in `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`.
No new public substrate API, directive, BIR variant, `BackendShape`, decoded
byte sidecar, parser-owned sidecar, JSON template, generic runtime expansion,
or x86 work is selected.

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

## Redress Cost Split

### Default in-cap branch: measured microbench reject

This is the expected W4 branch. It stays inside the 30-minute redress cap by
not editing frozen CSS template/runtime roots, `report.rs`, `gate.rs`, or
`RESULTS.md`.

Allowed source/evidence surface:

- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
- `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `skinny/REDRESS.md`

Outcome:

- If caller parity passes and the microbench records `decision=reject`,
  W4 closes `MEASURED-REJECT` with ASM-gen attempt evidence, not SIMD/ASM
  admission.
- No production consumer is shipped, no new orphan is introduced, and
  REDRESS records that the existing `byte_class_from_eq_set_64` production
  consumers remain unchanged.

### Rare branch: microbench pass

If the microbench records `decision=pass` with
`candidate_speedup_ratio >= 1.01`, W4 does **not** silently continue into an
unbudgeted production/gate rewrite. At 0.9x cap or earlier it records
`ROUTE-PRODUCTION-SPLIT` with the microbench artifact and routes a follow-up
production/gate slice for W5/Pass Alpha/SK-V13 planning.

The production split must be separately planned and must own:

- CSS template/runtime production wiring for `scan_block`;
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` narrow
  `sk-v12-waveW4` parent-diff authorization and tests;
- W4-current report/gate schema/CLI/tests;
- fresh post-production Criterion lanes and equality artifacts.

No production PASS is allowed in the default W4 redress unless CHALLENGE V3
explicitly accepts the cost as already paid and the implementation stays within
the cap.

## Microbench Artifact

Required artifact:

`restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`

Required fields:

```text
schema_id=sk-v12-w4-delimiter-find-microbench-v1
wave_id=SK-V12-W4
selected_candidate=a64_ascii_set_run_skip
caller_api=find_ascii_set_member64
delimiter_set_hex=7b7d3b
fixture_sha256=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374
synthetic_windows_sha256
sample_count
scalar_ns_per_iter
candidate_ns_per_iter
candidate_speedup_ratio
threshold_speedup_ratio=1.01
parity_status
decision=pass|reject
```

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

Required default-branch commands:

```text
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

`cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture` is required only if
redress edits CSS fixture/comparator/reporting code. The default microbench
reject branch does not.

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
orphan_status
consumer_path | no-production-consumer
lock16_status
redress_entry
source_grep_evidence
test_or_checkasm_evidence
redress_adjacency
material_differential
selected_by_w4
final_disposition
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

| Candidate | Required accounting |
|---|---|
| `bitmap_prefix_xor_64` | `consumer_path=runtime/src/grammars/json/scan.rs -> bbnf_simd::prefix_xor_64`; `orphan_status=production_reachable_scalar_delegate`; `lock16_status=scalar_delegate_no_new_admission`; REDRESS adjacency `88`. |
| `bitmap_next_set_bit` | `no-production-consumer`; `orphan_status=no_non_test_consumer_found`; checkasm evidence required; REDRESS adjacency `89`. |
| `bulk_emit_positions_64` | `consumer_path=bbnf_simd::compact_mask -> prim::bulk_emit_positions_64`; `orphan_status=production_reachable_scalar_delegate`; `lock16_status=scalar_delegate_no_new_admission`; REDRESS adjacency `89`. |
| `byte_context` | `no-production-consumer`; `orphan_status=support_test_only`; aarch64 primitive smoke/checkasm evidence required. |
| `cache_hints` | `no-production-consumer`; `orphan_status=support_test_only`; aarch64 primitive smoke/checkasm evidence required. |

## Owner Paths

Default microbench-reject branch may edit only:

- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
- `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `skinny/REDRESS.md`

The default branch must not edit:

- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`

Those paths are production-split ownership only after a microbench pass.

## Exit Mapping

- `MEASURED-REJECT`: caller checkasm/parity PASS; microbench `decision=reject`;
  orphan-disposition evidence records `orphan_count=0`; JSON guards hold; no
  production source remains. This is the expected W4 close.
- `ROUTE-PRODUCTION-SPLIT`: caller checkasm/parity PASS; microbench
  `decision=pass`; W4 halts before production wiring and records the routed
  production/gate split.
- `BLOCKED`: caller parity fails, W2 prerequisite fails when rerun, JSON guard
  fails without in-tranche demotion, or orphan disposition cannot honestly
  reach `orphan_count=0`.

No `BEHAVIOR-PASS-CSS-ADMIT` is claimed by the default W4 branch. The existing
CSS ADMIT candidate remains REDRESS-125 and is promoted only by W5 if all close
conditions hold.

## Revert Protocol

If a source attempt is rejected:

1. Save `/tmp/skv12-waveW4-rejected.patch`.
2. Revert behavior/source edits.
3. Retain only pure evidence artifacts and REDRESS documentation.
4. Commit measured REDRESS evidence as:

`docs(sk-v12-waveW4-redress): reject CSS delimiter ASM microbench`
