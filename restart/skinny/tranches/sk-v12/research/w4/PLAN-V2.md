# SK-V12 W4 PLAN-V2 - CSS Delimiter ASM Consumer And Orphan Disposition

Date: 2026-05-20.
Phase: W4 Plan, V2 after CHALLENGE V1 REVISE.
Status: PLAN-V2 for CHALLENGE.

## V1 CHALLENGE Changes

PLAN-V2 folds every `challenge-v1/CONSOLIDATED.md` blocker:

- One exact generated caller contract is selected: CSS `scan_block`
  delimiter member-find, not layout/trivia run-skip.
- W4 production PASS requires W4-current report/gate consumption; the retained
  REDRESS-125 report is baseline evidence only.
- Caller-level scalar/SIMD parity is required in addition to primitive
  `byte_class_from_eq_set_64` checkasm.
- Micro-prove-first is a named W4 artifact and gate input.
- Generated CSS reproducibility and runtime fact-stream tests are mandatory
  when generated template/runtime output moves.
- `orphan-disposition.md` is a hard redress output with A4 evidence fields.
- W3 is explicitly not running concurrently with W4 and is not required for the
  current ADMIT path; W3 remains mandatory only for FIXPOINT.

## Entry State

W4 enters redress only after CHALLENGE V2 accepts this plan.

- W1b-2b REDRESS-125 provides the baseline CSS ADMIT candidate:
  Track 1 `429.34420791225705 Mbps`; lightningcss threshold
  `169.92962215656692 Mbps`; strict fact-stream equality PASS.
- W2 REDRESS-122 satisfies the `escape_mask_64` correctness prerequisite for
  new SIMD admission.
- W4 research A1-A6 and CHALLENGE V1 agree the five carried orphan
  dispositions are defensible if redress records evidence rather than prose.

## Selected SPEC Candidate And Exact Caller

Selected SPEC Section 9 ASM-gen row:

`a64_ascii_set_run_skip`

Exact W4 caller/API:

`find_ascii_set_member64(bytes, cursor, end, set) -> usize`

This is the delimiter member-find member of the ASCII set-scan family. It is
not the A5 layout/trivia `skip while member` API. The selected production
consumer is only the generated CSS L4 declaration-values `scan_block` loop,
where the delimiter set is `b"{};"`.

The route consumes the existing grammar-neutral
`bbnf_simd::prim::byte_class_from_eq_set_64` dispatch surface. On aarch64 this
is the real NEON body in `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`.
No new public substrate API, directive, BIR variant, `BackendShape`, decoded
byte sidecar, parser-owned sidecar, or x86 work is selected.

## Scalar Reference

The executable caller-level scalar reference is:

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

The SIMD caller is equivalent only when it:

1. Preserves `cursor <= end <= bytes.len()`.
2. Processes full 64-byte windows with `byte_class_from_eq_set_64`.
3. Advances by 64 when the mask is zero.
4. Advances by `mask.trailing_zeros()` when the mask is nonzero.
5. Falls back to the scalar reference for tails shorter than 64 bytes.

## Micro-Prove-First Gate

Redress starts with caller parity and an isolated same-host microbench before
production routing.

Required W4 microbench artifact:

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

The synthetic windows must include no-hit windows, first-hit-at-each-lane
windows, delimiter-heavy windows, high-bit bytes, every tail length 0..63, and
the frozen CSS fixture repeated into longer scan regions. If
`candidate_speedup_ratio < 1.01` or parity fails, redress records
`MEASURED-REJECT`, saves the patch to `/tmp/skv12-waveW4-rejected.patch`, and
does not wire production.

## Caller Parity And Checkasm

Required test surface:

- Add a dedicated caller-level checkasm/parity test, either
  `checkasm_ascii_set_member_find_64.rs` or a clearly named scoped module in an
  existing checkasm file. It must compare the caller-level scalar reference
  above against the SIMD caller for cursor positions, end bounds, tails,
  first-hit lanes, no-hit windows, duplicate set entries, high-bit bytes, and
  the frozen CSS fixture.
- Preserve and rerun existing primitive checkasm for
  `byte_class_from_eq_set_64`.
- Rerun the W2 `escape_mask_64` checkasm gate because W4 claims a new SIMD
  admission after W2.

## Production Wiring

Production wiring is allowed only after the microbench artifact records
`decision=pass`.

Allowed production wiring:

- Update `skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs`
  so generated `scan_block` uses `find_ascii_set_member64` for its default
  non-delimiter advance path.
- Reflect the generated output in
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`.

No `skip_ws_and_comments`, `find_colon`, number scanning, token emission, JSON
template, `parse-that-regex`, generic runtime, or x86 source edit is selected.

## W4-Current Report And Gate

REDRESS-125 remains baseline evidence only. A production W4 PASS must add or
extend a W4-specific CSS SOTA report/gate path that consumes the current
post-W4 Criterion lanes and the W4 microbench artifact.

Minimum report/gate contract:

```text
schema_id=sk-v12-w4-asm-css-v1
wave_id=SK-V12-W4
redress_entry=REDRESS-126
selected_candidate=a64_ascii_set_run_skip
caller_api=find_ascii_set_member64
same_wave_consumer_class=generated_css_scan_block_delimiter_find
lock16_status=pass:scalar+checkasm+caller_parity+microbench+consumer
scalar_reference_status=pass:find_ascii_set_member_scalar
checkasm_or_parity_status=pass:byte_class+caller_find+escape_mask
microbench_artifact=restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json
track1_mbps
track2_or_oracle_mbps
lightningcss_mbps
threshold_mbps=lightningcss_mbps+1
admission_margin_mbps
strict_output_equality=pass
json_guard_state
orphan_disposition_path=restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md
```

If production is not wired because the microbench rejects, the W4-current
report/gate path is not required; the wave records a measured REDRESS reject
with the microbench artifact, parity evidence, and rejected patch.

If production is wired, W4 PASS must not use the W1b-2b report path as its own
gate. It may compare against REDRESS-125 as a baseline, but it must consume
fresh post-W4 Criterion numbers.

## Orphan Disposition Output

Redress must write:

`restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`

Required per-row fields:

```text
orphan_name
status
production_grep_evidence
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

| Candidate | Disposition |
|---|---|
| `bitmap_prefix_xor_64` | `inventory_demoted_with_evidence`; aarch64 body delegates to scalar; PMULL route remains REDRESS-88-adjacent and not selected. |
| `bitmap_next_set_bit` | `inventory_demoted_with_evidence`; aarch64 body delegates to scalar; CTZ route remains REDRESS-89-adjacent and not selected. |
| `bulk_emit_positions_64` | `inventory_demoted_with_evidence`; production-consumed scalar delegate through the existing compact-mask path, no claimed aarch64 body. |
| `byte_context` | `inventory_demoted_with_evidence`; support/test reachability only, not a production orphan requiring same-wave consumption. |
| `cache_hints` | `inventory_demoted_with_evidence`; support/test reachability only, not a production orphan requiring same-wave consumption. |

## Owner Paths

Allowed redress source paths:

- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w4/`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` only for a measured guard demotion or W5-directed
  reconciliation; W4 should normally leave `RESULTS.md` unchanged

Not selected:

- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/parse-that-regex/src/`
- generic runtime beyond the generated CSS module dependency already present
- x86 source
- W3 shared-file edits

## Required Verification Commands

When production is not wired because microbench rejects:

```text
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

When production is wired, add:

```text
cargo test -p codegen css_l4_declaration_values -- --nocapture
cargo test -p runtime css_l4_declaration_values_emit_fact_stream -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --skv12-w4-asm-css-report ../restart/skinny/tranches/sk-v12/research/w4/skv12-W4-asm-css.json --advisory
```

If W4 edits `report.rs`, `gate.rs`, or production `bbnf-simd`, JSON guard
evidence must be escalated from the retained W1a no-write root to a fresh
populated JSON guard root unless the final diff proves no JSON-producing path
moved and CHALLENGE V2 accepts that no-touch proof.

## Exit Mapping

- `BEHAVIOR-PASS-CSS-ADMIT`: microbench PASS; caller checkasm/parity PASS;
  production `scan_block` consumer wired; strict CSS equality PASS; W4-current
  CSS report/gate PASS; Track 1 remains `> lightningcss_mbps + 1`; JSON guards
  hold or have measured demotion; `orphan_count=0`.
- `BEHAVIOR-PASS-NONCLOSE`: microbench, parity, and production wiring pass but
  the CSS close bar misses; evidence only.
- `MEASURED-REJECT`: microbench rejects, parity fails, or production route
  regresses; save rejected patch and record REDRESS evidence. No production
  orphan or future-phase promise may remain.
- `BLOCKED`: CHALLENGE rejects PLAN-V2 or W2 correctness evidence is found
  stale.

## Revert Protocol

On source-attempt reject:

1. Save `/tmp/skv12-waveW4-rejected.patch`.
2. Revert behavior/source edits.
3. Keep retained microbench/parity artifacts and
   `orphan-disposition.md` only if they are pure evidence and do not imply a
   production consumer.
4. Commit measured REDRESS evidence as
   `docs(sk-v12-waveW4-redress): reject CSS delimiter ASM consumer`.

On PASS:

Commit source, generated output, W4 report/gate path, microbench artifact,
orphan disposition, and REDRESS entry as:

`feat(sk-v12-waveW4): admit CSS delimiter ASM consumer and orphan disposition`
