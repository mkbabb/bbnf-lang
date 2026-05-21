# SK-V12 W4 PLAN-V1 - ASM-Gen CSS Consumer And Orphan Disposition

Date: 2026-05-20.
Phase: W4 Plan.
Status: PLAN-V1 for CHALLENGE.

## Authority And Entry State

Authority is SPEC Section 9 plus W4 research A1-A6.

Entry gates are satisfied for planning:

- W1b-2b closed REDRESS-125 as `PASS-ADMIT-CANDIDATE` for
  `css_l4/declaration_values/direct_to_struct/main`, with Track 1
  `429.34420791225705 Mbps` and lightningcss threshold
  `169.92962215656692 Mbps`.
- W2 closed REDRESS-122 as `G-W2-ESCAPE-MASK-CORRECTNESS` /
  `G-W2-ESCAPE-MASK-LOCK16` PASS, so new SIMD/ASM admission is no longer
  blocked by the `escape_mask_64` falsifier.
- W4 research A1-A6 converged on one bounded ASM-gen route and a docs-only
  orphan accounting route.

## SPEC Owner-Path Correction

W4's SPEC Section 9 owner list names `skinny/crates/codegen/src/json_templates/`
but the selected same-wave consumer is the already admitted CSS generated row.
The redress implementation must be generated from the CSS template source and
then reflected into generated runtime output, so the plan amends the W4 owner
list to include:

- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`

No JSON template edit is selected by this plan. If CHALLENGE rejects this
owner-path correction, W4 returns to plan before redress.

## Selected Intervention

Select exactly one primary ASM-gen candidate:

`a64_ascii_set_run_skip`

The concrete W4 route is a CSS-local generated consumer over the existing
`bbnf_simd::prim::byte_class_from_eq_set_64` dispatch surface. On aarch64 this
surface is the real NEON body in
`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`, with scalar
reference and checkasm already present. W4 refreshes the checkasm harness for
the CSS delimiter/layout sets and wires the generated CSS block scanner to use
the 64-byte set mask to skip non-delimiter runs until one of `{`, `;`, or `}`
is found.

This is not a new public substrate API, directive, BIR variant, or
`BackendShape`. It is a grammar-owned generated consumer for an existing
grammar-neutral SIMD primitive.

## Scalar Reference And Lock 16 Shape

The scalar reference for the selected caller is:

```rust
fn find_ascii_set_member_scalar(bytes: &[u8], mut cursor: usize, end: usize, set: &[u8]) -> usize {
    debug_assert!(cursor <= end && end <= bytes.len());
    debug_assert!(set.len() <= 8);
    while cursor < end && !set.contains(&bytes[cursor]) {
        cursor += 1;
    }
    cursor
}
```

The SIMD caller is equivalent when it:

1. Processes full 64-byte windows with `byte_class_from_eq_set_64`.
2. Advances by 64 when the returned mask is zero.
3. Advances by `mask.trailing_zeros()` when a member is present.
4. Falls back to the scalar reference for tails below 64 bytes.

Required checkasm/parity refresh:

- Extend `checkasm_byte_class_from_eq_set_64.rs` with CSS delimiter set
  `b"{};"`, CSS layout set `[0x09, 0x0a, 0x0c, 0x0d, 0x20]`, high-bit bytes,
  no-hit windows, first-hit-at-each-lane windows, duplicate-set sanity, and
  tail caller parity.
- Preserve the existing scalar primitive reference in
  `src/scalar/byte_class_from_eq_set_64.rs`.
- Do not add a new orphan primitive.

## Micro-Prove-First

Redress order is binding:

1. Add the CSS-local scalar caller and SIMD caller behind generated CSS runtime
   tests/bench hooks.
2. Run an isolated same-host microbench over the frozen CSS fixture repeated
   enough times to compare scalar delimiter scan versus SIMD delimiter scan.
3. Only wire production `scan_block` to the SIMD caller if the microbench shows
   the SIMD caller is faster on the selected CSS hot leaf and parity is
   byte-identical.
4. If the microbench rejects the candidate, do not wire production. Record
   `MEASURED-REJECT`, save the rejected source patch, and keep only the plan /
   REDRESS evidence required by the contract.

The microbench artifact must be retained under
`restart/skinny/tranches/sk-v12/research/w4/` and cited in REDRESS.

## Source Owner Paths

Redress may edit only these paths:

- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `restart/skinny/tranches/sk-v12/research/w4/` measurement artifacts
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` only if W4 changes the admitted CSS row or records a
  measured JSON guard demotion

No `parse-that-regex`, JSON template, generic runtime, directive, IR,
`BackendShape`, public substrate, or x86 path is selected.

## Orphan Accounting Table

| Candidate | W4 disposition | Evidence route |
|---|---|---|
| `bitmap_prefix_xor_64` | `inventory_demoted_with_evidence` | A1 shows the aarch64 body delegates to scalar; PMULL prefix-XOR stays rejected by REDRESS 88 unless a future wave names a new hot consumer. Existing scalar/checkasm coverage remains proof-only, not production orphan credit. |
| `bitmap_next_set_bit` | `inventory_demoted_with_evidence` | A1 shows scalar delegate only and no selected production consumer. CTZ/bulk production rewires remain blocked by REDRESS 89 absent a fresh consumer. |
| `bulk_emit_positions_64` | `inventory_demoted_with_evidence` | A1/A4 show it is production-consumed as a scalar delegate through the existing compact-mask path, not an unconsumed aarch64 kernel. No new aarch64 body is claimed. |
| `byte_context` | `inventory_demoted_with_evidence` | A1 shows support-only test/smoke coverage and no production caller; it is not a production orphan requiring same-wave consumption. |
| `cache_hints` | `inventory_demoted_with_evidence` | A1 shows support-only prefetch/store helper coverage and no production caller; it is not a production orphan requiring same-wave consumption. |

W4 close requires `orphan_count=0` by this accounting plus the selected CSS
consumer evidence. A CHALLENGE finding that any row above is a real production
orphan requiring code deletion or consumption returns W4 to plan.

## Measurement And Gates

Required redress commands:

```text
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64 -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json --advisory
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

If W4 changes the CSS report path or moves `RESULTS.md`, redress must update the
CSS SOTA report and consume the refreshed report in the gate in the same wave.
Otherwise W4 records a no-write CSS/JSON proof with the retained REDRESS-125
report and unchanged `RESULTS.md` hash.

Exit mapping:

- `BEHAVIOR-PASS-CSS-ADMIT`: microbench faster, checkasm/parity PASS, strict
  CSS equality PASS, CSS Track 1 remains `> lightningcss_mbps + 1`, JSON guards
  hold or have measured demotion, and orphan count is zero.
- `BEHAVIOR-PASS-NONCLOSE`: parity and microbench pass but CSS close bar is not
  met; evidence only.
- `MEASURED-REJECT`: parity passes but the selected candidate is slower or the
  production route regresses; record microbench/full-row evidence and save the
  rejected source patch.
- `BLOCKED`: W2 correctness is found stale or CHALLENGE rejects the owner-path /
  orphan disposition before redress.

## Revert Protocol

On implementation FAIL or measured reject after source edits:

1. Save the rejected source patch at `/tmp/skv12-waveW4-rejected.patch`.
2. Revert source/runtime/generated/bench/gate changes.
3. Commit REDRESS evidence as
   `docs(sk-v12-waveW4-redress): reject ASM-gen CSS consumer`.

On PASS:

1. Commit source, generated output, retained microbench evidence, orphan
   disposition, and REDRESS entry as
   `feat(sk-v12-waveW4): admit ASM-gen CSS consumer and orphan disposition`.
2. Leave `RESULTS.md` unchanged unless W4 refreshes the admitted CSS row or
   records a guard demotion.
