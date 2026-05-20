# SK-V11 W5 R3 - AArch64 String Block SIMD Parity

Date: 2026-05-20.
Lane: W5 Phase 1 research R3.
Scope: aarch64 `string_block` / SIMD parity and REDRESS 106 material
differential.
Output: this file only.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 9 and the global
  micro-prove-first / material-differential rules.
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`.
- `skinny/crates/bbnf-simd/tests/`.
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`.
- `skinny/REDRESS.md` item 106.

## Decision

W5 Phase 1 should stay scalar-only.

The current AArch64 `StringSpecialBlock` body is useful evidence, not W5
production authority. W5 may reopen SIMD only as an explicitly selected optional
micro-proof with a new strict checkasm cell, same-host caller microbench, scalar
fallback, named same-wave generated string/key consumer, at most two target
rows, and the REDRESS 106 material differential below. Without that packet,
using SIMD would replay the exact failure mode W5 is required to avoid:
primitive parity passing while caller-level row movement fails.

## Evidence

- SK-V11 allows AArch64 Apple Silicon as the only SIMD/ASM implementation target,
  but every SIMD/ASM intervention must have scalar reference, strict
  differential/checkasm or product parity, same-host microbench, feature/fallback
  plan, same-wave consumer, and row gate (`SPEC.md:46-51`).
- W5 Section 9 makes string-block support optional and only after micro-proof.
  The W5 entry gate requires CHALLENGE to select the scalar span shape, one
  string/key caller, a cap, and at most two target rows; any SIMD body must carry
  a strict parity plan and REDRESS 106 material differential (`SPEC.md:542-567`).
- W5 explicitly forbids decoded scratch, retained string side tables,
  `StringBlock16` retained wrappers, primitive-parity-only production, and
  64-byte retained scans. Revert is mandatory on parity failure, row-floor miss,
  Unicode guard regression, or REDRESS 106 replay (`SPEC.md:571-587`).
- `string_block.rs` has a 16-byte scalar reference that emits terminator,
  escape, control, and non-ASCII masks, plus a NEON body that computes the same
  masks through lane compares and `movemask_u8x16`
  (`string_block.rs:5-72`).
- Existing tests cover smoke/parity for fixed 16-byte cases and one
  alignment-sweep case in `sk_v3_intrinsic_parity_aarch64`
  (`aarch64_primitives.rs:117-165`, `checkasm_parity.rs:617-640`). This is not
  yet a W5 production checkasm cell over caps, tails, all quote/escape/control
  offsets, random/adversarial bytes, and the selected generated caller.
- P2-B already classifies `STRING_SPECIAL_BLOCK_CALLER_MICROPROOF` as a
  caller-level proof, not a primitive-only proof. It requires scalar oracle
  first, strict differential second, AArch64 feature/fallback third,
  micro-proof before production wiring, and same-wave consumer last
  (`p2b-dav1d-process.md:247-269`).
- P2-C says the current 16-byte primitive has scalar and parity coverage, but a
  widened block would need its own scalar oracle because REDRESS 106 rejected the
  caller proof. Its same-wave consumer must be one narrow generated direct/typed
  string or key caller (`p2c-arch-esoterica.md:45-53`).
- REDRESS 106 is the binding blocker: scalar/reference parity and strict
  checkasm parity were green, but the full-string caller microbench failed with
  `0.774x` aggregate speedup versus the required `1.08x`; per-slice results
  were `unicode_mixed` `0.471x`, `unicode_escapes` `1.315x`, and
  `unicode_basic` `0.604x` (`skinny/REDRESS.md:3152-3165`).
- `CHECKASM-REPORT.md` also records a live NEON/scalar boundary divergence in
  the structural classifier path around `escape_mask_64` handoff. Any W5 string
  or escape SIMD plan that depends on that boundary semantics must close or
  explicitly avoid it before admission (`CHECKASM-REPORT.md:102-121`).

## REDRESS 106 Material Differential

W5 scalar span is materially different from REDRESS 106 only if it remains a
bounded scalar span route: one scalar oracle returns the string/key end offset
and decode-needed status under a selected cap, one generated direct/typed
string/key caller consumes that result in the same wave, and at most two
preselected rows are measured against the Section 0.4 direct floors. It does not
reuse `C5-full-string-proof`, does not promote
`match_string_at_quote_trusted_utf8` or the existing parse-that full-string loop
as the proof target, does not count `string_special_block_matches_scalar_reference`
or `sk_v3_intrinsic_parity_aarch64` as production admission, and does not add a
retained `StringBlock16`, 64-byte retained scan, decoded scratch, string side
table, or semantic string fact.

If W5 adds any `bbnf-simd` native body, the differential changes and the scalar
preblock is no longer sufficient. The SIMD attempt must be treated as a new
caller micro-proof, not as a narrower name for REDRESS 106.

## Required Checks

Scalar-only W5:

- No `bbnf-simd` checkasm gate is required if W5 does not use a SIMD/ASM body.
- Required proof is product parity plus same-host microbench for the
  CHALLENGE-selected generated direct/typed string/key caller and at most two
  selected rows.
- The run must record run id, host triple, build flags, sample count, selected
  cap, selected caller, Track 1 and Track 2 values, strict comparator source,
  guard floors, and Unicode residual status.

SIMD W5, if CHALLENGE explicitly selects it:

- Add or extend a W5-specific strict `bbnf-simd` checkasm cell for the selected
  string-block body. The cell must compare the scalar oracle to the native body
  under `BBNF_SIMD_STRICT=1` across alignments 0..63, every interesting-byte
  class, first-interesting offsets, caps/tails, all-clear blocks, non-ASCII
  blocks, mixed quote/escape/control cases, source immutability, stack/signal
  guards, and deterministic random/adversarial inputs.
- Run the existing primitive umbrella as a backstop:

```sh
cd skinny && RUSTFLAGS="-C target-cpu=native" cargo run -p xtask --release -- primitive-checkasm
```

- Run the targeted W5 string-block parity command once the W5 cell exists. Until
  then, the current `sk_v3_intrinsic_parity_aarch64` command is only a seed:

```sh
cd skinny && BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity sk_v3_intrinsic_parity_aarch64 -- --nocapture
```

- Run a same-host caller microbench that compares scalar-only baseline versus
  SIMD-enabled candidate on the same selected caller and rows, with scalar
  fallback active for non-aarch64, disabled SIMD, short tails, cap overflow, and
  any unproven boundary.
- Reject SIMD if selected rows miss Section 0.4 floors, if any full guard block
  regresses, if Unicode residual rows regress outside the W5 guard rules, or if
  the evidence only proves primitive throughput.

Representative admission command shape, replacing the row regex with the
CHALLENGE-selected set:

```sh
cd skinny && CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|github_events|update_center|random|distinct_values|gsoc-2018|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
cd skinny && CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Preblocked Routes

- Preblock any plan that uses current `scan_string_special_block` NEON parity as
  admission without a new W5 caller microbench.
- Preblock any widened 64-byte or four-lane string block without a new scalar
  64-byte oracle and strict W5 checkasm cell.
- Preblock retained wrappers, retained scans, decoded scratch, string side
  tables, semantic string facts, or primitive-only production.
- Preblock reuse of the REDRESS 106 full-string caller proof or its selected
  Unicode aggregate as W5 evidence.
- Preblock any plan that touches the open `escape_mask_64` boundary semantics
  without first closing or avoiding that handoff.
- Preblock any non-JSON or generic-code claim unless the same wave includes the
  required non-JSON string/literal proof.

## R3 Conclusion

The W5 Phase 1 implementation route should be scalar-only. AArch64 string-block
SIMD may remain in the research backlog, but it should not enter W5 production
unless it is explicitly selected as a separate W5 SIMD micro-proof and clears
the strict checkasm, same-host caller microbench, fallback, row-floor, guard, and
REDRESS 106 material-differential requirements above.
