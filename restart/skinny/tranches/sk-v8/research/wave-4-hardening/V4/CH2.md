# SK-V8 W4 Hardening V4 CH2

Verdict: ACCEPT.

Confidence: 96%.

## Findings

- The unchanged W4 fail-closed disposition remains grammar-neutral. W4 does
  not admit the JSON hand Track 2 scalar-parent fold, does not update
  `skinny/RESULTS.md`, and does not move generic runtime, codegen, BIR,
  directive, substrate, SIMD, parser, or generated Track 1 surfaces.
- No W4 JSON policy enters generic crates. The current W4 delta from the V1
  hardening state through HEAD is docs/redress only: `HANDOFF.md`,
  `skv8-W4-plan.md`, V2/V3 hardening artifacts, and `skinny/REDRESS.md`.
  `skinny/crates/bbnf-bench/src/direct_struct.rs`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, and
  `skinny/RESULTS.md` have no diff.
- CSS L4, Sheets, and BBNF-self are not exposed to a JSON-specific W4 behavior
  path because no generic code path is admitted. SPEC Section 2.1's non-JSON
  proof burden would become active for a generic edit; this rejected W4
  disposition has none.
- No hidden grammar-name leak is introduced by W4. The only saved rejected
  patch is `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`, and it touches
  only `skinny/crates/bbnf-bench/src/direct_struct.rs`. Current hand Track 2
  still parses object and array values through `self.value()?` child digests
  before folding, so the rejected scalar-parent fold is not active.
- No new directive, BIR variant, `BackendShape`, `UnionTape`, substrate
  surface, sidecar substrate, parser-owned cursor/fact slot, source hook,
  value-byte carry, cap-16 route, raw-f64 route, mantissa route, or generated
  helper coupling is admitted. REDRESS 93 records those exclusions and routes
  residual direct misses instead of upgrading digest evidence into product
  proof.
- Lock 14 remains correctly fail-closed. `direct_struct.rs` is still a frozen
  root; `validate_authorized_parent_diff` authorizes only the existing W2
  typed-owner parent diff and has no `sk-v8-wave4` allowance.
  `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 10/10.
- The V1 finding that W4 source admission would need a W4-aware checked report
  path, full-table maintain proof, and explicit Lock 14 parent-diff allowance
  remains true only for source or row-table admission. Because the source
  candidate is rejected, adding that allowance now would be an over-widening.
- HANDOFF's pending-hardening wording is not a CH2 defect before V4 closure.
  It does not overclaim source admission, W4 closure, W5 activation, or a
  Lock 14 allowance; it keeps W4 proposed/pending and W5-W6 conditional.

## Required Folds

None.
