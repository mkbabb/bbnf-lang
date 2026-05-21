# SK-V12 W2 Research Consolidated

Status: PASS to plan.

## Read-Only Cohort

Six research lenses covered scalar contract, checkasm coverage, aarch64
handoff, corpus parity, REDRESS preblocks, and Apple Silicon command shape.
No source files were edited.

## Consolidated Finding

The historical `escape_mask_64` bug is no longer reproducing in the existing
strict scanner parity harness at HEAD. The local command:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_parity -- --nocapture
```

passed, including `classifier_corpus_parity`.

That is not enough to close W2. The SPEC gate requires the falsifier and
boundary carry cases to be executable W2 evidence. Current coverage has two
gaps:

1. There is no direct `escape_mask_64(bs_mask, carry_in)` differential test.
2. The caller-level JSON scanner adversarial windows are not explicitly
   pinned; the existing strict classifier harness mostly exercises generic
   byte classification and corpus safety.

## Recommended Plan Surface

Plan W2 as a correctness-proof wave:

- Add a dedicated `checkasm_escape_mask_64` test with an independent scalar
  reference and direct coverage for carry-in/out, bit-0 continuation, bit-63
  trailing runs, all-backslash masks, sparse runs, and long runs crossing
  64-byte boundaries.
- Add caller-level JSON scanner parity for the xorshift falsifier seed
  `0xCAFEF00DBAADF00D`, long backslash runs, residual tails, alignments, and
  mixed ASCII/escape windows.
- Run strict checkasm, primitive-checkasm, check-json, check-conformance, and
  a native JSON guard check.
- Update `CHECKASM-REPORT.md` and `skinny/REDRESS.md` so the old open
  divergence is either retired with W2 evidence or left blocked on failure.

## Preblocks

W2 does not reopen REDRESS 28/33 tiny-string wiring, REDRESS 88 PMULL default
body, or REDRESS 89 CSSC CTZ/bulk replacement. W2 admits no throughput row and
creates no new SIMD primitive. If W2 fails, SIMD/ASM admission remains blocked
for W1b-1/W4 under SPEC Section 5.
