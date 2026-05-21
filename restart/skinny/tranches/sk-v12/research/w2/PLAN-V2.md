# SK-V12 W2 Plan V2 - `escape_mask_64` Correctness Prerequisite

Status: SELECTED for CHALLENGE V2.

Supersedes: `PLAN.md`, rejected by `challenge-v1/CONSOLIDATED.md`.

## Intervention

W2 remains a correctness prerequisite. It makes the `escape_mask_64` carry
contract executable and proves the JSON scanner caller handoff without
admitting a throughput row, SIMD optimization, CSS row, or parse-only result.

V2 resolves the V1 CHALLENGE rejection by splitting proof location:

- primitive mask/carry parity lives in `bbnf-simd`;
- caller-level JSON scanner parity lives in the runtime JSON scanner test
  surface, where the consumer already owns JSON string policy.

## Owner Paths

V2 amends SPEC Section 5 owner paths to include the caller test surface:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/REDRESS.md`

`scan.rs` ownership is narrow: adversarial scanner parity tests are allowed.
Non-test behavior edits are allowed only if those tests falsify HEAD and the
edit is the minimal carry handoff repair; otherwise runtime behavior edits are
out of scope.

## LOC Budget

Hard budget: <=180 hand/test LOC.

Expected slice:

- `checkasm_escape_mask_64.rs`: <=95 LOC.
- `scan.rs` adversarial tests: <=55 LOC.
- `CHECKASM-REPORT.md` + REDRESS entry: <=30 LOC net.

## Redress Tasks

1. Add `skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs`.
2. In that test, define an independent byte-walk scalar reference for
   `escape_mask_64`; do not call `bbnf_simd::escape_mask_64` from the
   reference.
3. Cover the xorshift seed `0xCAFEF00DBAADF00D`, carry-in true/false, bit-0
   continuation, bit-63 odd/even trailing runs, `u64::MAX`, sparse masks,
   deterministic random masks, and long backslash runs 1..128 split into
   64-bit stripes.
4. Add `#[cfg(test)]` runtime JSON scanner parity tests in
   `skinny/crates/runtime/src/grammars/json/scan.rs`, comparing
   `scan_structurals` with `scan_structurals_scalar`.
5. Runtime tests cover the historical 128-byte JSON-pool shape, residual tails
   0..63, copied alignments, mixed ASCII/escape windows, and odd/even slash
   runs before boundary quotes.
6. If tests pass at HEAD, make no behavior source change.
7. If tests fail, apply only the minimal carry handoff fix in `scan.rs`, then
   run the expanded JSON behavior guard below.
8. Update `CHECKASM-REPORT.md` from open divergence to W2 disposition only
   after the new proof cells pass.
9. Append REDRESS with the measured W2 disposition.

## Falsifiability Gate

Exit gate: `G-W2-ESCAPE-MASK-CORRECTNESS` / `G-W2-ESCAPE-MASK-LOCK16`.

PASS requires:

- direct `escape_mask_64` scalar-reference parity on the falsifier and
  adversarial carry cases;
- runtime JSON scanner scalar/SIMD parity on adversarial escape windows;
- corpus parity PASS;
- `CHECKASM-REPORT.md` or same-wave checkasm artifact records PASS;
- no new SIMD primitive, no row admission, no orphan added.

## Mandatory Verification

Focused W2 proof:

```sh
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p runtime json::scan -- --nocapture
cargo test -p bbnf-simd --release --test corpus_parity
```

No-touch JSON guard proof when no non-test runtime behavior moves:

```sh
git diff --name-only HEAD -- skinny/crates/runtime/src/grammars/json/scan.rs skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/codegen/src skinny/RESULTS.md
```

If non-test JSON scanner behavior changes, add:

```sh
cargo run -p xtask -- check-json
cargo run -p xtask -- check-conformance
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --advisory --check-results
awk -f ../restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk RESULTS.md
```

## Preblocked Routes

- No REDRESS 28/33 tiny-string active dispatch replay.
- No PMULL prefix-XOR default body from REDRESS 88.
- No CSSC CTZ / bulk-emission rewrite from REDRESS 89.
- No throughput admission and no parse-only SOTA claim.
- No x86 implementation work.
- No public substrate API, `BackendShape`, BIR variant, or directive change.

## Revert Protocol

On FAIL:

```sh
git diff --binary HEAD -- \
  skinny/crates/bbnf-simd/src/lib.rs \
  skinny/crates/bbnf-simd/src/aarch64 \
  skinny/crates/bbnf-simd/tests \
  skinny/crates/runtime/src/grammars/json/scan.rs \
  skinny/crates/bbnf-simd/CHECKASM-REPORT.md \
  skinny/REDRESS.md > /tmp/skv12-waveW2-rejected.patch
```

Inspect and split unrelated edits, revert only the W2-owned slice, append a
REDRESS rejection with the failing case, and leave SIMD/ASM admission blocked
for W1b/W4.
