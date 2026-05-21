# SK-V12 W2 Plan - `escape_mask_64` Correctness Prerequisite

Status: SELECTED for CHALLENGE.

## Intervention

W2 selects the C3 correctness unblocker from S-P3: make the
`escape_mask_64` carry contract executable and retire or preserve the old
NEON handoff falsifier based on same-host tests. W2 admits no throughput row
and lands no new optimization primitive.

The observed state at W2 research close is important: the existing strict
scanner harness already passes at HEAD, so the redress should first add
missing proof cells. Source behavior changes are allowed only if those cells
falsify HEAD.

## Owner Paths

From SPEC Section 5:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/REDRESS.md`

W2 does not own generated JSON, CSS L4, codegen, generic crates, or any x86
implementation path.

## Redress Tasks

1. Add a dedicated `checkasm_escape_mask_64` integration test under
   `skinny/crates/bbnf-simd/tests/`.
2. In that test, define an independent scalar reference by byte-walking
   backslash masks; do not call `bbnf_simd::escape_mask_64` from the reference.
3. Cover:
   - xorshift seed `0xCAFEF00DBAADF00D`, iteration 0, 128-byte JSON-pool shape;
   - carry-in true/false;
   - bit-0 continuation from previous stripe;
   - bit-63 odd/even trailing runs and `u64::MAX`;
   - sparse masks and deterministic random masks;
   - long backslash runs 1..128 split into 64-bit stripes.
4. Add caller-level JSON scanner adversarial parity to the same test or a
   sibling `checkasm_json_escape_scan` test, comparing
   `runtime::grammars::json::scan::scan_structurals` with
   `scan_structurals_scalar`.
5. Caller parity must include the historical JSON-pool falsifier, residual
   tails 0..63, alignment copies, mixed ASCII/escape windows, and odd/even
   slash runs before boundary quotes.
6. If those tests fail, minimally adjust
   `skinny/crates/runtime/src/grammars/json/scan.rs` only after CHALLENGE
   confirms owner-path expansion. Under the current SPEC owner table that file
   is not owned by W2, so a source fix outside bbnf-simd returns REVISE unless
   CHALLENGE amends the plan.
7. Update `CHECKASM-REPORT.md` from "open divergence" to W2 disposition only
   after the new tests pass.
8. Append REDRESS with the measured W2 disposition.

## Falsifiability Gate

Exit gate: `G-W2-ESCAPE-MASK-CORRECTNESS` / `G-W2-ESCAPE-MASK-LOCK16`
from SPEC Section 5 and P3-C §3.5.

PASS requires:

- direct `escape_mask_64` scalar-reference parity on the falsifier and
  adversarial carry cases;
- caller-level JSON scanner scalar/SIMD parity on adversarial escape windows;
- `BBNF_SIMD_STRICT=1` strict checkasm parity PASS;
- corpus parity PASS;
- JSON guard floors PASS or measured demotion in REDRESS;
- no new SIMD primitive, no row admission, and no orphan added.

## Mandatory Verification

Run from `skinny/` unless noted:

```sh
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- primitive-checkasm
cargo run -p xtask -- check-json
cargo run -p xtask -- check-conformance
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
```

If the native full guard hits an environmental low-frequency segment, rerun the
affected rows in the same Criterion root and then rerun `gate-json
--check-results`; record the rerun honestly in REDRESS.

## Preblocked Routes

- No REDRESS 28/33 tiny-string active dispatch replay.
- No PMULL prefix-XOR default body from REDRESS 88.
- No CSSC CTZ / bulk-emission rewrite from REDRESS 89.
- No throughput admission and no parse-only SOTA claim.
- No x86 implementation work.
- No public substrate API, `BackendShape`, BIR variant, or directive change.

## Revert Protocol

On FAIL, revert W2 test/report/source edits, save
`/tmp/skv12-waveW2-rejected.patch`, append a REDRESS rejection with the failing
case, and leave SIMD/ASM admission blocked for W1b/W4.
