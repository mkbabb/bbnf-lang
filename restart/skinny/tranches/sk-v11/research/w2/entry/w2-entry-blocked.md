# SK-V11 W2 Entry Record - BLOCKED

Date: 2026-05-20.

Wave: W2 - CSS L4 Generated Direct/Typed Intervention Proof.

Disposition: `BLOCKED`, no source dispatch authority.

## Entry Gate Check

`SPEC.md` Section 6 gives W2 authority only after W1b closes:

- W2 entry gate: W1b closed; CHALLENGE selects exactly one generated non-JSON
  direct or typed intervention.
- W2 task constraint: consume the W1b baseline; W2 may not create the first
  measurable non-JSON row.
- W2 exit gate threshold: Track 1 must be at least
  `ceil(W1b_css_baseline_mbps * 1.01)` on the selected non-JSON row.

REDRESS 112 rejected W1b under `G-W1b-NONJSON-BASELINE`. No
`W1b_css_baseline_mbps` exists, no generated CSS L4 Track 1 exists, and no
independent W1b oracle row was admitted.

## Evidence

The accepted W1b redress evidence showed:

- `skinny/crates/runtime/src/grammars/` contains generated JSON plus
  `sheets_witness`, not generated CSS L4.
- `skinny/crates/codegen/src/json_provider.rs` keeps runtime emission guarded by
  `ensure_runtime_profile`, with non-JSON runtime emission rejected before
  generated Track 1 can exist.
- The W1a companion gate still passes and the JSON gate remains valid:
  `cargo test -p bbnf-bench report::tests::w1a -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w1a -- --nocapture`, and
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.

## Result

W2 cannot run Phase 2 or Phase 3 without inventing the baseline it is required
to consume. That would violate the SPEC and hide W1b's falsification. The
non-JSON generated-intervention axis is therefore recorded as blocked for
SK-V11 unless a later Alpha/Pass-Omega contract creates a generated non-JSON
baseline wave with explicit owner authority.

No source patch, generated parser, SIMD kernel, benchmark row, gate schema, or
`skinny/RESULTS.md` row moved in W2.

## Downstream Route

This artifact is the `BLOCKED` route anticipated by `G-W8-DIRECT-FIXPOINT` and
`G-W9-CLOSE-SK-V11`. Direct-plane waves may continue only as JSON direct
closure/fixpoint waves; they must not claim that W2 admitted the non-JSON axis.
