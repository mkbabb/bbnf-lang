# SK-V10 W9 Plan - Reject Already-Wired Escape Production

Pass: Wave Plan.
Cycle: W9.
Date: 2026-05-19.
Gate: `G-W9-KERNEL-PRODUCTION`.
Disposition target: measured REJECT unless CHALLENGE identifies an admissible
same-commit production delta.

## Selected Intervention

Select `C6-existing-unescape-production` for negative redress. The accepted W8
proof is real, but the exact primitive/caller pair is already in production:
`unescape_string` already invokes `unescape_four_unicode_escapes`, which already
invokes `unescape_uxxxx_x4_neon` on aarch64.

W9 therefore cannot honestly land a new source behavior under the current
Section 12 wording. The plan is to reject W9 after running the required parity
and targeted direct-row measurement, preserving W8 as proof-only evidence.

## Owner Paths

Read/measure only:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v10/research/w9/w9-redress.md`

Do not edit production source in W9 unless CHALLENGE names a non-no-op delta
that still consumes the exact W8 proof.

## Falsifiability Gate

`G-W9-KERNEL-PRODUCTION` can pass only if:

- scalar fallback and differential parity remain green;
- the named production caller consumes the primitive in the same commit;
- target direct or typed rows meet their Section 0.2 floors;
- W10b/direct maintain floors hold.

The known risk is fatal: the named production caller already consumed the
primitive before W9. That fails the same-commit caller clause unless W9 changes
real production behavior. A no-op constant, wrapper, or feature re-gate is
pre-blocked by the anti-paper-close rule.

## Measurement Plan

Run:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd \
  unescape_uxxxx_x4_matches_scalar -- --nocapture

BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd \
  sk_v3_intrinsic_parity_aarch64 -- --nocapture

RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p parse-that-regex \
  unescape -- --nocapture

CARGO_TARGET_DIR=/tmp/skv10-w9-target \
CRITERION_HOME=/tmp/skv10-w9-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json/(unicode_escapes|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
```

Compute Mbps from the captured Criterion estimates using `bytes * 1000 / ns`.
The direct floors are `unicode_escapes >= 12527` and
`y_string_unicode >= 8027` for both Track 1 and Track 2.

## Revert Protocol

No production patch is expected. If CHALLENGE finds and redress attempts a
legitimate production delta, revert that source, generated output, gate/report
changes, and `RESULTS.md` as one slice on any parity failure, row-floor miss,
or W10b maintain miss. Save the rejected source patch to
`/tmp/skv10-waveW9-rejected.patch`.

## Pre-Blocked Routes

- No W3 consumer.
- No orphan or already-wired primitive claimed as same-wave integration.
- No parse-only SOTA close.
- No generic JSON policy leak.
- No direct-vs-typed relabeling.
- No no-op feature gate, wrapper, or telemetry-only `RESULTS.md` movement.
