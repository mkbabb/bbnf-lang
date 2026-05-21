# SK-V12 W2 A5 - REDRESS And Pin Preblocks

Scope: read-only REDRESS/material-differential audit for SPEC Section 5.

## Finding

W2 is a Lock 16 correctness prerequisite, not a row-movement or throughput
wave. It must resolve the `escape_mask_64` falsifier before any new
SIMD/ASM admission.

Material differentials W2 must carry:

- REDRESS 28 admitted host aarch64 parity primitives but rejected active
  16-byte tiny-string dispatch after a `twitter` regression. W2 is only
  escape-carry correctness; it does not replay tiny-string wiring.
- REDRESS 33 left `match_tiny_plain_string` parity-green but invalidated as
  the wrong parse-G boundary. W2 must not cite primitive parity as row
  movement.
- The current falsifier is newer than those rows:
  `0xCAFEF00DBAADF00D`, iteration 0, 128-byte JSON-pool buffer, from
  `CHECKASM-REPORT.md:102-121`.
- REDRESS 88 keeps PMULL prefix-XOR rejected as a default hot body.
- REDRESS 89 keeps CSSC CTZ / bulk consumer rejected as a global/default
  mask-emission rewrite.
- USER PIN D5 keeps zero-orphan aarch64 primitive discipline binding.
- USER PIN D6 keeps parse-time CSS L4 `> lightningcss` as top priority;
  W2 cannot waive correctness with JSON parse-only speed.

If W2 fails, SPEC Section 5 requires SIMD to remain blocked: W1b-1 must stay
scalar-only and W4 cannot admit any SIMD/ASM primitive.

## Key References

- `restart/skinny/tranches/sk-v12/SPEC.md:351-385`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-85`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`
- `skinny/REDRESS.md` REDRESS 28, 33, 88, 89, and 121
