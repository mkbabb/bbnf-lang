# SK-V10 W8 Escape Micro-Proof

Status: PASS.
Gate: `G-W8-ESCAPE-SEGMENT-MICROPROOF`.
Run id: `sk-v10-w8-escape-microproof`.

## Binding

- Candidate: `C6-hex-escape-proof`
- Primitive: `unescape_uxxxx_x4_neon`
- Caller: `unescape_string` through `unescape_four_unicode_escapes`
- Scalar oracle: `unescape_uxxxx_scalar` plus scalar JSON surrogate policy
- Differential harness: `w8_escape_microbench` valid/invalid cases and fixture
  parity
- Feature gate: `target_arch=aarch64`
- Host triple: `aarch64-apple-darwin`
- Build flags: `-C opt-level=3 -C target-cpu=native`
- Sample count: 25
- Threshold: aggregate caller speedup `>=1.08x`

## Measurement

Command:

```text
CARGO_TARGET_DIR=/tmp/skv10-w8-escape-target \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/Cargo.toml --release -- /Users/mkbabb/Programming/bbnf-lang
```

| Slice | Strings | Raw bytes | Rounds | Bytes/sample | Production ns | Scalar ns | Speedup |
|---|---:|---:|---:|---:|---:|---:|---:|
| `unicode_escapes` | 1251 | 820092 | 40 | 32803680 | 14113893 | 37208978 | 2.636x |
| `unicode_mixed` | 0 | 0 | 0 | 0 | 0 | 0 | 0.000x |
| `y_string_unicode` | 1600 | 26400 | 1271 | 33554400 | 59484198 | 56084246 | 0.943x |

Aggregate speedup over eligible fixed-width Unicode escape slices:
`1.268x`.

## Interpretation

`unicode_mixed` is recorded as zero eligible for C6 because its apparent `\u`
text is escaped-backslash data, not valid JSON Unicode escape syntax. The proof
therefore applies the aggregate threshold to `unicode_escapes` and
`y_string_unicode` while keeping escaped-backslash `\\u` in the differential
policy cases.

W8 proves that the current x4 Unicode escape path has caller-level headroom for
dense fixed-width escape contents. It does not move any `RESULTS.md` row and
does not wire new production behavior. W9 may consume this proof only for the
same primitive and caller.
