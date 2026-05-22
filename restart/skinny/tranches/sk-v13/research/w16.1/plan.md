# SK-V13 W16.1 Plan - Unicode Escape-Run Validation

Date: 2026-05-22.
Gate: `G-W16.1-JSON-UNICODE-ESCAPE-RUN-VALIDATION`.

## Selected Intervention

Add an aarch64-only fast path inside
`parse_that_regex::validate_unicode_escape_run` for four consecutive
non-surrogate JSON unicode escape units:

```text
\uXXXX\uXXXX\uXXXX\uXXXX
```

The fast path packs the four hex quartets, calls the existing
`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`, rejects invalid
hex with the existing error kind, and advances by 24 bytes only if all four
decoded units are outside the surrogate range. Any high or low surrogate keeps
the current scalar path so surrogate-pair validation and error offsets remain
unchanged.

## Implementation Shape

1. Add a small validation helper local to `parse-that-regex/src/lib.rs`:
   `validate_four_unicode_escapes(input, slash) -> Option<Result<usize, RegexError>>`.
2. In `validate_unicode_escape_run`, attempt the helper before the scalar
   first-unit decode. On `Some(Ok(next))`, set `slash = next` when another
   `\u` follows or return `Ok(next)`.
3. On `Some(Err(error))`, return the error. On `None`, run the existing scalar
   surrogate-aware branch unchanged.
4. Add unit tests covering valid four-unit BMP batches, invalid hex inside a
   batch, high-surrogate fallback, low-surrogate fallback/error, and mixed
   batched/scalar tails.
5. Keep `unescape_string` unchanged; it is already the REDRESS 107
   materializer consumer.

No public API, directive, BIR, BackendShape, substrate, CSS, or generated JSON
parser change is in scope.

## Micro-Prove-First

Before row admission, redress must prove that the validation helper is faster
than the scalar validation loop on a corpus-derived unicode escape slice. The
retained proof may be a focused Criterion lane or a temporary microbench
artifact archived under `restart/skinny/tranches/sk-v13/research/w16.1/`.

Required proof facts:

- host `aarch64-apple-darwin`;
- `RUSTFLAGS="-C target-cpu=native"`;
- scalar oracle is current `validate_unicode_escape_run` behavior;
- candidate slice includes valid consecutive `\uXXXX` runs from
  `unicode_escapes`;
- parity includes invalid-hex and surrogate fallback cases.

If the micro-proof fails, save `/tmp/skv13-waveW16.1-rejected.patch`, revert
the patch, and record REDRESS without row/status changes.

## Falsifiability Gate

W16.1 admits only if:

- `json/unicode_escapes/parse_only/main` Track 1 exceeds same-run sonic strict
  parse-only by at least 1 Mbps;
- `json/y_string_unicode/parse_only/main` is measured and records PASS, MISS,
  or REJECT honestly;
- `cargo test -p parse-that-regex unescape -- --nocapture` passes;
- `cargo test -p bbnf-bench parity -- --nocapture` passes or an equally scoped
  JSON parity test covers both target corpora;
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd unescape_uxxxx_x4_matches_scalar -- --nocapture` passes;
- `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture` passes;
- the retained Criterion run covers
  `json/(unicode_escapes|y_string_unicode)/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`;
- `gate-json --check-results` consumes any status movement or measured reject.

Guard condition: no previously admitted JSON row silently demotes in
`ROLLING-SOTA-DELTA.md`. This wave does not claim direct, typed, CSS, union, or
decision-engine movement.

## Revert Protocol

Revert only:

- `skinny/crates/parse-that-regex/src/lib.rs`;
- any W16.1-only gate/report/status files added during redress;
- W16.1 research artifacts under
  `restart/skinny/tranches/sk-v13/research/w16.1/` that are patch evidence, not
  the already-committed research/plan/challenge docs.

Dirty CSS parity sidecar JSON files stay unstaged.
