# SK-V7 Wave 2 — B1: per-`\uXXXX` TBL classifier + fused materializer

Status: design (uncommitted). Scope: parse-that-regex + bbnf-simd reuse + one
codegen-emitted call-site update. No grammar change. No new IR variant. No
sidecar. Hard cap 60 min implementation, 30 min measurement.

## 1. Provenance and Mandate

SK-V7 Wave A converged on the per-unit `\uXXXX` decode primitive as the single
highest-impact admission for the remaining unicode/escape gap.

- A2 (`restart/skinny/audit/skv7-A2-sota-strict-beat.md`): names this primitive
  as the moving lever for nine of thirteen parse-G rows. Quantified targets:
  `unicode_mixed` parse (currently 56.1 % sonic), `y_string_unicode` parse
  (46.0 %), `unicode_escapes` parse (80.4 %), `random` parse (65.5 %), plus
  five string-match-bottlenecked rows whose dominant residual is the inner
  `\uXXXX` decode loop: twitter, update_center, apache_builds, github_events,
  gsoc-2018.
- A4 (`restart/skinny/audit/skv7-A4-parse-that-gaps.md`): Top-1 missing
  primitive, named `unicode_escape_run_decode_utf8`. Calls out reuse of the
  existing `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` TBL kernel
  (already in tree at lines 74-121 of
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`) and the existing
  Hoehrmann scalar at lines 979-1047 of
  `skinny/crates/parse-that-regex/src/lib.rs`. Estimated 150-300 LOC of new
  code; remainder is wiring.
- `restart/skinny/audit/HANDOFF-SK-V6.md:76-80`: explicitly distinguishes this
  (per-unit decode) from the rejected "broad UTF-8 fusion as generated-baseline
  close." The broad fusion is dead. This candidate has structurally different
  shape and is admitted.

## 2. Distinction from REDRESS 64 (rejected four-unit run validator)

REDRESS 64 (`skinny/REDRESS.md:1582-1635`) rejected the four-unit contiguous
`\uXXXX` *validator* shape. The patch lifted `unicode_escapes` by +31.8 % but
regressed `y_string_unicode` by −3.7 % and failed to move `unicode_mixed`
(+1.8 %) or `gsoc-2018` (+0.2 %) past the +5 % companion threshold.

Why it failed (REDRESS 64 self-diagnosis at lines 1620-1628):

> dense contiguous Unicode-escape runs are only the `unicode_escapes` row.
> `y_string_unicode` has short runs and boundary-crossing surrogate shapes
> that do not amortize the four-unit path; `unicode_mixed` and `gsoc-2018` are
> not primarily fixed-width Unicode-escape validation rows.

Three structural differences in the present candidate (B1):

| Axis | REDRESS 64 (rejected) | B1 (present) |
|---|---|---|
| Granularity | Batch of 4 contiguous `\uXXXX` units | Single `\uXXXX` quartet at a time |
| Operation | *Validate* hex (mask check, no decode emit) | *Decode* hex into u16 + emit into output buffer |
| Caller shape | `validate_json_unicode_escape_run` (validation pass) | `unescape_json_string` (materialization pass) |
| Amortization assumption | Requires 4 consecutive `\u` escapes | Pays per single `\u`; no run requirement |
| Surrogate handling | Failed at unit-3/unit-4 boundary cross | Surrogate join is on the decode side, naturally per-pair |

The key insight: dense `\uXXXX` runs (validation amortization) and per-unit
decode throughput (single-escape amortization) are different facts on
different rows. `y_string_unicode` has short, often-1 escapes interleaved with
ASCII; `unicode_mixed` mixes 1-byte BMP escapes with literal UTF-8;
`gsoc-2018` has scattered single escapes. None benefit from validation
batching but all are dominated by the cost of the per-quartet decode +
surrogate join inside the materializer loop.

REDRESS 64 also touched only `validate_json_unicode_escape_run`
(lines 478-514). B1 touches `unescape_json_string` (lines 854-946) —
the materialization path. These call sites are disjoint: the validator runs
during `match_json_string` (parse-time validity), the materializer runs during
post-validation `unescape_json_string` (decoded-value production).

## 3. Existing Substrate (reused)

### 3.1 NEON TBL kernel (already in tree, parity-tested)

`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74-121`
(`unescape_uxxxx_neon`): single-quartet TBL kernel. Loads 4 ASCII hex bytes
into vector lanes 0..=3, runs `vqtbl1q_u8` against `HEX_NIBBLE_LUT`, masks
with explicit ASCII range checks for digit/upper/lower hex, ORs the alpha
adjust, returns `Option<u32>` (the decoded `u16` widened, or `None` on
invalid hex). Already exists; checkasm-tested via existing parity harness.

`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125-166`
(`unescape_uxxxx_x4_neon`): batched 4-quartet variant — the kernel that
REDRESS 64 used. We **do not** wire this in B1's hot path; it is retained as
a future optimization for the rare dense-run case but only if a new profile
names it.

`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:169-175`
(`join_surrogate_pair_neon`): algebraic surrogate combine, range-checked.
Reused verbatim.

`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-47`
(`unescape_uxxxx_scalar`): the bit-identical scalar parity anchor. The
checkasm differential test below pins all backends to this reference.

### 3.2 Scalar Hoehrmann UTF-8 validator (already in tree)

`skinny/crates/parse-that-regex/src/lib.rs:979-1047`
(`validate_utf8_codepoint`): the byte-level state machine. This is what we
emit *into* — once `\uXXXX` has produced a `u16` (or pair-joined `u32`), we
encode it to UTF-8 bytes and the downstream consumer of `unescape_json_string`
treats that segment as pre-validated UTF-8. Hoehrmann is not invoked on the
decoded segment because the encoder produces canonical UTF-8 by construction.

### 3.3 Current scalar decode path (replaced)

`skinny/crates/parse-that-regex/src/lib.rs:434-476`
(`decode_json_unicode_escape`): per-escape scalar decoder that calls
`read_hex_unit_with_error_offset` (lines 1054-1078) → `read_hex_unit_scalar`
(lines 1080-1092), each invoking `hex_nibble` (lines 1094-1102) four times.
This is the hot path that B1 short-circuits on aarch64.

The call site is `unescape_json_string` at
`skinny/crates/parse-that-regex/src/lib.rs:911-922`:

```rust
Some(b'u') => {
    let slash = cursor - 1;
    #[cfg(target_arch = "aarch64")]
    if let Some(batch) = unescape_four_unicode_escapes(bytes, slash, &mut out) {
        cursor = batch?;
        segment_start = cursor;
        continue;
    }
    let (ch, next) = decode_json_unicode_escape(bytes, slash)?;
    out.push(ch);
    cursor = next;
}
```

The four-unit batch path (`unescape_four_unicode_escapes`, lines 516-591) only
activates when four consecutive `\uXXXX` escapes are present. Outside that
amortization, the scalar `decode_json_unicode_escape` runs. B1's intervention
is on this scalar fallback.

## 4. Kernel Design

### 4.1 Public API surface

New file: `skinny/crates/parse-that-regex/src/unicode/escape_decode.rs`
(approximately 80 LOC).

```rust
//! Per-`\uXXXX` TBL classifier + fused UTF-8 materializer.
//!
//! Single-quartet entry: hot path for short `\uXXXX` escapes interleaved with
//! ASCII (twitter, github_events, gsoc-2018) and for boundary-crossing
//! surrogate pairs (y_string_unicode). The four-unit batched variant
//! (`unescape_uxxxx_x4_neon`) remains opt-in for the dense-run case only.

use crate::RegexError;

/// Decode a single `\uXXXX` quartet (the four hex ASCII bytes after `\u`)
/// into a `u16` UTF-16 code unit. Bit-identical to the scalar reference.
///
/// On aarch64 with NEON, dispatches to `unescape_uxxxx_neon`. Other targets
/// fall through to the scalar Hoehrmann-paired hex decoder.
#[inline]
pub fn decode_unicode_escape(quartet: &[u8; 4]) -> Option<u16> { /* ... */ }

/// Decode a single `\uXXXX` quartet **and** materialize it as UTF-8 bytes
/// directly into the output buffer. Handles BMP single units (1-3 UTF-8
/// bytes) inline. Surrogate pairs are signalled to the caller via the
/// `HighSurrogate(u16)` discriminant so the caller can fetch the trailing
/// `\uXXXX` and call `materialize_surrogate_pair` in one call.
#[inline]
pub fn materialize_unicode_escape(
    quartet: &[u8; 4],
    out: &mut String,
) -> Result<DecodeOutcome, RegexError> { /* ... */ }

pub enum DecodeOutcome {
    /// Single BMP code unit was materialized; advance cursor past the 6-byte
    /// `\uXXXX` escape.
    Bmp,
    /// High surrogate seen; caller must consume the trailing `\uXXXX`.
    HighSurrogate(u16),
}

/// Algebraic combine of a high+low surrogate pair followed by UTF-8 emission.
/// Bit-identical to `char::encode_utf8(combine(high, low))`.
#[inline]
pub fn materialize_surrogate_pair(
    high: u16,
    low_quartet: &[u8; 4],
    out: &mut String,
) -> Result<(), RegexError> { /* ... */ }
```

### 4.2 Body sketch — `materialize_unicode_escape`

```rust
#[inline]
pub fn materialize_unicode_escape(
    quartet: &[u8; 4],
    out: &mut String,
) -> Result<DecodeOutcome, RegexError> {
    #[cfg(target_arch = "aarch64")]
    let unit = unsafe {
        // SAFETY: quartet is a 4-byte reference; the NEON kernel reads
        // exactly four bytes via vld1q_lane_u8::<0..=3>.
        bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon(quartet.as_ptr())
    };
    #[cfg(not(target_arch = "aarch64"))]
    let unit = bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_scalar(quartet);

    let unit = unit.ok_or(RegexError {
        offset: 0, // caller adjusts to absolute offset
        kind: crate::RegexErrorKind::InvalidUnicodeEscape,
    })? as u16;

    if (0xd800..=0xdbff).contains(&unit) {
        return Ok(DecodeOutcome::HighSurrogate(unit));
    }
    if (0xdc00..=0xdfff).contains(&unit) {
        return Err(RegexError {
            offset: 0,
            kind: crate::RegexErrorKind::InvalidSurrogatePair,
        });
    }

    // Inline UTF-8 emission: BMP non-surrogate → 1, 2, or 3 bytes.
    // SAFETY: encode_utf8 is canonical for any valid char.
    let scalar = unit as u32;
    // SAFETY: scalar is BMP non-surrogate → always a valid char.
    let ch = unsafe { char::from_u32_unchecked(scalar) };
    let mut buf = [0u8; 4];
    out.push_str(ch.encode_utf8(&mut buf));
    Ok(DecodeOutcome::Bmp)
}
```

The "fused materializer" payoff: today's scalar path constructs a `char` and
calls `String::push`, which re-encodes through `char::encode_utf8` and a
bounds-checked `push_str`. The new path produces the UTF-8 bytes once and
appends them directly. For BMP single-byte (ASCII-range escapes like
`A`), this is one `vec.extend_from_slice(&buf[..1])`. The NEON kernel
already collapses the four `hex_nibble` calls into `vqtbl1q_u8` + range mask.

### 4.3 Surrogate pair body

```rust
#[inline]
pub fn materialize_surrogate_pair(
    high: u16,
    low_quartet: &[u8; 4],
    out: &mut String,
) -> Result<(), RegexError> {
    #[cfg(target_arch = "aarch64")]
    let low = unsafe {
        bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon(low_quartet.as_ptr())
    };
    #[cfg(not(target_arch = "aarch64"))]
    let low = bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_scalar(low_quartet);

    let low = low.ok_or(RegexError {
        offset: 0,
        kind: crate::RegexErrorKind::InvalidUnicodeEscape,
    })? as u16;

    let scalar = bbnf_simd::aarch64::unescape_uxxxx::join_surrogate_pair_neon(
        high as u32,
        low as u32,
    ).ok_or(RegexError {
        offset: 0,
        kind: crate::RegexErrorKind::InvalidSurrogatePair,
    })?;

    // SAFETY: join_surrogate_pair_neon range-checks both halves.
    let ch = unsafe { char::from_u32_unchecked(scalar) };
    let mut buf = [0u8; 4];
    out.push_str(ch.encode_utf8(&mut buf));
    Ok(())
}
```

## 5. Consumer Integration

### 5.1 Call site in `unescape_json_string`

Replace `parse-that-regex/src/lib.rs:911-922` with:

```rust
Some(b'u') => {
    let slash = cursor - 1;
    // Existing four-unit batch path stays for dense `\uXXXX\uXXXX\uXXXX\uXXXX`
    // runs (REDRESS 64 narrowed it to `unicode_escapes` row only; we keep it
    // because that row's +31.8 % was real and shippable on its own merits).
    #[cfg(target_arch = "aarch64")]
    if let Some(batch) = unescape_four_unicode_escapes(bytes, slash, &mut out) {
        cursor = batch?;
        segment_start = cursor;
        continue;
    }
    // B1 single-quartet path: hot for short escapes + surrogate pairs.
    let hex_start = slash + 2;
    let quartet: &[u8; 4] = bytes
        .get(hex_start..hex_start + 4)
        .and_then(|s| s.try_into().ok())
        .ok_or(RegexError {
            offset: slash,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        })?;
    let outcome = unicode::escape_decode::materialize_unicode_escape(quartet, &mut out)
        .map_err(|mut err| { err.offset = slash; err })?;
    cursor = hex_start + 4;
    if let DecodeOutcome::HighSurrogate(high) = outcome {
        // Trailing `\uXXXX` for the low half.
        if bytes.get(cursor) != Some(&b'\\') || bytes.get(cursor + 1) != Some(&b'u') {
            return Err(RegexError {
                offset: slash,
                kind: RegexErrorKind::InvalidSurrogatePair,
            });
        }
        let low_hex_start = cursor + 2;
        let low_quartet: &[u8; 4] = bytes
            .get(low_hex_start..low_hex_start + 4)
            .and_then(|s| s.try_into().ok())
            .ok_or(RegexError {
                offset: low_hex_start,
                kind: RegexErrorKind::InvalidUnicodeEscape,
            })?;
        unicode::escape_decode::materialize_surrogate_pair(high, low_quartet, &mut out)
            .map_err(|mut err| { err.offset = low_hex_start; err })?;
        cursor = low_hex_start + 4;
    }
}
```

The replaced `decode_json_unicode_escape` (lines 434-476) is retained and made
`pub(crate)` because integration tests call it directly
(`parse-that-regex/src/lib.rs:1293-1308`). It is no longer in the
materializer hot path.

### 5.2 Codegen path (no change)

`skinny/crates/codegen/src/json_typed_direct.rs:28` and
`skinny/crates/codegen/src/json_templates/view.rs:4` both call
`unescape_json_string` as a black box. No codegen template change is needed —
the new fast path lives entirely behind the same function symbol.

`skinny/crates/runtime/src/grammars/json/sink.rs:1,19,30,46,87` and
`skinny/crates/runtime/src/grammars/json/view.rs:213` — same observation.
Re-running `cargo run -p xtask --release -- check-json` after the patch is a
no-op for the generated tree.

This is the "same-wave consumer rule" satisfied: the new kernel lives in
`escape_decode.rs` and is wired in `unescape_json_string` in the same commit;
no orphan kernel.

## 6. Falsifiability Gate

### 6.1 Predicted row-level lifts (parse-G, retained)

| Row | Baseline sonic | Predicted post-B1 sonic | Threshold to pass | Reasoning |
|---|---:|---:|---:|---|
| unicode_mixed parse | 56.1 % | ≥ 78 % | +12 % absolute | Mixed BMP escapes + literal UTF-8; per-quartet decode is the dominant residual after L4 string scan. |
| y_string_unicode parse | 46.0 % | ≥ 70 % | +12 % absolute | Short escape runs + surrogate pairs; REDRESS 64 regressed this row, B1 must reverse the regression and lift it. |
| unicode_escapes parse | 80.4 % | ≥ 88 % | +5 % absolute | Already partially covered by `unescape_four_unicode_escapes`; B1 picks up the residual non-4-aligned tail. |
| distinct_values parse | 60.2 % | ≥ 75 % | +10 % absolute | Per-A4: string-match-bottlenecked row whose dominant inner loop is hex decode. |

### 6.2 May-lift rows (no veto if flat)

| Row | Baseline sonic | Hope | Veto threshold |
|---|---:|---:|---:|
| random parse | 65.5 % | ≥ 72 % | regression ≥ −3 % |
| twitter parse | 73.6 % | ≥ 80 % | regression ≥ −3 % |
| update_center parse | 59.6 % | ≥ 68 % | regression ≥ −3 % |
| apache_builds parse | (per A2) | ≥ +3 % | regression ≥ −3 % |
| github_events parse | (per A2) | ≥ +3 % | regression ≥ −3 % |
| gsoc-2018 parse | (per A2) | ≥ +3 % | regression ≥ −3 % |

### 6.3 Direct-workload predictions

Identical row set, identical thresholds. The direct receiver
(`json_typed_direct.rs:473`) calls `unescape_json_string` on the same raw
spans; the same per-quartet kernel sits behind it. If direct rows do not move
in lockstep with parse-G rows, the change is suspect (either the receiver is
not exercising the new code path, or the bench harness is timing
non-string-dominated rows).

### 6.4 Exit gate

B1 passes if **at least two of the four named must-lift rows** cross their
absolute thresholds **on both parse-G and direct**, AND no row in either
workload regresses by ≥ 3 %.

If only one must-lift row crosses: REJECT, revert, record in REDRESS as B1
(distinct from REDRESS 64 because the rejected shape is different).

If a row regresses ≥ 3 %: REJECT, revert. The likely cause would be cold-cache
inlining bloat — `unescape_json_string` is `pub` and called from many
generated sites; if `materialize_unicode_escape` is not inlined hot enough,
the indirect-call cost dominates the saved hex-nibble cycles.

### 6.5 Cold per-parse measurement only

Per the memory-banked `no-warm-benches` discipline, all measurements are cold
per-parse (no warmup). The bench harness is `bbnf-bench` Track 2 JSON parity;
the existing `gate.rs` already enforces cold runs. Median of 5 parses; 3
runs total to surface noise.

## 7. Scalar Reference (parity anchor)

The bit-identical scalar reference is already in tree at
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-47`
(`unescape_uxxxx_scalar`). No new scalar reference is needed for the hex
decode itself.

For the per-quartet *materializer* (the new addition), the scalar reference
is:

```rust
// Scalar reference for materialize_unicode_escape.
// This is the parity anchor that the NEON-backed implementation must match
// byte-for-byte for every (quartet, surrogate_state) input.
#[cfg(test)]
fn materialize_unicode_escape_scalar(
    quartet: &[u8; 4],
    out: &mut String,
) -> Result<DecodeOutcome, RegexError> {
    let unit = bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_scalar(quartet)
        .ok_or(RegexError {
            offset: 0,
            kind: crate::RegexErrorKind::InvalidUnicodeEscape,
        })? as u16;
    if (0xd800..=0xdbff).contains(&unit) {
        return Ok(DecodeOutcome::HighSurrogate(unit));
    }
    if (0xdc00..=0xdfff).contains(&unit) {
        return Err(RegexError {
            offset: 0,
            kind: crate::RegexErrorKind::InvalidSurrogatePair,
        });
    }
    let ch = char::from_u32(unit as u32).expect("BMP non-surrogate is always char");
    out.push(ch);
    Ok(DecodeOutcome::Bmp)
}
```

The only behavioural difference between `materialize_unicode_escape` and
its scalar reference is the path the UTF-8 bytes take into `out` —
`push(char)` vs `push_str(&buf[..n])` — which is observably identical at the
`out` byte-level.

## 8. checkasm Differential Test

New file: `skinny/crates/bbnf-simd/tests/checkasm_unicode_escape.rs`
(approximately 150 LOC).

Mirrors `tests/checkasm_byte_class_from_eq_set_64.rs:1-60` structurally
(deterministic xorshift, alignment sweeps, signal trampoline, adversarial
seeds).

Test surface:

1. **Exhaustive BMP enumeration**: every `u16` from `0x0000` to `0xFFFF`
   formatted as `\uXXXX` hex (uppercase, lowercase, mixed-case nibbles —
   3 × 65 536 = 196 608 cases). For each, scalar and NEON must return the
   same `u16` and `materialize_unicode_escape` must produce byte-identical
   `out` buffers.
2. **Invalid hex sweep**: every byte 0x00..=0xFF in each of the four
   positions; for any byte not in `[0-9A-Fa-f]`, both backends must return
   `None`/`Err` with identical error semantics.
3. **Surrogate-pair sweep**: all 1 024 × 1 024 valid (high, low) pairs in
   `(0xD800..=0xDBFF, 0xDC00..=0xDFFF)`, plus 1 024 invalid (high,
   non-low-surrogate) and 1 024 (non-high-surrogate, low) crosses.
4. **Adversarial xorshift seeds**: seeds drawn from `checkasm_parity.rs:55`
   (pre-clobber detection per V6 hardening); 65 536 random inputs across
   `\uXXXX` shape and across raw hex shape; differential vs scalar with
   stack-clobber-then guard.
5. **Alignment sweep**: place the quartet at offsets 0..16 within a 128-byte
   backing buffer; verify NEON kernel reads exactly four bytes (no
   over-read), via signal trampoline.

The xorshift adversarial seeds are critical: REDRESS 28 and REDRESS 33
(memory-banked `match_tiny_plain_string` history) showed pre-clobber bugs
that only surface under adversarial alignment.

## 9. LOC Budget

| File | LOC | Notes |
|---|---:|---|
| `parse-that-regex/src/unicode/escape_decode.rs` | ~80 | new file: 3 pub fns + `DecodeOutcome` enum |
| `parse-that-regex/src/unicode/mod.rs` | +1 | `pub mod escape_decode;` |
| `parse-that-regex/src/lib.rs` | ~30 | replace lines 911-922 with the B1 path |
| `bbnf-simd/tests/checkasm_unicode_escape.rs` | ~150 | new file: xorshift, alignment sweep, BMP enumeration, surrogate sweep |
| `parse-that-regex/tests/escape_decode_parity.rs` | ~40 | scalar reference + per-BMP byte-equality vs `decode_json_unicode_escape` |
| Total | ~301 | within A4's 150-300 LOC estimate (rounded up by test surface) |

No file in `skinny/crates/runtime/`, `skinny/crates/codegen/`, or
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` is modified. The
existing kernel is reused, not rewritten.

## 10. Wave Plumbing

- Wave 2 of SK-V7 (Wave 0 = comparator-plane repair, Wave 1 = legacy
  fold-back, Wave 2 = unicode/escape close, Wave 3+ = subsequent A2 rows).
- Hard cap 60 min implementation. Single-pass: write
  `escape_decode.rs`, write checkasm test, write parity test, edit
  `lib.rs` call site, regenerate (no-op for codegen), run
  `CARGO_TARGET_DIR=/tmp/skv7-B1-target cargo test -p parse-that-regex -p
  bbnf-simd --profile ax-iter`, fix any parity divergence.
- Hard cap 30 min measurement. Single-pass: build release binaries (baseline
  HEAD vs candidate HEAD~1), run `bbnf-bench` Track 2 JSON parity in cold
  per-parse mode, median-of-5 × 3 runs, archive raw CSV to
  `/tmp/skv7-B1-bench.csv`, summary to `/tmp/skv7-B1-summary.csv`, evaluate
  exit gate.
- On reject: `git checkout -- skinny/crates/parse-that-regex/
  skinny/crates/bbnf-simd/tests/checkasm_unicode_escape.rs`, save the
  rejected patch to `/tmp/skv7-B1-rejected.patch`, record a REDRESS B1 entry
  with raw measurements and the row-by-row threshold-failure analysis.
- On pass: single commit. Title:
  `feat(parse-that-regex): per-\uXXXX TBL classifier + fused UTF-8 materializer (SK-V7 B1)`.
  Body cites this design file, the A2 row list, the A4 estimate, the four
  must-lift thresholds, and the achieved deltas.

## 11. Non-goals and Carry-overs

- Broad UTF-8 fusion as a generated-baseline close is rejected
  (HANDOFF-SK-V6.md:76-77). B1 does not touch the UTF-8 *validation* path
  (`validate_utf8_codepoint`, `utf8_block`, `utf8_hoehrmann`).
- Four-unit batched *validation* (REDRESS 64) is not reopened. Four-unit
  batched *decode* (`unescape_uxxxx_x4_neon`) stays as the dense-run path it
  already is; B1 sits below it in the dispatch order.
- No grammar change. `JsonString` typed semantics are unchanged. No new
  `TypeDesc` variant. No new sink method. No new BIR variant. No new
  PrettifyHint.
- x86_64 path is not added in this wave. The aarch64 NEON kernel is gated
  by `#[cfg(target_arch = "aarch64")]`; other targets fall through to the
  scalar reference. If B1 passes on aarch64, an x86_64 AVX2/AVX-512 follow-up
  becomes Wave 3 — out of scope here.
- CSS Unicode escapes (`\41 ` style) are not touched. CSS L4 has its own
  escape-decode path in `bbnf-css`; that is a separate candidate.

## 12. Anti-regression Cross-check

Before final commit, run `cargo run -p xtask --release -- check-json` and
`cargo run -p xtask --release -- check-conformance` to ensure no generated
file drifted (the change is supposed to be invisible at the generated layer)
and no conformance fixture regressed. These checks are the on-disk gate per
REDRESS commentary at `skinny/REDRESS.md:1571-1574`.

## 13. References

- `restart/skinny/audit/skv7-A2-sota-strict-beat.md` — row-impact table for
  the unicode/escape cluster.
- `restart/skinny/audit/skv7-A4-parse-that-gaps.md` — Top-1 primitive listing,
  LOC estimate, existing-kernel reuse argument.
- `restart/skinny/audit/HANDOFF-SK-V6.md:76-80` — distinguishes per-unit decode
  from rejected broad UTF-8 fusion.
- `skinny/REDRESS.md:1582-1635` — REDRESS 64 rejection of four-unit retained
  validator; the structural baseline this design must out-distinguish.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-175` — reused
  NEON kernels + parity anchor.
- `skinny/crates/parse-that-regex/src/lib.rs:434-591,854-946,979-1102` —
  call sites: scalar decoder, four-unit batched, materializer, Hoehrmann.
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1-60` —
  structural template for the new differential harness.
