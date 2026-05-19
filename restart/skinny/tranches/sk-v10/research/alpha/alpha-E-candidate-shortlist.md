# Alpha-E Candidate Shortlist For SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-E shortlist. This is a contract shortlist only; it does
not dispatch implementation. Downstream S-P3 owns the detailed wave plan.

## Global Gates

Parse-only is retired as a SOTA close target. It remains diagnostic
substrate-guard evidence while all parse rows are `S / NO-GO`.

Micro-prove-first is mandatory for every substrate or kernel candidate. A
profile finding is not enough for S-P3 wave scoping. The candidate must first
show a same-host isolated micro-benchmark proving the primitive or call-site
change on representative slices, with scalar reference, intended consumer, host
flags, feature gates, and rejection threshold named.

## Candidate 1: Direct Output/Control-Path Contract

Goal: make `direct_to_struct` the first SK-V10 JSON frontier. Fourteen of
seventeen direct rows are still `N-direct / NO-GO`, while three digest rows are
already `A / GO`.

Owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- future S-P1/S-P2/S-P3 direct-profile artifacts

Falsifiability gate:
No direct row moves until S-P1 profiles direct rows specifically and S-P2/S-P3
define output-plane equivalence, independent Track 2 status, and strict
comparator semantics. Digest-only rows remain guard evidence until that
contract exists.

Risk:
HIGH. REDRESS 93 blocks scalar-parent folding; REDRESS 73 blocks helper-shape
transfer across generated/hand boundaries. It is still the largest winnable
pool in JSON.

## Candidate 2: `instruments` Typed Product Admission

Goal: add one measured `real_typed_struct` row for `instruments` first. It is
the most plausible typed generalization because it is a fixed top-level object
that the current typed DirectBuild schema model can express with structs,
arrays, options/nulls, and checksums.

Owner paths:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` only after gate disposition

Scalar reference:
serde_json and sonic-rs typed deserialization plus generated Track 1 typed
output. Track 2/oracle remains structurally independent and checksum-based.

Checkasm:
Not applicable.

Falsifiability gate:

- Generated Track 1, serde_json typed, sonic-rs typed, and Track 2/oracle
  checksums equal over the full fixture.
- Same-run `track1_real_typed_struct`, `track2_real_typed_struct`,
  `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct` Criterion
  rows exist with coherent run id and sample metadata.
- The row is `A / GO`: Track 1 time no worse than 1.10x sonic-rs typed time.
- Existing six typed GO rows maintain their current `A / GO` disposition.
- Any missing schema, parity, same-run metadata, or comparator evidence rejects
  without editing `RESULTS.md`.

Risk:
MEDIUM. The W1 row-table path is proven; the remaining risk is schema/parity and
whether `instruments` actually clears the typed sonic gate.

## Candidate 3: Root-Type Typed Generalization

Goal: unblock `github_events` and `gsoc-2018` typed rows by extending the typed
schema root model before attempting product admission.

Current blocker:
`github_events` is a top-level array and `gsoc-2018` is a top-level object map
keyed by numeric strings. The current `DirectRootSchema` points to named struct
types and the typed renderer is structured around `DirectTypeKind::Struct`.

Owner paths:

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`

Falsifiability gate:

- `DirectRootSchema` or successor root model can represent `Vec<T>` and
  map-entry roots without JSON-specific policy in generic code.
- Generated roots preserve full-fixture generated/serde/sonic checksum parity.
- No `RESULTS.md` row moves in the root-model wave unless the same wave also
  supplies measured typed comparator rows.

Risk:
MEDIUM-HIGH. This is a schema/codegen generalization, not a mechanical row-table
addition.

## Candidate 4: Existing-Substrate Unicode/String Kernel Pair

Goal: salvage only the W4 work that can be wired to existing hot paths without
W3: string block widening at `match_string_at_quote_trusted_utf8` and unicode
escape codec work at current unescape/string consumers.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`

Scalar reference:
Existing scalar string scanner, `read_hex_unit_scalar`,
`unescape_uxxxx_scalar`, and current string/unescape sink behavior.

Checkasm:
Required before production wiring. Missing standalone harnesses must be added
for the retained primitive surface; no orphan kernel ships.

Falsifiability gate:

- Micro-prove-first passes before S-P3 wave scoping.
- Same-wave production caller is a current caller such as
  `match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`,
  `decode_unicode_escape`, or `unescape_string`; W3 is not a caller.
- W10b maintain block holds on `canada`, `citm_catalog`, `instruments`,
  `marine_ik`, `mesh`, and `numbers`.
- Unicode rows may record NEAR-FAIL/FAIL honestly; no parse-only row becomes a
  SOTA admission while it remains `S / NO-GO`.

Risk:
HIGH until micro-proof and checkasm gaps close. Correctness/checkasm can be
bounded; row admission remains uncertain.

## Candidate 5: Comparator And Telemetry Refresh

Goal: create a clean SK-V10-open report identity and optional same-run sidecar
freshness manifest without allowing evidence ingestion to move behavior rows.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`

Falsifiability gate:
The refresh preserves current row dispositions unless a same-wave behavior gate
admits or rejects a row. Any new telemetry field must be consumed by
`gate-json` in the same wave. Sidecar freshness cannot produce parser data,
row output, substrate, or strict admission by itself.

Risk:
LOW-MEDIUM. Gate-only, but schema drift can corrupt later wave evidence.

## Rejected As SK-V10 Defaults

- W3 union/event substrate.
- W4 cascade-lock through W3.
- Canada typed shortcut without full-fixture proof.
- Parse-only SOTA close condition while parse rows remain `S / NO-GO`.
- Substrate/kernel intervention without micro-prove-first evidence.
- PMULL/CTZ production rewires as default hot paths.
