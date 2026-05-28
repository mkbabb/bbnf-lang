# Alpha-B - Competitor Deltas - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: strict comparator extraction and native-platform binding.
Output: this file.

## JSON Comparator State

JSON remains the guard baseline from SK-V15. PASS-IMPL V2 sustains the 51
strict measured JSON rows (`restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:15`).
Any SK-V16 wave touching JSON must re-run the touched rows on the same strict
plane and preserve Track 1 / Track 2 independence.

## CSS Comparator State

CSS L4 has no current SOTA admit. Rolling delta keeps every CSS row `OPEN`
and states that CSS remains open until a fresh typed row beats `cssparser`
with typed-summary equality (`restart/skinny/ROLLING-SOTA-DELTA.md:95-99`).

The SK-V15 close packet records the W11 retime:

| comparator | outcome |
|---|---|
| Track 1 typed parser | `2/4` corpus parses |
| cssparser typed comparator | `4/4` corpus parses |
| typed summary equality | false |
| Track 1 Mbps | `3.426` |
| cssparser Mbps | `1995.168` |
| admitted rows | `0` |

Evidence:
`restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:24-30`.

## SK-V16 Comparator Binding

| Domain | Admission comparator | Diagnostic only |
|---|---|---|
| JSON parse_only | strict sonic-rs skipper and strict same-row Track 2 guard | permissive or lossy rows |
| JSON direct_to_struct | strict product plane | digest-only equality |
| JSON real_typed_struct | strict typed product plane | closed-enum/FNV sidecars |
| CSS L4 | cssparser same-workload typed CSS document/value summary | lightningcss until Track 1 emits comparable CSSOM/value output |

## Native SIMD Binding

Admission and SIMD work are Apple M5 Max / aarch64 only. x86 and AVX-512 rows
are diagnostic signals and must not consume SK-V16 implementation scope.

Deep SIMD work is allowed only after S-P1 identifies a native hot leaf and the
wave carries:

- scalar reference implementation;
- checkasm or parity command;
- same-wave hot-path consumer;
- cold per-parse measurement on aarch64;
- strict output-plane equality.

## CSS Report Contract

CSS admission requires a gate-consumed report, not prose-only telemetry. The
report must include schema id, run id, host triple, native flags, corpus
manifest, corpus files, corpus bytes, value plane, comparator workload, live
admission sources, retired legacy proof count, Track 1 pass/error counts,
cssparser pass/error counts, typed summaries, typed-summary equality, Track 1
Mbps, cssparser Mbps, threshold, margin, admitted row count, and disposition.

S-P3 must name or author the executable consumer. The expected gate shape is:

- `cargo xtask gate-json --check-results --skv16-css-typed-report <path>`;
- `cargo xtask gate-json --check-results --skv16-dirty-generated-report <path>`;
- `cargo xtask gate-json --check-results --skv16-pattern-h-roundtrip-report <path>`;
- `cargo xtask gate-json --check-results --skv16-native-simd-report <path>` when native SIMD is in scope.

These are skinny xtask consumers; S-P3 must write them with `(cd skinny && ...)`
or `--manifest-path skinny/Cargo.toml` qualification.

Legacy proof sources that must fail a live CSS admission report:
`CSS_GENERATED_RS`, `emit_fact_stream`, `CssFullParseSummary`, `parse_full`,
brace-counter summaries, fact-stream-only output, and W8R broadcast rows.
