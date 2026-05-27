# SK-V14 W11 Close And Implementation Feedback

Date: 2026-05-27.

Status: W11 reconciliation was followed by W11A direct strict-product
implementation. W11A moved supported JSON `direct_to_struct` rows off the
digest plane and admitted thirteen strict product rows from cold native
`profile_direct` evidence. W11B then tested unicode strict products and
rejected them on same-run cold evidence without landing the transient source
patch. W11C then tested `gsoc-2018` strict products across numeric-key,
ordered, identity, full, and required-full variants, rejecting them on
same-run cold evidence without landing the transient source patch. W11D then
tested a generated parse_only context-threaded delimiter route and rejected it
on same-run cold evidence without landing the transient source patch.

## Authority

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 14.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R10.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` items 215 through 234.
- `restart/skinny/ROLLING-SOTA-DELTA.md`.
- `restart/skinny/tranches/sk-v14/HANDOFF.md`.

## Wave Disposition Ledger

| Wave | Disposition | Evidence anchor |
|---|---|---|
| W0 | ADMITTED | SK-V14 telemetry manifest and gate-consumed audit overlay at `fb0048de0`. |
| W1 | ADMITTED | Comparator/equality prune and JSON audit ledger at `591eafb07` + `5595e41de`. |
| W2 | REJECTED then ADMITTED after W2R | Dual-tree cycle rejected by REDRESS-183; amended skinny-only `regen-css` admitted at `45568e669` and closed at `1a415fe84`. |
| W3 | ADMITTED | Production CSS corpus loader at `b0a864f0b`. |
| W4 | ADMITTED after W4R | Ledger-only CSS prune at `cb16a2ea0`; REDRESS-185..208 carry the 24 row keys. |
| W5 | REJECTED then split | REDRESS-209/210/211/212 forced the W5A, W5B.0..W5B.4, W5C-GEN, W5D-DELETE split. |
| W5A | ADMITTED | Source-consuming generation request at `286233fa2`. |
| W5B.0..W5B.4 | ADMITTED | Lock14 gate, import closure, layout discard, pretty/span projection, and request consumer closed at `7b58cf6a2`, `6777465aa`, `6d8b4cdf7`, `af995e4a9`, and `ca871db04`. |
| W5C-GEN | ADMITTED | Provider-free generator body closed at `747d79170`. |
| W5D-DELETE | ADMITTED | Provider/template deletion closed at `0549b3ce2`. |
| W6.0 | REJECTED then ADMITTED after W6.0R | REDRESS-213 rejected the first CSS root collapse; corrected projection source closed at `d5599f4ef`. |
| W6.1..W6.8 | ADMITTED | Remaining root runtime projections closed through `b4c47666f`. |
| W7 | ADMITTED | Policy/union runtime wiring closed at `672b927d5`; REDRESS-214 marks the numbers direct row as prune-consumed, not admitted. |
| W8 | REJECTED | REDRESS-215: 0 / 24 CSS L4 rows admitted; generated Track 1 remains fact-stream, not full-parse equality plane. |
| W9 | MIXED | REDRESS-216: 11 / 17 JSON typed rows admitted; 17 / 17 direct rows remain open; 6 / 17 typed product surfaces remain missing. |
| W9Y | REJECTED | REDRESS-226: generated `y_string_unicode/real_typed_struct` root measured below strict sonic typed; no row moved. |
| W9AA | ADMITTED | REDRESS-227: `distinct_values/real_typed_struct` admitted through generated dynamic string-entry capture; typed state is now 12 / 17 admitted and 5 / 17 missing. |
| W9AB | ADMITTED | REDRESS-228: `canada/real_typed_struct` admitted through generated numeric lexeme capture; typed state is now 13 / 17 admitted and 4 / 17 missing. |
| W9AC | REJECTED | REDRESS-229: generated `gsoc-2018/real_typed_struct` numeric-key route measured below strict sonic typed; no row moved. |
| W10 | MIXED | REDRESS-217: 6 / 17 JSON parse_only rows admitted; 11 / 17 parse_only rows remain open. |
| W10R | MIXED | REDRESS-218: `canada/parse_only` admitted by parse-only prefix continuation; parse_only state is now 7 / 17 admitted and 10 / 17 open. |
| W10S | MIXED | REDRESS-219: `unicode_mixed/parse_only` admitted by string-end prefix scan; parse_only state is now 8 / 17 admitted and 9 / 17 open. |
| W10T | MIXED | REDRESS-220: `instruments/parse_only` admitted by the cold open-row sweep after W10S; parse_only state is now 9 / 17 admitted and 8 / 17 open. |
| W10V | MIXED | REDRESS-222: `citm_catalog/parse_only` admitted by the current-HEAD cold resweep after W10U; parse_only state is now 10 / 17 admitted and 7 / 17 open. |
| W10W | MIXED | REDRESS-223: `apache_builds/parse_only` admitted by the generated parse-only iterative stack; parse_only state is now 11 / 17 admitted and 6 / 17 open. |
| W10X | REJECTED | REDRESS-224: inline frame stack, 64-byte trusted string scan, and trusted syntax-mask residual routes admitted no parse_only rows. |
| W10Y/W10Z | REJECTED | REDRESS-225: plain-string structural fast path and cursor-return helper ABI admitted no parse_only residual rows. |
| W10AA | REJECTED | REDRESS-230: fused trusted string-end helper plus object-loop cleanup admitted no parse_only residual rows. |
| W11A | ADMITTED | REDRESS-231: strict product `direct_to_struct` route admitted 13 / 17 JSON direct rows; 4 rows remain open for missing generated product surfaces. |
| W11B | REJECTED | REDRESS-232: transient unicode strict products for `unicode_mixed` and `unicode_escapes` passed correctness but missed strict sonic by at least 2014.202 Mbps; no source patch landed and no row moved. |
| W11C | REJECTED | REDRESS-233: transient `gsoc-2018` strict products passed correctness but missed strict sonic in every numeric-key, ordered, identity, full, and required-full variant; no source patch landed and no row moved. |
| W11D | REJECTED | REDRESS-234: transient parse_only value-context delimiter threading passed correctness but missed strict sonic on all six residual rows; no source patch landed and no row moved. |

## Close-State Counts

| Family | ADMITTED | OPEN | MISSING / blocked | Governing evidence |
|---|---:|---:|---:|---|
| JSON parse_only | 11 | 6 | 0 | W10/W10R/W10S/W10T/W10V/W10W cold `profile_direct` evidence and REDRESS-217/218/219/220/222/223. |
| JSON direct_to_struct | 13 | 4 | 0 | W11A cold strict product evidence and REDRESS-231; remaining rows lack generated product surfaces. |
| JSON real_typed_struct | 13 | 0 | 4 | W9 cold typed evidence plus W9AA/W9AB generated products for `distinct_values` and `canada`; remaining missing products listed in REDRESS-216/227/228/229. |
| CSS L4 | 0 | 24 | 0 | W8 production corpus rejection and REDRESS-215. |

No residual row has an architectural-level intrinsic-block proof. The
remaining rows are implementation residuals, not closeable proof blocks.

## Residual Queue

1. CSS L4 must gain generated Track 1 CSS full-parse output on the same
   equality plane as lightningcss/cssparser. Fact-stream adapters, tiny
   fixtures, and profile-template shortcuts remain rejected by REDRESS-215.
2. JSON direct residuals remain only where generated strict product surfaces
   are absent at HEAD: `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and
   `y_string_unicode`. W11B proved that product-surface-only unicode routes
   are not enough for `unicode_mixed` or `unicode_escapes`; W11C proved that
   product-surface-only `gsoc-2018` routes are also insufficient.
3. Missing JSON typed products remain for `gsoc-2018`, `unicode_mixed`,
   `unicode_escapes`, and `y_string_unicode`; W11B's unicode products were
   reverted after measured rejection, and W11C's `gsoc-2018` products were
   also reverted after measured rejection.
4. JSON parse_only residuals remain for `twitter`, `github_events`,
   `update_center`, `random`, `gsoc-2018`, and
   `distinct_values`. W11D proved that context-threaded delimiter consumption
   is not enough to move any of these rows.

## Reconciliation

- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` agree on
  eleven parse_only admits, thirteen direct admits plus four direct opens,
  thirteen typed admits plus four missing typed products, and twenty-four CSS
  L4 opens.
- `skinny/REDRESS.md` carries the live residuals as REDRESS-215,
  REDRESS-216, REDRESS-217, REDRESS-218, REDRESS-219, REDRESS-220,
  REDRESS-222, REDRESS-223, REDRESS-224, REDRESS-225, REDRESS-226,
  REDRESS-227, REDRESS-228, REDRESS-229, REDRESS-230, REDRESS-231,
  REDRESS-232, REDRESS-233, and REDRESS-234.
- `skinny/RESULTS.md` now renders CSS L4 legacy CostFacts as historical claims
  with current `AUDIT-FALSIFIED_OPEN` status, so the manifest no longer embeds
  live-looking `A` / `GO` / `ADMITTED-PARITY` fragments for OPEN CSS rows.
- `restart/skinny/tranches/sk-v14/HANDOFF.md` points to actual implementation
  residuals instead of another Omega/Alpha governance loop.

## Verification

- `cargo xtask gate-json --check-results --skv14-existing-results-capture`
  is the row/report consumer for this W11 reconciliation.
- `cargo test -p bbnf-bench skv14_json_parse_only_report_accepts -- --nocapture`
  passed after the report-renderer reconciliation and W10W row movement.
- `cargo test -p xtask -- --nocapture` passed after the generated report
  check.
- W9AA local evidence before this close packet update: `cargo xtask
  regen-real-typed`, `cargo xtask check-real-typed`, focused
  `distinct_values_typed` and `unknown_string_capture` tests, plus cold
  `profile_direct` evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.tsv`.
- W9AB local evidence before this close packet update: `cargo xtask
  regen-real-typed`, `cargo xtask check-real-typed`, focused `canada_typed`
  and `emits_typed_direct_number_string_capture` tests, plus cold
  `profile_direct` evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.tsv`.
- W9AC local evidence before this close packet update: `cargo xtask
  regen-real-typed`, `cargo xtask check-real-typed`, focused
  `gsoc_2018_typed` and `emits_typed_direct_u32_keyed_map_entries` tests, plus
  cold reject evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W9AC-gsoc-2018-typed.tsv`.
- W10AA local evidence before this close packet update: `cargo xtask
  regen-json`, `cargo xtask check-json`, focused parse-that-regex/runtime/codegen
  parse_only tests, plus cold reject evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10AA-parse-only-fused-string-object-loop.tsv`.
- W11B local evidence before this close packet update: `cargo run
  --profile ax-iter -p xtask -- regen-real-typed`, `cargo run --profile
  ax-iter -p xtask -- check-real-typed`, focused `unicode_` and
  `direct_strict_product` tests, plus cold reject evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W11B-unicode-products.tsv`.
- W11C local evidence before this close packet update: `cargo run
  --profile ax-iter -p xtask -- regen-real-typed`, `cargo run --profile
  ax-iter -p xtask -- check-real-typed`, focused `typed_direct_`,
  `gsoc_2018_typed`, and `direct_strict_product` tests, plus cold reject
  evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-products.md`.
- W11D local evidence before this close packet update: `cargo xtask
  regen-json`, `cargo xtask check-json`, focused runtime/codegen parse_only
  tests, plus cold reject evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W11D-parse-only-threaded-context.md`.
- Close invariants remain: 16 locks, Pattern H count 67, Lock 10 five-shape
  `BackendShape` canon preserved, and generated JSON parse_only remains
  distinct from the tape-building path.

## W11 Disposition

W11/W10R/W10S/W10T/W10V/W10W close SK-V14 as a mixed tranche, with admitted
rows preserved and all unmet rows routed to implementation residuals. W10X,
W10Y/W10Z, W10AA, W9Y, W9AC, W11B, W11C, and W11D add post-close residual
rejection evidence; W9AA and W9AB add post-close typed admits for
`distinct_values/real_typed_struct` and `canada/real_typed_struct`.
Under the latest user instruction, the next work is implementation against the
residual queue, not a new Omega or Alpha pass unless a future source attempt
exposes a spec-level amendment that truly requires G-Omega.

## SK-V14 W11C JSON GSoC Product Probe Reject

- Item 233 closes `G-SK-V14-W11C-JSON-GSOC-PRODUCTS` as `REJECT`. No source
  patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidates added transient generated strict product surfaces for
  `gsoc-2018`: numeric top-level object keys, ordered fixed-member parsing for
  the Schema.org proposal/sponsor/author objects, identity-product and
  full-product variants, plus required-field full-product parsing. The patch
  was reverted after measurement and retained as
  `/tmp/skv14-W11C-gsoc-products-rejected.patch` with SHA-256
  `258bdb69a286b0e60b57543f127be7c57ca0561a5657454d0ce5d7639a74faa9`.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-real-typed`, `cargo run --profile ax-iter -p xtask --
  check-real-typed`, `cargo test --profile ax-iter -p codegen typed_direct_ --
  --nocapture`, `cargo test --profile ax-iter -p bbnf-bench
  gsoc_2018_typed -- --nocapture`, and `cargo test --profile ax-iter -p
  bbnf-bench direct_strict_product -- --nocapture`.
- Cold `profile_direct` evidence rejects the closest full-product route:
  `gsoc-2018/real_typed_struct` measured `5789.034` Mbps against strict sonic
  `6482.407` Mbps, and `gsoc-2018/direct_to_struct` strict product measured
  `5834.269` Mbps against strict sonic `6111.175` Mbps. The identity-product
  route reached higher absolute Track 1 throughput (`19909.635` typed,
  `19938.076` direct-strict) but widened the strict sonic gap
  (`24783.657` typed sonic, `24927.218` direct-strict sonic).
- Current JSON direct_to_struct state remains 13 / 17 ADMITTED and 4 OPEN.
  Current JSON real_typed_struct state remains 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W11D JSON parse_only Threaded Context Reject

- Item 234 closes `G-SK-V14-W11D-JSON-PARSE-ONLY-THREADED-CONTEXT` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate threaded value context through the generated
  parse_only iterative parser so completed scalar values and empty containers
  immediately consumed their object/array delimiters. The patch was reverted
  after measurement and retained as
  `/tmp/skv14-W11D-parse-only-threaded-context-rejected.patch` with SHA-256
  `98b9494008e0d810699788c1ed8c667b2de29727301be6d27b3f6cf65d2b7146`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3898.964` Mbps, `github_events` margin `-3216.303` Mbps,
  `update_center` margin `-4231.665` Mbps, `random` margin `-2333.190` Mbps,
  `gsoc-2018` margin `-13844.268` Mbps, and `distinct_values` margin
  `-5258.386` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11D-parse-only-threaded-context.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.
