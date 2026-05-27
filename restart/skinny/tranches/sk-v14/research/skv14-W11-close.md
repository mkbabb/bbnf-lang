# SK-V14 W11 Close And Implementation Feedback

Date: 2026-05-27.

Status: W11 reconciliation was followed by W11A direct strict-product
implementation. W11A moved supported JSON `direct_to_struct` rows off the
digest plane and admitted thirteen strict product rows from cold native
`profile_direct` evidence.

## Authority

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 14.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R10.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` items 215 through 227.
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
   are absent: `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and
   `y_string_unicode`.
3. Missing JSON typed products remain for `gsoc-2018`, `unicode_mixed`,
   `unicode_escapes`, and `y_string_unicode`.
4. JSON parse_only residuals remain for `twitter`, `github_events`,
   `update_center`, `random`, `gsoc-2018`, and
   `distinct_values`.

## Reconciliation

- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` agree on
  eleven parse_only admits, thirteen direct admits plus four direct opens,
  thirteen typed admits plus four missing typed products, and twenty-four CSS
  L4 opens.
- `skinny/REDRESS.md` carries the live residuals as REDRESS-215,
  REDRESS-216, REDRESS-217, REDRESS-218, REDRESS-219, REDRESS-220,
  REDRESS-222, REDRESS-223, REDRESS-224, REDRESS-225, REDRESS-226,
  REDRESS-227, REDRESS-228, REDRESS-229, REDRESS-230, and REDRESS-231.
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
- Close invariants remain: 16 locks, Pattern H count 67, Lock 10 five-shape
  `BackendShape` canon preserved, and generated JSON parse_only remains
  distinct from the tape-building path.

## W11 Disposition

W11/W10R/W10S/W10T/W10V/W10W close SK-V14 as a mixed tranche, with admitted rows preserved and all
unmet rows routed to implementation residuals. W10X, W10Y/W10Z, W10AA, W9Y,
and W9AC add post-close residual rejection evidence; W9AA and W9AB add post-close typed
admits for `distinct_values/real_typed_struct` and
`canada/real_typed_struct`.
Under the latest user instruction, the next work is implementation against the
residual queue, not a new Omega or Alpha pass unless a future source attempt
exposes a spec-level amendment that truly requires G-Omega.
