# SK-V13 W11.1 Research - JSON Direct Numeric-Array Reopen

Date: 2026-05-22.
Scope: read-only W11.N research cohort for the first JSON
`direct_to_struct` reopen under SPEC Section 15 and the 2026-05-21
addendum.

## Authority

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 15.
- `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md` W11.N contract.
- `skinny/RESULTS.md` head after SK-V13 W9.
- REDRESS 119/120 are history only; every row needs a fresh material
  differential.

## Six-Agent Cohort

### A - Row Table And First Target

The strict addendum bar is `Track 1 > sonic-rs strict + 1` on the same
corpus and output plane. Direct rows still below that pinned bar:

| Row | Status before W11.1 | Track 1 | sonic strict | +1 bar | Margin |
|---|---|---:|---:|---:|---:|
| `unicode_escapes/direct_to_struct` | `N-direct` | 5127 | 14040 | 14041 | -8914 |
| `gsoc-2018/direct_to_struct` | `N-direct` | 15318 | 23899 | 23900 | -8582 |
| `unicode_mixed/direct_to_struct` | `N-direct` | 4808 | 10496 | 10497 | -5689 |
| `distinct_values/direct_to_struct` | `N-direct` | 6281 | 11667 | 11668 | -5387 |
| `y_string_unicode/direct_to_struct` | `N-direct` | 4997 | 8679 | 8680 | -3683 |
| `twitter/direct_to_struct` | `N-direct` | 11838 | 15230 | 15231 | -3393 |
| `update_center/direct_to_struct` | `N-direct` | 8495 | 11277 | 11278 | -2783 |
| `github_events/direct_to_struct` | `N-direct` | 12277 | 14835 | 14836 | -2559 |
| `canada/direct_to_struct` | `N-direct` | 10456 | 12096 | 12097 | -1641 |
| `mesh/direct_to_struct` | `N-direct` | 8703 | 9941 | 9942 | -1239 |
| `random/direct_to_struct` | `N-direct` | 7902 | 8996 | 8997 | -1095 |
| `instruments/direct_to_struct` | A/GO below addendum | 12140 | 12442 | 12443 | -303 |
| `numbers/direct_to_struct` | A/GO below addendum | 12325 | 12598 | 12599 | -274 |

Held under the addendum: `apache_builds`, `unicode_basic`, `marine_ik`,
and `citm_catalog`.

Recommendation: dispatch `numbers/direct_to_struct` first. It has the
smallest pinned deficit, is correctness-green, and has a narrow numeric
array dispatch route.

### B - Object/Event Dispatch Rows

`twitter`, `github_events`, and `update_center` share a hot generated
envelope: `parse_object_value_at_direct::<JsonDigestSink>` dominates
fresh direct profiles. The fresh material differential is generated
direct event/FIRST-action lowering, not `JsonDigestSink` source hooks,
decoded string stats, or digest-local hashing. This is a later W11.N
route, with `github_events` the best candidate because the SK-V13
scoping report already names direct dispatch unroll/tail work.

### C - Numeric Rows

Numeric-heavy direct rows are dispatch/envelope-bound:

- `numbers`: 93.3% numeric bytes; rank-1 `parse_array_element_at_direct`
  at ~76.1%; numeric materialization ~15.5%.
- `canada`: 90.1% numeric bytes; rank-1 array dispatch ~85.3%;
  `materialize_f64` ~14.6%.
- `mesh`: 78.8% numeric bytes; array dispatch ~76.7%; numeric
  materializers ~9.9%.

The existing 4-digit UDOT helper is proof-only and unwired. W11.1
should not wire it: touching `bbnf-simd` would inherit W12 zero-orphan
obligations and does not target the dominant leaf. The admissible fresh
route is a generated direct numeric-array consumer that keeps
`NumberSpan` and strict materialization semantics but bypasses generic
value dispatch for dense numeric arrays.

### D - String / Unicode Rows

`unicode_escapes`, `unicode_mixed`, `distinct_values`, and
`y_string_unicode` need W12-class string/SIMD work. `unicode_escapes`
already reaches `unescape_uxxxx_x4_neon`; the remaining route is a
policy-level batched escape decode or 64-byte string-special context
with checkasm and same-row measurement. W11-only string hooks are likely
REDRESS 54/55/66/69 replays.

### E - Gate And Artifact Shape

Each W11.N subwave should use a row-local companion report:

- schema: `sk-v13-json-direct-reopen-v1`;
- flag: `--skv13-json-direct-reopen-report`;
- gate line:
  `G-W11-JSON-DIRECT-REOPEN <status> <row_id> <path>`;
- evidence artifact:
  `restart/skinny/tranches/sk-v13/research/w11.N/artifacts/direct-row-facts.json`;
- REDRESS is always appended;
- `RESULTS.md` and `ROLLING-SOTA-DELTA.md` change only on admission or
  disposition movement.

Minimum owner paths: generated JSON runtime/template direct sink files,
`skinny/crates/bbnf-bench/src/direct_struct.rs`, gate/report/Lock14
plumbing, `xtask`, `REDRESS.md`, and result/delta files only if the row
admits.

### F - SIMD / ASM Audit

There is no source symbol named `output_digest_hash`; the current digest
path is scalar `JsonDirectDigest::{fingerprint,hash_bytes,mix}`. A
`bbnf-simd` route would trigger the strict zero-orphan rule and belongs
in W12 unless kept private to an already-consumed direct path with
scalar reference and checkasm. Therefore W11.1 stays scalar/generated.

## Selected First Subwave

`W11.1 = json/numbers/direct_to_struct/main`.

Fresh material differential:

- generated direct numeric-array dispatch consumer for dense numeric
  arrays;
- no new substrate;
- no JSON source-hook/digest/hash/string/control replay;
- no `bbnf-simd` touch;
- same-wave consumer is `bbnf-bench::direct_struct::track1_digest`
  calling generated `runtime::generated_json::parse_direct`.

Risk:

- The gap is small enough to admit, but the generated dispatch change
  may be optimized away or lost in noise. If so, W11.1 records measured
  REDRESS and routes numeric-array specialization to a broader
  generated-FIRST/action lowering wave.
