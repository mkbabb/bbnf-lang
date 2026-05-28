# SK-V16 W0 Redress: Report Consumer Baseline

Date: 2026-05-28.
Disposition: ADMIT-W0.

## Scope

W0 implemented only the committed plan owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v16/research/w0/fixtures/*.json`
- this redress artifact

No runtime, codegen, SIMD, Pattern H, CSS provider, or generated output file
was staged for W0.

## What Landed

1. Added typed SK-V16 report schemas with `deny_unknown_fields`:
   CSS typed open, dirty generated inherited manifest, Pattern H roundtrip,
   and native SIMD not-in-scope/scoped-guard reports.
2. Added fail-closed validators:
   CSS stays 0/24 open, dirty generated stays inherited and W1-routed,
   Pattern H requires the exact `-mindepth 2` census and roundtrip proof, and
   native SIMD requires explicit `not_in_scope` in W0.
3. Added bench gate companion consumers for:
   `--skv16-css-typed-report`, `--skv16-dirty-generated-report`,
   `--skv16-pattern-h-roundtrip-report`, and
   `--skv16-native-simd-report`.
4. Added xtask passthrough and arity checks for the same four flags.
5. Added positive W0 fixtures consumed by `cargo xtask gate-json`.

## Guard Notes

`cargo xtask gate-json --check-results` already uses the xtask snapshot guard
and lock-gates-only path as the current JSON 51 guard. When SK-V16 side
reports are supplied, xtask still runs that snapshot validation before
delegating to `bbnf-bench` gate. The bench gate consumes all SK-V16 reports and
prints `G-SK-V16-W0-JSON-GUARD PASS delegated-to-xtask-snapshot`; it does not
enter the older live Criterion metadata path because that path rejects existing
target metadata with `runtime::generated_json::parse has unsupported native W0
capture policy`. W0 does not repair or weaken that legacy live-metadata path.

## Evidence

Passed:

```sh
(cd skinny && cargo test -p xtask skv16 -- --nocapture)
(cd skinny && cargo test -p bbnf-bench --bin gate skv16_ -- --nocapture)
(cd skinny && cargo test -p bbnf-bench --lib skv16_ -- --nocapture)
(cd skinny && cargo xtask gate-json --check-results)
(cd skinny && cargo xtask gate-json --check-results \
  --skv16-css-typed-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-css-typed-open.json \
  --skv16-dirty-generated-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-dirty-generated-inherited.json \
  --skv16-pattern-h-roundtrip-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-pattern-h-roundtrip.json \
  --skv16-native-simd-report ../restart/skinny/tranches/sk-v16/research/w0/fixtures/skv16-native-simd-not-in-scope.json)
```

Invariant checks:

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
# 16

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67
```

No-behavior-drift checks:

```sh
git diff --name-only -- crates/core/src/runtime skinny/crates/runtime/src/grammars
git status --short -- crates/core/src/runtime skinny/crates/runtime/src/grammars
git diff --name-only -- skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/bbnf-bench/src/generated_real_typed.rs
git diff --name-only
```

The protected behavior diffs shown by those commands are inherited entry-state
dirty files only:

- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- seven `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` files

They are explicitly manifested by `skv16-dirty-generated-inherited.json` and
remain routed to W1. W0 did not stage or modify them.

## W0 Exit Gate

- SK-V16 report consumers exist and are gate-consumed.
- Missing, unknown, producer-only, legacy CSS, wrong Pattern H, dirty unrouted,
  and native SIMD scope violations reject through tests.
- CSS remains 0/24 admitted in W0 fixtures.
- Native SIMD is explicit `not_in_scope`.
- Pattern H count remains 67 by the correct `-mindepth 2` census.
- JSON guard remains delegated to existing `cargo xtask gate-json
  --check-results` snapshot/lock-gate path.

## Routed Remainder

- W1 owns dirty generated disposition.
- W2 owns Lock 14/16 scan expansion.
- W3 owns CSS legacy proof quarantine.
- W4-W6 own CSS grammar-derived provider, typed API/equality, and typed SOTA.
- W7-W8 own Pattern H generator-owned roundtrip/collapse.
- W10 owns any scoped native primitive/tape/native consumer.
