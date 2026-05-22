# SK-V13 W10.2 CHALLENGE - CSS Vendor And Custom At-Rules

Wave: W10.2. Phase: CHALLENGE. Date: 2026-05-22.

Disposition: ACCEPT.

## CH1 Correctness

The plan selects a narrow vendor/custom CSS row:
`css_l4/vendor_and_custom_atrules/direct_to_struct/main`. It proves exactly two
rolling features: `vendor_prefixes` and `custom_at_rules`. It does not admit
nested rules, logical properties, grid, flexbox, arbitrary unknown at-rules, or
typed property groups.

The fixture is intentionally small and strict:

```css
@custom-media --narrow (max-width:30em);
@-webkit-keyframes fade{from{opacity:0}to{opacity:1}}
a{-webkit-user-select:none;-moz-user-select:none;user-select:none}
```

The row must prove the custom media rule, custom media condition, vendor
keyframes prefix, keyframes selectors, and vendor-prefixed declaration names.

## CH2 Generality / Lock 14

The plan keeps vendor/custom semantics inside a CSS-specific generated profile
and runtime module. Generic edits are limited to profile registration, runtime
exports, report/gate plumbing, lock14 owner inventory, and xtask passthrough.
No generic grammar policy, JSON string/number policy, public substrate API,
directive, BIR variant, `BackendShape`, SIMD, or x86 change is allowed.

## CH3 Regression / REDRESS

W10.2 must maintain SK-V12 declaration-values, W2 stylesheet/selectors, W3
declaration-values-extended, W4 visual-functions, W10.1 at-rules/media, and
JSON guard rows through the companion gate invocation. A feature-row admission
without the grouped W10.2 row and retained gate artifacts is a reject.

REDRESS-112, REDRESS-113, and REDRESS-123 through REDRESS-127 remain
`GATE-FEED` history rather than close authority for full CSS parity. REDRESS
28/33, 50-55, 60-72, 82-84, 88/89, and 126 remain blocked if redress tries to
reuse string, escape, SIMD, substrate, or orphan-kernel routes instead of the
selected generated CSS row.

## CH4 Cost

The row is bounded: one fixture, one generated runtime profile, one report
schema, one retained artifact set, and one Criterion group. Nested rules,
logical properties, grid, flexbox, and typed property groups remain W10
subwaves because folding them into this taxonomy row would mix unrelated
recursive and typed-layout semantics.

## CH5 Hidden Coupling

The grouped row and the two rolling feature rows are coupled by
`covered_feature_rows`. Gate validation must reject stale mappings and must not
silently admit `nested_rules` or layout rows because the fixture has no nested
rule recursion or grid/flexbox property semantics. The row id uses the P3 gate
spelling `vendor_and_custom_atrules`; report, runtime profile, and generated
module names must use the same spelling consistently.

## CH6 Anti-Paper-Close

Parse success is insufficient. The lightningcss sidecar must inspect typed
`CssRule::CustomMedia`, vendor-prefixed `CssRule::Keyframes`, and declaration
names. The gate must reread Criterion lanes, byte-compare
Track 1/oracle/lightningcss retained fact streams, verify fact-stream SHA-256,
and consume rolling-delta updates in the same wave. `CssRule::Unknown` is not
accepted as proof of arbitrary custom at-rule parity in this subwave.

## Required Redress Checks

- `cargo test -p runtime css_l4_vendor_and_custom_atrules`
- `cargo test -p codegen css_l4_vendor_and_custom_atrules --lib`
- `cargo test -p bbnf-bench --lib vendor_custom`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- W10.2 Criterion capture for the `nonjson_css_l4_w10_2` lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-vendor-custom-report ../restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`
