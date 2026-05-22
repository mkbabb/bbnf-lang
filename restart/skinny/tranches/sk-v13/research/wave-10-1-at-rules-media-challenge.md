# SK-V13 W10.1 CHALLENGE - CSS At-Rules And Media

Wave: W10.1. Phase: CHALLENGE. Date: 2026-05-22.

Disposition: ACCEPT.

## CH1 Correctness

The plan selects a narrow block-level CSS row:
`css_l4/at_rules_and_media/direct_to_struct/main`. It proves exactly two
rolling features: `at_rules_keyframes` and `media_queries`. It does not admit
custom at-rules, vendor prefixes, nested rules, declarations, selectors, or
visual functions.

The fixture is intentionally small and strict:

```css
@media screen and (min-width:1px){a{color:red}}
@keyframes k{from,50%,to{opacity:1}}
```

The row must prove media rule, media query, keyframes rule, keyframe selector
list, and nested body consumption. Declaration/selector details inside the
blocks are body-boundary evidence only.

## CH2 Generality / Lock 14

The plan keeps all at-rule/media semantics inside a CSS-specific generated
profile and runtime module. Generic edits are limited to profile registration,
runtime exports, report/gate plumbing, lock14 owner inventory, and xtask
passthrough. No generic grammar policy, JSON string/number policy, public
substrate API, directive, BIR variant, or `BackendShape` change is allowed.

## CH3 Regression / REDRESS

W10.1 must maintain SK-V12 declaration-values, W2 stylesheet/selectors, W3
declaration-values-extended, W4 visual-functions, and JSON guard rows through
the companion gate invocation. A feature-row admission without the grouped
W10.1 row and retained gate artifacts is a reject.

REDRESS-112, REDRESS-113, and REDRESS-123 through REDRESS-127 remain
`GATE-FEED` history rather than close authority for full CSS parity. REDRESS
28/33, 50-55, 60-72, 82-84, 88/89, and 126 remain blocked if a redress attempt
tries to reuse string, escape, SIMD, substrate, or orphan-kernel routes instead
of the selected generated CSS row.

## CH4 Cost

The row is bounded: one fixture, one generated runtime profile, one report
schema, one retained artifact set, and one Criterion group. Custom at-rules,
vendor prefixes, nested rules, logical properties, grid, flexbox, and typed
property groups remain W10 subwaves because folding them in would make the
bundle boundary unverifiable and exceed the row's strict parity claim.

## CH5 Hidden Coupling

The grouped row and the two rolling feature rows are coupled by
`covered_feature_rows`. Gate validation must reject stale mappings and must not
silently admit `custom_at_rules` because the fixture has no custom at-rule
taxonomy proof. The row id uses the P3 gate spelling
`at_rules_and_media`; report, runtime profile, and generated module names must
use the same spelling consistently.

## CH6 Anti-Paper-Close

Parse success is insufficient. The lightningcss sidecar must inspect typed
`CssRule::Media` and `CssRule::Keyframes`, reject `CssRule::Unknown`, reject
hidden `MediaCondition::Unknown`, and detect dropped keyframe blocks. The gate
must reread Criterion lanes, byte-compare Track 1/oracle/lightningcss retained
fact streams, verify fact-stream SHA-256, and consume rolling-delta updates in
the same wave.

## Required Redress Checks

- `cargo test -p runtime css_l4_at_rules_and_media`
- `cargo test -p codegen css_l4_at_rules_and_media --lib`
- `cargo test -p bbnf-bench --lib at_rules_and_media`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- W10.1 Criterion capture for the `nonjson_css_l4_w10_1` lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-at-rules-media-report ../restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`
