# AZ-II.cutover.O3a-C1 Research - CSS Admission, Payloads, and LightningCSS

**Agent**: AZ-II O3a-C1 research
**Date**: 2026-04-29
**Scope**: CSS-only root-cause classification for the O3a-C1 failed tests.
**No source edits**: this report is the only file created.

## Readout

The 17 C1 failures split into two different failure planes:

1. **Grammar admission failures**: bootstrap/tailwind full-corpus parses and
   LightningCSS parity fail because `CssL4Parser::parse` fails or returns an
   incomplete parse before semantic comparison can proceed. The O3a failure
   artifact records the failing tests but not the first rejected offset, so the
   exact CSS sub-production is labelled `unknown-admission-subproduction`.
2. **StructDirect payload/projection failures**: hex colors, named colors, and
   pseudo/selector typed graph checks fail after grammar admission because the
   generated StructDirect bodies either do not push the declared payload or the
   `CssStructBuilder` has no projection route for that nested selector/pseudo
   span.

This is not one root cause. Grammar admission must not be repaired by changing
payload projection, and payload projection must not be masked as parser
admission.

## Evidence Base

- C1 failure list: `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt:1`
  through `:15`, plus `:29` and `:42`, records the 17 CSS failures assigned to
  C1; `:170` records the run failure summary.
- C1 scope: `docs/tranches/AZ-II/waves/cutover/O3a-C1.md:14` through `:20`
  requires splitting corpus admission, color payloads, pseudo/selector
  payloads, and LightningCSS parity.
- CSS parser root: generated `CssL4Parser::parse` calls
  `parse_CssL4Parser_stylesheet`, then enforces EOF by returning
  `ParseErr::Syntax` when `pos != input.len()`
  (`crates/core/src/grammar/generated/css_l4.rs:166043`-`:166092`).
- CSS grammar root admits comments as whitespace via `@ws`
  (`grammar/css/l4/stylesheet.bbnf:5`-`:12`) and defines the corpus root as
  `stylesheet = ruleList ?w` (`grammar/css/l4/stylesheet.bbnf:41`-`:44`).
- Hex grammar declares a host-function u32 projection
  (`grammar/css/l4/color.bbnf:187`-`:190`), but the generated
  `parse_flat_CssL4Parser_hex` only consumes `#` and the regex and never calls
  `crate::css_types::parse_hex_color` or any `push_leaf_with_u64`
  (`crates/core/src/grammar/generated/css_l4.rs:12838`-`:12903`).
- Named colors declare literal u32 payloads
  (`grammar/css/l4/color.bbnf:36`-`:185`), but the generated
  `parse_altdispatch_CssL4Parser_namedColor` gives the rule `TypeDesc::Span`
  and emits only `push_leaf_with_unit()` plus `push_branch_tag(idx)`
  (`crates/core/src/grammar/generated/css_l4.rs:4894`-`:4913`,
  `:4920`-`:4939`, `:12787`-`:12808`).
- The CSS builder can materialise packed color payloads when it receives a u64:
  `push_leaf_with_u64` routes `u32::MAX`-bounded values to
  `CssTypedValue::Color(CssColor::Hex(...))`
  (`crates/core/src/runtime/css_l4/builder.rs:688`-`:701`). The generator is
  not calling that method for the failed color rules.
- `CssColor` has separate `Hex(u32)` and `Named { name, packed }` variants
  (`crates/core/src/runtime/css_l4/value.rs:489`-`:515`), but the builder only
  populates `Hex(u32)` for scalar u64 color values. Named color span+packed
  projection is absent.
- Pseudo/selector grammar declares `dirKeyword = "ltr" -> 0u8 | "rtl" -> 1u8`
  and `dirPseudo = ":dir" , "(" >> dirKeyword << ")"` while selectors are
  nested through `pseudoClass`, `compoundSelector`, `complexSelector`, and
  `selectorList` (`grammar/css/l4/selectors.bbnf:66`-`:73`,
  `:86`-`:106`).
- Generated `dirKeyword` consumes `ltr`/`rtl` but pushes only unit, not branch
  tag 0/1 (`crates/core/src/grammar/generated/css_l4.rs:26684`-`:26741`).
  `dirPseudo` opens a compound, delegates to `dirKeyword`, and closes without
  pushing a selector span (`crates/core/src/grammar/generated/css_l4.rs:29091`
  -`:29170`).
- The builder records selector spans only when `push_leaf_with_str` is called
  while the top frame is `OpenFrame::SelectorList`
  (`crates/core/src/runtime/css_l4/builder.rs:713`-`:739`). Nested pseudo
  compounds and Wrap frames deposit generic values instead, and
  `deposit_value` has no `SelectorList` arm
  (`crates/core/src/runtime/css_l4/builder.rs:227`-`:251`).
- LightningCSS parity calls `CssL4Parser::parse` before invoking
  `StyleSheet::parse`, so bootstrap/tailwind parity failures inherit the BBNF
  corpus admission failure unless the BBNF parse succeeds
  (`crates/core/tests/lightningcss_parity.rs:115`-`:143`).

## Root-Cause Classes

### G1 - Grammar Admission Unknown Sub-Production

Full-corpus Bootstrap and Tailwind cases fail at `CssL4Parser::parse` /
`parse_full`. The parser root enforces full consumption, so these are real
admission failures, not empty-document projection failures. The committed
baseline lists the test names and timings only; it does not include the first
`ParseErr::Syntax` offset or the rejected production. Until a focused run
captures offset + surrounding input, the exact grammar sub-production remains
`unknown-admission-subproduction`.

LightningCSS bootstrap/tailwind failures map here too: the parity harness
parses with BBNF first and only reaches LightningCSS after BBNF succeeds.

### P1 - Hex Color Host-Function Payload Not Emitted

The grammar's `hex` rule explicitly declares `-> crate::css_types::parse_hex_color(input) : u32`,
and the runtime builder can route u64/u32 values into `CssColor::Hex`.
Generated StructDirect output for `hex` only consumes the regex and closes the
compound; no host-function call and no `push_leaf_with_u64` are present.

Affected tests:

- `bbnf::css_l4 hex_color_roundtrip_3digit`
- `bbnf::css_l4 hex_color_roundtrip_6digit`
- `bbnf::css_l4 hex_color_roundtrip_8digit`
- `bbnf::css_l4_parity hex_color_3digit_expands_u32`
- `bbnf::css_l4_parity hex_color_6digit_materialises_u32`
- `bbnf::css_l4_parity hex_color_8digit_alpha_materialises`

### P2 - Named Color Literal Payload Replaced By Branch Tags

The grammar declares every named color as a literal mapped to a concrete
`0xRRGGBBAAu32`, but generated StructDirect `namedColor` does not emit those
constants. It emits unit plus branch-tag indices against a `TypeDesc::Span`
layout. That loses both the color name payload and the packed u32. The builder
cannot recover `white = 0xFFFFFFFFu32` or `aliceblue = 0xF0F8FFFFu32` from a
branch ordinal.

Affected tests:

- `bbnf::css_l4_named_color_parity white_materialises`
- `bbnf::css_l4_named_color_parity every_named_color_materialises_its_u32_payload`
- `bbnf::css_l4_parity named_color_aliceblue_fires_inline_u32`

### P3 - Pseudo/Selector Nested Payload Projection Loss

The selector grammar admits pseudo classes, including `:dir(ltr|rtl)`, but the
StructDirect projection does not place the pseudo selector text into the
selector list graph. Two concrete gaps are visible:

- `dirKeyword` literal branches emit unit only, so the declared 0/1 branch
  discriminant is not delivered to the builder.
- `dirPseudo` and other pseudo compounds close through nested frames; selector
  spans reach `SelectorList` only when a string leaf is pushed while the
  topmost frame is exactly `OpenFrame::SelectorList`. Nested pseudo Wrap /
  ArgList frames instead route through `deposit_value`, which has no selector
  collection arm.

Affected tests:

- `bbnf::css_l4_parity dir_pseudo_rtl_branch_fires_payload`
- `bbnf::css_l4_parity dir_pseudo_ltr_branch_fires_payload`
- `bbnf::css_l4_parity selector_parses_without_payload_loss`

`selector_parses_without_payload_loss` is grouped here rather than G1 because
its assertion is a typed-document graph assertion after `CssL4Parser::parse`.
If a focused rerun proves the failure is a parser `ParseErr` rather than an
empty typed graph, move only this test to G1 with the captured offset.

## Per-Test Mapping

| Failed test | Root-cause class | Evidence |
|---|---|---|
| `bbnf::ax_w0a2s_real_css_probe bootstrap_full_parse` | G1 grammar admission, unknown sub-production | Baseline `O3a-test-failures.txt:1`; test unwraps `CssL4Parser::parse` at `crates/core/tests/ax_w0a2s_real_css_probe.rs:22`-`:43`; parser EOF gate at `css_l4.rs:166043`-`:166092`. |
| `bbnf::css_l4 parse_bootstrap_css` | G1 grammar admission, unknown sub-production | Baseline `O3a-test-failures.txt:5`; `parse_full` is `CssL4Parser::parse(input).is_ok()` at `crates/core/tests/css_l4.rs:150`-`:155`. |
| `bbnf::ax_w0a2s_real_css_probe tailwind_full_parse` | G1 grammar admission, unknown sub-production | Baseline `O3a-test-failures.txt:15`; same parse root and EOF gate as bootstrap. |
| `bbnf::lightningcss_parity lightningcss_parity_bootstrap` | G1 grammar admission propagated into parity | Baseline `O3a-test-failures.txt:29`; parity harness BBNF parse precedes LightningCSS parse at `crates/core/tests/lightningcss_parity.rs:115`-`:143`. |
| `bbnf::lightningcss_parity lightningcss_parity_tailwind` | G1 grammar admission propagated into parity | Baseline `O3a-test-failures.txt:42`; same parity harness ordering. |
| `bbnf::css_l4 hex_color_roundtrip_3digit` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:2`; grammar projection at `grammar/css/l4/color.bbnf:187`-`:190`; generated hex body lacks payload push at `css_l4.rs:12838`-`:12903`. |
| `bbnf::css_l4 hex_color_roundtrip_6digit` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:3`; same evidence. |
| `bbnf::css_l4 hex_color_roundtrip_8digit` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:4`; same evidence. |
| `bbnf::css_l4_parity hex_color_3digit_expands_u32` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:9`; same evidence plus builder u64 color route at `crates/core/src/runtime/css_l4/builder.rs:688`-`:701`. |
| `bbnf::css_l4_parity hex_color_6digit_materialises_u32` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:11`; same evidence. |
| `bbnf::css_l4_parity hex_color_8digit_alpha_materialises` | P1 hex host-function payload not emitted | Baseline `O3a-test-failures.txt:12`; same evidence. |
| `bbnf::css_l4_named_color_parity white_materialises` | P2 named color literal payload replaced by branch tag | Baseline `O3a-test-failures.txt:6`; named color grammar constants at `grammar/css/l4/color.bbnf:36`-`:185`; generated namedColor emits unit/tag at `css_l4.rs:4894`-`:4939` and `:12787`-`:12808`. |
| `bbnf::css_l4_named_color_parity every_named_color_materialises_its_u32_payload` | P2 named color literal payload replaced by branch tag | Baseline `O3a-test-failures.txt:10`; same evidence. |
| `bbnf::css_l4_parity named_color_aliceblue_fires_inline_u32` | P2 named color literal payload replaced by branch tag | Baseline `O3a-test-failures.txt:13`; same evidence. |
| `bbnf::css_l4_parity dir_pseudo_rtl_branch_fires_payload` | P3 pseudo/selector nested payload projection loss | Baseline `O3a-test-failures.txt:7`; selector grammar at `grammar/css/l4/selectors.bbnf:66`-`:73`; generated `dirKeyword` emits unit only at `css_l4.rs:26684`-`:26741`; builder selector deposit constraint at `builder.rs:713`-`:739`. |
| `bbnf::css_l4_parity dir_pseudo_ltr_branch_fires_payload` | P3 pseudo/selector nested payload projection loss | Baseline `O3a-test-failures.txt:8`; same evidence. |
| `bbnf::css_l4_parity selector_parses_without_payload_loss` | P3 selector graph projection loss; reclassify to G1 if focused rerun proves parse rejection | Baseline `O3a-test-failures.txt:14`; selector grammar at `selectors.bbnf:86`-`:106`; generated selector frames at `css_l4.rs:34670`-`:34790`, `:35035`-`:35147`, `:35354`-`:35380`; builder selector deposit constraint at `builder.rs:227`-`:251` and `:713`-`:739`. |

## Source-Owner Implications

- P1 belongs to StructDirect emission for Flat/HRegex-host-function mapped
  rules: generated CSS must call the host function and push the u32 payload
  through `StructBuilder::push_leaf_with_u64`.
- P2 belongs to StructDirect AltDispatch mapped-literal emission: branch arms
  with literal `-> <scalar>` maps must push the declared scalar payload, not a
  branch ordinal.
- P3 belongs to selector/pseudo StructDirect projection: selector-family
  compounds need an explicit projection route into `Selector::{PseudoClass,
  PseudoElement, Class, Id, Type, Attribute, Combinator, Span}` rather than
  generic pending `CssTypedValue` fallback.
- G1 needs focused parser diagnostics before redress: rerun only the failing
  corpus parses with first `ParseErr::Syntax` offset and surrounding input. The
  current committed baseline is insufficient to name the rejected grammar
  production.
