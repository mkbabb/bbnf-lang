# AZ-II.cutover.O3a-C1 Redress Probe

**Agent**: AZ-II O3a-C1 redress/probe
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress`
**Branch**: `codex/azii-o3a-c1-redress`
**Date**: 2026-04-29
**Disposition**: HALT for source redress; READY for plan amendment. Focused CSS failure evidence is reproduced and source ownership is identified, but no source changes landed because C1 redress must wait for the amended plan/source-owner wave.

## Scope Guard

No source, grammar, generated parser, manifest, or test files were edited. The only intended write is this audit document. Cargo commands used an isolated target directory:

```text
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress
```

## Focused Reproductions

All commands were run from `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress`.

### `css_l4`

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress \
  cargo nextest run -p bbnf --test css_l4 --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Summary: build completed under `ax-iter` with existing generated-code warnings; nextest ran 18 tests: 14 passed, 4 failed.

Failing tests:

- `bbnf::css_l4 hex_color_roundtrip_3digit`: `hex #abc must expand to 0xAABBCCFF in a 4-byte KvPair payload`.
- `bbnf::css_l4 hex_color_roundtrip_6digit`: `hex #FF00FF must materialise as a 4-byte KvPair payload`.
- `bbnf::css_l4 hex_color_roundtrip_8digit`: `hex #12345678 must materialise as a 4-byte KvPair payload`.
- `bbnf::css_l4 parse_bootstrap_css`: `bootstrap.css: parse failed or incomplete`.

### `css_l4_parity`

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress \
  cargo nextest run -p bbnf --test css_l4_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Summary: 16 tests run: 9 passed, 7 failed.

Failing tests:

- `dir_pseudo_ltr_branch_fires_payload`: selector list did not contain `:dir(ltr)`.
- `dir_pseudo_rtl_branch_fires_payload`: selector list did not contain `:dir(rtl)`.
- `hex_color_3digit_expands_u32`: no `CssColor::Hex(0xAABBCCFF)` in the typed graph.
- `hex_color_6digit_materialises_u32`: no `CssColor::Hex(0xFF00FFFF)` in the typed graph.
- `hex_color_8digit_alpha_materialises`: no `CssColor::Hex(0x12345678)` in the typed graph.
- `named_color_aliceblue_fires_inline_u32`: named color produced no `CssColor::Hex` payloads; `got: []`.
- `selector_parses_without_payload_loss`: parse failed with `Syntax { offset: 0, rule: None }`.

Passing controls included color-space branch parsing, global keyword branch parsing, percentage materialization, media query parsing, keyframes parsing, and realistic typed leaf materialization. This narrows the failures to color scalar payloads and selector/pseudo structural capture, not a total CSS parser outage.

### `css_l4_named_color_parity`

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress \
  cargo nextest run -p bbnf --test css_l4_named_color_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Summary: 3 tests run: 1 passed, 2 failed.

Failing tests:

- `every_named_color_materialises_its_u32_payload`: `150/150 named colors failed payload parity`; preview begins `aliceblue: expected 0xF0F8FFFF, got None`.
- `white_materialises`: `left: None`, `right: Some(4294967295)`.

The grammar list test passed, so the failure is not missing grammar declarations.

### `ax_w0a2s_real_css_probe`

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress \
  cargo nextest run -p bbnf --test ax_w0a2s_real_css_probe --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Summary: 3 tests run: 1 passed, 2 failed.

Failing tests:

- `bootstrap_full_parse`: `bootstrap.css (280311 bytes) failed: Syntax { offset: 9317, rule: None }`.
- `tailwind_full_parse`: `tailwind.css (3642321 bytes) failed: Syntax { offset: 120685, rule: None }`.

Passing control:

- `normalize_full_parse`: `normalize.css: 6138 bytes -> rules=68 decls=114`.

Offset probes:

- `bootstrap.css` offset `9317` is line 390, column 1, at a declaration boundary in the `kbd` rule: `font-size: 0.875em;` followed by `color:`.
- `tailwind.css` offset `120685` is line 10749, column 1, at a declaration boundary inside `.space-x-0 > :not([hidden]) ~ :not([hidden])`.

The panic text still blames `skip_space`, but these offsets are not block-comment openings. Treat the admission lane as declaration/selector admission until a source probe proves otherwise.

### `lightningcss_parity`

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-c1-redress/target-azii-o3a-c1-redress \
  cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Summary: 4 tests run: 2 passed, 2 failed.

Failing tests:

- `lightningcss_parity_bootstrap`: `bootstrap.css: bbnf parse failed: Syntax { offset: 9317, rule: None }`.
- `lightningcss_parity_tailwind`: `tailwind.css: bbnf parse failed: Syntax { offset: 120685, rule: None }`.

Passing controls:

- `lightningcss_parity_normalize`: `bbnf rules+decls = 182 (rules=68, decls=114); lightningcss top-level rules = 34`.
- `color_channel_parity_rgb_family`: passed with diagnostic `bbnf_colors empty ... bbnf admitted 0 LargeAggregate records`, preserving an existing pending aggregate note.

## Root-Cause Separation

### Hex payloads

`grammar/css/l4/color.bbnf:189-190` declares:

```text
hex = "#" , /[0-9a-fA-F]{3,8}/
    -> crate::css_types::parse_hex_color(input) : u32 ;
```

The generated struct-direct parser does not call the host function. In `crates/core/src/grammar/generated/css_l4.rs:12838-12890`, `parse_flat_CssL4Parser_hex` matches `"#"` and scans `[0-9a-fA-F]{3,8}`, then returns `Ok(())` without `parse_hex_color(...)` or `push_leaf_with_u64(...)`.

Likely owner: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`. Its struct-direct `IrNode::Map` arm unwraps to the inner node, so `Map { Regex, HexConvert }` is erased before payload emission. The tape-side collector already preserves `Map { inner: Regex }` in `crates/core/src/backend/rust/emitter/shapes/flat/mod.rs:319-333`; struct-direct needs the same preservation and a host-fn payload emitter.

### Named color payloads

`grammar/css/l4/color.bbnf:36-185` declares 150 `namedColor` branches with `-> 0xRRGGBBAAu32`. The generated parser drops every payload. In `crates/core/src/grammar/generated/css_l4.rs:4894-4940`, matched branches call `builder.push_leaf_with_unit()` plus `push_branch_tag(...)`; no branch emits the declared `u32`.

Likely owner: `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`, especially `struct_direct_leaf_emit_for`. Current behavior treats `TypeDesc::U8` as unit and all other typed keyword cases as unit fallback; it has no `TypeDesc::U32` path. `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs` already extracts integer literal payloads as token streams, so the missing step is type-directed struct-direct emission.

### `:dir(...)` pseudo payload / selector graph loss

`grammar/css/l4/selectors.bbnf:67-68` declares `dirKeyword = "ltr" -> 0u8 | "rtl" -> 1u8` and `dirPseudo = ":dir" , "(" >> dirKeyword << ")"`.

The generated keyword parser at `crates/core/src/grammar/generated/css_l4.rs:26684-26740` calls `builder.push_leaf_with_unit()` for both `ltr` and `rtl`; `parse_arglist_CssL4Parser_dirPseudo` at `crates/core/src/grammar/generated/css_l4.rs:29091-29160` consumes `:dir(...)` but never pushes the full pseudo selector text. The runtime builder only appends selector strings from `push_leaf_with_str` while inside `OpenFrame::SelectorList` (`crates/core/src/runtime/css_l4/builder.rs:714-738`), so the typed selector graph cannot contain `:dir(ltr)` or `:dir(rtl)`.

Likely owners:

- `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` for typed keyword discriminant preservation.
- `crates/core/src/backend/rust/emitter/shapes/arglist.rs`, `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`, and `crates/core/src/backend/rust/emitter/shapes/unordered.rs` for selector-span capture through struct-direct selector rules.
- `crates/core/src/runtime/css_l4/builder.rs` if the chosen plan adds span-aware selector compound handling to `begin_compound` / `end_compound` instead of emitting explicit `push_leaf_with_str` at selector-shape close.

### Corpus admission / LightningCSS parity

The real-corpus failures share the same offsets across `ax_w0a2s_real_css_probe` and `lightningcss_parity`, proving LightningCSS parity fails because bbnf rejects bootstrap/tailwind first. `normalize.css` passes in both probes.

Likely owners:

- `grammar/css/l4/properties.bbnf` for declaration admission, especially typed vs generic declaration dispatch around `color:` and custom-property declarations.
- `grammar/css/l4/selectors.bbnf` plus `crates/core/src/backend/rust/emitter/shapes/unordered.rs` for selector admission where Tailwind uses escaped class names and nested pseudo selectors.
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs` is less likely for these two reproduced offsets. The generated `skip_space` is comment-aware in `crates/core/src/grammar/generated/css_l4.rs:4478-4565`, and neither failing offset is a comment opening.

## Proposed Source Diff Sketch

This is a sketch only; no source redress was applied.

1. Preserve and emit struct-direct map-regex payloads.
   - In `flat/struct_direct.rs`, mirror the tape path's `Map { inner: Regex }` preservation instead of unwrapping all maps.
   - Add a struct-direct equivalent of `flat/map_regex_host.rs` or reuse its descriptor logic to emit:
     `let __decoded_u32 = <host_fn>(matched_str); StructBuilder::push_leaf_with_u64(builder, __decoded_u32 as u64);`
   - Regenerate CSS and verify `parse_flat_CssL4Parser_hex` contains the host call and `push_leaf_with_u64`.

2. Add type-directed keyword payload emission.
   - In `keyword/struct_direct.rs`, extend `struct_direct_leaf_emit_for` so `TypeDesc::U32` plus a branch payload emits `push_leaf_with_u64`.
   - Keep JSON `null` semantics intact; do not globally reinterpret every `U8` keyword as a color-like `u64`.
   - For CSS/Sheets `U8` discriminants, route through a semantic tag path (`push_branch_tag` or a new typed-discriminant builder hook) rather than `CssStructBuilder::push_leaf_with_u64`, because that method currently means packed CSS color.

3. Preserve selector structural spans.
   - Pick one plan-owned approach:
     - Emit `push_leaf_with_str(&input[span_lo..span_hi])` when selector structural rules like `dirPseudo`, `simplePseudoClass`, `classSelector`, `idSelector`, and `compoundSelector` close successfully, or
     - Extend `CssStructBuilder` with span-aware compound handles so `end_compound` can synthesize `Selector::Span(...)` for selector rules.
   - Ensure the resulting typed graph keeps full pseudo strings such as `:dir(rtl)`, not only inner `rtl`.

4. Add focused admission probes before broad corpus reruns.
   - Minimal bootstrap slice around line 390: `kbd { font-size: 0.875em; color: var(--bs-code-color); }`.
   - Minimal tailwind slice around line 10749: `.space-x-0 > :not([hidden]) ~ :not([hidden]) { --tw-space-x-reverse: 0; margin-right: calc(0px * var(--tw-space-x-reverse)); }`.
   - Use those to decide whether the owner is declaration dispatch, selector grammar, or both.

## Verification Commands for Amended Redress

After source redress and regeneration, rerun:

```bash
CARGO_TARGET_DIR=<unique-or-shared-per-wave-target> cargo nextest run -p bbnf --test css_l4 --cargo-profile ax-iter --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique-or-shared-per-wave-target> cargo nextest run -p bbnf --test css_l4_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique-or-shared-per-wave-target> cargo nextest run -p bbnf --test css_l4_named_color_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique-or-shared-per-wave-target> cargo nextest run -p bbnf --test ax_w0a2s_real_css_probe --cargo-profile ax-iter --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique-or-shared-per-wave-target> cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Hard gate for C1 redress should require the three payload binaries to pass and require bootstrap/tailwind corpus failures either to pass or to block O6 with the exact minimal admission owner documented.
