# AZ-II.cutover.O3a-C1 Plan - CSS Admission, Payloads, and LightningCSS Parity

**Agent**: AZ-II O3a-C1 plan
**Date**: 2026-04-29
**Scope**: plan only; no source edits and no direct `O6.md` edit
**Input evidence**:
`docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`,
`docs/tranches/AZ-II/waves/cutover/O3a-C1.md`, and focused C1 rerun
below.

## Focused Evidence

Command run in this worktree:

```bash
cargo nextest run -p bbnf \
  --test css_l4 \
  --test css_l4_parity \
  --test css_l4_named_color_parity \
  --test lightningcss_parity \
  --test ax_w0a2s_real_css_probe \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture
```

Result: 44 tests run, 27 passed, 17 failed.

Important failure facts:

- `bootstrap.css` fails at offset `9317`; `tailwind.css` fails at
  offset `120685`. Both failures report that the shape-dispatcher
  support module's `skip_space` did not consume a leading or inter-rule
  block comment.
- All hex payload tests parse but find no `CssColor::Hex(...)`.
  Generated `css_l4.rs` has no `parse_hex_color` call; the current
  StructDirect path has unwrapped the `Map { Regex, HexConvert }`
  instead of calling the host function.
- `namedColor` generates `builder.push_leaf_with_unit()` plus
  `push_branch_tag(...)`; all 150 named-color payloads report `None`.
- `dirPseudo` parses but the typed selector graph does not contain
  `:dir(ltr)` or `:dir(rtl)`.
- `.foo#bar > baz.qux:hover { color: red; }` fails at offset `0`,
  pointing at selector admission before typed graph inspection.
- `lightningcss_parity_bootstrap` and
  `lightningcss_parity_tailwind` fail because the bbnf CSS parser
  rejects the same corpus inputs; `normalize.css` is green.

## Root-Cause Lanes

### Lane 1 - Corpus Admission

Primary owner: **grammar + emitter support**, with runtime verification.

Source owners:

- Grammar owner: `grammar/css/l4/stylesheet.bbnf` and adjacent
  `grammar/css/l4/*.bbnf` admission rules.
- Emitter owner:
  `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`
  and any shape call sites that bypass comment-aware `skip_space`.
- Runtime owner: `crates/core/src/runtime/css_l4/builder.rs` only if
  parse succeeds but commits an empty or malformed document.
- Test owner:
  `crates/core/tests/ax_w0a2s_real_css_probe.rs`,
  `crates/core/tests/css_l4.rs`,
  `crates/core/tests/lightningcss_parity.rs`.

Patch intent: prove the CSS `@ws` directive still classifies as
`WhitespaceWithBlockComment`, then make every StructDirect shape path
that consumes inter-rule or leading trivia call the comment-aware
support helper. Do not patch generated files by hand; fix grammar,
recognizer/support emission, then regenerate.

### Lane 2 - Hex Host-Function Payloads

Primary owner: **emitter**, with runtime consumption.

Source owners:

- Grammar owner: `grammar/css/l4/color.bbnf` for the canonical
  `hex = "#" , /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) : u32`
  contract.
- Emitter owner:
  `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`,
  `crates/core/src/backend/rust/emitter/shapes/hregex.rs`, and
  `crates/core/src/backend/rust/emitter/shapes/flat/map_regex_host.rs`.
- Runtime owner: `crates/core/src/runtime/css_l4/builder.rs`
  `push_leaf_with_u64`.
- Test owner:
  `crates/core/tests/css_l4.rs` and
  `crates/core/tests/css_l4_parity.rs`.

Patch intent: StructDirect must not erase `Map` nodes whose descriptor
is `HexConvert`; it must scan the matched regex, call the host path,
and route the decoded `u32` through `StructBuilder::push_leaf_with_u64`.
If the rule remains Flat-classified, port the host-map emission from
the tape-only helper into Flat StructDirect rather than relying on the
HRegex body that the current CSS `hex` rule does not reach.

### Lane 3 - Named-Color Inline Payloads

Primary owner: **emitter**, with runtime branch-tag consumption.

Source owners:

- Grammar owner: `grammar/css/l4/color.bbnf` `namedColor`.
- Emitter owner:
  `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
  and `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`.
- Runtime owner: `crates/core/src/runtime/css_l4/builder.rs`
  `push_leaf_with_u64` / `push_branch_tag`.
- Test owner:
  `crates/core/tests/css_l4_named_color_parity.rs` and
  `crates/core/tests/css_l4_parity.rs`.

Patch intent: per-branch `MapExpr::IntLit` payloads on
AltDispatch/Keyword literal branches must emit the concrete `u32`
payload, not `push_leaf_with_unit()`. Branch tags are necessary for
layout, but they are not a substitute for the payload. This is the same
source lane as Sheets branch-payload redress only if implementation
proves the helper can be grammar-general without widening C1 file
bounds.

### Lane 4 - Pseudo and Selector Graph Admission

Primary owner: **runtime + emitter**.

Source owners:

- Grammar owner: selector rules under `grammar/css/l4/*.bbnf`.
- Emitter owner:
  `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`,
  `crates/core/src/backend/rust/emitter/shapes/arglist.rs`, and
  `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`.
- Runtime owner:
  `crates/core/src/runtime/css_l4/builder.rs`,
  `crates/core/src/runtime/css_l4/value.rs`, and
  `crates/core/src/runtime/css_l4/document.rs`.
- Test owner: `crates/core/tests/css_l4_parity.rs`.

Patch intent: ensure selector and pseudo-class frames preserve the
source span that reached the parser. For `:dir(...)`, either the
ArgList/Keyword path must push a string span consumed by
`OpenFrame::SelectorList`, or the CSS builder must assemble the
pseudo-class text from the `dirPseudo` compound before the selector
list closes. For complex selectors, inspect grammar admission first
because the failing test rejects at offset `0`.

### Lane 5 - LightningCSS Parity

Primary owner: **O6 verification gate** after lanes 1-4 are green.

Source owners:

- Test owner: `crates/core/tests/lightningcss_parity.rs`.
- Runtime/grammar/emitter owners: inherited from lanes 1-4.

Patch intent: do not relax parity assertions. `lightningcss_parity`
currently fails because bbnf rejects `bootstrap.css` and
`tailwind.css`; O6 may not claim CSS parity while these two tests fail.

## Failure Assignment Matrix

| Failed test | Primary owner | Grammar owner | Runtime owner | Emitter owner | Test owner | O6 verification |
|---|---|---|---|---|---|---|
| `bbnf::ax_w0a2s_real_css_probe bootstrap_full_parse` | grammar/emitter admission | `grammar/css/l4/stylesheet.bbnf` | `runtime/css_l4/builder.rs` if parsed graph is empty | `shapes/dispatcher/support.rs`, shape skip-space callers | `ax_w0a2s_real_css_probe.rs` | `cargo nextest run -p bbnf --test ax_w0a2s_real_css_probe --cargo-profile ax-iter -- --nocapture`; then `lightningcss_parity` |
| `bbnf::ax_w0a2s_real_css_probe tailwind_full_parse` | grammar/emitter admission | `grammar/css/l4/stylesheet.bbnf` | `runtime/css_l4/builder.rs` if parsed graph is empty | `shapes/dispatcher/support.rs`, shape skip-space callers | `ax_w0a2s_real_css_probe.rs` | same as above |
| `bbnf::css_l4 parse_bootstrap_css` | grammar/emitter admission | `grammar/css/l4/stylesheet.bbnf` | `runtime/css_l4/builder.rs` if parsed graph is empty | `shapes/dispatcher/support.rs`, shape skip-space callers | `css_l4.rs` | `cargo nextest run -p bbnf --test css_l4 --cargo-profile ax-iter -- --nocapture` |
| `bbnf::css_l4 hex_color_roundtrip_3digit` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4.rs` | `cargo nextest run -p bbnf --test css_l4 --cargo-profile ax-iter -- --nocapture` |
| `bbnf::css_l4 hex_color_roundtrip_6digit` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4.rs` | same as above |
| `bbnf::css_l4 hex_color_roundtrip_8digit` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4.rs` | same as above |
| `bbnf::css_l4_parity hex_color_3digit_expands_u32` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4_parity.rs` | `cargo nextest run -p bbnf --test css_l4_parity --cargo-profile ax-iter -- --nocapture` |
| `bbnf::css_l4_parity hex_color_6digit_materialises_u32` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4_parity.rs` | same as above |
| `bbnf::css_l4_parity hex_color_8digit_alpha_materialises` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `flat/struct_direct.rs`, `hregex.rs`, `flat/map_regex_host.rs` | `css_l4_parity.rs` | same as above |
| `bbnf::css_l4_named_color_parity white_materialises` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `alt_dispatch/branches.rs`, `keyword/struct_direct.rs` | `css_l4_named_color_parity.rs` | `cargo nextest run -p bbnf --test css_l4_named_color_parity --cargo-profile ax-iter -- --nocapture` |
| `bbnf::css_l4_named_color_parity every_named_color_materialises_its_u32_payload` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `alt_dispatch/branches.rs`, `keyword/struct_direct.rs` | `css_l4_named_color_parity.rs` | same as above |
| `bbnf::css_l4_parity named_color_aliceblue_fires_inline_u32` | emitter payload | `grammar/css/l4/color.bbnf` | `runtime/css_l4/builder.rs::push_leaf_with_u64` | `alt_dispatch/branches.rs`, `keyword/struct_direct.rs` | `css_l4_parity.rs` | `css_l4_parity` plus `css_l4_named_color_parity` |
| `bbnf::css_l4_parity dir_pseudo_rtl_branch_fires_payload` | runtime/emitter selector materialization | selector and pseudo rules under `grammar/css/l4/*.bbnf` | `runtime/css_l4/builder.rs`, `value.rs`, `document.rs` | `keyword/struct_direct.rs`, `arglist.rs`, `flat/struct_direct.rs` | `css_l4_parity.rs` | `cargo nextest run -p bbnf --test css_l4_parity --cargo-profile ax-iter -- --nocapture` |
| `bbnf::css_l4_parity dir_pseudo_ltr_branch_fires_payload` | runtime/emitter selector materialization | selector and pseudo rules under `grammar/css/l4/*.bbnf` | `runtime/css_l4/builder.rs`, `value.rs`, `document.rs` | `keyword/struct_direct.rs`, `arglist.rs`, `flat/struct_direct.rs` | `css_l4_parity.rs` | same as above |
| `bbnf::css_l4_parity selector_parses_without_payload_loss` | grammar/runtime selector admission | selector rules under `grammar/css/l4/*.bbnf` | `runtime/css_l4/builder.rs`, `value.rs`, `document.rs` | `flat/struct_direct.rs`, selector shape calls | `css_l4_parity.rs` | same as above |
| `bbnf::lightningcss_parity lightningcss_parity_bootstrap` | O6 parity gate after admission | inherited from admission lane | inherited from admission lane | inherited from admission lane | `lightningcss_parity.rs` | `cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter -- --nocapture`; blocks O6 if red |
| `bbnf::lightningcss_parity lightningcss_parity_tailwind` | O6 parity gate after admission | inherited from admission lane | inherited from admission lane | inherited from admission lane | `lightningcss_parity.rs` | same as above; blocks O6 if red |

## Post-Redress Verification Commands

Run in this order after source redress and grammar regen:

```bash
cargo xtask regen --grammar css_l4 --check

cargo nextest run -p bbnf \
  --test ax_w0a2s_real_css_probe \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture

cargo nextest run -p bbnf \
  --test css_l4 \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture

cargo nextest run -p bbnf \
  --test css_l4_parity \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture

cargo nextest run -p bbnf \
  --test css_l4_named_color_parity \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture

cargo nextest run -p bbnf \
  --test lightningcss_parity \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture
```

Close command once the focused lanes are green:

```bash
cargo nextest run -p bbnf \
  --cargo-profile ax-iter \
  --no-fail-fast \
  -E 'test(/css_l4/) or test(/lightningcss/) or test(/ax_w0a2s_real_css_probe/)'
```

Then O6 may run its CSS lane:

```bash
make az-ii-bench-close WAVE=O6
```

If the Makefile still exposes only `make ay-bench-close`, the
orchestrator must first reconcile the O6 command-surface naming before
recording CSS performance truth.

## O6 Amendment Text for Orchestrator

Do not apply this plan agent. The orchestrator should patch
`docs/tranches/AZ-II/waves/cutover/O6.md` exactly in the sections
below after accepting C1.

### Amendment 1 - Scope

Patch intent: append to the O6 scope list:

```markdown
9. Treat O3a-C1 as a hard prerequisite for CSS parity: bootstrap.css
   and tailwind.css admission, hex/named-color payload materialization,
   pseudo/selector typed graph materialization, and lightningcss parity
   must be green before O6 records CSS semantic or performance truth.
```

### Amendment 2 - O6.2 CSS LightningCSS Parity

Patch intent: replace the O6.2 mechanism and sub-gate with:

```markdown
Mechanism: consume `docs/tranches/AZ-II/audit/O3a-C1-plan.md` and the
C1 redress commits before parity measurement. First run
`ax_w0a2s_real_css_probe`, `css_l4`, `css_l4_parity`, and
`css_l4_named_color_parity`; only then run `lightningcss_parity`.
Repair test harnesses only when the parser/runtime are already green
and the harness assertion no longer matches the typed document surface.

Sub-gate:
`cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
passes after the C1 focused suites pass. Any failure blocks O6 CSS
parity and O7; O6 may not publish CSS throughput or semantic parity
with `lightningcss_parity_bootstrap` or `lightningcss_parity_tailwind`
red.
```

### Amendment 3 - O6.6 CSS Bench Lane

Patch intent: prepend this blocker to O6.6:

```markdown
Blocker: do not run or archive CSS close-matrix numbers until
`lightningcss_parity_bootstrap` and `lightningcss_parity_tailwind` are
green on the post-O5 tree. If either is red, O6 records CSS as blocked
by O3a-C1 and does not write placeholder, stale, `PARSE_FAILED`, or
partial CSS entries into `post-AZ-II.json`.
```

### Amendment 4 - O6.12 O3a Cohort Close Matrix

Patch intent: add the C1-specific bullets:

```markdown
- C1 admission gate:
  `cargo nextest run -p bbnf --test ax_w0a2s_real_css_probe --cargo-profile ax-iter --no-fail-fast -- --nocapture`
  passes.
- C1 typed-payload gate:
  `cargo nextest run -p bbnf --test css_l4 --test css_l4_parity --test css_l4_named_color_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
  passes.
- C1 lightningcss gate:
  `cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
  passes. If not green, O6 is blocked from claiming CSS parity or CSS
  close-matrix throughput.
```

### Amendment 5 - Hard Gate

Patch intent: strengthen O6 hard gate 2:

```markdown
2. `cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
   passes after O3a-C1 focused admission and typed-payload suites are
   green. `lightningcss_parity_bootstrap` and
   `lightningcss_parity_tailwind` are hard blockers; O6 cannot claim
   CSS parity or publish CSS performance truth while either remains red.
```

## Redress Sequencing

1. Fix corpus admission first. It unblocks lightningcss parity and CSS
   bench admission.
2. Fix hex host-function payloads and named-color inline payloads in
   the emitter before touching parity tests. The tests currently assert
   real typed document payloads and should stay strict.
3. Fix pseudo/selector materialization after selector grammar
   admission is known green.
4. Run `lightningcss_parity` only after lanes 1-4 are green.
5. Block O6 on lightningcss parity if either bootstrap or tailwind is
   still red.
