# AZ-II.cutover.O3a-C1 - CSS Admission, Payloads, and LightningCSS Parity
**Opens after**: AZ-II.cutover.O3a baseline capture and six-agent audit synthesis
**Agents**: up to 10 parallel
**Hard gate**: every CSS parse, color, pseudo, selector, and lightningcss failure has a proved root cause and source owner before O6 claims CSS parity.
**Status**: complete_with_misses

2026-04-29 Round 1 triad complete: research, plan, and redress/probe
artifacts exist under `docs/tranches/AZ-II/audit/O3a-C1-*.md`.
Source redress is routed to CSS admission/payload owners and O6 CSS
truth remains blocked until the focused C1 suites and lightningcss
parity are green.

## Scope

1. Split CSS failures into corpus admission, color payloads,
   pseudo/selector branch payloads, and lightningcss parity lanes.
2. Distinguish grammar admission failures from StructDirect payload
   materialization failures.
3. Create or amend the owning implementation wave before source
   redress lands.
4. Ensure O6 measures CSS only after the source-owned semantic failures
   are green or explicitly blocking.

## Failure Assignment

| Lane | Failed tests |
|---|---|
| Corpus admission | `bbnf::ax_w0a2s_real_css_probe bootstrap_full_parse`; `bbnf::ax_w0a2s_real_css_probe tailwind_full_parse`; `bbnf::css_l4 parse_bootstrap_css` |
| Hex color payloads | `bbnf::css_l4 hex_color_roundtrip_3digit`; `bbnf::css_l4 hex_color_roundtrip_6digit`; `bbnf::css_l4 hex_color_roundtrip_8digit`; `bbnf::css_l4_parity hex_color_3digit_expands_u32`; `bbnf::css_l4_parity hex_color_6digit_materialises_u32`; `bbnf::css_l4_parity hex_color_8digit_alpha_materialises` |
| Named color payloads | `bbnf::css_l4_named_color_parity white_materialises`; `bbnf::css_l4_named_color_parity every_named_color_materialises_its_u32_payload`; `bbnf::css_l4_parity named_color_aliceblue_fires_inline_u32` |
| Pseudo/selector payloads | `bbnf::css_l4_parity dir_pseudo_rtl_branch_fires_payload`; `bbnf::css_l4_parity dir_pseudo_ltr_branch_fires_payload`; `bbnf::css_l4_parity selector_parses_without_payload_loss` |
| LightningCSS parity | `bbnf::lightningcss_parity lightningcss_parity_bootstrap`; `bbnf::lightningcss_parity lightningcss_parity_tailwind` |

## File Bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/O3a-C1-research.md` | create |
| `docs/tranches/AZ-II/audit/O3a-C1-plan.md` | create |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify |
| `grammar/css_l4/*.bbnf` | future redress |
| `crates/core/src/runtime/css_l4/**` | future redress |
| `crates/core/src/backend/rust/emitter/shapes/**` | future redress |
| `crates/core/tests/{css_l4*,lightningcss_parity,ax_w0a2s_real_css_probe}.rs` | future redress |

**Do NOT touch**: JSON, Sheets, BBNF runtime, `Parsed<R>`, or
`crates/tape/**` in C1 unless the plan lane first proves a shared
grammar-general emitter root and amends O3/O4/O5.

## Triumvirate Dispatch

| Lane | Agents | Deliverable |
|---|---:|---|
| Research | 3 | Admission root cause; color payload root cause; pseudo/selector and lightningcss root cause |
| Plan + wave creation | 1 | `O3a-C1-plan.md` plus O6/source-owner amendments |
| Redress | up to 4 | Source commits within the amended owner wave |
| Orchestrator | 1 | Integrate reports and run focused nextest |

## Hard Gate

1. `docs/tranches/AZ-II/audit/O3a-C1-research.md` separates grammar
   admission failures from payload materialization failures.
2. `docs/tranches/AZ-II/audit/O3a-C1-plan.md` assigns each failed CSS
   test to a source owner and O6 verification command.
3. Post-redress `cargo nextest run -p bbnf --test css_l4 --cargo-profile ax-iter -- --nocapture` passes.
4. Post-redress `cargo nextest run -p bbnf --test css_l4_parity --cargo-profile ax-iter -- --nocapture` passes.
5. Post-redress `cargo nextest run -p bbnf --test lightningcss_parity --cargo-profile ax-iter -- --nocapture` passes or blocks O6.

## Dependencies

- **Depends on**: AZ-II.cutover.O3a
- **Blocks**: O6 CSS parity/performance close
