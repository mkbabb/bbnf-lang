# SK-V13 W2 Redress - CSS Stylesheet Root + Selectors

Wave: W2. Phase: Redress. Date: 2026-05-21.

Disposition: PASS / ADMIT.

## What Landed

- Generated-style runtime profile
  `runtime::generated_css_l4_stylesheet_selectors::parser::parse`.
- Codegen runtime-profile provider and reproducibility check for
  `css_l4_stylesheet_selectors`.
- W2 fixture, retained Track 1 / golden / lightningcss fact artifacts, and
  `sk-v13-css-stylesheet-selectors-sota-v1` report.
- `gate-json` companion flag
  `--skv13-css-stylesheet-selectors-report`, which rereads Criterion lanes and
  retained equality artifacts.
- Lock 14 owner-path authorization for the W2 generated non-JSON profile.
- `RESULTS.md` grouped W2 row plus feature rows for `stylesheet_root`,
  `selectors`, `pseudo_classes`, `pseudo_elements`, and `attribute_selectors`.
- `ROLLING-SOTA-DELTA.md` moves the five covered CSS feature rows to
  `ADMITTED`.

## Measurement

Criterion group: `nonjson_css_l4_w2`.

| Lane | Mbps |
|---|---:|
| Track 1 generated stylesheet/selectors | 26894.878675 |
| Golden oracle | 3280.361264 |
| lightningcss strict same-plane sidecar | 595.049811 |
| Threshold (`lightningcss + 1`) | 596.049811 |
| Margin | 26298.828864 |

Strict equality: `pass:track1=golden=lightningcss`.

Fact stream SHA-256:
`834ab1ef672836d35446852190bee3509eb3e1019f4aa7f71f4b97bb514df3c3`.

## Gate

`G-W2-CSS-STYLESHEET-SELECTORS`: PASS.

Retained declaration-values CSS row and JSON guards are consumed by the same
`gate-json --check-results` path. W2 does not touch SIMD/ASM and carries
`lock16=n/a:no_simd_or_asm_claim`.

## Routed Remainder

W2 admits the stylesheet/root selector subset only. Remaining CSS L4 rows stay
open for W3/W4/W10 fan-out: declarations, at-rules, nesting, variables,
calc/var/url, visual functions, layout property groups, grid/flexbox, and typed
property groups.
