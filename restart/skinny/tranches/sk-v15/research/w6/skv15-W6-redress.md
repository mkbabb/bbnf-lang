# SK-V15 Wave W6 Redress: Typed CSS Same-Workload Retime

Status: ROUTE-W6-REJECT.

W6 does not admit CSS L4. It retires W8R, `CSS_GENERATED_RS`, fact-stream,
`CssFullParseSummary`, `parse_full`, brace-counter, and `lightningcss` evidence
from live CSS admission, then records a fresh typed rejection.

## Evidence

- Implementation commit: `cec47b56e`.
- Report artifact:
  `restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json`.
- Report SHA-256:
  `31439e588849f557abf79e84ce35bf371e89c5b1c7467b01b5a271c88b0ba37e`.
- Command:
  `SKV15_W6_REPORT_OUT=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf --test css_l4_w6_typed_retime --release -- --nocapture`.
- Result: passed `2` tests.

## Measurement

| field | value |
|---|---:|
| corpus files | 4 |
| corpus bytes | 979638 |
| sample count | 1 |
| Track 1 passes | 2 |
| Track 1 errors | 2 |
| cssparser passes | 4 |
| cssparser errors | 0 |
| Track 1 Mbps | 4.317 |
| cssparser Mbps | 2051.911 |
| threshold Mbps | 2052.911 |
| margin Mbps | -2048.594 |
| admitted rows | 0 |

Typed summary equality is false: Track 1 reports `6231` rules, `19450`
declarations, and `35164` values; cssparser reports `10136` rules, `20043`
declarations, and `99614` values.

## Workload

Track 1: `bbnf::grammar::generated::css_l4::CssL4Parser::parse` producing
`CssDocument`, then `bbnf::runtime::css_l4::visit_document` over typed
document/value nodes.

Comparator: `cssparser::StyleSheetParser` plus `RuleBodyParser`,
`DeclarationParser`, `AtRuleParser`, and `QualifiedRuleParser` callbacks over
the same corpus and command.

The first skinny `bbnf-bench` route was abandoned before commit because a
renamed root `bbnf` dependency creates a Cargo lockfile package collision with
the skinny workspace's separate `bbnf-regex` identity. The root test surface is
the non-shim route because it already owns both the generated typed parser and
the direct `cssparser` comparator dependency.

## Dependency Rows

- `DEP-W6-CSS-GENERATED-RS`: consumed as retired live-proof class.
- `DEP-W6-CSS-SUMMARY-FACT-STREAM`: consumed as retired live-proof class.
- `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`: consumed via root typed provider.
- `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM`: consumed by excluding legacy generated
  skinny CSS runtimes from W6 live admission.
- `DEP-W1-CSS-BROADCAST`: re-attested as diagnostic-only; no 24-row broadcast
  admission is permitted.

## Ledger

`skinny/RESULTS.md` does not move. Existing CSS rows remain
`not_admitted:SK-V15-W0-broadcast-diagnostic` and `AUDIT-FALSIFIED`.

`restart/skinny/ROLLING-SOTA-DELTA.md` records that W8R CSS values are retained
diagnostic full-parse evidence only and that W6's typed same-workload run keeps
CSS `OPEN`.

W7 is unblocked by disposition rather than admission: W6 now has a fresh
measured rejection, and no old proof remains live.
