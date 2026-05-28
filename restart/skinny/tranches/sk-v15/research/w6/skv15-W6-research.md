# SK-V15 W6 Research: CSS Same-Workload Retime And Old-Proof Retirement

Date: 2026-05-28.
Scope: W6 authority, old-proof inventory, typed comparator route, staging guards.
Output: this file.

## 1 - Findings

W6 is unblocked because W5 is `ADMIT-W5` and the typed provider landed in
`d80702388`. W6 entry is W5 admitted; W7 accepts W6 admitted or routed.

The old live CSS proof is spread across four surfaces:

- `skinny/crates/codegen/src/runtime_generator.rs` emits
  `CSS_GENERATED_RS`, `emit_fact_stream`, `emit_full_parse`,
  `CssFullParseSummary`, and `Result<String, CssFactError>`.
- Seven `skinny/crates/runtime/src/grammars/css_l4_*/` profiles expose
  fact-stream `parse()` and string `parse_full()` APIs.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs` times those string summaries
  against `cssparser` and `lightningcss`.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` preserve the
  W8R broadcast tuple as diagnostic evidence.

The replacement provider is the root typed CSS L4 parser:
`bbnf::grammar::generated::css_l4::CssL4Parser::parse(&str) ->
CssDocument`. W5 added `CssVisitor` and `visit_document`, so W6 can compare
typed document/value traversal rather than a fact-stream string or
four-counter summary.

The near-term comparator is `cssparser`, not `lightningcss`. SPEC binds
`lightningcss` out of live admission until Track 1 emits comparable CSSOM/value
output.

## 2 - Recommendations

Build a W6 typed retime module in `skinny/crates/bbnf-bench` that depends on
the root `bbnf` crate through a renamed path dependency. The module should:

- load the existing CSS L4 production corpus;
- run no warm-up loop;
- time fresh Track 1 `CssL4Parser::parse` runs and fresh `cssparser` runs in
  the same command;
- summarize Track 1 via `CssVisitor` over `CssDocument`;
- summarize `cssparser` through its parser callback API over the same sources;
- admit zero rows unless Track 1 beats same-run `cssparser` on that typed
  workload and the summary equality guard passes.

Update report/gate code only enough to consume W6 typed telemetry. Leave
historical W8R artifacts as diagnostic-negative evidence; do not reuse their
numbers as floors.

## 3 - Risks

Primary risk: the root typed parser may lose to `cssparser`. That is a valid
W6 routed outcome, not a reason to keep the old proof alive.

Secondary risk: current Lock 14 scans do not substitute for explicit W6 leak
scans. W6 must run direct `rg` checks for `CSS_GENERATED_RS`,
`CssFullParseSummary`, `Result<String, CssFactError>`, `emit_fact_stream`,
`emit_full_parse`, `LegacyPath`, and `LegacySegment` over live admission paths.

Staging risk: pre-existing dirty files include prior-tranche CSS JSON reports,
`docs/precepts`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and
seven dirty skinny CSS `generated.rs` files. W6 must stage explicit paths only.

## 4 - Sources

- `restart/skinny/tranches/sk-v15/SPEC.md` Section 9.
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W6.
- `restart/skinny/tranches/sk-v15/research/w5/skv15-W5-redress.md`.
- `skinny/crates/codegen/src/runtime_generator.rs`.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`.
- `crates/core/src/runtime/css_l4/document.rs`.
