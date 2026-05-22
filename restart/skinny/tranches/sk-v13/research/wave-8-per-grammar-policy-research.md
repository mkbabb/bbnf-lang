# SK-V13 W8 Research - Per-Grammar Policy, Sink/View, Flag Surface

Date: 2026-05-21.
Wave: W8, SPEC Section 12.
Scope: generated per-grammar policy ownership for JSON and CSS L4.
Disposition: research cohort converged; plan phase may proceed.

## Cohort

- Pascal (`019e4f0e-cd79-7350-a610-ef87d666251c`) audited generated config
  surfaces and policy-field ownership.
- Lagrange (`019e4f0e-cddb-7071-b9ba-4da7bfb3ce7f`) audited tape storage,
  `OffsetFlags`, and flag cursor semantics.
- Dewey (`019e4f0e-ce4f-7ba3-9594-2866ade12521`) audited JSON generated
  parse/view/sink policy consumers.
- Hooke (`019e4f0e-cebd-7200-b5c7-87a0650c003c`) audited CSS L4 generated row
  consumers and parity-test coverage.
- The local gate scout reconciled SPEC Section 12, DISPATCH-PROMPT W8
  anti-paper-close, REDRESS 121/54/55/66-69/80/82/84, and the W5-W7
  decision-engine report pattern.

## Findings

The legal W8 surface is narrow: expand existing generated `config.rs` modules
with private constants/helpers consumed by adjacent generated parser, sink, and
view code. Do not add a public `GrammarConfig` trait, generic `JsonSink`, public
substrate API, new directive, new BIR variant, or new `BackendShape`.

Tape storage is already grammar-neutral. `Tape`, `TapeBuilder`, sparse
`flag_cursors`/`flag_values`, and `OffsetFlags(pub u8)` can remain unchanged.
The leak is semantic naming in `runtime/src/tape/mod.rs`: public flag constants
are named `HAS_ESC` and `HAS_CONTROL`. JSON currently maps lexical
`StringMatch::needs_decode()` onto bit `0x01` through
`json::config::STRING_NEEDS_DECODE`, and Track 2 mirrors still call
`OffsetFlags::HAS_ESC` directly. W8 should preserve the raw bit value while
moving the meaning into JSON-owned config helpers.

`parse-that-regex::StringFlags` is lexical matcher state, not tape storage.
It has separate `HAS_ESC`, `HAS_CONTROL`, and `NEEDS_DECODE` bits. W8 must not
copy those bit positions into tape; the retained JSON tape fact remains the
grammar-owned "string needs decode" fact stored in physical bit `0x01`.

JSON already partially owns policy in `runtime/src/grammars/json/config.rs` and
`codegen/src/json_templates/config.rs`: structural bytes, string decode flag,
tiny-string caps, and literals live there. Remaining W8 work is to remove the
generic flag-name dependency, add a JSON-owned `string_needs_decode(flags)`
reader, and update the generated view plus Track 2 mirror so valid tape facts
are interpreted only through JSON policy.

The direct JSON sink path is JSON-owned and should stay that way. Generated
`parse_direct` builds `ParsedString { raw, needs_unescape }` and routes through
`JsonSink::*_source` hooks; default source hooks decode before semantic hooks.
That is an admissible W8 consumer seam only if it is tested as JSON-local
policy, not generalized as generic sink acceleration.

CSS L4 policy is still mostly inline in generated scanner/sink bodies. The
best same-wave CSS consumer is
`css_l4/declaration_values_extended/direct_to_struct/main` because it runs the
real generated scanner path rather than a captured constant fixture. The
policy-sensitive sites are whitespace/comment skipping, identifier escape
handling, quote normalization, URL close handling, value recursion, token
normalization, and fact-stream flag labels. Moving those labels/helpers into
the generated CSS `config.rs` module can be byte-identical but still proves
ownership and row consumption.

Several CSS rows have static fast paths and are poor W8 consumers:
`stylesheet_selectors`, `at_rules_and_media`, `vendor_custom`, and
`nested_layout` can return hardcoded fact streams. They are useful guards only
if touched. `declaration_values_extended` is the preferred live scanner row.

## Selected Research Route

Minimum W8 should:

1. Keep generic tape storage stable.
2. Rename or hide generic tape flag meanings so generic roots no longer expose
   JSON escape/control semantics.
3. Add JSON-owned helpers for physical string-decode flags and update generated
   JSON view/template plus Track 2 mirror to consume those helpers or a JSON
   local equivalent.
4. Add CSS declaration-values-extended generated config constants/helpers for
   row metadata, normalized token flags, quote/escape/trivia policy, and value
   recursion where touched.
5. Update CSS runtime and codegen templates together so generated parity stays
   deterministic.
6. Add a W8 gate report that names both row consumers and rejects
   support-only/policy-only closure unless row movement, admission, or measured
   architectural block is recorded.
7. Extend Lock 14 scanning to reject JSON flag names in generic tape roots and
   to authorize only the exact W8 owner paths.

## Same-Wave Consumers

Primary JSON consumer:

- `json/y_string_unicode/direct_to_struct/main`

This row is small, string/unicode-heavy, and exercises
`parse_direct -> ParsedString -> JsonSink::*_source -> JsonDigestSink`.

Primary CSS consumer:

- `css_l4/declaration_values_extended/direct_to_struct/main`

This row runs the generated scanner and strict equality harness against
cssparser/lightningcss. It is the right W8 non-JSON proof because policy
metadata extraction is consumed by executable row code rather than a captured
constant.

Guard rows:

- `json/twitter/direct_to_struct/main` or the existing JSON guard table if gate
  machinery already has table support.
- `css_l4/declaration_values/direct_to_struct/main` as a stable CSS guard.

## Gate Shape

W8 should add a gate-consumed report:

- Schema: `sk-v13-per-grammar-policy-v1`.
- Struct: `SkV13PerGrammarPolicyReport`.
- Flag: `--skv13-per-grammar-policy-report`.
- Gate print: `G-W8-PER-GRAMMAR-POLICY <row_move_toward_sota_status> <path>`.

Required report facts:

- JSON/CSS consumer row ids and same-wave consumer paths.
- Generic storage stability status.
- Public `GrammarConfig` status.
- Generic `JsonSink` acceleration status.
- Generic JSON policy token status.
- JSON flag semantics owner and physical bit preservation.
- CSS generated policy owner and scanner/sink consumer evidence.
- Lock 14 status.
- JSON and CSS guard status.
- Row movement status, block id, material differential, and REDRESS entry.

Accepted `row_move_toward_sota_status` values are `pass`, `admitted`, or
`measured_architectural_block`. Policy relocation with no row movement is
expected to close only as a measured architectural block. The likely block id
is:

`JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT`.

## Material Differential

W8 is materially distinct from REDRESS 121 because it is not a public
`GrammarConfig` or generic JSON policy replay. The selected route keeps
configuration private to generated grammar modules, keeps generic storage
unchanged, pairs JSON with a CSS L4 row, and makes the W8 report/gate consume
the policy ownership proof.

W8 is materially distinct from REDRESS 54/55/66-69, 80, 82, and 84 because it
does not add direct source hooks, decoded string stats/hashes, public
substrate APIs, one-row number patches, scalar-parent folds, or control
compaction replays. If row movement is absent, W8 must record that honestly
rather than treating policy ownership as row admission.

## Validation Inputs

Minimum validation for redress:

- `cargo test -p runtime json::`
- `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`
- `cargo test -p codegen css_l4_declaration_values_extended -- --nocapture`
- `cargo test -p bbnf-bench declaration_values_extended_cssparser_matches_generated_track1 -- --nocapture`
- `cargo test -p bbnf-bench declaration_values_extended_lightningcss_matches_generated_track1_and_cssparser -- --nocapture`
- `cargo test -p bbnf-bench skv13_per_grammar_policy_report -- --nocapture`
- `cargo test -p bbnf-bench --bin gate skv13_per_grammar_policy_report -- --nocapture`
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_per_grammar_policy_report_flag -- --nocapture`

The final advisory `gate-json` run should chain W5/W6/W7 decision evidence plus
the W8 policy report.
