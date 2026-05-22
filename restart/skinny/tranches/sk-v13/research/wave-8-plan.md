# SK-V13 W8 Plan - Per-Grammar Policy, Sink/View, Flag Surface

Cycle: W8 Plan. Scope: SPEC Section 12.

## Selected Intervention

Move the first concrete policy meanings out of generic tape roots and into
generated grammar-local config surfaces, then prove the new surface is consumed
by one JSON row and one CSS L4 row in the same wave.

W8 is a legality and consumption wave, not a support-only cleanup. It closes by
one of two routes:

1. ADMIT if the policy relocation moves or admits at least one named JSON/CSS
   row by P3-C `row_move_toward_sota` while preserving strict equality.
2. Otherwise close as the measured architectural block
   `JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT`, with
   JSON/CSS row consumers, Lock 14 proof, and gate-consumed evidence that the
   executable rows consumed the generated policy but throughput did not move.

Policy wiring alone is not enough. `JSON output unchanged`, byte-identical CSS
fact streams, and private config constants are guard evidence only unless the
W8 report records row movement, admission, or the named measured block.

## Owner Paths

Primary SPEC owner paths:

- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_*`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/parse-that-regex/`
- `skinny/crates/bbnf-bench/`

Selected implementation owner paths:

- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/grammars/json/config.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs` only if tests require
  a call-site helper rename; the writer path already calls
  `config::needs_decode_flags()`.
- `skinny/crates/codegen/src/json_templates/config.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs` only if the runtime
  generated writer changes.
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/config.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/generated.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/sink.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v13/research/w8/`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only if a row
  moves or admits.

`parse-that-regex` is an owner path but is not selected for redress unless the
implementation proves a scalar policy API is needed. Leaving
`parse-that-regex::StringFlags` untouched is preferred because those flags are
lexical matcher state, not tape facts.

Lock 14 redress must add `SK_V13_W8_OWNER_PATHS` and parent-diff matchers for
`sk-v13-waveW8` and `sk-v13-wave8-challenge`. The owner set must cover every
generic root touched by W8. Adding a touched generic root without scan coverage
is a CH2 fail.

## Implementation Shape

1. Keep physical tape storage stable: no field changes to `Tape`,
   `TapeBuilder`, `ValueRef`, `PayloadArena`, `flag_cursors`, or
   `flag_values`.
2. Rename generic `OffsetFlags` bit constants from JSON-semantic names to
   neutral storage names, preserving raw values:
   - bit `0x01` remains bit `0x01`.
   - bit `0x02` remains bit `0x02`.
   - no existing parity fixture changes raw flag bytes.
3. In JSON generated config/runtime and template config, define grammar-owned
   physical meanings:
   - `STRING_NEEDS_DECODE: u8 = <neutral bit 0>`
   - `needs_decode_flags() -> OffsetFlags`
   - `string_needs_decode(flags: OffsetFlags) -> bool`
4. Update JSON view/template reads to call `config::string_needs_decode`.
5. Update Track 2 JSON mirror to avoid `OffsetFlags::HAS_ESC`; it may either
   import a JSON-owned helper if visibility allows or use a local Track 2 JSON
   constant that documents the same physical bit without generic JSON naming.
6. In CSS declaration-values-extended generated config/runtime and template
   config, define private constants/helpers for touched policy:
   - row/schema/plane metadata stays in config.
   - normalized token flag label.
   - max value recursion.
   - quote bytes / escape byte / hex escape width where generated scanner
     consumers are updated.
   - trivia bytes/helpers where generated scanner consumers are updated.
7. Update CSS generated scanner and sink/template consumers to use the config
   helpers without changing the fact-stream semantics.
8. Add the W8 report, gate validator, and xtask passthrough.
9. Add Lock 14 forbidden generic tokens for JSON escape/control flag names in
   generic roots and make W8 owner-path authorization exact.
10. Add REDRESS-139 and W8 retained evidence artifacts.

The default redress target is expected to close as the named measured block.
Stable row movement from ownership relocation alone is unlikely; if movement is
not present, W8 must prove executable consumption and record the block.

## Falsifiability Gate

Primary gate: `G-W8-PER-GRAMMAR-POLICY`.

Pass conditions:

1. Report schema is `sk-v13-per-grammar-policy-v1`.
2. `wave_id = SK-V13-W8`.
3. `consumer_gate = G-W8-PER-GRAMMAR-POLICY`.
4. `json_consumer_row_id = json/y_string_unicode/direct_to_struct/main`.
5. `css_consumer_row_id =
   css_l4/declaration_values_extended/direct_to_struct/main`.
6. `generic_storage_status = stable`.
7. `public_grammar_config_status = absent`.
8. `generic_json_sink_acceleration_status = absent`.
9. `generic_json_policy_token_status = absent`.
10. `json_flag_semantics_owner = generated_json_config`.
11. `json_flag_physical_bit_status = preserved`.
12. `css_policy_owner = generated_css_config`.
13. `css_policy_consumer_status = generated_scanner_and_sink`.
14. JSON and CSS strict equality statuses pass.
15. Lock 14 status passes.
16. `row_move_toward_sota_status` is one of `pass`, `admitted`, or
    `measured_architectural_block`.
17. If status is `pass` or `admitted`, row measurement fields must include
    current Mbps, prior Mbps, SOTA Mbps where relevant, and strict comparator
    equality.
18. If status is `measured_architectural_block`, `block_id` must be
    `JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT` and the
    report must include material differential text.
19. W5/W6/W7 evidence chain remains pass/blocked without regression when the
    W8 advisory gate is run.

Reject states:

- Public `GrammarConfig`.
- Generic `JsonSink` acceleration.
- Generic tape roots still exporting JSON-named escape/control flag constants.
- JSON quote/backslash/control constants in generic roots.
- CSS row chosen from a captured static fast path instead of the live
  declaration-values-extended scanner.
- Status-only Lock 14 claim.
- Policy-only, support-only, scaffold-only, or future-consumer closure.
- Raw flag byte drift without explicit parity fixture regeneration and gate
  acceptance.
- JSON/CSS strict equality regression.

## Report Shape

Report schema: `sk-v13-per-grammar-policy-v1`.

Required fields:

- Provenance: `schema_version`, `wave_id`, `run_id`, `source_commit`,
  `host_triple`, `build_flags`, `feature_mask`, `consumer_gate`,
  `g_omega_status`.
- Consumers: `json_consumer_row_id`, `json_consumer_path`,
  `css_consumer_row_id`, `css_consumer_path`, `same_wave_consumer_class`.
- Storage/policy: `generic_storage_status`, `public_grammar_config_status`,
  `generic_json_sink_acceleration_status`, `generic_json_policy_token_status`,
  `json_flag_semantics_owner`, `json_flag_physical_bit_status`,
  `css_policy_owner`, `css_policy_consumer_status`.
- Equality/guards: `json_strict_equality_status`,
  `css_strict_equality_status`, `json_guard_state`, `css_guard_state`.
- Measurements: `json_row_mbps_before`, `json_row_mbps_after`,
  `css_row_mbps_before`, `css_row_mbps_after`, `row_move_toward_sota_status`.
- Lock 14: `lock14_status`, `lock14_owner_path_status`,
  `lock14_generic_scan_status`.
- Evidence: `policy_artifact_path`, `policy_artifact_sha256`.
- Disposition: `affected_row_ids`, `block_id`, `material_differential`,
  `redress_entry`.

Gate flag: `--skv13-per-grammar-policy-report`.

Gate print:

`G-W8-PER-GRAMMAR-POLICY <row_move_toward_sota_status> <path>`.

## Measurement Rows

Primary JSON row:

- `json/y_string_unicode/direct_to_struct/main`

Primary CSS row:

- `css_l4/declaration_values_extended/direct_to_struct/main`

Guard rows:

- Existing JSON guard table, with `json/twitter/direct_to_struct/main` as the
  named broad row if the report needs a single guard.
- `css_l4/declaration_values/direct_to_struct/main`.

W8 does not update `RESULTS.md` unless at least one row moves or admits.

## Preblocked Routes

Binding preblocks:

- REDRESS 121 public GrammarConfig / generic Lock 14 prose-only replay.
- REDRESS 54/55 direct source hook / decoded string source replay.
- REDRESS 66-69 direct receiver, scratch, and semantic-fact class.
- REDRESS 80 full-fixture/direct build shortcut.
- REDRESS 82 unicode codec proof-only closure.
- REDRESS 84 object-pair value-byte/control-tail replay.

Material differential:

- W8 keeps grammar policy private to generated modules.
- W8 keeps storage stable and moves only meanings.
- W8 pairs JSON and CSS row consumers in the same wave.
- W8 makes the report/gate consume the proof and rejects policy-only closure.

## Validation Commands

Minimum redress commands:

- `cargo test -p runtime json -- --nocapture`
- `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`
- `cargo test -p codegen css_l4_declaration_values_extended -- --nocapture`
- `cargo test -p bbnf-bench declaration_values_extended_cssparser_matches_generated_track1 -- --nocapture`
- `cargo test -p bbnf-bench declaration_values_extended_lightningcss_matches_generated_track1_and_cssparser -- --nocapture`
- `cargo test -p bbnf-bench skv13_per_grammar_policy_report -- --nocapture`
- `cargo test -p bbnf-bench --bin gate skv13_per_grammar_policy_report -- --nocapture`
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_per_grammar_policy_report_flag -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json
  --skv13-per-grammar-policy-report
  ../restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json`.
