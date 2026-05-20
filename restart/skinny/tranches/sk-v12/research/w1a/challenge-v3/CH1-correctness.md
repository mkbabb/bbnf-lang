# SK-V12 W1a CHALLENGE V3 - CH1 Correctness

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Lens: CH1 correctness.
Disposition: ACCEPT.

## Findings

V3 closes the CH1 blockers from CHALLENGE V2.

1. Exact owner/generated roster is now concrete enough for redress. `scan.rs`
   and `sink.rs` are explicitly editable source paths, while the generated JSON
   roster is exactly the eight files `config.rs`, `generated.rs`, `host.rs`,
   `mod.rs`, `parser.rs`, `value.rs`, `view.rs`, and `visitor.rs`.

2. `scan.rs` / `sink.rs` ownership is corrected. Current skinny code still
   shows why this was needed: `scan.rs` has a generated header, and
   `json_provider.rs` currently emits/checks both files from runtime sources.
   V3 requires redress to remove that provenance ambiguity and stop treating
   them as generated outputs.

3. `json_templates/mod.rs` is no longer fuzzy. V3 marks it explicitly not
   owned. The file does not currently exist, so creating or editing it during
   W1a would require returning to plan.

4. The seven-leak matrix is executable in plan shape. V3 requires the Lock 14
   scan to reject all seven forbidden generic leak classes and to include
   positive fixtures proving the same tokens remain legal in JSON-owned roots.

5. Orphan checks are now named executable tests:
   `json_config_policy_fields_are_consumed`,
   `grammar_profile_fields_are_consumed`, and `lock14_baseline`. The tests must
   prove config/profile fields have same-wave consumers and inert fields fail.

6. The AWK guard verifier is acceptable for CH1. It hardcodes every SPEC
   Section 0.5 direct and typed floor, fails missing rows, checks Track 1 and
   Track 2 columns, and currently returns `SK-V12 JSON guard floors PASS`
   against `skinny/RESULTS.md`.

## CH1 Disposition

ACCEPT. CH1 no longer blocks W1a V3 redress. The remaining burden is
implementation: redress must make the named tests real, update `check-json`
exactness to the eight-file generated roster, and keep `scan.rs` / `sink.rs`
out of generated-output enforcement.
