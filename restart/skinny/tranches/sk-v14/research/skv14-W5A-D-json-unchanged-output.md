# SK-V14 W5A-D: JSON Unchanged-Output Proof

Date: 2026-05-26.
Scope: Prove how W5A can keep JSON byte output unchanged while introducing the source-consuming runtime generator contract.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md`.
HEAD: `66d15dfb5`.

## 1. Findings

W5A is not a provider-deletion wave. SPEC Section 8 authorizes a grammar-neutral source-consuming contract module, `skinny/xtask/src/regen.rs`, `skinny/xtask/src/regen_css.rs`, a temporary Lock 14 guard, and W5 attribution artifacts, but not CSS provider/template deletion or `crates/core/src/runtime/` work (`restart/skinny/tranches/sk-v14/SPEC.md:641`, `restart/skinny/tranches/sk-v14/SPEC.md:644`, `restart/skinny/tranches/sk-v14/SPEC.md:645`, `restart/skinny/tranches/sk-v14/SPEC.md:657`, `restart/skinny/tranches/sk-v14/SPEC.md:658`). The W5A task list requires codegen to consume grammar source plus workspace metadata, CSS L4 source constructs to parse without grammar-name branches, `regen-css` to move all seven profiles to the source-consuming path, JSON unchanged-output proof through that same contract, Sheets/BBNF-self fail-closed or generated-role witnesses, and the seven CSS check companions (`restart/skinny/tranches/sk-v14/SPEC.md:662`, `restart/skinny/tranches/sk-v14/SPEC.md:665`, `restart/skinny/tranches/sk-v14/SPEC.md:666`, `restart/skinny/tranches/sk-v14/SPEC.md:668`). W5B later owns deleting providers/templates and preserving the JSON proof (`restart/skinny/tranches/sk-v14/SPEC.md:705`, `restart/skinny/tranches/sk-v14/SPEC.md:721`, `restart/skinny/tranches/sk-v14/SPEC.md:725`, `restart/skinny/tranches/sk-v14/SPEC.md:735`).

The dispatch contract makes this a read-only research artifact. Research outputs one artifact and no source edits (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:37`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:39`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:49`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:53`). Before W5A/W5B/W6/W7, Pass Omega V5 and the amended sequencing must exist, and provider/template deletion is forbidden before W5A proves all-seven CSS companions plus JSON/Sheets/BBNF-self proof (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:143`). SKINNY-TRIUMVIRATE requires research to read current source, RESULTS, and REDRESS, then emit one artifact with concrete file:line findings, recommendations, risks, and sources (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:13`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:27`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:35`).

REDRESS-209 is the specific failure W5A must avoid. It rejected monolithic W5 because CSS source and metadata were hashed only for freshness, not passed into codegen; current HEAD still emitted `regen-css` through `codegen::emit_runtime_profile(target.profile)` and a static provider/template mesh; and the skinny grammar parser rejected CSS L4 constructs such as `->` projections and `@{...}` span capture (`skinny/REDRESS.md:5173`, `skinny/REDRESS.md:5178`, `skinny/REDRESS.md:5180`, `skinny/REDRESS.md:5181`). The supersession note keeps REDRESS-209 as historical rejection but routes current work into W5A source-consuming generator capability followed by W5B deletion (`skinny/REDRESS.md:5189`, `skinny/REDRESS.md:5191`).

The current CSS regen path confirms the REDRESS-209 diagnosis. `RuntimeTarget` carries `profile`, `output_dir`, `check_command`, `source_inputs`, and `metadata_inputs` (`skinny/xtask/src/regen.rs:5`). `write_targets` validates inputs, then calls `codegen::emit_runtime_profile(target.profile)` without passing the source or metadata bytes (`skinny/xtask/src/regen.rs:14`, `skinny/xtask/src/regen.rs:18`). `check_target` repeats the same profile-only call (`skinny/xtask/src/regen.rs:30`, `skinny/xtask/src/regen.rs:32`). The source and metadata inputs are hashed and printed, but not returned or passed into codegen (`skinny/xtask/src/regen.rs:61`, `skinny/xtask/src/regen.rs:64`, `skinny/xtask/src/regen.rs:68`, `skinny/xtask/src/regen.rs:72`). `regen_css` enumerates the 15 CSS source files and `Cargo.toml`/`skinny/Cargo.toml` metadata, then assigns the same inputs to all seven runtime targets (`skinny/xtask/src/regen_css.rs:5`, `skinny/xtask/src/regen_css.rs:23`, `skinny/xtask/src/regen_css.rs:25`, `skinny/xtask/src/regen_css.rs:77`).

JSON is already closer to the desired shape, but still not the W5A contract. `regen-json` reads `skinny/grammars/json.bbnf` and calls `codegen::emit_from_source("json", &source)`, while `check-json` does the same and compares the output directory (`skinny/xtask/src/main.rs:152`, `skinny/xtask/src/main.rs:154`, `skinny/xtask/src/main.rs:159`, `skinny/xtask/src/main.rs:161`, `skinny/xtask/src/main.rs:163`). It consumes source, but not workspace metadata. The skinny workspace metadata names the generated root, JSON source, output directory, codegen knobs, and runtime tape mode (`skinny/Cargo.toml:54`, `skinny/Cargo.toml:60`, `skinny/Cargo.toml:64`, `skinny/Cargo.toml:69`). W5A's JSON proof should therefore compare the new source+metadata contract against the current `emit_from_source` result, then against checked-in files.

Codegen currently has two emission lanes. `emit_from_source` parses grammar source, compiles it, and emits with layout facts (`skinny/crates/codegen/src/lib.rs:98`, `skinny/crates/codegen/src/lib.rs:99`, `skinny/crates/codegen/src/lib.rs:100`, `skinny/crates/codegen/src/lib.rs:101`). `emit_runtime_profile` selects by grammar name and renders a static profile (`skinny/crates/codegen/src/lib.rs:117`, `skinny/crates/codegen/src/lib.rs:118`). `render_runtime_profile` branches over `RuntimeProvider` variants; CSS arms return provider template files, while JSON falls through and requires a sink-only lowering program before appending the generated direct parser (`skinny/crates/codegen/src/lib.rs:162`, `skinny/crates/codegen/src/lib.rs:166`, `skinny/crates/codegen/src/lib.rs:209`, `skinny/crates/codegen/src/lib.rs:211`, `skinny/crates/codegen/src/lib.rs:215`, `skinny/crates/codegen/src/lib.rs:217`). `RuntimeProvider` is an eight-variant enum and the runtime profile roster is a fixed eight-profile array (`skinny/crates/codegen/src/grammar_profile.rs:16`, `skinny/crates/codegen/src/grammar_profile.rs:100`). `json_provider` still supplies most JSON runtime files from static templates (`skinny/crates/codegen/src/json_provider.rs:48`, `skinny/crates/codegen/src/json_provider.rs:60`, `skinny/crates/codegen/src/json_provider.rs:64`, `skinny/crates/codegen/src/json_provider.rs:68`, `skinny/crates/codegen/src/json_provider.rs:72`).

The JSON generated-output surface is 8 generated files plus adjacent non-roster JSON-owned files. The profile expects `config.rs`, `generated.rs`, `host.rs`, `mod.rs`, `parser.rs`, `value.rs`, `view.rs`, and `visitor.rs` (`skinny/crates/codegen/src/json_provider.rs:3`). The checked-in runtime directory also has `event_grammar_witness.rs`, `scan.rs`, and `sink.rs`; `mod.rs` imports `scan` and `sink` even though they are not in the generated roster (`skinny/crates/runtime/src/grammars/json/mod.rs:1`, `skinny/crates/runtime/src/grammars/json/mod.rs:6`, `skinny/crates/runtime/src/grammars/json/mod.rs:7`). Lock 14 baseline classifies the eight JSON roster files as `generated_json_output`, but `scan.rs` and `sink.rs` as `per_grammar_runtime_source` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:129`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:153`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:159`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:165`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:171`). The proof must guard both surfaces: generated roster equality and whole-directory no-diff/no-hash-change.

The current grammar parser supports skinny JSON, but not the CSS/Self/Sheets constructs W5A must account for. It accepts only `@import` and `@token` directives, rejecting other directives as `BBNF-DIRECTIVE-NOT-IN-SKINNY` (`skinny/crates/grammar/src/lib.rs:80`, `skinny/crates/grammar/src/lib.rs:91`, `skinny/crates/grammar/src/lib.rs:93`). It parses literals, regexes, parentheses, identifiers, and postfix `*`, `?`, `+` (`skinny/crates/grammar/src/lib.rs:153`, `skinny/crates/grammar/src/lib.rs:196`, `skinny/crates/grammar/src/lib.rs:210`, `skinny/crates/grammar/src/lib.rs:217`, `skinny/crates/grammar/src/lib.rs:223`). Skinny JSON only uses this simple subset (`skinny/grammars/json.bbnf:1`, `skinny/grammars/json.bbnf:4`, `skinny/grammars/json.bbnf:11`). CSS L4 uses `@ws`, `@pretty`, `?w`, `>>`, `<<`, map projections, host calls, and `@{...}` capture (`grammar/css/l4/stylesheet.bbnf:12`, `grammar/css/l4/stylesheet.bbnf:16`, `grammar/css/l4/stylesheet.bbnf:53`, `grammar/css/l4/values.bbnf:69`, `grammar/css/l4/color.bbnf:190`, `grammar/css/l4/color.bbnf:220`, `grammar/css/l4/color.bbnf:228`). Sheets and BBNF-self also use map projections, `?w`, binary operators, imports, and pretty directives (`grammar/google-sheets/google-sheets.bbnf:6`, `grammar/google-sheets/google-sheets.bbnf:145`, `grammar/google-sheets/google-sheets.bbnf:171`, `grammar/bbnf/bbnf.bbnf:33`, `grammar/bbnf/bbnf.bbnf:42`, `grammar/bbnf/bbnf.bbnf:68`, `grammar/bbnf/bbnf.bbnf:70`).

## 2. Commands And Results

All commands were re-executed at HEAD `66d15dfb5`.

| Command | Result |
|---|---|
| `git rev-parse --short HEAD` | `66d15dfb5` |
| `git status --short` | Pre-existing unrelated dirty files under `restart/skinny/tranches/sk-v12/research/...`, `restart/skinny/tranches/sk-v13/research/...`, plus untracked `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md`; this artifact was not present. |
| `printf 'provider_rs=%s\ncss_l4_templates=%s\nall_templates=%s\n' ...` | `provider_rs=8`, `css_l4_templates=7`, `all_templates=8`. This matches W5A's no-deletion state and W5B's future deletion target. |
| `cargo xtask check-json` from `skinny/` | PASS. Cargo emitted existing patch/profile warnings, then ran `target/ax-iter/xtask check-json` successfully. |
| `cargo xtask check-conformance` from `skinny/` | PASS: `conformance: 21 valid fixtures accepted; 7 invalid fixtures rejected`. |
| `cargo test -p codegen --lib` from `skinny/` | FAIL at HEAD: 24 passed, 1 failed. Failure: `tests::json_config_policy_fields_are_consumed`, `STRING_NEEDS_DECODE has no generated consumer`. Treat this as a proof risk, not a W5A regression. |
| `cargo test -p codegen emission_is_deterministic --lib` | PASS: 1 passed. |
| `cargo test -p codegen grammar_profile_fields_are_consumed --lib` | PASS: 1 passed. |
| `cargo test -p codegen direct_parser_is_authored_from_sink_only_lowering --lib` | PASS: 1 passed. |
| `cargo test -p grammar parses_skinny_json_rules --lib` | PASS: 1 passed. |
| `cargo test -p grammar rejects_non_skinny_directives --lib` | PASS: 1 passed. |

Current JSON runtime whole-directory hashes:

```text
6cbc5efd122aa4c30f80205df2a429d676d016394994566763f5fefda325ae2c  skinny/crates/runtime/src/grammars/json/config.rs
a9cbd48bc3e561c793e211a6d39d789e6a0ece5ed41bfa72bddda26948644def  skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs
348fa99c5ee4c9e13b7dde0ad1cce0110ee728c8b8e3e7a8e71134987f5a8649  skinny/crates/runtime/src/grammars/json/generated.rs
f9bb60469545575d534054975f9f98b74b9a8480b9d08848fbc5e81045be5285  skinny/crates/runtime/src/grammars/json/host.rs
581105b65da877ed66efd2f91d570b368a2ea7413f89f9748cb3f04570a5488a  skinny/crates/runtime/src/grammars/json/mod.rs
4f12323de60c80d91d4330560ae6d585895b8b8c976fbbdc0481d09ae1b7c10b  skinny/crates/runtime/src/grammars/json/parser.rs
790cba2ed6473c2f65c0506943e4a040426198a8141e7902656ba88d49cb25df  skinny/crates/runtime/src/grammars/json/scan.rs
0ae3b72f81eeda98ad99acb92d5c6a408b0f4a3f417fae3c86c0f64f4f56e0dd  skinny/crates/runtime/src/grammars/json/sink.rs
e1150fdd3987086feb5656da5fa878a4cded10d4b6e3a8f4e4c4a4cd9c748564  skinny/crates/runtime/src/grammars/json/value.rs
10e6b6ed702dee42b7d3990f14b6a8cd42158cdf34d4de90ef473d710ffd7042  skinny/crates/runtime/src/grammars/json/view.rs
1e99f25b755a32396497677dd861b5d64da4cfbb79dc9a36401e1aba038dd86e  skinny/crates/runtime/src/grammars/json/visitor.rs
```

## 3. Exact Before/After Proof Candidates

Candidate A: byte-level generated-output proof, redress-local before/after.

Before implementing the W5A contract:

```sh
git rev-parse --short HEAD
cargo xtask check-json
find skinny/crates/runtime/src/grammars/json -maxdepth 1 -type f -print | sort | xargs shasum -a 256 > /tmp/skv14-w5a-json.before.sha256
```

After implementing the W5A contract and migrating `check-json` to the same source+metadata request path:

```sh
cargo xtask check-json
find skinny/crates/runtime/src/grammars/json -maxdepth 1 -type f -print | sort | xargs shasum -a 256 > /tmp/skv14-w5a-json.after.sha256
diff -u /tmp/skv14-w5a-json.before.sha256 /tmp/skv14-w5a-json.after.sha256
git diff --exit-code -- skinny/crates/runtime/src/grammars/json
```

Admission standard: `check-json` passes, `diff -u` is empty, and `git diff --exit-code` is clean for the JSON runtime directory. This covers the eight generated files plus `event_grammar_witness.rs`, `scan.rs`, and `sink.rs`.

Candidate B: in-code source+metadata contract equivalence.

Add a focused codegen test around the new contract:

```rust
let source = include_str!("../../../grammars/json.bbnf");
let metadata = include_str!("../../../Cargo.toml");
let old = emit_from_source("json", source).unwrap();
let new = emit_runtime_from_contract(RuntimeGenerationRequest {
    grammar_name: "json",
    grammar_source: source,
    workspace_metadata: metadata,
}).unwrap();
assert_eq!(old, new);
new.check_dir("../runtime/src/grammars/json").unwrap();
```

Admission standard: the test passes and `cargo xtask check-json` is wired through this same contract, not a separate compatibility path.

Candidate C: behavior-level JSON proof.

After W5A contract migration:

```sh
cargo xtask check-conformance
cargo test -p codegen emission_is_deterministic --lib
cargo test -p codegen grammar_profile_fields_are_consumed --lib
cargo test -p codegen direct_parser_is_authored_from_sink_only_lowering --lib
cargo test -p grammar parses_skinny_json_rules --lib
```

Admission standard: all pass. Do not require full `cargo test -p codegen --lib` until the pre-existing `json_config_policy_fields_are_consumed` failure is either fixed or explicitly routed as unrelated.

Candidate D: W5A no-deletion guard.

Before and after W5A:

```sh
printf 'provider_rs=%s\ncss_l4_templates=%s\nall_templates=%s\n' \
  "$(find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' ')" \
  "$(find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l | tr -d ' ')" \
  "$(find skinny/crates/codegen/src -maxdepth 1 -type d -name '*_templates' | wc -l | tr -d ' ')"
```

Admission standard for W5A: provider/template counts do not increase, and CSS provider/template deletion is absent. W5B owns the zero-count deletion target.

## 4. Risks

Risk 1: A JSON-only source-consuming proof can pass while CSS remains REDRESS-209-shaped. W5A must prove JSON through the same contract used by CSS `regen-css`, not through a parallel `regen-json` carve-out.

Risk 2: Hashing source/metadata is insufficient. The current `validate_inputs` shape hashes inputs for freshness only (`skinny/xtask/src/regen.rs:61`, `skinny/xtask/src/regen.rs:72`). The new request object must pass parsed source and parsed workspace metadata into codegen and expose tests that fail if metadata is absent.

Risk 3: The generated roster excludes `scan.rs`, `sink.rs`, and `event_grammar_witness.rs`. `cargo xtask check-json` alone only compares the emitted roster. The whole-directory hash/no-diff guard is needed to make "JSON unchanged-output" unambiguous.

Risk 4: Full `cargo test -p codegen --lib` fails at HEAD. A plan that makes full codegen tests mandatory without acknowledging the current `STRING_NEEDS_DECODE` failure may incorrectly classify W5A as regressed.

Risk 5: Supporting CSS constructs by grammar-name special cases would reopen Lock 14. The parser/contract must parse constructs such as `@ws`, `@pretty`, `?w`, `>>`, `<<`, `->`, and `@{...}` as grammar-neutral syntax, then either lower them or fail closed by construct name.

Risk 6: Sheets/BBNF-self proof can become paper-close if it merely says "unsupported." The fail-closed result should name the first unsupported construct and source location, or provide a generated-role witness through the same contract.

## 5. Recommended Plan Inputs

1. Introduce one `RuntimeGenerationRequest`/`RuntimeGenerationContract` module, likely `skinny/crates/codegen/src/grammar_provider.rs`, with fields for grammar identity from metadata, grammar source bytes, parsed workspace metadata, output directory, and runtime profile id. `regen-css`, `check-css-l4-*`, `regen-json`, and `check-json` should call this request path.

2. Keep JSON output unchanged by implementing the JSON request as `parse source -> passes::compile -> emit_with_layout`, then asserting equality against the current `emit_from_source("json", source)` output. Metadata must still be parsed and consumed, at minimum to validate source path, output directory, and runtime mode.

3. For CSS W5A, parse required V1 source constructs grammar-neutrally before emission. It is acceptable for W5A to retain provider modules internally until W5B, but the `xtask` call boundary must no longer pass only a profile string.

4. Add a temporary W5A Lock 14 guard that forbids new provider modules/template directories and fails if CSS provider/template deletion happened before W5B.

5. Use the proof bundle: `cargo xtask check-json`, whole-directory JSON hashes, `cargo xtask check-conformance`, targeted codegen/grammar tests, and the provider/template count guard. Carry the full codegen-lib failure as pre-existing unless the W5A implementation also fixes it within budget.

## 6. Sources

No external sources. All citations are local repository files re-read at HEAD `66d15dfb5`.
