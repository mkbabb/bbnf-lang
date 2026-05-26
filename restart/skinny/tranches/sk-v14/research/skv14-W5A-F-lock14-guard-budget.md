# SK-V14 W5A-F: Lock 14 Guard And Budget

Date: 2026-05-26.
Scope: W5A temporary Lock 14 guard, cap constraints, and no provider/template deletion proof.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md`.
HEAD: `66d15dfb504b3ef31b2f11f7c5529c8ce6505cd6`.

## §1 — Findings

W5A is now capability-only. SPEC §8 names the W5A owner paths as grammar parser/runtime-generation support, codegen runtime emission, an optional `grammar_provider.rs` successor, `regen.rs` / `regen_css.rs`, the temporary `lock14_baseline.rs` guard, and result/redress attribution; it explicitly excludes provider/template deletion from W5A entry (`restart/skinny/tranches/sk-v14/SPEC.md:637`-`658`). The required W5A tasks are source + workspace metadata into codegen, V1 source constructs parseable without `grammar_id == css_l4`, migrated `regen-css`, JSON unchanged-output proof, Sheets/BBNF-self witness/fail-closed proof, a temporary no-deletion/no-new-provider guard, and `regen-css` plus seven `check-css-l4-*` companions (`restart/skinny/tranches/sk-v14/SPEC.md:660`-`668`). The exit gate allows the provider count to remain non-zero in W5A, but no new provider module or template directory may be added; W5A is capped at <=1.0k C-1 part-A and cannot borrow from W5B or W6 (`restart/skinny/tranches/sk-v14/SPEC.md:670`-`680`).

REDRESS-209 is the load-bearing history. It rejected the original monolithic W5 because `regen-css` still emits through `codegen::emit_runtime_profile(target.profile)`, the static provider/template mesh is freshness-only source-hashed, and the skinny parser rejects CSS L4 `->` and `@{...}` constructs (`skinny/REDRESS.md:5173`-`5186`). Pass Omega V5 keeps REDRESS-209 as history and amends dispatch to W5A source-consuming generator capability followed by W5B provider/template deletion after W5A admits (`skinny/REDRESS.md:5189`-`5193`). The G-Omega signoff states the same split and says no LOCKS or ARCHITECTURE amendment is required (`restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md:14`-`20`).

The current source path is not yet source-consuming. `RuntimeTarget` already carries `source_inputs` and `metadata_inputs`, but `write_targets` and `check_target` call `codegen::emit_runtime_profile(target.profile)` after validation (`skinny/xtask/src/regen.rs:5`-`33`). `validate_inputs` hashes source and metadata and prints a digest, but does not pass bytes or parsed metadata into codegen (`skinny/xtask/src/regen.rs:61`-`74`). `emit_runtime_profile` selects by profile name only (`skinny/crates/codegen/src/lib.rs:117`-`120`), and `render_runtime_profile` still matches static `RuntimeProvider` variants for the seven CSS profiles plus JSON (`skinny/crates/codegen/src/lib.rs:162`-`210`). The profile registry itself is eight provider-backed profiles (`skinny/crates/codegen/src/grammar_profile.rs:16`-`26`, `skinny/crates/codegen/src/grammar_profile.rs:100`-`110`).

The parser gap remains real at HEAD. The skinny grammar parser accepts directives only for `@import` and `@token` (`skinny/crates/grammar/src/lib.rs:80`-`99`), and atoms are literals, regexes, groups, or identifiers only (`skinny/crates/grammar/src/lib.rs:196`-`231`). CSS L4 source uses value projection at `mathOperator` and span capture in `urlFunction` (`grammar/css/l4/values.bbnf:37`, `grammar/css/l4/values.bbnf:67`-`69`). W5A can therefore add only the minimal runtime-generation parser capability required by the source-consuming contract; a static body move would repeat the rejected W5 route.

The existing Lock 14 baseline is a useful gate but not yet a W5A guard. `validate()` checks allowlist entries, generated-header companion lint, git freeze, BackendShape surface, and generic-crate neutrality (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:599`-`605`). The baseline still accepts `per_grammar_provider` and `per_grammar_template` classes (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1709`-`1724`), includes provider/template paths in the allowlist (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:351`-`403`), and assembles current SK-V14 allowances only through W0/W2/W4 (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1081`-`1102`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1104`-`1157`). It freezes dirty/diff/cached paths under `FROZEN_ROOTS` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1160`-`1167`) and scans generic roots for a narrow forbidden token list (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1622`-`1650`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1697`-`1707`), but it does not currently express "W5A may touch generator contract owner paths while provider/template deletion is forbidden."

Gate surfaces already consume Lock 14 status. The bench gate calls `lock14_baseline::validate(&workspace)` before report handling (`skinny/crates/bbnf-bench/src/bin/gate.rs:50`-`53`). Report validators require `lock14_status == "pass:lock14_baseline::validate"` in multiple CSS/non-JSON gate rows (`skinny/crates/bbnf-bench/src/report.rs:7022`, `skinny/crates/bbnf-bench/src/report.rs:7272`), and SPEC requires the W5A same-wave consumers to be `cargo xtask regen-css` plus seven `check-css-l4-*` companions (`restart/skinny/tranches/sk-v14/SPEC.md:682`-`684`). The W5A guard should therefore live in `lock14_baseline.rs`, not as a prose-only research check.

## §2 — Executable Commands And Results

```sh
git rev-parse HEAD
# 66d15dfb504b3ef31b2f11f7c5529c8ce6505cd6

git status --short
# unrelated dirty restart research JSON files and one untracked handoff were present;
# no skinny/source paths or this artifact path were dirty before writing.

find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | sort | wc -l
# 8

find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | sort | wc -l
# 7

find skinny/crates/codegen/src -type d -name 'json_templates' | sort | wc -l
# 1

git status --porcelain -- skinny/crates/codegen/src skinny/xtask/src \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/RESULTS.md skinny/REDRESS.md
# no output

rg -n "emit_runtime_profile\\(|emit_from_source\\(|render_runtime_profile\\(|RuntimeProvider|source_inputs|metadata_inputs|check-css-l4" \
  skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/grammar_profile.rs \
  skinny/xtask/src/regen.rs skinny/xtask/src/regen_css.rs
# current call path shows source/metadata fields in xtask, but CSS runtime emission still reaches
# emit_runtime_profile(profile) and RuntimeProvider branches.

rg -n "grammar_name == \"css_l4\"|grammar_id == css_l4|grammar_id == \"css_l4\"|RuntimeProvider::CssL4|CssL4[A-Za-z]+ =>|Json =>" \
  skinny/crates/{codegen,grammar,passes,runtime,ir,bbnf-regex,bbnf-simd}/src \
  --glob '!**/*_provider.rs' --glob '!**/*_templates/**' --glob '!**/runtime/src/grammars/**'
# skinny/crates/codegen/src/lib.rs:167,173,179,185,191,197,203,209 still contain
# RuntimeProvider CSS/JSON branches in the generic codegen surface.
```

I did not run `cargo xtask regen-css`, the seven `check-css-l4-*` commands, or `cargo xtask gate-json`; those build/check paths can write target artifacts and are redress/plan verification, while this assignment is a 20-minute read-only research artifact.

## §3 — Risks

1. The temporary W5A guard can accidentally block W5A if it only preserves the pre-W5 owner-path allowance. Current allowances have SK-V14 W0/W2/W4 only; W5A needs a scoped allowance for parser/codegen/regen/lock14 owner paths, but not provider/template deletion.

2. The guard can accidentally admit static centralization if it checks only "no new provider/template dirs." W5A also needs an affirmative source-consuming proof: source bytes and workspace metadata must be consumed by the generator contract, not merely hashed for freshness.

3. A broad `lock14_baseline.rs` allowance can hide budget creep. The V5 CH4 fold binds W5A <=1.0k, W5B <=400, W5A+W5B <=1.4k, W6 unchanged <=2.0k, and any W6 borrowing returns REVISE before dispatch (`restart/audit/totality/astral/V5/hardening/CH4.md:15`-`23`).

4. The generic scan currently reports existing `RuntimeProvider::CssL4...` / `Json` branches in `codegen/src/lib.rs`. W5A may need a two-stage interpretation: temporary state permits existing legacy branches only if the migrated W5A call boundary for all seven CSS profiles no longer consumes them; W5B then deletes/retires the mesh and tightens the forward invariant to zero providers/templates.

5. Sheets/BBNF-self proof can be underspecified. V5 CH2 accepted the fold only after it required JSON unchanged-output plus Sheets and BBNF-self fail-closed or generated-role witnesses through the same parser/contract (`restart/audit/totality/astral/V5/hardening/CH2.md:17`-`25`).

## §4 — Recommended Plan Inputs

1. Add `SK_V14_W5A_OWNER_PATHS` to `lock14_baseline.rs`, limited to W5A owner paths: grammar parser/runtime-generation parser module, `codegen/src/lib.rs`, new `codegen/src/grammar_provider.rs` or successor module, `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and `lock14_baseline.rs`. Do not include existing `css_l4_*_provider.rs`, `css_l4_*_templates/`, or `json_templates/` as W5A-deletable owner paths.

2. Add an explicit temporary W5A provider/template invariant: the exact HEAD provider roster remains 8 and the exact HEAD CSS template directory roster remains 7; any deleted, renamed, or newly added provider/template path fails W5A. W5B owns the transition from this invariant to zero providers and zero CSS template dirs (`restart/skinny/tranches/sk-v14/SPEC.md:701`-`738`).

3. Add a source-consuming positive check beside the no-deletion check. Minimum proof inputs: a W5A emission request carrying grammar id, target/profile id, source bundle digest plus parsed source construct evidence, workspace metadata digest/fields, output roster, and runtime contract. `regen-css` must pass these into codegen; `emit_runtime_profile(profile)` alone should fail W5A.

4. Require no-provider/template-deletion proof in the plan and redress logs:
   `git diff --name-status -- skinny/crates/codegen/src | rg '(_provider\\.rs|_templates)'` must show no `D`, no `R`, and no unplanned `A`; the provider count must remain 8 and CSS template dir count 7 during W5A. Generated runtime output may refresh only through the migrated source-consuming path.

5. Keep cap accounting local. W5A source/test LOC <=1.0k, no W5B/W6 borrowing, and generated output uncounted only if byte-equivalent or explicitly produced by `regen-css` through the new path. If parser support for `->` and `@{...}` cannot fit inside W5A, return REVISE rather than move deletion forward.

6. Same-wave verification command set for the plan: `cargo xtask regen-css`; all seven `cargo xtask check-css-l4-*` commands named in `regen_css.rs:81`-`107`; JSON unchanged-output check selected by the plan; Sheets/BBNF-self fail-closed or generated-role witness command; and `cargo xtask gate-json --check-results` after `lock14_baseline.rs` grows the W5A guard. These are plan/redress commands, not research commands.

## §5 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:637`-`680`, `restart/skinny/tranches/sk-v14/SPEC.md:701`-`738`.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:135`-`145`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:207`-`215`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:224`-`228`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`-`39`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177`-`186`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190`-`200`.
- `restart/locks/LOCKS.md:349`-`364`, `restart/locks/LOCKS.md:377`-`397`.
- `skinny/REDRESS.md:5173`-`5193`.
- `restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md:14`-`20`, `restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md:61`-`65`.
- `skinny/xtask/src/regen.rs:5`-`33`, `skinny/xtask/src/regen.rs:61`-`74`, `skinny/xtask/src/regen_css.rs:5`-`23`, `skinny/xtask/src/regen_css.rs:25`-`75`, `skinny/xtask/src/regen_css.rs:81`-`107`.
- `skinny/crates/codegen/src/lib.rs:117`-`120`, `skinny/crates/codegen/src/lib.rs:162`-`210`.
- `skinny/crates/codegen/src/grammar_profile.rs:16`-`26`, `skinny/crates/codegen/src/grammar_profile.rs:100`-`110`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:599`-`605`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1160`-`1167`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1622`-`1650`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1697`-`1724`.
