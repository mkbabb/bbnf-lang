# SK-V14 W5A CH1: Correctness

Date: 2026-05-26.
Scope: CH1 correctness review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` for file:line support, measurable falsifiability gates, and verification-command mapping to actual repo commands/files.
Disposition: REVISE.

## §1 Findings

1. The plan's scope and source ledger are broadly correct. CH1 is required to check file:line support and measurable falsifiability (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:169`-`172`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112`-`119`). The plan cites SPEC §8 and all six W5A research artifacts as inputs (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:8`-`16`), and its intervention/owner paths match the SPEC W5A owner paths and entry requirements: grammar parser/runtime-generation support, codegen request entrypoint, `grammar_provider.rs`, `regen.rs`/`regen_css.rs`, W5A Lock 14 guard, and W5 attribution files (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`-`27`; `restart/skinny/tranches/sk-v14/SPEC.md:637`-`658`). This part is ACCEPT.

2. REVISE: the forbidden-call grep is inverted and would pass when the forbidden call is still present. The plan requires `regen-css` and every CSS companion to stop calling `codegen::emit_runtime_profile(target.profile)` at the `regen.rs` boundary (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`40`), but the verification command is a bare `rg -n "emit_runtime_profile\\(target\\.profile\\)" skinny/xtask/src/regen.rs` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:63`-`67`). At HEAD, the forbidden calls are present in both write and check paths (`skinny/xtask/src/regen.rs:14`-`18`, `skinny/xtask/src/regen.rs:30`-`32`), so the bare `rg` exits successfully in the failing state. The gate must assert zero matches, not merely print matches.

3. REVISE: the `w5a_` test filters are not concrete enough to be falsifiability gates. The plan lists `cargo test -p grammar w5a_ -- --nocapture` and `cargo test -p codegen w5a_ -- --nocapture` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:46`-`50`), but the current repo has no `w5a_` tests under `skinny/crates` or `skinny/xtask/src` by read-only grep. Research E already proposed named gates for the same contract: `w5a_runtime_contract_uses_source_and_metadata`, `w5a_sheets_bbnf_witnesses_use_runtime_contract`, and `w5a_named_unsupported_constructs` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:119`-`124`). The plan should name required tests or otherwise assert that the filtered commands ran nonzero W5A tests.

4. REVISE: the provider/template diff gate is non-failing. The plan requires no provider/template increase, deletion, or rename in W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`), and SPEC allows nonzero provider count only while forbidding new provider modules/template directories (`restart/skinny/tranches/sk-v14/SPEC.md:676`-`680`). Research F makes the stricter W5A guard explicit: provider roster remains 8, CSS template directories remain 7, and deletions/renames/unplanned additions fail (`restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:74`-`81`). The plan's command `git diff --name-status -- skinny/crates/codegen/src | rg '(_provider\\.rs|_templates)' || true` always exits successfully whether it finds a forbidden change or not (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:63`-`70`).

5. REVISE: two SPEC exit gates are asserted but not made executable. SPEC §8 requires full-table maintain within +/-1.0% and a W5A source/test LOC cap <=1.0k C-1 part-A (`restart/skinny/tranches/sk-v14/SPEC.md:679`-`680`). The plan mentions the LOC cap in prose (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:44`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:73`) but its verification block has no W5A delta-count command (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:46`-`71`). The repo has `cargo xtask lint-loc`, but that command enforces crate/global budgets, not the W5A <=1.0k source/test delta (`skinny/xtask/src/main.rs:186`-`231`). The full-table maintain gate is also absent from the falsifiability list and command set, aside from `cargo xtask gate-json --check-results` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`45`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:60`), which must be tied to a measurable W5A maintain proof or replaced with the actual table-refresh/check command.

6. The remaining verification commands map to real repo command surfaces. `cargo xtask` is a checked-in cargo alias (`.cargo/config.toml:128`-`139`), `skinny/Cargo.toml` includes `xtask`, `grammar`, and `codegen` workspace members (`skinny/Cargo.toml:3`-`15`), and `xtask` exposes `check-json`, `regen-css`, all seven `check-css-l4-*` commands, `lint-loc`, `bench-json`, and `gate-json` (`skinny/xtask/src/main.rs:10`-`39`). The seven CSS companion command names in the plan exactly match the `regen_css.rs` target roster and check wrappers (`skinny/xtask/src/regen_css.rs:25`-`75`, `skinny/xtask/src/regen_css.rs:81`-`117`).

## §2 Required plan edits if any

1. Replace the forbidden-call grep with an absence assertion, for example a gate that fails on any `emit_runtime_profile(target.profile)` match in `skinny/xtask/src/regen.rs`, plus a positive grep/test proving the new request path is called by `regen-css` and all seven companions.

2. Replace broad `w5a_` filters with named, required tests for parser constructs, source+metadata materiality, JSON equivalence, and Sheets/BBNF-self witness/fail-closed behavior. At minimum, carry the named gates recommended by W5A-E and add the CSS L4 construct parse/materiality test implied by W5A-A/B/C.

3. Make provider/template checks fail closed. Keep exact provider and CSS template counts at the W5A baseline, and make any provider/template path `A`, `D`, or `R` in staged or unstaged diff fail the gate.

4. Add an executable W5A source/test delta cap check for <=1.0k C-1 part-A. `cargo xtask lint-loc` can remain as a global budget check, but it does not replace a W5A-specific delta gate.

5. Add the measurable full-table maintain gate required by SPEC §8, or state precisely that `cargo xtask gate-json --check-results` consumes refreshed W5A results and enforces the +/-1.0% maintain condition.

## §3 Executable/read-only evidence

Read-only commands used:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '620,740p'
nl -ba restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md
rg -n "regen-css|check-css-l4|check-json|gate-json|check-conformance" skinny/xtask/src/main.rs skinny/xtask/src/regen_css.rs skinny/Cargo.toml
rg -n "^name = \"(grammar|codegen|xtask|bbnf-bench)\"|members|\"crates/(grammar|codegen|xtask|bbnf-bench)\"" skinny/Cargo.toml skinny/crates/*/Cargo.toml
rg -n "emit_runtime_profile\\(target\\.profile\\)|pub struct RuntimeTarget|fn write_targets|fn check_target|fn validate_inputs" skinny/xtask/src/regen.rs
rg -n "w5a_" skinny/crates skinny/xtask/src
nl -ba .cargo/config.toml | sed -n '128,141p'
nl -ba skinny/xtask/src/main.rs | sed -n '1,45p;152,165p;186,231p;273,335p;815,835p'
nl -ba skinny/xtask/src/regen_css.rs | sed -n '1,118p'
nl -ba skinny/xtask/src/regen.rs | sed -n '1,85p'
```

Results: governing sections and W5A artifacts were readable; xtask command names and workspace members exist; `emit_runtime_profile(target.profile)` is present at HEAD; `rg -n "w5a_" skinny/crates skinny/xtask/src` returned no matches; `CH1.md` did not exist before this write.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md`
- `.cargo/config.toml`
- `skinny/Cargo.toml`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
