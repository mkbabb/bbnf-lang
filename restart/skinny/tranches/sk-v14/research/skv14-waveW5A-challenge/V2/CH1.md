# SK-V14 W5A CHALLENGE V2 CH1: Correctness

Date: 2026-05-26.
Scope: CH1 correctness review of the revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`, limited to whether the V1 CH1 REVISE items were folded into measurable, fail-closed gates.
Disposition: REVISE.

## §1 Findings

1. ACCEPT: the revised plan folds the forbidden-call absence gate. V1 required an absence assertion for `emit_runtime_profile(target.profile)` plus a positive proof that the new request path is used (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md:23`). The revised plan states the call-boundary requirement (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`), makes the forbidden-call grep fail closed with `if rg ...; then exit 1; fi` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:86`), and adds a positive `RuntimeGenerationRequest|emit_runtime_from_request` grep over `regen.rs`, `lib.rs`, and `grammar_provider.rs` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:87`-`90`).

2. ACCEPT: the revised plan prevents named tests from paper-passing with zero matches. V1 required concrete named tests and nonzero W5A execution proof (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md:25`). The revised plan names exact grammar/codegen tests for CSS source facts, named unsupported constructs, source+metadata materiality, JSON equivalence, and Sheets/BBNF fail-closed behavior, each run with `--exact`, logged, and followed by `rg "test result: ok\\. [1-9][0-9]* passed"` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:61`-`70`).

3. REVISE: the provider/template gate is fail-closed for live working-tree `A`/`D`/`R` changes, but it still does not satisfy V1's staged-or-unstaged requirement. V1 required exact provider/template baselines and failure on any provider/template `A`, `D`, or `R` in staged or unstaged diff (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md:27`). The revised plan adds baseline count checks for 8 provider modules and 7 CSS template directories (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:91`-`92`) plus an unstaged `git diff --name-status -- skinny/crates/codegen/src | rg ...` failure gate (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:93`). Because that diff is not against `HEAD` and does not include `--cached`, a staged rename that preserves the 8/7 counts can still evade the `A`/`D`/`R` gate.

4. REVISE: full-table maintain is now present as a command, but the cited command does not execute the SPEC's +/-1.0% all-row maintain check. SPEC requires full-table maintain at +/-1.0% on all rows (`restart/skinny/tranches/sk-v14/SPEC.md:679`), and V1 required either a measurable maintain gate or a precise statement that `gate-json --check-results` consumes refreshed W5A results and enforces that condition (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md:31`). The revised plan asserts enforcement through `cargo xtask gate-json --check-results --skv14-existing-results-capture` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:44`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:80`). The xtask wrapper accepts and forwards the flag (`skinny/xtask/src/main.rs:273`-`305`), but the bench gate's `--skv14-existing-results-capture` branch reads `RESULTS.md`, renders the existing capture, rejects staleness, prints it, and returns before the normal benchmark-result gate path (`skinny/crates/bbnf-bench/src/bin/gate.rs:390`-`409`). The capture validator validates manifest shape, required row universe, and audit-overlay counts (`skinny/crates/bbnf-bench/src/report.rs:4147`-`4220`; `skinny/crates/bbnf-bench/src/report.rs:3504`-`3552`); the xtask W0 snapshot validator likewise checks row universe and audit-overlay counts (`skinny/xtask/src/main.rs:427`-`473`) and only requires `sk_v14_open_delta` to be non-empty (`skinny/xtask/src/main.rs:535`-`556`). That is executable as capture freshness, but not as a refreshed +/-1.0% full-table maintain proof.

5. ACCEPT: the W5A LOC cap is now executable. V1 required a W5A-specific source/test delta gate rather than `lint-loc` alone (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md:29`). The revised plan adds a component ledger totaling <=1.0k source/test LOC (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:47`-`56`) and an executable `git diff --numstat HEAD -- ... | awk ...` gate that prints `W5A source/test LOC delta` and fails when the delta exceeds 1000 (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:94`-`103`).

6. The V1 consolidated packet claims the intended folds were applied, including exact named tests, forbidden-call fail-closed checking, provider/template `A`/`D`/`R` checks, LOC ledger/gate, and full-table maintain routing (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:23`-`30`). CH1 V2 verifies the first, second, and LOC folds as sufficient, but the provider/template diff coverage and full-table maintain semantics still require edits before CH1 can accept.

## §2 Remaining required edits if any

1. Make the provider/template `A`/`D`/`R` gate cover staged and unstaged state, for example by using `git diff --name-status HEAD -- skinny/crates/codegen/src` or by checking both `git diff --name-status -- ...` and `git diff --cached --name-status -- ...`.

2. Replace or supplement `cargo xtask gate-json --check-results --skv14-existing-results-capture` with an executable full-table maintain proof that compares the refreshed W5A result table against the required baseline and fails on any row outside +/-1.0% or any correctness/audit-overlay downgrade.

## §3 Evidence

Read-only commands used:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '637,700p'
rg -n "skv14-existing-results-capture|check-results|gate-json" skinny/xtask/src skinny/crates/bbnf-bench/src
nl -ba skinny/xtask/src/main.rs | sed -n '273,305p;387,473p;535,556p'
nl -ba skinny/crates/bbnf-bench/src/bin/gate.rs | sed -n '390,409p'
nl -ba skinny/crates/bbnf-bench/src/report.rs | sed -n '3504,3552p;4147,4220p'
find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '
find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
git diff --name-status -- skinny/crates/codegen/src
git diff --cached --name-status -- skinny/crates/codegen/src
```

Observed: current provider/template counts are 8 and 7, and there is no current staged or unstaged diff under `skinny/crates/codegen/src`; the finding is about the revised plan gate's future coverage. `V2/CH1.md` did not exist before this write.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH1.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
