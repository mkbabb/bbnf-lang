# SK-V14 W5A CHALLENGE V4 CH4: Cap and Budget
Disposition: ACCEPT
Acceptance score: 95/100
Findings:
- ACCEPT: W5A keeps the hard <=1.0k C-1 part-A source/test LOC cap separate from W5B and W6. `SPEC.md:242` assigns W5A <=1.0k and says it cannot borrow from W5B or W6; `SPEC.md:243` gives W5B a distinct <=400 C-1 part-A slice and combined W5A+W5B <=1.4k; `SPEC.md:244` reserves W6 as a separate <=2.0k C-1 part-B aggregate.
- ACCEPT: The W5A component ledger is explicit and totals exactly 1000 LOC: parser/tests <=300, codegen contract/tests <=300, regen routing <=150, Lock 14 guard <=120, Sheets/BBNF witnesses <=100, and gate-attribution edits <=30 at `skv14-W5A-plan.md:47`-`54`. The zero-slack total is acceptable because the hard reject path is now executable, not discretionary.
- ACCEPT: The cap reject protocol is concrete. `SPEC.md:251`-`257` makes LOC budgets conjunctive with the 90-minute cap and requires split-or-REVISE on overflow; `SPEC.md:264`-`274` sets the 75-minute implementation/redress target and 90-minute hard ceiling. The W5A plan repeats "return REVISE before source edits" at `skv14-W5A-plan.md:56` and "commit or reject at cap" at `skv14-W5A-plan.md:109`.
- ACCEPT: Generated-output accounting is bounded. `SPEC.md:251`-`257` excludes generated files from source LOC only when named, diff-audited, and included in the revert slice. The W5A plan narrows that allowance to `cargo xtask regen-css` output at `skv14-W5A-plan.md:33` and `skv14-W5A-plan.md:56`, preventing generated CSS runtime churn from consuming or hiding W5A source/test budget.
- ACCEPT: The owner/non-owner boundary blocks W5B/W6 budget leakage. W5A owner paths are listed at `skv14-W5A-plan.md:20`-`27`; non-owner paths at `skv14-W5A-plan.md:29`-`35` exclude CSS provider/template deletion, root `crates/core/src/runtime/css_l4/`, and `grammar/css/l4/`. SPEC reinforces that W5A does not delete CSS providers/templates or `crates/core/src/runtime/` at `SPEC.md:657`-`658`, while `SPEC.md:686`-`691` pre-blocks borrowing W5B/W6 budget to paper over generator-contract gaps.
- ACCEPT: The executable LOC and boundary gates are sufficient for final confirmation. `skv14-W5A-plan.md:92`-`105` requires provider/template counts, staged/unstaged A/D/R rejection, and `git diff --numstat HEAD -- ...` with `test "$W5A_LOC" -le 1000`; `skv14-W5A-plan.md:106` separately rejects root CSS/grammar diffs.
Required folds:
- NONE
Evidence:
- `restart/skinny/tranches/sk-v14/SPEC.md:242`-`244`: W5A/W5B/W6 budgets and no W5A borrowing.
- `restart/skinny/tranches/sk-v14/SPEC.md:251`-`257`: generated-output accounting and split-or-REVISE requirement.
- `restart/skinny/tranches/sk-v14/SPEC.md:264`-`274`: 75-minute target, 90-minute hard ceiling, and cap overflow disposition.
- `restart/skinny/tranches/sk-v14/SPEC.md:657`-`680`: W5A entry/exit gates, no provider/template/root-runtime deletion, and <=1.0k no-borrow cap.
- `restart/skinny/tranches/sk-v14/SPEC.md:686`-`698`: pre-blocked W5A budget borrowing and reject/downstream protocol.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:20`-`35`: owner/non-owner ledger boundaries.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`-`56`: provider/template count invariant, <=1.0k no-borrow cap, component ledger, and generated-output allowance.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:84`-`106`: executable grep/count, A/D/R, LOC, and root CSS/grammar boundary gates.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:109`-`115`: hard-cap reject, revert slice, and W5A-only downstream unlock.
- Inventory commands run from repo root: `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '` returned `8`; `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '` returned `7`.
- Boundary command run from repo root: `git diff --name-status HEAD -- skinny/crates/codegen/src crates/core/src/runtime/css_l4 grammar/css/l4 skinny/crates/runtime/src/grammars` returned no paths.
