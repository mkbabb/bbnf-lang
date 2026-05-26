# SK-V14 W5A CHALLENGE V3 CH4: Cap and Budget
Disposition: ACCEPT
Acceptance score: 94/100
Findings:
- ACCEPT: V3 preserves the SPEC W5A cap exactly. `SPEC.md:242` binds W5A to <=1.0k C-1 part-A source/test LOC with no W5B/W6 borrowing, while `SPEC.md:243` gives W5B only <=400 LOC and `SPEC.md:244` reserves W6's separate C-1 part-B aggregate. The plan repeats the no-borrow rule at `skv14-W5A-plan.md:45` and `skv14-W5A-plan.md:56`.
- ACCEPT: The component ledger is feasible but zero-slack: parser/tests <=300, codegen contract/tests <=300, regen routing <=150, Lock 14 guard <=120, Sheets/BBNF witnesses <=100, and admit/reject gate-attribution edits <=30, totaling exactly 1000 at `skv14-W5A-plan.md:47`-`54`. The narrow parser/source-fact scope at `skv14-W5A-plan.md:40` keeps that ledger credible by excluding full CSS semantic generation.
- ACCEPT: The cap is executable, not advisory. `skv14-W5A-plan.md:96`-`105` defines a `git diff --numstat HEAD -- ...` owner-slice LOC calculation and rejects if `W5A_LOC` exceeds 1000. V1 explicitly required this fold at `HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:14` and records it landed at lines 26-27; V2 CH4 accepted the resulting ledger at `HARDENING-SKV14-W5A-V2-CONSOLIDATED.md:14`.
- ACCEPT: Generated-output accounting is explicit and does not leak into source/test LOC. `SPEC.md:251`-`257` says generated outputs are uncounted only if named, diff-audited, and included in the revert slice; the plan restates the same constraint for `cargo xtask regen-css` output at `skv14-W5A-plan.md:56` and excludes only generated CSS runtime output from non-owner status at `skv14-W5A-plan.md:33`.
- ACCEPT: The hard cap/reject protocol is concrete. `SPEC.md:264`-`274` sets the 75-minute implementation/redress target and 90-minute hard ceiling, and requires split-or-REVISE if it cannot fit. The plan repeats "commit or reject at cap" at `skv14-W5A-plan.md:109` and gives the rejected-patch/revert slice at `skv14-W5A-plan.md:111`.
- ACCEPT: Owner and non-owner file boundaries are bounded. Owner paths are listed at `skv14-W5A-plan.md:20`-`27`; non-owner paths at lines 29-35 exclude CSS provider/template deletion, root `crates/core/src/runtime/css_l4/`, and `grammar/css/l4/`. The provider/template count and staged/unstaged rename gates at `skv14-W5A-plan.md:92`-`95`, plus the root CSS/grammar no-diff gate at line 106, close the budget-boundary escape that V2 folded at `HARDENING-SKV14-W5A-V2-CONSOLIDATED.md:23`.
Required folds:
- NONE
Evidence:
- `restart/skinny/tranches/sk-v14/SPEC.md:242`-`244`, `:251`-`257`, `:264`-`274`, and `:637`-`698`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:20`-`56`, `:84`-`115`, and `:117`-`130`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:14`, `:26`-`27`, `:31`-`32`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V2/HARDENING-SKV14-W5A-V2-CONSOLIDATED.md:14`, `:23`-`25`.
- Inventory check run from repo root: `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` returned 8 and `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` returned 7, matching `skv14-W5A-plan.md:43` and `:92`-`:93`.
- Boundary diff check run from repo root: `git diff --name-status HEAD -- skinny/crates/codegen/src crates/core/src/runtime/css_l4 grammar/css/l4 skinny/crates/runtime/src/grammars` returned no paths.
