# SK-V14 W5A CHALLENGE V3 CH1: Measurability
Disposition: ACCEPT
Acceptance score: 96/100
Findings:
- ACCEPT: W5A now has source-backed scope and citations instead of an uncited implementation sketch. The plan cites the SPEC binding and six supporting W5A research inputs for source contract, parser constructs, CSS companion gates, JSON equivalence, Sheets/BBNF witnesses, and Lock 14 budget/guard coverage.
- ACCEPT: pass/fail criteria are executable and non-vague. Exact named cargo tests are paired with nonzero "test result: ok" log assertions, so broad zero-match filters cannot paper-close the gate.
- ACCEPT: source-consuming evidence is measurable at the call boundary. The plan requires `regen-css` and every `check-css-l4-*` companion to use the request path and adds a fail-closed grep gate rejecting `emit_runtime_profile(target.profile)` in `regen.rs`.
- ACCEPT: V2 provider/template staged rename coverage is folded. The plan has fixed count gates for provider modules and CSS template directories, plus both unstaged `git diff --name-status HEAD` and staged `git diff --cached --name-status` checks rejecting A/D/R provider or template changes.
- ACCEPT: V2 full-table maintain coverage is folded. W5A now requires exact no-diff proof for `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md`; `gate-json --check-results --skv14-existing-results-capture` is retained only as companion shape/freshness evidence.
- ACCEPT: LOC and budget measurability are concrete. The plan has a component ledger totaling 1000 LOC, a `git diff --numstat` source/test delta command over W5A owner paths, and a hard `test "$W5A_LOC" -le 1000` gate.
Required folds:
- NONE
Evidence:
- `restart/skinny/tranches/sk-v14/SPEC.md:637` defines W5A as the source-consuming runtime generator contract; `restart/skinny/tranches/sk-v14/SPEC.md:654`-`680` defines entry, tasks, exit gates, full-table maintain, and the <=1.0k cap.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:10`-`16` lists the source citations the plan consumes.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`45` states falsifiable gates for request-path routing, parser/source facts, JSON unchanged output, Sheets/BBNF fail-closed witnesses, provider/template counts, full-table no-diff maintain, and LOC cap.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:47`-`56` provides the component LOC ledger and pre-redress stop condition.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:60`-`81` names exact cargo tests, nonzero pass assertions, `check-json`, `regen-css`, all seven `check-css-l4-*` companions, `gate-json --check-results --skv14-existing-results-capture`, and `git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:87`-`106` provides fail-closed grep/count gates, staged and unstaged provider/template A/D/R checks, executable LOC delta calculation, and forbidden core CSS/grammar source diff checks.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11` and `:23`-`:32` identify and fold the original CH1 measurability gaps.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V2/HARDENING-SKV14-W5A-V2-CONSOLIDATED.md:11` and `:23`-`:25` identify and fold the remaining staged rename and full-table maintain gaps.
- Read-only checks run for CH1 context: `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` returned `8`; `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` returned `7`; `git status --short` showed unrelated pre-existing dirty files outside the requested V3 artifact path.
