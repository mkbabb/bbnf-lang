# SK-V14 W5A CHALLENGE V4 CH1: Measurability
Disposition: ACCEPT
Acceptance score: 97/100
Findings:
- ACCEPT: The W5A plan remains source-cited and measurable. It binds to SPEC Section 8 and names supporting research inputs for the source contract, parser constructs, CSS companion gates, JSON equivalence, Sheets/BBNF witnesses, and Lock 14 guard/budget coverage.
- ACCEPT: The gates are executable rather than vague. The plan names exact cargo tests, pairs each with a nonzero pass assertion, and includes concrete `check-json`, `regen-css`, seven `check-css-l4-*`, `gate-json`, and `git diff` maintain commands.
- ACCEPT: Source-consuming proof is checked at the call boundary. The plan requires `regen-css` and every CSS companion to use the new request path and includes a fail-closed grep rejecting `emit_runtime_profile(target.profile)` in `regen.rs`.
- ACCEPT: Provider/template coverage includes both current tree shape and staged/unstaged mutation checks. The plan fixes the provider count at `8`, template directory count at `7`, and rejects staged or unstaged A/D/R changes touching provider files or template directories.
- ACCEPT: Full-table maintain is exact no-diff proof, not a freshness proxy. W5A requires `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` to remain byte-identical to `HEAD`, while `gate-json --check-results --skv14-existing-results-capture` remains companion shape/freshness evidence only.
- ACCEPT: LOC and budget proof remains mechanically testable. The component ledger totals 1000 LOC, the executable `git diff --numstat` gate scopes W5A owner paths, and the hard cap remains `test "$W5A_LOC" -le 1000` with no W5B/W6 borrowing.
- ACCEPT: No V3 regression found. V3 CH1 already accepted the same measurable gate set, V3 consolidated recorded 7/7 ACCEPT with zero orphan REVISEs, and V4 review found no missing CH1 fold.
Required folds:
- NONE
Evidence:
- `restart/skinny/tranches/sk-v14/SPEC.md:637` defines W5A as the source-consuming runtime generator contract; `restart/skinny/tranches/sk-v14/SPEC.md:654`-`680` defines entry, tasks, exit gates, full-table maintain, and the <=1.0k cap.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:10`-`16` lists the source citations the plan consumes.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`45` states the falsifiable request-path, parser/source-fact, JSON unchanged-output, Sheets/BBNF fail-closed, provider/template count, exact full-table no-diff, and LOC-cap gates.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:47`-`56` provides the component LOC ledger and pre-redress stop condition.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:60`-`81` names exact tests, nonzero pass assertions, `check-json`, `regen-css`, all seven `check-css-l4-*` companions, `gate-json --check-results --skv14-existing-results-capture`, and `git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:87`-`106` provides fail-closed grep/count gates, staged and unstaged provider/template A/D/R checks, executable LOC delta calculation, and forbidden core CSS/grammar source diff checks.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:111`-`115` defines rejected-patch escrow, revert slice, same-wave consumers, and downstream routing.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11` and `:23`-`:32` identify the original CH1 measurability gaps and their first fold.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V2/HARDENING-SKV14-W5A-V2-CONSOLIDATED.md:11` and `:23`-`:25` identify the remaining staged rename and full-table maintain gaps and their fold.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V3/CH1.md:5`-`:10` accepts the measurable gate set, including exact tests, request-path grep, staged/unstaged provider/template coverage, exact full-table no-diff maintain, and executable LOC ledger.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V3/HARDENING-SKV14-W5A-V3-CONSOLIDATED.md:6`-`:7` records V3 as ACCEPT with zero orphan REVISEs; `:23`-`:25` records the measurable, executable plan and requires one additional clean challenge cycle.
- Read-only CH1 confirmation commands: `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '` returned `8`; `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '` returned `7`; unstaged and staged provider/template A/D/R greps returned no matches; `git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md` exited `0`.
