# SK-V14 W5B-FRONTEND CHALLENGE V1 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 Hidden Coupling.
Disposition: REVISE.

## Findings

The W5B-FRONTEND shape does not create an obvious sidecar substrate, but its
guards leave two hidden-coupling holes before redress.

1. Hidden provider/template replacement is not fully blocked. The plan forbids
   provider/template add/delete/rename at `skv14-W5B-FRONTEND-plan.md:102` and
   `skv14-W5B-FRONTEND-plan.md:144`, but not `M` edits to existing
   provider/template files. Current guard behavior rejects only `??`, `A`, `D`,
   and `R` at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1264`, and the
   current tests explicitly allow a modified CSS provider at
   `lock14_baseline.rs:2093`. That leaves a hidden replacement path.
2. Live provider dispatch preservation lacks a positive gate. The plan correctly
   forbids `RuntimeProvider`, `GrammarProfile`, and `render_runtime_profile`
   retirement at `skv14-W5B-FRONTEND-plan.md:146`; current code still uses
   those surfaces at `skinny/crates/codegen/src/grammar_provider.rs:1`,
   `grammar_provider.rs:78`, `skinny/crates/codegen/src/lib.rs:180`, and
   `skinny/crates/codegen/src/grammar_profile.rs:17`. The W5B gate list at
   `skv14-W5B-FRONTEND-plan.md:69` has no positive reachability grep despite
   NEW-CH5-V6-01 at `DISPATCH-PROMPT.md:187`.
3. Lock 14 owner-path-first is correctly planned but still pre-redress only.
   Current routing stops at W5A in `lock14_baseline.rs:1105` and
   `lock14_baseline.rs:1611`; the plan requires W5B roster and subject routing
   first at `skv14-W5B-FRONTEND-plan.md:36`, matching `SPEC.md:726` and
   NEW-CH5-V7-01 at `DISPATCH-PROMPT.md:189`.

## Required Fold

- Strengthen W5B topology gates to reject any provider/template path in
  `git status`, `git diff`, `git diff --cached`, or parent diff, including
  modified files, except `grammar_provider.rs`.
- Add a W5B-specific unit test proving modified existing provider/template
  files are rejected under W5B subjects.
- Add positive reachability gates proving `RuntimeProvider`, `GrammarProfile`,
  and `render_runtime_profile(profile, None)` remain live through W5B.
- Add an explicit no-sidecar clause: frontend IR/facts are request-local only,
  not emitted, retained, parser-owned, or runtime-queryable.

## Sources

- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:187`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:189`
- `restart/skinny/tranches/sk-v14/SPEC.md:726`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:36`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:102`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:144`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:146`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1264`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1611`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2093`
- `skinny/crates/codegen/src/grammar_provider.rs:1`
- `skinny/crates/codegen/src/grammar_provider.rs:78`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/codegen/src/grammar_profile.rs:17`
