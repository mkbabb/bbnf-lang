# SK-V13 S-P3 V3 CH5: Hidden Coupling

Commit under review: HEAD `eb8051016`, with S-P3 packet folded at `9f8bbfce5`
and V2 accepted at `b5f58b755`.

Verdict: ACCEPT.

Lens: no new directive/BIR/BackendShape/public substrate authority, no
parser-owned sidecar, no SPEC-local weakening of G-Omega or Lock 14, and no
hidden coupling between decision-engine, union, SIMD, or CSS waves.

## Evidence

- Authority is unchanged: ORCHESTRATOR CH5 rejects parallel substrates,
  sidecar producers, Lock 1 scanner renames, and Track 1 / Track 2 dishonesty
  (`restart/prompts/ORCHESTRATOR.md:81`-`:88`), while §3Z still requires two
  consecutive accepted cycles or a user pin (`restart/prompts/ORCHESTRATOR.md:104`-`:123`).
- S-P3 CH5 has the same substrate-union requirement and explicitly forbids
  parser-owned projections, retained cursors, aux tables, and sidecar event
  vectors (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134`-`:138`).
- SPEC keeps the authority narrow: no new directives, BIR variants,
  `BackendShape`, public substrate API, `UnionTape`, parallel substrate,
  parser-owned cursor, aux table, retained class vector, sidecar vector, or
  second source scanner (`restart/skinny/tranches/sk-v13/SPEC.md:285`-`:306`).
- G-Omega is not weakened locally. SPEC blocks W0/source/generated/gate/RESULTS/
  REDRESS work until G-Omega closes and S-P3 converges (`restart/skinny/tranches/sk-v13/SPEC.md:32`-`:43`,
  `:988`-`:1004`); DISPATCH repeats the same global block
  (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:36`-`:47`).
- Lock 14 remains stricter than the V1 defect: fleet-wide claims require CSS L4
  plus both Sheets and BBNF-self witnesses; one non-CSS witness is only scoped
  evidence (`restart/skinny/tranches/sk-v13/SPEC.md:370`-`:390`;
  `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:133`-`:146`).
- Decision, union, SIMD, and CSS waves are separated by row-consumed gates.
  Decision waves reject support-only facts and silent cascade fallback
  (`restart/skinny/tranches/sk-v13/SPEC.md:570`-`:677`); union must stay
  one-substrate with no sidecar/`UnionTape` and a same-wave row consumer
  (`restart/skinny/tranches/sk-v13/SPEC.md:715`-`:757`); CSS subwaves require
  generated feature-row consumers and serialized RESULTS/REDRESS writes
  (`restart/skinny/tranches/sk-v13/SPEC.md:759`-`:784`); SIMD wiring requires
  scalar/checkasm/corpus parity, same-wave consumer, and zero orphans
  (`restart/skinny/tranches/sk-v13/SPEC.md:822`-`:854`).
- The later HEAD-only Omega V5 packet does not create a bypass. It states the
  Omega packet is converged for presentation, but SK-V13 W0/source/generated/
  gate/RESULTS/REDRESS work remains blocked until both G-Omega closes and S-P3
  converges, and it is not user sign-off (`restart/audit/totality/astral/V1/hardening/HARDENING-OMEGA-V5-CONSOLIDATED.md:19`-`:23`,
  `:45`-`:52`).

## Required Fold Items

None. V3 CH5 finds no open hidden-coupling fold item.

## Verification

- `git diff --name-status b5f58b755..HEAD -- restart/skinny/tranches/sk-v13`
  produced no output, so HEAD did not alter the SK-V13 S-P3 packet after the V2
  accepted cycle.
- `git diff --name-status b5f58b755..HEAD` showed only Omega V5 hardening
  artifacts.
- `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V3/CH5.md`
  passed with no output.
