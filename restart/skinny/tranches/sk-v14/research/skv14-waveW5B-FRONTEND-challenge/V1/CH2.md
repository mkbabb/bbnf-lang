# SK-V14 W5B-FRONTEND CHALLENGE V1 CH2 Generality

Date: 2026-05-26.
Lens: CH2 Generality.
Disposition: ACCEPT.

## Findings

The W5B-FRONTEND plan preserves Lock 14 generality and does not narrow the wave
to a CSS-only path.

1. Lock 14 requires zero grammar-name branches, grammar modules, and public API
   types in generic crates at `restart/locks/LOCKS.md:349` and
   `SPEC.md:225`. The plan pre-blocks grammar-name branches and forbids new
   neutral module paths unless named before Lock 14 owner-path admission at
   `skv14-W5B-FRONTEND-plan.md:30` and `skv14-W5B-FRONTEND-plan.md:148`.
2. SPEC requires Lock 14 owner-path routing before source redress at
   `SPEC.md:726`; the plan names first-step `lock14_baseline.rs` work, exact
   W5B roster, parent-diff subjects, and tests at
   `skv14-W5B-FRONTEND-plan.md:36-46`.
3. Compatibility syntax remains frontend lowering rather than new public BBNF
   syntax. SPEC binds this at `SPEC.md:728`, and the plan lowers `@ws`,
   `@pretty`, `?w`, `>>`, `<<`, span capture, and typed projections into
   request-owned IR while keeping public `@ws` retired at
   `skv14-W5B-FRONTEND-plan.md:49-54` and
   `skv14-W5B-FRONTEND-plan.md:147`.
4. Lock 14 requires CSS positive proof and Sheets/BBNF-self witnesses at
   `restart/locks/LOCKS.md:377-387`. The plan carries CSS companion checks,
   JSON unchanged proof, and Sheets/BBNF-self fail-closed proof at
   `skv14-W5B-FRONTEND-plan.md:69-89` and
   `skv14-W5B-FRONTEND-plan.md:132-140`.
5. W5B remains frontend/import/IR closure. Provider-free generation and
   provider/template deletion remain W5C/W5D work per
   `skv14-W5B-FRONTEND-plan.md:55-60` and
   `skv14-W5B-FRONTEND-plan.md:144-150`.

## Required Fold

None for CH2. Carry these acceptance constraints into V2 and redress: no public
compatibility syntax, no grammar-name branches, no provider/template deletion,
and captured HEAD output for Lock 14, CSS, JSON, and Sheets/BBNF-self proof.

## Sources

- `restart/locks/LOCKS.md:349`
- `restart/locks/LOCKS.md:377`
- `restart/skinny/tranches/sk-v14/SPEC.md:225`
- `restart/skinny/tranches/sk-v14/SPEC.md:726`
- `restart/skinny/tranches/sk-v14/SPEC.md:728`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:30`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:36`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:49`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:132`
