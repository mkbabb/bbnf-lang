# Pass Omega V8 CH5 Hidden Coupling After CH1 Fold

Date: 2026-05-26.
Lens: CH5 hidden coupling.
Disposition: ACCEPT.

## Verdict

ACCEPT. After the CH1 exactness fold, Pass Omega V8 still closes the V2 CH5
hidden-coupling challenge. The fold adds exact test/table/nonzero/LOC proof
requirements; it does not add a substrate surface, BIR variant, BackendShape
variant, public syntax, provider/template mutation path, or early W5C/W5D
unlock.

## Reviewed Inputs

- Omega-A through Omega-F V8.
- `restart/audit/totality/astral/V8/locks-diff.md`.
- `restart/audit/totality/astral/V8/master-plan-diff.md`.
- Prior CH5 at `restart/audit/totality/astral/V8/hardening/CH5.md`.
- Lock 1, Lock 10, and Lock 14 in `restart/locks/LOCKS.md`.
- V2 CH5 source challenge at
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md`.

## Audit Matrix

| Check | Result | Evidence |
|---|---:|---|
| CH1 fold does not reopen CH5 | ACCEPT | CH1 is now folded into the proposed SPEC/master authority: construct rows require owner file/type, target representation, exact positive test, and exact fail-closed test (`restart/audit/totality/astral/V8/master-plan-diff.md:68`-`100`; `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:58`-`95`). Those requirements narrow proof; they do not authorize new implementation surfaces. |
| No substrate leak | ACCEPT | Lock 1 rejects hidden runtime identity, parser-owned sidecars, public substrate APIs, `UnionTape`, second tape, and retained classifier state without G-Omega (`restart/locks/LOCKS.md:75`-`126`, `restart/locks/LOCKS.md:137`-`154`). Omega-E keeps `SUBSTRATE.md` read/no-op and says no substrate, FactStream, SIMD/ASM, or BackendShape amendment follows from REDRESS-212 (`restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:47`, `restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:204`-`206`). |
| No BIR or BackendShape leak | ACCEPT | Lock 10 keeps `BackendShape` as a side-table search-domain fact and G-Omega-gates any new `BackendShape`, directive, or BIR variant (`restart/locks/LOCKS.md:269`-`280`). The V8 locks diff is zero delta and states no public syntax, BIR variant, BackendShape variant, substrate surface, or lock is added (`restart/audit/totality/astral/V8/locks-diff.md:3`-`26`). |
| No public syntax leak | ACCEPT | W5B.2 lowers `@ws`, `?w`, `>>`, and `<<` into request-local facts while public syntax remains retired (`restart/audit/totality/astral/V8/master-plan-diff.md:58`-`63`). W5B.3 explicitly forbids any new public directive, BIR, BackendShape, or substrate variant (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:55`, `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:151`-`155`). |
| W5B.0 is Lock14-only | ACCEPT | The original V2 CH5 failure was that W5B.0 coupled Lock 14 routing to import/`@ws` source work (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:18`-`22`). V8 now makes W5B.0 a LOCK14-GATE only: owner-path roster, parent-diff routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census, with no grammar/codegen/xtask frontend source edits (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:52`; `restart/audit/totality/astral/V8/master-plan-diff.md:55`-`57`). Omega-F further blocks frontend source edits until W5B.0 is executable and admitted (`restart/audit/totality/astral/V8/ΩF-migration-handoff.md:121`-`124`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:132`-`135`). |
| All-template guards | ACCEPT | V2 CH5 rejected CSS-only template guards because the topology has eight `*_templates` directories including `json_templates` (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:9`-`17`). V8 requires modified-provider/template rejection, all `_templates` guards, and exact W5B.0 tests including `w5b_lock14_frontend_rejects_modified_template` and `w5b_lock14_frontend_all_templates_guard_counts_8` (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:52`, `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:76`-`85`; `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:55`-`64`). Local census: `find skinny/crates/codegen/src -type d -name '*_templates' | wc -l` returns `8`. |
| W5C/W5D remain blocked | ACCEPT | The amended graph keeps W5C-GEN blocked until aggregate W5B-FRONTEND close and W5D-DELETE blocked until W5C-GEN close (`restart/audit/totality/astral/V8/master-plan-diff.md:25`-`43`). W5B.0 through W5B.3 are not close points, and W5B closes only at W5B.4 with same-commit consumer evidence (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:107`-`112`; `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:91`-`99`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:137`-`148`). |

## Required Carry

1. Keep `restart/locks/LOCKS.md` zero-delta. Local lock-count check returns
   `16`, matching `restart/audit/totality/astral/V8/locks-diff.md:11`-`17`.
2. Preserve W5B.0 as Lock14-only. W5B.0 may not edit grammar/codegen/xtask
   frontend implementation paths, may not close W5B-FRONTEND, and may only
   unlock W5B.1 after its own admit.
3. Preserve the all-template guard as a topology-wide guard over every
   `*_templates` directory, not CSS-only paths, and keep explicit modified
   provider/template rejection tests.
4. Preserve W5C-GEN and W5D-DELETE blocks. Provider-free generator-body
   replacement remains W5C-GEN scope; provider/template deletion remains
   W5D-DELETE scope.

Final: ACCEPT.
