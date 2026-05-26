# Pass Omega V8 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 hidden coupling.
Disposition: ACCEPT.

## Verdict

ACCEPT. Pass Omega V8 closes the W5B-FRONTEND V2 CH5 failure by moving the
coupled authority work into a Lock14-only W5B.0 gate, keeping W5B frontend
source work behind W5B.0, and keeping W5C-GEN / W5D-DELETE blocked until their
proper predecessors close. No hidden substrate, BIR, BackendShape, lock, or
public-syntax expansion is introduced.

## Audit

| Check | Result | Evidence |
|---|---|---|
| No substrate leak | ACCEPT | Lock 1 rejects parallel substrates, parser-owned sidecars, public substrate APIs, `UnionTape`, second tape, and retained classifier state without G-Omega (`restart/locks/LOCKS.md:75`, `restart/locks/LOCKS.md:118`-`126`, `restart/locks/LOCKS.md:137`-`153`). V8 routes `SUBSTRATE.md` read/no-op and says no substrate amendment follows from REDRESS-212 (`restart/audit/totality/astral/V8/ΩE-skinny-corpus.md:47`). |
| No BIR / BackendShape leak | ACCEPT | Lock 10 keeps the five `BackendShape` variants and G-Omega-gates any new directive, BIR variant, or BackendShape (`restart/locks/LOCKS.md:269`-`280`). V8 locks-diff is zero delta and repeats the five-shape canon (`restart/audit/totality/astral/V8/locks-diff.md:3`-`18`). |
| No public syntax leak | ACCEPT | W5B.1-W5B.3 lower compatibility constructs into request-local facts and explicitly prohibit public syntax revival / new public directive, BIR, BackendShape, or substrate variants (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:53`-`55`). `@ws` remains compatibility lowering, not a public directive revival (`restart/audit/totality/astral/V8/master-plan-diff.md:59`-`61`, `restart/audit/totality/astral/V8/master-plan-diff.md:79`). |
| W5B.0 Lock14-only | ACCEPT | V2 CH5 rejected the old plan because W5B.0 mixed Lock 14 routing with import / `@ws` source work (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:18`-`22`). V8 replaces it with W5B.0 LOCK14-GATE: owner-path roster, parent-diff routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census, with no grammar/codegen/xtask frontend source edits (`restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:52`, `restart/audit/totality/astral/V8/master-plan-diff.md:55`-`57`). |
| Provider/template guard coverage | ACCEPT | V2 CH5 rejected CSS-only guards because topology has eight `*_templates` directories, including `json_templates` (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:9`-`17`). V8 requires all `_templates` paths guarded and modified provider/template files rejected (`restart/audit/totality/astral/V8/ΩC-locks-amendments.md:61`-`66`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:120`-`121`). Local census: `find skinny/crates/codegen/src -type d -name '*_templates'` returns 8. |
| W5C/W5D blocked | ACCEPT | V8 requires W5B-FRONTEND to close only after W5B.0 through W5B.4 admit, keeps W5C-GEN blocked until aggregate W5B close, keeps W5D-DELETE blocked until W5C-GEN, and forbids treating W5B.0 through W5B.3 as W5B close (`restart/audit/totality/astral/V8/ΩB-skinny-lessons.md:51`-`52`, `restart/audit/totality/astral/V8/ΩD-master-plan-reconciliation.md:71`-`73`, `restart/audit/totality/astral/V8/ΩF-migration-handoff.md:55`-`63`). |

## Exact Folds

1. Keep `restart/locks/LOCKS.md` zero-delta. Preserve 16 locks, Lock 1
   substrate prohibitions, Lock 10 five-shape `BackendShape` canon, and Lock 14
   grammar-neutrality / generated-output gate. Do not add public syntax, a BIR
   variant, a BackendShape variant, a substrate surface, or a lock.
2. Apply the V8 MASTER-PLAN / SPEC fold: replace one-shot W5B-FRONTEND with
   W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3
   PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. W5B-FRONTEND closes only
   after all five admit.
3. W5B.0 is Lock14-only: owner-path roster, parent-diff subject routing,
   W5C/W5D subject rejection, modified-provider/template rejection tests,
   all-template guard, and generic owner-path leak census. It may not edit
   grammar/codegen/xtask frontend implementation paths, may not close
   W5B-FRONTEND, and may only unlock W5B.1.
4. Provider/template guards must match all protected provider/template paths:
   `(_provider\.rs|_templates)`, excluding only `grammar_provider.rs` where W5B
   owns request-boundary edits. Add/retain an all-template count gate expecting
   8 `*_templates` directories and explicit modified-file tests covering both
   CSS template paths and `json_templates`.
5. W5C-GEN remains blocked until aggregate W5B-FRONTEND close at W5B.4.
   W5D-DELETE remains blocked until W5C-GEN admits. Provider-free generator-body
   replacement stays W5C-GEN scope; provider/template deletion stays W5D-DELETE
   scope.
