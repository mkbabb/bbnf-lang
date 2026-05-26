# SK-V14 W5B-FRONTEND Redress: Sub-Wave Authority Rejection

Date: 2026-05-26.
Wave: W5B-FRONTEND PRUNE-3B.
Disposition: REDRESS-212 closes as REJECTED.

## Decision

W5B-FRONTEND cannot admit under the current SPEC shape. V7 correctly routed the
campaign to generic BBNF frontend/import/IR closure before W5C-GEN, but the
current W5B-FRONTEND wave is still authorized as one capped wave. The V2 plan
requires serial sub-slices to satisfy the dispatch-hard-cap discipline and
avoid deferring part of the frontend closure while W5C-GEN remains blocked.

No frontend/codegen/xtask source redress was attempted or retained for
W5B-FRONTEND. The closure is documentation-only: record the rejection, preserve
the W5A admitted request boundary, and route the sub-wave authority correction
through Pass Omega V8.

## Challenge Evidence

The W5B-FRONTEND V2 challenge did not converge:

- Acceptance was 2/7 lenses ACCEPT; CH1, CH2, CH4, CH5, and CH6 remained
  REVISE.
- CH4 found that four 30-minute internal slices plus final verification do not
  fit the single W5B wave cap.
- CH6 found that the plan replaced SPEC's +/-1.0% full-table maintain gate with
  exact no-diff without SPEC authority.
- The consolidated V2 packet names the required governance route: formal
  sub-waves through SPEC/Omega, or a narrowed one-slice redress that cannot
  claim W5B-FRONTEND closure and therefore cannot unblock W5C-GEN.

The challenge archive is:
`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/`.

## Proof Bundle

Commands run from repository root unless noted.

```sh
git diff --exit-code HEAD -- \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/crates/grammar/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs \
  skinny/crates/codegen/src/lib.rs \
  skinny/xtask/src/main.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs
```

Result: clean. No W5B-FRONTEND source owner path changed.

```sh
git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
```

Result: clean. No admit ledger or rolling-delta state was altered by the
rejection close.

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l | tr -d ' '
```

Result: `16` locks and `67` Pattern H files.

## Corrective Route

Pass Omega V8 is required because REDRESS-212 materially changes the SK-V14
wave graph and SPEC cap accounting. The proposed amendment is:

| Wave | Scope |
|---|---|
| W5B.0 LOCK14-GATE | W5B-FRONTEND owner-path roster, parent-diff routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census; no frontend source edits. |
| W5B.1 IMPORT-CLOSURE | Request-local import graph resolution and missing-import/import-cycle fail-closed gates. |
| W5B.2 LAYOUT-DISCARD | Request-local lowering for `@ws`, `?w`, `>>`, and `<<`, with public syntax still retired. |
| W5B.3 PRETTY-SPAN-PROJECTION | Request-local lowering for `@pretty`, `@{...}` span capture, projection metadata, and typed projections. |
| W5B.4 REQUEST-CONSUMER | `emit_runtime_from_request` consumes the frontend closure; JSON/Sheets/BBNF proof carry, `regen-css`, seven CSS companions, provider/template topology, and exact maintain evidence close W5B-FRONTEND. |

W5B-FRONTEND closes only after W5B.0 through W5B.4 admit. W5C-GEN remains
blocked until that aggregate close; W5D-DELETE remains blocked until W5C-GEN;
W6/W7 and new-admit waves remain blocked by the PRUNE chain.
