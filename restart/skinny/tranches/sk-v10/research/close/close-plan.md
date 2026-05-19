# SK-V10 Close Plan - Packet Reconciliation

Pass: Wave Plan.
Cycle: Close.
Date: 2026-05-19.
Gate: `G-CLOSE-SK-V10`.
Disposition target: PASS.

## Selected Intervention

Close SK-V10 by reconciling the packet documents and REDRESS ledger after W10.
The close wave is documentation-only. It does not edit behavior source,
generated parser output, SIMD primitives, benchmarks, row dispositions, or the
telemetry schema.

## Owner Paths

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/close/close-redress.md`
- `skinny/REDRESS.md`

Read-only verification:

- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`

`skinny/RESULTS.md` is not expected to change in Close because W10 already
rendered the accepted result surface.

## Tasks

- Mark Close as closed under a new REDRESS entry.
- Add a close redress artifact recording the final W0-W10 disposition table,
  current row counts, run id, gate evidence, and routed remainders.
- Update SPEC, DISPATCH, HANDOFF, and SYNTHESIS so they agree that SK-V10 is
  converged and no further SK-V10 row movement is authorized.
- Preserve W10's current report authority:
  `sk-v9-open:criterion-fnv64-6f007527061ee26d`.
- Route REDRESS 98 to Pass Omega and CSS L4 / Sheets / BBNF-self risk to the
  totality track.

## Falsifiability Gate

`G-CLOSE-SK-V10` passes only if:

- there is no open source patch;
- `RESULTS.md` row dispositions match accepted evidence: 17 parse `S / NO-GO`,
  6 direct `A / GO`, 11 direct `N-direct / NO-GO`, and 7 typed `A / GO`;
- `REDRESS.md` records W0-W10 plus Close;
- `gate-json --with-cost-facts --check-results` passes against the W10
  Criterion root;
- close documents agree on the route to Pass Omega and the totality track;
- no W3, parse-only SOTA, direct-vs-typed relabeling, helper-transfer,
  scalar-parent fold, sidecar, scratch/materialization, or generic JSON policy
  route is reopened.

## CHALLENGE Disposition

CHALLENGE is skipped for Close under `DISPATCH-PROMPT.md`: the plan is
documentation-only, stays inside accepted gate semantics, and avoids first-of-
class source edits, generic crates, codegen, `bbnf-simd`, and
runtime-outside-JSON behavior.

## Evidence Commands

```text
git status --short
```

```text
git diff --check
```

```text
CRITERION_HOME=/tmp/skv10-w10-full-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Revert Protocol

If a contradiction is found, revert only the Close documentation edits and
record the contradiction in a revised close plan. Accepted W0-W10 source,
result, and measurement commits are not reverted by close accounting.
