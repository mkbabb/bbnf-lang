# SK-V8 Alpha Hardening V2 Consolidated

Date: 2026-05-17.

Scope: V2 challenge close for the final SK-V8 Pass Alpha packet.

## Outcome

V2 disposition after final document revisions: ACCEPT for G-Alpha presentation
with W0-only dispatch.

The first V2 lens return had three residual REVISE items:

- CH2 required concrete per-wave Lock 14/generalization gates, grammar-aware
  comparator telemetry fields, non-JSON proof gates, REDRESS 36-38 coverage,
  and Omega no-weakening wording.
- CH3 required W2 to inherit a full-table maintain gate against `SK-V8-open`.
- CH4 required source LOC caps, inclusive hard-cap accounting, verification
  allowances, rerun ceilings, sidecar-freshness cost handling, and generated /
  RESULTS review accounting.

Those revisions are now applied in the final packet:

- `SPEC.md` Section 0.4 adds `grammar_id`, `domain`, `comparator_id`,
  `comparator_plane`, and `comparator_strictness`; it states that `gate-json`
  is the JSON instance of a grammar-aware report contract.
- `SPEC.md` Section 2 defines inclusive hard caps, source LOC caps,
  verification/rerun ceilings, and generated / RESULTS review cost.
- `SPEC.md` Section 2.1 adds the concrete Generality and Lock 14 gate:
  public API scan, grammar branch scan, primitive/table scan,
  template/provider boundary, and non-JSON proof for CSS L4, Sheets, and
  BBNF-self.
- `SPEC.md` W0 requires a Lock 14 baseline allowlist and explicit missing
  sidecar non-admission values.
- `SPEC.md` W1 binds grammar-aware comparator/report fields and non-JSON proof.
- `SPEC.md` W2 now requires current typed GO rows, current direct GO rows, and
  all 38 current main rows to maintain against `SK-V8-open`; non-target rows
  outside budget reject W2 with REDRESS.
- `SPEC.md` W3/W4 require generic-code edits to pass Section 2.1.
- `SPEC.md` W5 audits REDRESS 36, 37, and 38 plus CSS L4 / Sheets /
  BBNF-self implications.
- `SYNTHESIS.md`, `HANDOFF.md`, and `DISPATCH-PROMPT.md` now carry the
  no-weakening Omega and generality gate posture.

## Lens Summary

| Lens | V2 raw result | Post-revision disposition |
|---|---|---|
| CH1 Correctness | ACCEPT | ACCEPT |
| CH2 Generality | REVISE | ACCEPT after Section 0.4, Section 2.1, W0/W1/W5, and Omega wording revisions |
| CH3 Regression | REVISE | ACCEPT after W2 full-table maintain gate revision |
| CH4 Cost | REVISE | ACCEPT after source LOC, inclusive cap, verification, rerun, and review-cost revisions |
| CH5 Hidden Coupling | ACCEPT | ACCEPT |
| CH6 Next-Tranche Impact | ACCEPT | ACCEPT |

## G-Alpha Gate

This packet is ready to present for G-Alpha under the limited scope stated in
`HANDOFF.md` and `DISPATCH-PROMPT.md`:

- `G-Alpha closed` authorizes SK-V8 W0 only.
- W1-W6 remain conditional until W0 closes and their post-W0 plans name exact
  owner paths, row gates, pre-blocked routes, same-wave consumers, and
  verification budgets.
- `G-Alpha revise` returns to Alpha hardening with named revisions.
