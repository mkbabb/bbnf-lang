# SK-V8 Alpha Hardening V1 Consolidated

Date: 2026-05-17.

Scope: consolidated CH1-CH6 disposition for Pass Alpha SK-V7 -> SK-V8.

## Outcome

Overall V1 challenge disposition: REVISE.

The six alpha artifacts are evidence-complete enough to synthesize SK-V8, but
the alpha-F draft is not dispatchable as written. The final packet resolves the
REVISE findings by making SK-V8 an observability-first tranche:

- W0 is mandatory, telemetry-only, and the only implementation wave eligible
  for dispatch after G-Alpha.
- CostFacts gate binding moves before any behavior wave.
- Behavior waves after W0 are conditional on exact post-W0 owner paths, row
  thresholds, and challenge acceptance.
- PMULL prefix-XOR and CSSC CTZ/bulk remain rejected as production defaults.
  They are not SK-V8 default waves.
- Lock 14 becomes a per-wave gate, not a late cleanup theme.
- Direct digest rows remain guard rows and cannot be counted as typed product
  SOTA proof.

## Lens Dispositions

| Lens | V1 disposition | Blocking finding | Final-packet resolution |
|---|---|---|---|
| CH1 Correctness | REVISE | Alpha-F left exact row targets, outcome enum handling, and profile claims open. | `SPEC.md` defines the SK-V8 outcome enum, comparator classes, W0 telemetry target for every current row, and profile-conditional behavior gates. |
| CH2 Generality | REVISE | JSON evidence risked becoming generic architecture policy. | `SYNTHESIS.md` and `SPEC.md` state that JSON rows are the opening benchmark surface only; generic crates must use grammar-neutral facts or generated per-grammar output. |
| CH3 Regression | REVISE | W0 was not mandatory enough; pre-block reopen rules and full-table maintain gates needed hard wording. | `SPEC.md` makes W0 produce `SK-V8-open`, applies full-table maintain budgets, and makes reopen rules executable. |
| CH4 Cost | REVISE | Candidate waves were too broad, and bitmap asm was too permissive. | `SPEC.md` narrows W0/W1, makes W2-W4 conditional, and demotes bitmap asm to a reserve research route. |
| CH5 Hidden Coupling | REVISE | Sidecar producers, Track 1/2 honesty, and E4 bitmap framing were not tight enough. | `SPEC.md` requires every telemetry field to be consumed by the gate, every typed Track 2 oracle to be structurally independent, and no orphan primitive source. |
| CH6 Next-Tranche Impact | REVISE | The draft lacked a per-wave revert matrix and dispatchable hard-cap table. | `SPEC.md` carries phase caps, revert protocols, downstream effects, and no-dispatch-before-G-Alpha posture. |

## Required Revisions Applied

1. Citation discipline: final docs cite `skinny/RESULTS.md`, REDRESS items
   77-90, and the SK-V7 commit chain for route claims.
2. Outcome enum: SK-V8 explicitly extends the current schema outcome enum to
   include `K` and `N-direct`, because current RESULTS rows use both.
3. Comparator classes: final docs distinguish same-run strict anchors,
   same-run flaw probes, and sidecar planning signals.
4. W0 mandate: W0 creates the `SK-V8-open` baseline and blocks all behavior
   waves if any required telemetry field is unvalidated.
5. Full-table maintain: every wave carries a full-table no-regression guard
   unless its SPEC section names a stricter row set.
6. CostFacts ordering: CostFacts binding is W1, before typed, parse, or direct
   behavior waves.
7. Bitmap route: the alpha-E bitmap candidate is rejected as written. It can
   return only after W0/W1 profile evidence names bitmap work as a hot owner and
   a fresh challenge accepts the changed framing.
8. Lock 14: generic-crate JSON policy remains blocked; generated JSON output
   and grammar input files are the allowed JSON-specific surfaces.
9. Product-plane honesty: direct digest rows are guard rows. Typed wins require
   generated Track 1 plus a structurally independent oracle or Track 2.
10. G-Alpha posture: no SK-V8 wave dispatch before user sign-off. After
    sign-off, only W0 is dispatchable from this packet.

## Remaining Gate

This consolidated V1 packet is suitable for G-Alpha review only after the final
SK-V8 tranche documents land:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`

The G-Alpha decision is binary:

- `G-Alpha closed`: dispatch SK-V8 W0 only.
- `G-Alpha revise`: return to Alpha hardening with named revisions.

W1-W6 remain non-dispatchable until W0 closes and their post-W0 owner paths and
row gates are plan-updated.
