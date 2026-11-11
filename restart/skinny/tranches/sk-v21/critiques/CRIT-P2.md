# CRIT-P2 — Fused parse+value default lane (the 0.99× line)

Date: 2026-07-05. Item: P2. Convergence: 43/100.

## The central finding: the prototype was not delivered

`prototypes/PROTO-P2.md` does not exist. The only prototype report present on disk is
`prototypes/PROTO-P7.md`. P2 is the instrument the spec names as the answer to its own open
question — SPEC-DRAFT-P1.md:376-377: "Whether the fused lane's 0.99× survives cold-per-parse
discipline (it was measured under the old harness regime) — P2 answers." No P2 report means that
question is answered nowhere. The prototype's evidence quality cannot be judged because there is no
evidence; the method-honesty, loadavg-labeling, and verdict-follows tests are all vacuous.

This is not a minor gap. The spec elevates this exact lever to "the highest-ROI unfinished lever in
the corpus" (SPEC-DRAFT-P1.md:77-78), echoing the dig's own "highest-ROI unfinished lever in the
entire arc" (09:302-303). The one prototype tasked to de-risk the single highest-ROI item did not
run. P1 and P2 were to share the cold `json_canon_bench` harness, "built first in whichever runs
first" (SPEC-DRAFT-P1.md:371); with P2 absent that harness exists only if P1 built it, and no
evidence here shows it did.

## The number the spec rests on is warm, old-harness, and a proxy lane

Even setting the missing prototype aside, the 0.99× figure in SPEC-DRAFT-P1.md:76 is weaker than
the headline word "proven" (SPEC-DRAFT-P1.md:75) admits, on three independent counts:

1. **Warm and old-harness.** The figure originates from tranche AY's `AUDIT-D:70-85` (09:169-179).
   JSON has no cold-per-parse harness at all: the committed 8349 Mbps vs the same binary's 25,448
   warm is a ~3× harness gap (21:67-77), and 32 U1 records JSON measured WARM at loadavg 10.66 with
   no samply flame — "every cycles/byte split is inferred… DIRECTIONAL" (32:170-178). The 0.99×
   inherits that regime. The spec's own §0:12-13 concedes every ranking is DIRECTIONAL until the TW0
   quiet re-capture; the P2 question is precisely the conversion warm→cold, and it did not happen.

2. **A proxy lane, not the proposed lane.** The 0.99× is the emitted **visitor** lane (09:171-179).
   The fused parse+value lane the spec makes the default (SPEC-DRAFT-P1.md:76-77) was **never
   built** — "never proven at the JSON close gate" (09:301-303). The spec transfers the visitor
   analog's number onto an unbuilt construct; that is an inference the missing prototype was meant
   to test, not a measurement.

3. **The geomean masks fixture losses.** 0.99× is a 5-fixture geomean that loses on twitter (1.12×)
   and canada (1.03×) and beats only data/citm/data_xl (09:173-177). The spec's parenthetical
   "(beats on data/citm/data_xl)" is honest, but a per-fixture cold success criterion — not an
   aggregate that hides two losses — is what an AU-relative floor claim requires.

## Does §2 over-claim? Contained, but the headline verb outruns the evidence

The spec is internally honest in its hedges: §2:95-101 discloses the harness gap and mandates
`json_canon_bench` before any JSON figure is re-quoted; §13.2 flags the open question by name. Those
guardrails keep the draft from lying. What over-reaches is local: "The BEAT-sonic lever is proven"
(SPEC-DRAFT-P1.md:75) applied to the fused lane, when only a warm proxy analog is measured and the
fused lane is unbuilt. "Directionally proven on a warm proxy lane" is the honest phrasing until P2
runs cold. The lever's *direction* is strongly corroborated externally — simdjson stage-2 /
materialization dominance (22 §2, cited SPEC-DRAFT-P1.md:90-94) and sonic-rs's primary-representation
model both point the same way — so the plan is not misdirected. It is under-evidenced at the exact
seam P2 was to close.

## What another loop pass must produce

- Build `json_canon_bench` (N≥50 cold-per-parse, mirror of `css_canon_bench`) FIRST, as §2:95-101
  already requires; it does not yet exist even in prototype.
- Measure the **actual fused lane**, not the visitor analog, cold, per-fixture, loadavg-labeled,
  with an interactive samply flame (32:216 U1 checklist).
- State success as the fused lane meeting the AU-relative floor per fixture — twitter and canada
  included — not as a geomean that absorbs their losses.
- Re-word SPEC-DRAFT-P1.md:75 from "proven" to a directional claim until that capture lands.

Convergence is 43: the direction is sound and honestly hedged, but the de-risking prototype did not
run, the supporting number is warm + old-harness + a different lane, and the cold JSON harness the
whole ranking depends on is absent. This item requires another full loop pass.
