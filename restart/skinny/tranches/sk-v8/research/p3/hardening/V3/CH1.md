# SK-V8 S-P3 Hardening V3 CH1: Correctness Challenge

Scope: CH1 correctness review of the live S-P3 packet after
`restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md`, with
emphasis on whether V2 CH1's citation fold was actually resolved while W2
dispatch bounds and the G-Alpha lock remained intact.

## Verdict

REVISE. Confidence: 88%.

V3 preserved the substantive dispatch gates, W2 seed bounds, concrete path
resolution, and G-Alpha implementation lock. It did not fully satisfy the V2
CH1 citation requirement because multiple material P3 claims still cite broad
document-wide label bundles instead of current file:line references or exact
stable target sections.

## Blockers

1. Broad stable-label bundles do not satisfy CH1 claim traceability.

   - Governing rule: `restart/prompts/ORCHESTRATOR.md:81-84` defines CH1 as
     requiring every claim to cite a resolving file:line, commit SHA, RESULTS
     row, or REDRESS entry. The S-P3 CH1 specialization requires candidate
     traceability and measurable gates at `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110-115`.
   - V2 required fold: `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:35-37`
     requires replacing material bare-path citations with current file:line
     citations or stable section references naming the exact target section.
   - V3 fold mechanism: `restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:20-35`
     defines label classes, but the `SPEC` class spans Sections 0.1-0.5, 2,
     and 3-11, and the `HANDOFF` class spans Sections 2, 3a, and 4-10. Those
     labels resolve to documents, not exact target sections.
   - Remaining examples:
     `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:24`
     uses the broad SPEC label for W0 current-state, telemetry, same-wave
     consumer, and reject-signal claims.
     `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:26`
     uses the same broad SPEC label for W2 row selection, gates, and path
     constraints.
     `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:27`
     uses it again for W3 entry, gate, and substrate-boundary claims.
     `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:72-78`
     uses broad SPEC labels across wave gate rows.
     `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:57-115`
     uses broad SPEC/HANDOFF labels for telemetry field and W0 gate bindings
     where the exact source sections are knowable.

## Evidence

- Concrete local paths resolve. I found no missing backticked concrete paths
  under `restart/`, `skinny/`, `docs/`, or `audit-specs/` in the live V3
  packet.
- Future-artifact globs are resolved. The remaining future W0 references are
  naming patterns, not unresolved globs: `restart/skinny/tranches/sk-v8/SPEC.md:331-335`
  and `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:62-64` specify
  `wave-0-<topic>.md` under a concrete directory; I found no live
  `wave-0-*.md` source citation.
- W2 seed table and dispatch bounds remain intact:
  `restart/skinny/tranches/sk-v8/SPEC.md:177-189` retains the five-row W2
  seed table and strict planning floors; `restart/skinny/tranches/sk-v8/SPEC.md:460-468`
  binds W2 to that table unless a later accepted S-P3 revision expands it;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:117-125` preserves the
  same dispatch bound.
- The G-Alpha lock remains intact:
  `restart/skinny/tranches/sk-v8/SPEC.md:814-825` states no implementation
  before G-Alpha and only W0 after G-Alpha closes; `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`
  and `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:216-222` preserve the
  same lock; `restart/skinny/tranches/sk-v8/HANDOFF.md:5-7` agrees.
- W0/W1/W2 dispatch posture remains consistent with the live handoff:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:119-137` requires G-Alpha closure,
  W0 admission, exact owner paths, row gates, pre-blocked routes, revert
  protocol, and same-wave consumer before W1-W6 entry.

## Required Fold

Replace broad label-bundle citations on material claims with exact stable
section references or current file:line references. At minimum:

1. Replace `SPEC Sections 0.1-0.5, 2, and 3-11` usages with exact targets such
   as `SPEC Section 0.2 Comparator Classes`, `SPEC Section 0.4 Required
   Telemetry`, `SPEC Section 0.5 Opening Row Goalset`, `SPEC Section 3 W0`,
   `SPEC Section 4 W1`, `SPEC Section 5 W2`, `SPEC Section 6 W3`, `SPEC
   Section 10 Pre-Blocked Routes`, or `SPEC Section 11 G-Alpha`, as applicable.
2. Replace `HANDOFF Sections 2, 3a, and 4-10` usages with the exact named
   section that supports the claim, or with current file:line when the claim
   depends on live dispatch status.
3. For RESULTS/REDRESS evidence, name the specific row, REDRESS entry, or line
   span whenever a claim depends on measured state, a blocked route, or a
   threshold.
4. Preserve the already-good V3 properties: concrete paths resolve, future W0
   naming patterns are not cited as existing artefacts, W2 remains limited to
   the Section 0.5 seed table unless a later accepted S-P3 revision expands it,
   and no implementation dispatch occurs before G-Alpha.
