# SK-V13 S-P3 V1 Hardening Consolidated

Pass: S-P3 Synthesis-Plan.
Cycle: V1 CHALLENGE.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 verdict for the SK-V13 S-P3 V1 packet.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`.

## Verdict

`G-S-P3-V1-CHALLENGE`: REVISE.

Acceptance rate: 1/6 = 16.7%.
Critical defects: 0.
Open REVISE dispositions: CH1, CH2, CH3, CH4, CH6.

S-P3 V1 is a valid draft, but it cannot converge or dispatch W0. CH5 accepts
the hidden-coupling posture. The other five lenses require a V2 fold because
the draft SPEC/DISPATCH still lag the current P3-B through P3-E artifacts,
carry a weaker Lock 14 witness-cardinality rule than Omega V2, compress the
P3-E REDRESS ledger, split the wave cost model across incompatible manifests,
and leave support-only/unnamed-consumer paper-close surfaces in W5-W8 and row
subwaves.

## Lens Summary

| Lens | Disposition | Load-bearing finding | Blocks S-P3 |
|---|---|---|---|
| CH1 correctness | REVISE | P3-B through P3-E now exist, but P3-F, SPEC, and DISPATCH still say they were absent; P3A-0 should be W0 governance substrate, not an S-P2 intervention candidate; one W1 declaration-values maintain gate must use the P3-C `SK-V13-open` formula. | yes |
| CH2 generality / Lock 14 | REVISE | Fleet-wide grammar-neutral claims need CSS L4 plus both Sheets and BBNF-self fail-closed/generated-role witnesses. SPEC, DISPATCH, and P3-C still allow CSS plus only one of Sheets or BBNF-self. | yes |
| CH3 regression / REDRESS | REVISE | P3-E passes as a route ledger, but SPEC/DISPATCH do not fold its per-wave route-state matrix and still compress pre-blocks through P3-F. | yes |
| CH4 cost | REVISE | P3-B uses a packed W0-W11 bracket while SPEC/DISPATCH expand to W0-W15 plus W10.N/W11.N/W14.N subwaves; S-P3 needs one canonical costed manifest. | yes |
| CH5 hidden coupling | ACCEPT | The packet preserves one-substrate, no-sidecar, no-renamed-scanner, no Track 1/Track 2 coupling, and no view-boundary-as-admission controls. | no |
| CH6 anti-paper-close | REVISE | W5-W8 can still close as support/consumer plumbing without row movement or architectural block, and W10.N/W11.N/W13/W14.N lack explicit same-wave consumer lines in SPEC. | yes |

## Required V2 Fold

1. Replace all P3-B/P3-C/P3-D/P3-E absence language in P3-F, SPEC, and
   DISPATCH with a current authority map naming P3-A through P3-E and the
   sections each controls.

2. Reconcile the wave manifest to one source of truth. Either fold P3-B's packed
   W0-W11 bracket into SPEC/DISPATCH or revise P3-B to match the longer
   W0-W15 plus subwave plan. Every real triumvirate and subwave must have a
   budget, hard cap, consumer, revert slice, bracket-ceiling accounting, and
   bracket-forward rule.

3. Reclassify P3A-0 as `W0-GOVERNANCE-SUBSTRATE`. Add a trace matrix for
   P3A-1 through P3A-7 mapping candidate -> S-P2 source -> S-P1 antecedent ->
   limitation/fresh-evidence requirement.

4. Fold P3-C formulas into SPEC/DISPATCH gates. CSS and JSON thresholds must
   derive from W0 `SK-V13-open` same-run anchors, with the declaration-values
   maintain gate expressed as `max(lightningcss_open + 1.0, 0.98 *
   SK-V13-open Track1)` plus strict equality.

5. Replace the one-witness Lock 14 rule with the Omega V2 cardinality rule:
   fleet-wide grammar-neutral claims require CSS L4 plus both Sheets and
   BBNF-self fail-closed/generated-role witnesses; CSS plus only one non-CSS
   witness is scoped to the witnessed grammars.

6. Fold P3-D's telemetry schema into SPEC Section 0.4 and DISPATCH required
   packets, including row state, source commit, consumer gate, G-Omega status,
   CSS feature ids/statuses, domain extensions, and gate-json rejection rules.

7. Fold P3-E's route-state ledger into SPEC Section 20, then add exact
   `Pre-blocked REDRESS entries` subsections to every wave packet and mirrored
   DISPATCH entry. Preserve route statuses such as `BLOCKED-HISTORICAL`,
   `REOPEN-CONDITIONAL`, `GATE-FEED`, `HISTORY-LIFTED`, and `MIXED`.

8. Add row-movement or architectural-block exit gates to W5, W6, W7, and W8.
   Resolver, cost, cascade, policy, sink, and flag surfaces cannot close as
   support-only landings.

9. Add explicit `Same-wave consumer:` lines to W10.N, W11.N, W13, and W14.N in
   SPEC and mirror them in DISPATCH. Name the production caller or generated row
   path exercised in the same redress commit.

10. Preserve the accepted CH5 controls while folding: one retained substrate,
    no public substrate API or `UnionTape`, no parser-owned cursor/list, no aux
    density table, no retained side vector, no sidecar event vector, no second
    source scanner, no Track 1/Track 2 coupling, and no view-boundary validation
    as strict admission.

## Evidence

- CH1: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH1.md`.
- CH2: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH2.md`.
- CH3: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH3.md`.
- CH4: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH4.md`.
- CH5: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH5.md`.
- CH6: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH6.md`.

Validation:

- `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V1`
  passed with no output before consolidation.

## Disposition

Proceed to S-P3 V2 fold. Do not dispatch SK-V13 W0, prepare implementation
waves, edit source, edit generated runtime, or mutate `skinny/RESULTS.md` /
`skinny/REDRESS.md` from S-P3 V1. G-Omega also remains a hard pre-W0 gate.
