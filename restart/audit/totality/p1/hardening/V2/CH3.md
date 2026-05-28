# CH3 REGRESSION - T-P1 V2 (SK-V15)

## Verdict

ACCEPT.

The current T-P1 V2 packet is a fresh SK-V15 inventory fold at commit
`2fcbc1dc8`, not the historical SK-V14 V2 hardening surface previously at
this path. CH3 accepts the packet for regression, wave-graph, and REDRESS
discipline: it identifies the delete-before-rebuild failure class, binds
REDRESS-183/184/209..213 as pre-blocks, fences EventTape and typed-event
cursor work against sidecar resurrection, and keeps CSS source-sidecar
evidence comparator-only.

## Findings

| ID | Disposition | Finding | Evidence | Required action |
|---|---|---|---|---|
| CH3-V2-001 | ACCEPT | V2 folded the V1 CH3 requirement to add NEW-CH3-V5-01 delete/rebuild dependency language. The fold is visible in the packet rather than left as hardening prose. | V1 consolidated requires the fold at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:32-34`; V2 1D records `CH3-V1-005` at `1D-skinny-lessons.md:43-45` and fold disposition at `1D-skinny-lessons.md:168-170`; 1E records `CH3-V1-006` at `1E-locks-evidence.md:40`. | None. |
| CH3-V2-002 | ACCEPT | V3 W2R / V4 W4R-style wave-graph cycles are captured as the active precedent: destructive CSS/runtime work cannot outrun a rebuild provider. | REDRESS-183 records W2 dual-tree deletion before root CSS runtime generation existed and notes W2R supersession at `skinny/REDRESS.md:5090-5101`; REDRESS-184 records W4 provider/template deletion before replacement generation at `skinny/REDRESS.md:5103-5118`; 1D C-7 cites the whole cluster at `1D-skinny-lessons.md:152-153`. | None. |
| CH3-V2-003 | ACCEPT | REDRESS-209..213 remain pre-blocked for provider/template deletion, provider-free generator body gaps, sub-wave authority gaps, and destructive root runtime regeneration. | `skinny/REDRESS.md:5171-5293` records REDRESS-209 through REDRESS-213; Alpha-C summarizes them at `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26-33`; 1F carries the same do-not-redrive ledger at `1F-coherence-scan.md:126-135`. | None. |
| CH3-V2-004 | ACCEPT | The packet would not allow deletion before same-wave or prior-wave rebuild proof. It permits diagnostic demotion before rebuild only when no live provider is deleted. | SK-V15 synthesis couples CSS parser retirement to typed CSS proof at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:91-96`; NEW-CH3-V5-01 requires a dependency table at `SYNTHESIS.md:102-106`; handoff blocks deletion/retirement until provider proof at `restart/skinny/tranches/sk-v15/HANDOFF.md:64-67`; 1E adds LAC-1E-V2-15 at `1E-locks-evidence.md:139` with wave/cost carrier at `1E-locks-evidence.md:159`; 1F COH-003 marks top-level omission as drift at `1F-coherence-scan.md:71` and `1F-coherence-scan.md:106`. | None for T-P1 V2. S-P3 must still emit the actual dependency table before implementation waves execute. |
| CH3-V2-005 | ACCEPT | EventTape and typed-event cursor rows are regression-fenced and do not reopen rejected EventCursor, retained stream, class-lane, parser-owned cursor, or cross-call classifier routes. | 1A folded the fence at `1A-substrate-evidence.md:14` and applies it to typed-event cursor and EventTape rows at `1A-substrate-evidence.md:72-73`; 1B applies the same EventTape fence at `1B-codegen-evidence.md:67`; 1C fences EventTape proof-witness state at `1C-runtime-evidence.md:62` and `1C-runtime-evidence.md:98-100`. | None. |
| CH3-V2-006 | ACCEPT | CSS source-sidecar coupling is current-cycle fenced as comparator/output-plane evidence only, not runtime substrate, CSS Value API proof, or a route to re-admit broadcast CSS rows. | 1D treats CSS typed API as missing and deletion-coupled at `1D-skinny-lessons.md:151-153`; 1C says CSS fact streams are output-plane evidence only and sends comparator sidecars to current 1F at `1C-runtime-evidence.md:98-100`; 1F COH-015 re-anchors `fixture_sidecar_facts` and `same-plane-source-sidecar` line cites at `1F-coherence-scan.md:83`; auxiliary files are explicitly superseded at `1F-anti-pattern.md:6-14` and `1F-past-corpora.md:6-19`. | None. |
| CH3-V2-007 | ACCEPT | No admitted REDRESS row is silently demoted. JSON remains guard evidence, CSS is audit-demoted as implementation truth, and historical CSS admits are treated as history rather than independent current SOTA proof. | 1D preserves JSON guard status at `1D-skinny-lessons.md:72-90` and `1D-skinny-lessons.md:121-127`; CSS audit demotion and Value API gap are separated at `1D-skinny-lessons.md:147-153`; 1E states JSON evidence is stronger than CSS and CSS 24 admits are over-stated at `1E-locks-evidence.md:59-70`; 1F COH-011 preserves JSON guard baseline at `1F-coherence-scan.md:79`. | None. |

## NEW-CH3-V5-01 Coverage Judgment

Coverage is ACCEPT for T-P1 V2. The packet fully carries the NEW-CH3-V5-01
rule at the evidence/governance layer: every delete or retirement wave must
name the retired artefact, delete/retire wave, rebuild provider wave, proof
command, and evidence that the provider lands no later than the delete/retire
wave. T-P1 V2 does not itself owe the S-P3 implementation dependency table;
it correctly makes that table a blocker for later wave execution.

The packet therefore does not permit deletion before same-wave rebuild proof.
Any later plan that deletes CSS providers/templates, `CSS_GENERATED_RS`,
`CssFullParseSummary`, fact-stream parser contracts, root Pattern H runtime
files, or CSS runtime shims without the dependency-table proof would violate
this accepted CH3 finding and reopen REDRESS-183/184/209..213.
