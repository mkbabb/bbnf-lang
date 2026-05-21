---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-21T19:50:24Z
disposition: ACCEPT
scope: "CH2 generality, generated-output boundaries, Lock 14 routing"
artifacts_audited:
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md
  - restart/audit/totality/p3/hardening/V2/CH2.md
---

# T-P3 V3 CH2 Generality / Lock 14

## Lens Contract

This lens checks whether V3 preserves grammar generality, generated-output
boundaries, no new directive/BIR/BackendShape/public-substrate expansion, and
clear receiver/blocker/gate routing for Lock 14 questions. V2 already accepted
the CH2 substance: generated-output boundaries, five-shape discipline, Lock 14
per-wave gates, and CSS/Sheets/BBNF-self negative-control routing were all
accepted, while V2 failed only on CH1 source-map hygiene
(`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:12`,
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:25`,
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:26`).

## Verdict

Verdict: ACCEPT.

V3 preserves the V2 CH2 acceptance while folding the V2 CH1 hygiene repairs. It
keeps all new authority proposed-only, preserves the existing five-shape canon,
does not authorize new directives, BIR variants, BackendShape variants, public
substrate APIs, or retained sidecars, and keeps grammar-specific names behind
generated/provider manifests plus leak scans. The remaining Lock 14 questions
are routed to named receivers, blockers, and gates rather than left as ambient
future work.

## Evidence

| check | disposition | evidence |
|---|---|---|
| V2 CH2 carry-forward preserved | ACCEPT | V2 CH2 accepted the core generality result and recorded four carry-forward constraints: G-Omega must pin provider manifest schema/location; negative-control routing must not weaken below CSS plus Sheets/BBNF-self; fact streams must stay output planes or SinkOnly products, not a sixth BackendShape; shared primitives must keep grammar policy in generated config or caller data (`restart/audit/totality/p3/hardening/V2/CH2.md:42`, `restart/audit/totality/p3/hardening/V2/CH2.md:71`, `restart/audit/totality/p3/hardening/V2/CH2.md:74`, `restart/audit/totality/p3/hardening/V2/CH2.md:77`, `restart/audit/totality/p3/hardening/V2/CH2.md:80`). |
| V2 revise was not a CH2 defect | ACCEPT | The V2 consolidated verdict says CH2 accepted generated-output boundaries, five-shape discipline, Lock 14 per-wave gates, and negative-control routing; the required V3 fold was CH1 hygiene only: stale cycle wording and bare prompt citations (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:26`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:32`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:34`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:40`). |
| No surface expansion | ACCEPT | 3A states that all V3 deltas preserve the T-P2 convergence limits and propose no new directive, BIR variant, public substrate, retained sidecar, or BackendShape expansion without G-Omega/user approval (`restart/audit/totality/p3/3A-architecture-synthesis.md:27`). 3C repeats that no lock is renumbered and any new lock, lock retirement, directive, BIR variant, public substrate API, retained sidecar, or BackendShape expansion remains user/G-Omega gated (`restart/audit/totality/p3/3C-locks-v+1-diff.md:16`). 3E's onboarding test fails closed if arbitrary grammar support needs a new directive, BIR variant, BackendShape, public substrate API, retained sidecar, or hand-coded generic behavior (`restart/audit/totality/p3/3E-grammar-generalisation.md:114`). |
| Generated-output boundary | ACCEPT | The proposed Lock 14 hunk allows grammar names under `runtime/src/grammars/<name>/` only when emitted by the rostered generator from grammar source plus workspace metadata, and excludes hand-coded provider enums, root aliases, generic-crate grammar branches, grammar-named public generic APIs, generic-root tests/proofs, and grammar-shaped policy mining (`restart/audit/totality/p3/3C-locks-v+1-diff.md:272`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:275`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:277`). 3E requires future grammars to add only grammar source, workspace metadata, and optional declaration-crate host functions before regenerated provider/config/fact/sink/value/view/path/diagnostic/test surfaces; generic-crate diffs must be empty except generated runtime output (`restart/audit/totality/p3/3E-grammar-generalisation.md:91`, `restart/audit/totality/p3/3E-grammar-generalisation.md:98`, `restart/audit/totality/p3/3E-grammar-generalisation.md:99`). |
| Lock 14 remains grammar-general | ACCEPT | 3C requires generic crates to consume generated provider manifests and generated sink/fact/value/flag/fact surfaces rather than hand-coded RuntimeProvider variants, renderer branches, JSON punctuation alphabets, object/array/pair/string/number/bool/null role mining, callback names, or grammar-specific feature flags (`restart/audit/totality/p3/3C-locks-v+1-diff.md:281`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:283`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:286`). 3E's Lock 14 clauses separately require generated provider manifests, generated sink/fact/value/flag surfaces, grammar-shape census, generated/caller-owned primitive policy, CSS plus negative-control closure, and generated-output allowance fences (`restart/audit/totality/p3/3E-grammar-generalisation.md:121`, `restart/audit/totality/p3/3E-grammar-generalisation.md:123`, `restart/audit/totality/p3/3E-grammar-generalisation.md:124`, `restart/audit/totality/p3/3E-grammar-generalisation.md:125`, `restart/audit/totality/p3/3E-grammar-generalisation.md:126`, `restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:130`). |
| Five-shape canon preserved | ACCEPT | 3E states the invariant as finite-shape, data-driven selection and maps CSS L4, Sheets, and BBNF-self onto existing EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage variants (`restart/audit/totality/p3/3E-grammar-generalisation.md:51`, `restart/audit/totality/p3/3E-grammar-generalisation.md:61`). 3D reinforces that decision-engine work must not invent new directives, BIR variants, BackendShape values, or substrates, and its impact index explicitly verifies that decision-engine and non-JSON import consume the existing five shapes (`restart/audit/totality/p3/3D-skinny-fold.md:75`, `restart/audit/totality/p3/3D-skinny-fold.md:86`, `restart/audit/totality/p3/3D-skinny-fold.md:89`). |
| Per-wave Lock 14 gate | ACCEPT | 3C requires any wave touching generic crates, generated provider manifests, primitive policy manifests, runtime roots, codegen templates, decision-engine facts, or shared `bbnf-simd` consumers to run the Lock 14 baseline plus grammar-name and grammar-shape leak census in the same wave; it names generated provider registry, grammar-shape role mining, generated ownership, primitive policy source, CSS plus Sheets/BBNF-self witness or negative control, and decision-engine generated facts as minimum checks (`restart/audit/totality/p3/3C-locks-v+1-diff.md:290`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:293`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:294`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:296`). The 3C cost ledger routes the Lock 14 hunk to the registry/runtime wave and blocks generated names unless they are rostered output with per-wave grammar-shape leak scan (`restart/audit/totality/p3/3C-locks-crystallisation.md:131`). |
| Primitive policy ownership is generated or caller-owned | ACCEPT | 3E's primitive transfer table keeps alphabets, quote/escape/control policy, number policy, direct/fact sink callbacks, regex/HIR facts, resolver facts, and SIMD row gates sourced from generated grammar or caller data across CSS, Sheets, and BBNF-self (`restart/audit/totality/p3/3E-grammar-generalisation.md:77`, `restart/audit/totality/p3/3E-grammar-generalisation.md:81`, `restart/audit/totality/p3/3E-grammar-generalisation.md:82`, `restart/audit/totality/p3/3E-grammar-generalisation.md:83`, `restart/audit/totality/p3/3E-grammar-generalisation.md:84`, `restart/audit/totality/p3/3E-grammar-generalisation.md:86`, `restart/audit/totality/p3/3E-grammar-generalisation.md:87`). 3C likewise restricts shared `bbnf-simd`, parse-that, and future regex APIs to grammar-neutral facts and primitives, with quote/escape/control/delimiter/number/string/no-string/no-number policy coming from generated grammar config or caller data (`restart/audit/totality/p3/3C-locks-v+1-diff.md:302`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:304`). |
| Negative controls are not prose-only | ACCEPT | 3B proposes MP.NW6 for generated registry, grammar-owned surfaces, grammar-name and grammar-shape scans, and CSS plus Sheets/BBNF-self negative controls; MP.NW11 adds generated Sheets and BBNF-self witnesses or fail-closed telemetry (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:117`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:122`). 3E requires a positive CSS row plus Sheets or BBNF-self witness/negative-control before fleet-wide grammar-neutral wording (`restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:170`). |
| Lock 14 open questions are routed | ACCEPT | V3 keeps explicit receiver/blocker/gate routing for Lock 14-adjacent questions: 3A routes the generated registry manifest location through Pass Omega CRUD-1/CRUD-3 and G-Omega (`restart/audit/totality/p3/3A-architecture-synthesis.md:76`, `restart/audit/totality/p3/3A-architecture-synthesis.md:81`); 3B routes negative-control cardinality to G-Omega and the Lock 14 registry wave (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:164`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:169`); 3C routes provider-manifest layout to 3E plus Omega CRUD-1/CRUD-3 and requires G-Omega to pin schema (`restart/audit/totality/p3/3C-locks-crystallisation.md:136`, `restart/audit/totality/p3/3C-locks-crystallisation.md:141`); 3E routes fact-stream placement, provider layout, and negative-control fixture choice to named 3A/3C/S-P3 receivers and gates (`restart/audit/totality/p3/3E-grammar-generalisation.md:173`, `restart/audit/totality/p3/3E-grammar-generalisation.md:178`, `restart/audit/totality/p3/3E-grammar-generalisation.md:179`, `restart/audit/totality/p3/3E-grammar-generalisation.md:181`); 3F routes the manifest filename/schema to 3E plus Omega CRUD surfaces and G-Omega (`restart/audit/totality/p3/3F-migration-handoff.md:191`, `restart/audit/totality/p3/3F-migration-handoff.md:196`). |
| Migration and handoff preserve boundaries | ACCEPT | 3F proposes replacing hardcoded grammar registries, runtime profiles, root aliases, and provider branches with generated-provider/roster rows, and reclassifies hand-owned JSON/CSS sinks, scan modules, root tests, and proof witnesses as generated, rostered, archived, or fixture-only (`restart/audit/totality/p3/3F-migration-handoff.md:84`, `restart/audit/totality/p3/3F-migration-handoff.md:85`). Its V2 ledger blocks hardcoded provider/runtime-profile/root-alias branches, hand-owned per-grammar runtime surfaces, forced CSS fact streams into retained substrate categories, and two-surface/generic-branch onboarding language (`restart/audit/totality/p3/3F-migration-handoff.md:179`, `restart/audit/totality/p3/3F-migration-handoff.md:180`, `restart/audit/totality/p3/3F-migration-handoff.md:181`, `restart/audit/totality/p3/3F-migration-handoff.md:182`, `restart/audit/totality/p3/3F-migration-handoff.md:189`). |

## Required Revisions

Required revisions: none for CH2.

## Carry-Forward Constraints

1. G-Omega must pin the provider manifest schema/location before a Lock 14
   registry implementation begins; the V3 packet routes the decision but does
   not select a hidden generic runtime branch.
2. Do not weaken the negative-control rule below CSS plus Sheets or BBNF-self.
   The admitted SK-V12 CSS fact row remains evidence, not full CSS parity or
   fleet-wide grammar closure.
3. Keep fact streams as output planes or SinkOnly products with comparator
   provenance; do not let CSS fact streams become retained sidecars or a sixth
   BackendShape.
4. Shared primitives remain grammar-neutral byte operations. Grammar policy must
   come from generated grammar config or caller data, and any claimed
   grammar-neutral primitive must exercise a non-JSON consumer or record
   measured deletion/rejection.

## Cycle Disposition

CH2 disposition for T-P3 V3: ACCEPT.
