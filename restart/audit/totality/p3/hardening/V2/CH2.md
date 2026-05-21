---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-21T19:44:17Z
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
  - restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md
  - restart/audit/totality/p3/hardening/V1/CH2.md
  - restart/audit/totality/p3/hardening/V1/CH6.md
---

# T-P3 V2 CH2 Generality / Lock 14

## Lens Contract

This lens checks whether V2 preserves grammar generality, generated-output
boundaries, no new directive/BIR/BackendShape/public-substrate expansion, and
clear receiver/blocker/gate routing for Lock 14 questions. V1 CH2 already
accepted the substantive Lock 14 story, with carry-forward constraints on the
generated-output fence, negative controls, provider-manifest resolution,
fact-stream placement, and caller/generated-owned primitive policy
(`restart/audit/totality/p3/hardening/V1/CH2.md:51`,
`restart/audit/totality/p3/hardening/V1/CH2.md:83`). V1 failed convergence on
cost/routing, not CH2 substance: the consolidated hardening record says CH2 was
ACCEPT while CH6 required every future-cycle route to name receiver, blocker, and
gate (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:23`,
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:49`;
`restart/audit/totality/p3/hardening/V1/CH6.md:56`).

## Verdict

Verdict: ACCEPT.

V2 preserves the V1 CH2 generality result and repairs the Lock 14 routing gaps
that could have turned open questions into ambient future work. The packet keeps
the five-shape canon, blocks new directives/BIR/public substrate/BackendShape
expansion without G-Omega, allows generated grammar names only behind rostered
generated output, requires name and shape leak scans, and routes provider
manifest plus negative-control cardinality decisions to explicit receivers and
gates.

## Evidence

| check | disposition | evidence |
|---|---|---|
| No surface expansion | ACCEPT | 3A states that no new directive, BIR variant, public substrate, retained sidecar, or BackendShape expansion is proposed without G-Omega/user approval (`restart/audit/totality/p3/3A-architecture-synthesis.md:27`). 3C repeats that no lock is renumbered and any new directive, new BIR variant, public substrate API, retained sidecar, or BackendShape expansion is user/G-Omega gated (`restart/audit/totality/p3/3C-locks-v+1-diff.md:16`). 3E's onboarding test fails closed if a grammar needs a new directive, BIR variant, BackendShape, public substrate API, retained sidecar, or hand-coded generic behavior (`restart/audit/totality/p3/3E-grammar-generalisation.md:114`). |
| Generated-output boundary | ACCEPT | 3C's proposed Lock 14 text permits grammar names in `runtime/src/grammars/<name>/` only when emitted by the rostered generator from grammar source plus workspace metadata, and explicitly excludes hand-coded provider enums, root aliases, generic-crate grammar branches, grammar-named generic public APIs, generic-root proof fixtures, and grammar-shaped policy mining (`restart/audit/totality/p3/3C-locks-v+1-diff.md:272`). 3E's future-grammar onboarding test allows only grammar source, workspace metadata, and optional declaration-crate host functions, then requires generated provider/config/fact/sink/value/view/path/diagnostic/test surfaces with generic-crate diffs empty except generated runtime output (`restart/audit/totality/p3/3E-grammar-generalisation.md:91`). |
| Lock 14 is grammar-general, not JSON-specific | ACCEPT | 3A routes grammar neutrality through a generated registry/manifest contract with CSS/Sheets/BBNF-self negative controls (`restart/audit/totality/p3/3A-architecture-synthesis.md:39`). 3B adds MP.NW6 for generated registry, grammar-owned surfaces, grammar-name plus grammar-shape scans, and CSS plus Sheets/BBNF-self negative controls; MP.NW8 includes JSON/CSS/Sheets/BBNF-self backend-shape rows; MP.NW11 adds Sheets and BBNF-self witnesses or fail-closed telemetry (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:117`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:122`). |
| Five-shape canon preserved | ACCEPT | 3E states the invariant as finite-shape, data-driven selection, and its matrix maps CSS L4, Sheets, and BBNF-self onto the existing `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` shapes (`restart/audit/totality/p3/3E-grammar-generalisation.md:51`, `restart/audit/totality/p3/3E-grammar-generalisation.md:61`). The same artifact requires any future grammar to emit a five-shape eligibility report or generated unreachable-shape reasons (`restart/audit/totality/p3/3E-grammar-generalisation.md:105`). |
| Per-wave Lock 14 gate | ACCEPT | 3C's Lock 14 hunk requires any wave touching generic crates, generated provider manifests, primitive policy manifests, runtime roots, codegen templates, decision-engine facts, or shared `bbnf-simd` consumers to run a Lock 14 baseline plus grammar-name and grammar-shape leak census in the same wave (`restart/audit/totality/p3/3C-locks-v+1-diff.md:290`). The 3C cost/disposition ledger routes the Lock 14 hunk to a registry/runtime wave and blocks generated names unless they are rostered output with a per-wave grammar-shape leak scan (`restart/audit/totality/p3/3C-locks-crystallisation.md:131`). |
| Primitive policy ownership remains generated/caller-owned | ACCEPT | 3E's primitive transfer table requires caller or generated grammar ownership for alphabets, quote/escape/control policy, number policy, sink callbacks, resolver facts, and SIMD row gates across CSS, Sheets, and BBNF-self (`restart/audit/totality/p3/3E-grammar-generalisation.md:77`). 3C's Lock 14 text forbids shared `bbnf-simd`, parse-that, and future regex APIs from owning JSON/CSS constants; quote, escape, control, delimiter, number, string, and no-string/no-number policy must come from generated grammar config or caller data (`restart/audit/totality/p3/3C-locks-v+1-diff.md:302`). |
| Lock 14 open questions are routed | ACCEPT | V2 gives the generated provider manifest question explicit receivers, blockers, and gates in 3A, 3B, 3C, 3E, and 3F: 3A routes manifest location to Pass Omega CRUD-1/CRUD-3 plus G-Omega (`restart/audit/totality/p3/3A-architecture-synthesis.md:76`); 3B routes negative-control cardinality to G-Omega and the Lock 14 registry wave (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:164`); 3C routes provider-manifest schema to 3E plus Omega CRUD-1/CRUD-3 (`restart/audit/totality/p3/3C-locks-crystallisation.md:136`); 3E routes fact-stream placement and provider layout through 3A/3C and S-P3/Lock 14 registry gates (`restart/audit/totality/p3/3E-grammar-generalisation.md:173`); 3F routes manifest filename/schema to 3E plus Pass Omega CRUD-1/CRUD-3/CRUD-4 and G-Omega (`restart/audit/totality/p3/3F-migration-handoff.md:185`). |
| Migration and handoff preserve generated boundaries | ACCEPT | 3F proposes migration rows that replace hardcoded grammar registries, runtime profiles, root aliases, hand-coded provider branches, hand-owned per-grammar runtime surfaces, and JSON/CSS sinks with generated-provider, generated/rostered/archive/fixture, or non-JSON telemetry planes (`restart/audit/totality/p3/3F-migration-handoff.md:77`, `restart/audit/totality/p3/3F-migration-handoff.md:80`). Its V2 ledger blocks hardcoded provider/runtime-profile/root-alias branches, hand-owned per-grammar runtime surfaces in generic crates, and two-surface/generic-branch onboarding language (`restart/audit/totality/p3/3F-migration-handoff.md:173`, `restart/audit/totality/p3/3F-migration-handoff.md:183`). |

## Required Revisions

Required revisions: none for CH2.

## Carry-Forward Constraints

1. G-Omega must pin the provider manifest schema/location before any Lock 14
   registry implementation; V2 routes this through 3A/3C/3E/3F rather than
   embedding a hand-coded provider shape.
2. Do not weaken the negative-control rule below V2's CSS plus Sheets/BBNF-self
   standard. A single CSS fact-stream row remains admitted evidence, not full
   CSS parity or fleet-wide grammar closure.
3. Keep fact streams as output planes or SinkOnly products with comparator
   provenance; do not let CSS fact streams become retained sidecars or a sixth
   BackendShape.
4. Shared primitives remain grammar-neutral byte operations. Grammar policy must
   come from generated grammar config or caller data, and any claimed
   grammar-neutral primitive must exercise a non-JSON consumer or record
   measured deletion/rejection.

## Cycle Disposition

CH2 disposition for T-P3 V2: ACCEPT.
