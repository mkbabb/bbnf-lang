---
lens: CH5
name: HIDDEN COUPLING
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-21T20:09:00Z
files_audited:
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V1/CH5.md
  - restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md
scope: "CH5 hidden coupling only"
---

# CH5 Hidden Coupling - T-P3 V2

## Verdict

ACCEPT.

V2 preserves the V1 hidden-coupling boundary while folding CH4/CH6 repairs. The
new cost/routing ledgers do not smuggle a parallel substrate, retained sidecar,
grammar-name coupling, producer-only primitive, or implementation permission
through a planning surface. The residual coupling risks are explicitly routed as
gates rather than paper-closed.

## Evidence

| check | disposition | evidence |
|---|---|---|
| V1 revise set did not target CH5 | ACCEPT | V1 consolidated hardening recorded CH5 as accepted and required V2 repairs only for cost discipline and anti-paper-close routing (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:23`-`30`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:32`-`53`). V1 CH5's carry-forward guardrails were comparator-only CSS sidecars, precise Track 2 semantics, no retained class-column substrate, and no sidecar relaxation (`restart/audit/totality/p3/hardening/V1/CH5.md:52`-`65`). |
| CSS comparator sidecars | ACCEPT | 3C Lock 8 makes sidecars strict anchors only when corpus, output plane, host, strictness, freshness, sidecar status, and gate-consumed provenance match the candidate row (`restart/audit/totality/p3/3C-locks-v+1-diff.md:151`-`158`). 3B, 3D, and 3E keep the open risk visible: CSS feature waves must split if comparator scope exceeds cap, sidecars must be comparator-only, and every CSS row must emit output-plane provenance with no runtime dependency on sidecars (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:171`-`172`, `restart/audit/totality/p3/3D-skinny-fold.md:126`, `restart/audit/totality/p3/3E-grammar-generalisation.md:180`). |
| CSS fact streams and output planes | ACCEPT | 3A separates transient scanner/capacity inputs, comparator sidecars, admitted fact-stream outputs, and retained runtime substrate (`restart/audit/totality/p3/3A-architecture-synthesis.md:38`). The proposed Lock 1 text says fact streams are output-plane contracts, not retained internal sidecars, and require strict comparator/oracle provenance plus gate-consumed telemetry (`restart/audit/totality/p3/3C-locks-v+1-diff.md:77`-`82`). 3E repeats that CSS fact streams are not retained sidecars and not a sixth `BackendShape` (`restart/audit/totality/p3/3E-grammar-generalisation.md:129`, `restart/audit/totality/p3/3E-grammar-generalisation.md:140`). |
| Generated provider manifests and Lock 14 bridge | ACCEPT | 3A's V2 ledger routes generated registry work to a manifest contract and Lock 14 scan, with abrogation if generic crates need hand-coded grammar branches or JSON-shaped flags (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`). 3C Lock 14 allows grammar names only in rostered generated output and excludes hand-coded provider enums, root aliases, grammar branches, public grammar-named generic APIs, and grammar-shaped policy mining (`restart/audit/totality/p3/3C-locks-v+1-diff.md:272`-`288`). The per-wave gate requires generated provider registry, name/shape census, primitive policy source, CSS plus negative-control witness, and decision-engine generated facts when relevant (`restart/audit/totality/p3/3C-locks-v+1-diff.md:290`-`300`). |
| Lock 16 primitive/ASM bridge | ACCEPT | 3C Lock 16 requires each `core::arch::*`, `target_feature`, and `asm!` use-site to map to a manifest row with scalar reference, strict checkasm/parity command, grammar policy source, substrate target, retention lifetime, same-wave consumer, expected row/feature gate, rollback, abrogate threshold, and final disposition (`restart/audit/totality/p3/3C-locks-v+1-diff.md:338`-`347`). `escape_mask_64` is prerequisite-only unless wired to a JSON/CSS string or escape consumer in the same wave (`restart/audit/totality/p3/3C-locks-v+1-diff.md:355`-`362`), and every source-present primitive must end as wired, deleted, scalar-delegate, or architectural-blocked (`restart/audit/totality/p3/3C-locks-v+1-diff.md:364`-`371`). |
| Decision-engine routing | ACCEPT | 3D routes the decision-engine fold as replacement of the P1-P8 cascade, explicitly not as a new directive, BIR, `BackendShape`, or substrate (`restart/audit/totality/p3/3D-skinny-fold.md:75`). 3C keeps the five `BackendShape` variants as the search domain and makes new shape/directive/BIR additions G-Omega gated (`restart/audit/totality/p3/3C-locks-v+1-diff.md:199`-`209`). Regex/HIR facts are required for scanner/backend-shape influence, opaque strings are non-admitting, and stale/static fallback is non-admitting (`restart/audit/totality/p3/3C-locks-v+1-diff.md:211`-`217`). |
| Union and substrate reopen routing | ACCEPT | 3B's fresh union wave requires measured JSON/CSS row movement with a same-wave consumer or architectural-block proof (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:121`). 3C Lock 1 requires each e-graph candidate, backend rewrite, scanner plan, union candidate, and SIMD consumer to declare substrate target, retention lifetime, and policy owner; retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tape are rejected unless G-Omega amends Lock 1 (`restart/audit/totality/p3/3C-locks-v+1-diff.md:84`-`92`). REDRESS 96/97/98 replay requires fresh material differential, proofs, consumer, row gate, rollback, and abrogate threshold (`restart/audit/totality/p3/3C-locks-v+1-diff.md:95`-`101`). |
| Governance boundary | ACCEPT | The V2 diff remains proposed-only, and ACCEPT/MODIFY in 3C is lock-text disposition rather than implementation admission (`restart/audit/totality/p3/3C-locks-v+1-diff.md:12`-`16`). The footer blocks implementation waves from using proposed v+1 text as permission to edit source, write RESULTS/REDRESS, add a directive/BIR/lock/BackendShape/public substrate API, retain a sidecar, or dispatch W0 before G-Omega (`restart/audit/totality/p3/3C-locks-v+1-diff.md:403`-`414`). 3F carries the same refusal into the proposed handoff and checklist (`restart/audit/totality/p3/3F-migration-handoff.md:110`-`120`, `restart/audit/totality/p3/3F-migration-handoff.md:147`-`155`). |

## Required Revisions

None.

## Carry-Forward Guardrails

1. Pass Omega must keep CSS comparator/source sidecars comparator-only. They may
   anchor strict sidecar provenance, but they must not become runtime inputs or
   retained fact stores.
2. The generated provider manifest must be consumed as generated data. A
   handwritten provider enum, root alias, grammar branch, or generic
   grammar-shaped policy remains a Lock 14 failure.
3. Decision-engine and union waves may consume existing five-shape facts and
   existing tape/direct/admitted-output targets only. Any new substrate,
   `BackendShape`, directive, BIR variant, retained sidecar, or public substrate
   API remains G-Omega gated.
4. Lock 16 primitives must retain the strict manifest state machine: scalar
   reference, checkasm/parity, first consumer, row movement or measured
   rejection, and final disposition. Microbench or source presence alone remains
   non-admitting.

## Cycle Disposition

CH5 disposition for T-P3 V2: ACCEPT.
