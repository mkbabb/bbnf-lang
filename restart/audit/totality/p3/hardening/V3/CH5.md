---
lens: CH5
name: HIDDEN COUPLING
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-21T19:50:54Z
files_audited:
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V2/CH5.md
  - restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md
scope: "CH5 hidden coupling only"
---

# CH5 Hidden Coupling - T-P3 V3

## Verdict

ACCEPT.

V3 preserves the V2 hidden-coupling boundary after the CH1 source-map hygiene
fold. I found no retained comparator sidecar, hidden fact-stream substrate,
hand-coded generated-provider escape hatch, primitive/ASM producer-only bridge,
decision-engine surface expansion, or union-substrate replay path introduced by
the V3 packet.

## Evidence

| check | disposition | evidence |
|---|---|---|
| V2 CH5 carry-forward | ACCEPT | V2 CH5 accepted the hidden-coupling boundary and required no revisions (`restart/audit/totality/p3/hardening/V2/CH5.md:24`-`28`, `restart/audit/totality/p3/hardening/V2/CH5.md:45`-`68`). The V2 consolidated challenge narrowed the V3 fold to CH1 source-map hygiene while leaving CH5 accepted (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:12`-`15`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:29`). |
| CSS comparator sidecars | ACCEPT | 3C keeps sidecars as strict comparator anchors only when corpus, output plane, host, strictness, freshness, sidecar status, and gate-consumed artifact provenance match the row (`restart/audit/totality/p3/3C-locks-v+1-diff.md:151`). 3B and 3D keep the open coupling risk routed to BENCH/Lock text: comparator sidecars must be comparator-only and forbidden as runtime dependencies (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`, `restart/audit/totality/p3/3D-skinny-fold.md:126`). |
| CSS fact streams | ACCEPT | 3A separates transient scanner inputs, comparator sidecars, admitted fact-stream outputs, and retained runtime substrate (`restart/audit/totality/p3/3A-architecture-synthesis.md:38`). 3C states fact streams are output-plane contracts, not retained internal sidecars, and gates them on strict comparator/oracle provenance plus telemetry (`restart/audit/totality/p3/3C-locks-v+1-diff.md:77`-`82`). 3E repeats that fact streams are not retained sidecars and do not create a sixth `BackendShape` (`restart/audit/totality/p3/3E-grammar-generalisation.md:129`, `restart/audit/totality/p3/3E-grammar-generalisation.md:140`). |
| Generated provider manifests and Lock 14 bridge | ACCEPT | 3A routes generated registry work through generator-owned manifests, leak scans, and CSS/Sheets/BBNF-self controls (`restart/audit/totality/p3/3A-architecture-synthesis.md:39`, `restart/audit/totality/p3/3A-architecture-synthesis.md:71`). 3C allows grammar names only in rostered generated output and forbids hand-coded provider enums, root aliases, generic-crate grammar branches, grammar-named public APIs, and grammar-shaped policy mining (`restart/audit/totality/p3/3C-locks-v+1-diff.md:281`-`300`). 3E makes the same generated-provider and sink/fact/value/flag ownership the Lock 14 receiver, with hard-coded provider arrays/root aliases/grammar branches blocked (`restart/audit/totality/p3/3E-grammar-generalisation.md:123`, `restart/audit/totality/p3/3E-grammar-generalisation.md:166`). |
| Lock 14/16 primitive bridge | ACCEPT | 3A requires PrimitiveFacts to include source-present state, scalar reference, strict checkasm/parity, first same-wave consumer, substrate target, telemetry, abrogate gate, and zero-orphan disposition (`restart/audit/totality/p3/3A-architecture-synthesis.md:41`). 3C maps every `core::arch::*`, `target_feature`, and `asm!` use-site to a primitive manifest with same-wave production consumer and row/feature gate (`restart/audit/totality/p3/3C-locks-v+1-diff.md:338`-`347`), keeps `escape_mask_64` prerequisite-only until wired to a same-wave consumer (`restart/audit/totality/p3/3C-locks-v+1-diff.md:355`), and forces every source-present primitive into `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS` at close (`restart/audit/totality/p3/3C-locks-v+1-diff.md:364`). 3E blocks primitive-only imports without scalar oracle, checkasm, and row movement/rejection (`restart/audit/totality/p3/3E-grammar-generalisation.md:167`). |
| Decision-engine routing | ACCEPT | 3A routes regex/HIR facts into compile-time facts and the decision engine only after import gates, while scanner outputs remain local temp or existing-tape/direct-sink inputs unless G-Omega approves a new surface (`restart/audit/totality/p3/3A-architecture-synthesis.md:42`, `restart/audit/totality/p3/3A-architecture-synthesis.md:57`). 3B defines the decision-engine replacement as bounded resolver reports plus JSON/CSS equality rows, with egraph/CSP/stale-cost abrogate gates (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:158`). 3C keeps the five `BackendShape` variants as the search domain and keeps new shape/directive/BIR additions G-Omega gated (`restart/audit/totality/p3/3C-locks-v+1-diff.md:199`-`217`). 3D explicitly routes the decision-engine fold as replacement of P1-P8, not as a new directive, BIR, `BackendShape`, or substrate (`restart/audit/totality/p3/3D-skinny-fold.md:75`). |
| Union and substrate reopen routing | ACCEPT | 3B requires the fresh union wave to produce measured JSON/CSS row movement with a same-wave consumer or architectural-block proof (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:121`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:161`). 3C makes e-graph candidates, backend rewrites, scanner plans, union candidates, and SIMD consumers declare substrate target, retention lifetime, and policy owner; retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tapes are rejected unless G-Omega amends Lock 1 (`restart/audit/totality/p3/3C-locks-v+1-diff.md:84`-`92`). REDRESS 96/97/98 replay requires a fresh material differential, proof, same-wave consumer, row gate, rollback, and abrogate threshold (`restart/audit/totality/p3/3C-locks-v+1-diff.md:95`-`101`). |
| G-Omega and proposal boundary | ACCEPT | The locks diff remains proposed-only and explicitly bars T-P3 from editing `LOCKS.md` directly (`restart/audit/totality/p3/3C-locks-v+1-diff.md:7`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:12`). It also keeps new locks, lock retirements, directives, BIR variants, public substrate APIs, retained sidecars, and `BackendShape` expansion behind user/G-Omega (`restart/audit/totality/p3/3C-locks-v+1-diff.md:16`). 3F preserves the same boundary: V1 surface edits land through Pass Omega CRUD/user gate flow, W0 remains blocked before G-Omega and S-P3, and pre-G-Omega CRUD output is proposed diffs/logs only (`restart/audit/totality/p3/3F-migration-handoff.md:66`, `restart/audit/totality/p3/3F-migration-handoff.md:147`-`151`, `restart/audit/totality/p3/3F-migration-handoff.md:175`). |

## Required Revisions

None.

## Carry-Forward Guardrails

1. CSS sidecars remain comparator-only. They may anchor strict same-plane
   provenance but must not become runtime inputs, retained sidecars, or
   substrate state.
2. CSS fact streams remain admitted output-plane evidence only. They do not
   create a sixth `BackendShape`, a retained runtime substrate, or full CSS
   parity authority by themselves.
3. Generated provider manifests must stay generated and rostered. Hand-coded
   provider enums, root aliases, grammar branches, public grammar-named generic
   APIs, or JSON-shaped policy mining remain Lock 14 failures.
4. Decision-engine and union routes may consume existing five-shape facts and
   existing substrate targets only. Any new directive, BIR variant,
   `BackendShape`, public substrate API, `UnionTape`, retained class stream, or
   second tape remains G-Omega gated.
5. Lock 16 primitives remain non-admitting until scalar reference, strict
   checkasm/parity, same-wave production consumer, row movement or measured
   rejection, and final source-present disposition are all recorded.

## Cycle Disposition

CH5 disposition for T-P3 V3: ACCEPT.
