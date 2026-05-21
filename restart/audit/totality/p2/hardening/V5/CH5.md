# T-P2 V5 CH5 Hidden Coupling / Substrate Ownership

Pass: T-P2 Research.
Cycle: V5 unchanged-packet confirmation.
Lens: CH5 hidden coupling / Lock 1.
Date: 2026-05-21.
Agent: CH5.

## Verdict

ACCEPT.

The unchanged V4 packet still satisfies CH5. The packet does not add a sidecar,
public substrate API, new BIR variant, new `BackendShape` variant, or retained
parser-owned stream. The V4 delta is ownership and routing metadata over the V3
ledger: non-shortlist blockers remain blockers, REDRESS-slice mappings are not
admissions, and every high-risk scanner / union / parse-that path remains tied
to `substrate_target`, `retention_lifetime`, `policy_owner`, a same-wave
consumer, and fail-closed gates.

## Evidence

| id | disposition | evidence |
|---|---|---|
| CH5-V5-01 | ACCEPT | The pass definition says CH5 rejects any grounded design that implies a parallel substrate, sidecar producer, or Lock 1 violation, and specifically requires 2D mask streams to stay transient and 2B Layer 0 / Layer 1 to stay cleanly separated (`PASS-2-RESEARCH.md:121`-`125`). The same prompt also says T-P2 may not silently absorb a new directive, BIR variant, or substrate (`PASS-2-RESEARCH.md:207`). |
| CH5-V5-02 | ACCEPT | V4 consolidated already accepted CH5 on this unchanged packet: the packet remains research authority only, its non-shortlist blockers / command shapes / REDRESS mappings are not admissions, and CH5 accepted because Lock 1 fields remained authoritative with no retained sidecars, public substrates, new BIR / `BackendShape`, or parser-owned runtime streams (`HARDENING-T-P2-V4-CONSOLIDATED.md:12`-`18`; `:29`). V4 explicitly requested an unchanged-packet V5 confirmation (`HARDENING-T-P2-V4-CONSOLIDATED.md:32`-`37`). |
| CH5-V5-03 | ACCEPT | The V4 fold does not weaken earlier Lock 1 or REDRESS contracts; it adds counted-source repair, executable-ledger deltas, non-shortlist blockers, and REDRESS-slice ownership only (`T-P2-V4-FOLD-ADDENDUM.md:11`-`22`). The V4 executable rows either name exact commands / consumers or remain non-shortlist until missing strict parity, combined matrices, first consumers, caller placement, and row gates exist (`T-P2-V4-FOLD-ADDENDUM.md:43`-`58`). |
| CH5-V5-04 | ACCEPT | The V3 executable ledger is still the substrate authority. It requires `substrate_target`, `retention_lifetime`, and `policy_owner` on shared 2B/2E/2F candidates, with allowed targets limited to `local_temp_only`, `existing_tape`, `direct_sink`, or `admitted_fact_output` rather than a retained public substrate (`T-P2-V3-FOLD-ADDENDUM.md:86`-`101`). |
| CH5-V5-05 | ACCEPT | Structural scanner pressure remains transient. 2A keeps structural masks `local_temp_only` unless consumed into `existing_tape`, `direct_sink`, or `admitted_fact_output`, rejects retained union class columns and streaming cursors without a material differential, and rejects public substrate API expansion for On-Demand transfer (`2A-sota-landscape.md:62`; `:99`; `:123`-`:137`). |
| CH5-V5-06 | ACCEPT | 2B keeps Layer 0 as ABI / checkasm process infrastructure and Layer 1 as grammar-neutral primitive contracts, not grammar policy or a runtime substrate (`2B-primitive-vocabulary.md:66`-`67`; `:84`-`:124`). Its union / ASM checklist explicitly forbids retained class-column, streaming cursor, class-lane-only, parser-owned sidecar, and `UnionTape` shapes, and requires masks / positions to be consumed in one loop into an existing tape, direct sink, or admitted fact output (`2B-primitive-vocabulary.md:237`-`255`). |
| CH5-V5-07 | ACCEPT | 2C and 2D keep the public decision surface closed. 2C keeps `BackendShape` to the existing five variants and treats fact streams as admitted output-plane contracts, not hidden retained sidecars (`2C-grammar-neutrality.md:50`-`56`; `:128`-`:135`; `:141`-`:143`). 2D requires `backend_expr_language` to pass JSON/CSS equality with no new BIR or `BackendShape` variant without G-Omega, and rejects retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tapes unless Lock 1 is explicitly amended (`2D-cost-model.md:71`-`78`; `:92`-`:108`). |
| CH5-V5-08 | ACCEPT | 2E keeps host-arch primitives source-backed until row-local consumers and gates exist. Its hardware manifest requires the Lock 1 ownership fields, its source-present primitives must be wired/deleted/scalar-delegated/architecturally blocked, and PMULL/CSSC/UDOT/TBL-style routes remain non-shortlist without material-differential, consumer, comparator, rollback, and abort fields (`2E-host-arch-esoterica.md:115`-`139`; `:160`-`:199`). Cache hints are explicitly not standalone admissions (`2E-host-arch-esoterica.md:96`; `:237`-`:241`). |
| CH5-V5-09 | ACCEPT | 2F keeps parse-that ownership at the compile-time fact boundary. HIR/regex/scanner facts may feed the resolver, but runtime masks, classes, cursors, and scanner streams must stay transient in generated consumer loops (`2F-parse-that-gaps.md:38`-`41`; `:144`-`:149`). Its import boundary forbids new BIR variants, public substrate APIs, runtime parser-owned DFA state, persistent mask/class/cursor streams, sidecars, retained scanner caches, and `UnionTape`-like substrates without explicit Lock amendment (`2F-parse-that-gaps.md:162`-`:186`). |
| CH5-V5-10 | ACCEPT | The V4 REDRESS-slice table is routing, not a hidden dispatch license: a slice reaches S-P3 only when its blocker is cleared; otherwise it remains research evidence (`T-P2-V4-FOLD-ADDENDUM.md:60`-`64`). The PMULL/CSSC/union, source-present, cache-hint, and parse-that rows are non-shortlist or candidate-only behind exact consumer, matrix, delete-or-wire, and no-new-BIR/API blockers (`T-P2-V4-FOLD-ADDENDUM.md:74`-`79`). |

## Required Repairs

None for CH5 before V5 consolidation.

Carry forward to T-P3: the V3 ledger fields and V4 REDRESS-slice blockers must
remain hard manifest inputs. Any downstream wave that retains mask/class/cursor
streams, adds parser-owned runtime state, exposes a public substrate API,
introduces a second tape or `UnionTape`, or adds a new BIR / `BackendShape`
variant remains REVISE or REJECT unless the relevant lock is explicitly amended.

## Evidence Read

- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V4-CONSOLIDATED.md`
- `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
