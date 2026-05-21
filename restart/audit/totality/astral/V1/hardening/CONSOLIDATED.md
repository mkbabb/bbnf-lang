# Pass Omega V1 Hardening Consolidated

Pass: Pass Omega.
Cycle: V1 CHALLENGE.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 verdict for the Omega V1 substantive packet.
Output: this file.

## Verdict

`G-OMEGA-V1-CHALLENGE`: REVISE.

Acceptance rate: 3/6 = 50%.
Critical defects: 0.
Open REVISE dispositions: CH1, CH2, CH4.

Omega V1 is a valid substantive packet, but it cannot advance to CRUD or
G-Omega. CH1 found one unresolved citation in Ω-A; CH2 requires the Lock 14
non-JSON witness cardinality to be settled before fleet-wide grammar-neutral
wording; CH4 requires per-amendment cost ledgers in the Omega artifacts
themselves. CH3, CH5, and CH6 accept the REDRESS, hidden-coupling, and
next-tranche-impact posture.

| Lens | Disposition | Load-bearing finding | Blocks G-Omega |
|---|---|---|---|
| CH1 correctness | REVISE | ΩA-07 includes a non-resolving `passes/src/lib.rs:24-36` citation; rewrite it to cite `restart/ARCHITECTURE.md:1129` for the stale source claim and `skinny/crates/passes/src/lib.rs:28`-`50` for live code evidence. | yes |
| CH2 generality / Lock 14 | REVISE | Fleet-wide grammar-neutral wording must require one strict CSS L4 positive row plus both Sheets and BBNF-self fail-closed or generated-role witnesses; one negative control scopes the claim to witnessed grammars only. | yes |
| CH3 regression / REDRESS | ACCEPT | The packet preserves REDRESS 96/97/98, 119/120, and 121-127 as history/gates; no rejected route is silently reopened and row-plane truth is retained. | no |
| CH4 cost | REVISE | Ω-A, Ω-B, Ω-C/`locks-diff.md`, Ω-E, and Ω-F need local LOC budgets, propagation counts, risk classes, receivers, hard caps, and doc-vs-implementation splits. | yes |
| CH5 hidden coupling | ACCEPT | No parallel substrate, retained sidecar, Track 1/Track 2 collapse, public substrate API, new `BackendShape`, new BIR variant, or Lock 1 violation is introduced. | no |
| CH6 next-tranche impact | ACCEPT | Ω-F gives measurable G-Omega items and preserves the block on governance/source/RESULTS/REDRESS/SK-V13 W0 work before G-Omega plus skinny S-P3 convergence. | no |

## Required V2 Fold

Omega V2 must modify only the Omega V1 proposal artifacts under
`restart/audit/totality/astral/V1/` unless a later challenge explicitly expands
scope. It must not edit `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
`restart/locks/LOCKS.md`, `restart/HANDOFF.md`, `restart/MIGRATION.md`,
`restart/skinny/*.md`, source, generated runtime, gate outputs,
`skinny/RESULTS.md`, or `skinny/REDRESS.md`.

Required fold actions:

| Source lens | Required change | Affected artifacts |
|---|---|---|
| CH1 | Fix ΩA-07 citation hygiene while preserving the finding that the ARCHITECTURE implementation-status claim is stale and routes to CRUD-1. | `ΩA-coherence-audit.md` |
| CH2 | Resolve witness cardinality: fleet-wide Lock 14 transfer requires CSS L4 positive evidence plus both Sheets and BBNF-self negative-control/generated-role witnesses; one negative control is scoped only. | `locks-diff.md`, `ΩC-locks-amendments.md`, `ΩD-master-plan-reconciliation.md`, `master-plan-diff.md`, `ΩE-skinny-corpus.md` |
| CH2 | Make JSON/sonic and CSS/lightningcss/cssparser comparator names row metadata, not universal telemetry columns, when adding Sheets and BBNF-self witnesses. | `ΩE-skinny-corpus.md` |
| CH4 | Add local cost ledgers for Ω-A and Ω-B receiver families. | `ΩA-coherence-audit.md`, `ΩB-skinny-lessons.md` |
| CH4 | Fold the T-P3 3C cost ledger into Ω-C and mirror it in `locks-diff.md` as hunk metadata or a hunk cost table. | `ΩC-locks-amendments.md`, `locks-diff.md` |
| CH4 | Add a CRUD-5 per-surface cost ledger and split `bbnf-bench` implementation/gate obligations from document updates. | `ΩE-skinny-corpus.md` |
| CH4 | Add LOC/risk/propagation/hard-cap fields to ΩF-MIG and ΩF-HANDOFF rows; label decision-engine, generated-provider, primitive-manifest, and SIMD/ASM work as future implementation receivers, not CRUD-4 document work. | `ΩF-migration-handoff.md` |

## Evidence

- CH1: `restart/audit/totality/astral/V1/hardening/CH1.md`.
- CH2: `restart/audit/totality/astral/V1/hardening/CH2.md`.
- CH3: `restart/audit/totality/astral/V1/hardening/CH3.md`.
- CH4: `restart/audit/totality/astral/V1/hardening/CH4.md`.
- CH5: `restart/audit/totality/astral/V1/hardening/CH5.md`.
- CH6: `restart/audit/totality/astral/V1/hardening/CH6.md`.
- Substantive packet commit: `644b1fcbf`.

## Disposition

Proceed to Omega V2 fold. Do not prepare CRUD or present G-Omega from V1.
Pass Omega convergence still requires a later accepted challenge cycle with no
orphan unresolved REVISE.
