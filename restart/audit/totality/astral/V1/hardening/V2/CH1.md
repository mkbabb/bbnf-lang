# Pass Omega V2 CH1 Correctness

Pass: Pass Omega.
Cycle: V2 CHALLENGE.
Date: 2026-05-21.
Lens: CH1 Correctness.
Output: `restart/audit/totality/astral/V1/hardening/V2/CH1.md`.

## Verdict

ACCEPT.

The folded Omega packet at `234fca560` resolves the V1 CH1 blocker. The V1
challenge found one non-resolving citation in ΩA-07 and required the stale
ARCHITECTURE claim to cite `restart/ARCHITECTURE.md:1129` plus the live
`skinny/crates/passes/src/lib.rs:28`-`50` code path
(`restart/audit/totality/astral/V1/hardening/CH1.md:25`,
`restart/audit/totality/astral/V1/hardening/CH1.md:31`-`35`;
`restart/audit/totality/astral/V1/hardening/CONSOLIDATED.md:24`-`31`,
`restart/audit/totality/astral/V1/hardening/CONSOLIDATED.md:42`-`52`).
ΩA-07 now uses those resolving citations and preserves the CRUD-1 routing
without presenting `passes/src/lib.rs:24-36` as repository-root evidence
(`restart/audit/totality/astral/V1/ΩA-coherence-audit.md:23`,
`restart/ARCHITECTURE.md:1129`,
`skinny/crates/passes/src/lib.rs:28`-`50`).

## Findings

| Check | Disposition | Evidence | CH1 finding |
|---|---|---|---|
| CH1 authority | ACCEPT | PASS-OMEGA defines CH1 as file:line, commit SHA, and REDRESS correctness (`restart/prompts/pass-contracts/PASS-OMEGA.md:39`-`55`). ORCHESTRATOR requires claims to cite resolving file:line, commit, RESULTS row, or REDRESS entry (`restart/prompts/ORCHESTRATOR.md:74`-`88`). | Lens scope is correctly applied to the folded Omega packet. |
| V1 CH1 blocker folded | ACCEPT | V1 CH1 required replacing the non-resolving `passes/src/lib.rs:24-36` citation with `restart/ARCHITECTURE.md:1129` and `skinny/crates/passes/src/lib.rs:28`-`50` (`restart/audit/totality/astral/V1/hardening/CH1.md:25`, `restart/audit/totality/astral/V1/hardening/CH1.md:31`-`35`). The folded ΩA-07 row now uses those citations and cites current shape-choice spans at `skinny/crates/passes/src/lib.rs:387`-`434` and `skinny/crates/passes/src/lib.rs:446`-`506` (`restart/audit/totality/astral/V1/ΩA-coherence-audit.md:23`). | The exact V1 CH1 fold is present. |
| Citation resolution | ACCEPT | A packet-local citation scan over ΩA-ΩF plus `locks-diff.md` and `master-plan-diff.md` returned `citation-check ok`. The former bad string is confined to the historical V1 CH1/CONSOLIDATED records, while the folded substantive packet keeps only resolving ΩA-07 citations (`restart/audit/totality/astral/V1/ΩA-coherence-audit.md:23`). | No unresolved file:line anchor found in the folded Omega proposal artifacts for this lens. |
| Factual correctness of stale-reference repair | ACCEPT | ARCHITECTURE line 1129 still contains the stale `passes/src/lib.rs:24-36`, `shapes_for_json`, and `nominate_json` claim (`restart/ARCHITECTURE.md:1129`). Current `passes::compile` instead normalizes the grammar and derives materialization, shape facts, and recognizers from the normalized grammar (`skinny/crates/passes/src/lib.rs:28`-`50`). | ΩA-07's conclusion remains factual: the governance text is stale, and the live code evidence supports routing the repair to CRUD-1. |
| Commit and boundary facts | ACCEPT | The V1 substantive packet commit `644b1fcbf` and folded commit `234fca560` resolve. PASS-OMEGA requires the v+1 fold and CHALLENGE loop before CRUD/G-Omega (`restart/prompts/pass-contracts/PASS-OMEGA.md:76`-`94`), and G3 authorizes Pass Omega intake only, not governance/source/RESULTS/REDRESS/SK-V13 W0 work (`restart/audit/totality/p3/G3-PRESENTATION.md:62`-`68`). | The folded packet does not invent authority beyond the proposal-only Omega boundary. |
| G-Omega gating | ACCEPT | PASS-OMEGA requires G-Omega before any locks amendment merges and lists the presentation items (`restart/prompts/pass-contracts/PASS-OMEGA.md:96`-`110`). ORCHESTRATOR keeps G-Omega mandatory (`restart/prompts/ORCHESTRATOR.md:165`-`172`). ΩF still refuses missing consolidated verdicts, missing CRUD blockers, W0-before-G-Omega, weak comparator treatment, and source/gate edits without telemetry (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:80`-`86`, `restart/audit/totality/astral/V1/ΩF-migration-handoff.md:87`-`108`). | The packet preserves the proposal-only boundary and does not promote folded text into active governance. |

## Required Fold Items

None for CH1.

CH1 accepts the V2 fold. Remaining convergence depends on the other V2 lenses
and a consolidated Omega V2 verdict; this file does not authorize CRUD,
G-Omega presentation, governance edits, source edits, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, or SK-V13 W0.

## Evidence

- `rg -n "passes/src/lib.rs:24-36|non-resolving|ΩA-07|stale implementation defect|shapes_for_json|nominate_json" restart/audit/totality/astral/V1 -g '!hardening/CH1.md' -g '!hardening/CONSOLIDATED.md'` shows ΩA-07 cites the corrected `restart/ARCHITECTURE.md` and `skinny/crates/passes/src/lib.rs` anchors; the historical bad citation remains only in V1 hardening records.
- `git cat-file -e` confirmed `603308b3`, `20e5fe46`, `d37f1cc2`, `d4e1612b`, `726ab124`, `70e8348e`, `cae7b48b`, `644b1fcbf`, and `234fca560` resolve as commits.
- Packet-local file:line scan over ΩA-ΩF, `locks-diff.md`, and `master-plan-diff.md` returned `citation-check ok`.
- `git diff --check -- restart/audit/totality/astral/V1/hardening/V2/CH1.md` passed with no output.
