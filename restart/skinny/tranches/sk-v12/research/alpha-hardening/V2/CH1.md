# SK-V12 Alpha Hardening V2 - CH1 Correctness

Date: 2026-05-20.

Scope: CH1 correctness re-check for Pass Alpha SK-V11 -> SK-V12 after
`18f4b931` folded the V1 hardening revisions. Reviewed V1 CH1, V1
CONSOLIDATED, revised SK-V12 `SYNTHESIS.md` and `HANDOFF.md`, and Alpha A-F.

## Disposition

ACCEPT.

The V1 CH1 blockers are folded. The packet now gives local result/table
authority for the load-bearing Alpha-A extraction and top-level current-state
summaries, carries the guard-floor formula provenance beside the copied guard
tables, and removes the unsupported E1-E3 `>= 1 Mbps` / `>= 100 samples`
thresholds in favor of finite positive throughput plus recorded sample count.

This is acceptance of the Pass Alpha packet for CH1 correctness only. S-P1/S-P2
and later S-P3 behavior redress still have to produce fresh generated non-JSON
evidence, gate consumption, oracle independence, strict equality, and guard
preservation before any row movement can admit.

## V1 Fold Verification

| V1 CH1 fold | V2 status | Evidence |
|---|---|---|
| Add result/table citations for Alpha-A extracted surfaces and row movement claims. | ACCEPT | Alpha-A now anchors the close surface to `skinny/RESULTS.md:5-45`, gate metadata, overall notes, and REDRESS 119/120; its parse, direct, and typed tables each carry compact result-line sources, with the direct table also citing REDRESS 119 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md:22-25`, `:67-71`, `:95-101`, `:125-128`). |
| Add compact REDRESS/RESULTS citations to SK-V12 `SYNTHESIS.md` and `HANDOFF.md` current-state summaries. | ACCEPT | `SYNTHESIS.md` cites the current result surface, overall advisory notes, REDRESS 119/120, and the direct residual table beside the summaries (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:83-100`). `HANDOFF.md` adds the same compact anchors to the current-state block (`restart/skinny/tranches/sk-v12/HANDOFF.md:39-44`). |
| Cite or restate guard-floor formula provenance beside guard-floor tables. | ACCEPT | `SYNTHESIS.md` now cites SK-V11 SPEC Section 0.5 and restates the direct and typed formulas beside the guard tables (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:118-134`). Alpha-F also restates the direct and typed formula source beside its guard table (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:139-143`). The cited formulas match SK-V11 SPEC Section 0.5 (`restart/skinny/tranches/sk-v11/SPEC.md:147-172`). |
| Replace unsupported E1-E3 `>= 1 Mbps` and `sample count >= 100` baseline thresholds. | ACCEPT | Alpha-E now requires finite positive generated Track 1 and oracle/Track 2 Mbps plus sample count recorded from the bench artifact for E1, E2, and E3 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:102-113`, `:154-164`, `:201-212`). A search found no remaining E1-E3 `>= 1 Mbps`, `1 Mbps`, or `>= 100` baseline threshold. |

## Correctness Findings

No blocking CH1 correctness finding remains.

The revised packet preserves the V1 accepted plane discipline: parse-only rows
remain diagnostic; direct rows stay on the digest plane with same-run
sonic-rs/serde direct anchors; typed rows stay on the typed direct plane; and
absent C++ direct/typed sidecars are not promoted into SOTA evidence
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:30-58`,
`:96-130`).

The direct residual reopen posture remains consistent with REDRESS 119/120.
Alpha-C, Alpha-D, `SYNTHESIS.md`, and `HANDOFF.md` all carry the 13 direct rows
as a measured fixpoint/reopen ledger, not as first-wave SK-V12 targets
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:49-71`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md:228-251`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:97-116`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:39-44`).

The E4 intervention threshold remains measurable because it is defined from the
admitted selected baseline, not from the removed unsupported baseline floors:
`ceil(W1_selected_baseline_mbps * 1.01)` with finite positive independent
oracle/Track 2 Mbps and 100% fixture equality
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:225-274`;
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:84-91`,
`:182-186`).

## Residual CH1 Risk

The only residual risk is implementation-time, not a Pass Alpha CH1 blocker:
future waves must still prove generated Track 1 provenance, independent
same-plane oracle/Track 2, strict equality, finite measured throughput, sample
telemetry, and same-wave gate consumption before any non-JSON row can admit.
The revised Alpha packet states those fail-closed obligations in Alpha-E,
Alpha-F, `SYNTHESIS.md`, and `HANDOFF.md`.

## Changed Path

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CH1.md`
