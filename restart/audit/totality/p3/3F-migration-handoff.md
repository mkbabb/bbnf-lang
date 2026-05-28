---
agent: 3F
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-28T07:51:21Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: "MIGRATION.md + HANDOFF.md"
proposed_deltas_count: 7
delta_summary:
  carried_from_prior_cycle: [3F-MH-001, 3F-MH-002, 3F-MH-003, 3F-MH-004, 3F-MH-005, 3F-MH-006, 3F-MH-007]
  removed: []
  answered: [CH1-V1-002, CH4-COST-05, CH6-V1-02]
  newly_added: []
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: [CH1-V1-002, CH4-COST-05, CH6-V1-02]
---

# 3F - MIGRATION + HANDOFF + Next-Cycle Dispatch

## Executive Summary

This artifact proposes only. It does not amend `restart/MIGRATION.md` or `restart/HANDOFF.md`. The top-level V1 surfaces still present SK-V14 Pass Omega V8 W5B-FRONTENDR as current authority (`restart/HANDOFF.md:5`, `restart/HANDOFF.md:70`) while the live SK-V15 packet says JSON is honest, CSS L4 is contrived, Pattern H is not collapsed, and Decision Engine is scaffold (`restart/skinny/tranches/sk-v15/HANDOFF.md:8`-`11`). T-P1 must be carried as CLEAN-FINAL / G1-AUTO-PINNED rather than normal two-clean-cycle 3Z (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21`-`28`); T-P2 is a normal 3Z lock (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15`-`19`). V2 folds the V1 3F REVISE findings by routing current SK-V15 authority to the extant `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` and by making CRUD-4 cap handling executable. The next directive is therefore: T-P3 locks, Pass Omega V5 runs, CRUD-4 applies the authorized MIGRATION/HANDOFF current-state cleanup or records a blocked/extension decision with exact remainder/receiver/blocker/gate, G-Omega closes, and only then SK-V15 W0 implementation dispatch may begin.

## V2 Delta Summary

| bucket | delta ids | note |
|---|---|---|
| Carried from prior cycle | 3F-MH-001..3F-MH-007 | All seven V1 proposal deltas remain proposal-only. V2 revises 3F-MH-005 and 3F-MH-007 to fold CH1/CH4/CH6 REVISE findings. |
| Removed | none | No prior 3F delta is removed. |
| Answered | CH1-V1-002, CH4-COST-05, CH6-V1-02 | The absent skinny companion-prompt route is answered by the extant `DISPATCH-PROMPT.md`, and CRUD-4 follow-up cleanup is replaced with a blocked/extension protocol. |
| Newly added | none | V2 folds hardening dispositions into the existing seven deltas rather than creating new proposal IDs. |

## Proposed Delta Table

| delta id | proposed delta | source T-P1/T-P2 finding-id cited | affected V1 surface section | receiver / blocker / gate | rationale |
|---|---|---|---|---|---|
| 3F-MH-001 | Insert a current **SK-V15 Pass Omega V5 Migration Receiver** before `restart/MIGRATION.md` section 0.1. Keep existing Pass Omega V2..V8 sections as historical SK-V14 receiver records, not current SK-V15 dispatch authority. | T-P1 `COH-001` says top-level surfaces still route from SK-V14 while SK-V15 is open (`restart/audit/totality/p1/1F-coherence-scan.md:74`). T-P3 states T-P3 feeds Pass Omega V5 and does not edit V1 surfaces (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:55`-`57`). | `restart/MIGRATION.md` before `## 0.1 Pass Omega V2 Migration Receiver` (`restart/MIGRATION.md:30`); current historical sections occupy `restart/MIGRATION.md:30`-`144`. | Receiver: Pass Omega V5 CRUD-4. Blocker: T-P3 and Pass Omega convergence. Gate: G-Omega plus CRUD-4 log. | Avoids Pass Omega V5 name collision with the historical SK-V14 W5R section at `restart/MIGRATION.md:84`-`97` and gives downstream agents a current SK-V15 entry point. |
| 3F-MH-002 | Add a MIGRATION receiver table mapping SK-V15 implementation obligations to W0-W11 dependency rows: W0 telemetry, W1 CSS broadcast demotion, W2 Lock 14/16 gate restoration, W3 codegen leaks, W4 Pattern H, W5 CSS typed Value provider, W6 same-workload CSS retime, W7 Decision Engine, W8/W9 lowerers, W10 FNV quarantine, W11 PASS-IMPL V2 close. | T-P1 `RC-01`..`RC-11` enumerate receiver/gate rows (`restart/audit/totality/p1/1D-skinny-lessons.md:172`-`184`). T-P2 locks CSS, Lock 14/16, Decision Engine, and runtime-regex blockers as Pass Omega inputs (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:46`-`62`). | `restart/MIGRATION.md` section 0 current receiver plus cross-reference to section 17 sequence (`restart/MIGRATION.md:794`-`814`) and section 20 single carry-truth ledger (`restart/MIGRATION.md:919`-`932`). | Receiver: Pass Omega V5 CRUD-4 for docs; SK-V15 W0-W11 after G-Omega for implementation. Blocker: missing dependency-row proof. Gate: `restart/skinny/tranches/sk-v15/SPEC.md` dependency table and exit gates (`restart/skinny/tranches/sk-v15/SPEC.md:187`-`204`). | Makes migration route from actual SK-V15 proof rows instead of stale SK-V14 W5B routing. |
| 3F-MH-003 | Add a MIGRATION gate clause: no delete, retirement, provider/template removal, old CSS proof retirement, or runtime-shim deletion may happen before its rebuild provider lands no later than the delete wave. | T-P1 `C-7` identifies REDRESS-183/184/209..213 as wave-graph-cycle precedent (`restart/audit/totality/p1/1D-skinny-lessons.md:158`-`159`). SK-V15 Synthesis requires delete/rebuild dependency columns (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:102`-`106`). | `restart/MIGRATION.md` sections 17 and 19 gates (`restart/MIGRATION.md:794`-`814`, `restart/MIGRATION.md:833`-`917`). | Receiver: every migration deletion/retirement row. Blocker: absent provider proof or absent dependency row. Gate: SK-V15 dependency rows `DEP-W6-CSS-GENERATED-RS`, `DEP-W6-CSS-SUMMARY-FACT-STREAM`, `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W4-PATTERN-H-PROVENANCE`, and `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM` (`restart/skinny/tranches/sk-v15/SPEC.md:195`-`199`). | Prevents the V3/V4/V5/V6/V7/V8 delete-before-provider failure pattern from re-entering under SK-V15. |
| 3F-MH-004 | Add a MIGRATION/HANDOFF governance paragraph carrying T-P1 as clean-final/G1-auto-pinned, not normal 3Z, and T-P2 as normal 3Z. | T-P1 hardening says V5 is 100% ACCEPT but not normal two-clean 3Z because V4 was REVISE and V5 is ceiling (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:13`-`28`, `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:46`-`50`). T-P2 hardening says V2+V3 are normal consecutive clean cycles (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:33`-`42`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:68`-`77`). | `restart/MIGRATION.md` current receiver and `restart/HANDOFF.md` current totality override (`restart/HANDOFF.md:3`-`28`). | Receiver: Pass Omega V5 CRUD-4. Blocker: any text that rewrites T-P1 as normal 3Z. Gate: G-Omega. | Satisfies the T-P3 dispatch requirement to carry T-P1 clean-final/G1 and T-P2 normal 3Z honestly (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:41`-`48`). |
| 3F-MH-005 | Replace the top-level HANDOFF current override with SK-V15 state: PASS-IMPL V1 split truth, S-P3 V4 locked W0-W11 contract, T-P1/T-P2 governance, Pass Omega V5 as the next totality receiver, and current SK-V15 wave authority routed through the extant `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. Do not cite a missing skinny companion prompt as current authority unless a separate owning task creates it before citation. | T-P1 `COH-001` and `COH-003` flag stale top-level authority and Omega V8 sequence drift (`restart/audit/totality/p1/1F-coherence-scan.md:74`-`76`). SK-V15 HANDOFF states the live split truth and W0-W11 contract (`restart/skinny/tranches/sk-v15/HANDOFF.md:8`-`18`). T-P3 context currently names a non-existent SK-V15 wave-plan authority path (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:34`), while the extant dispatch contract names itself as the S-P3 V4 locked W0-W11 authority and lists the required read order (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:5`-`27`). | `restart/HANDOFF.md` current totality override and current authority list (`restart/HANDOFF.md:3`-`107`). | Receiver: Pass Omega V5 CRUD-4. Blocker: stale SK-V14 W5B.0 dispatch authority or a citation to a missing skinny companion prompt. Gate: G-Omega closure plus CRUD-4 application; SK-V15 W0 pre-dispatch verification consumes `DISPATCH-PROMPT.md`. | Cold-start agents should land on SK-V15 T-P3/Pass Omega V5, then the extant SK-V15 `DISPATCH-PROMPT.md`, not historical SK-V14 implementation dispatch or a missing authority path. |
| 3F-MH-006 | Add a top-level HANDOFF blocker matrix matching SK-V15 exact blockers to receiver/gate: CSS broadcast -> W1, CSS_GENERATED_RS/fact stream -> W5/W6, Lock 14/16 -> W2, codegen leaks -> W3, Pattern H -> W4, Decision Engine -> W7, lowerers -> W8/W9, FNV -> W10, close/no-orphans -> W11. | T-P1 `COH-004`..`COH-010` name gate-exclusion, Pattern H, Decision Engine, lowerer, CSS_GENERATED_RS, and preblock gaps (`restart/audit/totality/p1/1F-coherence-scan.md:77`-`83`). SK-V15 HANDOFF already lists exact blockers (`restart/skinny/tranches/sk-v15/HANDOFF.md:35`-`48`). | `restart/HANDOFF.md` after current state and before next move (`restart/HANDOFF.md:109`-`147` currently holds SK-V14 bar and dispatch rule). | Receiver: SK-V15 W0-W11 after Pass Omega V5. Blocker: any unresolved row. Gate: each SPEC exit gate (`restart/skinny/tranches/sk-v15/SPEC.md:246`-`465`). | Makes the top-level handoff executable and avoids prose-only "ready" claims. |
| 3F-MH-007 | Replace the current "Pass Omega V8 next-cycle dispatch directive" with a **Pass Omega V5/G-Omega -> SK-V15 W0** directive: T-P3 locks, G3 auto-passes under active pin, Pass Omega V5 runs, CRUD-4 updates HANDOFF/MIGRATION current-state truth or records a blocked/extension decision with exact remainder, receiver, blocker, and gate, G-Omega authorizes required V1 patches, then W0 dispatches through SKINNY triumvirate. Implementation waves remain blocked until CRUD-4 current-state truth is complete and G-Omega authorizes required spec patches. | T-P3 3F row requires next directive after Pass Omega V5 and says implementation waves do not begin until Pass Omega CRUD closes and G-Omega authorizes patches (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:118`-`121`). Pass Omega CRUD-4 owns HANDOFF+MIGRATION (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`-`74`) and G-Omega controls merge/next-cycle dispatch (`restart/prompts/pass-contracts/PASS-OMEGA.md:94`-`110`). PASS-3 and ORCHESTRATOR hard-cap rules require overruns to surface as extension decisions, not silent deferrals (`restart/prompts/totality/PASS-3-SYNTHESIS.md:202`-`206`, `restart/prompts/ORCHESTRATOR.md:224`-`227`). | `restart/HANDOFF.md` next-cycle directive (`restart/HANDOFF.md:149`-`177`). | Receiver: Pass Omega V5 CRUD-4, then SK-V15 W0 only after G-Omega. Blocker: no CRUD-4, no G-Omega, unresolved invariant, or incomplete current-state cleanup without blocked/extension record. Gate: G-Omega; then W0 entry gate in SK-V15 SPEC (`restart/skinny/tranches/sk-v15/SPEC.md:29`-`43`, `restart/skinny/tranches/sk-v15/SPEC.md:488`-`494`). | Gives the next worker a concrete, measurable dispatch path, prevents direct implementation dispatch from T-P3 prose, and closes the CRUD-4 follow-up-cleanup deferral aperture. |

## Proposal-Only Text Carriers

The following carriers are not applied here. They are suggested content shapes for Pass Omega V5 CRUD-4.

### MIGRATION Carrier

```md
## 0.0 Current SK-V15 Pass Omega V5 Migration Receiver

Status: proposal-only until Pass Omega V5 converges, CRUD-4 applies, and G-Omega closes.
T-P1 is CLEAN-FINAL / G1-AUTO-PINNED, not a normal two-clean-cycle 3Z lock.
T-P2 is a normal two-clean-cycle 3Z lock. SK-V15 implementation remains blocked
until this receiver is applied by CRUD-4 and G-Omega authorizes the required V1
patches.

| Receiver | Migration rule | Blocker | Gate |
|---|---|---|---|
| W0 telemetry | Capture SK-V15-open JSON guard and CSS diagnostic broadcast state. | missing gate-consumed telemetry | W0 exit gate |
| W1 CSS admission honesty | Demote/collapse W8R broadcast; no CSS live admit from shared tuple. | reused broadcast row | DEP-W1-CSS-BROADCAST |
| W2 Lock 14/16 restoration | Full-surface scan roots and exclusion reports are gate-consumed. | self-exempting scan | W2 exit gate |
| W3 codegen leaks | Remove one coherent generic leak family with non-JSON receiver proof. | generic grammar branch | DEP-W3-W6-CSS-PROVIDER-TEMPLATE |
| W4 Pattern H provenance | 67 runtime files gain true provenance and regen/check proof; no header-only close. | fake generated status | DEP-W4-PATTERN-H-PROVENANCE |
| W5/W6 CSS typed provider + retime | Typed CSS value/document/view/visitor lands before old CSS proof retires. | fact-stream or brace-counter proof | DEP-W6-CSS-GENERATED-RS / DEP-W6-CSS-SUMMARY-FACT-STREAM |
| W7-W9 Decision/lowerers | Decision Engine and all five BackendShape lowerers become load-bearing or gate-rejected. | zero rewrites, tautological CSP, marker lowerers, sixth shape | DEP-W7/8/9 |
| W10 FNV quarantine | FNV remains bench-only; no production arbiter. | production FNV correctness proof | DEP-W10-FNV-QUARANTINE |
| W11 close | PASS-IMPL V2 consumes every dependency row. | orphan row | DEP-W11-CLOSE-NO-ORPHANS |
```

### HANDOFF Carrier

```md
## Current Totality Override - 2026-05-28

Status: SK-V15 T-P3 synthesis is active. T-P1 closed clean-final/G1-auto-pinned,
not normal 3Z. T-P2 closed normal 3Z. After T-P3 cohort lock, G3 auto-passes
under the active non-G-Omega gate pin and the packet flows into Pass Omega V5.
No SK-V15 implementation wave dispatches until Pass Omega V5 CRUD-4 has updated
HANDOFF/MIGRATION and G-Omega has authorized the required V1 patches.
Current SK-V15 wave authority routes through
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`; do not cite a missing
skinny companion prompt as current authority unless a separate owning task
creates that file before citation.

CRUD-4 cap handling: before G-Omega, CRUD-4 either completes current-state
HANDOFF/MIGRATION cleanup, or records a blocked/extension decision naming the
exact remainder, receiver, blocker, and gate. Any remainder touching current
dispatch truth blocks SK-V15 W0 until resolved.

Next directive after Pass Omega V5/G-Omega: dispatch SK-V15 W0 through the
SKINNY triumvirate; then follow W1..W11 in SPEC order, preserving dependency
rows and stopping on any unresolved invariant.
```

## Consequences

| delta id | positive consequence | cost / risk | propagation |
|---|---|---|---|
| 3F-MH-001 | Current migration authority starts at SK-V15 instead of SK-V14 history. | 25-45 doc LOC; low risk if historical sections are not renumbered. | 1 surface: MIGRATION. |
| 3F-MH-002 | Every SK-V15 implementation obligation gets a receiver/blocker/gate row. | 80-140 doc LOC; medium risk due table density. | 2 surfaces: MIGRATION plus HANDOFF cross-link. |
| 3F-MH-003 | Delete-before-provider cycles are blocked in migration, not rediscovered during waves. | 20-40 doc LOC; medium risk if duplicated inconsistently. | 3 surfaces: MIGRATION, HANDOFF, SK-V15 SPEC reference. |
| 3F-MH-004 | Governance history stays honest and survives Pass Omega. | 10-25 doc LOC; low risk. | 2 surfaces: MIGRATION and HANDOFF. |
| 3F-MH-005 | Cold-start handoff routes to SK-V15 T-P3/Pass Omega V5 and the extant SK-V15 `DISPATCH-PROMPT.md`. | 125-230 doc LOC; medium risk because it replaces a long current-state block and must avoid the absent authority path. | 2 surfaces: HANDOFF and Pass Omega V5 CRUD-4 log. |
| 3F-MH-006 | Blockers become executable next-work rows, not prose. | 60-110 doc LOC; low-medium risk. | 2 surfaces: HANDOFF and SK-V15 SPEC references. |
| 3F-MH-007 | Prevents implementation waves from starting before V1 patch authorization or before current-state cleanup is closed/extension-routed. | 45-90 doc LOC; low risk. | 3 surfaces: HANDOFF, MIGRATION, Pass Omega V5 CRUD-4. |

## Next-Cycle Dispatch Directive

1. T-P3 V2 cohort completes 3A..3F, then CH1..CH7 hardening iterates until T-P3 lock or V5 ceiling. The active dispatch context says G3 auto-passes on cohort lock and only G-Omega requires user relinquish (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:9`-`11`, `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:123`-`129`).
2. On T-P3 lock, dispatch Pass Omega V5. Pass Omega consumes the latest totality cycle and skinny REDRESS/RESULTS into V1 spec surfaces (`restart/prompts/pass-contracts/PASS-OMEGA.md:3`-`5`) and specifically assigns HANDOFF+MIGRATION to CRUD-4 (`restart/prompts/pass-contracts/PASS-OMEGA.md:63`-`69`).
3. Pass Omega V5 CHALLENGE must converge before CRUD. CRUD must stay within the consolidated authorization; no CRUD agent edits beyond what CHALLENGE authorizes (`restart/prompts/pass-contracts/PASS-OMEGA.md:72`-`74`, `restart/prompts/pass-contracts/PASS-OMEGA.md:92`-`94`).
4. CRUD-4 resolves current-state HANDOFF/MIGRATION cleanup before G-Omega. If the cap blocks that cleanup, CRUD-4 records a blocked/extension decision naming the exact remainder, receiver, blocker, and gate; any remainder touching current dispatch truth blocks SK-V15 W0 until complete (`restart/prompts/totality/PASS-3-SYNTHESIS.md:202`-`206`, `restart/prompts/ORCHESTRATOR.md:224`-`227`).
5. G-Omega then authorizes the required V1 patches. After G-Omega closes, the V1 spec is v+1 and the next cycle dispatches per the Omega-F directive (`restart/prompts/pass-contracts/PASS-OMEGA.md:96`-`110`).
6. Only after Pass Omega V5 CRUD-4 current-state truth is complete, G-Omega has authorized the HANDOFF/MIGRATION patches, and SK-V15 authority routes through the extant `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`, may the orchestrator update HANDOFF to `ready-for-wave-W0` and dispatch SK-V15 W0 (`restart/skinny/tranches/sk-v15/SPEC.md:488`-`494`). W0..W11 then follow the dependency order in the dispatch lock (`restart/skinny/tranches/sk-v15/SPEC.md:29`-`43`) and the manifest (`restart/skinny/tranches/sk-v15/SPEC.md:172`-`185`).
7. W11 cannot close if any dependency row lacks proof, REDRESS route, revert evidence, or intrinsic-block proof; SK-V16 routing is remainder after proof, not a substitute for SK-V15 repair (`restart/skinny/tranches/sk-v15/SPEC.md:447`-`465`).

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 / CH6 | The current corpus already has a historical "Pass Omega V5 W5R" section (`restart/MIGRATION.md:84`-`97`) while SK-V15 dispatch calls the next astral pass "Pass Omega V5" (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:118`-`121`). Should CRUD-4 label the current pass "SK-V15 Pass Omega V5" and mark the older section "historical SK-V14 Pass Omega V5 W5R" to avoid false citation? | Pass Omega V5 CRUD-4. | Name collision between current SK-V15 Pass Omega V5 and historical SK-V14 Pass Omega V5 W5R. | G-Omega sign-off text and CRUD log. |
| CH3 | Does every proposed HANDOFF shortcut still preserve the pre-block list, especially REDRESS 183/184/209-213, 215, and FNV production migration (`restart/skinny/tranches/sk-v15/SPEC.md:467`-`484`)? | SK-V15 W0-W11 plans. | Reopened old route or omitted pre-block route. | CH3/CH7 during wave plan. |
