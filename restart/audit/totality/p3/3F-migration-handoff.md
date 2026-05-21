---
agent: "3F"
pass: "T-P3-synthesis"
cycle: "V3"
generated_at: "2026-05-21T19:46:41Z"
t_p1_inventories_consumed:
  - "1A-substrate-evidence"
  - "1B-codegen-evidence"
  - "1C-runtime-evidence"
  - "1D-skinny-lessons"
  - "1E-locks-evidence"
  - "1F-coherence-scan"
  - "1F-anti-pattern"
  - "1F-past-corpora"
  - "HARDENING-T-P1-V5-CONSOLIDATED"
t_p2_dossiers_consumed:
  - "2A-sota-landscape"
  - "2B-primitive-vocabulary"
  - "2C-grammar-neutrality"
  - "2D-cost-model"
  - "2E-host-arch-esoterica"
  - "2F-parse-that-gaps"
  - "T-P2-V2-FOLD-ADDENDUM"
  - "T-P2-V3-FOLD-ADDENDUM"
  - "T-P2-V4-FOLD-ADDENDUM"
  - "HARDENING-T-P2-V5-CONVERGED"
v1_surface_targeted:
  - "restart/MIGRATION.md"
  - "restart/HANDOFF.md"
proposed_deltas_count: 11
delta_summary:
  carried_from_prior_cycle:
    - "SK-V12 CSS L4 A/Go admission folded into top-level handoff and migration telemetry routing"
    - "T-P1 Lock 1, Lock 14, Lock 16, and root-workspace drift findings folded into migration fates"
    - "T-P2 primitive, cost-model, grammar-neutrality, and source-present gates folded into migration/handoff entry conditions"
  removed:
    - "Stale HANDOFF top-level N-direct/NoGo and two-surface grammar-onboarding wording"
    - "Rename-only treatment for simd/path legacy crates without archive/removal proof"
  answered:
    - "Whether T-P3 may edit V1 surfaces directly: no, proposal only until Pass Omega CRUD and user gate"
    - "Whether SK-V13 Wave 0 may start from skinny CSS admission alone: no, G-Omega and S-P3 remain preconditions"
  newly_added:
    - "3F-MIG-001 through 3F-MIG-006"
    - "3F-HANDOFF-001 through 3F-HANDOFF-005"
prior_cycle_dispositions_folded:
  accepted:
    - "G-T-P1 PASS"
    - "G-T-P2 PASS"
    - "SK-V12 CSS L4 PASS-ADMIT"
    - "G-T-P3-V2 CH2 ACCEPT"
    - "G-T-P3-V2 CH3 ACCEPT"
    - "G-T-P3-V2 CH4 ACCEPT"
    - "G-T-P3-V2 CH5 ACCEPT"
    - "G-T-P3-V2 CH6 ACCEPT"
  rejected:
    - "REDRESS-96/97/98 union-substrate preservation without material differential"
    - "Producer-only or orphan SIMD/source-present primitives as admission"
  revised:
    - "Lock 1 substrate closure narrowed to admitted grammar-runtime substrate, with fenced non-substrate fact streams"
    - "Lock 14 grammar-name exception narrowed to generated or rostered surfaces"
    - "G-T-P3-V2 CH1 stale-cycle wording and prompt citation hygiene"
---

## Executive Summary

3F proposes eleven V1-surface deltas: six for `restart/MIGRATION.md` and five for `restart/HANDOFF.md`. T-P3 is explicitly proposal-only: it may synthesize deltas, but V1 spec edits land through Pass Omega CRUD and user gate flow, not directly from this file (`restart/prompts/totality/PASS-3-SYNTHESIS.md:21`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:197`). The current HANDOFF already authorizes T-P3 while forbidding direct edits to `MIGRATION.md` and `HANDOFF.md` (`restart/HANDOFF.md:5`, `restart/HANDOFF.md:44`). The proposed migration delta replaces rename-only and hardcoded-grammar fates with generated-provider, source-present, cost-model, and archive-proof gates grounded by T-P1/T-P2 convergence (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:41`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:47`). The proposed handoff delta updates top-level state from T-P3 entry to G3/Omega dispatch readiness while preserving SK-V13's G-Omega-before-W0 block (`restart/skinny/tranches/sk-v13/HANDOFF.md:54`, `restart/skinny/tranches/sk-v13/HANDOFF.md:85`).

## V3 Delta Summary

| bucket | summary | evidence |
| --- | --- | --- |
| carried | SK-V12 CSS L4 is an admitted A/Go non-JSON row and must be carried into the migration and handoff state. | `skinny/RESULTS.md:94`, `skinny/RESULTS.md:145`, `skinny/REDRESS.md:3824` |
| carried | T-P1 and T-P2 convergence packets are authoritative inputs for T-P3 synthesis, but neither packet directly amends governance surfaces. | `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:37`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:49` |
| removed | Historical HANDOFF wording that reports N-direct/NoGo and two-surface onboarding is stale against current RESULTS and Lock 14 evidence. | `restart/HANDOFF.md:75`, `restart/HANDOFF.md:110`, `restart/audit/totality/p1/1F-coherence-scan.md:33`, `restart/audit/totality/p1/1F-coherence-scan.md:34` |
| removed | Migration rows that treat legacy path/SIMD crates as rename/defer items are insufficient without archive/removal and source-present gates. | `restart/MIGRATION.md:70`, `restart/MIGRATION.md:75`, `restart/audit/totality/p1/1E-locks-evidence.md:105`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:258` |
| answered | T-P3 output is the input to G3 and Pass Omega, not a direct edit authority for `MIGRATION.md` or `HANDOFF.md`. | `restart/prompts/totality/PASS-3-SYNTHESIS.md:179`, `restart/prompts/pass-contracts/PASS-OMEGA.md:57`, `restart/prompts/pass-contracts/PASS-OMEGA.md:76` |
| new | The next-cycle directive must specify measurable entry conditions for Omega CRUD and post-G-Omega dispatch. | `restart/prompts/totality/PASS-3-SYNTHESIS.md:127`, `restart/prompts/pass-contracts/PASS-OMEGA.md:96`, `restart/prompts/pass-contracts/PASS-OMEGA.md:110` |

## Proposed Delta Table

| proposed delta | source finding-id | affected V1-surface section | rationale |
| --- | --- | --- | --- |
| 3F-MIG-001: Replace rename/defer-only rows for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, and `bbnf-path-ts` with archive/removal-proof gates, retaining any V2 path emitter only as a separately reconstituted generated surface. | 1E-LAC-06, T2B-ABROGATE | `restart/MIGRATION.md` crate-disposition tables and aggregate disposition summary | `restart/MIGRATION.md` still lists `bbnf-path-ts` as deferred and `simd-scan` as renamed while `ser`/`gorgeous` are archive rows (`restart/MIGRATION.md:70`, `restart/MIGRATION.md:75`, `restart/MIGRATION.md:77`). T-P1 says archive/removal proof is required for these root crates (`restart/audit/totality/p1/1E-locks-evidence.md:105`), and T-P2 says abrogate gates require same-wave consumer, strict proof, and source-state closure rather than orphan retention (`restart/audit/totality/p2/2B-primitive-vocabulary.md:258`). |
| 3F-MIG-002: Add an explicit generated-provider/roster migration row that replaces hardcoded grammar registries, runtime profiles, root aliases, and hand-coded grammar provider branches. | P1-1B-D7, P1-1C-D1, T2C-LOCK14 | `restart/MIGRATION.md` mixed-fate crosswalk, `bbnf-codegen`, and runtime rows | T-P1 found runtime profiles hardcoded to JSON/CSS (`restart/audit/totality/p1/1B-codegen-evidence.md:47`) and root runtime exports that hardcode grammar names (`restart/audit/totality/p1/1C-runtime-evidence.md:58`). T-P2 requires generic crates to consume generated registry/config/facts rather than grammar branches (`restart/audit/totality/p2/2C-grammar-neutrality.md:91`, `restart/audit/totality/p2/2C-grammar-neutrality.md:132`). |
| 3F-MIG-003: Reclassify hand-owned JSON/CSS sinks, scan modules, root tests, and proof witnesses as generated, rostered, archived, or fixture-only; no hand-owned per-grammar runtime surface may remain in root generic crates. | P1-1C-D2, P1-1C-D7, LAC-1E-08 | `restart/MIGRATION.md` runtime/codegen/proof-fixture rows | T-P1 found hand-written per-grammar runtime files and root proof/test leaks (`restart/audit/totality/p1/1C-runtime-evidence.md:59`, `restart/audit/totality/p1/1C-runtime-evidence.md:83`). T-P1's Lock 14 amendment candidate permits generated non-JSON surfaces only behind rostered generated criteria (`restart/audit/totality/p1/1E-locks-evidence.md:107`), and T-P2 narrows generated sink/flag surfaces to generated metadata and verified closure (`restart/audit/totality/p2/2C-grammar-neutrality.md:186`). |
| 3F-MIG-004: Add a non-JSON telemetry/fact-output migration plane for CSS L4 instead of forcing CSS evidence into EventTape or retained substrate categories. | 1A-DIV-006, 1C-D5, SKV12-CSS-L4 | `restart/MIGRATION.md` runtime, bench, and generated-artifact crosswalk | T-P1 says CSS L4 fact-stream evidence is admitted but V1 lacks a category (`restart/audit/totality/p1/1A-substrate-evidence.md:46`, `restart/audit/totality/p1/1A-substrate-evidence.md:57`), and runtime evidence records CSS fact streams without EventTape (`restart/audit/totality/p1/1C-runtime-evidence.md:71`, `restart/audit/totality/p1/1C-runtime-evidence.md:72`). SK-V12 admitted the CSS L4 direct row with strict equality and lightningcss comparison (`skinny/RESULTS.md:94`, `skinny/REDRESS.md:3836`). |
| 3F-MIG-005: Replace P1-P8 cascade and thin `CostFacts` migration wording with a decision-engine tranche covering candidate generation, eqsat, CSP, active cost extraction, and conditional parse-that/regex imports. | T2D-DECISION, T2F-IMPORT | `restart/MIGRATION.md` `bbnf-codegen`, regex/cost-model, and deletion rows | T-P2 says P1-P8 is not an optimizer and the replacement decision engine includes candidate generation, eqsat, CSP, and active cost extraction (`restart/audit/totality/p2/2D-cost-model.md:25`, `restart/audit/totality/p2/2D-cost-model.md:30`). Parse-that HIR use is conditional and cannot become an opaque runtime dependency (`restart/audit/totality/p2/2F-parse-that-gaps.md:173`, `restart/audit/totality/p2/2F-parse-that-gaps.md:249`), while SK-V13 requires cascade deletion or gated retirement in G2 (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:59`). |
| 3F-MIG-006: Add a Lock 16 primitive/source-present manifest gate to all SIMD, ASM, and source-backed primitive migrations; rename to `bbnf-simd` is insufficient without wired consumer, scalar delegate, delete, or architectural block. | T2B-L16, T2E-SOURCE, T-P2-V4-NONSHORTLIST | `restart/MIGRATION.md` `bbnf-simd`, hardware, primitive, and source-present rows | T-P2 defines Lock 16 manifest fields and source-present states (`restart/audit/totality/p2/2B-primitive-vocabulary.md:196`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:215`). Host-architecture research adds hardware manifest fields and allowed source-present states (`restart/audit/totality/p2/2E-host-arch-esoterica.md:115`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:160`). T-P2 V4 marks rows without commands or first consumers as non-shortlist blockers (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:41`, `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`). |
| 3F-HANDOFF-001: Replace the current top-level state with "T-P3 synthesis queued for G3; Pass Omega next after G3; no V1-surface edit authority before Omega CRUD and G-Omega." | PASS3-G3, OMEGA-CRUD | `restart/HANDOFF.md` top override/status block | HANDOFF currently says T-P3 is authorized (`restart/HANDOFF.md:5`) and forbids direct V1-surface edits (`restart/HANDOFF.md:44`). PASS-3 routes converged T-P3 output to G3 and then HANDOFF/Omega update flow (`restart/prompts/totality/PASS-3-SYNTHESIS.md:179`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:189`), while Pass Omega defines CRUD as the surface-editing mechanism (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`, `restart/prompts/pass-contracts/PASS-OMEGA.md:76`). |
| 3F-HANDOFF-002: Extend the top-level entry packet with T-P3 artifacts, T-P1/T-P2 hardening packets, SK-V13 Alpha/S-P1/S-P2 convergence, and Pass Omega prompt references; demote the older SK-V6/N-direct body to historical lineage. | COH-001, S-P1/P2-CLOSED | `restart/HANDOFF.md` entry-packet and historical-state sections | HANDOFF's packet list already enumerates P1/P2/V1/skinny inputs (`restart/HANDOFF.md:18`, `restart/HANDOFF.md:42`) but the lower historical body still carries N-direct/NoGo state (`restart/HANDOFF.md:110`). T-P1 coherence flags that stale conflict (`restart/audit/totality/p1/1F-coherence-scan.md:33`), and SK-V13 handoff records S-P1 and S-P2 as closed with S-P3 next (`restart/skinny/tranches/sk-v13/HANDOFF.md:132`, `restart/skinny/tranches/sk-v13/HANDOFF.md:135`). |
| 3F-HANDOFF-003: Update current skinny state to "SK-V12 CSS L4 admitted A/Go, but SK-V13 remains blocked on full CSS, JSON, decision-engine, union, orphan, and G-Omega obligations." | SKV12-CLOSE, SKV13-G1-G7 | `restart/HANDOFF.md` current skinny state and next-move summary | RESULTS reports overall A/Go after the CSS L4 row (`skinny/RESULTS.md:94`, `skinny/RESULTS.md:145`), and REDRESS-127 records PASS-ADMIT with row telemetry (`skinny/REDRESS.md:3824`, `skinny/REDRESS.md:3836`). SK-V13 still requires 23 remaining CSS features, all JSON rows, decision-engine replacement, union material differential, zero orphans, and G-Omega before W0 (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:59`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:73`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:112`). |
| 3F-HANDOFF-004: Add explicit pre-Omega and post-G-Omega concurrency/refusal conditions: planning and audit may continue; implementation, generated runtime, gate, RESULTS, and REDRESS edits remain blocked until G-Omega and S-P3 conditions are met. | SKV13-GOMEGA-BLOCK, OMEGA-GATE | `restart/HANDOFF.md` concurrency/refusal block | SK-V13 HANDOFF allows S-P1/S-P2/S-P3 planning and Omega read-only work before G-Omega (`restart/skinny/tranches/sk-v13/HANDOFF.md:76`) and blocks W0/source/generated/gate/RESULTS/REDRESS edits (`restart/skinny/tranches/sk-v13/HANDOFF.md:85`). Pass Omega requires user G-Omega before lock-amendment merge and next-cycle dispatch (`restart/prompts/pass-contracts/PASS-OMEGA.md:96`, `restart/prompts/pass-contracts/PASS-OMEGA.md:110`). |
| 3F-HANDOFF-005: Normalize grammar-onboarding language to three surfaces: grammar file, workspace metadata, and optional declaration-crate host functions; generated per-grammar names are allowed only behind rostered/generator criteria. | COH-002, T2C-LOCK14 | `restart/HANDOFF.md` project-state and onboarding summary | T-P1 coherence flags stale two-surface wording against Lock 14's declaration-crate surface (`restart/audit/totality/p1/1F-coherence-scan.md:34`). T-P2 states onboarding is grammar source, workspace metadata, and optional declaration-crate host functions (`restart/audit/totality/p2/2C-grammar-neutrality.md:50`, `restart/audit/totality/p2/2C-grammar-neutrality.md:157`), while T-P1 hardening restricts generated per-grammar names to generated/rostered criteria (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:47`). |

## Proposed MIGRATION.md Delta Text

Use these paragraphs as Pass Omega CRUD input, not as direct edits.

1. Replace the legacy-crate disposition note with: "Archive/remove proof is required for `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, and `bbnf-path-ts` before they may be counted closed; rename or V2 deferral alone does not close Lock 11/12/14/16 drift. Reconstituted path or SIMD surfaces must enter through generated/provider manifests and source-present primitive gates." This follows the current migration rows and T-P1 archive-proof requirement (`restart/MIGRATION.md:70`, `restart/MIGRATION.md:77`, `restart/audit/totality/p1/1E-locks-evidence.md:105`).

2. Add a generated-provider migration row: "Hardcoded grammar profile, runtime-provider, root-export, recognizer, and materialization branches are ABROGATE-REPLACE unless they consume generated registry/config/facts or are moved to generated/fixture-only surfaces." This routes T-P1 runtime/codegen drift through T-P2 grammar-neutrality closure (`restart/audit/totality/p1/1B-codegen-evidence.md:81`, `restart/audit/totality/p1/1C-runtime-evidence.md:98`, `restart/audit/totality/p2/2C-grammar-neutrality.md:132`).

3. Add a CSS/non-JSON telemetry plane: "Admitted grammar-specific fact outputs may be kept only as fenced generated telemetry or comparator outputs until routed into a V1 runtime substrate category or explicitly excluded; CSS L4 is currently admitted as a direct-to-struct fact row, not EventTape." This resolves the T-P1 category gap and SK-V12 admission evidence (`restart/audit/totality/p1/1A-substrate-evidence.md:80`, `restart/audit/totality/p1/1C-runtime-evidence.md:102`, `skinny/RESULTS.md:94`).

4. Add a decision-engine tranche: "P1-P8 cascade, marker-string lowerers, thin `CostFacts`, and opaque regex programs are ABROGATE-REPLACE unless Pass Omega or a named S-P3 receiver wave installs candidate generation, eqsat, CSP, active cost extraction, and strict equivalence/cost evidence." This follows T-P2's cost-model and parse-that findings (`restart/audit/totality/p2/2D-cost-model.md:69`, `restart/audit/totality/p2/2D-cost-model.md:110`, `restart/audit/totality/p2/2F-parse-that-gaps.md:198`).

5. Add a Lock 16 primitive manifest row: "Every SIMD, ASM, hardware, table, mask, carry, or source-present primitive must have manifest identity, source state, strict mode, first consumer, command, scalar fallback or architecture block, and row movement before admission." This adopts the T-P2 primitive and host-architecture ledgers (`restart/audit/totality/p2/2B-primitive-vocabulary.md:196`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:184`).

## Proposed HANDOFF.md Top-Level State Delta

Use this as the replacement top-level state once G3 accepts the T-P3 packet.

```
## Current Override

T-P3 Synthesis has produced proposed V1 deltas and is queued for G3. Until G3
and Pass Omega complete, `ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`,
`MIGRATION.md`, `HANDOFF.md`, governance surfaces, source, generated runtime,
gate output, `skinny/RESULTS.md`, and `skinny/REDRESS.md` remain unchanged
except by their authorized pass owners.

After G3 closes, dispatch Pass Omega. Pass Omega must consume T-P1, T-P2,
T-P3, SK-V12 CSS admission, REDRESS-119/120/121-127, SK-V13 Alpha/S-P1/S-P2,
and current V1 surfaces. Pass Omega CRUD may prepare concrete edits, and
G-Omega remains the user gate before V1 amendment merge and any SK-V13 Wave 0
implementation, generated runtime, gate, RESULTS, or REDRESS work.
```

The replacement state is consistent with PASS-3's G3 flow (`restart/prompts/totality/PASS-3-SYNTHESIS.md:179`), Pass Omega CRUD/user-gate flow (`restart/prompts/pass-contracts/PASS-OMEGA.md:76`, `restart/prompts/pass-contracts/PASS-OMEGA.md:96`), and SK-V13's G-Omega block (`restart/skinny/tranches/sk-v13/HANDOFF.md:54`, `restart/skinny/tranches/sk-v13/HANDOFF.md:85`).

## Next-Cycle Dispatch Directive

Dispatch target: Pass Omega, then Omega CRUD, then G-Omega sign-off, then the next authorized totality/skinny cycle.

Entry conditions for Pass Omega after G3:

1. T-P3 must have all 3A-3F substantive synthesis artifacts present, plus hardening/challenge convergence or user-pinned G3 closure. PASS-3 requires T-P3 convergence to produce an updated V1 amendment packet and present queued locks/master/handoff/migration deltas at G3 (`restart/prompts/totality/PASS-3-SYNTHESIS.md:151`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:179`).
2. The G3 packet must include this 3F delta set, especially the measurable Omega/CRUD entry conditions, because CH6 specifically requires the 3F directive to be concrete and measurable (`restart/prompts/totality/PASS-3-SYNTHESIS.md:127`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:187`).
3. Pass Omega must consume the latest totality packets, skinny `RESULTS.md`, skinny `REDRESS.md`, current V1 surfaces, and SK-V13 evidence before any CRUD edit is prepared (`restart/prompts/pass-contracts/PASS-OMEGA.md:1`, `restart/prompts/pass-contracts/PASS-OMEGA.md:15`).

Entry conditions for Omega CRUD:

1. Omega-A through Omega-F must converge through challenge, with no critical orphan revise points. Pass Omega defines CRUD as post-convergence and constrained by the consolidated proposed amendment set (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`, `restart/prompts/pass-contracts/PASS-OMEGA.md:76`).
2. CRUD-4 owns concrete `HANDOFF.md` and `MIGRATION.md` updates and the next-cycle directive, so 3F deltas may be applied only through CRUD-4 or an explicitly authorized Omega CRUD equivalent (`restart/prompts/pass-contracts/PASS-OMEGA.md:70`).
3. If the apparent ordering conflict remains between "CRUD before G-Omega presentation" and "G-Omega before Totality CRUD," resolve it by treating CRUD output before G-Omega as proposed diff/log artifacts and by withholding governance merge until user G-Omega. Pass Omega describes CRUD completion before presenting G-Omega (`restart/prompts/pass-contracts/PASS-OMEGA.md:92`), while SK-V13 HANDOFF lists Totality CRUD after G-Omega in its pass sequence (`restart/skinny/tranches/sk-v13/HANDOFF.md:115`).

Entry conditions after G-Omega:

1. User G-Omega must close before lock amendments or V1-surface amendments are merged into authoritative state (`restart/prompts/pass-contracts/PASS-OMEGA.md:96`, `restart/prompts/pass-contracts/PASS-OMEGA.md:108`).
2. When G-Omega closes, V1 becomes the next spec version and the next totality pass cycle dispatches according to Omega-F's directive (`restart/prompts/pass-contracts/PASS-OMEGA.md:110`).
3. SK-V13 Wave 0 may dispatch only if G-Omega has closed and S-P3 has converged to an executable SPEC/DISPATCH; S-P2 authorizes S-P3 but keeps W0 subject to the G-Omega block (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:15`, `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:60`, `restart/skinny/tranches/sk-v13/HANDOFF.md:93`).

Measurable dispatch checklist:

| gate | measurable condition | source |
| --- | --- | --- |
| G3 | 3A-3F artifacts exist, T-P3 challenge converged or user pinned, and queued deltas are presented. | `restart/prompts/totality/PASS-3-SYNTHESIS.md:160`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:179` |
| Omega entry | Latest T-P1/T-P2/T-P3, V1 surfaces, `skinny/RESULTS.md`, and `skinny/REDRESS.md` are cited in Omega source maps. | `restart/prompts/pass-contracts/PASS-OMEGA.md:1`, `restart/prompts/pass-contracts/PASS-OMEGA.md:112` |
| CRUD entry | Omega challenge has converged and consolidated CRUD instructions exist for each owned surface. | `restart/prompts/pass-contracts/PASS-OMEGA.md:57`, `restart/prompts/pass-contracts/PASS-OMEGA.md:70` |
| G-Omega | User receives summary, challenge verdict, locks diff, master-plan delta, CRUD summary, and next-cycle directive. | `restart/prompts/pass-contracts/PASS-OMEGA.md:96`, `restart/prompts/pass-contracts/PASS-OMEGA.md:102` |
| SK-V13 W0 | G-Omega closed, S-P3 converged, and W0 SPEC/DISPATCH exists; otherwise source/gate/RESULTS/REDRESS edits remain blocked. | `restart/skinny/tranches/sk-v13/HANDOFF.md:85`, `restart/skinny/tranches/sk-v13/HANDOFF.md:128`, `restart/skinny/tranches/sk-v13/HANDOFF.md:140` |

## Consequences

| category | consequence | propagation |
| --- | --- | --- |
| positive | `MIGRATION.md` becomes a real gate ledger instead of a rename ledger for path/SIMD/archive crates, matching T-P1 archive-proof and T-P2 source-present rules. | Propagates to crate-disposition rows, Lock 11/12 closure checks, and Lock 16 primitive manifests (`restart/audit/totality/p1/1E-locks-evidence.md:89`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:349`). |
| positive | `HANDOFF.md` stops mixing current T-P3/Omega state with stale SK-V6/N-direct text. | Propagates to orchestrator reading order, G3 packet preparation, and SK-V13 W0 refusal conditions (`restart/audit/totality/p1/1F-coherence-scan.md:33`, `restart/skinny/tranches/sk-v13/HANDOFF.md:144`). |
| cost | Pass Omega CRUD-4 must edit both `HANDOFF.md` and `MIGRATION.md`, and Omega must reconcile whether CRUD produces proposed diffs before G-Omega or merges only after G-Omega. | The ordering tension is visible across Pass Omega and SK-V13 handoff sequencing (`restart/prompts/pass-contracts/PASS-OMEGA.md:92`, `restart/skinny/tranches/sk-v13/HANDOFF.md:115`). |
| cost | Legacy handwritten grammar/runtime/test/proof files need explicit fate assignment instead of staying as tolerated root drift. | Propagates to generated fixtures, runtime root exports, and proof witnesses (`restart/audit/totality/p1/1C-runtime-evidence.md:75`, `restart/audit/totality/p1/1F-anti-pattern.md:40`). |
| propagation | Decision-engine and primitive-manifest deltas route into future SK-V13 S-P3/W0 dispatch. | SK-V13 G2 requires decision-engine replacement, and G4 requires zero aarch64 orphans (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:59`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`). |

## V2 Cost And Routing Ledger

This ledger gives the migration and handoff deltas concrete receivers and gates. It also resolves the CRUD/G-Omega ordering ambiguity by making pre-G-Omega CRUD output proposed diffs/logs and post-G-Omega work the authoritative merge.

| delta | LOC budget | propagation surfaces | risk class | wave alignment | same-wave consumer / receiver | hard cap or abrogate gate |
| --- | ---: | ---: | --- | --- | --- | --- |
| 3F-MIG-001 | 60-140 docs | 4 | Medium-high | Omega CRUD-4 migration crate fates | Receiver: archive/remove proof rows for legacy crates. | Block rename-only closure; require archive/removal proof or reconstituted generated surface. |
| 3F-MIG-002 | 120-260 docs | 4 | High | Lock 14 registry wave / Omega CRUD-4 | Receiver: generated-provider/roster migration row. | Abrogate hardcoded provider/runtime-profile/root-alias branches. |
| 3F-MIG-003 | 120-260 docs | 4 | High | Runtime/codegen ownership cleanup | Receiver: generated/rostered/archive/fixture fate table. | Block hand-owned per-grammar runtime surface in generic crates. |
| 3F-MIG-004 | 80-180 docs/report | 4 | Medium | Non-JSON telemetry/BENCH feed | Receiver: CSS fact-output migration plane. | Block forcing CSS fact stream into EventTape or retained substrate. |
| 3F-MIG-005 | 120-260 docs plus named receiver wave | 5 | High | Decision-engine fold | Receiver: S-P3 decision-engine wave and Omega migration row. | Abrogate if no named receiver installs candidate generation, eqsat, CSP, active cost, and strict evidence. |
| 3F-MIG-006 | 120-260 docs | 5 | High | Lock 16 primitive/source-present wave | Receiver: primitive manifest/source-state migration gate. | Block source-present primitive retention without wired consumer, scalar delegate, delete, or architectural block. |
| 3F-HANDOFF-001 | 40-90 docs | 3 | High process | G3/Omega status | Receiver: top-level current override. | Block any claim that T-P3 directly edits V1 surfaces. |
| 3F-HANDOFF-002 | 60-140 docs | 4 | Medium | Entry-packet refresh | Receiver: HANDOFF source map and historical lineage demotion. | Block stale SK-V6/N-direct state as current authority. |
| 3F-HANDOFF-003 | 80-180 docs | 4 | High process | SK-V13 current-state gate | Receiver: current skinny state and next-move summary. | Block W0 dispatch from SK-V12 CSS admission alone. |
| 3F-HANDOFF-004 | 80-180 docs | 4 | High process | Pre-Omega/post-G-Omega refusal conditions | Receiver: concurrency/refusal block. | Block source/generated/gate/RESULTS/REDRESS edits until G-Omega and S-P3 conditions pass. |
| 3F-HANDOFF-005 | 60-140 docs | 3 | Medium-high | Grammar-onboarding language | Receiver: three-surface onboarding wording. | Block two-surface or generic-branch grammar onboarding language. |

## V2 Gated Open Questions

| lens | question | receiver | blocker | gate |
| --- | --- | --- | --- | --- |
| CH1 | Does the G3 packet include all 3A-3E source-linked deltas needed by Omega-F, or must 3F be revised after sibling artifacts land? | G3 packet owner / Pass Omega Omega-F. | 3F depends on sibling artifact finality. | G3 source map must cite all 3A-3F V2 artifacts or route a revise before Omega. |
| CH2 | What exact generated provider manifest filename and schema should replace runtime hardcoded providers? | 3E + Pass Omega CRUD-1/CRUD-3/CRUD-4. | T-P2 defines contract but not file/schema. | G-Omega pins manifest schema before Lock 14 registry implementation. |
| CH3 | How should future union attempts prove material differential without replaying REDRESS-96/97/98? | 3C Lock 1/16 and S-P3 union wave. | Prior union routes are historical failures but category is user-unblocked. | SPEC wave must name material differential, changed data movement, consumer, and row gate before redress. |
| CH4 | Which stale historical HANDOFF sections should be deleted versus retained under an explicit "archive lineage" heading? | Pass Omega CRUD-4. | T-P1 flags stale state but not deletion granularity. | CRUD-4 diff must separate current override from archive lineage and pass G-Omega review. |
| CH6 | Should Omega CRUD be described as preparing proposed diffs before G-Omega, with final merge after G-Omega, to reconcile Pass Omega and SK-V13 sequencing? | Pass Omega controller / G-Omega packet. | Pass Omega and SK-V13 HANDOFF sequence CRUD differently. | Pre-G-Omega CRUD emits proposed diffs/logs only; authoritative merge waits for user G-Omega. |
