---
agent: 3F
pass: T-P3-synthesis
cycle: V4-SKV18-totality
cycle_self_label: SK-V18
pass_omega_index: V6
cycle_label_note: "The synthesis-cohort hardening cycle is V4-SKV18-totality (shared with 3A/3B/3C/3D/3E); the distinct `pass_omega_index: V6` is the NEXT astral CRUD pass (Pass Omega V5 already CLOSED for SK-V17 at 33b51d8f4), carried in the body for the MIGRATION/HANDOFF routing. CH1-V1-C6 split reconciled: cohort cycle label unified to V4-SKV18-totality."
generated_at: 2026-06-01T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-anti-pattern, 1F-coherence-scan, 1F-past-corpora]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: "MIGRATION.md + HANDOFF.md + next-cycle dispatch directive"
proposed_deltas_count: 12
delta_summary:
  carried_from_prior_cycle: [3F-MH-001, 3F-MH-003, 3F-MH-004, 3F-MH-005, 3F-MH-006, 3F-MH-007]
  removed: [3F-MH-002]
  answered: [CH1-V1-002, CH4-COST-05, CH6-V1-02, CH4-V2-001]
  newly_added: [3F-MH-008, 3F-MH-009, 3F-MH-010, 3F-MH-011, 3F-MH-012, 3F-MH-013]
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "3F-MH-001 re-rooted: current receiver is the SK-V18 GENERALIZATION cycle (skinny tree), and the NEXT astral pass is Pass Omega V6 (V5 already CLOSED for SK-V17 at 33b51d8f4), not Pass Omega V5."
    - "3F-MH-002 (SK-V15 W0-W11 receiver table) REMOVED — superseded by the SK-V18 12-wave W-PRUNE→G1..G6→PROVE→H1 manifest carried in 3F-MH-009."
    - "3F-MH-003 delete-before-provider gate re-grounded on the SK-V18 PRUNE-before-GENERALIZE order + the four pruned-surface-vs-oracle dependency rows."
    - "3F-MH-005/006/007 re-rooted from SK-V15 DISPATCH-PROMPT/W0 onto the SK-V18 SPEC + the W-PRUNE-first dispatch lock."
    - "V1-FOLD (CH1-V1-C5): 3F-MH-004 + the §6 T-P3-lock body re-grounded off the SK-V15 HARDENING-T-P3-V5-CONSOLIDATED.md (target 77b6e9fd7, a 42-candidate/23-19 SK-V15 matrix whose facts contradict the SK-V18 21-candidate/9-11 split) onto the SK-V18 T-P1/T-P2 consolidated files + the in-cycle SK-V18 T-P3 V1 hardening."
    - "V3-FOLD (CH1-V3-C5): the THREE surviving SK-V15-style governance over-claims re-grounded onto the (already-correct) 3F-MH-004 record — Executive Summary (`totality T-P1 clean-final / T-P2 normal §3Z / T-P3 converged`), §6 MIGRATION-carrier prose (`T-P2 normal §3Z; T-P3 final-convergence lock under V≤5`), and the §7 HANDOFF override carrier (`T-P1/T-P2/T-P3 CONVERGED`) all re-stated as T-P1/T-P2 near-converged NON-normal-§3Z (consec=0, converged=false) + T-P3 in-cycle hardening; contradicted both MH-004 and the on-disk SK-V18 T-P1/T-P2 CONSOLIDATED files."
    - "V3-FOLD (CH1-V3-C5-row): 3F-MH-005 delta text (the block Pass Omega CRUD carries verbatim into restart/HANDOFF.md) changed from `totality T-P1/T-P2/T-P3 CONVERGED` to the MH-004 record, so the proposed HANDOFF replacement no longer carries the over-claim into a V1 surface."
    - "V1-FOLD (CH4-V1, 3F-MH-009): P3 figure cited verbatim from sk-v18/SPEC.md:435 (≈−5500 = −5460 6×910 replica bodies + ~−40 collapsed rows), aligned across the decisions table, P3 collapse row, and 3D's −5460."
    - "V1-FOLD (CH3-V1-R2, mirror): 3F-MH-003 + the CH3 open question state G2/G4/G6 entry is BLOCKED until the SK-V16/V17 REDRESS reconcile is on the committed ledger as a Pass-Omega-V6 / pre-W-PRUNE blocker (NOT deferred to SK-V19 entry); mirrors 3D-D08 / 3B CH3."
    - "V1-FOLD (CH1-V1-C6): cycle label reconciled to the cohort V4-SKV18-totality; the distinct Pass Omega index V6 is carried as pass_omega_index in the frontmatter."
  monotonic_fold_note: "Skinny SK-V18 S-P0..S-P3 findings fold INTO totality proposal inputs; totality never dictates back to live skinny. The skinny->totality fold is MONOTONIC."
---

# 3F — MIGRATION + HANDOFF + Next-Cycle Dispatch (SK-V18 Generalization Cycle)

## Executive Summary

This artifact PROPOSES only; it does not amend `restart/MIGRATION.md` or
`restart/HANDOFF.md` (Pass Omega CRUD applies post-G-Omega). The prior totality
cycle (V3) targeted SK-V15 Pass Omega V5; that pass CLOSED for SK-V17 at
`33b51d8f4`, so the NEXT astral pass is **Pass Omega V6**. The live top-level
surfaces still define SK-V18 as a totality-`crates/core/`-ADOPT cycle
(`restart/HANDOFF.md:17-19`) — but the CERTIFIED SK-V18 is the GENERALIZATION
cycle on the SKINNY tree: un-fork JSON+CSS into ONE `.bbnf`-driven generator
emitting JSON+CSS+Sheets, aarch64-only (`sk-v18/SPEC.md:19-21`,`:58-61`). That
is the single most material drift (COH18-001). The migration delta crystallises
the five concrete rename/abrogate/refactor decisions T-P1/T-P2 surfaced: the x86
crate-wide DELETE (P1, ≈−4500 LOC), the `CSS_GENERATED_RS` courier RETIRE (G2),
the 7-replica + 7-`RuntimeTarget`-row COLLAPSE (P3, ≈−5500 LOC), the phantom
`<G>` DELETE (G4), and the totality `css_types.rs` RELOCATE-or-delete (SK-V19).
The HANDOFF delta re-authors current state to (per the 3F-MH-004 record): skinny
S-P0..S-P3 certified + totality T-P1 near-converged NON-normal-§3Z (V7 lone clean
r=1.000, V8 broke the streak; consec=0, converged=false) / T-P2 near-converged
NON-normal-§3Z (converged=false, consec=0) / T-P3 in-cycle hardening (the current
V-cycle; NOT yet a final-convergence lock),
ready for Pass Omega V6 + wave implementation. The next-cycle directive makes
W-PRUNE P1-P5 the ONLY dispatch-eligible cluster FIRST per the SPEC dispatch
lock, and tees up the SK-V19 totality-fold (the skinny un-fork adopted into
`crates/core/`). The skinny->totality fold is MONOTONIC.

## V6 Delta Summary

| bucket | delta ids | note |
|---|---|---|
| Carried from prior cycle | 3F-MH-001, 3F-MH-003, 3F-MH-004, 3F-MH-005, 3F-MH-006, 3F-MH-007 | Six structural/routing deltas survive; all re-rooted from the SK-V15 packet onto the SK-V18 GENERALIZATION packet and the Pass Omega V6 receiver. |
| Removed | 3F-MH-002 | The SK-V15 W0-W11 receiver table is superseded by the SK-V18 12-wave W-PRUNE→G1..G6→PROVE→H1 manifest (now 3F-MH-009). |
| Answered | CH1-V1-002, CH4-COST-05, CH6-V1-02, CH4-V2-001 | Carried from the prior cycle's answered set; the absent-companion-prompt route is answered by the extant `sk-v18/SPEC.md` + `sk-v18/HANDOFF.md`; per-delta CH4 coverage retained. |
| Newly added (SK-V18) | 3F-MH-008..3F-MH-013 | Six SK-V18-specific deltas: x86 crate-wide DELETE, CSS courier RETIRE, 7-replica+RuntimeTarget COLLAPSE, phantom `<G>` DELETE, `css_types.rs` RELOCATE (SK-V19), and the SK-V18 12-wave migration receiver. |

## Migration Decisions Synthesised (the five rename/abrogate/refactor surfaces)

The SK-V18 evidence base surfaces five concrete migration decisions. Each is a
proposed `restart/MIGRATION.md` delta carrier (proposal-only; CRUD-applied at
Pass Omega V6).

| Decision | Kind | Receiver wave | Net LOC | Grounding |
|---|---|---|---|---|
| x86 surface crate-wide | DELETE | PRUNE-1 (P1) | ≈ −4500 | 1F `COH18-009`; 1A `1A-DIV` x86 row; 1D `D-4`/`G-2`; `sk-v18/SPEC.md:130-134`,`:573-600` |
| `CSS_GENERATED_RS` const courier (+ JSON `_RS` literals) | RETIRE | G2 (CSS) / G1 (JSON) | ≈ −910 CSS + JSON literals | 1F-anti-pattern courier row; 1D `C-1`/`G-6`; `runtime_generator.rs:701`; `sk-v18/SPEC.md:61-69` |
| 7 byte-identical css_l4 replicas + 7 `RuntimeTarget` rows | COLLAPSE | PRUNE-3 (P3) | ≈ −5500 (SPEC `:435`: 6×910 = −5460 replica bodies deleted + ~−40 collapsed rows + 1 `PartialEq` derive; 6 of 7 replicas deleted) | 1F-anti-pattern replica row (md5 `b654562c`); 1D `D-2`/`G-13`; `sk-v18/SPEC.md:80-85`,`:435`,`:635-663` |
| phantom `<G: EventGrammar>` axis | DELETE | G4 | — (decoration removal) | 1A `1A-SUB-023`; 1F `COH18-008`; 1D `D-5`/`G-8`; `sk-v18/SPEC.md:99-102` |
| totality `crates/core/src/css_types.rs` | RELOCATE-or-DELETE | SK-V19 (NOT SK-V18) | 66 LOC | 1F-anti-pattern `css_types.rs` row; 1F `COH18-006`/`U-COH18-002`; `LOCKS.md:349` names it verbatim |

## Proposed Delta Table

| delta id | proposed delta | source T-P1/T-P2 finding-id cited | affected V1 surface section | receiver / blocker / gate | rationale |
|---|---|---|---|---|---|
| 3F-MH-001 | Insert a current **SK-V18 Pass Omega V6 Migration Receiver** as a new `## 0.0`-class section in `restart/MIGRATION.md`, ABOVE the current SK-V17 tape-fold receiver. Keep the SK-V17 receiver (`restart/MIGRATION.md:30`-class) and the historical Pass Omega V2..V8 receivers as provenance-only, NOT current dispatch authority. | 1F `COH18-001` (top-level surfaces define a DIFFERENT, stale SK-V18; `restart/audit/totality/p1/1F-coherence-scan.md:75`). T-P3 feeds Pass Omega and does not edit V1 surfaces (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:31-33`). | `restart/MIGRATION.md` ABOVE `## 0.0 Current SK-V17 Tape-Fold Migration Receiver` (`restart/MIGRATION.md:30`-class); historical sections stay below. | Receiver: Pass Omega V6 CRUD. Blocker: T-P3 + Pass Omega convergence. Gate: G-Omega + CRUD log. | The CURRENT pass is Pass Omega V6 (V5 already CLOSED for SK-V17 at `33b51d8f4`); avoids a V5/V6 name collision and gives downstream agents a current SK-V18 entry point. |
| 3F-MH-003 | Add a MIGRATION gate clause: under SK-V18 the order is **PRUNE-before-GENERALIZE-before-PROVE**; no GENERALIZE/PROVE wave deletes a hand-written ORACLE (JSON `json_templates/`, the 7 css_l4 replica bodies, the CSS courier) before its grammar-DERIVED replacement lands byte-equivalent and the round-trip diff-control gate is GREEN. CH3-V1-R2: additionally, **G2/G4/G6 entry is BLOCKED** until the SK-V16/V17 REDRESS reconcile (the four-item pre-block is complete only for the SK-V15-W11 ledger; 1D U-5) is on the committed ledger as a Pass-Omega-V6 / pre-W-PRUNE blocker — these waves abut REDRESS items 51/53/247 and run during SK-V18 (mirrored 3D-D08 / 3B CH3). | 1D `Rejected-Route Pre-Block` (the three highest-regression moves abut a REDRESS reject; `restart/audit/totality/p1/1D-skinny-lessons.md:156-173`,`:244-248`). 1F `COH18-011` (the JSON `value_from_ref` byte-equal re-emit pre-gate is plan-bound). `sk-v18/SPEC.md` G1 diff-control gate (`:48`,`:440`). | `restart/MIGRATION.md` deletion/retirement gate sections (the §17/§19-class delete gates) + new §0.0 receiver. | Receiver: every SK-V18 GENERALIZE-wave deletion; the SK-V16/V17 reconcile receiver is Pass-Omega-V6 / pre-W-PRUNE. Blocker: absent byte-equivalent oracle proof, or an unreconciled SK-V16/V17 pre-block before G2/G4/G6. Gate: G1 JSON diff-control + G2 `CSS_GENERATED_RS`-deleted + P3 md5-distinct post-collapse witness + committed SK-V16/V17 pre-block reconcile. | Prevents the delete-before-replacement failure pattern (the lightningcss tree-walk regression 1D `C-3` names) from re-entering under the SK-V18 un-fork, and bars an SK-V16/V17-rejected shape from re-entering G2/G4/G6 with no committed fence. |
| 3F-MH-004 | Add a MIGRATION/HANDOFF governance paragraph carrying the **SK-V18** totality-pass provenance: **T-P1 SK-V18 as near-converged NON-normal-§3Z** (V7 lone clean r=1.000, V8 broke the streak; consec=0, converged=false — NOT a normal two-clean lock), **T-P2 SK-V18 as near-converged NON-normal-§3Z** (V1-V5; converged=false, consec=0; only single-cell citation-precision qualifiers from V4, no surviving REJECT), and **T-P3 SK-V18 as in-cycle hardening** (the current V-cycle; NOT yet a final-convergence lock). | SK-V18 T-P1 (`restart/audit/totality/p1/hardening/HARDENING-T-P1-CONSOLIDATED.md:44-52`); SK-V18 T-P2 (`restart/audit/totality/p2/hardening/HARDENING-T-P2-CONSOLIDATED.md:17-25`); SK-V18 T-P3 (the current cycle hardening verdicts under `restart/audit/totality/p3/hardening/V1/`). CH1-V1-C5: do NOT cite the SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md` (target `77b6e9fd7`, a 42-candidate / 23-ACCEPT-19-MODIFY SK-V15 matrix) for the SK-V18 state — its facts contradict the SK-V18 21-candidate / 9-ACCEPT-11-MODIFY split. | `restart/MIGRATION.md` §0.0 receiver + `restart/HANDOFF.md` current totality override. | Receiver: Pass Omega V6 CRUD. Blocker: any text rewriting T-P1/T-P2 SK-V18 as normal §3Z or citing the SK-V15 T-P3 V5 file as the SK-V18 record. Gate: G-Omega. | Keeps the governance history honest across the three SK-V18 totality passes (mirroring 3C's honest use of the SK-V18 T-P1/T-P2 consolidated files) and survives Pass Omega. |
| 3F-MH-005 | Replace the top-level HANDOFF current-state block: SK-V18 is the GENERALIZATION cycle on the SKINNY tree (un-fork JSON+CSS into ONE `.bbnf`-driven generator emitting JSON+CSS+Sheets, aarch64-only), with skinny S-P0..S-P3 CERTIFIED and totality (per the 3F-MH-004 record) T-P1 near-converged NON-normal-§3Z (V7 lone clean r=1.000, V8 broke the streak; consec=0, converged=false), T-P2 near-converged NON-normal-§3Z (converged=false, consec=0), T-P3 in-cycle hardening (NOT yet a final-convergence lock); route current SK-V18 wave authority through the extant `restart/skinny/tranches/sk-v18/SPEC.md` + `restart/skinny/tranches/sk-v18/HANDOFF.md`. STRIKE the stale `restart/HANDOFF.md:17-19` "SK-V18 adopts … into the totality `crates/core/` tree" definition; that adoption is SK-V19. | 1F `COH18-001` (HANDOFF defines a different SK-V18; `1F-coherence-scan.md:75`,`:94`). `sk-v18/HANDOFF.md:1` (the generalization handoff) + `sk-v18/SPEC.md:19-21`. | `restart/HANDOFF.md:17-19` (the stale SK-V18 paragraph) + the current override block (`restart/HANDOFF.md:3-28`). | Receiver: Pass Omega V6 CRUD. Blocker: stale totality-adopt SK-V18 definition or a citation to a missing companion prompt. Gate: G-Omega + W-PRUNE pre-dispatch verification consumes `sk-v18/SPEC.md`. | Cold-start agents must land on the certified SK-V18 generalization scope and the extant SK-V18 SPEC/HANDOFF, NOT the stale totality-adopt definition (which is the SK-V19 obligation). |
| 3F-MH-006 | Add a top-level HANDOFF **SK-V18 blocker matrix** matching each certified close-condition gap to its receiver wave: generator-does-not-exist → G1/G2/G3; 7-replica + RuntimeTarget rows → P3; phantom `<G>` → G4; CSS Value API absent → G4; CSS NEON dead at admission → G5/G6; x86 live → P1; Lock-14 green-by-exclusion → P4; metalang `parse_w11_1_number` leak → P5; Sheets is a 24-LOC stub → PROVE; CSS ratio directional-not-re-locked → H1. | 1D `D-1..D-8` divergence rows + `G-6/G-7/G-8/G-13` (`restart/audit/totality/p1/1D-skinny-lessons.md:82-114`,`:202-209`). 1F `COH18-002..010` (`1F-coherence-scan.md:76-85`). `sk-v18/SPEC.md:54-169` close conditions. | `restart/HANDOFF.md` after current state, before next move. | Receiver: SK-V18 W-PRUNE then G1..G6/PROVE/H1 after Pass Omega V6. Blocker: any unresolved row. Gate: each SPEC exit-gate (`sk-v18/SPEC.md:471-484` rerun ceilings + per-§ exit falsifiers). | Makes the top-level handoff executable next-work rows, not prose-only "ready" claims. |
| 3F-MH-007 | Replace the "Pass Omega V8 next-cycle dispatch directive" with a **Pass Omega V6/G-Omega → SK-V18 W-PRUNE** directive: T-P3 locks, G3 auto-passes under the active pin, Pass Omega V6 runs, CRUD updates HANDOFF/MIGRATION current-state truth or records a blocked/extension decision with exact remainder/receiver/blocker/gate, G-Omega authorizes the required V1 patches, then W-PRUNE (P1-P5) dispatches through the SKINNY triumvirate — the ONLY dispatch-eligible cluster on close. | T-P3 3F dispatch row (implementation waves do not begin until Pass Omega CRUD closes and G-Omega authorises patches). Pass Omega CRUD owns HANDOFF+MIGRATION (`restart/prompts/pass-contracts/PASS-OMEGA.md` CRUD-4-class). `sk-v18/SPEC.md:46-49` dispatch lock (W-PRUNE only dispatch-eligible). | `restart/HANDOFF.md` next-cycle directive (the historical Pass Omega V8 directive block). | Receiver: Pass Omega V6 CRUD, then SK-V18 W-PRUNE only after G-Omega. Blocker: no CRUD, no G-Omega, unresolved invariant, or incomplete current-state cleanup without a blocked/extension record. Gate: G-Omega; then the W-PRUNE entry (P1-P5 are entry-gate-free per `sk-v18/SPEC.md:433-437`). | Gives the next worker a concrete, measurable dispatch path that honours the SPEC's W-PRUNE-first dispatch lock; prevents direct implementation dispatch from T-P3 prose. |
| 3F-MH-008 | MIGRATION delta: record the **x86 surface as a crate-wide DELETE** (NOT just `src/x86_64/`). The deletion list is REACH-MATCHED to the verify grep: `bbnf-simd/src/x86_64/` (24 files), `bbnf-simd/ext/x86/` (vendored ASM), `bbnf-simd/build.rs` (nasm driver), the `nasm-rs` build-dep, `src/lib.rs:5 pub mod x86_64;` + the `#[cfg(target_arch="x86_64")]` arms, and the 9 compile-coupled `checkasm_parity.rs` x86_64 call sites (DECOUPLE in the SAME commit). aarch64-only is the SOLE admission platform; x86 is diagnostic-only, the prune target. | 1F `COH18-009` (x86 live in skinny, x86-free in totality, ARCH CollapsedStage x86-pinned vs aarch64-only plane; `1F-coherence-scan.md:83`,`:102`). 1D `D-4`/`G-2` (`1D-skinny-lessons.md:99-102`,`:198`). 2A REFUTATION: x86/AVX-512 closing an M5 Max row is REFUTED. `sk-v18/SPEC.md:130-134`,`:573-600`. | `restart/MIGRATION.md` x86/SIMD disposition rows + the ARCH `CollapsedStage`-x86 canon carrier (a companion SK-V19 ARCH reconcile, flagged in 3A). | Receiver: PRUNE-1 (P1), dispatchable now per SPEC. Blocker: a build break if the 9 checkasm call sites are not decoupled in-commit. Gate: `find …/x86_64 …/ext/x86 -type f == 0`; crate-wide `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` aarch64-neutral; `cargo build`/`cargo test --no-run` clean; `x86_tree_deleted == true`. | The single most consequential PRUNE item (≈−4500 LOC); a deletion list narrower than its verify grep ships a RED-by-construction gate (the V3 escape the SK-V18 fold fixed). |
| 3F-MH-009 | MIGRATION delta: add the **SK-V18 12-wave migration receiver table** (W-PRUNE P1-P5 + G1 + G2 + G3 + G4 + G5/G6 + PROVE + H1), mapping each wave to its migration consequence, net LOC, and exit gate. This REPLACES the removed SK-V15 W0-W11 row table (3F-MH-002). Campaign net ≈ **−10800 LOC** — a REDUCTION; the generalization DELETES far more than it adds. | 1D `G-13` (PRUNE list nets ≈−10800; `1D-skinny-lessons.md:209`). 1F-anti-pattern PRUNE-receiver table (`restart/audit/totality/p1/1F-anti-pattern.md:71-82`). `sk-v18/SPEC.md:429-449` wave manifest (12 waves, ≤12 ceiling exactly at cap). | `restart/MIGRATION.md` §0.0 receiver + cross-reference to the SK-V18 SPEC manifest. | Receiver: Pass Omega V6 CRUD for docs; W-PRUNE → G1..G6/PROVE/H1 after G-Omega for implementation. Blocker: missing wave→consequence proof row. Gate: `sk-v18/SPEC.md:471-484` per-wave rerun ceilings. | Makes migration route from the actual certified 12-wave manifest, not stale SK-V14/SK-V15/SK-V17 lineage; the receiver is a REDUCTION ledger, not an addition plan. |
| 3F-MH-010 | MIGRATION delta: record the **`CSS_GENERATED_RS` const courier RETIRE** (`runtime_generator.rs:701`) and the JSON `_RS` fixed-literals + `json_sink_direct`/`json_typed_direct` + `json_templates/` RETIRE, replaced by grammar-DERIVED emission. `verbatim_blob_present == false` campaign-wide; `emit_shape_source == lowered_program` (NOT `runtime_target`) — the relocated-seam falsifier. The hand-written content becomes byte-for-byte parity ORACLES, deleted post-equivalence, NOT the product. | 1F-anti-pattern courier + `_RS`-literal + grammar-named-module rows (`1F-anti-pattern.md:60-64`). 1D `C-1`/`G-6` (`1D-skinny-lessons.md:188`,`:202`). 2C REFUTATION: `find_css_significant` wire-as-is is REFUTED; the courier is not a generated artefact. `sk-v18/SPEC.md:61-69`,`:247-248`. | `restart/MIGRATION.md` codegen-courier disposition rows; companion §12 ARCH leak-scan hardening (3A). | Receiver: G2 (CSS courier) ∧ G1 (JSON literals/templates). Blocker: a relocated seam (per-grammar branch moved into a neutral data table). Gate: `CSS_GENERATED_RS` DELETED; `verbatim_blob_present == false`; `emit_shape_source == lowered_program`; G1 JSON byte-equivalence vs `json_templates/` oracle BEFORE oracle deletion. | The hand-written courier IS the SK-V18 G1/G2/G3 core (1F `COH18-003`); retiring it under the diff-control gate is what makes the generator REAL rather than a relabeled blob. |
| 3F-MH-011 | MIGRATION delta: record the **7-replica + 7-`RuntimeTarget`-row COLLAPSE** to ONE CSS config (the relocated-seam structural co-gate). The 7 byte-identical `css_l4_*/generated.rs` (md5 `b654562c`) collapse to ONE; `xtask/regen.rs` derives `PartialEq` for the R16 full-row collapse over BOTH nested structs (`frontend_requirements` #11 ∧ `output_labels` #12). `runtime_target_rows_collapsed == true`; `generator_grammar_count == 3` (json + css + sheets, NOT json + 7-css + sheets). | 1F-anti-pattern replica row (md5 `b654562c`; `1F-anti-pattern.md:45`). 1D `D-2`/`G-12`/`G-13 P3` (`1D-skinny-lessons.md:87-93`,`:208`). 2C REFUTATION: md5-distinctness ALONE does NOT prove the un-fork; the structural row-collapse co-gate is required. `sk-v18/SPEC.md:80-85`,`:635-663`,`:247`. | `restart/MIGRATION.md` replica/overfit disposition rows. | Receiver: PRUNE-3 (P3), dispatchable now. Blocker: a relocated seam caught ONLY by structural row-collapse, never by arm-grep. Gate: md5-distinct (post-collapse the binding witness is `runtime_target_rows_collapsed == true` over the full per-`grammar_name` config-tuple, NOT a self-glob over the deleted `css_l4_*`). | The replica overfit (one scan re-derived into 7 files) is the totality-tree relocated-seam analog (`ir/registry/strategy.rs` 9-grammar table, COH18-005) the SK-V19 fold inherits; collapsing it in skinny first is the monotonic precedent. |
| 3F-MH-012 | MIGRATION delta: record the **phantom `<G: EventGrammar>` DELETE** (`tape/mod.rs:175`,`:179`,`:197`), preserving the REAL `K=Kind` axis untouched. The `<G>` axis has ZERO non-test production instantiation; `phantom_generic_resolved == deleted`. AND record the companion **Lock 14 generality-vehicle reconcile** (1A-LOCK1-AMEND-001): strike "The `G:EventGrammar` type parameter is the generality vehicle" at `LOCKS.md:620` and re-anchor the generality claim on (a) the shared `Cursor` micro-trait (G4b, ≥2 non-collapsible impls) + (b) the config-breadth classifier — a 1-line LOCKS reconcile DEFERRED to SK-V19 / Pass Omega, NOT a T-P1 amendment. | 1A `1A-SUB-023` (census EMPTY of non-test instantiation) + `1A-LOCK1-AMEND-001` (Lock 14 reconcile; `restart/audit/totality/p1/1A-substrate-evidence.md:95`,`:180`). 1F `COH18-008` (`1F-coherence-scan.md:82`,`:101`). 1D `D-5`/`G-8` (`1D-skinny-lessons.md:103`,`:204`). `sk-v18/SPEC.md:99-102`. | `restart/MIGRATION.md` phantom-axis disposition row + `restart/locks/LOCKS.md:620` companion reconcile (LOCKS edit is Pass Omega CRUD-3, NOT 3F). | Receiver: G4 (the DELETE); SK-V19 / Pass Omega (the LOCKS:620 reconcile). Blocker: any re-anchor that revives the phantom or collapses the K-axis. Gate: `phantom_generic_resolved == deleted`; the K-axis dispatch (`JsonNodeKind`/`RootKind`/`ObjectKind`) preserved. | The certified plan DELETES the very axis Lock 14:620 names as "the generality vehicle"; the reconcile routes the generality claim onto the two axes the clause itself already names. No lock-count change; no shape/directive/substrate change. |
| 3F-MH-013 | MIGRATION delta: record the totality **`crates/core/src/css_types.rs` RELOCATE-or-DELETE** as an SK-V19 (NOT SK-V18) decision. The file Lock 14:349 names VERBATIM as "the current overfitting mess" is still live in `crates/core/src/` (66 LOC, a grammar-named host shim in the GENERIC core crate). Lock 14 (c) does NOT apply as-is (it admits ONLY a separate `crates/<grammar>/` declaration crate); admissible ONLY if relocated to a `crates/css/` declaration crate, else DELETE. | 1F-anti-pattern `css_types.rs` row (`1F-anti-pattern.md:66`). 1F `COH18-006`/`U-COH18-002` (`1F-coherence-scan.md:80`,`:121`). `LOCKS.md:349` (names the file). 2C REFUTATION: neutral-name-on-one-grammar does NOT prove neutrality. | `restart/MIGRATION.md` totality-tree disposition rows (a SK-V19 receiver entry, NOT an SK-V18 owner path). | Receiver: SK-V19 totality fold. Blocker: leaving the named mess in `crates/core/src/` as-is. Gate: confirm path is `crates/core/src/css_types.rs` (it is); at SK-V19 decide relocate-to-`crates/css/`-declaration-crate (admissible) vs delete. | The SK-V18 benched tree is skinny; `css_types.rs` is a totality-tree carrier. Recording it as an EXPLICIT SK-V19 migration decision (not silently dropped) discharges the dispatch's no-silent-disposition rule for the totality surface the SK-V18 generalization does not touch. |

## Proposal-Only Text Carriers

These carriers are NOT applied here. They are suggested content shapes for Pass
Omega V6 CRUD.

### MIGRATION Carrier — §0.0 SK-V18 Generalization Receiver

```md
## 0.0 Current SK-V18 Pass Omega V6 Migration Receiver

Status: proposal-only until Pass Omega V6 converges, CRUD applies, and G-Omega
closes. SK-V18 is the GENERALIZATION cycle on the SKINNY tree: un-fork JSON+CSS
into ONE `.bbnf`-driven generator emitting JSON+CSS+Sheets, aarch64-only, net
≈ −10800 LOC. The totality `crates/core/` adoption is SK-V19, NOT SK-V18.
Governance (per 3F-MH-004): T-P1 near-converged NON-normal-§3Z (V7 lone clean
r=1.000, V8 broke the streak; consec=0, converged=false — not a normal two-clean
§3Z lock); T-P2 near-converged NON-normal-§3Z (converged=false, consec=0); T-P3
in-cycle hardening (NOT yet a final-convergence lock). Order:
PRUNE-before-GENERALIZE-before-PROVE.

| Receiver wave | Migration consequence | Net LOC | Exit gate |
|---|---|---|---|
| P1 x86 DELETE | x86 crate-wide gone (src/x86_64 + ext/x86 + nasm driver + build-dep + dispatch arms + 9 checkasm call sites decoupled). | ≈ −4500 | find x86 == 0; aarch64-neutral grep; build/test --no-run clean; x86_tree_deleted == true |
| P2 warm CSS bench DELETE | warm micro-fixture machinery + SHA256 scaffold gone; 9-field cold oracle retained. | ≈ −700 | grep measure_mbps|lightningcss_facts == 0; css_canon_bench green |
| P3 replica COLLAPSE | 7 byte-identical css_l4 replicas → ONE config; 7 RuntimeTarget rows → ONE via PartialEq full-row. | ≈ −5500 (SPEC `:435`: −5460 = 6×910 replica bodies + ~−40 collapsed rows; 6 of 7 deleted) | runtime_target_rows_collapsed == true; generator_grammar_count == 3 |
| P4 Lock-14 gate FIX | runtime_generator.rs into strict GENERIC_SCAN_ROOTS; diagnostic-x86 exclusion dropped; FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}. MUST LAND BEFORE G2/G3. | ≈ +15 | re-inject forbidden token → RED/revert; lock14_gate_scans_codegen == true |
| P5 metalang PURGE | parse_w11_1_number ×7 → parse_number_* at template source. | ≈ 0 (rename) | grep -c parse_w11_1_number == 0; regen --check clean |
| G1 JSON projection | json_sink_direct / json_templates retired; SinkOnlyExpr AST-walk emitter; byte-equivalent vs oracle BEFORE oracle delete. | ≈ 0 generated | json byte-equivalence; verbatim_blob_present == false (JSON) |
| G2 CSS lowering | CSS_GENERATED_RS DELETED; css_balanced_component_scan primitive + fact-keyed projection. | ≈ −910 net | CSS_GENERATED_RS deleted; verbatim_blob_present == false |
| G3 un-fork emitter | RuntimeEmitterKind DELETED; dispatch on BackendShape, not grammar tag. | ≤450 hand | emit_shape_source == lowered_program; byte-equivalent output |
| G4 value-API + phantom | Cursor micro-trait (tape/cursor.rs) over EXISTING tape; <G> DELETED, K-axis preserved; JSON rich-nav byte-equal. | ≤450 hand | phantom_generic_resolved == deleted; shared trait ≥2 impls |
| G5/G6 NEON retarget | NEON onto the CSS scan shell; json/scan.rs neutralized; checkasm-gated scalar twin. | ≤450 hand | named SIMD call-site; checkasm parity green |
| PROVE Sheets | Sheets via the un-forked generator ONLY; precedence-tower core. | ≈ +200 | sheets_grammar_shape == pratt-operator; md5-distinct from JSON∧CSS |
| H1 honesty close | CSS framing honesty + corpus-in-timer + regen --check clean; CSS ratio re-locked. | ≈ 0 | css_canon_bench re-locked (≥1 regular corpus crossing >1.0×); regen --check clean |
```

### HANDOFF Carrier — Current Totality Override

```md
## Current Totality Override — SK-V18 Generalization Cycle

Status: SK-V18 is the GENERALIZATION cycle on the SKINNY tree (skinny/crates/):
un-fork the two hand-written/forked parsers (JSON + CSS) into ONE grammar-driven
generator emitting JSON + CSS + Sheets from `.bbnf`, aarch64-only, preserving
>SOTA honestly, net ≈ −10800 LOC. The totality `crates/core/` adoption is SK-V19.

Skinny S-P0..S-P3 CERTIFIED (12-wave SPEC). Totality T-P1 near-converged
NON-normal-§3Z (V7 lone clean r=1.000, V8 broke the streak; consec=0,
converged=false), T-P2 near-converged NON-normal-§3Z (converged=false, consec=0),
T-P3 in-cycle hardening (the current V-cycle; NOT yet a final-convergence lock).
After T-P3 cohort lock, G3 auto-passes under
the active non-G-Omega gate pin and the packet flows into Pass Omega V6. No
SK-V18 implementation wave dispatches until Pass Omega V6 CRUD has updated
HANDOFF/MIGRATION and G-Omega has authorized the required V1 patches.

Current SK-V18 wave authority routes through
`restart/skinny/tranches/sk-v18/SPEC.md` (the 12-wave manifest) and
`restart/skinny/tranches/sk-v18/HANDOFF.md` (the generalization handoff). The
prior `restart/HANDOFF.md:17-19` "SK-V18 adopts … into the totality `crates/core/`
tree" definition is STRUCK — that adoption is SK-V19.

Next directive after Pass Omega V6 / G-Omega: dispatch SK-V18 W-PRUNE (P1-P5)
through the SKINNY triumvirate — the ONLY dispatch-eligible cluster on close;
every GENERALIZE/PROVE/HONESTY wave stays blocked until its predecessor's exit
gate closes AND its entry-gate predicate holds GREEN.
```

## Consequences

| delta id | positive consequence | cost / risk | propagation |
|---|---|---|---|
| 3F-MH-001 | Current migration authority starts at SK-V18 Pass Omega V6, not SK-V17/SK-V14 history. | 25-45 doc LOC; low risk if historical sections are not renumbered. | 1 surface: MIGRATION. |
| 3F-MH-003 | The PRUNE-before-GENERALIZE delete-before-replacement guard is in migration, not rediscovered during waves. | 20-40 doc LOC; medium risk if duplicated inconsistently. | 3 surfaces: MIGRATION, HANDOFF, SK-V18 SPEC reference. |
| 3F-MH-004 | Governance history (3 totality passes, distinct lock provenance) stays honest and survives Pass Omega. | 15-30 doc LOC; low risk. | 2 surfaces: MIGRATION and HANDOFF. |
| 3F-MH-005 | Cold-start handoff routes to the certified SK-V18 generalization scope + extant SK-V18 SPEC/HANDOFF; the most material drift (COH18-001) is closed. | 100-200 doc LOC; medium risk because it strikes the stale SK-V18 paragraph and replaces a long current-state block. | 2 surfaces: HANDOFF and Pass Omega V6 CRUD log. |
| 3F-MH-006 | SK-V18 blockers become executable next-work rows, not prose. | 60-110 doc LOC; low-medium risk. | 2 surfaces: HANDOFF and SK-V18 SPEC references. |
| 3F-MH-007 | Prevents implementation waves from starting before V1 patch authorization; honours the W-PRUNE-first dispatch lock. | 40-80 doc LOC; low risk. | 3 surfaces: HANDOFF, MIGRATION, Pass Omega V6 CRUD. |
| 3F-MH-008 | The x86 crate-wide DELETE is recorded with a reach-matched deletion list (no RED-by-construction gate). | 30-60 doc LOC; medium risk (the 9 checkasm call sites must decouple in-commit or the build breaks). | 2 surfaces: MIGRATION + ARCH CollapsedStage-x86 reconcile (SK-V19). |
| 3F-MH-009 | Migration routes from the actual certified 12-wave REDUCTION ledger (≈−10800 LOC). | 80-140 doc LOC; medium risk due to table density. | 2 surfaces: MIGRATION + SK-V18 SPEC manifest. |
| 3F-MH-010 | The courier retirement is recorded under the diff-control + relocated-seam falsifier (no relabeled blob). | 30-50 doc LOC; medium risk if the seam re-enters via a neutral data table. | 2 surfaces: MIGRATION + §12 ARCH leak-scan hardening. |
| 3F-MH-011 | The 7-replica + RuntimeTarget COLLAPSE is recorded under the structural co-gate arm-grep cannot see. | 25-45 doc LOC; medium risk (post-collapse self-glob is unfalsifiable; the binding witness is `runtime_target_rows_collapsed`). | 2 surfaces: MIGRATION + the SK-V19 `ir/registry/strategy.rs` 9-grammar analog. |
| 3F-MH-012 | The phantom DELETE + Lock 14:620 reconcile are recorded together; the K-axis is preserved. | 20-40 doc LOC; low risk (LOCKS edit is Pass Omega CRUD-3, deferred to SK-V19). | 2 surfaces: MIGRATION + LOCKS:620 companion reconcile. |
| 3F-MH-013 | The totality `css_types.rs` RELOCATE-or-DELETE is an explicit SK-V19 decision, not a silent drop. | 15-30 doc LOC; low risk. | 1 surface: MIGRATION (SK-V19 receiver entry). |

## CH4 V6 Coverage Matrix

| delta id | LOC | propagation | risk | wave alignment | consumer / gate | hard-cap fit | fail action |
|---|---:|---:|---|---|---|---|---|
| 3F-MH-001 | 25-45 | 1 | low | Pass Omega V6 CRUD before G-Omega. | CRUD / G-Omega + CRUD log. | Doc-only receiver insertion; no implementation work. | If receiver cannot fit, record blocked/extension remainder; block W-PRUNE. |
| 3F-MH-003 | 20-40 | 3 | medium | Pass Omega V6 CRUD; enforced by GENERALIZE waves. | G1/G2/P3 oracle-equivalence gates. | One migration gate clause; oracle work in owning waves. | If oracle proof absent, fail closed by blocking the delete; record REDRESS/revert route. |
| 3F-MH-004 | 15-30 | 2 | low | Pass Omega V6 CRUD governance patch. | G-Omega sign-off on T-P1/T-P2/T-P3 governance text. | Doc-only governance paragraph. | If T-P1 rewritten as normal §3Z or T-P3 as two-clean, reject the CRUD hunk; block G-Omega. |
| 3F-MH-005 | 100-200 | 2 | medium | Pass Omega V6 CRUD before G-Omega; W-PRUNE verifies after. | HANDOFF override / G-Omega + W-PRUNE pre-dispatch. | Replacement of stale current-state block + strike of `:17-19`, not broad rewrite. | If stale SK-V18-adopt definition remains, record blocked/extension; keep W-PRUNE blocked. |
| 3F-MH-006 | 60-110 | 2 | low-medium | Pass Omega V6 CRUD; W-PRUNE..H1 consume blocker rows. | SK-V18 SPEC per-wave exit gates. | Compact HANDOFF blocker matrix; implementation stays in waves. | If any blocker row lacks a gate, block that wave entry or route G-Omega amendment. |
| 3F-MH-007 | 40-80 | 3 | low | T-P3 lock → G3 auto-pass → Pass Omega V6 CRUD → G-Omega → W-PRUNE. | CRUD, G-Omega, then SK-V18 W-PRUNE entry. | Directive replacement; no silent deferral. | If CRUD or G-Omega incomplete, record blocked/extension; keep W-PRUNE blocked. |
| 3F-MH-008 | 30-60 | 2 | medium | Pass Omega V6 CRUD (doc); PRUNE-1 (impl) after G-Omega. | P1 exit gate + ARCH CollapsedStage-x86 reconcile (SK-V19). | Doc receiver; the in-commit checkasm decouple is a P1 implementation obligation, not doc work. | If the deletion list is narrower than the verify grep, return REVISE (RED-by-construction gate). |
| 3F-MH-009 | 80-140 | 2 | medium | Pass Omega V6 CRUD (doc); W-PRUNE..H1 consume after G-Omega. | SK-V18 SPEC 12-wave manifest + rerun ceilings. | Compact 12-row REDUCTION ledger; ≤12 wave ceiling exactly at cap, no W13 overflow. CH4-V1: P3 = ≈−5500 cited verbatim from `sk-v18/SPEC.md:435` (−5460 replica bodies + ~−40 collapsed rows), aligned with the decisions table and 3D's −5460 replica-body figure. | If a wave→consequence row lacks proof, route G-Omega amendment or blocked/extension. |
| 3F-MH-010 | 30-50 | 2 | medium | Pass Omega V6 CRUD (doc); G1/G2 (impl). | G1 byte-equivalence + G2 courier-deleted + relocated-seam falsifier. | Doc receiver; oracle delete bound to G1/G2 gates. | If the seam re-enters via a neutral data table, fail the structural co-gate; block G3. |
| 3F-MH-011 | 25-45 | 2 | medium | Pass Omega V6 CRUD (doc); PRUNE-3 (impl). | `runtime_target_rows_collapsed == true` (R16 full-row PartialEq). | Doc receiver; the +1-line PartialEq derive is a P3 obligation. | If post-collapse witness is a self-glob, treat as unfalsifiable; require the row-collapse witness. |
| 3F-MH-012 | 20-40 | 2 | low | Pass Omega V6 CRUD (doc); G4 (DELETE); SK-V19/CRUD-3 (LOCKS:620). | `phantom_generic_resolved == deleted` + K-axis preserved + LOCKS:620 reconcile. | Doc receiver; the LOCKS:620 strike is Pass Omega CRUD-3, deferred to SK-V19. | If the re-anchor revives the phantom or collapses K-axis, reject; the LOCKS edit waits for SK-V19. |
| 3F-MH-013 | 15-30 | 1 | low | Pass Omega V6 CRUD (doc, SK-V19 receiver entry). | SK-V19 relocate-vs-delete decision gate. | Doc-only SK-V19 receiver entry; no SK-V18 implementation. | If left in `crates/core/src/` as-is, it is the named mess; require the SK-V19 decision. |

## Next-Cycle Dispatch Directive

Concrete, measurable entry conditions. The sequence is: T-P3 lock → G3
auto-pass → Pass Omega V6 → G-Omega → SK-V18 W-PRUNE (P1-P5 FIRST) → SK-V19
totality-fold tee-up.

1. **T-P3 lock.** The SK-V18 T-P3 cohort completes 3A..3F, then CH1..CH6 hardening
   iterates until T-P3 lock or the V≤5 ceiling. The SK-V18 T-P3 is IN-CYCLE
   (hardening at `restart/audit/totality/p3/hardening/V1/`), NOT yet a
   final-convergence lock (CH1-V1-C5: the SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md`
   is a prior-cycle record, not the SK-V18 state). G3 auto-passes on cohort lock
   under the active user pin; only G-Omega triggers user relinquish.
2. **Pass Omega V6 dispatch.** On T-P3 lock, dispatch Pass Omega **V6** (V5
   CLOSED for SK-V17 at `33b51d8f4`). Pass Omega consumes the SK-V18 totality
   cycle + the SK-V18 skinny REDRESS/RESULTS into V1 spec surfaces; HANDOFF +
   MIGRATION are assigned to the CRUD HANDOFF/MIGRATION leg (the SK-V17 analog
   was CRUD-4 at `c5a4f7644`).
3. **CHALLENGE-before-CRUD.** Pass Omega V6 CHALLENGE must converge before CRUD.
   CRUD stays within the consolidated authorization; no CRUD agent edits beyond
   what CHALLENGE authorizes.
4. **CRUD current-state cleanup.** CRUD resolves the current-state
   HANDOFF/MIGRATION cleanup before G-Omega — striking the stale
   `restart/HANDOFF.md:17-19` SK-V18-adopt definition (3F-MH-005), inserting the
   SK-V18 §0.0 receiver (3F-MH-001), and applying the five migration decisions
   (3F-MH-008..013). If the cap blocks that cleanup, CRUD records a
   blocked/extension decision naming the exact remainder, receiver, blocker, and
   gate; any remainder touching current dispatch truth blocks SK-V18 W-PRUNE
   until complete.
5. **G-Omega authorizes V1 patches.** G-Omega then authorizes the required V1
   patches (including the LOCKS:620 generality-vehicle reconcile if scoped into
   this Omega, else deferred to SK-V19 CRUD-3). After G-Omega closes, the V1
   spec is v+1.
6. **SK-V18 W-PRUNE dispatch (P1-P5 FIRST).** Only after Pass Omega V6 CRUD
   current-state truth is complete, G-Omega has authorized the patches, and
   SK-V18 authority routes through `restart/skinny/tranches/sk-v18/SPEC.md`, may
   the orchestrator update HANDOFF to `ready-for-W-PRUNE` and dispatch the
   **W-PRUNE (P1-P5) triumvirate** — the ONLY dispatch-eligible cluster on close
   (`sk-v18/SPEC.md:46-49`). P1-P5 are entry-gate-free and MAY land in parallel
   on disjoint paths (P1 `bbnf-simd/`, P3 `xtask/regen*.rs` + the 7
   `css_l4_*/generated.rs`); **P4 MUST land before G2/G3** (it tightens the
   Lock-14 gate the GENERALIZE waves depend on). Measurable entry conditions:
   `x86_tree_deleted == true` (P1), `runtime_target_rows_collapsed == true`
   (P3), `lock14_gate_scans_codegen == true` (P4), `grep -c parse_w11_1_number
   == 0` (P5).
7. **GENERALIZE/PROVE gating.** No GENERALIZE/PROVE wave dispatches until its
   predecessor closes its exit gate AND its entry-gate predicate holds GREEN AND
   the orchestrator/user dispatches the wave triumvirate. G1 → G2 (G1 ∧ P3
   close, P4 live) → G3 (G1 ∧ G2 close ∧ P4 live ∧ P3 row-collapse) → G4 (G1 ∧
   G2 ∧ G3 close); G5/G6 hangs off G3 PARALLEL to G4; PROVE needs G4 closed
   directly (NEVER dispatch PROVE before G4 closes); H1 needs G5/G6 ∧ PROVE
   close. H1 cannot close if the CSS >SOTA ratio is not re-locked on
   `css_canon_bench` with ≥1 regular corpus crossing >1.0× same-run.
8. **SK-V19 totality-fold tee-up.** SK-V19 is the totality adoption cycle: the
   SK-V18-proven un-fork adopted into `crates/core/`. Concrete SK-V19 entry
   carriers surfaced by THIS cycle (each cited, none silently dropped): (a) the
   `ir/registry/strategy.rs` 9-grammar `PRODUCTION_MANIFEST_TABLE`
   relocated-seam analog (COH18-005; R16 structural row-collapse over ALL 9
   rows); (b) the totality `css_types.rs` RELOCATE-or-DELETE (3F-MH-013); (c)
   the Pattern-H 67/71 baseline-command reconcile (`tape/` exclusion or +4
   substrate-trace, COH18-007); (d) the scanner-crate asymmetry resolution
   (`simd-scan` random-access `next_structural_at_or_after` + the 8/9
   `OnceCell<StructuralIndex>` emission re-route, COH18-015); (e) the LOCKS:620
   generality-vehicle 1-line reconcile (1A-LOCK1-AMEND-001). SK-V19 is REMAINDER
   after SK-V18 proof, NOT a substitute for SK-V18 generalization.

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 / CH6 | The corpus has a historical "Pass Omega V5" receiver (SK-V17, CLOSED at `33b51d8f4`); the next astral pass is **Pass Omega V6**. Should CRUD label the current pass "SK-V18 Pass Omega V6" and keep the SK-V17 V5 section provenance-only to avoid false-current citation? | Pass Omega V6 CRUD. | V5-already-closed name reuse vs the current SK-V18 V6 pass. | G-Omega sign-off text + CRUD log. |
| CH2 | Does the totality `crates/core/src/grammar/generated/{9}.rs` fork on the `ir/registry/strategy.rs` 9-grammar table (U-COH18-001), making the SK-V19 totality un-fork a REAL obligation rather than a relabel? The SK-V18 skinny COLLAPSE (3F-MH-011) is the monotonic precedent, but the totality breadth (9 grammars, 5 of 9 escaping the strict 4-name leak regex) is the SK-V19 R16 receiver. | SK-V19 totality fold. | Whether `PRODUCTION_MANIFEST_TABLE` is consumed by the generator or only `regen --check`. | CH2 / SK-V19 structural row-collapse co-gate over all 9 rows. |
| CH3 | Does every proposed migration deletion (x86, courier, replicas, phantom) preserve the SK-V18 Rejected-Route Pre-Block — REDRESS items 51, 53, 246, 247 — so a GENERALIZE wave does not re-implement a measured-and-reverted shape (second scanner / structural-stream driver / parser-local cursor)? | SK-V18 G2/G4/G6 wave plans + the Pass-Omega-V6 / pre-W-PRUNE SK-V16/V17 reconcile receiver (3F-MH-003). | A reopened REDRESS route or an omitted pre-block route; OR an unreconciled SK-V16/V17 pre-block (1D U-5: the committed ledger ends at SK-V15 W11, so SK-V16/V17 rejects are structurally invisible). | CH3-V1-R2: G2/G4/G6 entry is BLOCKED until the SK-V16/V17 reconcile is on the committed ledger (NOT deferred to SK-V19 entry — the waves run during SK-V18); CH3/CH7 re-check during each GENERALIZE wave plan (`1D-skinny-lessons.md:156-173`,`:244-248`). |
| CH4 / CH7 | Is the CSS >SOTA ratio re-lock (H1) a hard MIGRATION gate, or directional-only? The S-P1 ratios ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked (U-4). Migration must not credit the un-caveated "MEASUREMENT-VALID" closure word on the CSS ratio until `css_canon_bench` re-locks. | H1 wave + Pass Omega V6 CRUD. | An un-caveated CSS >SOTA closure word in MIGRATION/HANDOFF before the H1 re-lock. | H1 `css_canon_bench` re-lock (≥1 regular corpus crossing >1.0× same-run; `1D-skinny-lessons.md:239-243`). |
