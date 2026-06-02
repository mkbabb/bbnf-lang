---
agent: ΩF
pass: Pass-Omega-astral
cycle: V10
pass_label: "Pass Omega V10 (SK-V18 generalization cycle)"
generated_at: 2026-06-01T00:00:00Z
scope: "Ω-F — MIGRATION + HANDOFF + next-cycle dispatch directive (STAGED ONLY)"
consumes:
  - restart/audit/totality/p3/3F-migration-handoff.md   # the authoritative T-P3 synthesis (12 deltas)
  - restart/audit/totality/astral/V9/ΩF-migration-handoff.md   # prior astral Ω-F (SK-V15 lineage)
  - restart/HANDOFF.md   # live top-level surface (read-only; staged delta targets it)
  - restart/MIGRATION.md   # live top-level surface (read-only; staged delta targets it)
  - restart/skinny/tranches/sk-v18/SPEC.md   # the certified 12-wave manifest
  - restart/locks/LOCKS.md   # live LOCKS (read-only; LOCKS:349 / Lock-14 generality-vehicle reconcile)
staged_diff_files:
  - restart/audit/totality/astral/V10/migration-delta.staged.md
  - restart/audit/totality/astral/V10/handoff-delta.staged.md
disposition: PROPOSAL-ONLY — CRUD applies post-G-Omega V10
boundary: "STAGED DIFFS ONLY. This pass does NOT merge any governance surface. CRUD-4 (HANDOFF+MIGRATION) executes POST-G-Omega-V10."
---

# Ω-F — MIGRATION + HANDOFF + Next-Cycle Dispatch (Pass Omega V10, SK-V18 Generalization)

## Boundary Statement

This artefact PROPOSES only. It does not edit `restart/MIGRATION.md`,
`restart/HANDOFF.md`, `restart/locks/LOCKS.md`, `restart/ARCHITECTURE.md`, or
`restart/MASTER-PLAN.md`. The two staged diff files
(`migration-delta.staged.md`, `handoff-delta.staged.md`) are the content shapes
CRUD-4 applies AFTER the user signs off at the mandatory G-Omega V10 gate. Until
G-Omega V10 closes, every clause here is proposal-only.

## Pass-Label Reconciliation (V6 ↔ V10) — load-bearing

The upstream T-P3 `3F-migration-handoff.md` frontmatter carries
`pass_omega_index: V6`, derived from the SK-V15 lineage ("Pass Omega V5 already
CLOSED for SK-V17 at `33b51d8f4`, so the next astral pass is V6"). The LIVE
corpus, however, labels the astral lineage by the directory V-index, which is
authoritative for cold-start citation:

- The SK-V17 tape-fold pass CLOSED as **"Pass Omega V5"**
  (`restart/HANDOFF.md:5`; `33b51d8f4` = "pass-omega-v5-crud6-audit").
- The astral directory lineage is V1..V9 (SK-V15/SK-V17 hardening cycles), and
  THIS cycle is the **V10** directory (`restart/audit/totality/astral/V10/`),
  driven by the workflow `skv18-pass-omega-v10`
  (`restart/audit/totality/astral/V10/skv18-pass-omega-workflow.mjs:2`).
- The live `restart/HANDOFF.md:47` already records "Historical Pass Omega
  V2..V9 packets" — so a "V6" label for the CURRENT SK-V18 pass would COLLIDE
  with the historical V6 (`restart/MIGRATION.md:190` "Historical Pass Omega V6
  W5BR Migration Receiver").

RESOLUTION: this pass is **Pass Omega V10**. The 3F "V6" content-index is folded
to the live directory index V10 to prevent a false-current citation against the
historical V6 W5BR receiver. Everywhere 3F says "Pass Omega V6", the staged
diffs read "Pass Omega V10". This is the single corpus-cohesion correction Ω-F
makes to the otherwise-CONSUMED 3F; it does not alter any delta's substance,
receiver, blocker, or gate.

## What Ω-F Consumes vs Adds

Ω-F CONSUMES the T-P3 3F verbatim (all 12 deltas 3F-MH-001/003..013; 3F-MH-002
already removed upstream). It does NOT re-derive them. Ω-F adds three things on
top of the 3F synthesis, each required to make the diffs CRUD-applicable:

1. **The pass-label reconciliation** (V6→V10) above.
2. **The two STAGED diff files** that render the 3F proposal-only text carriers
   as concrete, line-anchored MIGRATION/HANDOFF deltas against the LIVE surfaces
   (3F gave content shapes; Ω-F anchors them at the live insertion points and
   the live struck lines).
3. **The governance-honesty cross-check** against the live SK-V17 surfaces — so
   the staged HANDOFF/MIGRATION current-state text supersedes (not contradicts)
   the live "Pass Omega V5 SK-V17 tape-fold CLOSED" state.

## The Single Most Material Drift (COH18-001) — confirmed live

The drift 3F names is CONFIRMED against the live surface at this snapshot:

- `restart/HANDOFF.md:16-19`: "The next IMPLEMENTATION tranche is **SK-V18**: it
  adopts the SKINNY-proven unified-tape / lazy-`ValueRef` / shared-NEON model
  into the totality `crates/core/` tree".
- `restart/HANDOFF.md:103-104`: the live SK-V17 dispatch directive says
  "dispatch **SK-V18 W0** (the `crates/core` tape-fold)".

Both define SK-V18 as a totality-`crates/core/`-adopt cycle. The CERTIFIED
SK-V18 is the GENERALIZATION cycle on the SKINNY tree
(`restart/skinny/tranches/sk-v18/SPEC.md:19-21`,`:58-61`): un-fork JSON+CSS into
ONE `.bbnf`-driven generator emitting JSON+CSS+Sheets, aarch64-only, ≈ −10800 campaign LOC (per-wave SPEC sum ≈ −10685; `sk-v18/SPEC.md:571`). The
`crates/core/` adoption is SK-V19. The staged HANDOFF delta
STRIKES `restart/HANDOFF.md:16-19` AND re-roots the SK-V18 line in the dispatch
directive (`:103-105`) onto the SKINNY generalization scope, reassigning the
`crates/core/` tape-fold to SK-V19.

## The Five Migration Decisions (3F-MH-008..013, consumed)

Each is a staged MIGRATION delta carrier (full text in
`migration-delta.staged.md`). Re-stated here with the live grounding re-verified
at this snapshot:

| Decision | Kind | Receiver wave | Net LOC | Live grounding re-verified |
|---|---|---|---|---|
| x86 surface crate-wide | DELETE | P1 | ≈ −4500 | `sk-v18/SPEC.md:435` ("DELETE the whole x86 surface crate-wide (aarch64-only) … ≈ −4500"); 1F `COH18-009` |
| `CSS_GENERATED_RS` courier (+ JSON `_RS` literals) | RETIRE | G2 / G1 | ≈ −910 CSS + JSON | `sk-v18/SPEC.md:61-69` (courier RETIRED, `verbatim_blob_present == false`); 1D `C-1`/`G-6` |
| 7 byte-identical css_l4 replicas + 7 `RuntimeTarget` rows | COLLAPSE | P3 | ≈ −5500 | `sk-v18/SPEC.md:435` (P3: 6×910 = −5460 replica bodies + ~−40 collapsed rows + 1 `PartialEq`) |
| phantom `<G: EventGrammar>` axis | DELETE | G4 | — (decoration) | `sk-v18/SPEC.md` G4 row (`:444`: "DELETE `<G>`"); 1A `1A-SUB-023` |
| totality `crates/core/src/css_types.rs` | RELOCATE-or-DELETE | SK-V19 | 66 LOC | `restart/locks/LOCKS.md:349` names it VERBATIM ("`crates/core/src/css_types.rs`"); SK-V19 receiver, NOT SK-V18 |

The Lock-14 generality-vehicle reconcile (1A-LOCK1-AMEND-001) attaches to the
phantom DELETE: the live `restart/locks/LOCKS.md:620` clause says "The
`G:EventGrammar` type parameter is the generality vehicle" — the certified plan
DELETES that axis. The 1-line LOCKS strike + re-anchor onto (a) the shared
`Cursor` micro-trait + (b) the config-breadth classifier is a Pass-Omega
CRUD-3 / SK-V19 edit, NOT an Ω-F edit. Ω-F only RECORDS the companion reconcile
in the MIGRATION phantom-axis disposition row; it does not touch LOCKS.

## Governance Honesty Carrier (3F-MH-004, consumed)

The staged HANDOFF/MIGRATION text carries the SK-V18 totality-pass provenance
EXACTLY as the 3F-MH-004 record, with the totality-tree consolidated files as
the authority (NOT the SK-V15 T-P3 V5 file):

- **T-P1 SK-V18**: near-converged NON-normal-§3Z. The current-cycle hardening
  verdict supersedes the 3F V7/V8 streak note: per the on-disk SK-V18 T-P1
  consolidated (`restart/audit/totality/p1/hardening/`), the pass is near-
  converged, NOT a normal two-clean §3Z lock. The staged text does NOT assert a
  normal §3Z lock and does NOT cite the SK-V15
  `HARDENING-T-P3-V5-CONSOLIDATED.md` (target `77b6e9fd7`, a 42-candidate /
  23-ACCEPT-19-MODIFY SK-V15 matrix) for the SK-V18 state.
- **T-P2 SK-V18**: near-converged NON-normal-§3Z (converged=false, consec=0;
  single-cell citation-precision qualifiers from V4, no surviving REJECT).
- **T-P3 SK-V18**: CONVERGED into the 3A..3F synthesis + the 3C-locks-v+1-diff
  (21 candidates disposed: 9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER; git apply
  --check exit 0 against live LOCKS.md). The dispatch context for THIS pass
  states T-P1/T-P2/T-P3 CONVERGED for the Pass-Omega-V10 consumption; the staged
  text states the totality cohort as CONVERGED-for-Pass-Omega while preserving
  the per-pass non-normal-§3Z honesty (T-P1/T-P2 near-converged) so no V1
  surface over-claims a normal two-clean lock.

CITATION RULE (carried into the refusal conditions): no staged text may rewrite
T-P1/T-P2 SK-V18 as a normal §3Z lock, and no staged text may cite the SK-V15
T-P3 V5 file as the SK-V18 record.

## The 12-Wave Migration Receiver (3F-MH-009, consumed)

The staged MIGRATION §0.0 receiver carries the certified 12-wave REDUCTION
ledger (P1-P5 + G1 + G2 + G3 + G4 + G5/G6 + PROVE + H1; ≈ −10800 campaign LOC,
per-wave SPEC sum ≈ −10685; `sk-v18/SPEC.md:429-449`,`:571`). The
receiver is a REDUCTION ledger, not an addition plan: the generalization DELETES
far more than it adds. Each wave maps to its migration consequence, net LOC, and
exit gate. The P3 figure is cited verbatim from `sk-v18/SPEC.md:435` (≈ −5500:
6×910 = −5460 replica bodies + ~−40 collapsed rows + 1 `PartialEq` derive).

## The PRUNE-before-GENERALIZE-before-PROVE Gate Clause (3F-MH-003, consumed)

The staged MIGRATION delta adds a deletion/retirement gate clause (attaching to
the live §17 sequence + §19 gates, `restart/MIGRATION.md:886`,`:925`): no
GENERALIZE/PROVE wave deletes a hand-written ORACLE (JSON `json_templates/`, the
7 css_l4 replica bodies, the CSS courier) before its grammar-DERIVED replacement
lands byte-equivalent and the round-trip diff-control gate is GREEN. CH3-V1-R2:
G2/G4/G6 entry is BLOCKED until the SK-V16/V17 REDRESS reconcile (the four-item
pre-block, complete only for the SK-V15-W11 ledger; 1D U-5) is on the committed
ledger as a Pass-Omega-V10 / pre-W-PRUNE blocker — these waves abut REDRESS
items 51/53/246/247 (1D:168-171; the 3F CH3 row 3F-migration-handoff.md:274;
item 246 = the W11T parse-only structural-STREAM driver reject that bounds G4)
and run during SK-V18.

## Corpus-Cohesion Cross-Checks (Ω-F lens)

| check | live anchor | staged delta status |
|---|---|---|
| MIGRATION §0.0 is currently the SK-V17 receiver; SK-V18 receiver must insert ABOVE it | `restart/MIGRATION.md:30` | Staged: new §0.0 SK-V18 receiver inserted above; current §0.0 renumbers to §0.1, prior §0.1 (SK-V15 V9) → §0.2, etc. (historical sections NOT renumbered destructively — provenance-only). |
| HANDOFF current override is "Pass Omega V5 SK-V17 tape-fold CLOSED" | `restart/HANDOFF.md:3-28` | Staged: a NEW "Current Totality Override — Pass Omega V10 / SK-V18 Generalization" block inserted above; the SK-V17 close stays as the immediately-prior provenance line. |
| HANDOFF:16-19 defines SK-V18 as totality-`crates/core/` adopt | `restart/HANDOFF.md:16-19` | Staged: STRUCK; re-rooted to SK-V19. |
| HANDOFF dispatch directive defines "SK-V18 W0 (the `crates/core` tape-fold)" | `restart/HANDOFF.md:103-105` | Staged: the SK-V18 line re-rooted to the SKINNY W-PRUNE generalization; the `crates/core` tape-fold reassigned to SK-V19. |
| LOCKS:349 names `crates/core/src/css_types.rs` verbatim | `restart/locks/LOCKS.md:349` | Staged: recorded as the SK-V19 RELOCATE-or-DELETE migration decision (NOT touched by Ω-F). |
| LOCKS:620 "G:EventGrammar … generality vehicle" | `restart/locks/LOCKS.md:620` | Staged: the companion reconcile RECORDED in the MIGRATION phantom row; the LOCKS strike is SK-V19 / CRUD-3, not Ω-F. |
| 16-lock count + 5 BackendShape canon preserved | `restart/locks/LOCKS.md:107-108` | Staged: NO lock-count change; NO shape change. The 3C-locks-v+1-diff is amendment-by-addition. |

## CRUD-4 Receiver Map (post-G-Omega V10)

| CRUD leg | Surface | V10 operation | Staged source |
|---|---|---|---|
| CRUD-4a | `restart/MIGRATION.md` | Insert §0.0 SK-V18 Pass Omega V10 receiver above the SK-V17 receiver; renumber current §0.0→§0.1, §0.1→§0.2…; apply the five migration decisions as disposition rows; add the PRUNE-before-GENERALIZE gate clause to §17/§19; add the governance-honesty paragraph. | `migration-delta.staged.md` |
| CRUD-4b | `restart/HANDOFF.md` | Insert the "Current Totality Override — Pass Omega V10 / SK-V18 Generalization" block above the live SK-V17 override; STRIKE `:16-19`; re-root the dispatch directive SK-V18 line (`:103-105`); add the SK-V18 blocker matrix; replace the next-cycle directive with the Pass-Omega-V10 → G-Omega → W-PRUNE directive. | `handoff-delta.staged.md` |

CRUD-4 stays within the consolidated G-Omega V10 authorization. If the cap
blocks the current-state cleanup, CRUD records a blocked/extension decision
naming the exact remainder, receiver, blocker, and gate; any remainder touching
current dispatch truth blocks SK-V18 W-PRUNE until complete.

## Next-Cycle Dispatch Directive (concrete, measurable entry conditions)

Sequence: T-P3 cohort lock → G3 auto-pass → **Pass Omega V10** → G-Omega V10 →
**SK-V18 W-PRUNE (P1-P5 FIRST)** → SK-V19 totality-fold tee-up.

1. **T-P3 cohort lock.** The SK-V18 T-P3 cohort (3A..3F + CH1..CH6 hardening) is
   CONVERGED: the 3C-locks-v+1-diff disposed all 21 candidates (9 ACCEPT, 11
   MODIFY, 0 REJECT, 1 DEFER; git apply --check exit 0). G3 auto-passes on cohort
   lock under the active user pin; only G-Omega V10 triggers user relinquish.
2. **Pass Omega V10 dispatch.** On T-P3 lock, dispatch Pass Omega **V10** (NOT
   V6 — V5 CLOSED for SK-V17 at `33b51d8f4`; V6..V9 are historical lineage).
   Pass Omega consumes the SK-V18 totality cycle + the SK-V18 skinny
   RESULTS/REDRESS into the V1 spec surfaces; HANDOFF + MIGRATION are assigned to
   CRUD-4 (the SK-V17 analog was CRUD-4 at `c5a4f7644`).
3. **CHALLENGE-before-CRUD.** Pass Omega V10 6-lens CHALLENGE must converge to
   3Z before CRUD. CRUD stays within the consolidated authorization; no CRUD
   agent edits beyond what CHALLENGE + G-Omega authorize.
4. **CRUD current-state cleanup (before G-Omega).** CRUD resolves the
   HANDOFF/MIGRATION current-state cleanup: striking the stale
   `restart/HANDOFF.md:16-19` SK-V18-adopt definition + re-rooting the
   `:103-105` dispatch line, inserting the SK-V18 §0.0 receiver, applying the
   five migration decisions. Cap-blocked remainder → blocked/extension record
   (remainder/receiver/blocker/gate); any remainder touching current dispatch
   truth blocks SK-V18 W-PRUNE.
5. **G-Omega V10 authorizes V1 patches.** Mandatory user gate. G-Omega
   authorizes the required V1 patches (ARCHITECTURE/MASTER-PLAN/LOCKS/
   HANDOFF/MIGRATION) including the 3C-locks-v+1-diff merge and, if scoped, the
   LOCKS:620 generality-vehicle reconcile (else deferred to SK-V19 CRUD-3).
   After G-Omega closes, the V1 spec is v+1.
6. **SK-V18 W-PRUNE dispatch (P1-P5 FIRST).** Only after Pass Omega V10 CRUD
   current-state truth is complete, G-Omega has authorized the patches, and
   SK-V18 authority routes through `restart/skinny/tranches/sk-v18/SPEC.md`, may
   the orchestrator set HANDOFF to `ready-for-W-PRUNE` and dispatch the W-PRUNE
   (P1-P5) triumvirate — the ONLY dispatch-eligible cluster on close
   (`sk-v18/SPEC.md:46-49`). P1-P5 are entry-gate-free and MAY land in parallel
   on disjoint paths (P1 `bbnf-simd/`, P3 `xtask/regen*.rs` + the 7
   `css_l4_*/generated.rs`); **P4 MUST land before G2/G3**. Measurable entry
   conditions: `x86_tree_deleted == true` (P1), `runtime_target_rows_collapsed
   == true` (P3), `lock14_gate_scans_codegen == true` (P4), `grep -c
   parse_w11_1_number json/generated.rs == 0` (P5; SPEC `:755`/`:570`-scoped —
   the unscoped crate-wide count is 15: 7 generated + 7 template-source + 1
   `lib.rs:565` test-assert).
7. **GENERALIZE/PROVE gating.** No GENERALIZE/PROVE wave dispatches until its
   predecessor closes its exit gate AND its entry-gate predicate holds GREEN AND
   the orchestrator/user dispatches the wave triumvirate. G1 → G2 (G1 ∧ P3
   close, P4 live) → G3 (G1 ∧ G2 close ∧ P4 live ∧ P3 row-collapse) → G4 (G1 ∧
   G2 ∧ G3 close); G5/G6 hangs off G3 PARALLEL to G4; PROVE needs G4 closed
   DIRECTLY (NEVER dispatch PROVE before G4 closes); H1 needs G5/G6 ∧ PROVE
   close. H1 cannot close if the CSS >SOTA ratio is not re-locked on
   `css_canon_bench` with ≥1 regular corpus crossing >1.0× same-run (the U-4
   directional-not-re-locked caveat).
8. **SK-V19 totality-fold tee-up.** SK-V19 is the totality adoption cycle: the
   SK-V18-proven un-fork adopted into `crates/core/`. Concrete SK-V19 entry
   carriers surfaced by THIS cycle (each cited, none silently dropped):
   (a) the `ir/registry/strategy.rs` 9-grammar `PRODUCTION_MANIFEST_TABLE`
   relocated-seam analog (COH18-005; R16 structural row-collapse over all 9
   rows); (b) the totality `css_types.rs` RELOCATE-or-DELETE (3F-MH-013);
   (c) the Pattern-H 67/71 baseline-command reconcile (COH18-007);
   (d) the scanner-crate asymmetry resolution (COH18-015); (e) the LOCKS:620
   generality-vehicle 1-line reconcile (1A-LOCK1-AMEND-001). SK-V19 is REMAINDER
   after SK-V18 proof, NOT a substitute for SK-V18 generalization.

## Refusal Conditions (CRUD-4 fails closed if any remains true after the patch)

- `restart/HANDOFF.md:16-19` still defines SK-V18 as the totality-`crates/core/`
  tape-fold, or the dispatch directive still says "SK-V18 W0 (the `crates/core`
  tape-fold)".
- `restart/MIGRATION.md` has no current SK-V18 Pass Omega V10 receiver above the
  SK-V17 receiver.
- Any staged text labels the current pass "Pass Omega V6" (a false-current
  collision with the historical V6 W5BR receiver, `restart/MIGRATION.md:190`).
- Any staged text rewrites T-P1/T-P2 SK-V18 as a normal §3Z lock, or cites the
  SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md` as the SK-V18 record.
- A delete/retire wave can run before its byte-equivalent rebuild-provider proof
  (the PRUNE-before-GENERALIZE gate is absent or overridable).
- G2/G4/G6 can enter with the SK-V16/V17 REDRESS pre-block unreconciled on the
  committed ledger.
- The x86 deletion list is narrower than its verify grep (a RED-by-construction
  gate; the V3 escape the SK-V18 fold fixed).
- The lock-count changes from 16 or a 6th `BackendShape` enters via this CRUD.
- G-Omega V10 is bypassed or treated as optional; or G1/G2/G3/G-Alpha/wave
  confirmation gates are treated as mandatory user stops under the active pin.

## Open Questions (carried from 3F, none blocking Ω-F)

| lens | question | receiver | gate |
|---|---|---|---|
| CH1/CH6 | Label the current pass "Pass Omega V10" (live directory index) and keep the historical V6 W5BR section provenance-only, to avoid false-current citation. RESOLVED by Ω-F: V10 is authoritative; the 3F "V6" content-index is folded to V10. | Pass Omega V10 CRUD-4. | G-Omega sign-off text + CRUD log. |
| CH2 | Does the totality `crates/core/src/grammar/generated/{9}.rs` fork on the `ir/registry/strategy.rs` 9-grammar table (U-COH18-001), making the SK-V19 totality un-fork a REAL obligation? The SK-V18 skinny COLLAPSE is the monotonic precedent; the totality 9-grammar breadth is the SK-V19 R16 receiver. | SK-V19 totality fold. | CH2 / SK-V19 structural row-collapse co-gate over all 9 rows. |
| CH3 | Does every proposed deletion (x86, courier, replicas, phantom) preserve the SK-V18 Rejected-Route Pre-Block — REDRESS items 51, 53, 246, 247 — so a GENERALIZE wave does not re-implement a measured-and-reverted shape? | SK-V18 G2/G4/G6 wave plans + the Pass-Omega-V10 / pre-W-PRUNE SK-V16/V17 reconcile receiver. | G2/G4/G6 entry BLOCKED until the SK-V16/V17 reconcile is on the committed ledger. |
| CH4/CH7 | Is the CSS >SOTA ratio re-lock (H1) a hard MIGRATION gate, or directional-only? The S-P1 ratios ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked (U-4). | H1 wave + Pass Omega V10 CRUD. | H1 `css_canon_bench` re-lock (≥1 regular corpus crossing >1.0× same-run). |
