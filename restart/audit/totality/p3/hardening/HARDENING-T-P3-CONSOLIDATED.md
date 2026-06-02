# HARDENING T-P3 Consolidated (SK-V18 Totality Synthesis)

Cohort cycle: `V4-SKV18-totality` (shared frontmatter of 3A/3B/3C/3D/3E/3F).
Pass-Omega index of the next astral CRUD pass: **V6** (V5 already CLOSED for
SK-V17 at `33b51d8f4`).

CHALLENGE ran **5 cycles**; `converged = true` (consec=2 per orchestrator
convergence state; voids=0). The §3Z record below reconciles the loop-mechanical
trace with the V5 governance note: this is **not** two untouched consecutive
clean cycles — it is **V3 clean substantive cycle + V5 all-ACCEPT final
confirmation after a V4 citation-only `REVISE`**, under the V≤5 hard ceiling.

## Per-Cycle r (r = accept / 7 lenses; converged ⟺ r≥0.95 ∧ reject=0)

| cycle | target packet | A | R(evise) | X(reject) | r | converged | consec | verdict |
|---|---|---:|---:|---:|---:|---|---:|---|
| V1 | `0a0508acd` | 3 | 4 | 0 | 0.429 | no | 0 | `REVISE` (CH1/CH4/CH5/CH6) |
| V2 | `7885b29ab` | 6 | 1 | 0 | 0.857 | no | 0 | `REVISE` (CH4 field-coverage) |
| V3 | `e6c1c2a84` | 7 | 0 | 0 | 1.000 | yes | 1 | `ACCEPT` (clean cycle 1) |
| V4 | `e6c1c2a84` | 6 | 1 | 0 | 0.857 | no | 0 | `REVISE` (CH1 one out-of-range citation) |
| V5 | `77b6e9fd7` | 7 | 0 | 0 | 1.000 | yes | 1→lock | `ACCEPT` (final convergence, V≤5 ceiling) |

Zero `REJECT` across all 35 lens runs. Every cycle's executable checks passed:
proposal-only scope (T-P3 never edits a V1 surface), `git diff --check` clean,
`3C-locks-v+1-diff.md` applies cleanly to live `LOCKS.md`, 16 numbered locks +
five `BackendShape` variants preserved, stale-pattern scan empty, 3C covers all
42 live 1E/2X candidates with zero silent drops, and the two PLANNED co-gate
symbols (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`)
read `rg = 0` (disclosed PLANNED, never cited live).

## Proposed-Deltas Census (per V1 surface)

| agent | V1 surface targeted | total | carried | removed | newly added (SK-V18) | answered |
|---|---|---:|---:|---:|---:|---:|
| 3A | `ARCHITECTURE.md` | 26 | 12 (`ARCH-3A-V1-D01..D12`) | 0 | 14 (`ARCH-3A-V4-SK18-D01..D14`) | 4 |
| 3B | `MASTER-PLAN.md` | 14 | 4 (`MP-3B-V1-D01/D02/D09/D10`) | 7 (`D03..D08,D11`) | 10 (`MP-3B-SKV18-D01..D10`) | 5 |
| 3C | `LOCKS.md` | 11 clauses | 2 addenda verbatim (SK-V15 V3, SK-V17 tape-fold) | 0 | 11 (`D-SKV18-L01/L05-L10/L06/L08/L10/L13/L14×3/L16×2`) | 21 LACs |
| 3D | (proposal-only; → ARCH/LOCKS/MASTER/MIGRATION) | 12 | 10 (`3D-D01..D10`, re-anchored) | 0 | 2 (`3D-D11`, `3D-D12`) | 6 |
| 3E | (proposal-only; → ARCH/LOCKS) | 18 | 11 (`3E-D01..D11`) | 0 | 7 (`3E-D12..D18`) | 6 |
| 3F | `MIGRATION.md` + `HANDOFF.md` + dispatch | 12 | 6 (`3F-MH-001/003..007`) | 1 (`3F-MH-002`) | 6 (`3F-MH-008..013`) | 4 |

Total proposed surface = **93 deltas / clauses** across six surfaces (50 carried
or re-anchored, 8 removed/retired, 50 newly added SK-V18, with the 3C 11 clauses
folding the 21 LACs). All re-anchored deltas re-key the retired SK-V15 W0-W11
numbering onto the certified SK-V18 12-wave manifest (P1-P5 / G1-G6 / PROVE / H1).
The single load-bearing reconciliation is the **SK-V18 tranche-identity pivot**
(3B / 3F COH18-001): the live surfaces still define "SK-V18" as the
`crates/core/` tape-fold — that adoption is re-keyed to **SK-V19**; certified
SK-V18 is the GENERALIZATION cycle on the skinny tree, net ≈ −10800 LOC.

## 3C Disposition Tally (21 SK-V18 candidates: 1E×7, 1A×1, 2C×3, 2D×4, 2E×3, 2F×3)

| disposition | count | candidates |
|---|---:|---|
| **ACCEPT** | **9** | LAC-1E-V5-01, LAC-1E-V5-02, LAC-1E-V5-03, LAC-1E-V5-05, LAC-1E-V5-06, LAC-2D-V3-01, LAC-2D-V3-02, LAC-2D-V3-04, LAC-2F-V3-01 |
| **MODIFY** | **11** | LAC-1E-V5-04, LAC-1E-V5-07, 1A-LOCK1-AMEND-001, LAC-2C-SK18-01, LAC-2C-SK18-02, LAC-2C-SK18-03, LAC-2D-V3-03, LAC-2E-V6-01, LAC-2E-V6-02, LAC-2E-V6-03, LAC-2F-V3-02 |
| **REJECT** | **0** | — |
| **DEFER** | **1** | LAC-2F-V3-03 (audit-scope assertion; re-entry trigger = any 2F-class re-audit citing a "balanced-scan gap"; folded as a one-line note into D-SKV18-L16, not dropped) |

**21/21 disposed, 0 silent drops.** The V1 transposition defect (Executive
Summary stated "11 ACCEPT / 9 MODIFY") was corrected to the row-verified
**9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER** (7 plain-ACCEPT + 2 bold-ACCEPT +
11 MODIFY + 1 DEFER = 21). The 21 LACs fold into **11 lock-addendum clauses**.

## Load-Bearing Accepted Lock Amendments (the 11 clauses; gate object = `3C-locks-v+1-diff.md`)

All amendments are an **addendum** appended after the SK-V17 tape-fold clause
(`LOCKS.md:622`) and before the `## v+1 Governance Boundary` (`:625`); they
preserve the 16 numbered locks and exactly five `BackendShape` variants and add
no directive, BIR variant, substrate, public substrate API, retained sidecar, or
sixth shape. The two disposed FIRST are the highest-leverage:

1. **D-SKV18-L14-named-primitive-gate** (LAC-1E-V5-01; Lock 14/16/8) — binds the
   §6 honest-finding escape as a machine-checked four-conjunct gate:
   (a) grammar-INVOKED-by-name, (b) output VARIES under invoking-rule mutation,
   (c) `verbatim_blob_present == false`, (d) PROFILE-PROVEN-NARROW-LEAF. Failing
   any one = REJECT. The single largest paper-close surface.
2. **D-SKV18-L05-L10-unfork** (LAC-1E-V5-02 + 2D-V3-01 + 2D-V3-02; Lock 1/5/10/14)
   — the relocated-seam firewall: `render(program)` reads shape ONLY from
   `program.policy_summary.backend_shape` (`emit_shape_source == lowered_program`),
   `RuntimeEmitterKind` DELETED; md5-distinctness is necessary-not-sufficient; the
   PLANNED `runtime_target_rows_collapsed` full-row `PartialEq` co-gate is mandatory.
3. **D-SKV18-L14-neutrality-proof** (1E-V5-03 + 2C-SK18-01/02) — neutral-named
   single-grammar primitive must be proven neutral by a non-that-grammar caller
   OR demoted (the `balanced_component_scan → css_balanced_component_scan` FORCED
   demotion); fleet-wide wording requires the full 9-grammar roster.
4. **D-SKV18-L08-aarch64-only** (1E-V5-04 + 2E-V6-01) — sharpens SK-V17
   aarch64-PRIMARY to aarch64-ONLY; the whole x86 surface is a P1 DELETION target.
5. **D-SKV18-L06-verbatim-blob** (1E-V5-05) — a `const CSS_GENERATED_RS: &str`
   `@generated` literal is hand-written, REJECT as "grammar-driven"; binding proof
   is round-trip byte-equivalence vs the deletable oracle.
6. **D-SKV18-L14-green-by-exclusion** (1E-V5-06) — P4 must move codegen surfaces
   into strict `GENERIC_SCAN_ROOTS` + extend `FORBIDDEN_GENERIC_TOKENS ⊇
   {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` (the certified
   SK-V18 SPEC form, byte-identical across 3A-D11/3B-P4/3C/3D/the v+1 diff),
   proven by re-inject-then-revert RED falsifier; lands BEFORE G2/G3.
7. **D-SKV18-L16-single-substrate-movemask** (2F-V3-01 + 2E-V6-03 + 2F-V3-03) —
   ONE scan substrate; `scan_balanced` vendored onto existing kernels + the one
   canonical SHRN movemask; PLANNED `bbnf_simd_single_mask_convention` co-gate.
8. **D-SKV18-L16-retarget-not-author** (2F-V3-02 + 2E-V6-02) — the admissible G6
   move RETARGETS an already-checkasm-gated kernel; speedup claims DEFER to H1.
9. **D-SKV18-L10-collapsed-slot** (2D-V3-04) — CollapsedStage is a SHAPE SLOT
   ONLY (diagnostic-only body) until a transient-mask proof + scalar oracle +
   checkasm + same-wave consumer land AND it clears RETIRED REDRESS 96/97/98.
10. **D-SKV18-L01-cursor-generality** (1A-LOCK1-AMEND-001 + 2D-V3-03) — strike
    "`G:EventGrammar` is the generality vehicle" (SK-V18 deletes `<G>`); re-anchor
    onto the `Cursor` micro-trait + config-breadth classifier; keep the e-graph
    ≥1-rewrite regression guard (`NormalizeDirectSinkCost`, live).
11. **D-SKV18-L13-pattern-h-recensus** (1E-V5-07 + 2C-SK18-03) — re-key Pattern H
    from absolute 67 to per-file provenance over the live census (71 at HEAD; +4
    is a tape-fold roster trace); structural full-row collapse over the 9-ident
    `strategy.rs` table is SK-V19-owned (≈+217 LOC).

## Disposition of Every Folded REVISE/REJECT (across the 5 cycles)

Zero REJECT was ever raised. Eight REVISE findings were folded to closure:

| finding | cycle | owner | disposition |
|---|---|---|---|
| `CH1-V1-001` stale out-of-range `2F:518` citation in v+1 diff context | V1 | 3C | FOLDED V2 — diff regenerated; `git apply --check` preserved. |
| `CH1-V1-002` absent SK-V15 `ORCHESTRATOR-PROMPT.md` reference | V1 | 3F | FOLDED V2 — routed to extant `sk-v18/SPEC.md` + `sk-v18/HANDOFF.md`. |
| `CH4-COST-01..05` W4/W7-W9 cap-realism, CSSOM bound, CRUD-4 cap | V1 | 3A/3B/3C/3E/3F | FOLDED V2 — 2D costed bands carried; W5 bounded to scoped typed CSS provider (no broad CSSOM); CRUD-4 → executable cap handling. |
| `CH5-V1-01/02` legacy `bbnf-regex` owner + runtime regex/DFA admission | V1 | 3A/3C | FOLDED V2 — canonical `parse-that-regex` owner; runtime regex/DFA requires prior Lock 1 G-Omega amendment (manifest necessary-not-sufficient). |
| `CH6-V1-01/02` Open-Questions missing receiver/blocker/gate + CRUD-4 aperture | V1 | 3A/3B/3C/3D/3E | FOLDED V2 — every Open-Questions row carries receiver/blocker/gate triad. |
| `CH4-V2-001` carried deltas lack row-level hard-cap-fit + fail-action | V2 | 3A/3B/3D/3E/3F | FOLDED V3 — CH4 coverage matrices add LOC, propagation, risk, wave, consumer/gate, cap-fit, fail-action per row. |
| `CH4-V2-002` 3C `D-L*` matrix lacks hard-cap-fit + fail-action columns | V2 | 3C | FOLDED V3 — per-clause cost matrix completed; no W12 / CSSOM / doc-only implementation gate. |
| `CH1-V4-001` `3A:56` out-of-range citation `V2/CH4.md:38-47` (file has 41 lines) | V4 | 3A | FOLDED V5 — re-keyed to in-range `V2/CH4.md:36`; citation-only repair, all 7 lenses ACCEPT. |

The V1 transposition correction (`9 ACCEPT / 11 MODIFY`) and the V3 token-set
convergence (`{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`
made byte-identical across all five carrier loci) are also folded and standing.

## G3-Gate Readiness

The **`3C-locks-v+1-diff.md`** hunk is the gate object the user authorizes at
**G-Omega**. Independently re-confirmed at HEAD: `git apply --check` exits 0
against live `restart/locks/LOCKS.md`; header `@@ -622,6 +622,33 @@` (6 context
+ 27 added); 16 numbered locks and the exact five-shape `BackendShape` canon
preserved; no new directive/BIR/substrate/public-API/retained-sidecar/sixth-shape;
both PLANNED co-gate symbols verified `rg = 0`. Under the active user pin, **G3
auto-passes** on cohort lock; **G-Omega is the next mandatory user gate** before
any LOCKS or V1-surface CRUD merges. Pass Omega V6 CHALLENGE must converge before
CRUD, and CRUD must complete current-state HANDOFF/MIGRATION cleanup (strike the
stale `HANDOFF.md:17-19` SK-V18-adopt definition, insert the §0.0 receiver, apply
the five migration decisions) before G-Omega.

**next-move = ready-for-G3 / Pass-Omega.**

## SK-V19 Tee-Up (totality-fold remainder, NOT a substitute for SK-V18)

SK-V19 adopts the SK-V18-proven un-forked generator into the 9-grammar
`crates/core/` fleet + onboards BBNF-self as the 4th-grammar litmus. Five named
receivers (each cited, none silently dropped): (a) the `ir/registry/strategy.rs`
9-ident `PRODUCTION_MANIFEST_TABLE` relocated-seam analog — R16 structural
row-collapse over ALL 9 rows (Lock 14 self-gate currently RED at 13 sites; ≈+217
LOC); (b) `css_types.rs` RELOCATE-to-`crates/css/`-or-DELETE; (c) the Pattern-H
67→71 provenance reconcile; (d) the `simd-scan` scanner asymmetry / `NibbleLut`
+ `WideLut` + 8/9 `OnceCell<StructuralIndex>` re-route (UNIFY vs renamed-parallel);
(e) the `LOCKS.md:620` generality-vehicle 1-line reconcile. SK-V19 entry condition
(1D U-1): census `crates/core/src/runtime` for line-1 `@generated` provenance +
md5-distinctness across the 9 grammars, mirroring the skinny P3 falsifier.

---

## 12-Line Summary

1. T-P3 SK-V18 totality synthesis CONVERGED at V5 (5 cycles, voids=0): V3 clean substantive cycle + V5 all-ACCEPT final confirmation after a V4 citation-only REVISE, under the V≤5 ceiling — NOT two untouched clean cycles.
2. Per-cycle r: V1=0.429 (REVISE), V2=0.857 (REVISE), V3=1.000 (clean), V4=0.857 (REVISE), V5=1.000 (lock); zero REJECT across all 35 lens runs.
3. Proposed-deltas census = 93 across six surfaces: 3A=26 (ARCH), 3B=14 (MASTER), 3C=11 clauses (LOCKS), 3D=12, 3E=18, 3F=12 (MIGRATION/HANDOFF/dispatch).
4. 3C disposition tally over 21 SK-V18 candidates: 9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER — 21/21 disposed, 0 silent drops, folded into 11 lock-addendum clauses.
5. Load-bearing accepted #1: D-SKV18-L14-named-primitive-gate — the (a)-(d) machine-checked four-conjunct gate on the §6 honest-finding escape (largest paper-close surface).
6. Load-bearing accepted #2: D-SKV18-L05-L10-unfork — the relocated-seam firewall (emit_shape_source==lowered_program) + RuntimeEmitterKind DELETE + PLANNED runtime_target_rows_collapsed co-gate.
7. Further accepted clauses: neutrality-proof (FORCED css_balanced demotion), aarch64-ONLY (x86 = P1 delete), verbatim-blob REJECT, green-by-exclusion P4 gate, single-substrate/one-movemask, retarget-not-author, CollapsedStage shape-slot, cursor-generality re-anchor, Pattern-H 67→71 recensus.
8. Eight REVISE findings folded to closure (CH1 citations ×2, CH4 cost ×7, CH5 regex-owner/runtime-DFA, CH6 open-questions triads, CH4-V2 row-fields ×2, CH1-V4 citation); zero REJECT ever raised; V1 9/11 transposition + token-set byte-identity standing.
9. Governance carried honestly: T-P1 near-converged NON-normal-§3Z (V7 lone clean, V8 broke streak), T-P2 near-converged NON-normal-§3Z, both NOT normal two-clean locks; 16 locks + five BackendShape variants preserved.
10. G3-gate readiness: the 3C-locks-v+1-diff.md hunk (@@ -622,6 +622,33 @@) applies cleanly to live LOCKS.md (git apply --check exit 0); both PLANNED co-gate symbols verified rg=0; it is the gate object the user authorizes at G-Omega.
11. next-move = ready-for-G3 / Pass-Omega-V6 — G3 auto-passes under the active user pin; G-Omega is the next mandatory user gate; Pass Omega V6 CHALLENGE→CRUD→G-Omega must precede any SK-V18 W-PRUNE dispatch.
12. SK-V19 tee-up: the totality-fold remainder adopts the un-fork into the 9-grammar crates/core/ fleet — receivers = 9-ident strategy.rs row-collapse (RED at 13 sites), css_types.rs relocate/delete, Pattern-H 67→71, simd-scan scanner-unify, LOCKS:620 reconcile, + BBNF-self 4th-grammar litmus.
