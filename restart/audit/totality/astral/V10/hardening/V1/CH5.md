# Pass Omega V10 — CH5 HIDDEN-COUPLING — Cycle V1

Lens: CH5 HIDDEN-COUPLING. Does any Ω-C lock amendment imply a parallel substrate
/ renamed sidecar / Lock-1 violation / Track1≡Track2 dishonesty? Does the
single-substrate union hold across every staged delta?

Scope: the 11 staged lock-addendum clauses in `locks-diff.md` (= the byte-identical
consolidation of converged T-P3 `3C-locks-v+1-diff.md`) + the cross-artefact CRUD
operations (ΩC disposition, ΩD master-plan re-key, ΩA OA-V10-10, the DEFER-bundle
token, the firewall tense). Census = 13 amendment/CRUD items.

## Spot-Verifications (load-bearing items)

- **`git apply --check` on staged `locks-diff.md`**: exit 0 against live
  `restart/locks/LOCKS.md` at HEAD. Hunk header `@@ -622,6 +622,33 @@`; insertion
  AFTER the SK-V17 Lock 16 clause (`LOCKS.md:622`) and BEFORE `## v+1 Governance
  Boundary` (`:625`). SK-V15 (`:581`-`607`) / SK-V17 (`:610`-`622`) addenda neither
  restated nor edited. APPLIES CLEANLY.
- **16 numbered locks** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`;
  no Lock 17. **Five `BackendShape`** confirmed in code:
  `skinny/crates/codegen/src/lower/mod.rs:20`-`24` + `ir/src/cost.rs:334`
  (`-> [BackendShape; 5]`). No sixth shape.
- **Both PLANNED co-gate symbols rg=0**: `runtime_target_rows_collapsed` and
  `bbnf_simd_single_mask_convention` return zero in `skinny/crates`+`skinny/xtask`
  — written PLANNED, never live. HONEST.
- **§H wave resolved**: master-plan-diff §0/§1 re-keys live §13.6 from "SK-V18
  Tape-Fold" → "SK-V19 Totality-Fold" (`MASTER-PLAN.md:974`), and stands up a NEW
  §13.7 SK-V18 GENERALIZATION receiver. The `crates/core/` fold is correctly NOT
  the certified SK-V18; the tranche-identity pivot is the single load-bearing
  reconciliation. CONSISTENT.
- **REDRESS reference resolved**: CollapsedStage clause cites `skinny/REDRESS.md:2795`-`2944`
  finding `:2928`-`2933` — the M5 Max scalar-cheaper-than-SIMD-cursor streamed-cursor
  finding (REDRESS 96/97/98, `G-W3-UNION-SUBSTRATE` RETIRED). Promotion-past-diagnostic
  must clear that retired prior. The clause does NOT revive the retired route; it
  GATES against it. NOT a revived REDRESS route.

## Coupling Resolution (the core lens question)

The single-substrate union does **not** silently hold across the totality tree —
and the amendment is HONEST about this, which is the correct CH5 disposition:

1. **Second classifier substrate (real, disclosed, deferred).** `crates/simd-scan/src/lib.rs:67`
   exports a SECOND `NibbleLut`/`WideLut` classifier convention
   (`pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut}`),
   distinct from the `:68` `{StructuralIndex, next_structural_at_or_after}` probe
   API — verified verbatim. The skinny falsifier
   (`rg build_nibble_luts|find_first_of_nibble_lut skinny/crates/bbnf-simd == 0`)
   is SKINNY-scoped; the single-movemask clause explicitly scopes it SKINNY and
   folds the totality second substrate into the SK-V19 scanner-unification DEFER.
   The skinny `rg=0` green is NOT claimed as a totality single-substrate proof.
   This is the renamed/parallel-scanner risk the lens hunts — and it is ACTIVE,
   PRICED (≈+217 reconcile + 8/9 OnceCell re-route, `3B:177`), and DEFERRED, NOT
   bolted into an SK-V18 gate. NO coupling dishonesty.
2. **Relocated-seam firewall (no renamed sidecar).** The un-fork is ONE
   `render(program)` reading shape ONLY from `backend_shape`; `RuntimeEmitterKind`
   DELETED is the post-G3 admission gate (`emitter_fork_present` MUST be false at
   G3, SPEC `:247`), NOT a live fact — at HEAD `RuntimeEmitterKind{CompiledLowering,
   RequestFacts}` is still live (`grammar_provider.rs:40`) and `render` still
   branches on `request.profile_contract.emitter` (`runtime_generator.rs:17`-`25`).
   1E D-1E-V5-02 says so plainly: "the un-fork is unbuilt." The skinny G2 firewall
   scans the SKINNY CSS surface (`skinny/.../grammars/css_l4_*` 7 dirs +
   `runtime_simd.rs`, all exist); totality `crates/core/src/runtime/css_l4/` (7
   files, exists) is the SK-V19-adoption seam. Scope conflation is NOT present.
3. **Lock-14 falsified/RED is DISCLOSED, not papered (OA-V10-10).** The live
   Lock-14 self-gate asserts ZERO but the 9-row grammar-named `idents` table is
   live in generic `crates/ir/src/registry/strategy.rs:137`-`185` (verified:
   `JsonParser`/`JsonGrammar`, `GoogleSheetsParser`, `CssL4Parser`,
   `BbnfBootstrap`/`BbnfParser`); `css_types.rs` present (2373 B). The Pattern-H
   clause states "the Lock-14 self-gate is currently FALSIFIED/RED" and tees the
   9-ident R16 collapse to SK-V19 (CF-09), refusing a 9-name patch as an SK-V18
   bolt-on. HONEST coupling disclosure.
4. **Lock-1 not violated; no sixth shape; no retained sidecar.** CollapsedStage is
   a SHAPE SLOT only (`collapsed_stage.rs:16` delegates to shared
   `tape_plan::render_rule(...Collapsed)`, diagnostic-only); the single-substrate
   clause re-homes `scan_balanced` into the ONE skinny `bbnf-simd` consuming the
   EXISTING kernels + the ONE canonical SHRN movemask (`movemask.rs:5`, verified).
   No public substrate API, no retained classifier state (transient-single-call).

## Disposition Census (13 items)

| # | Item | Lens verdict |
|---|---|---|
| 1 | Named-primitive (a)-(d) gate clause (LAC-1E-V5-01) | ACCEPT — machine-checked four-conjunct gate, no coupling; evidence in-bounds. |
| 2 | Relocated-seam firewall + un-fork clause (LAC-1E-V5-02+2D) | **REVISE — DEFECT-CH5-V1-01.** "`RuntimeEmitterKind` and both variants are DELETED … `generator_grammar_branch_count == … == 0`" reads as a LIVE assertion but is the post-G3 admission END-STATE (1E D-1E-V5-02 "the un-fork is unbuilt"; SPEC `:247` `emitter_fork_present` MUST be false at G3). The sibling SK-V17 clauses and 1E both use "unbuilt"/"MUST be false at G3" framing; the `runtime_target_rows_collapsed` co-gate in the SAME sentence carries the PLANNED qualifier but the DELETED clause does not. CORRECTION: tense-mark `RuntimeEmitterKind` deletion + the two `*_count==0` as PLANNED-at-G3 gates (matching the co-gate's own qualifier), not present-tense facts. |
| 3 | Neutrality-proof clause (LAC-1E-V5-03+2C) | ACCEPT — forced `balanced_component_scan`→`css_balanced_component_scan` demotion IS the discharge; scope-honest 3-grammar (not fleet-wide) wording. |
| 4 | aarch64-ONLY clause (LAC-1E-V5-04+2E) | ACCEPT — x86 surface = P1 DELETION target; sharpens SK-V17 aarch64-PRIMARY to aarch64-ONLY; no x86 close path admitted. |
| 5 | Verbatim-blob-courier clause (LAC-1E-V5-05) | ACCEPT — `CSS_GENERATED_RS: &str` courier (`runtime_generator.rs:701`) REJECT-as-grammar-driven; round-trip byte-equivalence binding. |
| 6 | Green-by-exclusion precondition clause (LAC-1E-V5-06) | ACCEPT — STRENGTHENS Lock-14 (weak roots→strict `GENERIC_SCAN_ROOTS`, extends `FORBIDDEN_GENERIC_TOKENS`); NOT a Lock-14 narrowing. |
| 7 | Single-SIMD-substrate + one-movemask clause (LAC-2F-V3-01) | **REVISE — DEFECT-CH5-V1-02.** Posture coupling-clean and the totality second-substrate honestly disclosed+deferred, BUT the DEFER token is inconsistent: the clause names `MP.SK19.SCANNER-UNIFY` while the converged owner is `MP-3B-SKV18-D07` / `3B:177` ("renamed/parallel-scanner risk is ACTIVE … Decide UNIFY") — and `MP.SK19.SCANNER-UNIFY` appears in ZERO live MP surface and the staged master-plan-diff. CORRECTION: cite the owning `MP-3B-SKV18-D07` (3B:177) token, or stage the `MP.SK19.SCANNER-UNIFY` entry into master-plan-diff so the DEFER is not dangling. (Evidence anchors `aarch64/byte_class_from_eq_set_64.rs:79`-`87` [87 LOC, `vaddv_u8` pack at `:83`-`84`] and `simd-scan/src/lib.rs:67` are now CORRECT — V3 DEFECT-CH5-V3-01/02 RESOLVED.) |
| 8 | Retarget-not-author clause (LAC-2E-V6-02+2F) | ACCEPT — RETARGETS checkasm-gated `byte_class_from_eq_set_64` onto live recursive shell (`runtime_simd.rs:169`,`:180`-`204` verified); kernel carries byte-set as CALLER DATA (grammar-neutral); no author-from-scratch loop. |
| 9 | CollapsedStage shape-slot clause (LAC-2D-V3-04) | ACCEPT — SHAPE SLOT only, diagnostic-only body; correctly gates promotion against RETIRED REDRESS 96/97/98 (`REDRESS.md:2928`-`2933`). No revived route. |
| 10 | Cursor-generality re-anchor clause (1A-LOCK1-AMEND-001+2D) | ACCEPT — `<G>` axis strike re-anchors onto `Cursor` micro-trait + config-breadth classifier; e-graph regression-guard (≥1 asserted rewrite). Marked SK-V19 LOCKS reconcile. |
| 11 | Pattern-H re-census clause (LAC-1E-V5-07+2C) | **REVISE — DEFECT-CH5-V1-03.** The HONEST disclosure "the Lock-14 self-gate is currently FALSIFIED/RED (asserts ZERO, the literal 13-crate scan returns 13)" is correct (verified: 9-row `idents` table live at `strategy.rs:137`-`185`), but it embeds an active Lock-14 FALSIFICATION inside an ACCEPTed locks-addendum without a binding SK-V18-vs-SK-V19 disposition LINE in the clause itself — CF-09 (tee R16 collapse to SK-V19) lives only in ΩA, not in the staged lock text. CORRECTION: carry the CF-09 "do NOT bolt a 9-name regex widen as an SK-V18 patch; R16 collapse → SK-V19" disposition INTO the clause so the RED self-gate is not a free-floating admission inside a governance surface. |
| 12 | ΩD §13.6→SK-V19 re-key + new §13.7 (MP CRUD) | ACCEPT — tranche-identity pivot correct; `crates/core/` fold is NOT certified SK-V18; staged-only, no live edit. |
| 13 | ΩC disposition matrix (9A/11M/0R/1D, 21 candidates) | ACCEPT — ZERO silent drops, one disposition per candidate; lone DEFER (LAC-2F-V3-03) is AUDIT-SCOPE with a real re-entry trigger, not a coupling. |

## Notes for Orchestrator (out-of-lens)

- The CH5-DEFECT-V1-02 / -V1-03 SCOPE-NOTE markers embedded in the diff are
  PRIOR-cycle defect IDs carried as self-documenting scope notes (the V2/V3 p3
  hardening CH5 RESOLVED chain); they are legitimate scope annotations, NOT
  forward-references or fabrications. Out-of-lens.

## Verdict

The substrate-union story is COUPLING-HONEST: every place the union does NOT hold
on the totality tree (the `simd-scan` second `NibbleLut`/`WideLut`, the unbuilt
un-fork, the RED Lock-14 self-gate) is DISCLOSED and DEFERRED to SK-V19, never
claimed green and never bolted into an SK-V18 gate. No revived REDRESS route, no
non-applying diff, no Lock-14 NARROWING (the green-by-exclusion clause TIGHTENS
it), no Lock-1 violation, no sixth shape, no public substrate API, no retained
sidecar. The three REVISEs are precision/disposition defects (firewall tense, the
dangling `MP.SK19.SCANNER-UNIFY` token vs owning `MP-3B-SKV18-D07`, and the
free-floating RED self-gate without an in-clause SK-V19 tee), not coupling
dishonesty. 3/13 REVISE = 23% — BELOW the >=30% cycle-V1 floor; the two
load-bearing firewall/substrate clauses (items 2, 7) carry the REVISEs, so the
floor-miss reflects that the V3 p3-cycle already burned down the citation defects
this lens would otherwise have found, not under-scrutiny.

TALLY accept=10 revise=3 reject=0
