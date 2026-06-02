# Pass Omega V10 — CH5 HIDDEN-COUPLING — Cycle V2

Lens: CH5 HIDDEN-COUPLING. Does any Ω-C lock amendment imply a parallel substrate
/ renamed sidecar / Lock-1 violation / Track1≡Track2 dishonesty? Does the
single-substrate union hold across every staged delta?

Scope: the 11 staged lock-addendum clauses in `locks-diff.md` + the cross-artefact
CRUD operations (ΩC disposition, ΩD §13.6→SK-V19 re-key + §13.7 stand-up, ΩA
OA-V10-10, the SCANNER-UNIFY DEFER token, the firewall tense, the handoff/migration
deltas). Census = 14 amendment/CRUD items.

## V1→V2 carry: the three V1 REVISEs are RESOLVED in the staged diff

Cycle V1 raised three REVISEs (DEFECT-CH5-V1-01 firewall tense, -V1-02 dangling
`MP.SK19.SCANNER-UNIFY`, -V1-03 free-floating RED self-gate). All three are now
folded into the staged `locks-diff.md` and verified RESOLVED at HEAD:

- **V1-01 (firewall tense) RESOLVED.** The relocated-seam clause now reads "the
  un-fork is UNBUILT at HEAD (`RuntimeEmitterKind{CompiledLowering,RequestFacts}`
  still live, `runtime_generator.rs:17`-`25` still branches on
  `request.profile_contract.emitter`; 1E D-1E-V5-02)" and frames the
  `RuntimeEmitterKind`-DELETED + `*_count==0` end-state as "a PLANNED-at-G3 gate …
  NOT a present-tense fact." Verified live: `runtime_generator.rs:17`-`25` DOES
  still `match request.profile_contract.emitter`; `RuntimeEmitterKind` live at
  `grammar_provider.rs:33`,`:40`. The tense now matches reality.
- **V1-02 (dangling SCANNER-UNIFY) RESOLVED.** The single-substrate clause now
  cites the owning `MP-3B-SKV18-D07` (`3B:177`, verbatim-matched: "the
  renamed/parallel-scanner risk is ACTIVE … Decide UNIFY vs
  renamed-parallel-scanner; ≈+217 reconcile + 8/9 OnceCell re-route") AND the
  staged master-plan-diff §24 Diff-4 tee-up (verified: Diff 4 stages the
  `simd-scan` vs skinny `bbnf-simd` probe-API asymmetry as an SK-V19 receiver
  row). The DEFER token is no longer dangling.
- **V1-03 (RED self-gate) RESOLVED.** The Pattern-H clause now carries the CF-09
  in-clause disposition verbatim: "DISPOSITION (CF-09, in-clause): do NOT bolt a
  9-name regex widen as an SK-V18 patch — the R16 PartialEq full-row collapse +
  the roster-wide regex widen + `css_types.rs` relocation/deletion are a SK-V19
  reconcile (D11b, ≈+217, SK-V19-owned)." The RED admission is now bounded
  inside the governance surface by an in-clause SK-V18-vs-SK-V19 line.

## Spot-Verifications (load-bearing items)

- **`git apply --check` on staged `locks-diff.md`: exit 0** against live
  `restart/locks/LOCKS.md` at HEAD. Hunk `@@ -622,6 +622,33 @@`; insertion AFTER
  the SK-V17 Lock 16 clause (`LOCKS.md:622`) and BEFORE `## v+1 Governance
  Boundary` (`:625`, verified). SK-V15 (`:581`-`607`) / SK-V17 (`:610`-`622`)
  addenda neither restated nor edited. APPLIES CLEANLY.
- **16 numbered locks** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`
  (grep-verified); no Lock 17. **Five `BackendShape`** confirmed in code:
  `skinny/crates/codegen/src/lower/mod.rs:20`-`24` (the five `LOWERING` arms) +
  `ir/src/cost.rs:334` `all_backend_shapes() -> [BackendShape; 5]`. No sixth shape.
- **Both PLANNED co-gate symbols rg=0**: `runtime_target_rows_collapsed` and
  `bbnf_simd_single_mask_convention` return zero in `skinny/crates`+`skinny/xtask`
  — written PLANNED, never live. HONEST.
- **§H wave resolved**: live MASTER-PLAN §24 carries `MP.SK18.W0..W6` at `:1346`;
  master-plan-diff Diff 4 re-keys it to SK-V19 and ΩD stands up a NEW §13.7 SK-V18
  GENERALIZATION receiver. The `crates/core/` fold is correctly NOT the certified
  SK-V18; the tranche-identity pivot (SK-V18="tape-fold"→SK-V19; SK-V18=skinny
  GENERALIZATION) is the single load-bearing reconciliation and is CONSISTENT
  across ΩD `:49`-`:50` and the staged diff.
- **REDRESS reference resolved**: CollapsedStage clause cites `skinny/REDRESS.md:2795`-`2944`
  finding `:2928`-`2933` (M5 Max scalar-cheaper-than-SIMD-cursor, streamed-cursor
  REDRESS 96/97/98 RETIRED). The clause GATES promotion-past-diagnostic against
  that retired prior; it does NOT revive it. NOT a revived REDRESS route.
- **simd-scan second substrate (real, disclosed)**: `crates/simd-scan/src/lib.rs:67`
  carries `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut}`
  — a second `NibbleLut`/`WideLut` classifier convention, distinct from the `:68`
  probe API. Verified present. The single-movemask clause scopes its falsifier
  SKINNY and folds the totality second substrate into SK-V19 (D-SKV18-L01).

## The NEW V2 defect — the byte-identity-to-3C claim is FALSE

The V1 REVISE fixes above could not have been folded into a diff that was
byte-identical to `3C-locks-v+1-diff.md` — and indeed it is NOT. The staged
`locks-diff.md` diff body and the converged `3C-locks-v+1-diff.md` diff body have
DIFFERENT md5 hashes (`292d33b8…` vs `72438d6b…`) and DIFFER in 6 of the 11
clauses (firewall un-fork: +"UNBUILT at HEAD … PLANNED-at-G3"; neutrality:
+MP.NW6/`scoped non-JSON witness` re-scope; single-substrate:
+`MP-3B-SKV18-D07`/§24-tee-up/CH5-DEFECT-V1-03 scope note; CollapsedStage:
+UNKNOWN-2D-05 discharge paragraph; cursor-generality: +8-of-9-NOT-re-verified
re-census-owed + parenthetical decision-engine note; Pattern-H: +the CF-09
in-clause DISPOSITION). The clause COUNT matches (11=11), which is what a
shallow check would catch — but the bodies are not byte-identical.

Yet ΩC asserts byte-identity in TWO places: `:19`-`:20` ("the byte-identical
consolidation of the converged … `3C-locks-v+1-diff.md`") and `:123`-`:124`
("Staged … `locks-diff.md` body is byte-identical to the converged T-P3
`3C-locks-v+1-diff.md`"). ΩA `:13` calls it "the staged 3C locks diff" implying
the same identity. This is an UNCITED/FALSE provenance claim — precisely the kind
of dishonesty this lens hunts. The diff itself is CORRECT and strictly HARDER than
3C (it is the V1/V2/V3-hardened consolidation that resolves the three V1 REVISEs);
the defect is the prose mislabeling its provenance as byte-identical-to-3C rather
than hardened-FROM-3C.

Note the contrast with ΩA `:72` ("Verified byte-identical
`{EagerTape,…,CollapsedStage}`"): THAT byte-identity is about the 5-shape CANON,
which IS genuinely byte-identical and PASSES. The defect is localized to the
diff-body-vs-3C claim, not the shape canon.

## Coupling Resolution (the core lens question)

The single-substrate union does NOT silently hold across the totality tree, and
the amendment is HONEST about every place it breaks — the correct CH5 disposition:

1. **Second classifier substrate** (`simd-scan` `NibbleLut`/`WideLut`): real,
   disclosed, PRICED (≈+217 + 8/9 OnceCell re-route), DEFERRED to SK-V19 via
   `MP-3B-SKV18-D07` AND staged into master-plan-diff Diff 4. Skinny `rg=0` green
   NOT claimed as a totality single-substrate proof. NO coupling dishonesty.
2. **Relocated-seam firewall**: ONE `render(program)` reading shape only from
   `backend_shape`; `RuntimeEmitterKind`-DELETED is the post-G3 gate, NOT a live
   fact (verified still-live at HEAD). Skinny G2 scans skinny CSS surface;
   totality `crates/core/src/runtime/css_l4/` is the SK-V19 seam. No scope
   conflation, no renamed sidecar.
3. **Lock-14 RED disclosed (OA-V10-10)**: self-gate asserts ZERO, returns 13
   (9-row `idents` table live at `strategy.rs:137`-`185`, verified). Clause states
   FALSIFIED/RED and tees R16 collapse to SK-V19 (CF-09 in-clause). HONEST.
4. **Lock-1 intact; no sixth shape; no retained sidecar**: CollapsedStage is a
   SHAPE SLOT (diagnostic-only); single-substrate clause re-homes `scan_balanced`
   into the ONE skinny `bbnf-simd` consuming EXISTING kernels + the ONE canonical
   SHRN movemask (`movemask.rs:5`). Transient-single-call; no public substrate API.
5. **handoff/migration deltas**: prose (no apply-able fence); coupling-honest —
   `runtime_target_rows_collapsed == true` + `generator_grammar_count == 3` marked
   as PROVE-gates; md5-distinctness named insufficient; totality-tree analog routed
   to SK-V19. No hidden coupling.

## Disposition Census (14 items)

| # | Item | Lens verdict |
|---|---|---|
| 1 | Named-primitive (a)-(d) gate clause (LAC-1E-V5-01) | ACCEPT — machine-checked four-conjunct gate; no coupling. |
| 2 | Relocated-seam firewall + un-fork clause (LAC-1E-V5-02+2D) | ACCEPT — V1-01 RESOLVED; tense now "UNBUILT at HEAD … PLANNED-at-G3" matches live `runtime_generator.rs:17`-`25` / `grammar_provider.rs:40`. No renamed sidecar. |
| 3 | Neutrality-proof clause (LAC-1E-V5-03+2C) | ACCEPT — forced `balanced_component_scan`→`css_balanced_component_scan` demotion IS the discharge; `scoped non-JSON witness` per MP.NW6, not fleet-wide. |
| 4 | aarch64-ONLY clause (LAC-1E-V5-04+2E) | ACCEPT — x86 = P1 DELETION target; sharpens SK-V17 aarch64-PRIMARY; no x86 close path. |
| 5 | Verbatim-blob-courier clause (LAC-1E-V5-05) | ACCEPT — `CSS_GENERATED_RS:&str` courier REJECT-as-grammar-driven; round-trip byte-equivalence binding. |
| 6 | Green-by-exclusion precondition clause (LAC-1E-V5-06) | ACCEPT — STRENGTHENS Lock-14 (weak→strict `GENERIC_SCAN_ROOTS`); NOT a narrowing. |
| 7 | Single-SIMD-substrate + one-movemask clause (LAC-2F-V3-01) | ACCEPT — V1-02 RESOLVED; cites owning `MP-3B-SKV18-D07` (3B:177 verbatim) + staged §24 Diff-4 tee-up; DEFER no longer dangling. Skinny-scoped falsifier honest. |
| 8 | Retarget-not-author clause (LAC-2E-V6-02+2F) | ACCEPT — RETARGETS checkasm-gated `byte_class_from_eq_set_64` onto live recursive shell (`runtime_simd.rs:169`,`:180`-`204`); CALLER-DATA byte-set; no author-from-scratch loop. |
| 9 | CollapsedStage shape-slot clause (LAC-2D-V3-04) | ACCEPT — SHAPE SLOT, diagnostic-only; gates promotion against RETIRED REDRESS 96/97/98; UNKNOWN-2D-05 discharge does not re-open the x86-refused row. No revived route. |
| 10 | Cursor-generality re-anchor clause (1A-LOCK1-AMEND-001+2D) | **REVISE — DEFECT-CH5-V2-01.** The `<G>` strike + `Cursor`/config-breadth re-anchor is sound, and the clause HONESTLY de-settles the SK-V17 8-of-9 count ("NOT re-verified at the SK-V18 HEAD census post-P3-collapse … re-census owed at G4/G5 with the census command cited"). But the re-census is owed against the SAME generic-crate roster (`crates/ir/registry/strategy.rs`) the Pattern-H clause (item 11) declares FALSIFIED/RED at HEAD — the "alphabet-as-data across the generated grammars" generality witness rides on a roster whose grammar-name leak is an OPEN Lock-14 RED, and the clause does not cross-reference that the (b) config-breadth axis's witness-count owes its re-census to the SAME RED surface. CORRECTION (locks-diff cursor-generality clause): add a one-clause cross-reference that the (b) classifier config-breadth re-census at G4/G5 is gated on the Pattern-H 9-ident RED self-gate disposition (CF-09 / D11b), so the generality re-anchor is not silently witnessed on a known-leaking roster. |
| 11 | Pattern-H re-census clause (LAC-1E-V5-07+2C) | ACCEPT — V1-03 RESOLVED; CF-09 in-clause DISPOSITION now bounds the RED self-gate admission; 9-row table verified live at `strategy.rs:137`-`185`. |
| 12 | ΩC byte-identity-to-3C claim (`:19`-`:20`, `:123`-`:124`) + ΩA `:13` echo | **REJECT — DEFECT-CH5-V2-02 (uncited/false claim).** ΩC asserts the staged diff body is "byte-identical to the converged T-P3 `3C-locks-v+1-diff.md`"; it is NOT — md5 `292d33b8…` ≠ `72438d6b…`, 6 of 11 clauses differ (the V1/V2/V3 hardening folds, including the three V1 REVISE fixes). The diff is correct and HARDER than 3C, but the provenance label is false. CORRECTION: ΩC `:19`-`:20` and `:123`-`:124` must read "the V10-hardened consolidation OF the converged 3C diff (folding the V1-V3 CHALLENGE corrections); diff body differs from `3C-locks-v+1-diff.md` by the hardening deltas; `git apply --check` exits 0 and the 11-clause/9A-11M-0R-1D disposition is preserved" — and ΩA `:13` must drop "the 3C locks diff" identity phrasing. (This is the load-bearing provenance claim a downstream CRUD merge would trust; left as-is, CRUD-3 / CRUD-6 would assert a byte-identity that fails.) |
| 13 | ΩD §13.6→SK-V19 re-key + new §13.7 (MP CRUD) | ACCEPT — tranche-identity pivot correct; live §24 `MP.SK18.W0..W6` at `:1346` re-keyed; `crates/core/` fold NOT certified SK-V18; staged-only. |
| 14 | ΩC disposition matrix (9A/11M/0R/1D, 21 candidates) | ACCEPT — ZERO silent drops, one disposition per candidate; lone DEFER (LAC-2F-V3-03) is AUDIT-SCOPE with a real re-entry trigger, not a coupling. |

## Notes for Orchestrator (out-of-lens)

- Live surfaces (`MASTER-PLAN.md`, `LOCKS.md`, `MIGRATION.md`, `HANDOFF.md`,
  `ARCHITECTURE.md`) are all clean at HEAD — the staged-only invariant is honored.
- The CH5-DEFECT-V1-02 / -V1-03 SCOPE-NOTE markers inside the diff are PRIOR-cycle
  defect IDs carried as self-documenting scope notes (the p3 V2/V3 RESOLVED chain),
  legitimate annotations, not fabrications. Out-of-lens.

## Verdict

The substrate-union story is COUPLING-HONEST at the diff level: every place the
union does NOT hold on the totality tree (the `simd-scan` second
`NibbleLut`/`WideLut`, the unbuilt un-fork, the RED Lock-14 self-gate) is DISCLOSED
and DEFERRED to SK-V19, never claimed green and never bolted into an SK-V18 gate.
The three V1 REVISEs are all RESOLVED in the staged diff (firewall tense, the
SCANNER-UNIFY owner+tee-up, the in-clause CF-09). No revived REDRESS route, no
Lock-14 NARROWING (green-by-exclusion TIGHTENS it), no Lock-1 violation, no sixth
shape, no public substrate API, no retained sidecar; `git apply --check` exits 0.

But the hardening that resolved the V1 REVISEs is exactly what FALSIFIES the ΩC
prose: the staged diff is NO LONGER byte-identical to `3C-locks-v+1-diff.md`
(md5 differs; 6/11 clauses changed), yet ΩC `:19`/`:123` and ΩA `:13` still assert
byte-identity. That uncited/false provenance claim is the lone REJECT (item 12) — a
downstream CRUD merge that trusts it would assert a failing byte-identity. One
REVISE (item 10) flags the cursor-generality re-census silently riding the same
RED roster the Pattern-H clause declares falsified. 1 REVISE + 1 REJECT / 14 = 14%
— BELOW the >=30% cycle-V1 floor, which is correct: the V1 cycle already burned the
three load-bearing firewall/substrate/self-gate REVISEs and the staged diff folded
all three, so V2 finds only the second-order provenance/cross-reference residue,
not under-scrutiny.

TALLY accept=12 revise=1 reject=1
