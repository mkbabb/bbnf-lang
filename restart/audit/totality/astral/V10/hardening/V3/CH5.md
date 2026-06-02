# Pass Omega V10 — CH5 HIDDEN-COUPLING — Cycle V3

Lens: CH5 HIDDEN-COUPLING. Does any Ω-C lock amendment imply a parallel substrate
/ renamed sidecar / Lock-1 violation / Track1≡Track2 dishonesty? Does the
single-substrate union hold across every staged delta?

Scope: the 11 staged lock-addendum clauses in `locks-diff.md` + the cross-artefact
CRUD operations (ΩC disposition, ΩD §13.6→SK-V19 re-key + §13.7 stand-up, ΩA
OA-V10-10 / CRUD-3 / CRUD-6, the SCANNER-UNIFY DEFER token, the firewall tense,
the handoff/migration/architecture deltas). Census = 14 amendment/CRUD items.

## V1→V2→V3 carry: all prior REVISEs/REJECT are RESOLVED in the staged tree

The V1 cycle burned three REVISEs (firewall tense, dangling `MP.SK19.SCANNER-UNIFY`
owner, free-floating RED self-gate); the V2 cycle burned one REJECT (the
byte-identity-to-3C provenance lie). All four are folded and re-verified RESOLVED
at HEAD:

- **V1-01 (firewall tense) RESOLVED.** The relocated-seam clause now reads "the
  un-fork is UNBUILT at HEAD (`RuntimeEmitterKind{CompiledLowering,RequestFacts}`
  still live, `runtime_generator.rs:17`-`25` still branches on
  `request.profile_contract.emitter`; 1E D-1E-V5-02) … a PLANNED-at-G3 gate …
  NOT a present-tense fact." Verified live: `runtime_generator.rs:17`-`25` DOES
  still `match` on `RuntimeEmitterKind::{CompiledLowering,RequestFacts}`;
  `grammar_provider.rs:33`,`:40`-`42` carries the live `enum RuntimeEmitterKind`
  with both variants; `:110` reads `request.profile_contract.emitter`. Tense
  matches reality.
- **V1-02 (SCANNER-UNIFY owner) RESOLVED.** The single-substrate clause cites the
  owning `MP-3B-SKV18-D07` (verified: `3B:197` is the D07 decision; `3B:177` is
  the `MP.SK19.SCANNER-UNIFY` row, verbatim "the renamed/parallel-scanner risk is
  ACTIVE … Decide UNIFY vs renamed-parallel-scanner; ≈+217 reconcile + 8/9
  OnceCell re-route") AND stages it into master-plan-diff Diff 4 §24 sub-item (c)
  ("the `simd-scan` vs skinny `bbnf-simd` probe-API asymmetry … decide UNIFY vs
  renamed-parallel-scanner + 8/9 OnceCell re-route"). DEFER no longer dangling.
- **V1-03 (RED self-gate) RESOLVED.** The Pattern-H clause carries the CF-09
  in-clause disposition verbatim: "DISPOSITION (CF-09, in-clause): do NOT bolt a
  9-name regex widen as an SK-V18 patch — the R16 PartialEq full-row collapse +
  the roster-wide regex widen + `css_types.rs` relocation/deletion are a SK-V19
  reconcile (D11b, ≈+217, SK-V19-owned)." RED admission is bounded in-clause.
- **V2-01 (cursor-generality riding RED roster) RESOLVED.** The cursor-generality
  clause now carries the cross-reference: "The (b) config-breadth re-census at
  G4/G5 is GATED on the Pattern-H 9-ident RED self-gate disposition … the (b)
  witness-count may NOT be settled on that known-leaking roster until the CF-09 /
  D11b SK-V19 reconcile lands." The generality re-anchor is no longer silently
  witnessed on a falsified roster.
- **V2-02 (byte-identity-to-3C REJECT) RESOLVED.** ΩC `:22`-`:25` now reads "The
  diff body is NOT byte-identical to `3C-locks-v+1-diff.md` (md5 differs; 6 of 11
  clauses changed); `git apply --check` exits 0 and the 11-clause / 9A-11M-0R-1D
  disposition is preserved." ΩA `:13` now reads "the V10-hardened consolidation …
  hardening deltas, not byte-identical." ΩA CRUD-3 (`:246`) reads "diff body
  differs from 3C by the V1–V3 hardening deltas." The false provenance label is
  gone; the honest hardened-FROM-3C framing replaced it everywhere.

## Spot-Verifications (load-bearing items, re-run at HEAD)

- **`git apply --check` on staged `locks-diff.md`: exit 0** against live
  `restart/locks/LOCKS.md` at HEAD. Hunk `@@ -622,6 +622,33 @@`; insertion AFTER
  the SK-V17 Lock 16 clause (`LOCKS.md:622`) and BEFORE `## v+1 Governance
  Boundary` (`:625`, verified by reading `:610`-`625`). SK-V15 (`:581`-`607`) /
  SK-V17 (`:610`-`622`) addenda neither restated nor edited. APPLIES CLEANLY.
- **16 numbered locks** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`
  (grep-verified, exactly 16); no Lock 17. **Five `BackendShape`** confirmed in
  code: `skinny/crates/codegen/src/lower/mod.rs:20`-`24` (the five `LOWERING`
  arms) + `ir/src/cost.rs:334` `all_backend_shapes() -> [BackendShape; 5]`. No
  sixth shape.
- **Both PLANNED co-gate symbols rg=0**: `runtime_target_rows_collapsed` and
  `bbnf_simd_single_mask_convention` both return zero in `skinny/crates`+
  `skinny/xtask` — written PLANNED, never live. HONEST.
- **§H wave resolved**: live MASTER-PLAN §24/§13.6 carries the SK-V18 tape-fold
  receiver; master-plan-diff Diff 4 re-keys it to SK-V19 (`MP.SK19.W0..W6`) and
  ΩD stands up a NEW §13.7 SK-V18 GENERALIZATION 12-wave block (ΩD `:14`-`:15`,
  `:24`-`:25`, `:49`-`:51`). The tranche-identity pivot (SK-V18 = GENERALIZATION
  → NEW §13.7; SK-V18-tape-fold → SK-V19, re-keyed §13.6) is CONSISTENT across ΩD,
  master-plan-diff, and 3B. The `crates/core/` fold is correctly NOT the certified
  SK-V18.
- **REDRESS reference resolved**: the CollapsedStage clause cites
  `skinny/REDRESS.md:2795`-`2944` finding `:2928`-`2933`. Verified: that span is
  the M5-Max scalar-cheaper-than-SIMD-cursor finding ("the scalar
  `consume_structural`/delimiter path … is cheaper than materializing or streaming
  a SIMD structural cursor … `G-W3-UNION-SUBSTRATE` is therefore retired, not
  merely blocked"). The clause GATES promotion-past-`diagnostic-only` against that
  RETIRED prior; it does NOT revive it. NOT a revived REDRESS route.
- **The SCANNER-UNIFY DEFER token `D-SKV18-L01` RESOLVES (preliminary mislabel
  FALSIFIED).** `D-SKV18-L01` is named in BOTH the firewall clause
  (`D-SKV18-L01 / MP.SK19.SCANNER-UNIFY`) and the single-substrate clause
  (`COH18-015 / D-SKV18-L01`). I initially read this as a wrong-decision
  cross-wire (the scanner-unification decision is `D-SKV18-L16-single-substrate-movemask`,
  not the cursor-generality `D-SKV18-L01-cursor-generality`). But the converged
  source `3C-locks-crystallisation.md:90` — the `D-SKV18-L16-single-substrate-movemask`
  row ITSELF — writes the bundle as "(COH18-015 / D-SKV18-L01 / MP.SK19.SCANNER-UNIFY)":
  3C deliberately single-prices the cursor-generality reconcile AND the
  scanner-unification into ONE shared ≈+217-LOC SK-V19 disposition (the
  "8/9 OnceCell re-route" is common to both). The staged locks-diff reproduces
  the 3C bundle byte-faithfully. NOT a mislabel; NOT a dangling token. ACCEPT.
- **`css_provider_source` field collision DISCLOSED.** The firewall clause's
  PLANNED predicate `css_provider_source == generated` is explicitly flagged
  "distinct from the live `bbnf-bench/src/report.rs` bench-report field of the
  same name; not yet a codegen/firewall gate symbol." Verified: `report.rs:1168`
  carries `pub css_provider_source: String` as a live bench-report field. The diff
  DISCLOSES the name collision rather than hiding it. Not a renamed sidecar.
- **Addendum convergence framing ACCURATE.** "T-P1 … V7 lone clean r=1.000, V8
  broke the streak; consec=0, converged=false" matches HARDENING-T-P1-CONSOLIDATED
  `:44`-`:52` (V7 r=1.000 consec→1; V8 4 anchor nits r=0.920 consec→0). "T-P2 …
  converged=false, consec=0; only single-cell citation-precision qualifiers from
  V4, no surviving REJECT" matches HARDENING-T-P2-CONSOLIDATED `:17`-`:25`.

## The fresh V3 defect — the single-movemask evidence mis-singularizes the live duplicate-pack count

The single-substrate clause forbids "a second … `vaddv_u8` shift-add pack
convention" and anchors its evidence on exactly ONE site:
`byte_class_from_eq_set_64.rs:79`-`87` ("the second `vaddv_u8` shift-add pack the
one-movemask rule forbids; the `scalar/` twin is 38 LOC and carries no such
pack"). A full `rg` of `unsafe fn movemask_u8x16` across `bbnf-simd/src/aarch64/`
at HEAD returns FIVE definitions, not two:

- `movemask.rs:4` — the canonical `pub` pack (`vshrn_n_u16::<4>` SHRN).
- `byte_class_from_eq_set_64.rs:79` — local copy, `vaddv_u8` shift-add (the named site).
- `bracket_depth_mask_64.rs:74` — local copy, **byte-identical** `vaddv_u8` shift-add body.
- `comment_body_mask_64.rs:68` — local copy, **byte-identical** `vaddv_u8` shift-add body.
- `match_tiny_plain_string.rs:100` — local copy, SHRN body (a non-delegating
  duplicate of the canonical convention).

So at HEAD skinny `bbnf-simd` already carries FOUR non-delegating local
`movemask_u8x16` definitions besides the canonical pack — THREE of them
byte-identical `vaddv_u8` shift-add packs (`byte_class_from_eq_set_64`,
`bracket_depth_mask_64`, `comment_body_mask_64`), plus a fourth SHRN duplicate.
The clause's own PLANNED co-gate `bbnf_simd_single_mask_convention` is described
correctly as "counting DISTINCT non-delegating pack implementations (alias-immune)"
— the right mechanism — but the EVIDENCE anchor frames `byte_class_from_eq_set_64`
as "**the** second" pack, when the single-movemask rule is ALREADY violated
≥three-fold inside skinny at HEAD, before any `scan_balanced` vendor. This is the
CH5 concern: the duplicate-mask substrate the co-gate must collapse is
under-counted in the very evidence the clause cites, so a downstream G2-entry
author gating against the one named site would miss the other three live
non-delegating copies. The coupling MECHANISM is honest (distinct-pack census);
the EVIDENCE framing understates the live in-skinny pack-duplication count.
REVISE.

## Coupling Resolution (the core lens question)

The single-substrate union does NOT silently hold across the totality tree, and
the amendment is HONEST about every place it breaks — the correct CH5 disposition:

1. **Second classifier substrate** (`simd-scan` `NibbleLut`/`WideLut`,
   `crates/simd-scan/src/lib.rs:67`, verified, distinct from the `:68` probe API):
   real, disclosed, PRICED (≈+217 + 8/9 OnceCell re-route), DEFERRED to SK-V19 via
   the single-priced `(COH18-015 / D-SKV18-L01 / MP.SK19.SCANNER-UNIFY)` bundle
   owned by `MP-3B-SKV18-D07` and staged into master-plan-diff Diff 4. Skinny
   `rg=0` NOT claimed as a totality single-substrate proof. 1F:44 confirms the two
   scanners are "FUNCTIONALLY PARALLEL with divergent APIs — the
   renamed-parallel-scanner risk is ACTIVE." NO coupling dishonesty.
2. **Relocated-seam firewall**: ONE `render(program)` reading shape only from
   `backend_shape`; `RuntimeEmitterKind`-DELETED is the post-G3 gate, verified
   still-live at HEAD (`grammar_provider.rs:40`-`42`). Skinny G2 scans skinny CSS
   surface; totality `crates/core/src/runtime/css_l4/` is the SK-V19 seam. No
   scope conflation, no renamed sidecar. md5-distinctness named
   NECESSARY-NOT-SUFFICIENT; the `runtime_target_rows_collapsed` PartialEq co-gate
   is mandatory.
3. **Lock-14 RED disclosed (OA-V10-10)**: self-gate asserts ZERO, returns 13
   (9-row grammar-named `idents` table live at `strategy.rs:137`-`185`, verified:
   Json/GoogleSheets/CssL4[only `CssL4Parser`]/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty).
   Clause states FALSIFIED/RED and tees R16 collapse to SK-V19 (CF-09 in-clause).
   HONEST.
4. **Lock-1 intact; no sixth shape; no retained sidecar**: CollapsedStage is a
   SHAPE SLOT (`collapsed_stage.rs:16` delegates to `tape_plan::render_rule(…
   Collapsed)`, diagnostic-only, verified); the single-substrate clause re-homes
   `scan_balanced` into the ONE skinny `bbnf-simd` consuming EXISTING kernels +
   the ONE canonical SHRN movemask (`movemask.rs:5`). No public substrate API,
   no cross-call retained classifier state.
5. **handoff/migration/architecture deltas**: prose (no apply-able fence);
   coupling-honest — `runtime_target_rows_collapsed == true` +
   `generator_grammar_count == 3` marked as PROVE-gates; migration-delta `:87`
   states "md5-distinctness ALONE does NOT prove the un-fork; the structural
   row-collapse co-gate is required"; the totality-tree analog (9-grammar
   `strategy.rs` table, COH18-005) routed to the SK-V19 fold; architecture-delta
   keeps CollapsedStage a SHAPE SLOT (demote-to-diagnostic), no sixth shape, no
   substrate API. No hidden coupling.

## Disposition Census (14 items)

| # | Item | Lens verdict |
|---|---|---|
| 1 | Named-primitive (a)-(d) gate clause (LAC-1E-V5-01) | ACCEPT — machine-checked four-conjunct gate; `named_primitive_falsifier_pass` / `*_abcd_pass` arms verified live at SPEC `:269`,`:284`,`:884`,`:1030`. No coupling. |
| 2 | Relocated-seam firewall + un-fork clause (LAC-1E-V5-02+2D) | ACCEPT — V1-01 RESOLVED; tense "UNBUILT at HEAD … PLANNED-at-G3" matches live `runtime_generator.rs:17`-`25` / `grammar_provider.rs:40`-`42`. `css_provider_source` bench-field collision DISCLOSED. No renamed sidecar. |
| 3 | Neutrality-proof clause (LAC-1E-V5-03+2C) | ACCEPT — forced `balanced_component_scan`→`css_balanced_component_scan` demotion IS the discharge; `scoped non-JSON witness` per live MP.NW6 (`MASTER-PLAN.md:662`) / H.W4.LOCK14 (`:605`), not fleet-wide; third witness inherits the PROVE falsifier. |
| 4 | aarch64-ONLY clause (LAC-1E-V5-04+2E) | ACCEPT — x86 = P1 DELETION target; sharpens SK-V17 aarch64-PRIMARY (`LOCKS.md:622`) to aarch64-ONLY; FEAT_SVE2 ABSENT honored; no x86 close path. |
| 5 | Verbatim-blob-courier clause (LAC-1E-V5-05) | ACCEPT — `CSS_GENERATED_RS:&str` courier (`runtime_generator.rs:701`) REJECT-as-grammar-driven; round-trip byte-equivalence binding. |
| 6 | Green-by-exclusion precondition clause (LAC-1E-V5-06) | ACCEPT — STRENGTHENS Lock-14: moves leak surfaces from weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` (`lock14_baseline.rs:2442`) into strict `GENERIC_SCAN_ROOTS` (`:2409`), extends `FORBIDDEN_GENERIC_TOKENS` (`:2420`). NOT a narrowing. |
| 7 | Single-SIMD-substrate + one-movemask clause (LAC-2F-V3-01) | **REVISE — DEFECT-CH5-V3-01.** Coupling MECHANISM honest (distinct-pack census co-gate; `simd-scan` second substrate disclosed+deferred; `D-SKV18-L01` bundle resolves to 3C:90). But the EVIDENCE anchor frames `byte_class_from_eq_set_64.rs:79`-`87` as "**the** second `vaddv_u8` shift-add pack," while at HEAD skinny `bbnf-simd/src/aarch64/` carries FOUR non-delegating local `movemask_u8x16` definitions besides the canonical pack — THREE byte-identical `vaddv_u8` shift-add packs (`byte_class_from_eq_set_64.rs:79`, `bracket_depth_mask_64.rs:74`, `comment_body_mask_64.rs:68`) plus a fourth SHRN duplicate (`match_tiny_plain_string.rs:100`). The single-movemask rule is ALREADY violated ≥three-fold in skinny before any vendor. CORRECTION (locks-diff single-substrate clause): the evidence parenthetical must read "the `vaddv_u8` shift-add pack is replicated NON-delegating across `byte_class_from_eq_set_64.rs:79`-`87`, `bracket_depth_mask_64.rs:74`, and `comment_body_mask_64.rs:68` at HEAD (plus a fourth SHRN duplicate at `match_tiny_plain_string.rs:100`); the `bbnf_simd_single_mask_convention` co-gate's distinct-pack count is ≥4 at G2 entry, not a single 'second' pack" — so a G2-entry author gates against the live multi-pack census, not one named site. |
| 8 | Retarget-not-author clause (LAC-2E-V6-02+2F) | ACCEPT — RETARGETS checkasm-gated `byte_class_from_eq_set_64` onto the live recursive shell `find_css_significant` (`runtime_simd.rs:169`,`:180`-`204` verified; ≤13-byte set split into two ≤8 eq-set fans as CALLER DATA); no author-from-scratch loop. |
| 9 | CollapsedStage shape-slot clause (LAC-2D-V3-04) | ACCEPT — SHAPE SLOT, diagnostic-only (`collapsed_stage.rs:16` delegates); gates promotion against RETIRED REDRESS 96/97/98 (`REDRESS.md:2928`-`2933` verified G-W3-UNION-SUBSTRATE retired); UNKNOWN-2D-05 discharge does not re-open the x86-refused row. No revived route, no sixth shape. |
| 10 | Cursor-generality re-anchor clause (1A-LOCK1-AMEND-001+2D) | ACCEPT — V2-01 RESOLVED; (b) config-breadth re-census now GATED on the Pattern-H 9-ident RED disposition (CF-09 / D11b); e-graph regression-guard grounded (`NormalizeDirectSinkCost` live `backend_egraph.rs:191`-`193`, instantiated `:75`). `<G>`-strike sound. |
| 11 | Pattern-H re-census clause (LAC-1E-V5-07+2C) | ACCEPT — V1-03 RESOLVED; CF-09 in-clause DISPOSITION bounds the RED self-gate; 9-row table verified live at `strategy.rs:137`-`185`; `css_types.rs` = 66 LOC verified. |
| 12 | ΩC provenance framing (`:19`-`:25`, `:123`-`:133`) + ΩA `:13` / CRUD-3 `:246` | ACCEPT — V2-02 REJECT RESOLVED; ΩC now reads "NOT byte-identical (md5 differs; 6 of 11 clauses changed); git apply --check exits 0; 11-clause / 9A-11M-0R-1D preserved"; ΩA `:13` / CRUD-3 read "the V10-hardened consolidation … hardening deltas, not byte-identical." Honest hardened-FROM-3C framing. |
| 13 | ΩD §13.6→SK-V19 re-key + new §13.7 (MP CRUD) | ACCEPT — tranche-identity pivot correct and CONSISTENT across ΩD `:14`-`:15`,`:49`-`:51` + master-plan-diff Diff 4; `crates/core/` fold NOT certified SK-V18; staged-only. |
| 14 | ΩC disposition matrix (9A/11M/0R/1D, 21 candidates) | ACCEPT — ZERO silent drops, one disposition per candidate; lone DEFER (LAC-2F-V3-03) is AUDIT-SCOPE with a real `ls`-both-trees re-entry trigger, not a coupling. |

## Notes for Orchestrator (out-of-lens)

- Live surfaces (`MASTER-PLAN.md`, `LOCKS.md`, `MIGRATION.md`, `HANDOFF.md`,
  `ARCHITECTURE.md`) are clean at HEAD — the staged-only invariant is honored.
- The `D-SKV18-L01` token is the cursor-generality DEFER decision that 3C:90
  single-prices INTO the scanner-unification bundle; it resolves and is NOT a
  mislabel (preliminary read corrected against the converged 3C source).
- The CH5-DEFECT-V1-02 / -V1-03 SCOPE-NOTE markers inside the diff are PRIOR-cycle
  defect IDs carried as self-documenting scope notes; legitimate annotations.
  Out-of-lens.

## Verdict

The substrate-union story is COUPLING-HONEST across every staged delta: every
place the union does NOT hold on the totality tree (the `simd-scan` second
`NibbleLut`/`WideLut`, the unbuilt un-fork, the RED Lock-14 self-gate) is
DISCLOSED and DEFERRED to SK-V19 — priced, owned by `MP-3B-SKV18-D07`, staged
into master-plan-diff Diff 4 — never claimed green and never bolted into an
SK-V18 gate. `git apply --check` exits 0; 16 locks; five `BackendShape`; both
co-gate symbols rg=0. No revived REDRESS route (the CollapsedStage clause GATES
against the RETIRED streamed-cursor finding), no non-applying diff, no Lock-14
NARROWING (green-by-exclusion TIGHTENS it), no Lock-1 violation, no sixth shape,
no public substrate API, no retained sidecar, no Track1≡Track2 dishonesty
(md5-distinctness named insufficient, the PartialEq full-row collapse mandatory).
The four prior-cycle defects (V1 firewall tense / SCANNER-UNIFY owner /
free-floating RED, V2 byte-identity lie, V2 cursor-on-RED-roster) are all folded
and re-verified RESOLVED.

The lone fresh V3 defect (item 7) is second-order: the single-movemask clause's
co-gate MECHANISM is sound, but its evidence anchor mis-singularizes a
duplicate-`vaddv_u8`-pack substrate that already exists THREE-fold inside skinny
`bbnf-simd` at HEAD (`byte_class_from_eq_set_64.rs:79`, `bracket_depth_mask_64.rs:74`,
`comment_body_mask_64.rs:68`) — understating the live in-skinny pack-duplication
the `bbnf_simd_single_mask_convention` census must collapse. 1 REVISE / 14 = 7%
— BELOW the >=30% cycle-V1 floor, which is CORRECT and EXPECTED: V1 burned the
three load-bearing firewall/substrate/self-gate REVISEs and V2 burned the
byte-identity REJECT + the cursor-on-RED-roster REVISE; all five were folded into
the staged tree, so V3 finds only the second-order movemask-census residue, not
under-scrutiny.

TALLY accept=13 revise=1 reject=0
