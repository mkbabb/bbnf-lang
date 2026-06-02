---
agent: 3B
pass: T-P3-synthesis
cycle: V4-SKV18-totality
generated_at: 2026-06-01T20:30:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: MASTER-PLAN.md
proposed_deltas_count: 14
delta_summary:
  carried_from_prior_cycle: [MP-3B-V1-D01, MP-3B-V1-D02, MP-3B-V1-D09, MP-3B-V1-D10]
  removed: [MP-3B-V1-D03, MP-3B-V1-D04, MP-3B-V1-D05, MP-3B-V1-D06, MP-3B-V1-D07, MP-3B-V1-D08, MP-3B-V1-D11]
  answered: [CH4-COST-01, CH4-COST-02, CH4-COST-04, CH6-V1-01, CH4-V2-001]
  newly_added: [MP-3B-SKV18-D01, MP-3B-SKV18-D02, MP-3B-SKV18-D03, MP-3B-SKV18-D04, MP-3B-SKV18-D05, MP-3B-SKV18-D06, MP-3B-SKV18-D07, MP-3B-SKV18-D08, MP-3B-SKV18-D09, MP-3B-SKV18-D10]
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "The V3 cycle targeted SK-V15. MASTER has since absorbed §13.5 (SK-V15 PRUNE/REBUILD) and §13.6 (the OLD SK-V18 tape-fold) via Pass Omega CRUD (MP-3B-SKV17-D01..D08). The prior SK-V15-routing deltas D03-D08/D11 are CONSUMED by the now-landed §13.5/§13.6 text and are RETIRED here, not re-proposed against text that no longer needs them."
    - "MP-3B-V1-D01/D02 (current-state authority note; scoped-landing classification) survive as standing-discipline deltas and are carried."
    - "MP-3B-V1-D09 (primitive manifest discipline) and D10 (FNV quarantine) survive as standing receivers and are carried."
    - "V1-FOLD (CH2-V1-R03): MP-3B-SKV18-D10 + the §13.5-premise prose replace the bare 'measurement-valid' closure word with 'directionally-valid pending the H1 css_canon_bench re-lock' — the un-caveated word the row's own fail-action forbids is removed."
    - "V1-FOLD (CH4-V1 D04): the ad-hoc −10700 net figure replaced with the per-wave SPEC sum P1 −4500 + P2 −700 + P3 −5500 + P4 +15 + P5 0 = ≈−10685 (cited sk-v18/SPEC.md:433-437); P3 −5500 = 6×910 replica bodies + ~−40 collapsed rows + 1 PartialEq derive, matching the delta-row figure."
    - "V1-FOLD (CH3-V1-R2): the CH3 REGRESSION open question RETIMES the SK-V16/V17 REDRESS reconcile from an SK-V19-entry obligation to a Pass-Omega-V6 / pre-W-PRUNE blocker; G2/G4/G6 entry is BLOCKED until the SK-V16/V17 pre-block reconcile is on the committed ledger (cross-ref 3D-D08 / 3F-MH-003)."
---

## Executive Summary

`restart/MASTER-PLAN.md` is one cycle behind a SCOPE PIVOT. Its §13.6
"SK-V18 Tape-Fold Adoption" block, the §25 Implementation Order, and the
HANDOFF SK-V18 paragraph all define SK-V18 as the `crates/core/` tape-fold
(the five LOCKED fold designs, MP.SK18.W0-W6). The CERTIFIED SK-V18 is a
DIFFERENT tranche: the GENERALIZATION cycle — ONE grammar-driven generator
emitting JSON+CSS+Sheets from `.bbnf`, on the SKINNY tree, aarch64-only, net
≈−10800 LOC, a 12-wave PRUNE→G1..G6→PROVE→H1 manifest (T-P1 COH18-001;
`sk-v18/SPEC.md:19-21`). The `crates/core/` adoption MASTER labels "SK-V18"
is now SK-V19 (T-P1 COH18-014; 2C SK-V18→SK-V19 boundary). The reconciliation
re-keys the §13.6 tape-fold receivers to a SK-V19 block, adds a NEW §13.7
SK-V18 GENERALIZATION receiver block mapping the 12 waves with same-wave
consumers and exit-gate falsifiers, and tees up SK-V19 with the three carried
totality-tree leaks (the 9-ident `strategy.rs` table, `css_types.rs`, the
`simd-scan` scanner asymmetry). It revives no refuted route and synthesises
no new directive / BIR variant / sixth `BackendShape`: the tape stays a
substrate-manifest category, the 5-shape canon is unchanged. T-P3 PROPOSES
only; Pass Omega CRUD applies any accepted text post-G-Omega.

## V4 Delta Summary

| bucket | ids | note |
|---|---|---|
| carried | MP-3B-V1-D01, D02, D09, D10 | Standing-discipline deltas (current-state authority note; scoped-landing classification; primitive-manifest gate; FNV quarantine) survive the pivot and are re-grounded on the SK-V18 T-P1/T-P2 evidence. |
| removed | MP-3B-V1-D03, D04, D05, D06, D07, D08, D11 | The SK-V15-routing deltas are CONSUMED by the now-landed §13.5 SK-V15 block + §13.6 prose (Pass Omega applied them via MP-3B-SKV17-D01..D08). Re-proposing them against text that already carries them would be a no-op; retired. |
| answered | CH4-COST-01, CH4-COST-02, CH4-COST-04, CH6-V1-01, CH4-V2-001 | The prior-cycle hardening REVISE findings are preserved as answered; the new SK-V18 deltas carry the same per-row LOC/propagation/risk/wave/consumer/cap/fail fields (CH4 matrix below). |
| newly added | MP-3B-SKV18-D01..D10 | The SK-V18 scope-pivot reconciliation: re-key §13.6 tape-fold to SK-V19; add §13.7 SK-V18 GENERALIZATION 12-wave block; reconcile the P1-P5/G1-G6/PROVE/H1 waves; SK-V19 tee-up; the §13 H-row and §5 F.W5 alignment notes. |

## SK-V18 Scope-Pivot Reconciliation (the load-bearing finding)

The single largest reconciliation this cycle is NOT a wave landing or a
refutation: it is a tranche-IDENTITY pivot the V1 surfaces have not yet
absorbed. Three surfaces define "SK-V18" as the `crates/core/` tape-fold:

- `restart/MASTER-PLAN.md:974` §13.6 "SK-V18 Tape-Fold Adoption Receiver Block
  … ADOPTS the proven `Tape`/`ValueRef`/`select_classifier` into crates/core".
- `restart/MASTER-PLAN.md:1415-1422` §25 "SK-V18 W0 (§13.6 MP.SK18.W0) dispatch
  to ADOPT the proven substrate into crates/core".
- `restart/HANDOFF.md:17-19` "The next IMPLEMENTATION tranche is **SK-V18**: it
  adopts the SKINNY-proven … model into the totality `crates/core/` tree".

The CERTIFIED SK-V18 is the GENERALIZATION cycle on the SKINNY tree
(`sk-v18/SPEC.md:19-21`, `:58-61`: "each generator/gate verifiable by grepping
`skinny/crates/`, NOT `crates/core/` (the TOTALITY tree is the SK-V19 adoption
target)"). T-P1 COH18-001 calls this "the sharpest drift". The reconciliation:

1. The §13.6 tape-fold receivers (MP.SK18.W0-W6, the F1-F9 fold designs) are
   RE-KEYED to a SK-V19 block — they ARE the `crates/core/` adoption, which the
   certified plan defers to SK-V19 (T-P1 COH18-014; 2C `SK-V19 adopts to 9`).
2. A NEW §13.7 SK-V18 GENERALIZATION receiver block maps the 12 certified waves.
3. SK-V18 (generalization) is sequenced AFTER the §13.5 SK-V15 CSS-honesty repair
   and the SK-V17 skinny tape-proof, but BEFORE the SK-V19 `crates/core/` fold —
   the monotonic skinny→totality direction (`sk-v18/SPEC.md:60`; 1D D-1).

This pivot is also the SK-V18→SK-V19 THESIS: the skinny tree forks (two couriers
+ 7 replicas, T-P1 D-1/D-2/D-3) AND the totality tree carries the relocated-seam
analog (the 9-ident `strategy.rs` table, COH18-005). SK-V18 proves the un-fork on
3 grammars in skinny; SK-V19 adopts it to the 9-grammar `crates/core/` fleet.

## Wave Classification Ledger

### A-J Tranche Set (unchanged classification)

The A-J tranche set remains a pending V1 implementation skeleton; MASTER records
59 stub waves as planning census, not landed tranche work
(`restart/MASTER-PLAN.md:202`, `:218-219`). The SK-V18 generalization cycle does
NOT change the A-J classification; it routes its 12 waves to the same H/F/B/G/J
receivers via the new §13.7 block. The §5 F gate ("Rust lowerer emits equal
generated runtime for seed grammars", `:196`/`:519` F.W5 "Nine seed grammars
build through new template") is the F-tranche statement of the SK-V18 un-fork
claim — but T-P1 COH18-004 shows neither impl tree realises it yet (skinny: 2
forked arms + courier + 7 replicas; totality: 9 generated files fed by a
grammar-named lookup table). The F.W5 close gate is FED by SK-V18's un-forked
generator (skinny) and adopted at SK-V19 scale (totality).

| MASTER wave group | V4 status | evidence | allocation note |
|---|---|---|---|
| A.W0..A.W4 | pending | `restart/MASTER-PLAN.md:290-307` | Unchanged; no implementation tranche starts before SK-V15→SK-V18→SK-V19 close + Pass Omega/G-Omega. A.W2 "metadata accepts current nine grammars" (`:307`) is the SK-V19 fleet onboarding receiver (COH18-014). |
| B.W0..B.W4 | pending | `restart/MASTER-PLAN.md:192`, `:351-370` | Unchanged; B "Tape/direct works" gate is fed by the SK-V17-proven `Tape` and SK-V18 G4 shared-trait, adopted at SK-V19. Lock 1 one-substrate forbids the parallel substrate. |
| C.W0..C.W5 | pending | `restart/MASTER-PLAN.md:386-419` | Unchanged; the decision spine is LOAD-BEARING (T-P1 D-10), the selection DEPTH under the Sheets tower is the open L10 stressor tested only at G3 (1D D-10). |
| D.W0..D.W5 | pending | `restart/MASTER-PLAN.md:433-453` | Unchanged; regex/HIR facts stay an import boundary; the CSS scan uses the scalar shell + checkasm oracle (G5/G6, `sk-v18/SPEC.md:413`). |
| E.W0..E.W4 | pending | `restart/MASTER-PLAN.md:468-484` | Unchanged; no 6th `BackendShape`, no new BIR variant — the tape stays a substrate-manifest category (1D NONE LACs; LAC-2F-FOLD-02 carried). |
| F.W0..F.W5 | pending; F.W5 is the un-fork statement | `restart/MASTER-PLAN.md:196`, `:519` | F.W5 "Nine seed grammars build through new template" IS the un-fork claim T-P1 COH18-004 shows unrealised; SK-V18 proves it on 3 (skinny), SK-V19 scales to 9 (totality). |
| G.W0..G.W4 | pending | `restart/MASTER-PLAN.md:548-565` | Unchanged; future-grammar proof depends on the SK-V18 PROVE Sheets negative control + SK-V19 BBNF-self litmus (2C SK-V18-2C-9-GRAMMAR-FLEET-ONBOARDING-TEST). |
| I.W0..I.W4 | pending | `restart/MASTER-PLAN.md:884-900` | Unchanged; no SK-V18 evidence changes I ordering. |
| J.W0..J.W5 | pending | `restart/MASTER-PLAN.md:915-935` | Unchanged; J.W1/J.W5 consume the SK-V18 H1 honesty close + SK-V19 fleet close, not stale CSS admits. |

### §13.5 SK-V15 Block (predecessor; landed-as-receiver)

The §13.5 SK-V15 PRUNE-then-REBUILD block (`restart/MASTER-PLAN.md:912-973`) is
the CSS-honesty repair predecessor of SK-V18. It remains an active-pending
receiver block; SK-V18 generalization does not refute it. The CSS-honesty
foundation SK-V15 lays (CSS is diagnostic until typed value output) is the
PREMISE the SK-V18 >SOTA close inherits: T-P1 COH18-013 UPGRADES the SK-V15
"CSS contrived" verdict to "CSS >SOTA is directionally-valid pending the H1
`css_canon_bench` re-lock; the overfit is IMPLEMENTATION (forks/replicas), not
measurement" — so SK-V18 retires the forks,
not the measurement. The §13.5 block is preserved as-is; no delta touches it.

### §13.6 OLD SK-V18 Tape-Fold Block (RE-KEY to SK-V19)

| MASTER receiver (current §13.6 label) | V4 classification | reason | proposed re-key |
|---|---|---|---|
| §13.6 block header "SK-V18 Tape-Fold Adoption" | mis-labelled tranche | T-P1 COH18-001: certified SK-V18 is generalization on skinny; this block is the `crates/core/` adoption | Re-key the BLOCK to "§13.6 SK-V19 Totality-Fold Adoption Receiver Block"; the F1-F9 fold designs are unchanged, only the tranche label and sequencing move. |
| MP.SK18.W0..W6 (F7/F1/F3/F2/F6/F5+F8/F9) | tape-fold receivers, SK-V19 | These adopt `Tape`/`ValueRef`/`select_classifier` into `crates/core/` (`:980-981`) — the certified SK-V19 target | Re-key to MP.SK19.W0..W6; the per-row LOC/consumer/gate text is preserved verbatim (no fold-design content changes). |
| The F4 BackendShape-canon disposition (`:1023-1028`) | canon precedent, carried | LAC-2F-FOLD-02: the tape is a substrate-manifest category, not a 6th shape | Preserved unchanged; coheres with the SK-V18 generalization (the un-forked emitter reads `BackendShape`, not a grammar tag — `sk-v18/SPEC.md:71-78`). |

### NEW §13.7 SK-V18 GENERALIZATION Receiver Block (the 12 waves)

These are PROPOSED MASTER receiver rows, not implementation dispatch, imported
from the certified `sk-v18/SPEC.md` §2 wave manifest (`:431-447`). Every wave
carries a same-wave consumer and an exit-gate falsifier. No wave admits before
its predecessor closes its exit gate (the binding lattice, `sk-v18/SPEC.md:535-547`).
The campaign is net ≈−10800 LOC — a REDUCTION, no overflow risk
(`[generated-size-budget]`).

| SK-V18 receiver | T-P1/T-P2 source | manual LOC / risk | MASTER alignment | same-wave consumer / gate | exit-gate falsifier (turns RED) |
|---|---|---:|---|---|---|
| MP.SK18.P1 DELETE the x86 surface crate-wide | T-P1 D-4 (`1D:99-102`); COH18-009; addendum A6 | 0 add / ≈−4500 del / med (build-soundness coupling) | H.W5 x86 successor; aarch64-only | The `checkasm_parity.rs` decouple lands SAME commit; 12 aarch64 parity harnesses retained | `find …/x86_64 …/ext/x86 -type f == 0` (today 28); `cargo build`/`cargo test --no-run` clean (`x86_tree_deleted==true`) |
| MP.SK18.P2 DELETE warm micro-fixture CSS bench | T-P2 addendum 5; `sk-v18/SPEC.md:608-633` | ≈−700 / low | H.W6/BENCH timed-plane | The retained `css_canon_bench` cold harness consumes the extracted 9-field oracle | `grep -c 'measure_mbps\|lightningcss_facts' == 0` (today 48); `corpus_in_timer==true` |
| MP.SK18.P3 collapse 7 css_l4 replicas + RuntimeTarget row-collapse | T-P1 D-2 (`1D:87-93`); COH18-005 analog; addendum 2/R16 | ≈−5500 / high (relocated seam) | A.W2 nine-grammar census; G3 row-collapse co-gate | The G3 un-fork consumes the `RuntimeTarget: PartialEq` full-row derive (both nested structs) | `md5 …/{json,css_l4}/generated.rs` no identical pair; `runtime_target_rows_collapsed==true` |
| MP.SK18.P4 fix Lock-14 green-by-exclusion gate (BEFORE G2/G3) | T-P1 D-7 (`1D:108-111`); COH18-012; addendum A3 | ≈+15 / high | H.W4.LOCK14; §13.5 W2 gate restoration; MP.NW6 | The G1/G2/G3 emitter waves are neutrality-scanned AS authored (P4 is their entry-gate) | re-inject `SHEETS_GENERATED_RS`→RED; `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`; `lock14_gate_scans_codegen==true` |
| MP.SK18.P5 purge metalang bench-wave-id leak | T-P1 D-8 (`1D:112-114`); addendum A1/regen | ≈0 (rename-only) / low | clean-regen discipline | The 1:1 regen of `json/generated.rs` consumes the template-source rename | `grep -c parse_w11_1_number == 0` (today 7); `regen --check` exit 0 |
| MP.SK18.G1 JSON projection — `SinkOnlyExpr` AST-walk emitter | T-P2 R-C C1; T-P1 D-9 (91.5% leaf); `sk-v18/SPEC.md:438` | ≤450 hand / high (first-of-class) | H.W4 SinkOnly; JSON 51-row guard | The byte-equiv diff vs `json_templates/` oracle BEFORE oracle deletion (COH18-011) | `.bbnf`-mutation falsifier; `g1_hot_leaf_preserved==true`; `json_strict_rows_admitted==51` |
| MP.SK18.G2 CSS lowering — `css_balanced_component_scan` + fact-keyed projection | T-P2 R-B; T-P1 D-6 (94.1% scan); §6 (a)-(d) gate; `sk-v18/SPEC.md:439` | ≤450 hand / high | H.W6 CSS >SOTA; §13.5 W5 CSS typed provider | The arg-mutation falsifier + the 9-field cssparser oracle (gate BEFORE speed); `CSS_GENERATED_RS` DELETED | `css_typed_summary_equal==true` before speed; `track1_rich/lcss > 1.0×` same-run ∧ no pre-G2 regression |
| MP.SK18.G3 un-fork the emitter — DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | T-P1 D-3/COH18-003/COH18-008; T-P2 R-A; addendum 3; `sk-v18/SPEC.md:440` | ≤450 hand / high (relocated-seam §5-risk-1) | F.W5 un-fork; H.W4 5-shape; MP.NW6 Lock 14 | Generated output byte-equivalent to G1/G2-closed files (PATH change, not OUTPUT) | 5-conjunct exit: `emitter_fork_present==false`; `generator_grammar_branch_count==0`; `generator_grammar_type_count==0`; `emit_shape_source==lowered_program`; `runtime_target_rows_collapsed==true` |
| MP.SK18.G4 shared value-API trait + phantom resolution — `Cursor` micro-trait, DELETE `<G>` | T-P1 D-5/COH18-008; T-P2 R-D; addendum 4; `sk-v18/SPEC.md:441` | ≤450 hand / med-high | B substrate; G "path/value/visitor"; §13.5 W5 CSS provider | JSON `value.rs` navigation byte-equal diff vs pre-G4 (preserve-rich-ast, not "by construction") | `phantom_generic_resolved==deleted`; `json_rich_navigation_preserved==true`; `shared_trait_non_collapsible==true` |
| MP.SK18.G5/G6 neutral scan retarget — NEON onto the CSS scan shell + neutralize json/scan.rs | T-P1 D-6; T-P2 R-F; addendum 6; `sk-v18/SPEC.md:442` | ≤450 hand / med-high | H.W2/H.W2.5 Lock 16; H.W5 NEON | The (P3-collapsed singular) generated scan CALLS the shared `runtime_simd` primitive (no orphan kernel) | `acceleration_at_admission==admission` BOTH conjuncts: generated.rs caller census non-empty ∧ `simd_admission_profile_sampled==true` |
| MP.SK18.PROVE Sheets via the un-forked generator ONLY — precedence-tower core | T-P1 1D U-2; T-P2 R-E-2 / 2C SHEETS-PRECEDENCE-TOWER; `sk-v18/SPEC.md:443` | ≈+200 Sheets adoption / med-high (make-or-break) | G future-grammar gate; the negative control | The Sheets value type instantiates the G4 trait; Sheets `generated.rs` md5-distinct from JSON∧CSS | `sheets_grammar_shape==pratt-operator`; `generator_grammar_count==3`; no `const.*_RS.*r#` Sheets blob; BINDING FALLBACK `N` if shim-needed |
| MP.SK18.H1 CSS framing honesty + corpus-in-timer + regen --check clean | T-P1 1D U-4 (load-depressed re-lock); R14/R-A0-1; `sk-v18/SPEC.md:444` | 0 source / low | H.W6 honesty; J.W1 close | The H1 quiet re-capture + PASS-IMPL close audit consume the deferred G6 figure | `materialization_framing==lazy-rich-vs-eager-cssom`; `host_loadavg<1.0`; ≥1 regular corpus crossing >1.0× same-run |

Binding lattice (`sk-v18/SPEC.md:535-547`): P-cluster (P5 before G1; P4 live
before G2/G3; P3 dual-gates G2 + binds G3) → G1 → G2 (G1∧P3) → G3 (G1∧G2∧P4∧P3)
→ {G4 (G1∧G2∧G3); G5/G6 (P1∧P3∧G3, PARALLEL to G4)} → PROVE (G3∧G4, PARALLEL to
G5/G6) → H1 (G5/G6∧PROVE). Wave count = 12, at the skinny ceiling.

### SK-V19 Tee-Up (the totality-fold tranche)

SK-V19 is the totality-fold tranche: it ADOPTS the SK-V18-proven un-forked
generator into the 9-grammar `crates/core/` fleet and onboards BBNF-self as the
4th-grammar litmus (beyond the SK-V18 JSON+CSS+Sheets-3 witness). Its receivers
are the three carried totality-tree leaks T-P1/T-P2 verified at HEAD but routed
forward (their CLOSE is a SK-V19 wave, NOT SK-V18-closeable):

| SK-V19 receiver | T-P1/T-P2 source | reason | same-wave consumer / gate |
|---|---|---|---|
| MP.SK19.W0..W6 (the re-keyed §13.6 tape-fold F1-F9) | the §13.6 block + 1D D-11 (clean substrate KEEP) | The `crates/core/` adoption of the SK-V17-proven `Tape`/`ValueRef` | Per-grammar regen ×8/×9; SoA exactly-one-encoding closure (Lock 1) |
| MP.SK19.UNFORK 9-ident `strategy.rs` row-collapse | T-P1 COH18-005/COH18-012; 2C SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK | The totality relocated-seam analog: 9 grammar-named `idents` rows; Lock 14 self-gate is RED (13 sites) | R16 `PartialEq` row-collapse over ALL 9 idents + widen leak regex to 9 names (≈+217 reconcile) |
| MP.SK19.CSS-TYPES relocate-or-delete `css_types.rs` | T-P1 COH18-006/U-COH18-002; 2C SK-V18-2C-CSS-TYPES-HOST-SHIM-LEAK | The lock-NAMED mess in generic core; Lock 14 (c) admits only a `crates/<grammar>/` declaration crate | Relocate to `crates/css/` declaration crate (admissible) OR delete |
| MP.SK19.SCANNER-UNIFY `simd-scan` probe-API reconcile | T-P1 COH18-015 (`1F:104`) | The renamed/parallel-scanner risk is ACTIVE: skinny `bbnf-simd` and totality `simd-scan` are functionally parallel with divergent APIs | Decide UNIFY vs renamed-parallel-scanner; ≈+217 reconcile + 8/9 OnceCell re-route |
| MP.SK19.BBNF-SELF 4th-grammar litmus | T-P2 2C SK-V15-2C-BBNF-SELF-FUTURE-GRAMMAR (`2C:80`) | BBNF-self exercises recursive grammar-source ownership; onboard with no `Bbnf` branch in generic crates | regen/check diff + generic-code no-change gate; source/metadata-only addition |
| MP.SK19.FLEET-ONBOARD the remaining 6 grammars | T-P2 2C SK-V18-2C-9-GRAMMAR-FLEET-ONBOARDING-TEST (`2C:218`) | SK-V18 witnesses 3; SK-V19 scales the onboarding test to 9 + arbitrary user grammars | add source+metadata, `regen --check`, fail if any generic owner path changes except generated manifests |

SK-V19 entry condition (1D U-1 `verify_action`): at SK-V19 entry, census
`crates/core/src/runtime` for line-1 `@generated` provenance + md5-distinctness
across the 9 grammars, mirroring the skinny P3 falsifier — to learn whether the
totality 9 already un-fork or inherit the same forked-emitter problem at 9× scale
(U-COH18-001 open question).

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale, LOC/risk/wave alignment |
|---|---|---|---|
| MP-3B-SKV18-D01: Re-author the SK-V18 tranche identity — SK-V18 is the GENERALIZATION cycle on the skinny tree; the `crates/core/` tape-fold is SK-V19. | T-P1 COH18-001 (`1F:75`); `sk-v18/SPEC.md:19-21`,`:58-61`; 1D D-1. | §13.6 header, §25 Implementation Order, §13 H.W4 cross-refs. | 80-160 doc LOC, high routing risk, Pass Omega CRUD only. Aligns every "SK-V18" reference to the certified generalization plan before any later wording is read. |
| MP-3B-SKV18-D02: Re-key the §13.6 MP.SK18.W0..W6 tape-fold receivers to MP.SK19.W0..W6 (SK-V19 Totality-Fold Adoption block); preserve the F1-F9 fold-design content verbatim. | T-P1 COH18-001/COH18-014 (`1F:75`,`:88`); 2C SK-V18→SK-V19 boundary (`2C:299`,`:340`). | §13.6 block (`:974-1041`). | 60-120 doc LOC (relabel + sequencing), high routing risk. The fold-design content is correct; only the tranche label and the downstream sequencing move. |
| MP-3B-SKV18-D03: Add a NEW §13.7 SK-V18 GENERALIZATION receiver block mapping the 12 waves (P1-P5/G1-G6/PROVE/H1) with same-wave consumer + exit-gate falsifier columns. | `sk-v18/SPEC.md:431-447` (wave manifest); `:535-547` (lattice); T-P1 D-1..D-8. | New §13.7 after the re-keyed §13.6. | 280-460 doc LOC, high routing risk, new pending waves MP.SK18.P1..H1. Same-wave consumers + RED falsifiers required for every row (the §13.7 table above). |
| MP-3B-SKV18-D04: Route the P-cluster (P1-P5) to its MASTER receivers and record the build-soundness coupling + the P4-before-G2/G3 hard ordering. | T-P1 D-4/D-7/D-8; `sk-v18/SPEC.md:573-754`; COH18-009/COH18-012. | §13.7 P-rows; §13 H.W5 (x86), H.W4.LOCK14, §13.1 Lock 16. | P1 ≈−4500/med, P2 ≈−700/low, P3 ≈−5500/high, P4 ≈+15/high, P5 ≈0/low. The P1↔checkasm decouple is SAME-commit; P4 is the entry-gate of G2/G3, not a preference. |
| MP-3B-SKV18-D05: Route the G-cluster (G1-G6) to its MASTER receivers with the §6 (a)-(d) named-primitive gate and the 5-conjunct G3 un-fork exit. | T-P1 D-3/D-5/D-6/D-9; T-P2 R-A/R-B/R-C/R-D/R-F; `sk-v18/SPEC.md:438-442`,`:358-393`. | §13.7 G-rows; §13 H.W4 (SinkOnly/5-shape), H.W6 (CSS >SOTA), H.W2.5 (Lock 16). | G1-G6 each ≤450 hand LOC, high/med-high risk. The `css_balanced_component_scan` is FORCED-demoted (CSS-scoped) per the s6/C4 neutrality finding; the un-forked emitter reads `BackendShape`, not a grammar tag. |
| MP-3B-SKV18-D06: Route PROVE (Sheets negative control) + H1 (honesty close) with the BINDING FALLBACK `N` and the `materialization_framing` disclosure. | T-P1 1D U-2/U-4; T-P2 R-E-2 / 2C SHEETS-PRECEDENCE-TOWER (`2C:360`); `sk-v18/SPEC.md:443-444`,`:187-205`. | §13.7 PROVE/H1 rows; §13 H.W6, H.W7 (Pratt); J.W1 close. | PROVE ≈+200/med-high, H1 0 source/low. A Sheets emission needing a shim is the negative-control fail `N` (generalization NOT real), surfaced honestly, never paper-closed. H1 discloses `lazy-rich-vs-eager-cssom`. |
| MP-3B-SKV18-D07: Add the SK-V19 totality-fold tee-up block (re-keyed tape-fold + the three carried totality-tree leaks + BBNF-self litmus + fleet onboarding). | T-P1 COH18-005/006/015/U-COH18-001; T-P2 2C 9-GRAMMAR-FLEET / BBNF-SELF / TOTALITY-TREE-9-IDENT-LEAK / CSS-TYPES (`2C:218`,`:223`,`:224`,`:80`). | §13.6 (re-keyed) + a SK-V19 sub-section; §5 A.W2 nine-grammar census. | 220-380 doc LOC, high routing risk. The 9-ident `strategy.rs` leak (Lock 14 self-gate RED at 13 sites), `css_types.rs`, and the `simd-scan` asymmetry are verified at HEAD but their CLOSE is a SK-V19 wave — DEFER, do NOT bolt a patch into an SK-V18 gate. |
| MP-3B-SKV18-D08: Update §25 Implementation Order from "SK-V18 = crates/core adopt" to "SK-V15 (CSS honesty) → SK-V17 (skinny tape proof) → SK-V18 (skinny generalization) → SK-V19 (crates/core fold)". | T-P1 COH18-001; `sk-v18/SPEC.md:60`; MASTER `:1415-1422`. | §25 Implementation Order; §24 Carry Ledger. | 60-140 doc LOC, medium governance risk, no implementation wave. Restores the monotonic skinny→totality direction; no SK-V18 wave is engineered-deferred without its named close + G-Omega receiver. |
| MP-3B-SKV18-D09: Reconcile the F.W5 / §5 F gate as the un-fork statement — proven on 3 grammars at SK-V18 (skinny), scaled to 9 at SK-V19 (totality); neither tree realises it yet. | T-P1 COH18-004 (`1F:78`); 2C SK-V18-2C-9-GRAMMAR-FLEET (`2C:218`). | §5 tranche set F row (`:196`,`:519`), §5.3 YAML trajectory F slot. | 40-100 doc LOC, medium risk, no implementation wave. Prevents F.W5 reading as already-satisfied; the "nine seed grammars through new template" claim is the SK-V18→SK-V19 obligation. |
| MP-3B-SKV18-D10: Mark the carried §13.5 SK-V15 CSS verdict as UPGRADED — CSS >SOTA is directionally-valid pending the H1 `css_canon_bench` re-lock; the overfit is IMPLEMENTATION (forks/replicas), not measurement. | T-P1 COH18-013 (`1F:87`); 1D D-9 split; `sk-v18/SPEC.md:104-128`. | §13.5 SK-V15 block CSS clauses; §13 H.W6. | 40-80 doc LOC, medium risk. SK-V18 retires the forks, not the measurement; carry the directional caveat (loadavg 4.35, H1 re-lock) until the bench row is re-locked — do NOT carry the un-caveated "MEASUREMENT-VALID" closure word the row's own fail-action forbids (CH2-V1-R03). |
| MP-3B-V1-D01 (carried): SK-V18 current-state authority note before the tranche census. | T-P1 COH18-001 (re-grounded; was COH-001 SK-V15). | §5 tranche set, §25. | 60-120 doc LOC, high routing risk, Pass Omega CRUD only. Aligns later rows to the SK-V18 generalization plan. |
| MP-3B-V1-D02 (carried): Preserve A-J stubs as pending; label scoped landings scoped/partial/refuted, not V1/root close. | MASTER `:202`,`:218-219`; 1D D-9 (guard) / COH18-013 (CSS directional). | §5 tranche set, §13 H ledger. | 80-160 doc LOC, medium regression risk. Prevents scoped achievements becoming paper close (the JSON 51/51 guard is a guard, not generalization proof). |
| MP-3B-V1-D09 (carried): H.W2/H.W2.5 primitive vocabulary as a manifest plus later selected consumers; source-present-unwired is not an admit. | T-P2 2B (scalar oracle, strict differential, aarch64 gate, same-wave consumer); 1D D-6 (NEON dead at admission). | §13 H.W2/H.W2.5, §13.1 Lock 16 allowlist. | Manifest gate 120-280; future slices 80-350 each only with consumer. The eq-set kernel exists checkasm-gated but is NOT retargeted onto the shell — admission requires BOTH conjuncts (caller census ∧ profile-sampled). |
| MP-3B-V1-D10 (carried): FNV quarantine — bench-side clean/KEEP; production `emit_full_parse` FNV is live non-equality telemetry, not bench-quarantined. | T-P1 1D G-5 split (`1D:201`); the production path:line. | §13 H ledger, §23 Risk, J.W1/J.W5. | 80-220 manual + 100-240 fixture LOC, medium risk. No production FNV arbiter or correctness proof; the live telemetry distinction is recorded honestly. |

## CH4 Coverage Matrix

| delta | LOC | propagation count | risk | wave alignment | consumer / gate | hard-cap fit | fail action |
|---|---:|---:|---|---|---|---|---|
| SKV18-D01 | 80-160 doc | 3 | high routing | Pass Omega before any SK-V18 dispatch | Pass Omega CRUD + G-Omega acceptance | Doc-only authority repair; no implementation cap. | Reject CRUD or record REDRESS; block SK-V18 W0 until identity is current. |
| SKV18-D02 | 60-120 doc | 1 | high routing | §13.6 re-key (no fold-content change) | Pass Omega CRUD | Relabel + sequencing only; fold designs unchanged. | Reject if the relabel drops a fold-design row; preserve F1-F9 verbatim. |
| SKV18-D03 | 280-460 doc | 4 | high routing | MP.SK18.P1..H1 (the 12 waves) | Each §13.7 row's same-wave consumer + RED falsifier | Receiver-map doc; implementation cap per wave (P=30min, G=≤90min wall). | Missing consumer/falsifier blocks CRUD; non-fit wave routes intrinsic block/REDRESS, no W12 (12-wave ceiling). |
| SKV18-D04 | P1 −4500 + P2 −700 + P3 −5500 + P4 +15 + P5 0 = ≈−10685 net (per `sk-v18/SPEC.md:433`-`437`; P3 −5500 = 6×910 replica bodies + ~−40 collapsed rows + 1 PartialEq derive) | 5 | mixed (P3/P4 high) | P1-P5 | P1 checkasm-decouple, P2 cold-harness, P3 G3 row-collapse, P4 emitter-wave entry-gate, P5 1:1 regen | P-waves fit 30min each; P1 build-soundness is SAME-commit; P4-before-G2/G3 is a hard ordering. | A list narrower than the verify grep is RED-by-construction (the V5 reach hazard); REDRESS/revert. CH4-V1: the ad-hoc −10700 is dropped; the per-wave sum cites SPEC verbatim. |
| SKV18-D05 | G1-G6 ≤450 each | 6 | high/med-high | G1-G6 | byte-equiv diff (G1), arg-mutation+oracle (G2), 5-conjunct exit (G3), nav byte-equal (G4), caller-census∧profile (G5/G6) | Each fits ≤90min wave wall + 30/45min redress; arg-derivation under-delivery is a documented larger-cap REVISE. | Oracle-before-speed; a primitive failing any of (a)-(d) is REJECT; relocated-seam fires → REDRESS. |
| SKV18-D06 | PROVE ≈+200; H1 0 source | 3 | med-high (PROVE) / low (H1) | PROVE, H1 | Sheets value type instantiates G4 trait; H1 quiet re-capture + PASS-IMPL audit | PROVE fits 45min redress; H1 the one quiet re-capture is its defining measurement, not a rerun. | Sheets-needs-shim = `N` (generalization NOT real), honest, never stub-proved; undisclosed framing = H1 RED. |
| SKV18-D07 | 220-380 doc | 4 | high routing | MP.SK19.* receivers | R16 row-collapse (9 idents), css_types relocate/delete, scanner-unify decision, BBNF-self gate | Doc receiver-map; SK-V19 implementation cost is SK-V19's, not charged to SK-V18. | DEFER to SK-V19; do NOT bolt a 9-name regex widen into an SK-V18 gate as a patch. |
| SKV18-D08 | 60-140 doc | 2 | medium governance | SK-V15→V17→V18→V19 sequence | Pass Omega CRUD + G-Omega + W0 entry gate | Implementation-order governance text only. | Block SK-V18 W0 until order is accepted; unresolved remainder → REDRESS. |
| SKV18-D09 | 40-100 doc | 2 | medium | F.W5 (proven 3 / scaled 9) | F close gate; A.W2 nine-grammar census | Classification text only; no implementation. | Reject F.W5 reading as satisfied; keep it the SK-V18→SK-V19 obligation. |
| SKV18-D10 | 40-80 doc | 2 | medium | §13.5 CSS clauses, H.W6 | H1 `css_canon_bench` re-lock gate | Classification text only. | Keep directional caveat until the bench row is re-locked; no un-caveated MEASUREMENT-VALID. |
| V1-D01 | 60-120 doc | 3 | high routing | Pass Omega before W0 | Pass Omega CRUD + G-Omega | Doc-only authority repair. | Block SK-V18 W0 until authority is current. |
| V1-D02 | 80-160 doc | 2 | medium regression | No implementation wave; H/J close evidence | Pass Omega CRUD + J close-report reference | Classification text only. | Keep rows scoped/partial/refuted; block paper close. |
| V1-D09 | 120-280 + 80-350/slice | 3 | high | H.W2 then selected consumers | W2 manifest + scalar/checkasm/same-wave consumer | Manifest + selected slices only. | Block/demote unwired primitives; require row movement or REDRESS. |
| V1-D10 | 80-220 + 100-240 | 3 | medium | J.W1/J.W5 bench guard | production FNV scan + adversarial fixtures | Bench quarantine + scan only. | Block production FNV migration; record the live-telemetry distinction. |

## Consequences

**Positive.** The reconciliation lands the SK-V18 scope pivot the V1 surfaces
have not absorbed — every "SK-V18" reference resolves to the certified
generalization plan, and the `crates/core/` adoption gets its own SK-V19 home.
The §13.7 block gives MASTER a precise 12-wave receiver map with same-wave
consumers and RED falsifiers, so no generalization wave can paper-close. The
net ≈−10800 LOC campaign is recorded as a REDUCTION (no generated-size budget
overflow). The SK-V19 tee-up captures the three totality-tree leaks (the 9-ident
table, `css_types.rs`, the scanner asymmetry) as named SK-V19 receivers with
verify_actions, so the SK-V18→SK-V19 boundary is explicit, not lost.

**Cost.** High routing churn: D01/D02/D03/D07/D08 touch §13.6, §13.7, §25, §5,
§13 H-rows, and the HANDOFF cross-references (3F owns HANDOFF). The §13.6 re-key
must not drop a fold-design row (F1-F9 preserved verbatim). The §13.7 block is
280-460 doc LOC. All deltas are proposal-only; Pass Omega CRUD applies them
post-G-Omega — the cost lands in the CRUD pass, not here.

**Propagation.** The pivot propagates to 3A (ARCHITECTURE Sheets-by-construction
drift, COH18-002), 3C (no LACs from 1D this pass; the LAC-2F-FOLD-02 canon and
the 2C SK-V18 LACs are 3C's), 3D (the skinny→totality monotonic fold), 3E (the
9-grammar BackendShape matrix + Sheets/BBNF-self negative controls), and 3F
(the HANDOFF SK-V18 paragraph re-author + the SK-V19 next-cycle directive).
The SK-V19 receiver rows cross-reference 3E's grammar-generalisation story.

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 CORRECTNESS | Should Pass Omega re-key §13.6 in place (relabel SK-V18→SK-V19) and ADD §13.7, or restructure §13 into a single SK-V15→V18→V19 receiver supersection? | Pass Omega CRUD owner for `restart/MASTER-PLAN.md`. | The 3A/3F cross-surface wording and the §13 sub-section shape are not selected. | Pass Omega CRUD acceptance before G-Omega authorization. |
| CH2 GENERALITY | Does the totality 9-grammar `crates/core/` tree ALREADY un-fork, or fork on the `strategy.rs` grammar-named table (making SK-V19's un-fork a real obligation, not a relabel)? | SK-V19 entry census (U-COH18-001). | The `for_grammar_with_manifest` consumer path (generator vs `regen --check`) is not yet traced. | SK-V19 entry gate: census `crates/core/src/runtime` provenance + md5-distinctness across 9, mirroring P3. |
| CH3 REGRESSION | Does any §13.7 wave re-open a REDRESS-fenced route (eager value tree, StructRegistry per-leaf, fact-stream admission, x86, bracket_depth_mask)? | The §13.7 wave owners + the 1D Rejected-Route Pre-Block. | The SK-V16/V17 REDRESS coverage is not in the committed ledger (1D U-5); the four-item pre-block is complete only for the SK-V15-W11 ledger, so an SK-V16/V17-era streamed-cursor/second-scanner reject is structurally invisible. | CH3-V1-R2 RETIME: the SK-V16/V17 REDRESS reconcile is a **Pass-Omega-V6 / pre-W-PRUNE blocker** (per U-5's "before Pass Omega ratification"), NOT a SK-V19-entry obligation — because the SK-V18 G2 (`css_balanced_component_scan`), G4 (`Cursor`/`<G>` delete), and G6 (NEON retarget) waves abut items 51/53/247 and run DURING SK-V18. G2/G4/G6 entry is BLOCKED until the SK-V16/V17 pre-block reconcile is on the committed ledger. Cross-ref 3D-D08 / 3F-MH-003. |
| CH4 COST | If G2/G3/G4 or PROVE exceeds the SK-V18 ≤90min wave wall / ≤450 LOC cap, is the route intrinsic block, REDRESS, or a G-Omega wave-graph amendment (W12 is unavailable, the 12-wave ceiling is exact)? | SK-V18 wave governance + the affected wave owner. | Cap evidence from the G2 arg-derivation + the PROVE tower; the 12-wave ceiling is at the cap. | Plan/redress cap gate: row-level intrinsic block, REDRESS/revert, or G-Omega amendment before redress. |
| CH6 ANTI-PAPER-CLOSE | What exact PASS-IMPL close-audit fields should H1 require before routing SK-V19, given a Sheets `N` (negative-control fail) must surface honestly? | H1 / PASS-IMPL owner with 3F/HANDOFF alignment. | The Pass Omega/HANDOFF SK-V19 next-cycle wording from 3F is not yet accepted. | H1 close gate: accept each axis or record row-level intrinsic-block proof; a Sheets `N` blocks the generalization claim, never paper-closed. |
