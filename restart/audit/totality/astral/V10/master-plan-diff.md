# STAGED master-plan-diff — Pass Omega V10 SK-V18 Generalization Reconciliation (Ω-D)

Pass: SK-V18 PASS OMEGA (astral synthesis, cycle V10), SCOPE Ω-D.
Status: **STAGED ONLY — NOT APPLIED.** The actual CRUD merge into
`restart/MASTER-PLAN.md` executes POST-G-Omega, after user sign-off.
Consumes: T-P3 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md`,
deltas `MP-3B-SKV18-D01..D10` + carried `MP-3B-V1-D01/D02/D09/D10`); T-P3 3F
(`3F-migration-handoff.md`, `3F-MH-005`/`3F-MH-007`/`3F-MH-013` HANDOFF/SK-V19
cross-refs); T-P3 3D/3E (skinny→totality fold; 9-grammar matrix). Master HEAD at
staging: `25297a7fc`.

CRITICAL BOUNDARY: this file PRODUCES STAGED DIFFS ONLY. No live governance
surface (MASTER-PLAN.md, ARCHITECTURE.md, LOCKS.md, HANDOFF.md, MIGRATION.md) is
edited by this pass. Pass Omega CRUD applies the accepted diff post-G-Omega.

The 16-lock count + 5 `BackendShape` variants are PRESERVED — amendment by
addition, no renumber. This diff revives no refuted route and synthesises no new
directive / BIR variant / 6th `BackendShape`: the tape stays a substrate-manifest
category; the 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage}` is unchanged.

---

## 0. Landed-Work Reconciliation (SK-V14..V18 commit SHAs)

Before staging text, the §H tranche set is audited against landed work. The git
log establishes which waves LANDED, which are REFUTED, which remain PENDING, and
which are NEW. (SHAs are at staging HEAD `25297a7fc`.)

| Tranche | Wave span | Status | Commit evidence | Reconciliation |
|---|---|---|---|---|
| SK-V14 | Alpha + W0..W11 census | LANDED as AUDIT-ZERO baseline; 59 stub waves PENDING | `496a81417`..`13f82dcdc` (alpha); SYNTHESIS audit-zero | §5 already records this (`:215`-`:234`); no §H tranche A-J wave landed. No change. |
| SK-V15 | W0..W11 PRUNE-then-REBUILD | **LANDED** (CSS-honesty repair) | W0 `a82196b9e` → W11 close `66232b7c3`; W7 decision-spine `9a0079cfb`; W8/W9 lowerers `a913a1ffa`/`a21573cdf`; W10 FNV `b8b61b047` | §13.5 SK-V15 block marked "active pending" throughout — must flip to **LANDED-as-receiver**; the §13.5 CSS verdict carries the `MP-3B-SKV18-D10` directional UPGRADE. |
| SK-V16 | W0..W6-tape | **LANDED** (CSS >SOTA rearchitecture + shared flat-tape substrate) | W0 `c64148ef2` → W6-tape `1c5bd7a25`; `CSS_GENERATED_RS` quarantine `232479e4d`; structural-equality `ea8138056` | Not a §H receiver block today; the SK-V16 tape substrate is the empirical antecedent SK-V17 proved. Recorded in the §13.7 P-cluster antecedent note (no new §H block needed — SK-V16 folded into SK-V17 lineage). |
| SK-V17 | W0..W5 skinny tape-fold | **LANDED** (unified-tape / lazy-`ValueRef` / NEON classifier proven in skinny) | W0 `fb8848cf2`, W1 `3a37c29d8`/`c2a48fcbb`, W2 `6dad81fb9`, W3 NEON `6bb4b2a6c`, W4/W5 close `f6a38445b` | §13.6 currently labels the `crates/core/` fold "SK-V18" and says it "active after SK-V17 skinny W0-W5 + G-Omega". SK-V17 skinny **closed** (`f6a38445b`). The §13.6 block must RE-KEY to SK-V19 (its true target) per `MP-3B-SKV18-D02`. |
| SK-V18 | W-PRUNE/G1..G6/PROVE/H1 (12 waves) | **PENDING — certified, dispatchable W-PRUNE-only** | Alpha `83b66db42`; S-P0 `0fbee121f`; S-P1 `9b52e162d`/`784ceb418`; S-P2 `820798161`; S-P3 SPEC `4e4aa0648`; T-P1 `6fb812752`; T-P2 `3f6eb603d`; T-P3 `25297a7fc` | The CERTIFIED SK-V18 is the GENERALIZATION cycle on the skinny tree — NOT the `crates/core/` fold §13.6 names. A NEW §13.7 SK-V18 GENERALIZATION receiver block maps its 12 waves. |

**Refuted census this cycle: 0.** No §H A-J wave is refuted by SK-V18 evidence;
the A-J classification is unchanged (`MP-3B-V1-D02` carried). The single
load-bearing reconciliation is the SK-V18 tranche-IDENTITY pivot, not a wave
refutation.

**The pivot, one line:** MASTER's §13.6 / §25 / HANDOFF define "SK-V18" as the
`crates/core/` tape-fold (MP.SK18.W0-W6, the F1-F9 fold designs). The certified
SK-V18 is the GENERALIZATION cycle (skinny tree, ONE `.bbnf`-driven generator,
JSON+CSS+Sheets, aarch64-only, ≈ −10800 campaign LOC (per-wave SPEC sum ≈−10685), 12-wave PRUNE→G1..G6→PROVE→H1).
The `crates/core/` adoption MASTER labels "SK-V18" is now SK-V19 (T-P1
COH18-001 the HANDOFF scope-drift + COH18-014 the literal SK-V18→SK-V19 boundary;
2C SK-V18→SK-V19 boundary; `sk-v18/SPEC.md:19-21`,`:58-61`).

---

## 1. Staged Diff 1 — Re-key §13.6 SK-V18 Tape-Fold → §13.6 SK-V19 Totality-Fold

Applies `MP-3B-SKV18-D01` + `MP-3B-SKV18-D02`. The F1-F9 fold-design content is
preserved VERBATIM; only the tranche label, the header, the wave IDs
(MP.SK18.W*→MP.SK19.W*), and the downstream sequencing move.

**Header re-key** (`restart/MASTER-PLAN.md:974`):

```diff
-### §13.6 SK-V18 Tape-Fold Adoption Receiver Block (downstream of SK-V15; active after SK-V17 skinny W0-W5 + G-Omega)
+### §13.6 SK-V19 Totality-Fold Adoption Receiver Block (downstream of SK-V18 generalization; active after SK-V18 W-PRUNE→G1..G6→PROVE→H1 close + G-Omega)
```

**Preamble re-key** (`:976`-`:990`): the prose body keeps the five LOCKED T-P2
fold designs (LAC-2F-FOLD-01..05, F1-F9) verbatim. Three sentence-level edits:

```diff
-Per MP-3B-SKV17-D01/D02, this block sits downstream of §13.5 SK-V15. SK-V15
-closes the CSS-honesty PRUNE-then-REBUILD repair first; SK-V17 then EMPIRICALLY
-PROVES the unified-tape / lazy-`ValueRef<G>` / NEON classifier model in skinny
-(`restart/skinny/tranches/sk-v17/SPEC.md:264`-`269`); SK-V18 is the totality
-fold that ADOPTS the proven `Tape`/`ValueRef`/`select_classifier` into
-crates/core and retires the eager-`OpenFrame` / AoS-`TapeRec` / per-leaf
+Per MP-3B-SKV18-D01/D02, this block sits downstream of §13.7 SK-V18. SK-V15
+closed the CSS-honesty PRUNE-then-REBUILD repair (`66232b7c3`); SK-V17 then
+EMPIRICALLY PROVED the unified-tape / lazy-`ValueRef<G>` / NEON classifier
+model in skinny (`restart/skinny/tranches/sk-v17/SPEC.md:264`-`269`; W0-W5
+closed `f6a38445b`); SK-V18 (§13.7) GENERALIZES it into ONE `.bbnf`-driven
+generator on the skinny tree; SK-V19 is the totality fold that ADOPTS the
+un-forked, tape-proven `Tape`/`ValueRef`/`select_classifier` into
+crates/core and retires the eager-`OpenFrame` / AoS-`TapeRec` / per-leaf
 `StructRegistry` fold-targets.
```

```diff
-SK-V18 W0 dispatches only AFTER SK-V17 skinny W0-W5 close proves the model and Pass
-Omega / G-Omega authorise. There is no implicit 6th wave, no W7, and no
+SK-V19 W0 dispatches only AFTER SK-V18 W-PRUNE→G1..G6→PROVE→H1 close proves the
+un-forked generator on 3 grammars (skinny) and Pass Omega / G-Omega authorise.
+There is no implicit 6th wave, no W7, and no
 challenge-time implementation overflow.
```

```diff
-Global SK-V18 gates:
+Global SK-V19 gates:
```

The five global-gate bullets (`:994`-`:1011`) keep their text; the body
references to "SK-V18 by-construction proof" (`:1001`-`:1002`) re-key to "SK-V19
adopts the SK-V18-proven 3-grammar un-fork to the 9-grammar fleet". The 5-shape
canon bullet (`:996`-`:998`) is UNCHANGED (no 6th variant).

**Receiver-row wave-ID re-key** (the table at `:1013`-`:1021`): rename
`MP.SK18.W0..W6` → `MP.SK19.W0..W6` throughout; per-row LOC / consumer / gate /
F-candidate / LAC text is preserved VERBATIM (no fold-design content changes).
The F4 BackendShape-canon disposition (`:1023`-`:1028`) is preserved unchanged —
LAC-2F-FOLD-02: the tape is a substrate-manifest category, not a 6th shape.

**Cross-ref re-key in MP-3B-SKV17 footers** (`:1030`-`:1040`): `MP.SK18.W0`/
`MP.SK18.W2`/`MP.SK18.W4`/`MP.SK18.W5` → `MP.SK19.W0`/`W2`/`W4`/`W5`; the
substrate-manifest fence + decision-engine WIRING text is unchanged.

---

## 2. Staged Diff 2 — NEW §13.7 SK-V18 GENERALIZATION Receiver Block (12 waves)

Applies `MP-3B-SKV18-D03/D04/D05/D06`. Inserted AFTER the re-keyed §13.6 (before
§14 Tranche I, currently `:1042`). Imported from the certified
`sk-v18/SPEC.md` §2 wave manifest (`:431`-`:447`) + lattice (`:535`-`:547`).
Every wave carries a same-wave consumer + an exit-gate falsifier. No wave admits
before its predecessor closes its exit gate. ≈ −10800 campaign LOC
(per-wave SPEC sum ≈−10685; `sk-v18/SPEC.md:571`, whose own token reads "PRUNE net
LOC ≈ −10800") — a REDUCTION, no `[generated-size-budget]` overflow.

```diff
+### §13.7 SK-V18 GENERALIZATION Receiver Block (the 12-wave skinny un-fork; active after §13.5 SK-V15 + SK-V17 close + Pass Omega/G-Omega)
+
+Per MP-3B-SKV18-D01/D03, the CERTIFIED SK-V18 is the GENERALIZATION cycle on the
+SKINNY tree: two forked parsers (JSON + CSS) collapse into ONE grammar-driven
+generator emitting JSON + CSS + Sheets from `.bbnf`, preserving >SOTA (CSS beats
+lightningcss 1.66-3.38×; JSON beats sonic-rs strict), aarch64-only,
+≈ −10800 campaign LOC (per-wave SPEC sum ≈−10685; `sk-v18/SPEC.md:571`). Every surface
+citation is the benched skinny tree (`skinny/crates/`), NOT
+`crates/core/` (the TOTALITY tree is the SK-V19 adoption target). The certified
+plan is `restart/skinny/tranches/sk-v18/SPEC.md` (`:19`-`:21`,`:58`-`:61`).
+
+Global SK-V18 gates (every wave carries the §2.1 generality + Lock-14 gate):
+- ONE generator emits JSON + CSS (+ Sheets at PROVE). The
+  `generator_grammar_count == 3` (json + css + sheets, NOT json + 7-css + sheets
+  — the P3 collapse) is a PROVE-EXIT gate, NOT a per-wave global: through
+  G1-G6 the count is 2 (json + css); Sheets enters the generator only at the
+  PROVE wave and only on a non-`N` verdict (`sk-v18/SPEC.md:254`: "MUST be 3 at
+  PROVE (json+css+sheets); 7-css inflation = the P3 overfit, REJECT"), mirroring
+  the PROVE-row falsifier below. This is the `scoped non-JSON witness` (3-grammar)
+  un-fork per the live MP.NW6 (`restart/MASTER-PLAN.md:662`)
+  single-negative-control standard (CSS + one Sheets control); the F.W5
+  nine-grammar close is FED by this generator and adopted at SK-V19 scale — §13.7
+  does NOT satisfy F.W5, and fleet-wide / grammar-neutral wording requires SK-V19
+  OR both Sheets AND BBNF-self in one wave.
+- One un-forked emitter dispatched on the LOWERED program, NOT a grammar tag:
+  `emitter_fork_present == false`; `generator_grammar_branch_count == 0`;
+  `generator_grammar_type_count == 0`; `emit_shape_source == lowered_program`.
+  `render(program)` reads output-shape ONLY from
+  `program.policy_summary.backend_shape` (the 5-shape canon, UNCHANGED — no 6th).
+- The relocated seam closes STRUCTURALLY: the R16 `RuntimeTarget: PartialEq`
+  full-row derive recurses into BOTH nested structs (`frontend_requirements`
+  #11 ∧ `output_labels` #12); `runtime_target_rows_collapsed == true`.
+- aarch64-only: the eq-set fan is the one real NEON Layer-1 body; x86/SVE remain
+  diagnostic. preserve-rich-ast: JSON's rich tree preserved by CONCRETE
+  FALSIFIER, not by construction.
+- The §6 named-primitive (a)-(d) escape is the ONLY admission for a
+  grammar-derived primitive (T-P2 literature-validated).
+
+| SK-V18 receiver | T-P1/T-P2 source | manual LOC / risk | MASTER alignment | same-wave consumer / gate | exit-gate falsifier (turns RED) |
+|---|---|---:|---|---|---|
+| MP.SK18.P1 DELETE the x86 surface crate-wide | T-P1 D-4 (`1D:99-102`); COH18-009; addendum A6 | 0 add / ≈−4500 del / med | H.W5 x86 successor; aarch64-only | The 11 `checkasm_parity.rs` x86_64 call sites DECOUPLE in the SAME commit (re-grep before merge as counts may drift); 12 aarch64 parity harnesses retained | `find …/x86_64 …/ext/x86 -type f == 0` (today 28); `cargo build`/`cargo test --no-run` clean (`x86_tree_deleted==true`) |
+| MP.SK18.P2 DELETE warm micro-fixture CSS bench | T-P2 addendum 5; `sk-v18/SPEC.md:608-633` | ≈−700 / low | H.W6/BENCH timed-plane | The retained `css_canon_bench` cold harness consumes the extracted 9-field oracle | `grep -c 'measure_mbps\|lightningcss_facts' bbnf-bench/src/nonjson_css_l4.rs == 0` (today 48; the certified SPEC `:627` exit-gate falsifier + owner-path `:614` bind the P2 gate to `bbnf-bench/src/nonjson_css_l4.rs` ALONE — the `src/`-qualified path disambiguates from the 7-hit `bbnf-bench/benches/nonjson_css_l4.rs`, and the 16 crate-wide hits in `bin/gate.rs` are NOT a P2 gate target, no SPEC/1D/3B wave owns their retirement; SPEC `:633` is the R14/H1 INDEPENDENT disclosure note, it binds NOTHING about the gate); `corpus_in_timer==true` |
+| MP.SK18.P3 collapse 6 of 7 byte-identical css_l4 replicas + RuntimeTarget row-collapse | T-P1 D-2 (`1D:87-93`); COH18-005 analog; addendum 2/R16 | ≈−5500 / high (relocated seam) | A.W2 nine-grammar census; G3 row-collapse co-gate | The G3 un-fork consumes the `RuntimeTarget: PartialEq` full-row derive (both nested structs) | `md5 …/{json,css_l4}/generated.rs` no identical pair; `runtime_target_rows_collapsed==true` |
+| MP.SK18.P4 fix Lock-14 green-by-exclusion gate (BEFORE G2/G3) | T-P1 D-7 (`1D:108-111`); COH18-012; addendum A3 | ≈+15 / high | H.W4.LOCK14; §13.5 W2 gate restoration; MP.NW6 | The G1/G2/G3 emitter waves are neutrality-scanned AS authored (P4 is their entry-gate) | re-inject `SHEETS_GENERATED_RS`→RED; `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`; `lock14_gate_scans_codegen==true` |
+| MP.SK18.P5 purge metalang bench-wave-id leak | T-P1 D-8 (`1D:112-114`); addendum A1/regen | ≈0 (rename-only) / low | clean-regen discipline | The 1:1 regen of `json/generated.rs` consumes the template-source `json_sink_direct.rs` rename; the `lib.rs:565` test-assert updates SAME pass | `grep -c parse_w11_1_number json/generated.rs == 0` (today 7; the certified SPEC `:755`/`:570` binds the P5 gate to `json/generated.rs` ALONE — the unscoped crate-wide count is 15: + 7 template-source `json_sink_direct.rs` + 1 `lib.rs:565` test-assert, both driven to 0 by the same rename+regen but NOT the SPEC gate target); `regen --check` exit 0 |
+| MP.SK18.G1 JSON projection — `SinkOnlyExpr` AST-walk emitter | T-P2 R-C C1; T-P1 D-9; `sk-v18/SPEC.md:438` | ≤450 hand / high | H.W4 SinkOnly; JSON 51-row guard | The byte-equiv diff vs `json_templates/` oracle BEFORE oracle deletion (COH18-011) | `.bbnf`-mutation falsifier; `g1_hot_leaf_preserved==true`; `json_strict_rows_admitted==51` |
+| MP.SK18.G2 CSS lowering — `css_balanced_component_scan` + fact-keyed projection | T-P2 R-B; T-P1 D-6; §6 (a)-(d) gate; `sk-v18/SPEC.md:439` | ≤450 hand / high | H.W6 CSS >SOTA; §13.5 W5 CSS typed provider | The arg-mutation falsifier + the 9-field cssparser oracle (gate BEFORE speed); `CSS_GENERATED_RS` DELETED | `css_typed_summary_equal==true` before speed; `track1_rich/lcss > 1.0×` same-run ∧ no pre-G2 regression |
+| MP.SK18.G3 un-fork the emitter — DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` | T-P1 D-3/COH18-003/COH18-008; T-P2 R-A; `sk-v18/SPEC.md:440` | ≤450 hand / high (relocated-seam) | F.W5 un-fork; H.W4 5-shape; MP.NW6 Lock 14 | Generated output byte-equivalent to G1/G2-closed files (PATH change, not OUTPUT) | 5-conjunct exit: `emitter_fork_present==false`; `generator_grammar_branch_count==0`; `generator_grammar_type_count==0`; `emit_shape_source==lowered_program`; `runtime_target_rows_collapsed==true` |
+| MP.SK18.G4 shared value-API trait + phantom resolution — `Cursor` micro-trait, DELETE `<G>` | T-P1 D-5/COH18-008; T-P2 R-D; `sk-v18/SPEC.md:441` | ≤450 hand / med-high | B substrate; G path/value/visitor; §13.5 W5 CSS provider | JSON `value.rs` navigation byte-equal diff vs pre-G4 (preserve-rich-ast, not "by construction") | `phantom_generic_resolved==deleted`; `json_rich_navigation_preserved==true`; `shared_trait_non_collapsible==true` |
+| MP.SK18.G5/G6 neutral scan retarget — NEON onto the CSS scan shell + neutralize json/scan.rs | T-P1 D-6; T-P2 R-F; `sk-v18/SPEC.md:442` | ≤450 hand / med-high | H.W2/H.W2.5 Lock 16; H.W5 NEON | The (P3-collapsed singular) generated scan CALLS the shared `runtime_simd` primitive (no orphan kernel) | `acceleration_at_admission==admission`: generated.rs caller census non-empty ∧ `simd_admission_profile_sampled==true` |
+| MP.SK18.PROVE Sheets via the un-forked generator ONLY — precedence-tower core | T-P1 1D U-2; T-P2 R-E-2 / 2C SHEETS-PRECEDENCE-TOWER; `sk-v18/SPEC.md:443` | ≈+200 / med-high (make-or-break) | G future-grammar gate; the negative control | The Sheets value type instantiates the G4 trait; Sheets `generated.rs` md5-distinct from JSON∧CSS | `sheets_grammar_shape==pratt-operator`; `generator_grammar_count==3`; no `const.*_RS.*r#` Sheets blob; BINDING FALLBACK `N` if shim-needed |
+| MP.SK18.H1 CSS framing honesty + corpus-in-timer + regen --check clean | T-P1 1D U-4 (load-depressed re-lock); R14/R-A0-1; `sk-v18/SPEC.md:444` | 0 source / low | H.W6 honesty; J.W1 close | The H1 quiet re-capture + PASS-IMPL close audit consume the deferred G6 figure | `materialization_framing==lazy-rich-vs-eager-cssom`; `host_loadavg<1.0`; ≥1 regular corpus crossing >1.0× same-run |
+
+Binding lattice (`sk-v18/SPEC.md:535-547`): P-cluster (P5 before G1; P4 live
+before G2/G3; P3 dual-gates G2 + binds G3) → G1 → G2 (G1∧P3) → G3 (G1∧G2∧P4∧P3)
+→ {G4 (G1∧G2∧G3); G5/G6 (P1∧P3∧G3, PARALLEL to G4)} → PROVE (G3∧G4, PARALLEL to
+G5/G6) → H1 (G5/G6∧PROVE). Wave count = 12, at the skinny ceiling (no W12).
+The P-cluster (P1-P5) is the ONLY dispatch-now-eligible cluster on S-P3 close;
+every GENERALIZE/PROVE/HONESTY wave is blocked until its predecessor closes its
+exit gate AND its entry-gate predicate holds GREEN AND the orchestrator/user
+dispatches the wave triumvirate.
+
+Per MP-3B-SKV18-D04, the P-cluster build-soundness coupling is recorded: P1's
+`checkasm_parity.rs` x86 decouple is SAME-commit across all 11 x86_64 call sites
+(a deletion list narrower than
+the verify grep ships a RED-by-construction gate); P4 is the ENTRY-GATE of
+G2/G3, a hard ordering, not a preference. Per MP-3B-SKV18-D05, each G-wave is
+≤450 hand LOC under the §6 (a)-(d) named-primitive gate; the
+`css_balanced_component_scan` is FORCED-demoted (CSS-scoped) per the s6/C4
+neutrality finding; the un-forked emitter reads `BackendShape`, not a grammar
+tag. Per MP-3B-SKV18-D06, a Sheets emission needing a shim is the negative-
+control fail `N` (generalization NOT real), surfaced honestly, never paper-
+closed; H1 discloses `materialization_framing == lazy-rich-vs-eager-cssom`.
+
+Per the CH3-V1-R2 retime (3B/3D-D08/3F-MH-003): G2/G4/G6 entry is BLOCKED until
+the SK-V16/V17 REDRESS reconcile (the four-item pre-block is complete only for
+the SK-V15-W11 ledger; 1D U-5) is on the committed ledger as a
+Pass-Omega-V10 / pre-W-PRUNE blocker — these waves abut REDRESS items
+51/53/246/247 (1D:168-171; the 3F CH3 row 3F-migration-handoff.md:274;
+item 246 = the W11T parse-only structural-STREAM driver reject that bounds G4)
+and run DURING SK-V18.
```

---

## 3. Staged Diff 3 — §25 Implementation Order Reconciliation

Applies `MP-3B-SKV18-D08`. The §25 footer (`:1415`-`:1422`) currently sequences
"SK-V18 W0 (§13.6 MP.SK18.W0) dispatch to ADOPT the proven substrate into
crates/core". Restore the monotonic skinny→totality direction.

```diff
-Per MP-3B-SKV17-D01, the SK-V17/SK-V18 tape-fold is sequenced DOWNSTREAM of
-SK-V15: SK-V15 W0-W11 close the CSS-honesty repair first; SK-V17 skinny W0-W5
-then PROVE the unified-tape / lazy-`ValueRef<G>` / NEON classifier model; only
-after that proof and Pass Omega / G-Omega authorisation does SK-V18 W0 (§13.6
-MP.SK18.W0) dispatch to ADOPT the proven substrate into crates/core. The
-direction is monotonic skinny→totality; MASTER never dictates back to a live
-skinny iteration. No SK-V18 fold wave is engineered-deferred without its named
-SK-V17-close + G-Omega receiver.
+Per MP-3B-SKV18-D08, the generalization→totality sequence is: SK-V15 W0-W11
+CLOSED the CSS-honesty PRUNE-then-REBUILD repair (`66232b7c3`); SK-V16 W0-W6
+landed the CSS >SOTA rearchitecture + shared flat-tape substrate (`1c5bd7a25`);
+SK-V17 skinny W0-W5 CLOSED, PROVING the unified-tape / lazy-`ValueRef<G>` / NEON
+classifier model (`f6a38445b`); SK-V18 (§13.7) is the skinny GENERALIZATION
+cycle — ONE `.bbnf`-driven generator emitting JSON+CSS+Sheets, un-forking the
+two parsers on 3 grammars, aarch64-only, ≈ −10800 campaign LOC (per-wave SPEC sum
+≈−10685; the W-PRUNE cluster is
+dispatch-eligible on S-P3 close; every GENERALIZE/PROVE/HONESTY wave is
+predecessor-gated); only after SK-V18 H1 close and Pass Omega / G-Omega
+authorisation does SK-V19 W0 (§13.6 MP.SK19.W0) dispatch to ADOPT the un-forked,
+tape-proven generator into the 9-grammar crates/core fleet + onboard BBNF-self.
+The direction is monotonic skinny→totality; MASTER never dictates back to a live
+skinny iteration. No SK-V18 wave is engineered-deferred without its named close +
+G-Omega receiver; no SK-V19 fold wave dispatches without its SK-V18-H1-close
+predecessor.
```

---

## 4. Staged Diff 4 — §24 Carry Ledger Re-key + SK-V19 Tee-Up Rows

Applies `MP-3B-SKV18-D07`. The §24 SK-V18 tape-fold carry row is a SINGLE line at
`restart/MASTER-PLAN.md:1346` (the §24 section header is `:1336`); it re-keys to
SK-V19; the three carried totality-tree leaks + BBNF-self litmus + fleet
onboarding are added as SK-V19 receivers (DEFER — their CLOSE is a SK-V19 wave,
NOT SK-V18-closeable).

```diff
-| SK-V18 tape-fold adoption (MP.SK18.W0..W6) | §13.6 / SK-V17 skinny W0-W5 close + G-Omega | The fold's proven `Tape`/`ValueRef`/`select_classifier` is not yet adopted into crates/core; eager-`OpenFrame`, AoS-`TapeRec`, per-leaf-`StructRegistry`, and CSS fact-stream-String fold-targets persist. | Per MP-3B-SKV17-D06/D08: MP.SK18.W4 fences per-leaf `StructRegistry::layout(rule)` (W3 generator resolves layout once at codegen; `arena.rs:47` coupling severed by F1); MP.SK18.W0 gates MP.SK18.W2 to exactly-one SoA encoding across all 8 carriers; a dual AoS/SoA end-state re-opens REDRESS-53. Dispatched only after SK-V17 skinny W0-W5 close proves the model and G-Omega authorises; no engineered-defer-without-receiver. | omega + skinny |
+| SK-V18 generalization (MP.SK18.P1..H1; the 12-wave skinny un-fork) | §13.7 / §13.5 SK-V15 + SK-V17 close + Pass Omega/G-Omega | The two forked parsers (JSON+CSS) + 7 css_l4 replicas + the CSS const courier + the `RuntimeEmitterKind` fork + the phantom `<G>` axis persist; ONE `.bbnf`-driven generator not yet realised. | Per MP-3B-SKV18-D03/D04/D05/D06: each §13.7 wave carries a same-wave consumer + RED exit-gate falsifier; W-PRUNE is dispatch-eligible on S-P3 close, every GENERALIZE/PROVE/HONESTY wave predecessor-gated; a Sheets `N` (shim-needed) is the honest negative-control fail, never paper-closed; ≈ −10800 campaign LOC (per-wave SPEC sum ≈−10685; a REDUCTION). | omega + skinny |
+| SK-V19 totality-fold adoption (MP.SK19.W0..W6, re-keyed §13.6 F1-F9) | §13.6 / SK-V18 H1 close + G-Omega | The SK-V18-proven un-forked generator is not yet adopted into the 9-grammar crates/core fleet; eager-`OpenFrame` / AoS-`TapeRec` / per-leaf-`StructRegistry` fold-targets persist in totality. | Per-grammar regen ×8/×9; SoA exactly-one-encoding closure (Lock 1); dispatched only after SK-V18 H1 close proves the 3-grammar un-fork (skinny); no engineered-defer-without-receiver. | omega + skinny |
+| SK-V19 totality-tree leaks (the 3 carried at HEAD, routed forward) | §13.6 SK-V19 sub-section / SK-V19 waves | (a) the 9-ident `ir/registry/strategy.rs` grammar-named table (Lock 14 self-gate RED at 13 sites); (b) `crates/core/src/css_types.rs` (named in the Lock 14 `:349` "The current overfitting mess —" enumeration, 66 LOC, in generic core); (c) the `simd-scan` vs skinny `bbnf-simd` probe-API asymmetry — all verified at HEAD but SK-V19-closeable, NOT SK-V18. | (a) R16 `PartialEq` row-collapse over ALL 9 idents + widen leak regex to 9 names (≈+217); (b) relocate to `crates/css/` declaration crate (admissible per Lock 14(c)) OR delete; (c) decide UNIFY vs renamed-parallel-scanner + 8/9 OnceCell re-route. DEFER — do NOT bolt a 9-name regex widen into an SK-V18 gate. | omega + skinny |
+| SK-V19 BBNF-self 4th-grammar litmus + 9-grammar fleet onboarding | §13.6 SK-V19 sub-section / §5 A.W2 nine-grammar census | SK-V18 witnesses 3 grammars (JSON+CSS+Sheets); SK-V19 scales the onboarding test to 9 + arbitrary user grammars. BBNF-self exercises recursive grammar-source ownership with no `Bbnf` branch in generic crates. | regen/`--check` diff + generic-code no-change gate; source/metadata-only addition; fail if any generic owner path changes except generated manifests. SK-V19 entry census: `crates/core/src/runtime` line-1 `@generated` provenance + md5-distinctness across the 9 (mirrors the skinny P3 falsifier). | omega + skinny |
```

---

## 5. Staged Diff 5 — §5 F.W5 / §13.5 CSS Verdict Reconciliation

Applies `MP-3B-SKV18-D09` (F.W5 un-fork statement) + `MP-3B-SKV18-D10`
(§13.5 CSS verdict UPGRADE) + carried `MP-3B-V1-D02` (A-J stubs pending).

**§5 F-tranche row + §5.3 YAML F slot** (`:196`/`:519`): annotate F.W5 "Nine seed
grammars build through new template" as the UN-FORK statement — neither impl
tree realises it yet (T-P1 COH18-004); proven on 3 grammars at SK-V18 (skinny),
scaled to 9 at SK-V19 (totality). Prevents F.W5 reading as already-satisfied.

```diff
+Per MP-3B-SKV18-D09, the §5 F gate / F.W5 "Nine seed grammars build through new
+template" IS the un-fork statement T-P1 COH18-004 shows UNREALISED in BOTH impl
+trees (skinny: 2 forked arms + courier + 7 replicas; totality: 9 generated files
+fed by a grammar-named lookup table). SK-V18 proves the un-fork on 3 grammars
+(skinny); SK-V19 scales it to 9 (totality). The F.W5 close gate is FED by
+SK-V18's un-forked generator and adopted at SK-V19 scale — it is NOT satisfied
+at HEAD.
```

**§13.5 SK-V15 block CSS clauses + §13 H.W6** (`:912`-`:973`): the §13.5 block
flips from "active pending" to **LANDED-as-receiver** (SK-V15 W0-W11 closed
`66232b7c3`); the CSS verdict is UPGRADED per `MP-3B-SKV18-D10`.

```diff
+Per MP-3B-SKV18-D10, the SK-V15 CSS verdict is UPGRADED: T-P1 COH18-013 raises
+the "CSS contrived" verdict to "CSS >SOTA is directionally-valid pending the H1
+`css_canon_bench` re-lock; the overfit is IMPLEMENTATION (forks/replicas), not
+measurement". SK-V18 retires the FORKS, not the measurement. Carry the
+directional caveat (loadavg 4.35 at capture, H1 quiet re-lock pending) until the
+bench row is re-locked — do NOT carry the un-caveated "MEASUREMENT-VALID"
+closure word the row's own fail-action forbids (CH2-V1-R03).
```

Per carried `MP-3B-V1-D02`: the A-J stubs stay PENDING; scoped landings labelled
scoped/partial/refuted, not V1/root close (the JSON 51/51 guard is a guard, not
generalization proof).

---

## 6. Staged Diff 6 — §13 H-Row + Lock-10 Cross-Ref Alignment

The §13 H-block cross-references currently thread "SK-V18 fold (§13.6)" through
H.W1 (`:642`), H.W4 (`:646`), the Lock-10 inheritance row (`:616`), and the §13
preamble (`:584`-`:592`). These re-key the tranche label: the SK-V17-proven
substrate feeds the gate; the SK-V18 GENERALIZATION (§13.7) un-forks the emitter;
the SK-V19 fold (§13.6) adopts it. The substrate / decision-engine WIRING text is
UNCHANGED — only the wave-ID label (`MP.SK18.W*`→`MP.SK19.W*`) and the tranche
attribution move. The 5-shape canon (`:616` Lock-10 row) is UNCHANGED.

These are label-only edits. Propagation cost, enumerated: 6 `MP.SK18.W*`→
`MP.SK19.W*` rename sites in this diff — H.W1 (`:642`), H.W4 (`:646`), the Lock-10
inheritance row (`:616`), and the §13 preamble (`:584`-`:592`) ×3 — plus the 8
MP-3B-SKV17-footer rename sites already counted in Diff 1 (`:1030`-`:1040`,
`:1013`-`:1021`). Total Diff-6 sites = 6; no fold-design, gate, or substrate
content changes.

---

## 7. Invariant Check (post-stage; verifies the staged diff would preserve)

- **16-lock count PRESERVED.** No staged diff touches LOCKS.md; the count stays
  16 (`grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` = 16). The Lock 14:620
  phantom-axis reconcile + Lock 14:349 css_types.rs reconcile are SK-V19 / Pass
  Omega CRUD-3 obligations (3F-MH-012/013), NOT staged here.
- **5-shape canon verbatim, NO 6th.** Every staged block preserves
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; the tape stays
  a substrate-manifest category (LAC-2F-FOLD-02). No 6th `BackendShape`.
- **No new BIR variant / directive.** The un-forked emitter reads `BackendShape`
  from the lowered program; no new emit category.
- **aarch64-only.** P1 deletes x86 crate-wide; the NEON eq-set is the one real
  Layer-1 body; x86/SVE diagnostic.
- **preserve-rich-ast.** G4 preserves JSON's rich tree by CONCRETE FALSIFIER.
- **No re-opened REDRESS.** The §13.7 gates fence AZ-IV eager, StructRegistry
  per-leaf, fact-stream admission, x86, second-substrate; CH3-V1-R2 blocks
  G2/G4/G6 entry until the SK-V16/V17 reconcile is committed (the abutting
  rejected routes are REDRESS items 51/53/246/247 — 1D:168-171; item 246 bounds
  G4's structural-stream-driver route).
- **≈ −10800 campaign LOC = REDUCTION** (per-wave SPEC sum ≈−10685;
  `sk-v18/SPEC.md:571`). No `[generated-size-budget]` overflow.
- **STAGED ONLY.** No live governance surface is edited by this pass; Pass Omega
  CRUD applies post-G-Omega.

---

## 8. Application Order (for the POST-G-Omega CRUD pass)

1. Diff 1 (re-key §13.6 → SK-V19) FIRST — frees the "SK-V18" label.
2. Diff 2 (NEW §13.7 SK-V18 GENERALIZATION block) — inserts after re-keyed §13.6.
3. Diff 3 (§25 Implementation Order) + Diff 5 (§5 F.W5 / §13.5 CSS) — sequence + classification.
4. Diff 4 (§24 Carry Ledger re-key + SK-V19 tee-up rows).
5. Diff 6 (§13 H-row label alignment) LAST — propagates the re-key to cross-refs.

The §13.6 re-key MUST NOT drop a fold-design row (F1-F9 preserved verbatim). The
§13.7 block is ≈81 staged/rendered doc lines (the Diff 2 hunk is 81 added lines,
including the 12-row wave table whose rows render one doc line each — no 4-7×
expansion). All diffs are proposal-only; the cost lands in the CRUD pass, not
here.
