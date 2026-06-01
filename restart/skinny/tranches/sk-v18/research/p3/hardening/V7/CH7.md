# SK-V18 S-P3 CHALLENGE — CH7 (cycle V7) — CLOSE-CONDITION-HONESTY lens

Target: `restart/skinny/tranches/sk-v18/SPEC.md` (§0 close/goalset/telemetry, §1 §6-escape law,
§2/§2.1 manifest + lattice, §6 G3 un-fork, §9 PROVE Sheets fallback, §10 H1 honesty close, §11 route
ledger, the §1638 close restatement). Bound by S-P2 §3 sequencing + the S-P0 addenda
(`SYNTHESIS-AUDIT-OVERFIT.md`). Lens: is the close condition the HONEST generalization goalset (one
generator, >SOTA preserved with the lazy-rich framing disclosed, x86 gone, net LOC negative); is the §6
fallback binding (a non-grammar-derivable parser surfaces HONESTLY, no `_RS` blob, never paper-closed)?

Prior cycle (V6, CH7) found ONE REVISE: the H1 close-bearing `css_sota_ratio_held` column reused the
verbatim G2 "same-run regression vs the pre-G2 baseline" phrasing at a plane where pre-G2 code is gone.
**That REVISE is FOLDED** — verified this pass (below). V7's task: drive out any RESIDUAL precision
REVISE and catch any genuine REJECT.

---

## Pillar 1 — ONE generator (close-cond #1/#2/#3, §1638 restatement)

- **`generator_grammar_count == 3`** (json + css + sheets, NOT json + 7-css + sheets — the P3 collapse,
  R-A0-2). Bound THREE ways: close-cond #1 (line 67), PROVE exit (line 1474/1502), §0.4 column (line
  253, REJECT if `!= 3`). The 7-css inflation is the named P3 overfit. **ACCEPT.**
- **`verbatim_blob_present == false` campaign-wide** (line 69), cleared by G1+G2, the `.bbnf`-mutation
  test the binding gate (not the cohort-carried ≈910-LOC figure, §1 line 330). **ACCEPT.**
- **Un-forked emitter on the LOWERED program, not a grammar tag** — `emitter_fork_present == false` ∧
  `generator_grammar_branch_count == 0` ∧ `generator_grammar_type_count == 0` ∧
  `emit_shape_source == lowered_program` (close-cond #2, G3.2 five-conjunct exit lines 1096-1118). The
  FOURTH conjunct (`emit_shape_source`) closes §5-risk-1: without it the relocated seam riding the
  neutral per-profile columns passes conjuncts 1-4 under a green gate (line 1116-1117). The §1 standing
  seam-scan (line 338-342) is the SAME field-set as the G3 conjunct-5 grep, so the standing scan is as
  strong as G3's own. **ACCEPT** — no paper-close that relocates the fork into data.
- **The relocated seam is closed STRUCTURALLY** — `runtime_target_rows_collapsed == true` via the R16
  `RuntimeTarget: PartialEq` full-row derive recursing into BOTH nested structs (`frontend_requirements`
  #11, `output_labels` #12; line 80-85, G3.3 line 1128-1141). A hand-rolled prose-field compare is the
  named REJECT (shallow-compare false-green of EITHER nested struct). md5-distinct is
  NECESSARY-NOT-SUFFICIENT, explicit campaign-wide. **ACCEPT** — the only check the arm-grep is
  syntactically incapable of seeing, bound as the structural co-gate.

## Pillar 2 — >SOTA preserved HONESTLY (close-cond #6/#12, §0.2, §0.5, §10)

- **The lazy-rich framing is DISCLOSED, not re-labeled away.** `materialization_framing` enum is CLOSED
  to `{lazy-rich-vs-eager-cssom|undisclosed}` (§0.4 line 255) so the gate can REJECT any other string;
  `undisclosed` is the pre-H1 default and turns the H1 gate RED (line 1581/1590). An unqualified "beats
  CSSOM"/"equal-work" claim behind the re-label WITHOUT the materialization-depth asymmetry disclosed is
  a REJECT (R-A0-1, §0.2 line 191-194, H1 falsifier line 1570). **ACCEPT** — the OR-escape that let the
  re-label close the honesty gate (R-A0-1) is closed; the asymmetry must be EXPLICIT.
- **The binding floor is the SAME-RUN ratio, NOT the un-re-locked S-P1 absolute.** `track1_rich/lcss >
  1.0×` same-run ∧ no same-run regression vs the pre-G2 baseline CAPTURED AT G2 ENTRY in one quiet run
  (close-cond #6 line 105-118). The S-P1 absolutes (bootstrap 2.190, tailwind 3.375, mcw 1.658, animate
  2.101, loadavg 4.35, NOT re-locked) are DIRECTIONAL antecedents, explicitly NOT the floor — keying the
  close on an un-re-locked absolute is named as the unfalsifiable-gate hazard the SAME-RUN comparison
  REPLACES (line 116-118). **≥1 regular corpus (animate OR bootstrap) crossing mandatory** (§0.5 line
  309-316). **ACCEPT** — the close compares two figures in ONE quiet plane where a load-depressed
  antecedent cannot; no vacuous absolute-keyed gate.
- **Gate-before-speed** — `css_typed_summary_equal` (9-field cssparser oracle) holds BEFORE any speed
  admission (close-cond #6 line 127-128, §0.2 line 182, §0.4 line 285); the speed falsifier is
  admissible ONLY after parity passes (an incomplete arg-derivation diverges → REJECT before any speed
  row). JSON: strict-vs-sonic-rs strict, same-plane, per-iter oracle, 51/51 cold. **ACCEPT.**
- **Hot-leaf throughput-equivalence PROVEN, not asserted** — JSON by `g1_hot_leaf_preserved`
  (byte-equivalent inline cfg + sink call sites) ∧ `g1_json_guard_rows_held` (51/51 within ±1.0%); CSS
  scan by oracle-parity-then-same-run-ratio (line 119-126). **ACCEPT.**

## Pillar 3 — x86 gone (close-cond #7, P1, §11)

- BOTH surfaces (`src/x86_64/` 24 files + `ext/x86/` vendored ASM + nasm `build.rs`) DELETED crate-wide;
  falsifier `find …/x86_64 …/ext/x86 -type f == 0` ∧ aarch64-neutral grep ∧ `cargo build`/`cargo test
  --no-run` clean (close-cond #7 line 130-133, P1). The deletion list is reach-matched to the verify
  grep (§11 P1 row line 1610 — a narrower list is the named RED-by-construction hazard). **ACCEPT** —
  falsifiable, reach-matched, no x86/AVX/SVE/nasm survivor admitted.

## Pillar 4 — net LOC negative (close-cond, §2 manifest)

- Net ≈ **−10800** (line 21-22, 61, 451, 1637), a falsifiable REDUCTION. Per-wave deletions sum
  consistently: P1 ≈−4500 + P2 ≈−700 + P3 ≈−5500 + P4 ≈+15 + P5 ≈0 = ≈−10685 deletions; Sheets adoption
  ≈+200; G-wave hand source ≤450/wave-capped with generated outputs net ≈0. The `≈` is honest
  approximation within the stated band; `[generated-size-budget]` flagged as a REDUCTION, no overflow.
  **ACCEPT** — the figure is directional/approximate and load-bearingly NEGATIVE, not a precise gate.

## The §6 fallback binding (the lens's other half) — §1, §9, §0.3, §11

- **The Sheets binding fallback is honest and surfaced as the negative control.** If Sheets cannot emit
  via the generator ONLY, generalization is NOT real — `sheets_emission_path == shim` → outcome **`N`**
  (negative-control fail, §0.3 line 213/220-224), surfaced HONESTLY, do NOT stub-prove, do NOT
  hand-write a `_GENERATED_RS` Sheets block (close-cond #9 line 149-151, §9 line 1486-1488, §11 PROVE
  row line 1620). The N-vs-L-vs-S distinction is unambiguous: `N` = generalization-not-real (Sheets
  shim); `L` = honest NON-Sheets residual loss; `S` = admission-capable parse not crossing the bar. A
  shim is NEVER `S`, NEVER `L`, NEVER paper-closed; the gate does NOT paper-close a shim as a pass (line
  1515-1517). **ACCEPT** — the binding fallback is the single most load-bearing honesty clause and it is
  bound to a falsifiable column with an explicit no-paper-close.
- **`sheets_grammar_shape == pratt-operator` proven by a CONCRETE STRUCTURAL FALSIFIER, not "by
  construction"** — ≥7 chained per-level descent fns + the cyclic `paren_expr→expression` back-edge,
  machine-counted; a flat-stream (R-E-3 REJECT) or single-recursive-tree emission has FEWER than 7
  chained level fns and FAILS the count (line 1457-1464). **ACCEPT** — no hollow "third-JSON" litmus
  closes the negative control.
- **If the precedence tower breaks (G3 cannot render recursive `CallRule`/`RepeatLoop` chains from
  grammar structure), that becomes a §6 honest-finding** — a named, `.bbnf`-invoked, parameterized
  precedence primitive with a scalar/checkasm reference, never a silent blob, never a paper-close (§9
  line 1488-1491, §1638 close, S-P2 §4 R-E candidate). PROVE does not paper-close (line 1491). **ACCEPT.**
- **The §6 named-primitive escape (R-A0-3, "the single largest paper-close surface") is machine-(a)-(d)-
  gated** — (a) grammar-INVOKED-by-name; (b) emitted-output VARIES under invoking-rule mutation (the
  byte-set/numeric-class mutation distinguishing derived from relabeled — byte-equivalence alone is
  satisfiable by routing the SAME literal); (c) `verbatim_blob_present == false`; (d)
  PROFILE-PROVEN-NARROW-LEAF (primitive LOC ≤ profiled hot-leaf extent — the god-kernel REJECT bounding
  size). Any of the four failing = relabeled hand-written blob = REJECT (§1 line 357-378). **ACCEPT** —
  (a)/(b) bound the routing, (d) bounds the size; the escape cannot admit an arbitrarily large relabeled
  blob.
- **The CH6 FORCED demotion to `css_balanced_component_scan`** (GROUND s6/C4) — the offered non-CSS
  dischargers (JSON `{}`/`[]`, Sheets `paren_expr`) are parse-with-emit descents structurally
  INCOMPATIBLE with the CSS byte-SKIP shell, so the "invoke the SAME primitive" branch is UNREACHABLE
  and the CSS-scoped name is the FORCED outcome; the gate REJECTs a neutral name with zero
  structurally-compatible non-CSS caller (§1 line 380-392, §11 G2 row). **ACCEPT** — a neutrally-named
  CSS-only primitive (an overfit-in-waiting) is foreclosed, discharged by the rename not a fabricated
  caller.
- **Acceleration-at-admission fallback honesty** — `acceleration_at_admission == admission` REQUIRES
  BOTH conjuncts (close-cond #10 line 153-164, §0.4 line 251): (i) the generated-`generated.rs` caller
  census non-empty AND (ii) `simd_admission_profile_sampled == true` (the `runtime_simd` entry in the
  `css_canon_bench` samply sample with non-zero self-time). A census hit in dead/unreachable code the
  profile does NOT see == `dead`, NOT `admission`; the enum is the SAME two-value domain §0.3 and §8/G6
  decide on (a third state would make the gate non-deterministic, line 251). **ACCEPT** — no
  source-census-only paper-close of a NEON claim.

---

## The V6 REVISE disposition (verified FOLDED this pass)

V6/CH7 flagged the H1 `css_sota_ratio_held` column (and its G3/exit-gate twins) for reusing the
verbatim G2 "same-run regression vs the pre-G2 baseline" phrasing at a plane where pre-G2 code is gone.
Re-grepped this pass (`grep -n "no same-run regression vs the pre-G2\|re-confirmed DIRECTIONALLY\|never
re-measures pre-G2" SPEC.md`):

- **H1 exit-gate (line 1564-1567)** now reads: "re-confirmed DIRECTIONALLY against the G2-RECORDED
  `track1_rich_over_lcss_ratio_pre_g2` baseline — the pre-G2 code is gone post-G2 so H1 never
  re-measures pre-G2 same-run, per close-cond #6."
- **H1 telemetry column (line 1587)** carries the SAME directional-re-confirmation phrasing.
- **H1 task #5 (line 1556)** reads "the H1 quiet re-capture re-confirms directionally."

The verbatim G2 phrasing is GONE from the H1 close-bearing loci; all three now match the authoritative
§0 framing (close-cond #6 line 113, §0.2 line 180, §0.4 line 264 — "H1 re-confirms directionally, NOT
re-measured at H1"). **The V6 REVISE is discharged. ACCEPT.**

Independent re-check of the G3 twin (line 1125 prose "no same-run regression vs the pre-G2 baseline"):
at G3 the pre-G2 baseline IS the freshly G2-recorded figure and G3 is the wave immediately after G2, so
the G3 conjunct-8 phrasing is honest — and its CONSUMED column (line 1162) is explicit:
"re-confirmed DIRECTIONALLY against the G2-RECORDED … never re-measured here, per close-cond #6." No
residual misleading phrasing at G3. **ACCEPT — no new REVISE inherited.**

---

## Sequencing / addenda cross-checks under the lens

- **The close gates consume a sound, acyclic DAG** (§2.1 lattice line 535-544, matches S-P2 §3): PRUNE →
  G1 → G2 → G3 → {G4 → PROVE ∥ G5/G6} → H1. PROVE entry is the explicit conjunction `G3 ∧ G4` (G4 a
  DIRECT predecessor, seq/C6 fold — PROVE NEVER admits before G4 closes, §9 line 1423-1432); G5/G6 hangs
  off G3 PARALLEL to G4 (seq/C7); the two branches join at H1. No close claim hangs off a broken
  sequence. **ACCEPT.**
- **Every close-bearing telemetry column is consumed in a named wave slice** (no producer-only field),
  per `[typed-materialization-invariant]` — §0.4 line 274-290 enumerates each of the 13 + supporting
  columns to its consuming wave; each wave §-telemetry block closes with "every emitted column is
  consumed in the … slice (no producer-only field)." **ACCEPT** — a producer-only column FAILS the wave;
  no decorative close column.
- **No addenda violation under the lens** — verbatim-blob, distinct-output (the 3-co-gate CONJUNCTION,
  not an md5 check), single-emitter (the 4-conjunct with the `emit_shape_source` relocated-seam guard),
  phantom-generic, corpus-in-timer/timed-plane-symmetry, acceleration-wiring all bind THREE ways
  (close-gate row + §0.4 REJECT column + §1/§11 pre-block). **ACCEPT.**
- **Outcome enum honesty** (§0.3) — `N` (negative-control fail / shim), `L` (honest non-Sheets residual),
  `S` (admission-capable non-SOTA honest residual), a NEON speedup CLAIM is `A` ONLY under the
  timed-plane binding AND `host_loadavg < 1.0`; a corpus-in-timer figure under load ≥ 1.0 (or no stamp)
  is `S` not `A`; a checkasm PASS without any corpus-in-timer figure is `C` not `A`; a recognition-only
  `A` (`track1_full_parse`) does NOT discharge the typed close. **ACCEPT** — no outcome lets a paper-close
  or a load-depressed figure masquerade as an admit.

---

## Adversarial sweep for a residual REVISE (proportionate)

I checked the close-bearing loci for any phrasing that would MISLEAD an implementer (a wording nit on a
1656-line doc is a REVISE only if materially clarifying):

- Net-LOC `≈−10800` vs the wave-sum `≈−10485..−10685`: the `≈` is honest approximation, load-bearingly
  NEGATIVE, not a precise gate — NOT a REVISE.
- §3.6 W-PRUNE telemetry: `corpus_in_timer (P2; true)` and `runtime_target_rows_collapsed (P3; true)`
  appear in BOTH the P-cluster slice AND later G2/G3/H1 slices — but each is RE-asserted (not
  producer-only) at each consuming wave, and §0.4 line 254 explicitly notes `corpus_in_timer` is
  MUST-be-true at G2/H1 with G6 deferring its figure to H1. The repeat is intentional cross-wave
  re-assertion, not a producer-only leak — NOT a REVISE.
- The §1638 close restatement is consistent with §0.1's twelve-clause close on every pillar (one
  generator / un-forked / shared trait / phantom deleted / >SOTA same-run / x86 gone / gate meaningful /
  Sheets fallback honored / NEON at admission / regen clean / net −10800). No drift. NOT a REVISE.

No residual REVISE surfaces under the CLOSE-CONDITION-HONESTY lens. The doc has converged on this lens:
the V6 REVISE is folded and no new one is inherited.

---

## Verdict

The close condition IS the honest generalization goalset on all four pillars: **ONE generator**
(`generator_grammar_count == 3`, `verbatim_blob_present == false`, un-forked emitter on the lowered
`BackendShape` with the `emit_shape_source` relocated-seam fourth conjunct closing §5-risk-1, the
structural R16 row-collapse the arm-grep cannot see); **>SOTA preserved with the lazy-rich framing
DISCLOSED** (the CLOSED enum + RED `undisclosed` default, the same-run ratio replacing the un-re-locked
S-P1 absolute, ≥1-regular-corpus mandatory, gate-before-speed, no vacuous absolute-keyed or
admit-relabeled close); **x86 gone** (falsifiable, reach-matched); **net LOC negative** (≈−10800,
falsifiable REDUCTION). The §6 fallback is binding and honest: a Sheets shim is outcome `N` (never
paper-closed, never `S`/`L`, no `_RS` blob), the named-primitive escape is machine-(a)-(d)-gated with
the god-kernel size bound and the FORCED CSS-scoped demotion, and the acceleration claim requires the
runtime-reachability conjunct. No unfalsifiable gate, no broken sequence, no addenda violation under
this lens.

The single prior-cycle (V6) REVISE — the H1 verbatim-G2-phrasing reuse — is FOLDED and re-verified on
disk; no residual REVISE remains and none is inherited. The lens is CLEAN this cycle.

TALLY accept=22 revise=0 reject=0
