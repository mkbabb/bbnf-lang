# SK-V18 S-P3 CHALLENGE — V6 / CH7 (CLOSE-CONDITION-HONESTY)

Lens: CLOSE-CONDITION-HONESTY. Is the close condition the honest generalization goalset (one
generator; >SOTA preserved with the lazy-rich framing disclosed; x86 gone; net LOC negative); and is
the §6 fallback binding (a parser that cannot be grammar-derived without a shim is surfaced honestly,
no `_RS` blob)? Adversarial reviewer, cycle V6 (post-V1–V5, all reject=0). Mandate: drive out the
RESIDUAL precision REVISEs toward a 2-consecutive-clean fixed point; catch any genuine REJECT; be
PROPORTIONATE.

Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1642 lines) against S-P2 sequencing
(`research/p2/SYNTHESIS-RESEARCH.md §3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`).

---

## Enumeration — every close/gate/telemetry claim under the lens

### Pillar 1 — ONE generator (close-cond #1, #9; restated §11)

- **#1 `generator_grammar_count == 3`** (json+css+sheets; defended against the P3 7-css inflation
  overfit R-A0-2). Gate REJECTs `!= 3`. Sheets is the genuine third grammar via a distinct
  `grammar_name="google_sheets"` row collapsing to itself under the R16 full-row `PartialEq`. The
  collapse-to-one vs differentiate decision is bound at P3 (§3.3), with minting fake `.bbnf` roots
  explicitly forbidden (the EXACT overfit addendum 2 names). **ACCEPT** — falsifiable, addenda-aligned.
- **#1 `verbatim_blob_present == false` campaign-wide** + the CSS const courier
  (`CSS_GENERATED_RS`) and JSON 7× `push_str` both RETIRED. Cleared by G1+G2, machine-checked by the
  `.bbnf`-mutation falsifier (not a line-count). **ACCEPT.**
- **#2 un-forked emitter** dispatched on `BackendShape`, not a grammar tag: `emitter_fork_present ==
  false ∧ emit_shape_source == lowered_program ∧ generator_grammar_branch_count == 0 ∧
  generator_grammar_type_count == 0`, with the relocated-seam structural co-gate
  `runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`, BOTH nested structs). The fifth
  conjunct (`emit_shape_source`) is the load-bearing defence against the §5-risk-1 relocated seam the
  arm-grep cannot see. **ACCEPT** — the un-fork close is structurally honest, not a relabel.

### Pillar 2 — >SOTA preserved with the lazy-rich framing DISCLOSED (close-cond #6, #12; §0.2; H1)

- **Lazy-rich framing disclosure is BINDING, not optional.** `materialization_framing` enum is CLOSED
  to `{lazy-rich-vs-eager-cssom | undisclosed}`; `undisclosed` is the pre-H1 default that turns the
  H1 gate RED; the gate REJECTs anything but the disclosed value AND any unqualified "beats CSSOM"
  re-label (R-A0-1/R14). The materialization-depth asymmetry must be disclosed EXPLICITLY at H1
  (`generated.rs:297-304` lazy-rich vs eager full CSSOM). **ACCEPT** — the framing-honesty hazard the
  addenda flag is properly closed; no honest-residual-disguised-as-admit path.
- **The close is NOT keyed on an un-re-locked absolute ratio.** The binding gate is the SAME-RUN
  falsifier `track1_rich/lightningcss > 1.0×` ∧ no regression vs the parser's OWN pre-G2 baseline,
  captured AT G2 ENTRY in ONE quiet run (so a uniform host-load depression cancels on both sides). The
  S-P1 absolute ratios (2.190 / 3.375 / 1.658 / 2.101) are explicitly DIRECTIONAL antecedents under
  loadavg 4.35, NOT the floor — keying the close on them is named as the unfalsifiable hazard the
  same-run comparison replaces. **ACCEPT** — this is the honest, load-robust close.
- **No vacuous close.** `g2_sota_ratio_held` PASS REQUIRES ≥1 REGULAR corpus (animate OR bootstrap)
  crossing > 1.0× with no regression; tailwindcss below 1.0× is an honest recorded residual, NOT
  tranche-blocking, provided a regular corpus holds; mcw/full-corpus regression reported honestly.
  **ACCEPT** — the hardest corpus is not paper-closed and a zero-crossing close is impossible.
- **JSON close**: 51/51 strict-vs-sonic-rs cold, same-plane, per-iter oracle, within ±1.0% of the
  PINNED `SK-V18-open` baseline across every wave. **ACCEPT** — honest, no framing asymmetry.
- **Outcome enum** honestly separates A (admit) from L (non-Sheets residual loss), N (negative-control
  fail), S (honest non-SOTA residual). A G6 NEON speedup CLAIM is `A` ONLY under the H1 quiet-bar
  (`host_loadavg < 1.0`), else `S`; a checkasm-only PASS is `C` (speedup deferred); a recognition-only
  `track1_full_parse` `A` does NOT discharge the typed >SOTA close. **ACCEPT** — no admit-relabel of a
  residual; the load caveat is enforced at the enum, not paper-closed.

  **REVISE — H1 `css_sota_ratio_held` regression-anchor phrasing (the close-bearing column).** The
  SPEC's own authoritative mechanics get the post-G2 measurement right: close-cond #6 (lines 108–113)
  states the regression falsifier `abs(ratio_postG2 − ratio_preG2)/ratio_preG2 ≤ noise band` **FIRES
  AT G2** and "the pre-G2 figure cannot be re-derived at H1 (the pre-G2 code is gone post-G2), so H1
  only RE-CONFIRMS the already-closed G2 close-ratio DIRECTIONALLY against the G2-recorded baseline,
  never re-measures pre-G2"; the column definition (line 264) and the comparator table (line 180)
  agree EXACTLY ("the G2-exit gate fires on and H1 re-confirms directionally — NOT re-measured at
  H1"). BUT the H1 exit-gate (line 1553), the H1 `css_sota_ratio_held` telemetry column (line 1573),
  and the G3 conjunct-8 column (line 1156) reuse the verbatim G2 phrasing "no **same-run** regression
  vs the pre-G2 baseline." At G3/H1 the pre-G2 code is GONE (G3 conjunct-6 makes CSS output
  byte-equivalent to the G2-closed file — PATH not OUTPUT — and H1 is later still), so a "same-run
  regression vs the pre-G2 baseline" measurement is the very thing #6 says cannot exist post-G2. An
  H1 implementer reading the close column would attempt an impossible same-run pre-G2 capture. This is
  a transcription looseness, not an unfalsifiable gate (the falsifiable anchor — the G2-RECORDED
  `track1_rich_over_lcss_ratio_pre_g2`, defined at line 264 — exists and the G2 gate where the
  falsifier FIRES is sound), so REVISE not REJECT.

  EXACT one-line edit (line 1573, the close-bearing H1 column; the G3 line-1156 twin and the H1
  exit-gate line-1553 twin fold identically):

  `css_sota_ratio_held                        (same-run track1_rich/lightningcss > 1.0× per corpus, re-confirmed DIRECTIONALLY against the G2-RECORDED track1_rich_over_lcss_ratio_pre_g2 baseline — the pre-G2 code is gone post-G2 so H1 never re-measures pre-G2 same-run, per close-cond #6; the S-P1 absolutes DIRECTIONAL, not the floor)`

### Pillar 3 — x86 gone (close-cond #7; P1)

- `x86_tree_deleted == true`; `find …/src/x86_64 …/ext/x86 -type f == 0` (today 28); crate-wide
  aarch64-neutral grep clean; `cargo build` + `cargo test --no-run` clean (the build-soundness
  coupling via the same-commit `checkasm_parity.rs` decouple). Gate REJECTs `x86_tree_deleted !=
  true`. **ACCEPT** — falsifiable, consistent, reach-matched to the verify grep.

### Pillar 4 — net LOC negative (close-cond §0.1; restated §11 "net ≈ −10800 LOC")

- The campaign net is consistently `≈ −10800` at lines 21–22, 61, 451, 1623, sourced from the addenda
  authority (§4: "Net LOC ≈ −10800 — deletes far more than the campaign adds"). The close goal is a
  falsifiable REDUCTION. **ACCEPT** on the close claim itself.
- **Minor precision note (NOT a REVISE, recorded for the fold).** Line 567–568 labels the PRUNE
  cluster's disk-truth net as "PRUNE net LOC ≈ −10800," reusing the campaign figure for the cluster
  alone; the cluster components (P1 ≈ −4500 + P2 ≈ −700 + P3 ≈ −5500 ≈ −10700) plus the campaign adds
  (G1–G6 ≤450 each + PROVE +200 + P4 +15) would make the cluster net and the campaign net distinct
  magnitudes. Both figures are approximate (`≈`) and the close condition keys on the campaign net
  (unambiguous), so this is not materially misleading to an implementer and stays sub-REVISE — but a
  fold could disambiguate "PRUNE net" vs "campaign net" at 567–568.

### §6 fallback binding — shim surfaced honestly, no `_RS` blob

- **Close-cond #9 carries the binding fallback explicitly:** "if Sheets cannot emit via the generator
  ONLY, generalization is NOT real — surface HONESTLY, do NOT stub-prove, do NOT hand-write a
  `_GENERATED_RS` Sheets block (§5-risk-5)." **ACCEPT.**
- **PROVE binds `sheets_emission_path == shim` to outcome `N`** (negative-control fail) everywhere
  (lines 213, 220–224, 1473, 1497, 1505–1506, 1514–1516) — never `S`, never `L`, never paper-closed;
  the gate does NOT paper-close a shim as a pass. The N-vs-L-vs-S distinction is internally consistent
  in the SPEC (the consolidated-doc §4 "outcome L" at line 135 is a stale prior-cycle artefact OUTSIDE
  my target; the SPEC itself is correct). **ACCEPT.**
- **The §6 named-primitive escape (the single largest paper-close surface, R-A0-3) is machine-(a)-(d)-
  gated, not prose-reviewed.** (a) grammar-INVOKED-by-name; (b) emitted-output VARIES under
  invoking-rule mutation (the byte-set/numeric-class mutation that distinguishes derived from
  relabeled); (c) `verbatim_blob_present == false`; (d) PROFILE-PROVEN-NARROW-LEAF with a concrete
  machine-check — `*_primitive_loc <= *_profiled_leaf_extent` (numerator/denominator columns for BOTH
  G1 leaves and the G2 `css_balanced_component_scan`), the god-kernel REJECT. **ACCEPT** — the
  fallback escape cannot admit an arbitrarily large relabeled blob; (d) bounds the size, (b) bounds the
  routing.
- **The CH6 FORCED demotion to `css_balanced_component_scan`** (s6/C4) is folded: the offered non-CSS
  dischargers (JSON `{}`/`[]`, Sheets `paren_expr`) are parse-with-emit descents structurally
  incompatible with the CSS byte-SKIP shell, so the neutral name is an overfit-in-waiting and the
  CSS-scoped name is FORCED; the gate REJECTs a neutral name with zero structurally-compatible non-CSS
  caller, discharged by the rename, NOT a fabricated caller. **ACCEPT** — honest, addenda-aligned.
- **Acceleration-at-admission fallback honesty:** `acceleration_at_admission == admission` REQUIRES
  BOTH the generated-`generated.rs` caller census AND `simd_admission_profile_sampled == true` (the
  runtime-reachability conjunct); a census hit in dead/unreachable code that the profile does NOT see
  == `dead`, not `admission`. The enum is the SAME two-value domain §0.3 and §8/G6 decide on. **ACCEPT**
  — no source-census-only paper-close.

---

## Sequencing / addenda cross-checks under the lens

- The close-condition lattice (§2.1) matches S-P2 §3: PRUNE → G1 → G2 → G3 → {G4 → PROVE ∥ G5/G6} →
  H1; PROVE entry is the explicit conjunction `G3 ∧ G4` (G4 a DIRECT predecessor, seq/C6), G5/G6 hangs
  off G3 PARALLEL to G4 (seq/C7). The two parallel branches join at H1. **ACCEPT** — the close gates
  consume a sound, acyclic DAG; no close claim hangs off a broken sequence.
- Every close-bearing telemetry column is consumed in a named wave slice (no producer-only field), per
  `[typed-materialization-invariant]`. **ACCEPT.**
- No addenda violation surfaced under the lens: verbatim-blob, distinct-output (3-co-gate), single-
  emitter, phantom-generic, corpus-in-timer, acceleration-wiring all bind THREE ways (close-gate +
  pre-block + REJECT column). **ACCEPT.**

---

## Verdict

The close condition IS the honest generalization goalset on all four pillars: ONE generator
(`generator_grammar_count == 3`, `verbatim_blob_present == false`, un-forked emitter on the lowered
`BackendShape`); >SOTA preserved with the lazy-rich framing DISCLOSED (the closed enum + RED default,
the same-run ratio replacing the un-re-locked absolute, ≥1-regular-corpus mandatory, no vacuous or
admit-relabeled close); x86 gone (falsifiable, reach-matched); net LOC negative (campaign ≈ −10800,
falsifiable REDUCTION). The §6 fallback is binding and honest: a shim is outcome `N` (never paper-
closed), no `_RS` blob, and the named-primitive escape is machine-(a)-(d)-gated with the god-kernel
size bound and the FORCED CSS-scoped demotion.

ONE residual precision REVISE: the H1 close-bearing `css_sota_ratio_held` column (line 1573, with the
G3 line-1156 and H1 exit-gate line-1553 twins) reuses the verbatim G2 "same-run regression vs the
pre-G2 baseline" phrasing at a plane where the pre-G2 code is gone — contradicting the SPEC's own
close-cond #6 / line-264 / line-180 "H1 re-confirms DIRECTIONALLY, never re-measures pre-G2." The
falsifiable anchor (the G2-recorded baseline) exists, so this is a misleading-phrasing REVISE, not an
unfalsifiable-gate REJECT. No REJECT.

TALLY accept=17 revise=1 reject=0
