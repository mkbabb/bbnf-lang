# SK-V18 S-P3 CHALLENGE — V8 / CH7 (CLOSE-CONDITION-HONESTY lens)

Cycle V8 (eighth hardening cycle). Lens: CLOSE-CONDITION-HONESTY — is the close condition the honest
generalization goalset (one generator; >SOTA preserved with the lazy-rich framing disclosed; x86 gone;
net LOC negative), and is the §6 fallback binding (a parser that cannot be grammar-derived without a shim
surfaces honestly, no `_RS` blob)?

Read against S-P2 sequencing (`research/p2/SYNTHESIS-RESEARCH.md §3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1/§5/§6`). Prior cycles V6 (revise=1: the H1
verbatim-G2-phrasing reuse) and V7 (revise=0). The V6 REVISE is verified FOLDED on disk this pass — see
the directional re-confirm cross-check below. SPEC is 1661 lines; every locus re-grepped at the cited
line. PROPORTIONATE: a wording nit is a REVISE only if it would mislead an implementer.

---

## Enumeration of every close/wave-gate/telemetry claim under the lens

### Pillar 1 — ONE generator (the generalization core)

- **Close-cond #1 (line 63-69):** `generator_grammar_count == 3` (json+css+sheets, NOT the P3-collapse 7),
  CSS courier + JSON 7× `push_str` RETIRED, `verbatim_blob_present == false` campaign-wide. Falsifiable
  (md5-distinct + `.bbnf`-mutation + grep). **ACCEPT.**
- **Close-cond #2/#3 (line 71-85):** un-forked emitter — `emitter_fork_present == false`,
  `emit_shape_source == lowered_program`, `generator_grammar_branch_count/type_count == 0`,
  `runtime_target_rows_collapsed == true`. The `emit_shape_source` fourth conjunct (G3.2 line 1114-1121)
  closes §5-risk-1; the R16 full-row `PartialEq` structural co-gate (line 668-679) catches the relocated
  seam the arm-grep cannot see. Field-set IDENTICAL to the G3 exit conjunct-5 grep (line 343/501). All
  falsifiable. **ACCEPT.**
- **Close restatement (line 1630-1632):** carries `verbatim_blob_present==false`,
  `generator_grammar_count==3`, `emitter_fork_present==false`, `emit_shape_source==lowered_program`,
  `runtime_target_rows_collapsed==true`. Consistent with §0.1. **ACCEPT.**
- **Sequence (S-P2 §3 line 137):** G3 entry = G1 ∧ G2 ∧ P4 live ∧ P3 row-collapse — matches SPEC §2.1
  lattice (line 542). Acyclic DAG; no close hangs off a broken sequence. **ACCEPT.**

### Pillar 2 — >SOTA preserved with the lazy-rich framing DISCLOSED

- **Close-cond #6 (line 105-128):** the binding gate is the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧
  no same-run regression vs the pre-G2 baseline CAPTURED-AT-G2-ENTRY; the S-P1 absolutes are DIRECTIONAL
  antecedents, NOT the floor (keying on an un-re-locked absolute is the unfalsifiable hazard the same-run
  comparison REPLACES). Gate-before-speed (9-field oracle / 51-row). `≥1 regular corpus (animate OR
  bootstrap) crossing is mandatory` (line 118). NOT vacuous, NOT absolute-keyed. **ACCEPT.**
- **§0.2 comparator table (line 181-185) + CSS framing honesty (line 187-195):** the bar is
  `lazy-rich-vs-eager-cssom`; an unqualified "beats CSSOM" behind a re-label WITHOUT the
  materialization-depth asymmetry disclosed is a REJECT (R-A0-1). Closed enum
  `{lazy-rich-vs-eager-cssom|undisclosed}` with `undisclosed` the RED default (line 256, 1585, 1594).
  The re-label branch admissible ONLY with the asymmetry disclosed. **ACCEPT** — the lazy-rich asymmetry
  is disclosed, not buried; matches addendum 5 / R14 / S-P2 §5-risk-7.
- **Directional re-confirm cross-check (the FOLDED V6 REVISE).** The G3 telemetry (line 1165), H1
  exit-gate (line 1568-1571), and H1 telemetry `css_sota_ratio_held` (line 1591) now all read
  "re-confirmed DIRECTIONALLY against the G2-RECORDED baseline ... the pre-G2 code is gone post-G2 so H1
  never re-measures pre-G2 same-run, per close-cond #6." This matches §0.2 line 181 ("H1 only re-confirms
  the G2-recorded ratio directionally") and the §0.4 schema note line 265. The V6 verbatim-G2-phrasing
  defect is FOLDED. **ACCEPT** — falsifiable anchor (the G2-recorded baseline) preserved, no plane
  contradiction remains.
- **Load-robustness (line 200-205) + outcome enum (line 207-234):** a corpus-in-timer figure under
  `host_loadavg >= 1.0` (or unstamped) is outcome `S` (directional honest residual), NOT `A`; a checkasm
  PASS with no corpus-in-timer figure is `C`, never `A`; a recognition-only `A` does NOT discharge the
  typed close. The honesty of the >SOTA CLAIM is gated, not asserted. **ACCEPT.**
- **G2 / H1 gate consumption (line 1035, 1568, 1591):** PASS REQUIRES ≥1 REGULAR corpus crossing;
  tailwind below 1.0× is an honest residual recorded, NOT tranche-blocking; mcw/full-corpus regression
  reported honestly. The gate is binding AND honest about its tolerated residual. **ACCEPT.**

### Pillar 3 — x86 gone (aarch64-only)

- **Close-cond #7 (line 130-133):** BOTH x86 surfaces + nasm driver DELETED; `find …/src/x86_64
  …/ext/x86 -type f == 0`; aarch64-neutral grep clean; `cargo build` + `cargo test --no-run` clean.
  P1 (§3.1 line 596-606) reach-matches the deletion list to the verify grep (the V5 reach hazard) and
  binds the same-commit `checkasm_parity.rs` decouple (build-soundness). Falsifiable, reach-matched.
  **ACCEPT.**

### Pillar 4 — net LOC negative

- **Close-cond (line 22, 61, 454, 571, 1640-1641):** net `≈ −10800` everywhere, the `≈` an honest
  approximation, load-bearingly NEGATIVE. Wave deletions (P1 ≈−4500, P2 ≈−700, P3 ≈−5500; G-wave adds
  ≈0) sum within the `≈`. Falsifiable REDUCTION; `[generated-size-budget]` no-overflow. **ACCEPT**
  (V7-disposed; re-confirmed).

### The §6 fallback binding (the negative-control teeth)

- **Close-cond #9 (line 143-152):** Sheets emits THROUGH the un-forked G3 generator ONLY;
  `sheets_grammar_shape == pratt-operator` (non-hollow, machine-checked ≥7 chained level fns + cyclic
  back-edge, line 1461-1467); md5-DISTINCT; no `const.*_RS.*r#` Sheets blob. **BINDING FALLBACK:** if
  Sheets cannot emit via the generator ONLY → surface HONESTLY, do NOT stub-prove, do NOT hand-write a
  `_GENERATED_RS` Sheets block. **ACCEPT.**
- **Outcome enum (line 214, 221-225) + PROVE exit/falsifiers (line 1485-1530):** a shim maps to
  `sheets_emission_path == shim` → outcome `N` (generalization NOT real), surfaced honestly, NEVER `S`,
  NEVER `L`, NEVER paper-closed; the gate does NOT paper-close a shim as a pass. The route ledger
  (line 1624) pre-blocks the `const SHEETS_GENERATED_RS` courier and the stub-prove. Consistent
  three ways. **ACCEPT** — the §6 fallback is binding and honest.
- **Sequence (S-P2 §3 line 140-141, seq/C6):** PROVE entry = `G3 ∧ G4` explicit conjunction, G4 a
  DIRECT (not merely transitive) predecessor; G5/G6 hangs off G3 PARALLEL to G4 (seq/C7); the two join
  at H1. Matches SPEC §2.1 lattice (line 544-546) and PROVE entry (line 1427-1436). No broken sequence
  under the lens. **ACCEPT.**
- **The named-primitive §6 escape ((a)-(d) gate, line 358-379):** machine-checked grammar-INVOKED +
  output-VARIES-under-mutation + `verbatim_blob_present==false` + PROFILE-PROVEN-NARROW-LEAF (god-kernel
  size bound, primitive LOC ≤ profiled extent). The FORCED CSS-scoped demotion to
  `css_balanced_component_scan` (s6/C4, line 381-393) is the honest outcome — a neutrally-named CSS-only
  primitive is overfit-in-waiting. **ACCEPT.**
- **NEON at admission (close-cond #10, line 154-165; §8 exit line 1345-1357):**
  `acceleration_at_admission == admission` REQUIRES BOTH conjuncts — the `generated.rs` caller census AND
  `simd_admission_profile_sampled == true` (runtime-reachability, non-zero self-time); a source-census-only
  PASS is REJECT. The honesty of the acceleration claim is gated, not asserted. **ACCEPT.**

### Cross-cutting

- Every close-bearing telemetry column is consumed in a named wave slice (§0.4 line 275-291; per-wave
  REJECT lists) — no producer-only field, per `[typed-materialization-invariant]`. **ACCEPT.**
- No addenda violation surfaced under the lens; all six bind THREE ways (close-gate + §0.4 pre-block +
  REJECT column). **ACCEPT.**

---

## The one residual precision REVISE

**REVISE — §11 close-condition restatement (line 1635): the >SOTA pillar drops the binding
`≥1 regular corpus (animate OR bootstrap)` quantifier the canonical close carries.**

Line 1635 reads ">SOTA preserved honestly (CSS same-run `track1_rich/lcss > 1.0×` with no same-run
regression vs the pre-G2 baseline ...)". Every other close-bearing locus — close-cond #6 (line 118,
"≥1 regular corpus crossing is mandatory"), §0.5 tranche-success (line 317, "≥1 regular corpus
mandatory"), the G2 gate column (line 1035, "PASS REQUIRES >= 1 REGULAR corpus ... tailwindcss below
1.0× is an honest residual recorded, NOT tranche-blocking"), and the H1 `css_sota_ratio_held` column
(line 1591, "on ≥1 regular corpus (animate OR bootstrap)") — carries the per-corpus quantifier AND the
tailwind-residual tolerance. The restatement is the document's single-paragraph crystallization of the
close (the locus an orchestrator quotes for "did SK-V18 close?"). Dropping the quantifier there makes
"CSS same-run `track1_rich/lcss > 1.0×`" read as an ALL-FOUR-corpora requirement, which would WRONGLY
treat a tailwind miss as close-blocking — directly contradicting lines 306/1035/1591 that record a
tailwind miss as a tolerated honest residual. This misleads in the over-strict direction at the
highest-visibility close locus.

This is a REVISE, not a REJECT: the binding gates (§0.1 #6, §0.5, G2 line 1035, H1 line 1591) are sound
and falsifiable; only the recap omits the quantifier.

EXACT one-line edit (line 1635), insert the quantifier so the restatement matches the binding form:

> the >SOTA preserved honestly (CSS same-run `track1_rich/lcss > 1.0×` on ≥1 regular corpus (animate OR
> bootstrap) with no same-run regression vs the pre-G2 baseline, cold corpus-in-timer — the S-P1
> absolutes DIRECTIONAL, not the binding floor; a tailwind miss is a tolerated honest residual; JSON
> 51/51 strict-vs-sonic-rs; `materialization_framing==lazy-rich-vs-eager-cssom` disclosed),

---

## Verdict

The close condition IS the honest generalization goalset on all four pillars: **ONE generator**
(`generator_grammar_count==3`, `verbatim_blob_present==false`, un-forked emitter on the lowered
`BackendShape` with the `emit_shape_source` relocated-seam fourth conjunct and the structural R16
row-collapse); **>SOTA preserved with the lazy-rich framing DISCLOSED** (the CLOSED enum + RED
`undisclosed` default, the same-run ratio replacing the un-re-locked absolute, gate-before-speed, the
FOLDED V6 directional-re-confirm phrasing verified on disk); **x86 gone** (falsifiable, reach-matched);
**net LOC negative** (≈−10800, falsifiable REDUCTION). The §6 fallback is binding and honest: a Sheets
shim is outcome `N` (never paper-closed, never `S`/`L`, no `_RS` blob), the named-primitive escape is
machine-(a)-(d)-gated with the god-kernel size bound and the FORCED CSS-scoped demotion, and the NEON
claim requires the runtime-reachability conjunct. No unfalsifiable gate, no broken sequence, no addenda
violation under this lens.

ONE residual precision REVISE remains: the §11 close-restatement (line 1635) drops the binding
`≥1 regular corpus` quantifier the canonical close carries — a misleading over-strict omission at the
crystallized close locus, fixable by a one-clause insertion. The V6 REVISE is FOLDED and re-verified; no
REJECT surfaces. This is the residual the fixed-point requires driving out before the next cycle can
certify clean.

TALLY accept=24 revise=1 reject=0
