# SK-V18 S-P3 CHALLENGE — V9 / CH7 (CLOSE-CONDITION-HONESTY lens)

Cycle V9 (ninth hardening cycle). Lens: CLOSE-CONDITION-HONESTY — is the close condition the honest
generalization goalset (ONE generator; >SOTA preserved with the lazy-rich framing DISCLOSED; x86 gone;
net LOC negative), and is the §6 fallback binding (a parser that cannot be grammar-derived without a
shim surfaces honestly, no `_RS` blob)?

Read against S-P2 sequencing (`research/p2/SYNTHESIS-RESEARCH.md §3`) and the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1/§5/§6`). Prior cycles under this lens: V6 (revise=1: the
H1/G3 directional-re-confirm phrasing) → V7 (revise=0) → V8 (revise=1: the §11 close-restatement drops
the `≥1 regular corpus` quantifier). The trajectory is NOT yet 2-consecutive-clean (V7 clean, V8 not).
The V8 REVISE was re-verified on disk this pass: it is NOT folded — line 1635 is byte-unchanged and the
SPEC is untracked (`git status` `?? SPEC.md`, no fold-commit between V8 and now). SPEC is 1661 lines;
every locus re-grepped at the cited line. PROPORTIONATE: a wording nit is a REVISE only if it would
mislead an implementer at a close-bearing locus.

---

## Enumeration of every close/wave-gate/telemetry claim under the lens

### Pillar 1 — ONE generator (the generalization core)

- **Close-cond #1 (lines 63-69):** `generator_grammar_count == 3` (json+css+sheets, NOT the P3-collapse
  7-css inflation R-A0-2); the CSS const courier (`CSS_GENERATED_RS`) + JSON 7× `push_str` RETIRED;
  `verbatim_blob_present == false` campaign-wide. Falsifiable (md5-distinct + `.bbnf`-mutation + grep),
  addenda-2-aligned. **ACCEPT.**
- **Close-cond #2/#3 (lines 71-85):** un-forked emitter — `emitter_fork_present == false`,
  `emit_shape_source == lowered_program`, `generator_grammar_branch_count/type_count == 0`,
  `runtime_target_rows_collapsed == true`. The `emit_shape_source` FOURTH conjunct (G3.2 lines
  1114-1121) closes §5-risk-1 — without it the relocated seam riding the neutral per-profile columns
  passes conjuncts 1-4 under a green gate. The R16 full-row `PartialEq` structural co-gate (lines
  668-679, both nested structs) catches the seam the arm-grep is syntactically blind to. The standing
  seam field-set (line 343/501) is IDENTICAL to the G3 exit conjunct-5 grep. All falsifiable. **ACCEPT.**
- **Close restatement (lines 1630-1634), pillar-1 clauses:** carries `verbatim_blob_present==false`,
  `generator_grammar_count==3`, `emitter_fork_present==false`, `emit_shape_source==lowered_program`,
  `runtime_target_rows_collapsed==true`, `json_rich_navigation_preserved==true`,
  `phantom_generic_resolved==deleted` — consistent with §0.1. **ACCEPT.**
- **Sequence (S-P2 §3 line 137; SPEC §2.1 lattice line 542):** G3 entry = G1 ∧ G2 ∧ P4 live ∧ P3
  row-collapse. Acyclic DAG; no close hangs off a broken sequence. **ACCEPT.**

### Pillar 2 — >SOTA preserved with the lazy-rich framing DISCLOSED

- **Close-cond #6 (lines 105-128):** binding gate = SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no
  same-run regression vs the pre-G2 baseline CAPTURED-AT-G2-ENTRY in one quiet run; the regression
  falsifier FIRES AT G2 and H1 only RE-CONFIRMS directionally (the pre-G2 code is gone post-G2). The
  S-P1 absolutes (2.190/3.375/1.658/2.101) are DIRECTIONAL antecedents, NOT the floor — keying on an
  un-re-locked absolute is named as the unfalsifiable hazard the same-run comparison REPLACES.
  Gate-before-speed (9-field oracle / 51-row). `≥1 regular corpus (animate OR bootstrap) crossing is
  mandatory` (line 118). NOT vacuous, NOT absolute-keyed. **ACCEPT.**
- **§0.2 comparator table (lines 181-185) + CSS framing honesty (lines 187-195):** the bar is
  `lazy-rich-vs-eager-cssom`; an unqualified "beats CSSOM" behind a re-label WITHOUT the
  materialization-depth asymmetry disclosed is a REJECT (R-A0-1). Closed enum
  `{lazy-rich-vs-eager-cssom|undisclosed}` with `undisclosed` the RED pre-H1 default (lines 256, 1585,
  1594). **ACCEPT** — the lazy-rich asymmetry is disclosed, not buried; matches addendum 5 / R14 /
  §5-risk-7.
- **Directional re-confirm phrasing (the FOLDED V6 REVISE), re-verified.** G3 telemetry (line 1165), H1
  exit-gate (lines 1568-1571), and H1 `css_sota_ratio_held` (line 1591) all read "re-confirmed
  DIRECTIONALLY against the G2-RECORDED baseline ... the pre-G2 code is gone post-G2 so H1 never
  re-measures pre-G2 same-run, per close-cond #6." Matches §0.2 (line 181) and §0.4 (line 265). The V6
  defect is FOLDED and holds on disk. **ACCEPT.**
- **Load-robustness (lines 200-205) + outcome enum (lines 207-234):** a corpus-in-timer figure under
  `host_loadavg >= 1.0` (or unstamped) is outcome `S` (directional honest residual), NOT `A`; a checkasm
  PASS with no corpus-in-timer figure is `C`, never `A`; a recognition-only `track1_full_parse` `A` does
  NOT discharge the typed close. The honesty of the >SOTA CLAIM is gated, not asserted. **ACCEPT.**
- **G2 / H1 gate consumption (lines 1035, 1568, 1591):** PASS REQUIRES ≥1 REGULAR corpus crossing;
  tailwind below 1.0× is an honest residual recorded, NOT tranche-blocking; mcw/full-corpus regression
  reported honestly. The gate is binding AND honest about its tolerated residual. **ACCEPT.**
- **Close-cond #12 / PASS-IMPL (lines 171-174):** the close audit accepts every axis OR records a
  row-level intrinsic-block proof WITH measurement; the H1 framing honesty is disclosed
  (`materialization_framing == lazy-rich-vs-eager-cssom`). No paper-close path. **ACCEPT.**

### Pillar 3 — x86 gone (aarch64-only)

- **Close-cond #7 (lines 130-133):** BOTH x86 surfaces + nasm driver DELETED; `find …/src/x86_64
  …/ext/x86 -type f == 0`; aarch64-neutral grep clean; `cargo build` + `cargo test --no-run` clean. P1
  (§3.1 lines 596-606) reach-matches the deletion list to the verify grep (the V5 reach hazard) and
  binds the same-commit `checkasm_parity.rs` decouple (build-soundness). Falsifiable, reach-matched.
  **ACCEPT.**

### Pillar 4 — net LOC negative

- **Close-cond (lines 22, 61, 454, 1640-1641):** net `≈ −10800` campaign-wide, the `≈` an honest
  approximation, load-bearingly NEGATIVE; `[generated-size-budget]` no-overflow. Falsifiable REDUCTION.
  **ACCEPT.** (V6 recorded a sub-REVISE that line 570-571 labels the PRUNE-cluster disk-truth net "PRUNE
  net LOC ≈ −10800," reusing the campaign figure for the cluster; both are `≈` and the close keys on the
  unambiguous campaign net, so it stays sub-REVISE — re-confirmed sub-REVISE this pass, NOT inflated.)

### The §6 fallback binding (the negative-control teeth)

- **Close-cond #9 (lines 143-152):** Sheets emits THROUGH the un-forked G3 generator ONLY;
  `sheets_grammar_shape == pratt-operator` (non-hollow, machine-checked ≥7 chained level fns + cyclic
  back-edge, lines 1461-1467); md5-DISTINCT; no `const.*_RS.*r#` Sheets blob. **BINDING FALLBACK:** if
  Sheets cannot emit via the generator ONLY → surface HONESTLY, do NOT stub-prove, do NOT hand-write a
  `_GENERATED_RS` Sheets block. **ACCEPT.**
- **Outcome enum (lines 213-225) + PROVE exit/falsifiers (lines 1485-1530):** a shim maps to
  `sheets_emission_path == shim` → outcome `N` (generalization NOT real), surfaced honestly, NEVER `S`,
  NEVER `L`, NEVER paper-closed; the gate does NOT paper-close a shim as a pass (line 1519). The route
  ledger (line 1624) pre-blocks the `const SHEETS_GENERATED_RS` courier and the stub-prove. Consistent
  three ways. **ACCEPT** — the §6 fallback is binding and honest.
- **Sequence (S-P2 §3 lines 140-141, seq/C6+C7):** PROVE entry = `G3 ∧ G4` explicit conjunction, G4 a
  DIRECT (not merely transitive) predecessor (line 1427-1436); G5/G6 hangs off G3 PARALLEL to G4
  (seq/C7, line 1311); the two join at H1. Matches SPEC §2.1 lattice (lines 544-546). No broken sequence
  under the lens. **ACCEPT.**
- **The named-primitive §6 escape ((a)-(d) gate, lines 358-379):** machine-checked grammar-INVOKED +
  output-VARIES-under-mutation + `verbatim_blob_present==false` + PROFILE-PROVEN-NARROW-LEAF (god-kernel
  size bound, primitive LOC ≤ profiled extent, columns at 882-884/1030-1031). The FORCED CSS-scoped
  demotion to `css_balanced_component_scan` (s6/C4, lines 381-393) is the honest outcome — the offered
  non-CSS dischargers (JSON `{}`/`[]`, Sheets `paren_expr`) are parse-with-emit descents structurally
  incompatible with the CSS byte-SKIP shell, so the neutral name is overfit-in-waiting and the
  CSS-scoped name is FORCED; the gate REJECTs a neutral name with zero structurally-compatible non-CSS
  caller, discharged by the rename, NOT a fabricated caller. **ACCEPT.**
- **NEON at admission (close-cond #10, lines 154-165; §8 exit lines 1345-1357):**
  `acceleration_at_admission == admission` REQUIRES BOTH conjuncts — the `generated.rs` caller census
  AND `simd_admission_profile_sampled == true` (runtime-reachability, non-zero self-time); a
  source-census-only PASS (a census hit in dead/unreachable code the profile does NOT see) == `dead` ==
  REJECT. The enum is the SAME two-value domain §0.3 and §8/G6 decide on. **ACCEPT** — no
  source-census-only paper-close.

### Cross-cutting

- Every close-bearing telemetry column is consumed in a named wave slice (§0.4 lines 275-291; per-wave
  REJECT lists) — no producer-only field, per `[typed-materialization-invariant]`. **ACCEPT.**
- No addenda violation surfaced under the lens; all six bind THREE ways (close-gate + §0.4 pre-block +
  REJECT column). **ACCEPT.**

---

## The one residual precision REVISE (carried from V8, re-verified UNFOLDED on disk)

**REVISE — §11 close-condition restatement (line 1635): the >SOTA pillar drops the binding
`≥1 regular corpus (animate OR bootstrap)` quantifier the canonical close carries.**

Line 1635 reads: "the >SOTA preserved honestly (CSS same-run `track1_rich/lcss > 1.0×` with no same-run
regression vs the pre-G2 baseline, cold corpus-in-timer — the S-P1 absolutes DIRECTIONAL, not the
binding floor; JSON 51/51 strict-vs-sonic-rs; `materialization_framing==lazy-rich-vs-eager-cssom`
disclosed)". Every OTHER close-bearing locus carries the per-corpus quantifier AND the tailwind-residual
tolerance: close-cond #6 (line 118, "≥1 regular corpus (animate OR bootstrap) crossing is mandatory"),
§0.5 tranche-success (line 317, "≥1 regular corpus mandatory"), the G2 gate column (line 1035, "PASS
REQUIRES >= 1 REGULAR corpus ... tailwindcss below 1.0× is an honest residual recorded, NOT
tranche-blocking"), and the H1 `css_sota_ratio_held` column (line 1591, "on ≥1 regular corpus (animate
OR bootstrap) ... a tailwind miss recorded as an honest residual at G2 is re-confirmed as a residual
here NOT re-litigated as an H1 block"). The §11 restatement is the document's single-paragraph
crystallization of the close — the locus an orchestrator quotes for "did SK-V18 close?". Dropping the
quantifier there makes "CSS same-run `track1_rich/lcss > 1.0×`" read as an ALL-FOUR-corpora requirement,
which would WRONGLY treat a tailwind miss as close-blocking, directly contradicting lines
118/306/317/1035/1591 that record a tailwind miss as a tolerated honest residual. This misleads in the
over-strict direction at the highest-visibility close locus — a material implementer-misleading defect,
not a cosmetic nit.

This is a REVISE, not a REJECT: the binding gates (§0.1 #6, §0.5, G2 line 1035, H1 line 1591) are sound
and falsifiable; only the §11 recap omits the quantifier. It is the SAME residual V8 named, re-verified
unfolded on disk (line 1635 byte-unchanged; SPEC untracked, no intervening fold-commit) — so the fixed
point is NOT yet reached and this cycle cannot certify clean.

EXACT one-line edit (line 1635), insert the quantifier + the residual-tolerance clause so the
restatement matches the binding form at lines 118/317/1035/1591:

> the >SOTA preserved honestly (CSS same-run `track1_rich/lcss > 1.0×` on ≥1 regular corpus (animate OR
> bootstrap) with no same-run regression vs the pre-G2 baseline, cold corpus-in-timer — the S-P1
> absolutes DIRECTIONAL, not the binding floor; a tailwind miss is a tolerated honest residual; JSON
> 51/51 strict-vs-sonic-rs; `materialization_framing==lazy-rich-vs-eager-cssom` disclosed),

---

## Verdict

The close condition IS the honest generalization goalset on all four pillars: **ONE generator**
(`generator_grammar_count==3`, `verbatim_blob_present==false`, un-forked emitter on the lowered
`BackendShape` with the `emit_shape_source` relocated-seam fourth conjunct and the structural R16
full-row `PartialEq` row-collapse over BOTH nested structs); **>SOTA preserved with the lazy-rich
framing DISCLOSED** (the CLOSED enum + RED `undisclosed` default, the same-run ratio replacing the
un-re-locked absolute, gate-before-speed, ≥1-regular-corpus mandatory at every binding gate, the FOLDED
V6 directional-re-confirm phrasing re-verified); **x86 gone** (falsifiable, reach-matched, build-sound);
**net LOC negative** (campaign ≈−10800, falsifiable REDUCTION). The §6 fallback is binding and honest: a
Sheets shim is outcome `N` (never paper-closed, never `S`/`L`, no `_RS` blob), the named-primitive
escape is machine-(a)-(d)-gated with the god-kernel size bound and the FORCED CSS-scoped demotion, and
the NEON claim requires the runtime-reachability conjunct. No unfalsifiable gate, no broken sequence, no
addenda violation under this lens — no REJECT.

ONE residual precision REVISE remains: the §11 close-restatement (line 1635) drops the binding
`≥1 regular corpus (animate OR bootstrap)` quantifier the canonical close carries — the SAME defect V8
named, re-verified UNFOLDED on disk, a misleading over-strict omission at the crystallized close locus,
fixable by a one-clause insertion. Because this carried-forward REVISE is still live, the 2-consecutive-
clean fixed point is NOT reached; this is the residual the fixed point requires driving out before the
next cycle can certify clean.

TALLY accept=25 revise=1 reject=0
