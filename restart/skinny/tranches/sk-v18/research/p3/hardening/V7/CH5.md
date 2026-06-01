# SK-V18 S-P3 CHALLENGE — CH5 SOTA-PRESERVATION (cycle V7)

Lens: is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer, the per-corpus floors), the
hot leaves preserved as gated primitives, and does NO wave regress the 94.1%/91.5% leaves silently?
Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1656 lines) against S-P2 §3 sequencing + the S-P0
addenda. Posture: PROPORTIONATE residual-precision drive-out toward the 2-consecutive-clean fixed
point; a wording nit is a REVISE only if it would mislead an implementer. Prior cycles reject=0; the
V6 CH5 REVISE (R-CH5-1, the PROVE `json_css_preservation_held` column) is VERIFIED FOLDED this pass
(§9 line 1507 + REJECT clause line 1515). Every claim below re-grounded on disk at the cited lines.

---

## Enumeration — every wave-gate / telemetry / close claim under the SOTA-PRESERVATION lens

### Primary question: the >SOTA-regression gate is bound at BOTH G2 AND G6 — ACCEPT

- **G2 explicit >SOTA-regression gate, distinct from parity (§5 G2.2 conjunct 4, lines 957–967;
  telemetry `g2_sota_ratio_held` line 1032; close-cond #6 lines 104–128; §0.2 line 180; §0.5 lines
  302–307).** The binding falsifier is the SAME-RUN `track1_rich/lightningcss > 1.0×` per corpus AND
  no same-run regression vs the parser's OWN pre-G2 baseline `track1_rich_over_lcss_ratio_pre_g2`
  captured AT G2 ENTRY in one quiet run, on `css_canon_bench` cold corpus-in-timer, parity
  gate-BEFORE-speed. The S-P1 absolutes (2.190/3.375/1.658/2.101) are correctly DIRECTIONAL
  antecedents, NOT the floor — the same-run two-figures-one-plane construction defuses the
  load-depressed-absolute unfalsifiable hazard. "Oracle parity does NOT prove throughput; the bench
  re-measure is the binding regression falsifier" is explicit (lines 965–966). The pre-G2 baseline
  captured at G2-entry (un-re-derivable post-G2) is the sound resolution of "where does the floor
  come from after the old code is gone." ACCEPT.
- **G6 >SOTA binding (§8 exit gate lines 1341–1363; §0.3 outcome rules lines 224–228; telemetry
  `g6_speedup_median_mbps`/`corpus_in_timer` lines 1384, 254).** Correctly SPLIT: the checkasm
  differential + `neon_significant_skip_matches_scalar` over the REAL 71KB–495KB corpora is a
  CORRECTNESS plane only; any Mbps/speedup FIGURE comes ONLY from the corpus-in-timer symmetric
  `css_canon_bench` harness; the speedup CLAIM is DEFERRED to the H1 symmetric timer
  (`g6_speedup_median_mbps` null pre-H1; G6 outcome `C` until H1). A figure off the checkasm plane is
  an addendum-5 plane-mismatch REJECT (lines 1371–1372, 1394–1396). The acceleration-at-admission gate
  requires BOTH conjuncts (the `generated.rs` non-`#[cfg(test)]` caller census AND
  `simd_admission_profile_sampled == true` with non-zero self-time), with a falsifiable RED predicate
  `self_time_samples == 0` (lines 1349–1351), the enum closed to the two-value domain. ACCEPT.
- **Per-corpus floors, not a corpus-average (§0.5 lines 294–307; §0.4 broadcast guard lines 289–290;
  H1 route ledger line 1621).** The CSS bar is per-corpus with ≥1 REGULAR corpus (animate OR
  bootstrap) mandatory; tailwindcss below 1.0× is an honest residual not tranche-blocking provided a
  regular holds; mcw is the full-corpus integration check. `gate-json` REJECTs any single-tuple
  broadcast (`sample_count==1` or one tuple across corpus rows). A corpus-average for per-corpus
  ratios is a pre-blocked H1 route. ACCEPT — per-corpus and broadcast-proof.

### Hot leaves preserved as gated primitives — ACCEPT

- **CSS 94.1% leaf as `css_balanced_component_scan` (§1 (a)-(d) gate lines 357–392; §5 G2.3 lines
  981–1000; telemetry `g2_balanced_scan_primitive_abcd_pass`/`_loc`/`_profiled_leaf_extent` lines
  1026–1028).** (a) grammar-INVOKED, (b) ARG-VARIES under invoking-rule mutation, (c)
  `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF with the machine-checked
  `primitive_loc <= profiled_leaf_extent` god-kernel REJECT. The FORCED CSS-scoped demotion (s6/C4,
  both non-CSS dischargers grounded as byte-SKIP-vs-parse-with-emit incompatible) is honest. The same
  primitive is the SINGLE G6 NEON-retarget call site (one seam for G2+G6) — the 94.1% leaf preserved
  as a gated primitive AND accelerated at the same site. ACCEPT.
- **JSON 91.5% leaf preserved (§4 G1.2 conjunct 3 lines 839–845; G1.3 leaf primitives lines 853–868;
  `g1_hot_leaf_preserved`/`g1_dispatch_triple_not_lcd_collapsed` lines 875–876).** The
  `parse_object_value_at_direct`/`parse_array_element_at_direct` bodies re-emitted byte-equivalent
  (identical inline cfg + `sink.*` call sites) with the explicit "do NOT LCD-collapse the
  value/object/array dispatch triple" guard. The two leaf primitives carry the same (a)-(d) gate with
  the (d) `loc <= profiled_leaf_extent` machine-check. ACCEPT.

### No wave regresses the 94.1%/91.5% leaves silently — the cross-wave chain

- **G1/G2/G3/G4/G5-G6/H1 each carry an explicit JSON/CSS preservation conjunct as a CONSUMED gate
  column** — verified on disk: G1 `g1_hot_leaf_preserved` + `g1_json_guard_rows_held` (lines 875,
  883); G2 `g2_sota_ratio_held` + `g2_cssparser_oracle_parity` (lines 1031–1032); G3
  `g3_json_hot_leaf_preserved` (conjunct 7) + `g3_css_sota_ratio_held` (conjunct 8) as gate-REJECT
  columns (lines 1161–1168) — the path-only G3 change re-asserts BOTH leaves byte-stable via conjunct
  6 byte-equivalence; G4 `json_rich_navigation_preserved` + "JSON 51/51 held" (lines 1233–1236); G5/G6
  `json_guard_held` (lines 1362, 1386); H1 `json_guard_held` + `css_sota_ratio_held` (lines 1586–1587).
  The §0.4 schema REJECTs producer-only fields and any G1 row with `g1_hot_leaf_preserved != true` /
  `json_strict_rows_admitted != 51` (lines 286–287). The V6 fold ADDED the missing PROVE link
  (`json_css_preservation_held`, lines 1507/1515), closing the silent-regression seam on the shared
  emit path PROVE is the last to touch. The chain is now complete across ALL seven waves. ACCEPT on
  the chain's COMPLETENESS; the ONE residual is the PRECISION of the V6-added PROVE column's CSS
  clause — the REVISE below.

---

## REVISE

### R-CH5-1 (REVISE) — §9 PROVE: the folded `json_css_preservation_held` CSS clause directs an un-measurable, mis-scoped CSS measurement, inconsistent with the G3/H1 phrasing of the SAME check

**Section:** Section 9 (PROVE), telemetry column `json_css_preservation_held`, line 1507.

**The defect (material, not cosmetic).** The V6 fold correctly bound the §0.5 "across every
generalization wave" floor at PROVE via this column — that intent is sound and ACCEPTED. But the
column's CSS clause reads:

> `... AND CSS same-run track1_rich/lcss > 1.0× no-regression on the corpus-in-timer harness, vs
> the G3-closed shipped files; ...`

This phrasing directs the PROVE implementer to run a SAME-RUN no-regression CSS measurement on the
corpus-in-timer harness. That is un-measurable AND mis-scoped at PROVE, on three grounds the rest of
the SPEC already settles:

1. **The same-run pre-G2 comparison is un-measurable post-G2.** PROVE runs after G3 (its entry is
   `G3 ∧ G4 closed`), so the pre-G2 code is GONE — exactly the situation G3 and H1 resolve. The SAME
   CSS check at G3 (`g3_css_sota_ratio_held`, line 1162) and at H1 (`css_sota_ratio_held`, line 1587)
   is phrased "**re-confirmed DIRECTIONALLY against the G2-RECORDED `track1_rich_over_lcss_ratio_pre_g2`
   baseline — the pre-G2 code is gone post-G2 so [the wave] never re-measures pre-G2 same-run, per
   close-cond #6**." PROVE — equally downstream of G2 — OMITS this qualifier and instead says
   "same-run ... no-regression," directing the implementer toward a measurement that close-cond #6
   (lines 111–113) declares un-derivable post-G2.

2. **`corpus_in_timer` is a G2/H1-only obligation, not a PROVE one.** The §0.4 schema (line 254)
   binds `corpus_in_timer` "MUST be true at **G2/H1**"; G6 emits no `corpus_in_timer` column (deferred
   to H1). PROVE is the Sheets wave, parallel to G5/G6, and does not run the CSS corpus-in-timer
   harness. Naming "on the corpus-in-timer harness" in PROVE's slice creates a phantom obligation the
   schema does not scope to PROVE.

3. **Byte-equivalence already discharges PROVE's CSS preservation — definitionally, with no fresh
   bench.** The SAME column already names the correct mechanism: `dirty_generated_state == clean for
   grammars/{json,css_l4}/generated.rs` (byte-equal vs the G3-closed shipped files). This is exactly
   how G3 conjunct 6 (lines 1120–1126) binds the un-fork's CSS preservation — byte-equivalence proves
   the leaf bytes cannot move, so the throughput is preserved without any ratio re-measurement. If
   PROVE's touch to the shared `render(program)` body leaves `css_l4/generated.rs` byte-identical to
   its G3-closed form, the CSS >SOTA is preserved by construction. A fresh "same-run no-regression"
   CSS ratio is neither needed nor measurable at PROVE.

Left as-is, a PROVE implementer reads the column as requiring a same-run corpus-in-timer CSS
no-regression bench — un-measurable for the pre-G2 leg, mis-scoped per §0.4 — and either fabricates a
figure or stalls, when the binding and sufficient check is byte-equivalence (already named). This is
the residual-precision REVISE the lens exists to drive out: it materially changes what a PROVE
implementer must measure, and it leaves PROVE the ONE wave whose CSS-preservation phrasing diverges
from the directional-re-confirmation discipline G3 and H1 carry.

**Exact one-line edit.** In §9 line 1507, replace the CSS clause so it binds CSS preservation to
byte-equivalence (the G3-conjunct-6 mechanism), DROPPING the un-measurable same-run / corpus-in-timer
phrasing:

```
json_css_preservation_held                 (true — JSON 91.5% leaf byte-equal + 51/51 guard within ±1.0% of SK-V18-open AND CSS track1_rich/lcss preserved by byte-equivalence of grammars/css_l4/generated.rs vs the G3-closed shipped file (dirty_generated_state == clean) — NOT a fresh corpus-in-timer re-measure (PROVE runs post-G2, the pre-G2 same-run baseline is gone per close-cond #6; PROVE preserves the CSS leaf by byte-equality, not a ratio bench); the shared render(program) body PROVE re-touches must not perturb the JSON/CSS hot leaves; dirty_generated_state == clean for grammars/{json,css_l4}/generated.rs — binds the §0.5 across-every-generalization-wave floor at the one emit-path-touching wave-slice)
```

Rationale: byte-equivalence vs the G3-closed CSS file is the only check PROVE can perform AND is
sufficient (byte-equal ⇒ throughput-equal), it matches the G3 conjunct-6 mechanism, and it removes the
phantom corpus-in-timer obligation §0.4 scopes to G2/H1 only — aligning PROVE's CSS-preservation
phrasing with the directional/byte-equal discipline G3 and H1 carry. The JSON clause and the
`dirty_generated_state == clean` anchor are retained unchanged; the §0.5-floor binding the V6 fold
established is preserved.

---

## Items inspected and ACCEPTED (no churn)

- **V6 R-CH5-1 fold verified landed** — `json_css_preservation_held` is present in §9 telemetry (line
  1507) and in the REJECT list (line 1515 `; json_css_preservation_held != true`). The §0.4 line-278
  reference (`dirty_generated_state ... at P3/G3/PROVE`) is now reconciled — PROVE's slice references
  `dirty_generated_state == clean` inside the composite column. The fold's INTENT (bind the §0.5 floor
  at PROVE) is sound; only the CSS-clause PRECISION is the residual (R-CH5-1 above). ACCEPT-as-folded.
- **G3 path-only byte-equivalence (conjunct 6, lines 1120–1122, 1160)** — `dirty_generated_state ==
  clean` + "diff of regenerated vs shipped == empty for every grammar" correctly binds the un-fork as
  a PATH change not an OUTPUT change; the JSON/CSS leaves cannot move at G3. Sound.
- **§0.3 outcome enum SOTA rules (lines 220–232)** — a NEON speedup CLAIM is `A` ONLY under
  timed-plane binding + H1 quiet-bar (`host_loadavg < 1.0`); a corpus-in-timer figure under load is
  `S`; a checkasm PASS with no corpus figure is `C`; a recognition-only `track1_full_parse` `A` does
  NOT discharge the typed close (preserve-rich-ast). No unfalsifiable admit path. Sound.
- **Load-robustness caveat (§0.2 lines 199–204; §5-risk-7 inheritance)** — the QUIET re-capture
  (`host_loadavg < 1.0`) gating every ABSOLUTE Mbps claim, with the same-run ratio as the load-robust
  ground-truth, is consistently threaded G2→G6→H1. The H1 harness MUST stamp `host_loadavg`; an
  absolute claim with `host_loadavg ≥ 1.0` or no stamp is RED (lines 1573–1576). Sound, non-circular.
- **H1 close (§10 lines 1559–1596)** — re-confirms the G2 ratio DIRECTIONALLY (NOT re-measuring the
  gone pre-G2 code), discloses `materialization_framing == lazy-rich-vs-eager-cssom` (the CLOSED-enum
  two-value REJECT, lines 255, 1590–1591), holds `json_guard_held` + `css_sota_ratio_held`. The
  lazy-rich-vs-eager honesty (no unqualified "beats CSSOM" behind a re-label) is sound. ACCEPT.
- **Sequencing soundness for the SOTA chain** — G2 entry dual-gates G1 ∧ P3; G6 entry P1 ∧ P3 ∧ G3
  (the singular `css_scan_call_site_singular` depends on P3 so the retarget lands into ONE scan, not
  7); PROVE entry `G3 ∧ G4` parallel to G5/G6; the §2.1 lattice (lines 535–544) is acyclic and matches
  S-P2 §3. No broken sequence touching the SOTA floors. ACCEPT.
- **Addenda binding** — addendum 5 (corpus-in-timer) bound at G2/H1 (the G2-entry capture + the H1
  deferred figure); addendum 6 (acceleration-at-admission, BOTH conjuncts source-census ∧
  `simd_admission_profile_sampled`) bound at G6 (lines 251, 1341–1351, 1377). No addenda violation.
  ACCEPT.

---

## Verdict summary

The >SOTA-regression gate IS bound at G2 AND G6 on the corpus-in-timer per-corpus floors; the 94.1%
and 91.5% hot leaves ARE preserved as (a)-(d)-gated primitives; the no-silent-regression chain is now
COMPLETE across all seven waves (the V6 fold closed the PROVE gap). ONE residual precision REVISE: the
V6-added PROVE `json_css_preservation_held` column directs a SAME-RUN corpus-in-timer CSS no-regression
measurement that is un-measurable post-G2 (the pre-G2 baseline is gone, per close-cond #6) and
mis-scoped (`corpus_in_timer` is a G2/H1-only obligation per §0.4), diverging from the
directional-re-confirmation / byte-equivalence phrasing G3 conjunct 8 and H1 both carry; the fix binds
PROVE's CSS preservation to byte-equivalence (the G3-conjunct-6 mechanism — the only check PROVE can
perform and sufficient to preserve the leaf). No genuine REJECT — no unfalsifiable gate stands, the
sequence is acyclic and sound, no addenda are violated. The fix is a single-column re-phrasing.

TALLY accept=12 revise=1 reject=0
