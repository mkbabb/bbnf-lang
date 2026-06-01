# SK-V18 S-P3 CHALLENGE — CH5 SOTA-PRESERVATION (cycle V6)

Lens: is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer, the per-corpus floors), the
hot leaves preserved as gated primitives, and does NO wave regress the 94.1%/91.5% leaves silently?
Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1642 lines) against S-P2 §3 sequencing and the S-P0
addenda. Posture: PROPORTIONATE residual-precision drive-out toward the 2-consecutive-clean fixed
point; a wording nit is a REVISE only if it would mislead an implementer. Prior cycles V1–V5 reject=0;
GROUND `sota.md` accept=13/0/0 folded. Every claim below re-grounded on disk this pass.

---

## Enumeration — every wave-gate / telemetry / close claim under the SOTA-PRESERVATION lens

### The >SOTA-regression gate is bound at BOTH G2 AND G6 (the lens's primary question) — ACCEPT

- **G2 explicit >SOTA-regression gate, distinct from parity (§5 G2.2 conjunct 4, lines 951–961;
  telemetry `g2_sota_ratio_held` line 1026; close-cond #6 lines 104–128; §0.2 line 180; §0.5 lines
  302–307).** The binding falsifier is the SAME-RUN `track1_rich/lightningcss > 1.0×` per corpus AND
  no same-run regression vs the parser's OWN pre-G2 baseline `track1_rich_over_lcss_ratio_pre_g2`
  captured AT G2 ENTRY in one quiet run, on `css_canon_bench` cold corpus-in-timer, with parity
  gate-BEFORE-speed. The S-P1 absolutes (2.190/3.375/1.658/2.101) are correctly demoted to DIRECTIONAL
  antecedents, NOT the floor — the same-run two-figures-one-plane construction defuses the
  load-depressed-absolute unfalsifiable-gate hazard. The "oracle parity does NOT prove throughput; the
  bench re-measure is the binding regression falsifier" disjunction is explicit (lines 959–961). The
  pre-G2 baseline being captured at G2-entry (un-re-derivable post-G2) is the sound resolution of the
  "where does the floor come from after the old code is gone" problem. ACCEPT — sound, falsifiable,
  per-corpus, not unfalsifiable.
- **G6 >SOTA binding (§8 exit gate lines 1335–1354; §0.3 outcome rules lines 224–228; telemetry
  `g6_speedup_median_mbps`/`corpus_in_timer` lines 1375, 1368).** Correctly SPLIT: the checkasm
  differential + `neon_significant_skip_matches_scalar` over the REAL 71KB–495KB corpora is a
  CORRECTNESS plane only; any Mbps/speedup FIGURE comes ONLY from the corpus-in-timer symmetric
  `css_canon_bench` harness; the speedup CLAIM is DEFERRED to the H1 symmetric timer
  (`g6_speedup_median_mbps` null pre-H1; G6 outcome `C` until H1). A figure off the checkasm plane is
  an addendum-5 plane-mismatch REJECT (lines 1362–1363, 1385–1387). This is the corpus-in-timer floor
  bound at G6 exactly as the lens demands, with the honest "no speedup claim on the wrong plane / one
  wave too early" discipline. ACCEPT.
- **Per-corpus floors, not a corpus-average (§0.5 lines 294–307; H1 route ledger line 1607).** The CSS
  bar is per-corpus with ≥1 REGULAR corpus (animate OR bootstrap) crossing mandatory; tailwindcss
  below 1.0× is an honest residual not tranche-blocking provided a regular holds; mcw is the
  full-corpus integration check. A corpus-average substituting for per-corpus ratios is a pre-blocked
  H1 route. `gate-json` REJECTs any single-tuple broadcast (`sample_count==1` or one tuple across
  multiple corpus rows, line 289–290). ACCEPT — the floors are per-corpus and broadcast-proof.

### Hot leaves preserved as gated primitives — ACCEPT

- **CSS 94.1% leaf as `css_balanced_component_scan` (§1 (a)-(d) gate lines 357–392; §5 G2.3 lines
  975–1005; telemetry `g2_balanced_scan_primitive_abcd_pass`/`_loc`/`_profiled_leaf_extent` lines
  1020–1022).** The primitive is (a) grammar-INVOKED, (b) ARG-VARIES under invoking-rule mutation, (c)
  `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF with the machine-checked
  `primitive_loc <= profiled_leaf_extent` god-kernel REJECT. The FORCED demotion to the CSS-scoped name
  (s6/C4, both non-CSS dischargers grounded structurally incompatible byte-SKIP-vs-parse-with-emit) is
  the correct honest resolution. The same primitive is the SINGLE G6 NEON-retarget call site (one seam
  for G2+G6), so the 94.1% leaf is preserved as a gated primitive AND accelerated at the same site.
  ACCEPT.
- **JSON 91.5% leaf preserved (§4 G1.2 conjunct 3 lines 833–839; G1.3 leaf primitives lines 847–862;
  `g1_hot_leaf_preserved`/`g1_dispatch_triple_not_lcd_collapsed` lines 869–870).** The
  `parse_object_value_at_direct`/`parse_array_element_at_direct` bodies are re-emitted byte-equivalent
  (identical inline cfg + `sink.*` call sites), with the explicit "do NOT LCD-collapse the
  value/object/array dispatch triple" guard (regresses the monomorphized-sink leaf, rC §5). The two
  leaf-scanner primitives (`decode_json_string_to_arena`, `parse_number_*`) carry the same (a)-(d)
  gate with the (d) `loc <= profiled_leaf_extent` machine-check. ACCEPT.

### No wave regresses the 94.1%/91.5% leaves silently — the cross-wave preservation chain

- **G1 / G2 / G3 / G4 / G5-G6 / H1 each carry an explicit JSON/CSS preservation conjunct as a CONSUMED
  gate column** — verified on disk: G1 `g1_hot_leaf_preserved` + `g1_json_guard_rows_held` (lines
  869, 877); G2 `g2_sota_ratio_held` + `g2_cssparser_oracle_parity` (lines 1025–1026); G3
  `g3_json_hot_leaf_preserved` (conjunct 7) + `g3_css_sota_ratio_held` (conjunct 8) as gate-REJECT
  columns (lines 1155–1156, 1162–1164) — the path-only G3 change re-asserts BOTH leaves byte-stable;
  G4 `json_rich_navigation_preserved` with "JSON 51/51 held" (lines 1227–1230, 1261); G5/G6
  `json_guard_held` (lines 1353, 1377); H1 `json_guard_held` + `css_sota_ratio_held` (lines 1572–1573,
  1578). The §0.4 schema correctly REJECTs producer-only fields and any G1 row with
  `g1_hot_leaf_preserved != true` / `json_strict_rows_admitted != 51` (lines 286–287). This is a sound,
  per-wave, no-silent-regression chain across every wave that re-emits or re-routes the leaves —
  with the SINGLE exception named below. ACCEPT for G1/G2/G3/G4/G5-G6/H1; the PROVE link is the
  REVISE.

---

## REVISE

### R-CH5-1 (REVISE) — §9 PROVE: the one emit-path-touching wave with NO JSON/CSS preservation gate column

**Section:** Section 9 (PROVE), exit gate lines 1445–1470 + telemetry block lines 1487–1498 + the
REJECT list lines 1500–1508.

**The gap (material, not cosmetic).** PROVE re-touches the SHARED un-forked `render(program)` emit body
— it makes that one body render the Sheets precedence-tower `BackendShape` path, adds a `RuntimeTarget`
row, and relaxes the import-closure as data (§9 Tasks lines 1430–1443). That same `render(program)`
body is the one that emits the JSON `SinkOnly`-shape renderer carrying the 91.5% leaf and the CSS scan
carrying the 94.1% leaf. Yet PROVE's exit gate and telemetry carry the SOTA-preservation chain's column
NOWHERE: PROVE has `generated_md5_distinct` (which proves Sheets ≠ JSON/CSS, NOT that JSON/CSS bytes are
unchanged vs their G3-closed form), the four addendum-2 co-gates, `sheets_grammar_shape`, the trait
instantiation — but NEITHER `g3_json_hot_leaf_preserved`/`json_guard_held` NOR `css_sota_ratio_held`/
`dirty_generated_state(regen --check byte-equiv vs prior)`. PROVE is the ONLY generalization wave
touching the shared emit path whose slice omits the JSON/CSS preservation conjunct (G1/G2/G3/G4/G5-G6/H1
all carry it; verified above). The global §0.5 floor ("all 51 JSON rows maintain ... **across every
generalization wave**", lines 309–310) is PROSE, not a PROVE-slice CONSUMED column; under §0.4's binding
"every emitted field is consumed in the same wave / no producer-only field" discipline it has no
enforcement hook at PROVE. A PROVE change to `render(program)` that perturbs the JSON `SinkOnly` arm to
accommodate the shared Sheets path would pass EVERY PROVE gate column and md5-distinctness, and be
caught only at H1 (one wave too late) — and H1's revert path ("reopen the producing wave PROVE", line
1585) presupposes the regression was ATTRIBUTED to PROVE, which the missing PROVE column is precisely
what would attribute.

**Exact one-line edit.** In §9 telemetry (after line 1497 `sheets_emission_path`), add the consumed
preservation conjunct, and add it to the REJECT list (line 1505):

```
json_css_preservation_held                 (true — JSON 91.5% leaf byte-equal + 51/51 guard within ±1.0% of SK-V18-open AND CSS same-run track1_rich/lcss > 1.0× no-regression on the corpus-in-timer harness, vs the G3-closed shipped files; the shared render(program) body PROVE re-touches must not perturb the JSON/CSS hot leaves; dirty_generated_state == clean for grammars/{json,css_l4}/generated.rs)
```

and append to line 1505's REJECT clause: `; json_css_preservation_held != true`.

(Equivalently: add `g3_json_hot_leaf_preserved`, `g3_css_sota_ratio_held`, and `dirty_generated_state`
to PROVE's consumed slice. The minimal, DRY form is the single composite column above.) Rationale: this
binds the §0.5 "across every generalization wave" floor to the one wave-slice where it is currently
unenforced, closing the silent-regression seam on the SHARED emit path PROVE is the last to touch. This
is the residual precision REVISE the lens exists to drive out; it materially changes what an
implementer of PROVE must measure.

---

## Items inspected and ACCEPTED (no churn)

- **§0.4 line 278 minor consistency** — the prose says `dirty_generated_state` is consumed "at
  P3/G3/PROVE", but PROVE's §9 telemetry block does not list it. This is SUBSUMED by R-CH5-1 (the
  composite column names `dirty_generated_state == clean`); no separate edit needed — flagging only so
  the fold reconciles line 278 with the new PROVE column rather than leaving a dangling reference.
  ACCEPT as-folded-under-R-CH5-1.
- **G3 path-only byte-equivalence (conjunct 6, lines 1114–1116, 1153–1154)** — `dirty_generated_state
  == clean` + "diff of regenerated vs shipped == empty for every grammar" correctly binds the un-fork
  as a PATH change not an OUTPUT change; the JSON/CSS leaves cannot move at G3. Sound.
- **§0.3 outcome enum SOTA rules (lines 220–231)** — a NEON speedup CLAIM is `A` ONLY under
  timed-plane binding + H1 quiet-bar (`host_loadavg < 1.0`); a corpus-in-timer figure under load is
  `S` (directional residual), a checkasm PASS with no corpus figure is `C`; a recognition-only
  `track1_full_parse` `A` does NOT discharge the typed close (preserve-rich-ast). No unfalsifiable
  admit path. Sound.
- **Load-robustness caveat (§0.2 lines 199–204; §5-risk-7 inheritance throughout)** — the QUIET
  re-capture (`host_loadavg < 1.0`) gating every ABSOLUTE Mbps claim, with the same-run ratio as the
  load-robust ground-truth, is consistently threaded G2→G6→H1. Sound and non-circular.
- **H1 close (§10 lines 1549–1582)** — re-confirms the G2 ratio directionally (NOT re-measuring the
  gone pre-G2 code), discloses `materialization_framing == lazy-rich-vs-eager-cssom` (the CLOSED-enum
  two-value REJECT, lines 255, 1576–1577), holds `json_guard_held` + `css_sota_ratio_held`. The
  lazy-rich-vs-eager honesty (no unqualified "beats CSSOM" behind a re-label) is sound. ACCEPT.
- **Sequencing soundness for the SOTA chain** — G2 entry dual-gates on G1 ∧ P3; G6 entry on
  P1 ∧ P3 ∧ G3 (the singular-call-site `css_scan_call_site_singular` depends on P3 collapse so the
  retarget lands into ONE scan, not 7); the §2.1 lattice (lines 535–544) is acyclic and matches S-P2
  §3. No broken sequence touching the SOTA floors. ACCEPT.
- **Addenda binding** — addendum 5 (corpus-in-timer) bound at G2/G6/H1; addendum 6
  (acceleration-at-admission, the BOTH-conjuncts source-census ∧ `simd_admission_profile_sampled`
  profile-reachability) bound at G6 (lines 251, 1335–1344, 1368–1370). No addenda violation. ACCEPT.

---

## Verdict summary

The >SOTA-regression gate IS bound at G2 AND G6 on the corpus-in-timer per-corpus floors; the 94.1% and
91.5% hot leaves ARE preserved as (a)-(d)-gated primitives; the no-silent-regression chain holds across
G1/G2/G3/G4/G5-G6/H1. ONE residual precision REVISE: PROVE re-touches the shared `render(program)` emit
body but is the sole emit-path-touching wave whose slice omits a CONSUMED JSON/CSS-preservation column,
leaving the §0.5 "across every generalization wave" floor unenforced at PROVE (a silent-regression seam
on the shared path, caught only at H1 one wave too late). No genuine REJECT — the floor exists in prose,
the sequence is sound, no addenda are violated. The fix is a single composite telemetry column + one
REJECT clause.

TALLY accept=12 revise=1 reject=0
