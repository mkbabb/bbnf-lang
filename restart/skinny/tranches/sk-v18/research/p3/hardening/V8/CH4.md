# SK-V18 S-P3 CHALLENGE — CH4 (cycle V8) — Lens: TELEMETRY-COMPLETENESS

Target: `restart/skinny/tranches/sk-v18/SPEC.md` (§0.1 close conditions #1–#12, §0.3 outcome enum, §0.4
required telemetry — the 13 binding columns + the supporting columns + the gate-consumer reconciliation,
§3.6 W-PRUNE telemetry, §4–§10 per-wave telemetry blocks). Cross-read against S-P2 §3 sequencing
(`research/p2/SYNTHESIS-RESEARCH.md`) + the 6 addenda + R16 (`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`).

Lens question: do the required telemetry columns + the gate-consumer cover every close-condition predicate,
each column produced by a named wave? Any close predicate with no telemetry, or column with no producing
wave / emitted-but-unconsumed producer-only field? Verdict per claim ACCEPT / REVISE / REJECT.
Proportionate: a wording nit is a REVISE only if it would mislead an implementer.

## Method

Two-direction completeness ledger, re-grepped on disk this pass:
(A) every §0.1 close predicate (#1–#12) → its telemetry column(s) → the named producing wave + the
gate-consumer REJECT clause that consumes them;
(B) every §0.4 schema column (the binding 13 + the supporting set) AND every per-wave telemetry-block column
(P-cluster §3.6, G1.4, G2.5, G3.5, G4.4, G5/G6 §8, PROVE §9, H1 §10) → its named producing wave AND a
consuming REJECT/verdict slice (the inverse — a column emitted but consumed by no slice, the
`[typed-materialization-invariant]` "producer-only fails the wave", §0.4 line 240 / 291).

Confirmed the two prior residuals are FOLDED:
- V6 (R-CH4.1, `corpus_in_timer` over-attributing G6): §0.4 line 255 now reads "MUST be true at G2/H1; the
  corpus-in-timer figure is DEFERRED from G6 to H1, so G6 emits no `corpus_in_timer` column". FOLDED.
- V7 (R-CH4.2, G1/G2 producer-only contradiction): G1 REJECT (line 894) now appends
  `g1_dispatch_triple_not_lcd_collapsed != true` and carves out `line_delta_vs_oracle`/`g1_leaf_primitive_count`
  as non-gating evidence; G2 REJECT (line 1049) now appends `g2_css_rich_projection_not_flattened != true`
  + `g2_css_replica_singular != true` and carves out `g2_sota_ratio_directional_antecedent`. FOLDED.

## Enumeration (every wave-gate / telemetry / close claim under this lens)

### Direction A — every close predicate has telemetry produced by a named wave: ACCEPT (12/12)

| Close (§0.1) | Telemetry column(s) | Producing wave | Gate-consumer REJECT |
|---|---|---|---|
| #1 one generator / no blob | `generator_grammar_count` (=3 PROVE), `verbatim_blob_present` | PROVE (count); G1+G2 (blob) | PROVE §9 (1514), G1.4 (891), G2.5 (1048) |
| #2 un-forked emitter | `emitter_fork_present`, `generator_grammar_branch_count`, `generator_grammar_type_count`, `emit_shape_source` | G3.5 | G3 5-conjunct (1168–1171) |
| #3 relocated seam closed | `runtime_target_rows_collapsed` | P3 §3.6 (777), G3.5 (1160) (re-assert G2.5 1041, PROVE 1507) | P3 (784), G3 (1170), G2 (1049), PROVE (1518) |
| #4 shared trait ≥2 non-collapsible | `json_rich_navigation_preserved`, `shared_trait_impl_count`, `shared_trait_non_collapsible` | G4.4 (1271–1273) | G4 3-conjunct (1277) |
| #5 phantom `<G>` deleted | `phantom_generic_resolved` | G4.4 (1270) | G4 (1277) |
| #6 >SOTA honest | `g2_cssparser_oracle_parity`, `g2_sota_ratio_held`/`g3_css_sota_ratio_held`/`css_sota_ratio_held`, `track1_rich_over_lcss_ratio_pre_g2` (G2-entry capture), `g1_hot_leaf_preserved`/`g3_json_hot_leaf_preserved`, `g1_json_guard_rows_held`/`json_guard_held`, `corpus_in_timer` | G1.4, G2.5 (+G2-entry capture), G3.5, H1 | G1 (894), G2 (1047–1048), G3 (1171), H1 (1596) |
| #7 x86 gone | `x86_tree_deleted` | P1 §3.6 (774) | P1 (783) |
| #8 Lock-14 meaningful | `lock14_gate_scans_codegen`, `forbidden_generic_tokens_extended` | P4 §3.6 (778–779) | P4 (785) |
| #9 Sheets negative control | `sheets_grammar_shape`, `sheets_emission_path` (→N), `generator_grammar_count==3`, `sheets_value_instantiates_g4_trait`, `import_closure_relaxation_is_data` | PROVE §9 (1503–1510) | PROVE (1514–1519) + the §9 N-verdict for `sheets_emission_path` |
| #10 NEON at admission | `acceleration_at_admission`, `simd_admission_caller`, `simd_admission_profile_sampled` | G5/G6 §8 (1381–1383) | G5/G6 (1393–1394) |
| #11 generated-state clean | `dirty_generated_state`/`regen_check_clean`, `metalang_leak_present`, `generated_md5_distinct` | P3/G3/PROVE/H1; P5 §3.6 (780); P3/G3/PROVE | P5 (786), H1 (1596), G3 (1170), P3 (784), PROVE (1514) |
| #12 PASS-IMPL / framing | `materialization_framing` | H1 §10 (1585) | H1 (1594) |

Every close predicate maps to a telemetry column emitted by a named wave AND a gate-consumer REJECT clause
(or, for `sheets_emission_path == shim`, the binding `N` negative-control verdict — correctly NOT a
gate-REJECT, surfaced not paper-closed, §9 line 1519). No close predicate is unfalsifiable; none is
telemetry-less. The closed-domain enums a gate decides on (`acceleration_at_admission ∈ {admission|dead}`,
`sheets_emission_path ∈ {generator-only|shim}`, `phantom_generic_resolved`, `emit_shape_source`,
`materialization_framing`, `dirty_generated_state`, `json_scan_rs_neutralized`) are CLOSED and consistent
across §0.3 / §0.4 / the producing block; the schema even bars a third `acceleration_at_admission` state
(§0.4 line 252). The V6 fold holds: G3 emits no `corpus_in_timer` column and §0.4 (255) omits G3, so no
inverse over-attribution survives. ACCEPT.

### Direction B — every schema/block column has a producing wave + consuming slice: ACCEPT except one supporting-tier orphan

- **The 13 binding §0.4 columns** (244–256): each carries an explicit "MUST be X at <wave>"; each named wave's
  producing block emits it; multi-wave claims honored at BOTH named waves —
  `verbatim_blob_present` G1 (880)+G2 (1040); `runtime_target_rows_collapsed` P3 (777)+G3 (1160) [also G2/PROVE];
  `corpus_in_timer` G2 (1037)+H1 (1586). The G1/G2 producer-only set is now reconciled (V7 fold). ACCEPT.
- **Per-wave blocks re-walked for producer-only contradiction (the V7 defect class, extended to ALL waves):**
  - P-cluster §3.6 (774–780, REJECT 783–786): 7/7 consumed. ✓
  - G1.4 (876–887, REJECT 890–894): post-V7-fold, every gating column consumed; `line_delta_vs_oracle` +
    `g1_leaf_primitive_count` explicitly carved as non-gating evidence. ✓
  - G2.5 (1028–1041, REJECT 1044–1049): post-V7-fold, `g2_css_rich_projection_not_flattened` +
    `g2_css_replica_singular` now consumed; `g2_sota_ratio_directional_antecedent` carved as evidence. ✓
  - G3.5 (1156–1166, REJECT 1168–1174): 9/9 consumed (all conjuncts incl. `g3_css_sota_ratio_held`,
    `generated_md5_distinct`). ✓
  - G4.4 (1270–1273, REJECT 1276–1280): 4/4 consumed. ✓
  - G5/G6 §8 (1381–1390, REJECT 1393–1401): 9 gating columns consumed (`simd_admission_caller` as "empty",
    enums as `!=`/`∉`); `g6_speedup_median_mbps` explicitly null-pre-H1, non-gating, deferred to H1. ✓
  - PROVE §9 (1501–1511, REJECT 1514–1519): 10 columns as `!=`; `sheets_emission_path` consumed as the
    binding N-verdict (correctly not a `!=` REJECT). ✓
  - H1 §10 (1585–1591, REJECT 1594–1600): 7/7 consumed (`host_loadavg` + `g6_speedup_median_mbps` jointly
    in the absolute-claim quiet-bar predicate). ✓
- **Supporting columns** (262–273): the retained SK-V15/16/17 schema lives only in §0.4 and is consumed via the
  reconciliation roll-call (276–284) globs + the §0.4 global REJECT clause (285–291). Mapped each on disk:
  `css_corpus` → benched-set REJECT (288); `css_sample_*`/`css_track1_rich_median_mbps`/
  `track1_rich_over_lcss_ratio`/`track1_rich_over_lcss_ratio_pre_g2` → the three roll-call globs;
  `css_comparator_plane`/`css_typed_summary_equal`/`css_rich_ast_preserved` → global REJECT (285–287);
  `json_strict_rows_admitted`/`json_sonic_rs_strict_delta`/`g1_hot_leaf_preserved` → G1 slice (278);
  the rest → their named slices. EXCEPT the one orphan below. The V7 `css_comparator_plane` roll-call
  sub-REVISE-bar note stands ACCEPT (it IS consumed by the global REJECT 285, only the restatement
  under-enumerates).

## The one residual REVISE

**R-CH4.3 (V8) — §0.4 line 264 + the reconciliation roll-call (line 276): `css_lightningcss_full_cssom_median_mbps`
is the lone supporting column matched by NO reconciliation glob AND named in NO REJECT clause, contradicting
the paragraph's own universal "no producer-only field" claim.**

The reconciliation paragraph (line 275–276) opens with the binding universal: "EVERY supporting column above
is consumed in a named wave slice (no producer-only field)". It then attributes the CSS-Mbps columns via
three globs only: ``css_sample_*``/``css_track1_*``/``track1_rich_over_lcss_ratio*`` consumed at G2/H1.

The supporting column `css_lightningcss_full_cssom_median_mbps` (line 264 — the lightningcss-bar denominator
of the binding ratio `track1_rich/lightningcss > 1.0×`) matches NONE of those globs (it is `css_lightningcss_*`,
not `css_track1_*`, not `css_sample_*`, not `track1_rich_over_lcss_ratio*`) and is named in NO REJECT clause
anywhere in the doc (grep confirms zero REJECT-clause occurrences). Its companion numerator
`css_track1_rich_median_mbps` IS covered by the `css_track1_*` glob, so the asymmetry is exact: the numerator
is claimed-consumed, the denominator is the lone uncovered supporting column.

This is the SAME defect class V7 caught (a column emitted under "no producer-only field" with no consuming
slice), in the supporting-column tier rather than the per-wave-block tier — and it is materially DISTINCT
from V7's sub-REVISE-bar `css_comparator_plane` note: `css_comparator_plane` is omitted from the roll-call
but IS rescued by the global REJECT clause (285, `css_comparator_plane==full-cssom`), so an implementer wires
it regardless; `css_lightningcss_full_cssom_median_mbps` is rescued by neither a glob nor a REJECT clause, so
the universal claim is FALSE for it.

Why it misleads an implementer (clears the proportionality bar): a `gate-json` author enumerating the
supporting columns to wire consumers (per §0.4 line 240 "a column emitted but never consumed FAILS the wave")
reads the universal "EVERY supporting column above is consumed", finds the lcss-bar Mbps column with no glob
and no REJECT predicate, and faces the same irreconcilable choice V7 named — treat it as wave-failing
(false-RED on every G2/H1 CSS bench row that emits the denominator) or silently disregard the block's own
invariant. This is a precision defect, NOT a broken sequence or an addenda violation (the lcss-bar Mbps is
genuine evidence consumed implicitly as the binding ratio's denominator; addendum 5 corpus-in-timer coverage
is intact at G2/H1) — REVISE, not REJECT.

EXACT one-line edit (§0.4, line 276) — extend the roll-call glob to cover the lcss-bar column, mirroring V7's
resolution pattern (consume it in its real slice rather than carve out, since it IS the binding ratio's
denominator measured on the same G2/H1 row):

change
  ``above is consumed in a named wave slice (no producer-only field): `css_sample_*`/`css_track1_*`/``
to
  ``above is consumed in a named wave slice (no producer-only field): `css_sample_*`/`css_track1_*`/`css_lightningcss_*`/``

(i.e. add the ``css_lightningcss_*`` glob to the G2/H1-consumed list so `css_lightningcss_full_cssom_median_mbps`
— the binding-ratio denominator — is no longer the lone supporting column outside every consuming slice.)

## No REJECT

No unfalsifiable gate (every gate has a turning-RED falsifier + a closed-domain enum where it decides on one
value). No broken sequence under this lens (telemetry producers align with the seq §3 / lattice §2.1 entry
gates; the seq/C6+C7 and s6/C4 folds introduce no telemetry-less predicate). No addenda violation (the 6
addenda + R16 each bind a telemetered column with a producing wave + a REJECT clause / N-verdict). The V6 and
V7 REVISEs are both folded. The lone residual defect is the supporting-tier orphan above.

TALLY accept=18 revise=1 reject=0
