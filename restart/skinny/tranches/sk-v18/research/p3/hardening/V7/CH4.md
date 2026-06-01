# SK-V18 S-P3 CHALLENGE — CH4 (cycle V7) — Lens: TELEMETRY-COMPLETENESS

Target: `restart/skinny/tranches/sk-v18/SPEC.md` (§0.1 close conditions #1–#12, §0.3 outcome enum, §0.4
required telemetry + supporting columns + gate-consumer, §3.6 W-PRUNE telemetry, §4–§10 per-wave telemetry
blocks). Cross-read against S-P2 §3 sequencing (`research/p2/SYNTHESIS-RESEARCH.md`) + the 6 addenda + R16
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`).

Lens question: do the required telemetry columns + the gate-consumer cover every close-condition predicate,
each column produced by a named wave? Any close predicate with no telemetry, or column with no producing
wave / emitted-but-unconsumed producer-only field? Verdict per claim ACCEPT / REVISE / REJECT.
Proportionate: a wording nit is a REVISE only if it would mislead an implementer.

## Method

Two-direction completeness ledger:
(A) every §0.1 close predicate (#1–#12) → its telemetry column(s) → the named producing wave + the
gate-consumer REJECT clause that consumes them;
(B) every §0.4 schema column AND every per-wave telemetry-block column → its named producing wave AND a
consuming REJECT clause (the inverse: a column emitted but consumed by no slice — the
`[typed-materialization-invariant]` "producer-only fails the wave", §0.4 line 240).
Confirmed the V6 REVISE (R-CH4.1, `corpus_in_timer` over-attributing G6) is FOLDED: §0.4 line 254 now reads
"MUST be true at G2/H1; ... so G6 emits no `corpus_in_timer` column".

## Enumeration (every wave-gate / telemetry / close claim under this lens)

### Direction A — every close predicate has telemetry produced by a named wave: ACCEPT (12/12)

| Close (§0.1) | Telemetry column(s) | Producing wave | Gate-consumer REJECT |
|---|---|---|---|
| #1 one generator / no blob | `generator_grammar_count` (=3 at PROVE), `verbatim_blob_present` | PROVE (count); G1+G2 (blob) | PROVE §9 (1513), G1.4 (888), G2.5 (1045) |
| #2 un-forked emitter | `emitter_fork_present`, `generator_grammar_branch_count`, `generator_grammar_type_count`, `emit_shape_source` | G3.5 | G3 5-conjunct (1165–1168) |
| #3 relocated seam closed | `runtime_target_rows_collapsed` | P3 §3.6, G3.5 (re-assert G2.5, PROVE) | P3 (781), G3 (1167), G2 (1046), PROVE (1514) |
| #4 shared trait ≥2 non-collapsible | `json_rich_navigation_preserved`, `shared_trait_impl_count`, `shared_trait_non_collapsible` | G4.4 | G4 3-conjunct (1272–1274) |
| #5 phantom `<G>` deleted | `phantom_generic_resolved` | G4.4 | G4 (1272) |
| #6 >SOTA honest | `g2_cssparser_oracle_parity`, `g2_sota_ratio_held`/`g3_css_sota_ratio_held`/`css_sota_ratio_held`, `track1_rich_over_lcss_ratio_pre_g2` (G2-entry capture), `g1_hot_leaf_preserved`/`g3_json_hot_leaf_preserved`, `g1_json_guard_rows_held`/`json_guard_held`, `corpus_in_timer` | G1.4, G2.5 (+G2-entry capture), G3.5, H1 | G1 (888), G2 (1044–1045), G3 (1168), H1 (1592) |
| #7 x86 gone | `x86_tree_deleted` | P1 §3.6 | P1 (781) |
| #8 Lock-14 meaningful | `lock14_gate_scans_codegen`, `forbidden_generic_tokens_extended` | P4 §3.6 | P4 (782) |
| #9 Sheets negative control | `sheets_grammar_shape`, `sheets_emission_path` (→N), `generator_grammar_count==3`, `sheets_value_instantiates_g4_trait`, `import_closure_relaxation_is_data` | PROVE §9 | PROVE (1511–1515) |
| #10 NEON at admission | `acceleration_at_admission`, `simd_admission_caller`, `simd_admission_profile_sampled` | G5/G6 §8 | G5/G6 (1389–1391) |
| #11 generated-state clean | `dirty_generated_state`/`regen_check_clean`, `metalang_leak_present`, `generated_md5_distinct` | P3/G3/PROVE/H1; P5; P3/G3/PROVE | P5 (783), H1 (1592), G3 (1168), P3 (781), PROVE (1510) |
| #12 PASS-IMPL / framing | `materialization_framing` | H1 §10 | H1 (1590) |

Every close predicate maps to a telemetry column emitted by a named wave AND a gate-consumer REJECT clause.
No close predicate is unfalsifiable; none is telemetry-less. The closed-domain enums a gate decides on
(`acceleration_at_admission ∈ {admission|dead}`, `sheets_emission_path ∈ {generator-only|shim}`,
`phantom_generic_resolved`, `emit_shape_source`, `materialization_framing`, `dirty_generated_state`,
`json_scan_rs_neutralized`) are CLOSED and consistent across §0.3 / §0.4 / the producing block; the schema
even bars a third `acceleration_at_admission` state explicitly (§0.4 line 251). `sheets_emission_path ==
shim` is correctly NOT a gate-REJECT but the binding `N` verdict — surfaced, not paper-closed (§9). The G3
conjunct-8 phrasing "on the corpus-in-timer harness" (line 1126) is consistent with its column descriptor
(line 1162: re-confirmed DIRECTIONALLY against the G2-RECORDED baseline, never re-measured pre-G2) — G3
emits no `corpus_in_timer` column and its §0.4 schema (254) correctly omits G3, so no inverse
over-attribution survives the V6 fold. ACCEPT.

### Direction B — every schema/block column has a producing wave: ACCEPT except the producer-only set

- **The 13 binding §0.4 columns** (242–256): each carries an explicit "MUST be X at <wave>"; each named
  wave's producing block emits it; multi-wave claims honored at BOTH waves
  (`runtime_target_rows_collapsed` P3+G3+G2+PROVE; `verbatim_blob_present` G1+G2; `corpus_in_timer` G2+H1).
  ACCEPT.
- **Supporting columns** (261–272): the retained SK-V15/16/17 CSS+JSON schema (line 274) lives ONLY in §0.4
  and is consumed via the §0.4 global REJECT clause (283–290) / the inline descriptors, NOT re-emitted in the
  per-wave blocks (confirmed on disk: `css_corpus`/`css_sample_*`/`css_comparator_plane`/`css_track1_*`
  appear nowhere outside §0.4). Their producing wave is the G2/H1 CSS bench row carrying the standing schema.
  The dual-naming (`json_strict_rows_admitted` ↔ `g1_json_guard_rows_held`; `css_typed_summary_equal` ↔
  `g2_cssparser_oracle_parity`) is ONE measurement under two names on the same row — V6-validated, sound.
  `css_corpus` is consumed via the benched-set REJECT (288–289). `named_primitive_falsifier_pass` is the
  umbrella name realized per-wave as `*_abcd_pass` (283). ACCEPT.
- **`css_comparator_plane` roll-call omission (sub-REVISE bar).** It is the lone supporting column absent
  from the reconciliation column-by-column roll-call (275–283), yet it IS consumed — its inline descriptor
  (262) states "consumed at G2/H1" and the §0.4 global REJECT clause (285) consumes
  `css_comparator_plane==full-cssom`. The universal claim "EVERY supporting column above is consumed in a
  named wave slice" is therefore TRUE for it; only the redundant restatement under-enumerates. An implementer
  wires it from the binding REJECT clause (285) regardless. Below the proportionality bar — does not mislead.
  ACCEPT.
- **The producer-only set (the one REVISE — see below).** Five columns are emitted in a per-wave telemetry
  block but named in NO clause of that wave's gate-consumer REJECT, against the block's own
  "producer-only fails the wave" / "no producer-only field" invariant (§0.4 line 240; G1.4 header line 870;
  line 290).

## The one residual REVISE

**R-CH4.2 (V7) — §4 G1.4 + §5 G2.5: emitted-but-unconsumed producer-only columns contradict the block's own
"producer-only fails the wave" / "every emitted field is consumed" invariant.**

The G1.4 header (line 870) binds explicitly: "Telemetry (emitted AND consumed by `gate-json` in the G1
slice; **producer-only fails the wave**)." §0.4 line 240 binds campaign-wide: "a column emitted but never
consumed FAILS the wave — `[typed-materialization-invariant]`". Line 290: "Every emitted field is consumed
in the same wave." Yet the per-wave REJECT clauses omit:

- **G1 (REJECT clause 887–891):** `g1_dispatch_triple_not_lcd_collapsed` (876) and `g1_leaf_primitive_count`
  (878) are emitted but appear in NO REJECT predicate. `g1_dispatch_triple_not_lcd_collapsed` is a *genuine
  gate obligation* (close-#6 / G1.3 anti-LCD-collapse, Pre-Blocked route line 1615 "an LCD-unify of the
  dispatch triple") left unconsumed — not merely informational.
- **G2 (REJECT clause 1041–1046):** `g2_css_rich_projection_not_flattened` (1035, the G4-co-gate-not-foreclosed
  evidence) and `g2_css_replica_singular` (1036, the P3-singular re-assertion) are emitted but in NO REJECT
  predicate; `g2_sota_ratio_directional_antecedent` (1033) is likewise unconsumed.

The contrast that proves this is a defect not a style choice: `line_delta_vs_oracle` (884) is the SAME shape
(emitted, non-gating) but is EXPLICITLY carved out — "SOFT tripwire only; advisory, never REJECT" + the
REJECT clause's closing "`line_delta_vs_oracle` does NOT gate" (891). The five columns above carry no such
carve-out, so under "producer-only fails the wave" they are self-contradictory: a legitimately-emitted
evidence column would FAIL its own wave.

Why it misleads an implementer (clears the proportionality bar): a `gate-json` author reading G1.4's
"producer-only fails the wave" alongside the REJECT clause faces an irreconcilable choice — either treat
these emitted-but-unlisted columns as wave-failing (false-RED on every G1/G2 close), or silently disregard
the block's own invariant. And `g1_dispatch_triple_not_lcd_collapsed` is a real close obligation that loses
its binding consumer. This is a precision defect, NOT a broken sequence or an addenda violation (the LCD,
rich-projection, and replica obligations ARE covered in prose / at G4 / at P3) — REVISE, not REJECT.

EXACT one-line edits:

1. **§4, G1 REJECT clause (line 891)** — change the tail
   `...; `g1_json_guard_rows_held != true`. `line_delta_vs_oracle` does NOT gate.`
   to
   `...; `g1_json_guard_rows_held != true`; `g1_dispatch_triple_not_lcd_collapsed != true`. `line_delta_vs_oracle` and `g1_leaf_primitive_count` are emitted EVIDENCE, do NOT gate.`

2. **§5, G2 REJECT clause (line 1046)** — change the tail
   `...; `runtime_target_rows_collapsed != true`. Absolute-Mbps figures carry the §5-risk-7`
   to
   `...; `runtime_target_rows_collapsed != true`; `g2_css_rich_projection_not_flattened != true`; `g2_css_replica_singular != true`. `g2_sota_ratio_directional_antecedent` is emitted DIRECTIONAL evidence, does NOT gate. Absolute-Mbps figures carry the §5-risk-7`

(Either consume the column in the REJECT clause where it is a real obligation
— `g1_dispatch_triple_not_lcd_collapsed`, `g2_css_rich_projection_not_flattened`, `g2_css_replica_singular` —
or carve it out as non-gating evidence the way `line_delta_vs_oracle` already is —
`g1_leaf_primitive_count`, `g2_sota_ratio_directional_antecedent` — so no emitted column silently violates
the "producer-only fails the wave" rule.)

## No REJECT

No unfalsifiable gate (every gate has a turning-RED falsifier + a closed-domain enum where it decides on one
value). No broken sequence under this lens (telemetry producers align with the seq §3 / lattice §2.1 entry
gates; the seq/C6+C7 and s6/C4 folds introduce no telemetry-less predicate). No addenda violation (the 6
addenda + R16 each bind a telemetered column with a producing wave + a REJECT clause). The V6 REVISE is
folded. The lone residual defect is the G1/G2 producer-only contradiction above.

TALLY accept=16 revise=1 reject=0
