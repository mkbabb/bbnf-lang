# SK-V18 S-P3 CHALLENGE — CH4 (cycle V6) — Lens: TELEMETRY-COMPLETENESS

Target: `restart/skinny/tranches/sk-v18/SPEC.md` (§0.1 close conditions, §0.3 outcome enum, §0.4 required
telemetry + supporting columns + gate-consumer, §2 rerun ceilings, §3.6 W-PRUNE telemetry, §4–§10 per-wave
telemetry blocks). Cross-read against S-P2 §3 sequencing (`research/p2/SYNTHESIS-RESEARCH.md`) + the 6
addenda + R16 (`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`).

Lens question: do the required telemetry columns + the gate-consumer cover every close-condition predicate,
each column produced by a named wave? Any close predicate with no telemetry, or column with no producing
wave? Verdict per claim: ACCEPT / REVISE / REJECT. Proportionate: a wording nit is a REVISE only if it
would mislead an implementer.

## Method

Built the two-direction completeness ledger:
(A) every §0.1 close-condition predicate (#1–#12) → its telemetry column(s) → the named producing wave that
emits them, and the gate-consumer REJECT clause that consumes them;
(B) every §0.4 schema column (the 13 binding + the supporting set, lines 242–272) → the named producing wave
that emits it (the inverse: a column declared in the schema but produced/consumed by no wave-slice).

## Enumeration (every wave-gate / telemetry / close claim under this lens)

### Direction A — every close predicate has telemetry produced by a named wave: ACCEPT

| Close (§0.1) | Telemetry column(s) | Producing wave(s) | Gate-consumer REJECT |
|---|---|---|---|
| #1 one generator / no blob | `generator_grammar_count`, `verbatim_blob_present` | PROVE (count); G1+G2 (blob) | PROVE §9, G1.4, G2.5 |
| #2 un-forked emitter | `emitter_fork_present`, `generator_grammar_branch_count`, `generator_grammar_type_count`, `emit_shape_source` | G3.5 | G3 5-conjunct |
| #3 relocated seam closed | `runtime_target_rows_collapsed` | P3 §3.6, G3.5 (also G2.5, PROVE re-assert) | P3, G3, G2, PROVE |
| #4 shared trait ≥2 non-collapsible | `json_rich_navigation_preserved`, `shared_trait_impl_count`, `shared_trait_non_collapsible` | G4.4 | G4 3-conjunct |
| #5 phantom `<G>` deleted | `phantom_generic_resolved` | G4.4 | G4 |
| #6 >SOTA honest | `g2_cssparser_oracle_parity`(=`css_typed_summary_equal`), `g2_sota_ratio_held`/`css_sota_ratio_held`, `track1_rich_over_lcss_ratio_pre_g2`, `g1_hot_leaf_preserved`, `g1_json_guard_rows_held`/`json_guard_held`, `corpus_in_timer` | G1.4, G2.5, H1 | G1/G2/G3/G6/H1 |
| #7 x86 gone | `x86_tree_deleted` | P1 §3.6 | P1 |
| #8 Lock-14 meaningful | `lock14_gate_scans_codegen`, `forbidden_generic_tokens_extended` | P4 §3.6 | P4 |
| #9 Sheets negative control | `sheets_grammar_shape`, `sheets_emission_path`(→N), `generator_grammar_count==3`, `sheets_value_instantiates_g4_trait`, `import_closure_relaxation_is_data` | PROVE §9 | PROVE |
| #10 NEON at admission | `acceleration_at_admission`, `simd_admission_caller`, `simd_admission_profile_sampled` | G5/G6 §8 | G5/G6 |
| #11 generated-state clean | `dirty_generated_state`/`regen_check_clean`, `metalang_leak_present`, `generated_md5_distinct` | P3/G3/PROVE/H1; P5; P3/G3/PROVE | P5, H1, G3, P3, PROVE |
| #12 PASS-IMPL / framing | `materialization_framing` | H1 §10 | H1 |

Every close predicate maps to a telemetry column emitted by a named wave AND a gate-consumer REJECT clause.
No close predicate is unfalsifiable; no close predicate is telemetry-less. The two enums whose domain a
gate decides on (`acceleration_at_admission ∈ {admission|dead}`, `sheets_emission_path ∈ {generator-only|
shim}`) are CLOSED and consistent across §0.3 / §0.4 / the G6 / PROVE producing blocks (the schema even
bars a third `acceleration_at_admission` state explicitly, §0.4 line 251). `sheets_emission_path == shim`
is correctly NOT a gate-REJECT but the binding `N` verdict — surfaced, not paper-closed (§9). ACCEPT.

### Direction B — every schema column has a producing wave: ACCEPT except one

- **The 13 binding columns** (lines 242–256): each carries an explicit "MUST be X at <wave>" and each named
  wave's producing block emits it. Verified the multi-wave claims are honored at BOTH waves:
  `runtime_target_rows_collapsed` (G3/P3) → P3 §3.6 + G3.5 ✓; `verbatim_blob_present` (G1/G2) → G1.4 + G2.5 ✓.
- **Supporting columns** (lines 258–272): the §0.4 reconciliation (lines 274–283) attributes each to a named
  consuming wave, and line 274 pins their PRODUCER as "the retained SK-V15/16/17 JSON + CSS schema [that]
  stays in force." Confirmed on disk: `css_typed_summary_equal`, `css_sample_count`, `css_comparator_plane`,
  `css_rich_ast_preserved` are all retained SK-V17 schema columns (present in `sk-v17/SPEC.md`), so their
  producing wave IS the G2/H1 CSS bench row that carries the standing schema — they are NOT orphaned. The
  dual-naming `css_typed_summary_equal` (retained-schema name + §1 standing law) ↔ `g2_cssparser_oracle_parity`
  (G2.5 SK-V18-framed restatement) is ONE measurement under two names, both on the same G2 row. ACCEPT.
- **`named_primitive_falsifier_pass`** (line 268) is the umbrella name; line 283 explicitly realizes it
  per-wave as `g1_leaf_primitive_abcd_pass` / `g2_balanced_scan_primitive_abcd_pass`. Mapped producer. ACCEPT.
- **`projection_generality_exercise` / `simd_non_json_exercise`** (§2.1 line 519–520) appear once, in no
  telemetry block and no REJECT clause. They are standing CH7/Lock-14 generality-lens riders (which grammars
  exercise the non-JSON proof), NOT `--skv18-generalization-report` gate columns and NOT close predicates;
  they fall outside the "column emitted-but-not-consumed" invariant. Weakest spot, but not a telemetry gap.
  ACCEPT.

## The one residual REVISE

**R-CH4.1 — §0.4, line 254: `corpus_in_timer` schema descriptor over-attributes wave G6 (a column whose
named producing/consuming wave does not emit or consume it).**

The §0.4 schema declares:

  `corpus_in_timer (bool; addendum 5 — MUST be true at G2/G6/H1; ...)`

But the named wave **G6 produces no `corpus_in_timer` column and the G6 gate does not consume one**:
- The G5/G6 producing telemetry block (SPEC lines 1368–1377) has 10 columns; `corpus_in_timer` is absent.
- The G5/G6 gate-consumer REJECT clause (lines 1380–1388) does not check `corpus_in_timer`.
- G6 DEFERS the entire corpus-in-timer measurement to H1 by design: `g6_speedup_median_mbps` is null pre-H1,
  the corpus-in-timer figure is "produced at H1" (line 271/1375), and "the G6 outcome is `C` until H1
  produces the figure" (line 1351). So at G6 there is no corpus-in-timer measurement to assert true.
- Confirming the stray: the §0.4 reconciliation enumeration (lines 274–283) — which lists every column's
  consuming wave — does NOT list `corpus_in_timer` at all; its wave attribution lives ONLY in the line-254
  inline descriptor. `corpus_in_timer` IS genuinely produced+consumed at G2 (G2.5 line 1028 + REJECT line
  1039) and H1 (line 1568 + REJECT line 1577). G6 is the false third.

Why it misleads an implementer (so it clears the proportionality bar): a gate-json author reading the
binding 13-column schema would wire a `corpus_in_timer == true` check into the G6 row, which the G6 producer
never emits — yielding a spurious missing-column FAIL / false-RED on every G6 close, even though G6's real
corpus-in-timer obligation correctly lands at H1. This is the lone "column with a declared producing wave
that has no producing slice for it" in the doc. It is a precision defect, not a broken sequence or an
addenda violation (addendum 5's corpus-in-timer obligation IS covered, at G2 + H1), so REVISE, not REJECT.

EXACT one-line edit (§0.4, line 254): change

  `corpus_in_timer                  (bool; addendum 5 — MUST be true at G2/G6/H1; ...)`

to

  `corpus_in_timer                  (bool; addendum 5 — MUST be true at G2/H1; the corpus-in-timer figure is DEFERRED from G6 to H1, so G6 emits no corpus_in_timer column; ...)`

(i.e. drop `G6` from the MUST-be-true wave list — G6's corpus-in-timer figure is the H1-produced
`g6_speedup_median_mbps`, asserted under `corpus_in_timer` at H1, not at G6).

## No REJECT

No unfalsifiable gate (every gate has a turning-RED falsifier + a closed-domain enum where it decides on
one). No broken sequence under this lens (telemetry producers align with the seq §3 / lattice §2.1 entry
gates; the seq/C6+C7 and s6/C4 folds do not introduce a telemetry-less predicate). No addenda violation
(the 6 addenda + R16 each bind a telemetered column with a producing wave + a REJECT clause). The lone
defect is the G6 over-attribution above.

TALLY accept=15 revise=1 reject=0
