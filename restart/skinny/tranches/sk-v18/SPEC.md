# SK-V18 SPEC — S-P3 Wave Plan (ONE Grammar-Driven Generator Emitting JSON + CSS + Sheets From `.bbnf`, >SOTA Preserved Honestly, aarch64-Only)

Date: 2026-06-01.

Status: S-P3 Synthesis-PLAN packet (cycle V5 — folds the V5-independent GROUND re-validation
disposition: `sota.md` accept=13/0/0 with one P5↔G1 call-site sequencing note carried into G1;
`seq.md` accept=8 revise=2 — C6/C7 transcription-defect corrections folded so PROVE never admits
before G4 closes and G5/G6 hangs off G3 parallel-to-G4, NOT under G4; `s6.md` accept=6 revise=1 —
C4 folded so the `balanced_component_scan` neutrality demotion to `css_balanced_component_scan` is
the FORCED outcome for the named candidates, since the offered non-CSS dischargers are
parse-with-emit descents structurally incompatible with the CSS byte-SKIP shell). This file is NOT
an implementation dispatch. It folds Pass Alpha's SK-V18 goalset, the S-P1 profile
(`research/p1/SYNTHESIS-PROFILE.md`), the S-P2 converged candidate shortlist + lattice
(`research/p2/SYNTHESIS-RESEARCH.md`), the S-P0 audit-overfit addenda + PRUNE-list + R16
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`), and the S-P3 PA–PE cohort
(`research/p3/{pa,pb,pc,pd,pe}-*.md`) into a PRUNE-before-rebuild W-PRUNE→G1..G6→PROVE→H1 wave
plan. Live audit HEAD `83b66db42`.

SK-V18 is the GENERALIZATION cycle — the inflection backtrack: two hand-written/forked parsers
(JSON + CSS) collapse into ONE grammar-driven generator emitting JSON + CSS + Sheets from `.bbnf`,
preserving >SOTA (CSS beats lightningcss 1.66–3.38×; JSON beats sonic-rs strict). Net LOC ≈
**−10800** (the campaign DELETES far more than it adds).

Authority:

- `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md` (the hot leaves: CSS
  `find_component_delim` 79.5% / scalar-scan 94.1%; JSON `parse_object_value_at_direct` +
  `parse_array_element_at_direct` 91.5%; G6=WIRE; the load-honesty caveat §0; json/scan.rs
  zero-sampled).
- `restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md` (the candidate shortlist
  R-A..R-F §1, the coupling lattice §2, the PRUNE→G1..G6→PROVE→H1 per-wave entry/exit-gate
  sequencing §3, the §6 findings + the (a)-(d) primitive gate §4, the residual risks §5).
- `restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (the 6 binding addenda
  §1, the residual census R1..R16 + R-A0-1..3 §2, the PRUNE-list P1..P5 §4, the sequencing
  constraints §5, the CH7 lens §6, the §6 named-primitive escape).
- `restart/skinny/tranches/sk-v18/research/p3/{pa-prune-waves,pb-g1-g2-waves,pc-g3-g4-waves,
  pd-g5g6-prove-h1-waves,pe-gate-telemetry-close}.md` (the S-P3 cohort folded here).
- `restart/skinny/tranches/sk-v18/research/p2/hardening/V5-independent/{sota,seq,s6}.md` (the GROUND
  re-validation findings folded into the relevant waves below).
- `restart/skinny/tranches/sk-v17/SPEC.md` (the STRUCTURE TEMPLATE — Section 0/1/2 mirrored).

Host: aarch64 / Apple M5 Max ONLY. x86 is a PRUNE target (P1), not a measured plane. NO x86, NO
AVX-512, NO SVE, NO i8mm-without-antecedent.

Dispatch lock:

- No SK-V18 implementation wave dispatches from S-P3 itself.
- W-PRUNE (P1–P5) is the ONLY dispatch-now-eligible cluster on close of S-P3; every
  GENERALIZE/PROVE/HONESTY wave remains blocked until its predecessor closes its exit gate AND its
  entry-gate predicate holds GREEN AND the orchestrator/user dispatches the wave triumvirate.

---

## Section 0 — Close Condition And Goalset

### 0.1 — Global Close Condition

SK-V18 is the GENERALIZATION cycle — the inflection backtrack. It closes only when ALL of these are
simultaneously true. Every surface citation is the benched skinny tree (`skinny/crates/`); each
generator/gate is verifiable by grepping `skinny/crates/`, NOT `crates/core/` (the TOTALITY tree is
the SK-V19 adoption target, not the SK-V18 benched surface). Net LOC ≈ **−10800**.

1. **ONE generator emits JSON + CSS + Sheets from `.bbnf`.** A single grammar-driven generator
   exists in `skinny/crates/codegen/`; it consumes the three `.bbnf` roots (JSON, CSS, Sheets) and
   emits three NON-IDENTICAL `generated.rs` parsers, each grammar-DERIVED — NOT a const `&str`
   courier, NOT a fixed-literal `render()`, NOT a relabeled blob. `generator_grammar_count == 3`
   (json + css + sheets — NOT json + 7-css + sheets, the P3 collapse, R-A0-2). The CSS const courier
   (`runtime_generator.rs:701 CSS_GENERATED_RS`) and the JSON 7× `push_str` fixed-literal
   (`json_sink_direct.rs`) are both RETIRED; `verbatim_blob_present == false` campaign-wide.

2. **One un-forked emitter path, dispatched on the LOWERED program, not a grammar tag.**
   `RuntimeEmitterKind{CompiledLowering,RequestFacts}` (`grammar_provider.rs:39-43`) is DELETED;
   `runtime_generator.rs:16` no longer forks on a grammar-family kind. `render(program)` reads its
   output-shape ONLY from `program.policy_summary.backend_shape` (`sink_only.rs:48`, the
   grammar-NEUTRAL 5-shape `BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`,
   `lower/mod.rs:18`), NEVER from a `RuntimeTarget` field. `emitter_fork_present == false`;
   `generator_grammar_branch_count == 0`; `generator_grammar_type_count == 0`;
   `emit_shape_source == lowered_program`.

3. **The relocated seam is closed STRUCTURALLY, not just by arm-grep.** The 7 byte-identical
   `css_l4_*/generated.rs` replicas (md5 `b654562c…`) collapse to ONE CSS config; the 7 xtask
   `RuntimeTarget` rows collapse via the R16 recipe — `RuntimeTarget: PartialEq` full-row derive
   (+1 line; today `Clone,Copy,Debug` only, `regen.rs:5`) that recurses into BOTH nested structs
   `frontend_requirements` (field #11, `regen.rs:17`) AND `output_labels` (field #12, `regen.rs:18`)
   automatically. `runtime_target_rows_collapsed == true`.

4. **The shared value-API trait — both JSON and CSS (and Sheets) instantiate ONE seam.** A thin
   `Cursor`/`DocumentView` micro-trait over the surviving `ValueRef<K>` is extended to CSS and
   Sheets; it shares ONLY the laziness/cursor contract, NEVER navigation. JSON's rich tree
   (`get`/`pairs`/typed `JsonValue`/recursive visitor) is preserved by a CONCRETE FALSIFIER, NOT
   "by construction" — `json_rich_navigation_preserved == true` is PROVEN by the G4.2-conjunct-2
   byte-equal diff (JSON's `value.rs` navigation surface diffs EMPTY vs its pre-G4 form ∧ JSON 51/51
   held), an "asserted by construction" close is the unfalsifiable-gate hazard and is REJECT. ≥2 real
   impls (JSON `ValueRef<K>`, CSS `CssNode`) PROVEN NON-COLLAPSIBLE by the G4.2-conjunct-3 falsifier
   (`shared_trait_non_collapsible == true`: removing either impl's navigation surface does NOT compile
   against the other's), NOT a bare ≥2 count. (The trait may NOT LCD-flatten JSON's rich navigation to
   a CSS-flat sweep; §5-risk-4.)

5. **The phantom `<G>` is resolved by DELETE.** `tape/mod.rs:175 ValueRef<…G: EventGrammar =
   AnyGrammar>` + the latent `DocumentView`/`type Root` (`:227-232`) — zero non-test production
   consumers (the `_proof_compiles` census excluding `_tests.rs` is EMPTY). DELETE the `<G>` axis
   (preserving the REAL `K=Kind` axis untouched). `phantom_generic_resolved == deleted`.

6. **>SOTA preserved HONESTLY — CSS beats lightningcss, JSON beats sonic-rs strict.** On the cold,
   corpus-in-timer canonical harness (`css_canon_bench`, the P2-survivor path), the regenerated
   grammar-driven CSS parser BEATS lightningcss full-CSSOM per corpus by the binding SAME-RUN
   falsifier: `track1_rich/lightningcss > 1.0×` per corpus measured same-run AND not regressed vs the
   grammar-driven CSS parser's OWN pre-G2 baseline `track1_rich/lcss`. That pre-G2 baseline is CAPTURED
   AT G2 ENTRY (the G2 harness measures the pre-G2 tree-checkout AND the post-G2 build in ONE quiet run,
   so the regression falsifier `abs(ratio_postG2 − ratio_preG2)/ratio_preG2 ≤ noise band` FIRES AT G2 and
   is the binding G2-exit gate); the pre-G2 figure cannot be re-derived at H1 (the pre-G2 code is gone
   post-G2), so H1 only RE-CONFIRMS the already-closed G2 close-ratio DIRECTIONALLY against the
   G2-recorded baseline, never re-measures pre-G2. The S-P1 absolute ratios
   (bootstrap 2.190, tailwindcss 3.375, material-components-web 1.658, animate 2.101) are DIRECTIONAL
   antecedents (S-P1 §0, loadavg 4.35, NOT re-locked) — they are the directional expectation, NOT the
   close floor; keying the close on an un-re-locked absolute ratio is the unfalsifiable-gate hazard the
   SAME-RUN comparison replaces (the close compares two figures measured in ONE quiet plane, where an
   absolute load-depressed antecedent cannot). ≥1 regular corpus (animate OR bootstrap) crossing is
   mandatory. JSON's 51/51 strict-vs-sonic-rs cold rows remain admitted, same-plane. The 94.1%
   CSS scalar scan and the 91.5% JSON sink leaves are re-emitted with bodies whose throughput-equivalence
   is PROVEN by the named falsifiers — NOT asserted: the JSON leaves by `g1_hot_leaf_preserved`
   (byte-equivalent inline cfg + sink call sites) ∧ `g1_json_guard_rows_held` (51/51 within ±1.0% of
   `SK-V18-open`), the CSS scan by `g2_cssparser_oracle_parity` (the 9-field EXACT oracle, gate-BEFORE-speed)
   THEN the same-run `track1_rich/lightningcss > 1.0×` ∧ no same-run regression vs the G2-entry-captured
   pre-G2 baseline (the speed falsifier is admissible ONLY after the parity falsifier passes — an
   incomplete arg-derivation diverges from the oracle and is REJECT before any speed row is read).
   `corpus_in_timer == true`. Oracle parity (9-field cssparser / 51-row JSON strict) holds BEFORE any speed
   admission.

7. **x86 is gone (aarch64-only).** BOTH x86 surfaces (`src/x86_64/` 24 files + `ext/x86/` vendored
   ASM + the nasm `build.rs` driver) are DELETED crate-wide; `find …/src/x86_64 …/ext/x86 -type f
   == 0`; crate-wide aarch64-neutral grep clean; `cargo build` + `cargo test --no-run` clean (the
   falsifier per P1). The single-arch kernel surface is the R-F retarget host.

8. **Lock-14 gate is MEANINGFUL (no green-by-exclusion).** `runtime_generator.rs` + the JSON
   sink/typed/template surfaces are moved from the weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` into strict
   `GENERIC_SCAN_ROOTS`; `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`; the
   `diagnostic-x86` exclusion is dropped. `lock14_gate_scans_codegen == true`; the
   re-inject-a-`SHEETS_GENERATED_RS`-token falsifier turns the gate RED (a bare `JsonSink` is NOT
   in the scoped set and would not fire — §3.4), then is reverted. P4 lands BEFORE
   G2/G3 (so the un-forked emitter is neutrality-scanned AS it is authored).

9. **Sheets proves the generalization is REAL (the negative control).** `google-sheets.bbnf` emits
   a working parser THROUGH the un-forked G3 generator ONLY — its 7-level precedence tower is
   structurally unlike both JSON and CSS, so it CANNOT be a relabeled JSON/CSS courier.
   `sheets_grammar_shape == pratt-operator` (non-hollow); Sheets `generated.rs` md5-DISTINCT from
   JSON ∧ CSS; no `const.*_RS.*r#` Sheets blob; the Sheets value type instantiates the G4 trait.
   `w5a_sheets` flips from "fails closed: missing import closure" to "emits a working parser" via the
   import-closure-DERIVED-FROM-FACTS relaxation (a frontend-requirements DATA change, NOT a `match
   grammar` arm). **BINDING FALLBACK:** if Sheets cannot emit via the generator ONLY, generalization
   is NOT real — surface HONESTLY, do NOT stub-prove, do NOT hand-write a `_GENERATED_RS` Sheets
   block (§5-risk-5).

10. **NEON acceleration reaches the hot path AT ADMISSION.** The checkasm-gated
    `bbnf-simd::find_ascii_set_member64`/`byte_class_from_eq_set_64` kernel is RETARGETED onto the
    scalar recursive shell of `find_component_delim` (the measured 94.1% CSS hot leaf), landing as a
    SHARED grammar-neutral runtime primitive the (P3-collapsed singular) generated scan CALLS — NOT
    bespoke vector code re-emitted per-grammar. `acceleration_at_admission == admission` is proven by
    BOTH binding conjuncts (a source-census-only proof its own §8/G6 gate REJECTs): (i) the
    generated-`generated.rs` caller census (`rg runtime_simd::find_… …/grammars/*/generated.rs`
    non-empty), NOT a `#[cfg(test)]` caller, AND (ii) `simd_admission_profile_sampled == true` (the
    runtime-reachability conjunct — the `runtime_simd` entry appears in the `css_canon_bench` samply
    sample with non-zero self-time; a `generated.rs` call site in dead/unreachable code that the census
    sees but the profile does NOT == `dead`, NOT `admission`). The zero-sampled `json/scan.rs` (R12) is
    neutralized/retired (cheap; no JSON classifier authored — S-P1 has NO JSON G5 hot leaf).

11. **Generated-state cleanliness.** All grammar parsers + xtask rows are fresh generator output;
    `cargo xtask regen --check` exit 0; no hand-patched `// @generated` file; no metalang leak
    (`parse_w11_1_number` → `parse_number`; no `w[0-9]+`/corpus/`sk_v` tag in shipped runtime, P5).

12. **PASS-IMPL close audit.** The close audit accepts every axis or records a row-level
    intrinsic-block proof with measurement; the H1 CSS framing honesty is disclosed
    (`materialization_framing == lazy-rich-vs-eager-cssom`, the honest S-P1 framing).

### 0.2 — Comparator Classes

Two >SOTA comparators, each on its honest plane. Strict admission is EXECUTABLE, not prose.

| Subject | Comparator (the bar) | Plane | Admission role |
|---|---|---|---|
| CSS L4 `track1_rich` (grammar-driven, post-G2/G3) | lightningcss full-CSSOM | lazy-rich typed projection vs eager full CSSOM build | THE CSS >SOTA bar; cold, corpus-in-timer, same-run, N≥50 median; the binding gate is the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no same-run regression vs the pre-G2 baseline — and that pre-G2 baseline is CAPTURED AT G2 ENTRY (the G2 harness times the pre-G2 tree-checkout and the post-G2 build in ONE quiet run), so the regression falsifier FIRES at G2 exit and H1 only re-confirms the G2-recorded ratio directionally (the pre-G2 figure is NOT re-derivable post-G2); the S-P1 absolute ratios are DIRECTIONAL antecedents, not the floor |
| CSS L4 `track1_full_parse` | lightningcss full-CSSOM | recognition-only vs full CSSOM | recognizer floor; A-shaped but recognition-only (`parse_full`, ZERO typed-field materialization vs `track1_rich`'s 9 materialized fields, S-P1 §3), NOT preserve-rich-ast — does NOT by itself discharge the typed close |
| CSS cssparser 9-field oracle | — | typed equality reference | EXACT correctness anchor; gate BEFORE speed; structurally distinct from `track1_rich` |
| JSON Track 1 (SinkOnly direct, post-G1) | sonic-rs strict | strict-vs-strict, same-plane | THE JSON >SOTA bar; the 51/51 cold strict guard carried from the W0 lock |
| JSON Track 2 / 51-row oracle | — | strict equality reference | per-iter equality anchor; structurally distinct from Track 1 |

**CSS framing honesty (the binding H1 disclosure, R14 / R-A0-1).** The CSS bar is
`lazy-rich-vs-eager-cssom`: `track1_rich` (`rich_summary`, 9 materialized fields) is LAZY-RICH — it
re-derives every field from `(source, offset)` spans, writing NOTHING to the arena
(`css_l4_declaration_values/generated.rs:297-304`), comparable to lightningcss's eager full-CSSOM
build (equal-depth typed value work, not a count-only structural probe). The honest framing MUST be
disclosed EXPLICITLY at H1: `materialization_framing == lazy-rich-vs-eager-cssom`. An unqualified
"beats CSSOM" / "equal-work" claim behind a re-label, WITHOUT the materialization-depth asymmetry
disclosed, is a REJECT (R-A0-1). The symmetric-comparator branch is preferred; the re-label branch is
acceptable ONLY with the asymmetry disclosed.

**JSON framing honesty.** JSON is strict-vs-sonic-rs strict, same-plane, per-iter oracle equality;
no framing asymmetry — the existing W0-locked >SOTA proof, carried forward.

**Load-robustness caveat (cross-cutting, S-P1 §0 / §5-risk-7).** The S-P1 absolute Mbps capture ran
under concurrent-session load (loadavg 4.35) and is DIRECTIONAL, NOT re-locked. The load-robust
ground-truths are (1) the same-run `track1_rich/lcss` ratios and (2) the relative hot-leaf rank. A
QUIET re-capture is required before ANY absolute Mbps claim in H1; G6 may report only its checkasm
PASS/FAIL pre-H1, and any speedup FIGURE comes from the corpus-in-timer symmetric harness (same
plane both sides), deferring the speedup CLAIM to the H1 symmetric timer.

### 0.3 — Outcome Enum

```text
A   admit-shaped (> the >SOTA bar, on its honest plane)
C   correctness (oracle parity; gate before speed)
G   GO (substrate/gate green, no behavior-regressing change)
L   loss (crosses no bar; honest residual recorded, NOT paper-closed)
N   negative-control fail (Sheets cannot emit via the generator ONLY → generalization NOT real) /
    not-applicable-not-needed (e.g. G5 json/scan.rs retire — nothing on product path)
P   prune-clean (a P-wave: deletion + gate-tightening verified, zero generalization risk)
R   redress (a wave failed its exit gate; blocks every downstream entry-gated wave)
S   substrate-guard / non-SOTA / honest-residual (admission-capable parse that does not cross the bar; measured, recorded, NOT paper-closed)
```

`A`/`C`/`G`/`L`/`S` are valid emitter-wave outcomes. The BINDING negative-control verdict for PROVE
(Sheets) is: a Sheets emission needing a shim (Sheets cannot emit via the generator ONLY) is the
negative-control fail `N` per the enum above — surfaced honestly, never `S`, never paper-closed,
NEVER stub-proved; `L` is reserved for an honest non-Sheets residual loss (a wave that crosses no bar
but does not itself prove generalization unreal). Generalization is NOT real on `N` and is surfaced honestly. A NEON speedup CLAIM (G6) is `A` ONLY when the
timed-plane binding holds AND the H1 quiet-bar holds (`host_loadavg < 1.0` on the corpus-in-timer
re-capture); a corpus-in-timer figure produced under `host_loadavg >= 1.0` (or with no `host_loadavg`
stamp) is `S` (DIRECTIONAL honest residual with the load caveat), NOT `A`; a checkasm PASS without ANY
corpus-in-timer figure is `C` (correctness proven, speedup deferred to H1), NEVER `A`. A recognition-only
`A` (`track1_full_parse`, recognition-only — `parse_full`, ZERO typed-field materialization vs `track1_rich`'s
9 materialized fields, S-P1 §3 — NOT preserve-rich-ast) does NOT by itself discharge the CSS typed
>SOTA close. A `P` wave
carries zero generalization risk (pure deletion + gate-tightening) and deletes no >SOTA-bearing code.

### 0.4 — Required Telemetry (the `--skv18-generalization-report` schema)

The gate consumer is `(cd skinny && cargo xtask gate-json --check-results
--skv18-generalization-report <path>)`. It REJECTs the run if ANY required column is missing,
mis-typed, or a producer-only field (a column emitted but never consumed FAILS the wave —
`[typed-materialization-invariant]`). The 13 binding generalization columns (each grounded in a
named addendum / residual):

```text
verbatim_blob_present            (bool; addendum 1 — MUST be false at G1/G2 close; a @generated &str literal in codegen = REJECT)
generator_grammar_branch_count   (int;  addendum 2/3 — MUST be 0 at G3; a generic branch selecting on grammar name/family = REJECT)
generator_grammar_type_count     (int;  addendum 2/3 — MUST be 0 at G3; a grammar-named type discriminating the emit path = REJECT)
runtime_target_rows_collapsed    (bool; addendum 2/R16 — MUST be true at G3/P3; the STRUCTURAL co-gate the arm-grep cannot see; via RuntimeTarget: PartialEq full-row over BOTH nested structs)
emit_shape_source                (enum {lowered_program|runtime_target}; addendum 3 — MUST be lowered_program at G3; render(program) reads NO target.*field — the relocated-seam falsifier)
emitter_fork_present             (bool; addendum 3 — MUST be false at G3; RuntimeEmitterKind/CompiledLowering/RequestFacts grep == 0)
phantom_generic_resolved         (enum {deleted|instantiated|present}; addendum 4 — MUST be deleted at G4; the <G>=EventGrammar axis, test-excluded)
json_rich_navigation_preserved   (bool; addendum 4 — MUST be true at G4; makes the ≥2-impl count necessary-not-sufficient; preserve-rich-ast)
acceleration_at_admission        (enum {admission|dead}; addendum 6 — MUST be admission at G6; admission REQUIRES BOTH conjuncts — a generated.rs hot-loop caller census non-empty (the source proof) AND `simd_admission_profile_sampled == true` (the runtime-reachability proof: the runtime_simd entry appears in the css_canon_bench samply sample with non-zero self-time); a #[cfg(test)]-only-or-unreachable caller, OR a generated.rs call site in dead/unreachable code that the census sees but the profile does NOT, == dead; the enum is the SAME two-value domain §0.3 and §8/G6 decide on — a third state would make the gate non-deterministic between the schema and the G6 falsifier)
sheets_grammar_shape             (enum {pratt-operator|flat-stream|tree|courier|hollow}; addendum 2 / R-E — MUST be pratt-operator at PROVE; any other shape (flat-stream/tree = <7 chained level fns per the §9 falsifier; courier/hollow = relabeled) REJECTs)
generator_grammar_count          (int;  addendum 2 — MUST be 3 at PROVE (json+css+sheets); 7-css inflation = the P3 overfit, REJECT)
corpus_in_timer                  (bool; addendum 5 — MUST be true at G2/H1; the corpus-in-timer figure is DEFERRED from G6 to H1, so G6 emits no corpus_in_timer column — G6's corpus-in-timer figure is the H1-produced g6_speedup_median_mbps, asserted under corpus_in_timer at H1; the real corpus inside the timed region, cold, no warm micro-fixtures, no more-work competitor)
materialization_framing          (enum {lazy-rich-vs-eager-cssom|undisclosed}; addendum 5/R14 — MUST be exactly lazy-rich-vs-eager-cssom at H1; the enum is CLOSED to these two values so the gate can REJECT any other (an open `|...` accepts any string and is unfalsifiable); `undisclosed` is the pre-H1 default and turns the H1 gate RED; the honest CSS framing, no unqualified "beats CSSOM" behind a re-label)
```

Supporting columns the gate also consumes (per-wave evidence the above 13 lean on):

```text
css_corpus / css_sample_count(>=50) / css_sample_statistic(median) / css_sample_mode(cold)
css_comparator_plane(==full-cssom for the lightningcss bar — the plane-match column the G2/H1 gate REJECTs on; consumed at G2/H1)
css_track1_rich_median_mbps / css_lightningcss_full_cssom_median_mbps / track1_rich_over_lcss_ratio
track1_rich_over_lcss_ratio_pre_g2(the grammar-driven parser's OWN pre-G2 baseline, CAPTURED AT G2 ENTRY in one quiet run alongside the post-G2 build; the close-condition-#6 regression anchor the G2-exit gate fires on and H1 re-confirms directionally — NOT re-measured at H1, NOT the load-depressed S-P1 absolute)
css_typed_summary_equal(9-field cssparser oracle; gate before speed) / css_rich_ast_preserved
json_strict_rows_admitted(51/51) / json_sonic_rs_strict_delta / g1_hot_leaf_preserved(the G1 producer the close-condition #6 names; G3 re-asserts it as g3_json_hot_leaf_preserved)
generated_md5_distinct(no byte-identical pair across generated.rs) / dirty_generated_state(clean|retired)
lock14_gate_scans_codegen / forbidden_generic_tokens_extended / named_primitive_falsifier_pass(per (a)-(d))
shared_trait_impl_count(>=2) / shared_trait_non_collapsible(the G4.2-conjunct-3 substitution falsifier; the necessary-NOT-sufficient companion to the bare count)
simd_admission_profile_sampled(the addendum-6 runtime-reachability conjunct — the runtime_simd entry appears in the css_canon_bench samply sample with non-zero self-time, NOT a source grep)
host_loadavg(the 1m loadavg stamped on the H1 capture; an ABSOLUTE Mbps claim REQUIRES < 1.0) / g6_speedup_median_mbps(corpus-in-timer ONLY; null pre-H1, produced at H1)
```

The retained SK-V15/16/17 JSON + CSS schema stays in force for the guard rows. EVERY supporting column
above is consumed in a named wave slice (no producer-only field): `css_sample_*`/`css_track1_*`/
`track1_rich_over_lcss_ratio*` consumed at G2/H1; `css_typed_summary_equal`/`css_rich_ast_preserved`
at G2 (gate-before-speed); `json_strict_rows_admitted`/`json_sonic_rs_strict_delta`/`g1_hot_leaf_preserved`
at G1 (the 51/51 guard + the 91.5% leaf); `generated_md5_distinct`/`dirty_generated_state` at P3/G3/PROVE;
`shared_trait_impl_count`/`shared_trait_non_collapsible` at G4 (the ≥2-impl count + its non-collapsibility
falsifier); `simd_admission_profile_sampled` at G6 (the runtime-reachability conjunct of
`acceleration_at_admission`); `host_loadavg`/`g6_speedup_median_mbps` at H1 (the quiet-bar + the deferred
speedup figure); `lock14_gate_scans_codegen`/`forbidden_generic_tokens_extended` at P4 (the re-inject
falsifier); `named_primitive_falsifier_pass` at G1/G2 (the per-(a)-(d) `*_abcd_pass` arms). The gate REJECTs: any
CSS row missing `css_sample_count>=50` / `css_sample_statistic==median` / `css_sample_mode==cold` /
`css_comparator_plane==full-cssom` for the lightningcss bar / `css_typed_summary_equal==true` before
speed / `css_rich_ast_preserved==true`; any G1 row with `g1_hot_leaf_preserved != true` or
`json_strict_rows_admitted != 51`; any P4 row with `forbidden_generic_tokens_extended != true`; any
named-primitive row with `named_primitive_falsifier_pass != true`; any row whose corpus is not in the
benched set (no phantom `normalize`); any single-tuple broadcast (`sample_count==1` or one tuple across
multiple corpus rows). Every emitted field is consumed in the same wave.

### 0.5 — Opening Row Goalset

The CSS >SOTA bar is per-corpus, against lightningcss full-CSSOM, cold N≥50 median, corpus-in-timer.
The benched corpus set is fixed (`css_l4_corpus.rs`): `{bootstrap, tailwindcss,
material-components-web, animate}`. `normalize` is NOT in this set. The S-P1 absolute ratios are
DIRECTIONAL antecedents (captured under loadavg 4.35, S-P1 §0, NOT re-locked) — the binding per-corpus
gate is the SAME-RUN falsifier `track1_rich/lcss > 1.0×` AND no same-run regression vs the parser's OWN
pre-G2 baseline (`track1_rich_over_lcss_ratio_pre_g2`, both measured in one quiet plane so a uniform
host depression cancels). The absolute column is the expectation, NOT the floor:

| Corpus | S-P1 `track1_rich/lcss` directional antecedent | Close-state obligation (the BINDING same-run gate) | Fallback if regressed |
|---|---:|---|---|
| bootstrap (regular) | 2.190 | `> 1.0×` same-run ∧ no same-run regression vs pre-G2, cold N≥50, corpus-in-timer | REDRESS G2; record hot-leaf attribution; do NOT paper-close |
| tailwindcss (hardest) | 3.375 | same-run `> 1.0×` ∧ no regression; load-tail dispersion noted (S-P1 stddev 159.8) | honest residual recorded; not tranche-blocking provided ≥1 regular holds |
| material-components-web (full) | 1.658 (smallest antecedent) | same-run `> 1.0×` ∧ no regression; the full-corpus integration check | report median delta honestly |
| animate (regular) | 2.101 | same-run `> 1.0×` ∧ no same-run regression vs pre-G2 | REDRESS G2; record residual |

JSON guard floor: all 51 JSON rows maintain A/GO strict same-plane vs sonic-rs strict, throughput
within ±1.0% of `SK-V18-open`, across every generalization wave (the G1 91.5% hot-leaf MUST-preserve).

Tranche-level success criterion: **the ONE grammar-driven generator emits JSON + CSS + Sheets, each
grammar-DERIVED, with the shared trait instantiated ≥2 ways, the phantom deleted, x86 gone, the
Lock-14 gate meaningful, AND the per-corpus CSS >SOTA same-run ratio `> 1.0×` with no same-run
regression vs the pre-G2 baseline (the S-P1 absolutes are DIRECTIONAL antecedents, not the floor; ≥1
regular corpus mandatory) AND JSON 51/51 held.** If a generalization wave proves a grammar-derived parser
CANNOT preserve the >SOTA without hand-shaping, that is a genuine §6 finding — admitted ONLY as a
named, (a)-(d)-gated, grammar-parameterized primitive (Section 1), recorded honestly, NEVER a silent
blob. The honest re-capture of absolute Mbps under quiet load is required before any absolute claim
(H1).

---

## Section 1 — Non-Negotiables (standing law on every wave)

**The 6 binding addenda (SYNTHESIS-AUDIT §1) — each a REJECT trigger on every wave CHALLENGE:**

1. **verbatim-blob.** A `@generated` file that is a verbatim `&str` literal in codegen is
   hand-written, NOT derived — REJECT as "grammar-driven". `verbatim_blob_present == false`
   campaign-wide (cleared by G1+G2). The CSS-courier LOC (`CSS_GENERATED_RS` ≈910 LOC) is
   cohort-carried, NOT gate-keyed; the binding gate is the boolean + the `.bbnf`-mutation test.
2. **distinct-grammar-output.** N grammars ⇒ N non-identical `generated.rs`; md5-distinctness is
   NECESSARY-NOT-SUFFICIENT — the 3-co-gate CONJUNCTION is {md5-distinct ∧
   `generator_grammar_branch_count==0` ∧ `generator_grammar_type_count==0` ∧
   `runtime_target_rows_collapsed==true`}. The relocated seam (a per-grammar branch moved into a
   neutral-identifier data table) is caught ONLY by the structural row-collapse, never by a regex.
3. **single-emitter-path.** ONE grammar-agnostic emitter; any grammar-family fork is REJECT.
   `emitter_fork_present==false`; `emit_shape_source==lowered_program` (the emitter reads its
   output-shape from the LOWERED PROGRAM, never from
   `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`
   — the §5-risk-1 relocated-seam binding; the field-set is IDENTICAL to the G3 exit-gate conjunct 5
   grep, so the standing seam-scan is as strong as G3's own).
4. **phantom-generic.** A generic `<G>` never instantiated with a real type outside `#[cfg(test)]`
   is decorative — instantiate-or-delete (DELETE default; the `_proof_compiles::<JsonEventGrammar>`
   is test-only and must NOT false-green). Points at the `G`=EventGrammar axis, NOT the REAL `K`=Kind
   axis. The companion `json_rich_navigation_preserved==true` makes the ≥2-impl count
   necessary-not-sufficient (preserve-rich-ast).
5. **timed-plane-symmetry + corpus-in-timer.** The >SOTA comparator does EQUAL work on the REAL
   corpus, COLD (no micro-fixtures, no more-work competitor; canonical `css_canon_bench` is honest).
   `corpus_in_timer==true`. The warm 2000-iter micro-fixture path (`nonjson_css_l4.rs:3091
   measure_mbps`) is DELETED (P2). Any Mbps figure comes from the symmetric corpus-in-timer harness.
6. **acceleration-wiring.** A NEON/ASM acceleration claim must show the kernel reached AT ADMISSION
   (hot path), not only under `#[cfg(test)]`. The RETIRE branch is gated on a samply non-top-N
   MEASUREMENT (S-P1 profile-first), not an assertion; every primitive lands WITH its hot-path
   consumer in the same commit (no orphan kernel). `acceleration_at_admission==admission`.

**The §6 named-primitive escape — the (a)-(d) gate (SYNTHESIS-RESEARCH §4, the single largest
paper-close surface, R-A0-3).** A fully grammar-derived parser that CANNOT preserve the >SOTA without
a hand-shaped core admits that core ONLY as a NAMED, `.bbnf`-INVOKED, grammar-DERIVED-data,
machine-gated primitive — NEVER a silent blob, NEVER a paper-close. Each primitive is admissible ONLY
under ALL FOUR:

- **(a) grammar-INVOKED-by-name** — the `.bbnf` rule names/invokes the primitive (machine-checked).
- **(b) emitted-output-VARIES-under-invoking-rule-mutation** — mutate the invoking `.bbnf` rule ⇒
  the emitted ARG byte-sets / numeric class change (the BYTE-SET/numeric-class mutation; a kernel that
  does NOT vary under its own rule's class mutation is a relabeled fixed courier, even if the
  surrounding skeleton varies — byte-equivalence alone is satisfiable by routing the SAME literal
  through the new walk, so (b) is what distinguishes derived from relabeled).
- **(c) `verbatim_blob_present == false`** — no surviving verbatim literal.
- **(d) PROFILE-PROVEN-NARROW-LEAF** — the primitive covers a SINGLE hot leaf attributable to a named
  S-P1 hot leaf (one scan/classify/emit kernel); the surrounding structural SKELETON MUST be
  walk-derived. A "primitive" spanning a rule's whole body or an unprofiled region is REJECT
  regardless of (a)-(c) (machine-checkable: primitive LOC vs the profiled hot-leaf extent).

A primitive failing ANY of the four is a relabeled hand-written blob = REJECT. The named primitives
this campaign admits: `balanced_component_scan` (CSS, the 94.1% leaf — G2/G6; FORCED-demoted to
`css_balanced_component_scan` per the GROUND s6/C4 finding below), the JSON `string`/`number` leaf
scanners (the 91.5% leaf — G1).

**Neutrality-proof obligation (CH6) — the FORCED demotion (GROUND `s6.md` C4, folded).**
`balanced_component_scan` is named neutrally but exercised ONLY by CSS in this campaign — its
balanced-recognizer SHELL must be PROVEN neutral by at least one NON-CSS invocation invoking the SAME
primitive ELSE it is demoted to an honestly CSS-scoped name. The GROUND re-validation grounded both
offered non-CSS dischargers on disk and found them **structurally incompatible** with the CSS shell:
JSON `{}`/`[]` nesting (`generated.rs:833-834`, dispatches to `parse_object_direct`/`parse_array_
direct` with `sink.*` emit) and Sheets `paren_expr` (`google-sheets.bbnf:137`, descends into
`expression`) are both PARSE-with-emit descents, whereas the CSS shell `consume_balanced_at`
(`generated.rs:693-713`) is a byte-SKIP that emits NOTHING. The "invoke the SAME primitive" branch is
therefore UNREACHABLE by the named candidates, and the demotion to `css_balanced_component_scan` is
the **FORCED, not contingent, outcome** — the campaign ships the CSS-scoped name (a neutrally-named
CSS-only primitive is an overfit-in-waiting). The G2/G6 neutrality gate is discharged by the
CSS-scoped rename; the gate REJECTs a neutral name with zero structurally-compatible non-CSS caller.

**The remaining standing non-negotiables:**

- **Lock 1 — one substrate.** Exactly ONE retained tape (the existing
  `Tape`/`ValueRef`/`PayloadArena`). No second tape, no eager value tree, no parser-owned
  facts/cursor, no sidecar event vector.
- **Lock 14 — grammar-neutral.** No JSON/CSS/Sheets policy in generic crates. All routing is DERIVED
  from the `.bbnf`/`BackendShape` shape, never hand-curated; relocating per-rule branching into
  projection DATA or flag form is the Lock-14 re-entry seam and is FORBIDDEN (the §5-risk-1 relocated
  seam). Every generic-crate edit carries a non-JSON proof (§2.1).
- **preserve-rich-ast.** The typed CSSOM and JSON rich tree are NEVER flattened for speed. CSS
  `track1_rich` stays lazy `ValueRef`-view projection over the tape (lazy not eager — re-derived from
  spans, nothing eager to the arena); JSON's `get`/`pairs`/typed-`JsonValue`/recursive-visitor
  navigation is preserved — PROVEN at G4 by the byte-equal diff of JSON's `value.rs` navigation
  surface vs its pre-G4 form (not "by construction", per close-condition #4)
  (`json_rich_navigation_preserved==true`).
- **dav1d discipline on every primitive.** Scalar reference FIRST + checkasm differential parity (the
  `neon_significant_skip_matches_scalar` guard over the REAL 71KB-495KB corpora) BEFORE wiring;
  same-wave hot-path consumer per primitive (no orphan kernel ships, the SK-V5 failure). The checkasm
  differential is a CORRECTNESS gate only; speedup CLAIMS defer to the H1 symmetric timer.
- **No fact-stream String as admission plane.** `emit_fact_stream`/`CssFullParseSummary` are
  diagnostic-only, never an admission surface.
- **No deferrals / no paper-close.** A wave cannot close on "wired"/"advisory"/"future
  consumer"/"integrated"/"paper close" language without a measured bench row or a turning-RED
  falsifier. No wave closes on a future-phase promise. Every miss becomes REDRESS evidence or an
  explicit routed residual — never a silent retreat.
- **clean-regen.** Generated files are always fresh generator output (delete + regen ⇒
  byte-equivalent); `cargo xtask regen --check` exit 0; no hand-patched `// @generated` file.
- **aarch64 only.** No x86, no AVX-512, no SVE. x86 is DELETED (P1), not a fallback.
- **Strict comparator gates.** No CSS >SOTA admission except against the same-run lightningcss
  full-CSSOM on a matching plane with `css_typed_summary_equal==true` proven before speed; no JSON
  admission except strict-vs-sonic-rs strict, same-plane, per-iter oracle.

---

## Section 2 — Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Impl/redress cap |
|---|---|---|---|---|---:|
| P1 | §3 | PRUNE: DELETE the whole x86 surface crate-wide (aarch64-only) | Dispatchable now (W-PRUNE triumvirate) | 0 source-add; ≈ −4500 deletion; checkasm decouple ≤30 edited LOC | 30 min |
| P2 | §3 | PRUNE: DELETE the warm micro-fixture CSS bench | Dispatchable now | ≈ −700 (warm machinery + SHA256 scaffold + micro-fixtures; small retained-oracle relocation) | 30 min |
| P3 | §3 | PRUNE: collapse the 7 byte-identical css_l4 replicas + RuntimeTarget row-collapse | Dispatchable now | ≈ −5500 (6×910 replica bodies deleted; +1 `PartialEq` derive; ~−40 collapsed rows) | 30 min |
| P4 | §3 | PRUNE: fix the Lock-14 green-by-exclusion gate (MUST LAND BEFORE G2/G3) | Dispatchable now | ≈ +15 (root-list moves + token-list extension + `diagnostic-x86` deletion) | 30 min |
| P5 | §3 | PRUNE: purge the metalang bench-wave-id leak | Dispatchable now | ≈ 0 (rename-only at template source; 1:1 regen) | 30 min |
| G1 | §4 | JSON projection — `SinkOnlyExpr` AST-walk emitter (R-C C1) | Conditional on P-cluster close (P4 live, P5 closed) | ≤450 hand source/test/gate LOC; generated `json/generated.rs` named + diff-audited (net ≈ 0) | ≤90 min wave wall; 30 min/redress |
| G2 | §5 | CSS lowering — `css_balanced_component_scan` primitive + fact-keyed projection (R-B B⊃A) | Conditional on G1 ∧ P3 close (P4 live) | ≤450 hand source/test/gate LOC; new `lower/css_scan.rs` + `css_scan_direct.rs` + primitive shell + arg-derivation; `CSS_GENERATED_RS` DELETED | ≤90 min wave wall; 30 min/redress (MED-HIGH; documented larger cap if arg-derivation under-delivers) |
| G3 | §6 | Un-fork the emitter — DELETE `RuntimeEmitterKind`, dispatch on `BackendShape` (R-A A) | Conditional on G1 ∧ G2 close ∧ P4 live ∧ P3 row-collapse | ≤450 hand source/test/gate LOC; generated output byte-equivalent to G1/G2-closed shipped files (PATH change, not OUTPUT) | ≤90 min wave wall; 30 min/redress (HIGH-risk; documented larger cap if §5-risk-1 seam fires) |
| G4 | §7 | Shared value-API trait + phantom resolution — `Cursor` micro-trait, DELETE `<G>` (R-D A) | Conditional on G1 ∧ G2 ∧ G3 close | ≤450 hand source/test/gate LOC; new `tape/cursor.rs` (directory-module); JSON rich-nav byte-equal | ≤90 min wave wall; 30 min/redress (MED-HIGH; documented larger cap if "trait too thin" forces width REDRESS) |
| G5/G6 | §8 | Neutral scan retarget — NEON onto the CSS scan shell + neutralize json/scan.rs (R-F A) | Conditional on P1 ∧ P3 ∧ G3 close ∧ the S-P1 94.1% hot-leaf measurement (PARALLEL to G4/PROVE) | ≤450 hand source/test LOC; `runtime_simd` entry + scalar twin + checkasm extension; generated SIMD call-site named separately | ≤90 min wave wall; 45 min/redress (MED-HIGH; G5 folds in, LOW) |
| PROVE | §9 | Sheets via the un-forked generator ONLY — precedence-tower core (R-E-2) | Conditional on G3 ∧ G4 close (transitively G1 ∧ P3); OVERLAPS G5/G6 but starts LATER — PROVE entry needs G4 closed (G4 is a DIRECT predecessor, never merely transitive — seq/C6), whereas G5/G6 entry needs only G3, so PROVE can only begin after G4 closes; NEVER dispatch PROVE before G4 closes | ≈ +200 Sheets adoption (≈+30 grammar-root + xtask; the rest generated, named + diff-audited) | ≤90 min wave wall; 45 min/redress (MED-HIGH; tower is the expected break point) |
| H1 | §10 | CSS framing honesty + corpus-in-timer + regen --check clean (the honesty close) | Conditional on G5/G6 ∧ PROVE close | 0 source LOC default; docs/RESULTS/REDRESS reconciliation + symmetric re-measure + regen-check | ≤90 min wave wall; 30 min/redress (LOW) |

**Wave count = 12** (5 PRUNE + G1 + G2 + G3 + G4 + G5/G6 + PROVE + H1; G5/G6 is ONE wave; ≤12 skinny
ceiling, exactly at the cap). Active candidate shortlist = R-A..R-F (6 recommended) over the
PRUNE→G1..G6→PROVE→H1 lattice.

LOC budgets are conjunctive with the per-wave cap and rerun ceilings. They count hand-edited source,
tests, gate/report/schema code, and hand-written doc/result edits. Generated outputs do not consume
the source LOC budget, but every generated file must be named, diff-audited, and in the revert slice.
A plan exceeding its LOC budget or its cap MUST split before dispatch or return REVISE. Per
`[generated-size-budget]` the campaign is net `−10800` — a REDUCTION, no overflow risk.

Phase caps (`[dispatch-hard-cap]`: every dispatch carries "HARD CAP: N min. At 0.9N commit, at N
halt"):

| Phase | Cap |
|---|---:|
| Research | 20 min per agent |
| Plan | 15 min |
| CHALLENGE | 90 min when first-of-class, substrate-touching, primitive, or high-risk (G1/G2/G3/G4/G5-G6/PROVE are all first-of-class) |
| Implementation/redress | 30 min per dispatch (45 min for the MED-HIGH G5/G6 + PROVE cluster), ≤90 min wave wall for G-waves; 30 min for P-waves |

Rerun ceilings (one full gate refresh per wave; a second requires a REDRESS cost note — extra reruns
are REDRESS evidence, not retry room):

| Wave | Focused verification | Rerun ceiling |
|---|---|---|
| P1 | `find …/x86_64 …/ext/x86 = 0`, crate-wide aarch64-neutral grep, `cargo build`/`cargo test --no-run` clean | one gate refresh |
| P2 | `grep measure_mbps\|lightningcss_facts = 0`, `css_canon_bench` green, 9-field oracle retained | one gate refresh |
| P3 | md5-distinct, `runtime_target_rows_collapsed == true` (R16 full-row PartialEq, BOTH nested structs) | one gate refresh |
| P4 | re-inject a forbidden-set token (a `GENERATED_RS`-bearing courier token e.g. `SHEETS_GENERATED_RS`, NOT a bare `JsonSink`) → RED/revert, `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`, `lock14_gate_scans_codegen == true` | one gate refresh |
| P5 | `grep -c parse_w11_1_number = 0`, no `w[0-9]+`/`sk_v` tag, `regen --check` clean | one gate refresh |
| G1 | byte-equiv diff-control, `.bbnf`-mutation falsifier, hot-leaf grep, per-leaf (a)-(d), JSON 51/51 maintain, generated diff | one full gate refresh; second requires REDRESS cost note |
| G2 | oracle parity, >SOTA ratio re-measure, arg-mutation falsifier, neutrality (CSS-scoped), generated diff, JSON 51/51 maintain | one full gate refresh; second requires REDRESS cost note |
| G3 | 5-conjunct exit gate, byte-equivalence regen, JSON/CSS MUST-preserves | one full gate refresh; second requires REDRESS cost note |
| G4 | 3-conjunct exit, no-second-substrate, JSON nav byte-equality | one full gate refresh; second requires REDRESS cost note |
| G5/G6 | scalar/checkasm differential, NEON parity over real corpora, caller census, JSON 51/51 maintain | one full gate refresh; second requires REDRESS cost note |
| PROVE | md5-distinct trio, the 4 addendum-2 co-gates, `sheets_grammar_shape == pratt-operator`, trait instantiation | one full gate refresh; second requires REDRESS cost note |
| H1 | framing disclosure, corpus-in-timer, `regen --check` exit 0, >SOTA ratios held, JSON 51/51, the DEFERRED G6 speedup figure + the QUIET (`host_loadavg < 1.0`) re-capture | the ONE quiet re-capture H1 mandates (the deferred G6 figure + the >SOTA directional re-confirmation) is H1's defining measurement, NOT a rerun; any SECOND quiet re-capture requires a REDRESS cost note; no source-moving rerun unless a producing wave (G6/PROVE) is reopened |

### 2.1 — Generality And Lock-14 Gate (every wave carries this)

Every wave has this exit gate, with extra checks when generic crates are edited:

- **Public API scan.** No new public JSON/CSS/Sheets-named API in generic crates.
- **Grammar-branch scan.** No generic branch selects behavior by grammar name, grammar family,
  corpus name, rule role, object/array role, field name, or layout role.
  `generator_grammar_branch_count==0 ∧ generator_grammar_type_count==0` at and after G3.
- **Relocated-seam scan (the STRUCTURAL co-gate the arm-grep cannot see).**
  `runtime_target_rows_collapsed==true` via the R16 recipe — `RuntimeTarget: PartialEq` full-row
  derive recursing into BOTH nested structs `frontend_requirements` (field #11) AND `output_labels`
  (field #12). A hand-rolled prose-field comparison risks a shallow-compare false-green of EITHER
  nested struct and is FORBIDDEN — the full-row derive is the ONLY admitted mechanism (+1 line; both
  nested structs already derive `PartialEq,Eq`). `emit_shape_source==lowered_program` (the emitter
  body reads NO `target.*`/`contract.*` field) is the companion falsifier: grep the `render(program)`
  body for any read of
  `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`
  == 0 (the SAME field-set as the G3 exit-gate conjunct 5, so the standing scan cannot pass a seam G3 catches).
- **Primitive/table scan.** No generic primitive, SIMD kernel, or classifier embeds CSS/JSON/Sheets
  structural policy unless it is generated byte-set data plus opaque class ordinals with a scalar
  reference and a same-wave consumer (the §1 (a)-(d) gate). The CSS `css_balanced_component_scan` and
  the NEON eq-set kernel take grammar-DERIVED byte-set ARGS as caller data; the kernel does not author
  grammar policy. The balanced-recognizer SHELL carries the CH6 neutrality-proof obligation, FORCED to
  the CSS-scoped name (s6/C4).
- **Role/fact boundary.** Generic code may store/search generated structural class ordinals or opaque
  fact ids, but rule-role/recovery/layout/record-boundary meaning lives ONLY inside generated grammar
  modules keyed by parser state + class/byte.
- **Template/provider boundary.** CSS/Sheets-specific templates/providers remain per-grammar
  surfaces; generic codegen consumes grammar-derived facts, not hand-coded policy under neutral names.
  The import-closure relaxation for a single-file grammar (Sheets) is a frontend-requirements DATA
  change (present-iff-grammar-has-imports, derived from facts), NOT a `match grammar` arm.
- **Witness-emission scan-root coupling (P4).** The `JsonEventGrammar`/`SheetsEventGrammar` witnesses
  live in `runtime/`. IF the un-forked generator EMITS a grammar-named `EventGrammar` literal into the
  generated runtime, the `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` must catch it
  (`EventGrammar`/`*EventGrammar` in the extension) — Sheets is the FIRST grammar to exercise the
  witness-emission coupling. (G4 DELETEs the `EventGrammar` axis entirely, so post-G4 NO `EventGrammar`
  literal can be emitted.)
- **Non-JSON proof (the SK-V18 first-mover triple).** The projection-generality riders exercised are
  JSON + CSS + Sheets (`projection_generality_exercise ∈ {json, css_l4, google_sheets}`); the NEON
  SIMD non-JSON exercise is `css_l4` (`simd_non_json_exercise=css_l4`; S-P1 has NO JSON G5 hot leaf).
  Sheets is the negative-control that proves grammar-DERIVED emission (its precedence tower cannot be a
  relabeled JSON/CSS courier). A wave that lets CSS/JSON/Sheets policy into a generic crate fails the
  generality lens (the CH7 Overfit-Prune lens, binding on every wave).

Allowed grammar-specific surfaces: grammar inputs (`.bbnf`), generated parser output, per-grammar
providers/templates, tests, host/API schema facts. The CH7 lens is binding on every wave: every new
code is grammar-derived (template + grammar metadata + emission command), never hand-written under
`// @generated`; every admit via a real source change, strict same-plane, per-iter oracle; every
generated output passes round-trip; no scaffold-only landing counts as an admit; the §6 escape
qualifies ONLY under (a)-(d).

**The binding wave lattice (the seq/C6+C7 corrections folded — PROVE never admits before G4 closes;
G5/G6 hangs off G3 PARALLEL to G4, NOT under G4):**

```
P-cluster (P1..P5; P5 BEFORE G1; P4 live BEFORE G2/G3; P3 dual-gates G2 + binds G3)
  └─ G1  JSON projection (R-C C1)            entry: P-cluster closed (P4 live, P5 closed)
       └─ G2  CSS lowering (R-B B⊃A)         entry: G1 ∧ P3 closed ∧ P4 live   (DUAL gate)
            └─ G3  un-fork emitter (R-A A)   entry: G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse
                 ├─ G4  shared trait + phantom (R-D A)   entry: G1 ∧ G2 ∧ G3 closed
                 │    └─ PROVE  Sheets (R-E-2)           entry: G3 ∧ G4 closed   (PARALLEL to G5/G6) ──┐
                 └─ G5/G6  neutral scan (R-F A)          entry: P1 ∧ P3 ∧ G3 closed   (PARALLEL to G4/PROVE) ──┤
                                                                                                              └─ H1  honesty close   entry: G5/G6 ∧ PROVE both closed (the two parallel branches join at H1)
```

---

## Section 3 — W-PRUNE: P1–P5 (the deletion + gate-tightening cluster)

Pass: the P-cluster lands FIRST in the standing order (`[refactor-first-order]`: prune → optimize →
grammar/semantic). It carries ZERO generalization risk (pure deletion + gate-tightening, zero
>SOTA-bearing code removed — the warm bench DID NOT produce the headline numbers, S-P1 §0) and
reduces the surface every GENERALIZE wave touches. P1, P2, P3, P5 have NO entry-gate (independent,
dispatchable as soon as the W-PRUNE triumvirate is dispatched). P4 has no entry-gate but a hard EXIT
obligation: it MUST land BEFORE G2/G3. P1–P5 are mutually independent within the cluster (disjoint
file roots: P4 `lock14_baseline.rs`, P2 `nonjson_css_l4.rs`, P5 `json_sink_direct.rs`/`json/generated.rs`,
P1 `bbnf-simd/`, P3 `xtask/regen*.rs` + the 7 `css_l4_*/generated.rs`) and MAY land in parallel
commits provided no two race a shared file (`[agent-orchestration]`: commit before parallelizing; P3
and P4 commit serially if they touch the same `xtask` file, else parallel).

**Disk-truth ledger (re-grepped at HEAD `83b66db42`):** P1 `find …/x86_64 …/ext/x86 -type f` = 28
files; `checkasm_parity.rs` 11 x86_64 tokens (9 active compile-coupled call sites). P2
`grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` = 48. P3 `md5 …/css_l4_*/generated.rs`
= 7× `b654562c…`; `regen.rs:5` `#[derive(Clone, Copy, Debug)]` (NO `PartialEq`). P4
`lock14_baseline.rs` `GENERIC_SCAN_ROOTS:2409`, `FORBIDDEN_GENERIC_TOKENS:2420`,
`SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`, `("…/x86_64","diagnostic-x86"):2463`,
`accepts_current_allowlist:2729`. P5 `grep -c parse_w11_1_number json/generated.rs` = 7. PRUNE net LOC
≈ −10800.

### §3.1 — P1: DELETE the WHOLE x86 surface crate-wide (aarch64-only)

Residual R8 (x86 two surfaces, wrong-arch). Addendum axis A6. The single most consequential PRUNE item
and the ONLY one with a build-soundness coupling.

Owner paths: `bbnf-simd/src/x86_64/` (24 files — DELETE); `bbnf-simd/ext/x86/` (4 files incl.
~3554-LOC vendored x264/FFmpeg ASM — DELETE); `bbnf-simd/build.rs` (102-LOC nasm-rs driver — DELETE);
`bbnf-simd/Cargo.toml:19` (`nasm-rs = "0.3"` + `:14-16` comments — DELETE); `bbnf-simd/src/lib.rs:5`
(`pub mod x86_64;`), `:247` (doc ref), `:285-288` (`#[cfg(target_arch="x86_64")]` arms — DELETE);
`bbnf-simd/tests/checkasm_parity.rs` (the 9 active compile-coupled `*_scalar(…)` call sites at
`:458,:464,:467,:477,:478,:484,:493,:497,:502` + the `#[ignore]` x86 harness — DECOUPLE-OR-DELETE,
retaining the aarch64 parity assertions); `bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:10,12,15`
(x86 doc strings — CLEAN); the two report MDs (x86 narrative — CLEAN).

Entry gate: **none** (pure deletion).

Tasks: (1) DELETE both x86 surfaces + nasm driver + nasm build-dep + `pub mod x86_64;` + the
`#[cfg(target_arch="x86_64")]` dispatch arms. (2) DECOUPLE the `checkasm_parity.rs` x86_64 reference
block (9 active `*_scalar(…)` + the `#[ignore]` x86 harness) IN THE SAME COMMIT as the `src/x86_64/`
deletion — retain the 12 aarch64 single-kernel differential harnesses + `checkasm_common.rs` +
`parity.rs` (checkasm count = 14 = 12 single-kernel + 2 aggregate). (3) CLEAN the residual x86 doc
strings + the two report MDs to aarch64-neutral.

Exit-gate falsifier (turns RED): `find …/src/x86_64 …/ext/x86 -type f` == 0 (today 28); `grep -riE
--include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/` collapses to the
neutral-comment floor (a non-neutral hit RED); **BUILD-SOUNDNESS:** `cargo build` AND `cargo test
--no-run` clean (the 9 call sites resolve into `src/x86_64/`; deleting the directory WITHOUT the
same-commit decouple BREAKS the build). Telemetry: `x86_tree_deleted == true`. Outcome `P`.

Binding sequencing note: **P1 ↔ `checkasm_parity.rs` is a build-soundness coupling, NOT a one-line
`rm -rf`** — the deletion list MUST be reach-matched to the verify grep (a list narrower than the grep
ships a RED-by-construction gate, the V5 reach hazard), and the decouple MUST land in the SAME
commit/wave. P1 reduces the surface BEFORE G5/G6 touch the SIMD dispatch; the single-arch kernel
surface P1 leaves is the R-F retarget target for G6. INDEPENDENT (no entry-gate).

### §3.2 — P2: DELETE the warm micro-fixture CSS bench

Residual R13 (warm micro-fixture CSS bench). Addendum 5 (timed-plane-symmetry + corpus-in-timer). The
warm path hits THREE addenda at once but did NOT produce the headline numbers (those came from the
canonical `css_canon_bench`, S-P1 §0) — pure contrivance deletion, zero >SOTA risk.

Owner paths: `bbnf-bench/src/nonjson_css_l4.rs` (DELETE `measure_mbps`/`*_lightningcss_facts` — 48
grep hits incl. `lightningcss_facts:528`, `measure_mbps:3091` — + the per-fixture SHA256/byte
scaffolding over the 85–357-byte micro-fixtures); `bbnf-bench/benches/nonjson_css_l4.rs` (the bench
entry); KEEP `css_canon_bench.rs` + `w2_rich_cssom_bench.rs` + the 9-field `assert_rich_strict_equality`
oracle (`nonjson_css_l4.rs:451`).

Entry gate: **none**.

Tasks: (1) EXTRACT the 9-field `assert_rich_strict_equality` oracle (`:451`) to a retained location
BEFORE gutting the file. (2) DELETE the warm `measure_mbps`/`*_lightningcss_facts` machinery + the
SHA256/byte scaffolding + the micro-fixture corpus. (3) Confirm `css_canon_bench` (cold, N≥50, real
corpus, no broadcast) remains the sole CSS throughput harness.

Exit-gate falsifier: `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` == 0 (today 48);
`css_canon_bench.rs` present + green; the extracted 9-field oracle still asserts. Telemetry:
`corpus_in_timer == true`. Outcome `P`.

Binding sequencing note: P2 leaves only the cold/real-corpus harness so the R-F (G6) speedup
measurement + H1 framing measure honestly. R14/H1 disclosure is NOT P2's job — the
lazy-rich-vs-eager-CSSOM framing is disclosed at H1. INDEPENDENT.

### §3.3 — P3: COLLAPSE the 7 byte-identical css_l4 replicas + RuntimeTarget row-collapse

Residual R4 (7 byte-identical css_l4 replicas). Addendum 2 (distinct-grammar-output, a 3-co-gate
CONJUNCTION). Carries the relocated-overfit-seam structural gate (the one residual-overfit risk
surviving into GENERALIZE) and the R16 recipe-pin.

Owner paths: `runtime/src/grammars/css_l4_*/generated.rs` (7 files, all md5 `b654562c…`, 910 LOC each
— COLLAPSE to ONE CSS `generated.rs`); `xtask/src/regen.rs` (ADD `PartialEq` to the `:5` derive — the
R16 recipe-pin; collapse the 7 css_l4 rows to ONE config per `grammar_name`);
`xtask/src/regen_css.rs:45,63,81,99,117,135,153` (the 7 `profile`-distinct rows).

Entry gate: **none**. **The collapse-vs-differentiate decision is bound HERE (S-P3), not deferred
(resolving R-A0-2):** the DISK EVIDENCE is collapse-to-one (one `stylesheet.bbnf`, byte-identical
output; `generator_grammar_count == 3` = json + css + sheets) — so the default is COLLAPSE-TO-ONE.
Manufacturing 7 fake `.bbnf` roots to satisfy a distinctness gate is the EXACT overfit addendum 2
forbids; differentiate ONLY where the profiles are genuinely distinct grammars, NEVER erase a real
`profile` discriminator, NEVER mint a fake root.

Tasks: (1) COLLAPSE the 7 byte-identical `css_l4_*/generated.rs` to ONE CSS `generated.rs`, preserving
cold-bench corpus coverage. (2) ADD `PartialEq` to `RuntimeTarget`'s derive (`regen.rs:5`:
`Clone, Copy, Debug` → `Clone, Copy, Debug, PartialEq`) — the R16 recipe-pin (one line; both nested
structs already derive `PartialEq, Eq`). (3) COLLAPSE the 7 `RuntimeTarget` css_l4 rows to ONE config
per `grammar_name` (modulo the two artefact-path columns `output_dir`/`expected_files`).

Exit-gate falsifier (the 3-co-gate CONJUNCTION — md5 alone is necessary-NOT-sufficient):
**md5-distinct** — the falsifier has TWO phases that must NOT be conflated. (pre-collapse RED witness)
today `md5 …/css_l4_*/generated.rs | sort | uniq -c` shows 7× `b654562c…` → RED. (post-collapse GREEN
witness) the 7 css_l4 files NO LONGER EXIST after collapse, so the self-glob over `css_l4_*` would be a
SINGLE file with no possible pair — an unfalsifiable check; the binding post-collapse witness is the
CROSS-GRAMMAR distinctness `md5 …/grammars/{json,css_l4}/generated.rs | sort | uniq -c` → NO byte-identical
pair (the singular CSS output distinct from JSON; the sheets row joins this set at PROVE,
`generated_md5_distinct` over `{json,css_l4,sheets}`). `generated_md5_distinct == true`;
**`runtime_target_rows_collapsed == true`** (the
STRUCTURAL co-gate, R16). The R16 mechanism has TWO distinct uses that must NOT be conflated: (i) the
**relocated-seam check** uses the `RuntimeTarget: PartialEq` FULL-ROW derive over EVERY field
(including BOTH nested structs `frontend_requirements` field #11 AND `output_labels` field #12) with NO
exclusion — a per-grammar branch relocated into ANY neutral `RuntimeTarget` field (including a nested
struct) makes two would-be-equal rows compare UNEQUAL under the full-row `PartialEq`, turning the gate
RED even though the arm-census grep is blind to it; a recipe recursing into ONLY ONE nested struct is
the EXACT shallow-compare false-green R16 names — FORBIDDEN. (ii) the **row-collapse count** is the
SEPARATE check that the 7 css_l4 rows reduced to ONE config: `count(distinct
config-tuple-minus-(output_dir,expected_files)) per grammar_name == 1 for css_l4` — the two
artefact-path columns `output_dir`/`expected_files` are legitimately per-replica and are excluded from
THIS count ONLY (they are NOT excluded from the full-row `PartialEq` seam-check in (i)). The full-row
derive proves NO hidden per-grammar seam; the projected count proves the 7 collapsed to 1. The co-gate also requires
`generator_grammar_branch_count == 0 ∧ generator_grammar_type_count == 0` (G3's exit, not P3's); P3
lands the structural-collapse half so the G3 un-fork can satisfy the whole. Outcome `P`.

Binding sequencing note: **P3 is a dual-gate predecessor of G2** — G2 entry-gates on BOTH G1 AND P3
(a P3 failure blocks G2 independent of G1), because G2 re-derives the CSS scan and would re-create the
replica overfit if it derived into 7 byte-identical files. P3 also **binds to G3** (the relocated-seam
structural check IS the G3 un-fork's third close-gate surface). The `RuntimeTarget: PartialEq` derive
P3 lands is the ONE structural co-gate threading R-A (un-fork) / R-B (P3 collapse) / R-E (distinct
`grammar_name="google_sheets"` row). The collapse-vs-differentiate WHICH-branch is decided HERE.

### §3.4 — P4: FIX the Lock-14 green-by-exclusion gate (MUST LAND BEFORE G2/G3)

Residual R9 (Lock-14 green-by-exclusion gate). Addendum axis A3. The gate PASSES today
(`accepts_current_allowlist` = 2/0) — a green gate over standing leaks is worse than a red one —
because `GENERIC_SCAN_ROOTS` deliberately OMITS the codegen leak surface, routing it into a weaker
check that never runs the neutrality scan, and tags the x86 tree `"diagnostic-x86"`.

Owner paths: `lock14_baseline.rs:2409` (`GENERIC_SCAN_ROOTS` — MOVE the codegen leak surface INTO this
strict scan), `:2420` (`FORBIDDEN_GENERIC_TOKENS` — EXTEND), `:2442` (`SKV15_W2_EXTRA_COVERAGE_ROOTS`
— REMOVE the codegen surface), `:2463` (`("…/x86_64","diagnostic-x86")` — DROP, x86 is gone via P1),
`:2729` (`accepts_current_allowlist` — the asserting test); the moved surfaces:
`codegen/src/runtime_generator.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/`,
`grammar_provider.rs`.

Entry gate: **none** — BUT a hard EXIT obligation: **P4 MUST land BEFORE the emitter rebuild
(G1/G2/G3).** This is the load-bearing intra-cluster sequencing fact (an entry-gate ON G2/G3, not a
preference): the un-forked emitter must be neutrality-scanned AS it is authored, or a grammar-named
branch / const-`&str` courier could be re-introduced under a blind gate.

Tasks: (1) MOVE `runtime_generator.rs` + the JSON sink/typed/template surfaces from the weak
`SKV15_W2_EXTRA_COVERAGE_ROOTS` (`:2442`) INTO strict `GENERIC_SCAN_ROOTS` (`:2409`). (2) EXTEND
`FORBIDDEN_GENERIC_TOKENS` (`:2420`) with `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` —
the `_RS` token is scoped to the grammar-body-courier suffix `GENERATED_RS` (catching BOTH `CSS_GENERATED_RS`
and `JSON_PARSE_ONLY_GENERATED_RS`, the two grammar-body couriers G1/G2 retire) so the six surviving
MOD/HOST/PARSER/SINK scaffold consts in the now-strict-scanned `runtime_generator.rs` do not false-RED P4
under the plain `source.contains` substring semantics (`lock14_baseline.rs:2695`) — a bare `CSS_`/`_RS`
would collide with those surviving scaffold consts; this catches the JSON `_RS` couriers G1 retires, the CSS
`CSS_GENERATED_RS` courier G2 retires, AND any `EventGrammar` literal G3 would emit (Sheets is the FIRST
grammar to exercise the witness-emission coupling). (3) DROP
the `"diagnostic-x86"` exclusion (`:2463`).

Exit-gate falsifier (the gate must be MEANINGFUL, not merely green): **the re-inject falsifier** —
re-inject a token FROM the extended forbidden set into `runtime_generator.rs` (a `GENERATED_RS`-bearing
grammar-body courier token e.g. `SHEETS_GENERATED_RS`, or an `EventGrammar` literal — NOT a bare `JsonSink`,
which is NOT in the set and would not fire)
→ `accepts_current_allowlist`
turns RED, then revert (`lock14_gate_scans_codegen == true`); `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS,
CSS_GENERATED_RS, EventGrammar, *EventGrammar}`; `GENERIC_SCAN_ROOTS` contains the moved codegen surfaces;
`SKV15_W2_EXTRA_COVERAGE_ROOTS` no longer routes them; the `diagnostic-x86` entry is gone;
`accepts_current_allowlist` GREEN and meaningful (green AFTER the re-inject/revert proof, not
green-by-exclusion). Outcome `P`.

Binding sequencing note: **P4 MUST land BEFORE G2/G3** — the single load-bearing intra-cluster ordering
constraint. The `FORBIDDEN_GENERIC_TOKENS` extension is the cross-cutting predecessor of every emitter
wave. Witness-emission scan-root coupling: the witnesses live in `runtime/` (NOT the P4 codegen scan
root); IF the un-forked generator EMITS a grammar-named `EventGrammar` literal into the generated
runtime, the `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` must catch it at its emit site.
P4 shares only the conceptual x86 reference with P1 (P1 deletes the tree, P4 deletes the exclusion
entry; commit P1 first OR same-wave so the dropped exclusion does not dangle on a still-present tree).

### §3.5 — P5: PURGE the metalang bench-wave-id leak

Residual R15 (metalang leak `parse_w11_1_number` ×7). Addendum axis A1/regen. The bench wave-id is
baked into the SHIPPED `runtime/src/grammars/json/generated.rs` — a regen-discipline violation.

Owner paths: `codegen/src/json_sink_direct.rs` (the GENERATOR/TEMPLATE source — fix HERE, so `regen
--check` stays clean); `runtime/src/grammars/json/generated.rs:801,841,881,955,1007,1019,1031` (the 7
shipped `parse_w11_1_number_*` symbols — regenerated clean as `parse_number_*`; NEVER hand-patched).

Entry gate: **none**.

Tasks: (1) RENAME `parse_w11_1_number_*` → `parse_number_*` AT THE GENERATOR/TEMPLATE SOURCE
(`json_sink_direct.rs`) — never hand-patch the generated file. (2) Regenerate `json/generated.rs` so
the shipped runtime carries no `w[0-9]+`/corpus/`sk_v` wave tag.

Exit-gate falsifier: `grep -c parse_w11_1_number json/generated.rs` == 0 (today 7); no
`w[0-9]+`/corpus-name/`sk_v` tag in the shipped runtime; `cargo xtask regen --check` clean (a hand-patch
that diverges from fresh generator output fails `regen --check` → RED — the proof the fix landed at the
SOURCE). Telemetry: `metalang_leak_present == false`. Outcome `P`.

**Binding sequencing note (GROUND `sota.md` P5↔G1 call-site finding, folded).** P5 purges the metalang
leak BEFORE G1 regenerates JSON (G1's `regen --check` must be clean — a surviving `parse_w11_1_number`
would fail G1's clean-regen gate). The fix MUST be at the generator/template source, not the shipped
artefact. **The P5 metalang rename touches the JSON hot-leaf call sites at `json/generated.rs:841` /
`:881`** — exactly the `parse_object_value_at_direct`/`parse_array_element_at_direct` 91.5% leaf G1's
identical-call-site byte-equivalence check guards. Therefore **the P5 rename and G1's identical-call-site
byte-equivalence check MUST be ordered: P5 closes first; G1 re-asserts the metalang-leak-zero and the
hot-leaf preservation on the G1-REGENERATED file** (G1.2.3 / G1.2.5), not a stale one. This is a SPEC
sequencing note (P5-before-G1, already implied by the standing order), NOT an S-P2 SOTA defect. P5 is
otherwise INDEPENDENT within the cluster (disjoint from P1/P2/P3/P4).

### §3.6 — W-PRUNE telemetry columns (consumed by `--skv18-generalization-report`)

```text
x86_tree_deleted                  (P1; true)
corpus_in_timer                   (P2; true — warm path gone)
generated_md5_distinct            (P3; true — no byte-identical pair)
runtime_target_rows_collapsed     (P3; true — RuntimeTarget: PartialEq full-row, BOTH nested structs)
forbidden_generic_tokens_extended (P4; true — FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar})
lock14_gate_scans_codegen         (P4; true — re-inject falsifier proves coverage)
metalang_leak_present             (P5; false)
```

The `gate-json` consumer REJECTS a P-wave row if: `x86_tree_deleted != true` (P1); `corpus_in_timer !=
true` (P2); `generated_md5_distinct != true` OR `runtime_target_rows_collapsed != true` (P3);
`lock14_gate_scans_codegen != true` OR `forbidden_generic_tokens_extended != true` (P4);
`metalang_leak_present != false` (P5). Every emitted P-cluster column is consumed in its named P-wave
slice (no producer-only field). The `runtime_target_rows_collapsed` column is the structural co-gate the
R16 recipe-pin binds: the
gate-consumer author MUST compute it over the structurally-expanded row inlining EVERY nested-struct
field — BOTH `frontend_requirements` AND `output_labels` — via the `RuntimeTarget: PartialEq` full-row
derive (NOT a hand-rolled prose-field list, which risks a shallow-compare false-green of either nested
struct). This is the ONLY check that catches the relocated seam the arm-census grep is syntactically
incapable of seeing.

**G-wave entry-gates that consume the P-cluster:** **G1** entry = P-cluster closed (P4 live, P5
closed); **G2** entry = G1 ∧ **P3** closed ∧ P4 live (dual gate); **G3** entry = G1 ∧ G2 closed ∧ P4
live ∧ **P3** row-collapse; **G5/G6** entry = **P1** ∧ **P3** ∧ G3 closed ∧ the S-P1 94.1% hot-leaf
measurement.

---

## Section 4 — G1: JSON projection (R-C C1 — `SinkOnlyExpr` AST-walk emitter)

**G1 replaces (grounded).** `json/generated.rs` (1235 LOC) is the byte-concatenation of three sources
(`runtime_generator.rs:29-37 emit_compiled`): (1) `include_str!("json_templates/generated.rs")` (the
391-LOC tape-recognizer oracle); (2) `JSON_PARSE_ONLY_GENERATED_RS` const-`&str` courier
(`runtime_generator.rs:195` — a verbatim `_RS` blob); (3) `json_sink_direct::render(sink_only)` — the
SinkOnly direct-to-struct path whose 7 fixed-literal `push_str` blocks (`render_header:80`,
`render_entry:97`, `render_value_dispatch:125`, `render_container_rules:252`, `render_string_rule:327`,
`render_number_rules:368`, `render_utility_rules:498`) carry the **91.52% hot leaf**
(`parse_object_value_at_direct` 79.82% + `parse_array_element_at_direct` 11.70%, profile §2). The only
grammar-derived bytes today are the header comment interpolation (`:75`) and the number-emitter
`{prefix}` (`:457-461`). Everything else is the L1 "grammar-driven banner over hand-written body"
REJECT. G1's task (R-C C1): make `json_sink_direct.rs` a recursive emitter over the `SinkOnlyExpr` tree
(`lower/sink_only.rs:68-96`: Entry / Seq / Alt{mode,branches} / RepeatLoop{body,min} / OptionalBranch /
ByteLiteral(bytes) / RegexProgram{span_kind,pattern} / CallRule{callee} / DirectBuild(DirectShape) /
ValueProject / Return) like the in-tree `lower/tape_plan.rs::render_expr` + `json_typed_direct.rs` —
NOT a fixed-literal stringifier. Fold the parse-only `_RS` courier into the SAME walk so G1 retires
BOTH blob shapes.

**G1.1 — Entry gate (GREEN before dispatch).** P-cluster closed (P1–P5 all GREEN), **P4 live**
specifically (the Lock-14 fix live so the new emitter is neutrality-scanned AS authored); **P5 closed**
(the `parse_w11_1_number_*` leak ships 7× in `json/generated.rs` — emitted by the `json_sink_direct.rs`
TEMPLATE at the `:147/:187/:227` emit points; the rename lands at the SOURCE per §3.5, never on the
shipped artefact; per the GROUND P5↔G1 finding, the P5 rename touches the hot-leaf call sites at
`json/generated.rs:841`/`:881`, so P5 closes first and G1 re-emits the already-renamed names OR G1's
walk subsumes P5 by deriving the fn names from the `.bbnf` `number` rule; the P5 falsifier MUST hold on
the G1-REGENERATED file); P4's `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`
live so the JSON `JSON_PARSE_ONLY_GENERATED_RS` courier G1 retires is caught at its emit site.

**G1.2 — Exit gate (MEASURABLE; the binding proof, NOT a line delta).**

1. **Byte-equivalence diff-control (the BINDING proof, CH7).** The same-wave regen MUST byte-match the
   regenerated `json/generated.rs` against (a) the `json_templates/` byte-for-byte oracle AND (b) the
   current shipped `generated.rs`, BEFORE the oracle is deleted (mechanism: `EmittedSource::check_dir`,
   `lib.rs:74`, exact `actual != *source`; + `emission_is_deterministic` /
   `direct_parser_is_authored_from_sink_only_lowering`, `lib.rs:481-621`). The **±5% line-count delta is
   a SOFT tripwire ONLY** — advisory, NEVER a REJECT.
2. **The `.bbnf`-mutation falsifier (proves derivation).** Mutate `grammar/json/json.bbnf` (drop the
   `bool` rule), regen, the emitted dispatch MUST lose the `b't'`/`b'f'` arms (a fixed body fails →
   REJECT); revert after firing.
3. **Hot-leaf preservation (the 91.5% MUST-preserve, mechanical).** The regenerated `generated.rs` still
   contains `fn parse_object_value_at_direct` (and `parse_array_element_at_direct`) with identical
   `#[inline(always)]`/`#[inline(never)]` cfg shape AND the same `sink.object_*`/`sink.array_*` call
   sites (the call sites the P5 rename touched — re-asserted byte-equivalent on the regenerated file).
   **Do NOT LCD-collapse the value/object/array dispatch triple** (they differ only by sink prefix —
   parameterize per `{prefix}`, do NOT unify; an LCD-unify erases the monomorphized-sink leaf and
   regresses the 91.5%, rC §5).
4. **`verbatim_blob_present == false`.** BOTH the 7 SinkOnly `push_str` literals AND the parse-only `_RS`
   courier are folded into the derived walk (grep the regenerated codegen for `r#"..."#` blob bodies in
   `json_sink_direct.rs` ⇒ 0; `JSON_PARSE_ONLY_GENERATED_RS` ⇒ deleted).
5. **P5 re-assertion (on the regenerated file).** `grep -c parse_w11_1_number` on the G1-regenerated
   `json_sink_direct.rs` == 0; no `w[0-9]+`/corpus/`sk_v` tag in the shipped JSON runtime; `regen
   --check` clean.

**G1.3 — The leaf-scanner named primitives (the §6 (a)-(d) gated escape).** The structural SKELETON
(dispatch match, container loops, literal arms, the 3 sink-prefix variants) is walk-derived by C1. Only
the proven-hot inner LEAF kernels stay byte-stable as named, `.bbnf`-INVOKED primitives: (i)
**`decode_json_string_to_arena`** — invoked by the `.bbnf` `string` rule; covers
`match_tiny_plain_string_direct` (`:336`) + `unescape_string` Cow-borrow (profile leaf #3, 3.45%); (ii)
**`parse_number_*`** — invoked by the `.bbnf` `number` rule; covers the `b'-' | b'0'..=b'9'` digit
fast-path (`:306`) + `materialize_u64` (profile leaf #8, 0.53%).

Per-primitive (a)-(d) contract (ALL FOUR or REJECT): **(a)** grammar-INVOKED — the `string`/`number`
rule carries `-> decode_json_string_to_arena` / `-> parse_number_*`, the emitter reads the callee FROM
the rule's `->`, not a literal; **(b)** emitted-output VARIES under invoking-rule mutation — WIDEN the
`number` rule's digit class in `.bbnf` → the `b'0'..=b'9'` literal in the emitted `parse_number_*`
kernel WIDENS (a kernel that does NOT vary under its own rule's class mutation is a relabeled fixed
courier); **(c)** `verbatim_blob_present == false` — no `_RS`/raw-string body for the leaf; **(d)**
PROFILE-PROVEN-NARROW-LEAF — primitive LOC ≤ the profiled hot-leaf extent (string-scan/digit-scan only,
NOT the dispatch/loop skeleton). Do NOT LCD-unify the dispatch triple (structural, not a leaf primitive).

**G1.4 — Telemetry (emitted AND consumed by `gate-json` in the G1 slice; producer-only fails the wave).**

```text
g1_json_generated_byte_equivalent        (regenerated == oracle == shipped, pre-deletion; the BINDING proof)
g1_bbnf_mutation_falsifier_fires          (drop bool ⇒ b't'/b'f' arms vanish; revert)
g1_hot_leaf_preserved                     (parse_object_value_at_direct + parse_array_element_at_direct: same inline cfg + sink.* call sites, on the regenerated file)
g1_dispatch_triple_not_lcd_collapsed      (value/object/array remain 3 sink-prefix variants, not unified)
verbatim_blob_present                     (false; both SinkOnly literals AND parse-only _RS folded)
g1_leaf_primitive_count                   (2: decode_json_string_to_arena, parse_number_*)
g1_leaf_primitive_loc                     (per-primitive shell LOC — the (d) machine-check numerator, mirroring g2_balanced_scan_primitive_loc)
g1_leaf_primitive_profiled_leaf_extent    (per-primitive profiled hot-leaf LOC — string-scan/digit-scan extent; (d) PASS REQUIRES g1_leaf_primitive_loc <= this, so (d) is machine-checked not asserted)
g1_leaf_primitive_abcd_pass               (per-primitive (a)∧(b)∧(c)∧(d) all green; the (d) arm is the g1_leaf_primitive_loc <= g1_leaf_primitive_profiled_leaf_extent comparison)
g1_metalang_leak_count                    (parse_w11_1_number on REGENERATED file == 0)
g1_json_guard_rows_held                   (51/51 JSON rows A/GO strict same-plane; throughput within ±1.0% of SK-V18-open — the pinned baseline, NOT a floating one)
line_delta_vs_oracle                      (SOFT tripwire only; advisory, never REJECT)
```

The `gate-json` consumer REJECTS the G1 row if: `g1_json_generated_byte_equivalent != true`;
`g1_bbnf_mutation_falsifier_fires != true`; `g1_hot_leaf_preserved != true`; `verbatim_blob_present !=
false`; any `g1_leaf_primitive_abcd_pass` arm false; `g1_leaf_primitive_loc >
g1_leaf_primitive_profiled_leaf_extent` for any primitive (the (d) god-kernel REJECT, mirroring G2);
`g1_metalang_leak_count != 0`; `g1_json_guard_rows_held != true`; `g1_dispatch_triple_not_lcd_collapsed != true`. `line_delta_vs_oracle` and `g1_leaf_primitive_count` are emitted EVIDENCE, do NOT gate.

**G1.5 — Caps, reruns, revert, downstream.** Cap: ≤90 min wave wall; 30 min/redress. Rerun ceiling: one
full gate refresh; a second requires a REDRESS cost note. Pre-blocked routes: any JSON throughput
regression on the 91.5% leaf; an LCD-unify of the dispatch triple; a relabeled fragment masquerading as
a leaf primitive (fails (b) or (d)); trading the SinkOnly blob for the `_RS` blob (only one folded); a
courier-swap that passes byte-equivalence but fails the `.bbnf`-mutation falsifier. Revert: revert the
G1 codegen + regenerated-output commits together, restore the shipped `json/generated.rs` + the
`json_templates/` oracle, record a G1 REDRESS naming the failed gate. **Downstream: G1 REJECTION BLOCKS
G2, G3, G4, G5/G6, PROVE** (the un-forked emitter consumes G1's grammar-walk pattern; G2 inherits G1's
facts-bus discipline; G5/G6 + Sheets emit THROUGH the un-forked generator — every G1 descendant
entry-gates transitively through G3 ⊃ G1). Outcome `A`/`C`/`G` on close, `R`
on miss.

---

## Section 5 — G2: CSS lowering (R-B B⊃A — `css_balanced_component_scan` primitive + fact-keyed projection)

**G2 replaces (grounded).** The CSS provider is emitted by `emit_request_facts`
(`runtime_generator.rs:76-103`); for `generated.rs` it does `normalize(CSS_GENERATED_RS)` (`:91`) — a
pure splice of `const CSS_GENERATED_RS: &str = r#"..."#` (`:701`→ terminator, ≈910 LOC). The `.bbnf` is
NEVER consumed by this path (`emit_from_request:12-27` routes `RequestFacts` straight to
`emit_request_facts`; only `CompiledLowering` reaches `lower::lower_to_rust`). **CSS does not touch
`lower/` at all today.** The load-bearing parser: `CssFullParser` (`:1125-1467`), a flat
balanced-delimiter recognizer (NOT a recursive-descent rule-tree walk). The **94.1% hot leaf**:
`find_component_delim` (`:1357-1380`, 79.5%) + `consume_balanced_at` (`:1393-1413`, 14.6%) — the scalar
byte-at-a-time scan over `{ } ; :` plus string/comment skips and nested `()[]{}` balancing. The lazy
rich projection (preserve-rich-ast, the >SOTA product): `CssNode`/`CssRule`/`CssDeclaration`/
`CssTypedValue` (`:744-954`), `CssDocument::rich_summary` (`:958-1035`), `CssTypedValue::classify`
(`:929-953`).

**The deep finding (frames everything).** The grammar describes a RICH RECURSIVE CSSOM, but the >SOTA
recognizer is a STRUCTURE-RECOVERING DELIMITER SCAN that deliberately does NOT recurse the rule tree. A
naive grammar-walk lowering (one parse-fn per rule) produces the combinator-shaped recursive descent
(lightningcss's own architecture) that **categorically regresses >SOTA**. So G2 derives the
delimiter-scan recognizer FROM grammar-supplied data (alphabet, structural-byte set, branch tags, entry
rule), emitting the scan SHAPE the profile attributes the win to. The full grammar-IR tree-walk (R-B
Candidate C) is REJECTED. G2's recommendation (R-B B⊃A): a **hybrid** — the delimiter-scan recognizer
CORE lands as the **`css_balanced_component_scan` grammar-parameterized NAMED PRIMITIVE** (the §6
honest-finding, gated (a)-(d), FORCED-CSS-scoped per s6/C4); the drivers + lazy rich projection land as
**grammar-fact-keyed `push_str` emit blocks** (Candidate A, inheriting G1's facts-bus discipline).

**G2.1 — Entry gate (the DUAL binding predicate; GREEN before dispatch).** **G2 dual-gates; a P3
failure blocks G2 INDEPENDENT of G1.** **G1 closed** (G2 reuses G1's projecting-renderer discipline;
"the projecting renderer must exist first"; a G1 REDRESS BLOCKS G2). **P3 closed (the independent
conjunct)** — the 7 `css_l4_*` rows must collapse to ONE CSS config + the `RuntimeTarget` row-collapse
(R16 full-row `PartialEq`) BEFORE G2 derives CSS, else G2 re-derives the SAME scan into 7 byte-identical
files and re-creates the replica overfit. **P3 ∧ P4 conjunct:** `runtime_target_rows_collapsed == true`
(R16) AND P4's `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS}` live so the CSS const courier is caught at its
emit site. P4 MUST be live BEFORE G2.

**G2.2 — Exit gate (MEASURABLE).**

1. **CSS `generated.rs` grammar-DERIVED.** `verbatim_blob_present == false`; `CSS_GENERATED_RS` grep ==
   0 (`runtime_generator.rs:701` gone). The ≈910-LOC courier LOC is cohort-carried, NOT gate-keyed on the
   exact figure; the binding gate is `verbatim_blob_present == false` + the mutation test.
2. **The `css_balanced_component_scan` named primitive passes the per-primitive (a)-(c) mutate
   falsifier:** mutate the INVOKING `.bbnf` rule (the `stylesheet`/`ruleBlock`/`declaration` structural
   shape) → the emitted ARG byte sets change (the delimiter set, the open/close pairs, the
   comment/string skip flags). A primitive whose args do NOT vary under the `.bbnf` mutation is a
   relabeled blob = REJECT. **PLUS (d) PROFILE-PROVEN-NARROW-LEAF:** the primitive covers the single
   94.1% scalar-scan hot leaf; its shell LOC ≤ the profiled extent; the drivers + projection are
   fact-keyed-emitted AROUND it, NOT inside it.
3. **9-field cssparser oracle CORRECTNESS parity held** (gate-before-speed). The emitted CSS scan's
   typed summary equals the cssparser same-workload typed summary EXACTLY across the 4 benched corpora.
   An incomplete structural-alphabet derivation diverges from the oracle (parity REJECT) — the
   make-or-break (the structural-alphabet-derivation gap).
4. **The EXPLICIT >SOTA-regression gate (distinct from parity).** On `css_canon_bench` — COLD,
   **corpus-in-timer**, the P2-survivor cold/real-corpus harness — the binding SAME-RUN falsifier:
   `track1_rich/lightningcss > 1.0×` per corpus measured same-run AND not regressed vs the parser's OWN
   pre-G2 baseline `track1_rich_over_lcss_ratio_pre_g2` captured in the SAME run (so a uniform host-load
   depression cancels on both sides). The S-P1 absolute figures (bootstrap 2.190, tailwindcss 3.375,
   material-components-web 1.658 min, animate 2.101) are DIRECTIONAL antecedents inheriting §5-risk-7's
   QUIET-recapture caveat — the directional expectation, NOT the binding floor (keying the gate on an
   un-re-locked absolute ratio is the unfalsifiable hazard the same-run comparison replaces). G2
   RE-DERIVES the 94.1% scan, so oracle parity does NOT prove throughput preservation — **the same-run
   bench re-measurement (ratio > 1.0× ∧ no same-run regression vs pre-G2) is the binding regression
   falsifier.** The same-run ratio is load-robust; absolute Mbps is DIRECTIONAL and not re-locked.
5. **The neutrality-proof obligation (CH6; the FORCED CSS-scoped demotion, GROUND s6/C4).** The primitive
   is named `css_balanced_component_scan` from the outset: the GROUND re-validation grounded both offered
   non-CSS dischargers on disk (JSON `{}`/`[]` nesting at `generated.rs:833-834` dispatching to
   `parse_object_direct`/`parse_array_direct` with `sink.*` emit; Sheets `paren_expr` at
   `google-sheets.bbnf:137` descending into `expression`) and found them PARSE-with-emit descents,
   structurally incompatible with the CSS byte-SKIP shell `consume_balanced_at` (`generated.rs:693-713`)
   which emits NOTHING. The "invoke the SAME primitive" branch is therefore UNREACHABLE by the named
   candidates, so the CSS-scoped name is the FORCED outcome. The gate REJECTS a NEUTRAL name with zero
   structurally-compatible non-CSS caller; `g2_balanced_scan_neutrality_discharged` is GREEN via the
   CSS-scoped rename, NOT a fabricated cross-grammar caller. (The inner alphabet-scan sub-kernel — the
   `bbnf-simd` eq-set member scan — remains genuinely neutral caller-data; only the balanced-recognizer
   SHELL is CSS-scoped.)

**G2.3 — The `css_balanced_component_scan` named primitive (the PRIMARY §6 finding; the (a)-(d) gated
escape).** The 94.1% hot leaf is a flat balanced-delimiter recognizer whose delimiter alphabet
(`{}:;`) and structural-byte dispatch (`' " / ( [ {`) are EMERGENT from the rule shapes. The honest
path: the balanced scan lands as the grammar-parameterized `css_balanced_component_scan` named
primitive, grammar-INVOKED, taking grammar-DERIVED byte-set ARGS, with a per-primitive mutate-falsifier.
**This primitive is ALSO the G6 NEON-retarget call site — ONE seam for G2+G6** (no orphan kernel, no
per-grammar re-emit; the WIRE consumes the P3-collapsed SINGLE scan).

Per-primitive (a)-(d) contract (ALL FOUR or REJECT): **(a)** grammar-INVOKED — the emitter reads the
entry rule + structural-byte set FROM `stylesheet.bbnf`'s block/declaration/at-rule shape (the new
`lower/css_scan.rs` arg-derivation pass), not a literal; the call site names
`css_balanced_component_scan(...)` with grammar-derived args; **(b)** emitted ARGS VARY under
invoking-rule mutation — mutate the invoking rule's structural-byte shape in `stylesheet.bbnf` → the
emitted delimiter byte-array constant changes (an arg keyed only off a decorative parameter FAILS (b),
the single largest paper-close surface R-A0-3); **(c)** `verbatim_blob_present == false` —
`CSS_GENERATED_RS` grep == 0, the scan body lives in the shared `runtime_scan`/`runtime_simd` surface,
NOT a `r#"..."#` splice; **(d)** PROFILE-PROVEN-NARROW-LEAF — primitive shell LOC ≤ the profiled 94.1%
extent (`find_component_delim` + `consume_balanced_at`); the drivers (`parse_block`/`parse_declaration`
shells ≤4.2%) + the lazy projection (`CssNode`/`CssRule`/`classify`) are fact-keyed-emitted AROUND it
(Candidate A), NOT absorbed (the god-kernel risk).

**The arg-derivation pass (the G2 make-or-break).** G2 adds a new `lower/css_scan.rs` analysis that
DERIVES the structural alphabet (which bytes open/close blocks, which terminate declarations, the
comment/string skip set, the `:`-split, the at-rule-vs-qualified branch tags from the `AT_RULE_FLAG`
projection) FROM `stylesheet.bbnf`'s structure, producing a `CssScanProgram` (the CSS analog of
`SinkOnlyProgram`). `css_scan_direct::render` (sibling of `json_sink_direct::render`) emits the
recognizer + lazy projection as `push_str` blocks PARAMETERIZED by those facts. If the derivation is
incomplete/wrong, the emitted scan either (a) diverges from the 9-field oracle (parity REJECT) OR (b) is
hand-patched back to match (collapses to a verbatim blob, L1 REJECT). The mitigation IS Candidate B:
derive only the ARG byte sets (a smaller, more tractable derivation), keep the scan ALGORITHM in the
named primitive — but then (b) MUST prove the args vary. This is the single most likely G2 REDRESS site.

**G2.4 — The lazy rich projection emit blocks (Candidate A).** The drivers + the lazy rich projection
(`CssNode`/`CssRule`/`CssDeclaration`/`CssTypedValue::classify` derived from the grammar's
at-rule/declaration/value-class facts) land as grammar-fact-keyed `push_str` emit blocks — the same
template-keyed-by-grammar-facts pattern G1 lands, NOT a generic IR tree-walk. The addendum-4 LCD-flatten
co-gate (tracked here, gated at G4): G2's projection emit MUST keep CSS's
`CssRule::selector_count`/`CssDeclaration::typed_value` rich API intact and NOT flatten it toward a JSON
common denominator (`json_rich_navigation_preserved == true` is the G4 co-gate; G2 must not foreclose
it).

**G2.5 — Telemetry (emitted AND consumed by `gate-json` in the G2 slice).**

```text
g2_css_generated_grammar_derived          (verbatim_blob_present == false; CSS_GENERATED_RS grep == 0)
g2_balanced_scan_primitive_abcd_pass      ((a)∧(b)∧(c)∧(d) all green)
g2_balanced_scan_primitive_loc            (the shell LOC; the (d) machine-check)
g2_balanced_scan_profiled_leaf_extent     (find_component_delim + consume_balanced_at LOC; (d) PASS REQUIRES g2_balanced_scan_primitive_loc <= this — a primitive larger than the profiled extent is a god-kernel, REJECT)
g2_balanced_scan_arg_mutation_fires       (mutate stylesheet.bbnf structural-byte shape ⇒ emitted delimiter byte-array changes; revert)
g2_balanced_scan_neutrality_discharged    (true via the FORCED css_balanced_component_scan rename; NO fabricated cross-grammar caller)
g2_cssparser_oracle_parity                (EXACT 9-field, 4/4 corpora; gate-before-speed)
g2_sota_ratio_held                        (per-corpus track1_rich/lightningcss > 1.0× same-run AND no same-run regression vs track1_rich_over_lcss_ratio_pre_g2, cold corpus-in-timer; PASS REQUIRES >= 1 REGULAR corpus (animate OR bootstrap) crossing > 1.0× with no regression — close-condition #6; tailwindcss below 1.0× is an honest residual recorded, NOT tranche-blocking, provided a regular corpus holds; mcw/full-corpus regression is reported honestly)
g2_sota_ratio_directional_antecedent      (bootstrap 2.190 | tailwind 3.375 | mcw 1.658 | animate 2.101 — DIRECTIONAL S-P1 expectation, NOT the binding floor; the binding gate is the same-run comparison)
corpus_in_timer                           (true; the binding §0.4 column MUST-be-true at G2 — the P2-survivor cold/real-corpus css_canon_bench plane)
g2_css_rich_projection_not_flattened      (CssRule/CssDeclaration rich API intact; G4 co-gate not foreclosed)
g2_css_replica_singular                   (P3-collapsed single CSS config; not re-derived 7×)
verbatim_blob_present                     (false)
runtime_target_rows_collapsed             (true; R16 full-row PartialEq, P3 conjunct re-asserted)
```

The `gate-json` consumer REJECTS the G2 row if: `g2_css_generated_grammar_derived != true`; any
`g2_balanced_scan_primitive_abcd_pass` arm false; `g2_balanced_scan_primitive_loc >
g2_balanced_scan_profiled_leaf_extent` (the (d) god-kernel REJECT); `g2_balanced_scan_arg_mutation_fires != true`;
`g2_balanced_scan_neutrality_discharged != true`; `g2_cssparser_oracle_parity != true` (gate before ANY
speed admission); `g2_sota_ratio_held != true`; `corpus_in_timer != true`; `verbatim_blob_present !=
false`; `runtime_target_rows_collapsed != true`; `g2_css_rich_projection_not_flattened != true`; `g2_css_replica_singular != true`. `g2_sota_ratio_directional_antecedent` is emitted DIRECTIONAL evidence, does NOT gate. Absolute-Mbps figures carry the §5-risk-7
QUIET-recapture caveat (DIRECTIONAL until H1); the load-robust RATIO is the binding gate.

**G2.6 — Caps, reruns, revert, downstream.** Cap: ≤90 min wave wall; 30 min/redress; G2 is MED-HIGH
(the structural-alphabet-derivation gap) and may carry a documented larger redress cap if the
arg-derivation pass under-delivers — RECORDED, not silent. Rerun ceiling: one full gate refresh.
Pre-blocked routes: the full grammar-IR tree-walk (R-B Candidate C); a hand-patched scan body that
passes the oracle but fails the arg-mutation falsifier; a NEUTRALLY-named CSS-only primitive (the
false-neutral overfit — FORCED to the CSS-scoped name per s6/C4); a god-kernel absorbing the
drivers/projection (fails (d)); re-deriving the scan into 7 byte-identical files; any >SOTA admission on
a non-corpus-in-timer or warm/micro-fixture plane; a `track1_rich/lightningcss` below the S-P1 ratio
reported as a pass. Revert: revert `lower/css_scan.rs` + `css_scan_direct.rs` + primitive-shell +
regenerated-output commits together, restore the `CSS_GENERATED_RS` courier + the 7 (now P3-collapsed)
css_l4 configs, record a G2 REDRESS naming the failed gate (if the arg-derivation pass is the failure,
record the structural-alphabet-derivation gap as the named residual — do NOT paper-close with a
hand-patched blob). **Downstream: G2 REJECTION BLOCKS G3, G4, G6, PROVE.** Outcome `A`/`C`/`G` on close,
`R` on miss.

---

## Section 6 — G3: un-fork the emitter (R-A A — DELETE `RuntimeEmitterKind`, dispatch on `BackendShape`)

DELETE `RuntimeEmitterKind`; dispatch the single `render(program)` body on the lowered `BackendShape`
(`lower/mod.rs:18`), the already-Lock-14-clean grammar-NEUTRAL 5-shape discriminator. The decision stays
grammar-DERIVED (a cost-model over rule shapes, `cost.chosen`, NOT a config field). Candidate B
(`ProjectionSpec` value) is ABSORBED into A's per-`BackendShape` renderers (each declares its own
roster, retiring `COMPILED_RUNTIME_FILES`/`REQUEST_FACTS_RUNTIME_FILES` as per-arm constants). Candidate
C (no discriminator, one canonical fold) is the SK-V19 end-state, deferred on >SOTA-preservation burden.

Owner paths: `runtime_generator.rs:16-25` (DELETE the `match … emitter` fork; replace with `match
program.policy_summary.backend_shape { … }` per-`BackendShape` renderer dispatch), `:29-70`
(`emit_compiled` becomes the `SinkOnly`/`CollapsedStage`-shape renderer), the retired CSS arm (`:88-95`
post-G2); `grammar_provider.rs:31-43` (DELETE `pub enum RuntimeEmitterKind`; REMOVE the `emitter` field
from `RuntimeProfileContract`; the `first_unsupported` gate at `:110-111` re-keys on `BackendShape`);
`grammar_profile.rs` (per-arm roster constants → per-`BackendShape`-renderer rosters); `xtask/src/
regen.rs:5-19` (DELETE the `emitter` field #9; ADD the `PartialEq` derive — the R16 +1-line recipe);
generated runtime output (named, diff-audited; byte-equivalent to the G1/G2-closed shipped files, since
G3 changes the PATH not the OUTPUT); `bbnf-bench/`, `skinny/RESULTS.md`; `skinny/REDRESS.md` if rejected.

**G3.1 — Entry gate (GREEN before dispatch).** **`G1 ∧ G2 closed ∧ P4 live ∧ P3 (row-collapse)`** — a
4-conjunct gate. **G1 closed** (JSON emits via the `SinkOnlyExpr` AST-walk projector, so the un-forked
`SinkOnly`-shape renderer inherits a DERIVED body). **G2 closed** (CSS actually LOWERS to a real lowered
CSS scan IR, so the un-forked renderer has a non-const `BackendShape` renderer input). **P4 live** (the
Lock-14 gate is meaningful, so the un-forked emitter is neutrality-scanned AS authored — P4 MUST land
BEFORE G3). **P3 (row-collapse)** (`runtime_target_rows_collapsed == true` already holds, so when G3
removes the `emitter` field the structural-row invariant is already enforced). CHALLENGE acceptance
required (G3 is first-of-class, structure-touching, HIGH-risk): the un-fork is NOT a relocation of the
fork into a neutral data table; the per-`BackendShape` renderer reads its shape from the lowered program,
never from a `RuntimeTarget`/`RuntimeProfileContract` field.

**G3.2 — Exit gate (the FIVE-conjunct binding proof).**

1. **`emitter_fork_present == false`** (addendum 3, R3). The `RuntimeEmitterKind` enum and BOTH variants
   deleted (grep `RuntimeEmitterKind|CompiledLowering|RequestFacts` over `codegen/src/` + `xtask/src/` ==
   0, test-excluded). One `render(program)` path serves JSON+CSS dispatched on `BackendShape`.
2. **`generator_grammar_branch_count == 0`** (addendum 2 conjunct, R3). No generic branch selects by
   grammar name/family (arm-census grep over the `render(program)` body for `match grammar`/`if
   grammar_name ==`/`"json"`/`"css"`/`"google_sheets"` literals == 0).
3. **`generator_grammar_type_count == 0`** (addendum 2 conjunct, R3). No grammar-named type
   discriminates the emit path (no `JsonEmitter`/`CssEmitter`/`*Emitter`-per-grammar type; the only
   discriminator is the neutral `BackendShape` enum).
4. **`runtime_target_rows_collapsed == true`** (addendum 2 conjunct, R16, the STRUCTURAL co-gate). The
   R16 full-row `PartialEq` derive holds AFTER the `emitter` field is removed from `RuntimeTarget`. The
   ONLY check that catches the relocated seam (the arm-census grep is syntactically incapable of seeing
   it).
5. **`emit_shape_source == lowered_program`** (addendum 3, the FOURTH conjunct — the §5-risk-1 binding).
   The un-forked `render(program)` body reads its output-shape ONLY from
   `program.policy_summary.backend_shape` (`sink_only.rs:48`), NEVER from a `RuntimeTarget`/
   `RuntimeProfileContract` field (grep the `render(program)` body for any read of
   `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`
   == 0). **Without this fourth conjunct, the §5-risk-1 relocated seam riding the neutral per-profile
   columns passes all of conjuncts 1-4 under a green gate.** The binding distinction between an HONEST
   un-fork and a paper-close that relocates the fork into data.

Plus the cross-cutting MUST-preserves (the un-fork changes the PATH, not the OUTPUT): **(6)
byte-equivalent generated output** (`cargo xtask regen --check` clean; diff of regenerated vs shipped ==
empty for every grammar); **(7) JSON 91.5% hot leaf preserved** (`parse_object_value_at_direct` +
`parse_array_element_at_direct` re-emitted with identical `inline(always)` shape + `sink.*` call sites
through the un-forked `SinkOnly`-shape renderer; the JSON >sonic-rs-strict guard holds); **(8) CSS >SOTA
preserved** (same-run `track1_rich/lightningcss > 1.0×` with no same-run regression vs the pre-G2
baseline on the corpus-in-timer harness — the S-P1 absolutes are DIRECTIONAL, not the floor).

**G3.3 — The R16 row-collapse recipe (the SINGLE structural co-gate, pinned to this packet).** Today
`RuntimeTarget` derives `Clone, Copy, Debug` ONLY (`regen.rs:5`) — NO `PartialEq`; it carries `emitter`
(field #9, `:15`), `frontend_requirements` (field #11, `:17`), `output_labels` (field #12, `:18`).
Recipe (PREFERRED, +1 line): add `PartialEq` to the `RuntimeTarget` derive — the R16 mechanism because
it **recurses into BOTH nested structs automatically** (both already derive `PartialEq, Eq` at
`grammar_provider.rs:45`/`:91`, so the recurse is free; only the `RuntimeTarget` line changes). NOT a
hand-rolled prose-field comparison (the audit names this REJECT — "a recipe that recurses into
`output_labels` only would slip a future seam riding `frontend_requirements`"; "a hand-rolled prose-field
comparison risks a shallow-compare false-green of EITHER nested struct"). Numbering hazard: the #11/#12
ordinals are `RuntimeTarget`'s (`regen.rs` lines 17/18); the SAME two structs recur as
`RuntimeProfileContract` fields #3/#4 (`grammar_provider.rs:35-36`) — the R16 derive lives on
`RuntimeTarget` (`regen.rs:5`). Co-gate timing: P3 lands the row-collapse co-gate; G3 removes the
`emitter` field (#9); the `runtime_target_rows_collapsed == true` invariant must hold ACROSS the field
removal.

**G3.4 — The §6 generality stress (the precedence-tower CANDIDATE, deferred to PROVE).** G3's un-fork is
where the Sheets precedence tower's generality is FIRST stressed, but the §6 finding (if any) surfaces at
PROVE, not G3 (Sheets does not emit in this wave). G3's obligation is to render recursive
`CallRule`/`RepeatLoop` chains from grammar structure (`sink_only.rs:69-96`) — IF the un-forked
`render(program)` body cannot, the tower breaks at PROVE and a named, `.bbnf`-invoked, parameterized
precedence primitive surfaces (gated (a)-(d)). G3 must NOT special-case any recursion depth or rule-name;
the recursion comes from `SinkOnlyExpr` structure, the neutral IR.

**G3.5 — Telemetry, caps, reruns, revert, downstream.**

```text
emitter_fork_present             (bool;  MUST be false — RuntimeEmitterKind/CompiledLowering/RequestFacts grep == 0)
generator_grammar_branch_count   (int;   MUST be 0   — no grammar-name branch in the emit path)
generator_grammar_type_count     (int;   MUST be 0   — no grammar-named emit-path type)
runtime_target_rows_collapsed    (bool;  MUST be true — R16 full-row PartialEq over BOTH nested structs)
emit_shape_source                (enum {lowered_program|runtime_target}; MUST be lowered_program — render(program) reads NO target.* field)
generated_md5_distinct           (bool;  MUST be true — no byte-identical pair across grammars/{json,css_l4}/generated.rs; addendum-2 conjunct re-asserted at G3)
dirty_generated_state            (enum {clean|retired}; MUST be clean — regen --check byte-equivalent vs G1/G2-closed shipped files; conjunct 6)
g3_json_hot_leaf_preserved       (bool;  MUST be true — parse_object_value_at_direct + parse_array_element_at_direct re-emitted through the SinkOnly-shape renderer with identical inline cfg + sink.* call sites; the 51/51 JSON guard held; conjunct 7)
g3_css_sota_ratio_held           (bool;  MUST be true — same-run track1_rich/lightningcss > 1.0×, re-confirmed DIRECTIONALLY against the G2-RECORDED track1_rich_over_lcss_ratio_pre_g2 baseline on the corpus-in-timer harness — the pre-G2 code is gone post-G2 so the regression falsifier FIRES at G2, never re-measured here, per close-cond #6; the S-P1 absolutes DIRECTIONAL, not the floor; conjunct 8)
```

The `gate-json` consumer REJECTS the G3 row if: `emitter_fork_present != false`;
`generator_grammar_branch_count != 0`; `generator_grammar_type_count != 0`;
`runtime_target_rows_collapsed != true`; `emit_shape_source != lowered_program`; `generated_md5_distinct
!= true`; `dirty_generated_state != clean`; `g3_json_hot_leaf_preserved != true`; `g3_css_sota_ratio_held
!= true`. Conjuncts 6/7/8 (the cross-cutting MUST-preserves — byte-equivalent output, JSON hot leaf, CSS
>SOTA) are CONSUMED columns the gate REJECTs on, NOT prose-only gates; every emitted column is consumed
in the G3 slice (no producer-only field).

Cap: ≤90 min wave wall; 30 min/redress. G3 is HIGH-risk (the relocated seam) and may carry a documented
larger redress cap if the §5-risk-1 seam fires — RECORDED, not silent. Rerun ceiling: one full gate
refresh. Pre-blocked routes: the relocated seam (un-forking the visible enum while leaving a per-grammar
branch in a neutral data table — `target.profile`-selected `ProjectionSpec`, the per-profile columns); a
per-grammar `match grammar_name` arm; md5-distinct treated as sufficient (necessary-NOT-sufficient); a
behaviour change to the JSON/CSS bodies masquerading as an un-fork (the output must be byte-equivalent).
Revert: revert `runtime_generator.rs` + `grammar_provider.rs` + `grammar_profile.rs` + `regen.rs` +
generated-output commits as ONE slice; restore RESULTS; add a REDRESS naming WHICH conjunct failed
(fork-present / grammar-branch / grammar-type / row-collapse / emit-shape-source) and the relocated-seam
witness if conjunct 5 fired. **Downstream: G3 REJECTION BLOCKS G4, G6, PROVE.** Outcome `G`/`A`/`C` on
close, `R` on miss.

---

## Section 7 — G4: shared value-API trait + phantom resolution (R-D A — `Cursor` micro-trait, DELETE `<G>`)

A thin `Cursor` micro-trait over the surviving `ValueRef<K>` + extend the existing `DocumentView` to
CSS/Sheets; DELETE the phantom `<G>`. The unique candidate giving ≥2 impls that CANNOT collapse to the
lesser — the trait shares the laziness/cursor contract, NEVER navigation, so JSON's rich tree
(`get`/`pairs`/typed `JsonValue`/recursive visitor) is preserved — PROVEN by the G4.2-conjunct-2
byte-equal diff, not "by construction"
(`json_rich_navigation_preserved == true`). Candidate B (tree-shaped `Value` stack) is REJECTED (HIGH
LCD-flatten hazard or dead degenerate CSS impls); candidate C (`DocumentView`+stream-only) under-delivers
(shares the stream, not value navigation).

Two separable sub-tasks (the `<G>` DELETE is INDEPENDENT of the trait): **G4a — the phantom `<G>` DELETE
(may land FIRST within G4)** — remove the `G: EventGrammar = AnyGrammar` parameter + `_grammar:
PhantomData<fn() -> G>` field from `ValueRef` (`tape/mod.rs:175-181`); the K-axis (`_kind:
PhantomData<fn() -> K>`) is the REAL Kind axis and is PRESERVED; DELETE the `EventGrammar` trait +
`AnyGrammar` enum (`tape/event_grammar.rs`) + the test-only `*EventGrammar` witnesses
(`grammars/{json,sheets_witness}/event_grammar_witness.rs`) + their `_proof_compiles` consumers (the
non-test instantiation census is EMPTY, re-confirmed on disk). **G4b — the `Cursor` micro-trait** —
define the thin trait over the now-clean `ValueRef<K>` sharing ONLY the cursor/laziness contract
(advance, child, offset, kind-tag-read), extend `DocumentView` (`tape/mod.rs:227-232`) to CSS/Sheets; ≥2
real impls: JSON `ValueRef<K>` (rich tree nav) + CSS `CssNode` (flat sweep) that CANNOT collapse to the
lesser.

Owner paths: `tape/mod.rs:175-223` (REMOVE the `G` param + `_grammar` field; `erase` at `:202` loses its
`G`), `:227-232` (extend `DocumentView` to the CSS/Sheets root types); `tape/event_grammar.rs` (DELETE
the file); `grammars/{json,sheets_witness}/event_grammar_witness.rs` (DELETE) + their `_proof_compiles`
consumers; `tape/cursor.rs` (NEW — the thin `Cursor` micro-trait; directory-module isomorphic, not a
flat sibling, `[directory-module-structure]`); `grammars/json/value.rs` + `grammars/css_l4_*/` (the ≥2
impls); generated runtime output (named, diff-audited — the JSON rich-nav surface must stay byte-equal);
`bbnf-bench/`, `skinny/RESULTS.md`; `skinny/REDRESS.md` if rejected.

**G4.1 — Entry gate (GREEN before dispatch).** **`G1 ∧ G2 ∧ G3 closed`** — a 3-conjunct gate. G1 ∧ G2
closed (JSON and CSS both emit DERIVED value-API surfaces). G3 closed (the un-forked emitter emits BOTH
JSON and CSS value-API surfaces THROUGH ONE path; a trait over two forked emitters is a trait over two
substrates = the LCD-flatten REJECT). CHALLENGE acceptance required (G4 is MED-HIGH-risk — the §5-risk-4
"trait too thin" critique): the `Cursor` trait shares the cursor/laziness contract, NOT a forced common
value shape; ANY trait wide enough to satisfy the "too thin" critic is wide enough to LCD-flatten JSON
(the REJECT). The honest generalization IS the cursor/laziness contract.

**G4.2 — Exit gate (the THREE-conjunct binding proof).**

1. **`phantom_generic_resolved == deleted`** (addendum 4, R5). The `<G>` / `EventGrammar` / `AnyGrammar`
   axis is deleted (grep `EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData` over
   `runtime/src/` == 0, test-excluded; the grep MUST test-exclude — the standing
   `_proof_compiles::<JsonEventGrammar>` is test-only and must NOT false-green). The grep targets the `G`
   (EventGrammar) axis, NOT the REAL `K` (Kind) axis — `_kind: PhantomData<fn() -> K>` survives
   (preserve-rich-ast).
2. **`json_rich_navigation_preserved == true`** (addendum 4, R6 — makes the ≥2-impl count
   necessary-not-sufficient). JSON's rich tree navigation (`get`/`pairs`/typed `JsonValue`/recursive
   visitor) is byte-equal after the trait extraction (the JSON `value.rs` navigation surface diffs empty
   vs its pre-G4 form; JSON 51/51 held; value-plane population parity intact). The trait must NOT
   LCD-flatten JSON's tree toward CSS's flat sweep.
3. **≥2 real impls that CANNOT LCD-collapse to the lesser** (addendum 4 + R6). JSON `ValueRef<K>` (rich
   tree) + CSS `CssNode` (flat sweep) — non-collapsibility proven by a CONCRETE MACHINE-CHECKED falsifier,
   NOT a bare ≥2 count (`shared_trait_impl_count >= 2` is necessary-NOT-sufficient — the count alone
   admits a degenerate-equal CSS impl): `shared_trait_non_collapsible == true` is the differential —
   substitute JSON's `ValueRef<K>` navigation impl for the CSS `CssNode` impl (and vice-versa) and the
   crate FAILS to compile (the rich-tree `get`/`pairs` surface has no CSS analogue; the flat-sweep
   surface has no JSON analogue), proving the two navigation surfaces are structurally distinct and
   neither is the other's degenerate. A degenerate-equal CSS impl that is just JSON's nav with dead
   branches COMPILES under the substitution → `shared_trait_non_collapsible == false` → REJECT
   (Candidate B's failure mode).

Plus the cross-cutting MUST-preserve: **(4) No second substrate (Lock 1).** The `Cursor` trait is a view
over the EXISTING `Tape`/`ValueRef`/`PayloadArena`; NO second tape, no parallel cursor type, no eager
value tree (grep for a new `*Tape`/`*Cursor` substrate type == 0; laziness intact — no per-leaf
`Box::new`).

**G4.3 — The §5-risk-4 mitigation (the "trait too thin" critique, bound to telemetry).** The binding
mitigation TIES the verdict to the two falsifiable telemetry columns (`phantom_generic_resolved ==
deleted` ∧ `json_rich_navigation_preserved == true`), NOT to a subjective width judgement. The rebuttal
is structural: ANY trait wide enough to satisfy the critic is wide enough to LCD-flatten JSON (the
REJECT) — so the honest generalization is the cursor/laziness contract, and the
≥2-non-collapsible-impl gate (conjunct 3) is what proves it is a real generalization. The critique cannot
reopen the candidate choice without producing a wider trait that passes conjunct 2 — which the §5-risk-4
analysis shows is impossible.

**G4.4 — Telemetry, caps, reruns, revert, downstream.**

```text
phantom_generic_resolved         (enum {deleted|instantiated|present}; MUST be deleted — the <G>=EventGrammar axis, test-excluded)
json_rich_navigation_preserved   (bool; MUST be true — JSON tree nav byte-equal vs pre-G4; the ≥2-impl count's necessary-not-sufficient companion)
shared_trait_impl_count          (int; MUST be >= 2 — JSON ValueRef<K> + CSS CssNode; necessary-NOT-sufficient on its own)
shared_trait_non_collapsible     (bool; MUST be true — the conjunct-3 substitution falsifier: swap JSON's nav impl for CSS's ⇒ compile FAILS; a degenerate-equal CSS impl COMPILES ⇒ false ⇒ REJECT)
```

The `gate-json` consumer REJECTS the G4 row if: `phantom_generic_resolved != deleted`;
`json_rich_navigation_preserved != true`; `shared_trait_impl_count < 2`; `shared_trait_non_collapsible !=
true` (the bare count is necessary-NOT-sufficient — the substitution falsifier is the binding
non-collapsibility proof); or a second substrate / eager value tree is present (Lock 1). Every emitted
column is consumed in the G4 slice (no producer-only field).

Cap: ≤90 min wave wall; 30 min/redress. G4 is MED-HIGH; a documented larger redress cap is allowed if
the "trait too thin" critique forces a trait-width REDRESS — RECORDED, not silent. Rerun ceiling: one
full gate refresh. Pre-blocked routes: a tree-shaped `Value` stack (Candidate B); a
`DocumentView`+stream-only trait (Candidate C); a degenerate-equal CSS impl; deleting the REAL `K`
(Kind) axis (preserve-rich-ast); a second substrate; an eager value tree; a `_proof_compiles` test
false-greening the phantom grep. Revert: revert the `tape/mod.rs` + `tape/event_grammar.rs` (deletion) +
`tape/cursor.rs` (new) + the impls + generated-output commits as ONE slice; restore RESULTS; add a
REDRESS naming WHICH conjunct failed (phantom-resolved / json-nav-preserved / impl-count) and whether the
trait LCD-flattened JSON. **Downstream: G4 REJECTION BLOCKS PROVE** (the Sheets value type instantiates
the G4 trait — the phantom-`<G>` resolution made concrete by a third impl; G4 is a DIRECT conjunct of
PROVE's entry gate). G4 does NOT block G6 (G6 wires the NEON, independent of the value-API trait; G5/G6
hangs off G3, parallel to G4). Outcome `G`/`A`/`C` on close, `R` on miss.

---

## Section 8 — G5/G6: neutral scan retarget (R-F A — inner-skip vectorize)

**Lever:** retarget the existing checkasm-gated `bbnf-simd` kernel
(`byte_class_from_eq_set_64`/`find_ascii_set_member64`, `bbnf-simd/src/lib.rs:209`) onto the scalar
recursive shell of `find_component_delim` (CSS hot leaf, 79.5% alone / 94.1% with `consume_balanced_at`).
**WIRE** per S-P1 §3 (94.1% ≫ ~8% wire threshold). **G5 = neutralize/retire the zero-sampled
`json/scan.rs`** (cheap, no JSON classifier authored; JSON product path is scan-free). Candidate A is a
RETARGET, not a wire-as-is — the dead `find_css_significant`/`find_comment_close` (R7) were written for a
flatter function and do NOT cover the recursive hot path. Candidate B (balanced-consume bitmap) is the
DOCUMENTED upgrade path (record here, build only if a post-A measurement shows the `consume_balanced_at`
14.6% tail dominates); Candidate C (table-classifier unify) is REJECTED (lo6-collision + JSON↔CSS
coupling).

**Entry gate (GREEN before dispatch).** Per S-P2 §3 G5/G6 row: **P1 ∧ P3 ∧ G3 closed ∧ the S-P1 94.1%
hot-leaf measurement** (no orphan kernel). **PARALLEL to G4/PROVE — G5/G6 needs only G3, NOT G4** (the
seq/C7 correction: G5/G6 does NOT hang under G4; it hangs off G3 alongside G4). Concretely: **P1 closed**
(the x86 surface is gone crate-wide; the `bbnf-simd` kernel surface is single-arch when R-F retargets; a
live x86 arm blocks G5/G6); **P3 closed** (the 7 css_l4 replicas collapsed to ONE CSS scan AND
`runtime_target_rows_collapsed == true`; the retargeted call site MUST land into the P3-COLLAPSED single
CSS scan — re-emitting per-replica re-forks the shape G3 un-forks; a P3 failure blocks G6 independent of
G3); **G3 closed** (the un-forked grammar-agnostic emitter exists; the `runtime_simd` CALL must be
emitted by the single un-forked emitter, not a CSS-family fork); **S-P1 profile present** (the 94.1% /
79.5% hot-leaf measurement is the standing mandate for the WIRE branch). CHALLENGE accepts
(first-of-class, primitive-touching): the kernel ALREADY EXISTS and is alphabet-data (the generator emits
a CALL, not vector code per grammar); the significant set spans ≤13 bytes (>8 eq-set cap) and uses the
two-fan OR-reduce SALVAGED byte-exact from the dead `find_css_significant:180-204`; the vector skip stops
AT `([{'"/` and hands recursion back to the scalar shell; error positions come from the scalar shell.

**Tasks.** (1) **G5 — neutralize the zero-sampled `json/scan.rs`** (it appears ZERO times in the JSON
profile; its NEON scanner lives only on tape/`parse_only` probe paths — retire it or fold onto the shared
`bbnf-simd` surface; NO bespoke JSON classifier; `json_scan_rs_neutralized = retired|neutralized`). (2)
**G6 — author the `runtime_simd` retarget entry** (a thin neutral wrapper over the existing
`bbnf-simd::find_ascii_set_member64`/`byte_class_from_eq_set_64`), with the set-split logic salvaged
byte-exact from the dead `find_css_significant:180-204` (two ≤8 eq-set fans OR-reduced for the ≤13-byte
significant family; the set is CALLER DATA — CSS passes its delimiter set; the structural family `' " /
( [ { ) ] }` is the recognizer's own constant). (3) **G6 — swap the generated inner-skip call site** (the
single P3-collapsed CSS scan's per-byte `_ => pos + 1` inert advance routes to the `runtime_simd` entry;
the recursion/string-skip/comment-skip stay scalar; `consume_balanced_at`'s OWN inert advance reuses the
SAME entry; the call site is emitted by the G3 un-forked emitter, ONCE; land consumer + entry in ONE
commit — addendum 6 no-orphan law). (4) **G6 — author the dav1d scalar-reference + checkasm differential
FIRST** (scalar reference = the existing `find_component_delim` inner loop + `significant_ref`,
`lib.rs:506`; checkasm differential = extend `checkasm_byte_class_from_eq_set_64`; retarget
`neon_significant_skip_matches_scalar`, `lib.rs:562`, to the recursive shell over the REAL 71KB–495KB
corpora, NOT the micro-cases; aarch64 NEON/dotprod ONLY). (5) **G6 — DELETE-or-salvage the dead
R7/R10/R11 kernels in the same wave** (salvage the set-split; retire `find_comment_close` ONLY if
retargeting to the comment-consume proves unsafe, gated on the samply non-top-N measurement; no dead
`#[cfg(test)]`-only NEON kernel survives).

**Exit gate (MEASURABLE).** `acceleration_at_admission == admission`, PROVEN by BOTH (i) the
generated-`generated.rs` caller census (`rg runtime_simd::find_… skinny/crates/runtime/src/grammars/*/
generated.rs` NON-EMPTY; `simd_admission_caller` is a `generated.rs` hot-loop call site, NOT a
`#[cfg(test)]` caller; post-G6 the L6 census target MOVES off `lib.rs:574` — a surviving test-only
admission proof FAILS the wave) AND (ii) a RUNTIME-REACHABILITY proof that the call site is on the LIVE
benched parse path — a samply re-sample of `css_canon_bench` attributes self-time to the `runtime_simd`
entry (addendum 6 is a profile-first MEASUREMENT, not a grep: a call site present in `generated.rs` but
in dead/unreachable code would false-green the source census, so the profile-attribution is the binding
second conjunct). `simd_admission_profile_sampled == true` (the entry appears in the css_canon_bench
sample, non-zero self-time); the RED predicate is `self_time_samples == 0` (the entry ABSENT from the
sample, or present at exactly zero attribution) → `acceleration_at_admission == dead` → REJECT — the
conjunct gates on PRESENCE-with-attribution, not a speedup magnitude (the magnitude is the deferred H1
figure). `neon_significant_skip_matches_scalar == PASS` over the REAL 71KB–495KB
corpora (micro-case-only PASS does NOT satisfy this). `checkasm_differential == PASS` — the CORRECTNESS
plane (G6 may report ONLY this PASS/FAIL pre-H1; the three retarget seams covered bit-exact: (a) the
≤13-byte two-fan OR-reduce salvage; (b) the skip stops AT `([{'"/`; (c) error positions reproduced from
the scalar shell). Timed-plane binding (addendum 5): any Mbps/speedup FIGURE comes from the
corpus-in-timer symmetric `css_canon_bench` harness and inherits §5-risk-7's QUIET-recapture caveat —
**the speedup CLAIM is DEFERRED to the H1 symmetric timer** (`g6_speedup_median_mbps` is null pre-H1;
the G6 outcome is `C` until H1 produces the figure). `css_scan_call_site_singular == true` (exactly ONE
generated call site post-P3) ∧ `significant_set_is_caller_data == true`. `json_scan_rs_neutralized ∈
{retired, neutralized}` (G5; outcome `N` — nothing on product path) ∧ `json_guard_held == true` (51/51
within ±1.0% of `SK-V18-open`).

**Falsifiers (each gate RED-able).** Caller-census: revert the generated call-site swap →
`simd_admission_caller` empty → `acceleration_at_admission == dead` → RED; re-apply. Parity: mutate the
salvaged two-fan OR-reduce by one byte → `neon_significant_skip_matches_scalar == FAIL` over the corpora →
RED; revert. Singular-site: if P3 has NOT collapsed and the call is emitted 7 ways →
`css_scan_call_site_singular == false` → RED (this is why G6 entry-gates on P3). Orphan-kernel: author a
JSON classifier with no hot consumer → no profile anchor → REJECT (G5 authors NOTHING for JSON).
Plane-mismatch: emit a `g6_speedup_median_mbps` from the checkasm plane (not corpus-in-timer) →
addendum-5 REJECT.

**Telemetry (PD-specific columns emitted + consumed by `gate-json`).**

```text
acceleration_at_admission                  (admission | dead — admission REQUIRES BOTH conjuncts: the caller census over grammars/*/generated.rs (NOT #[cfg(test)]) AND simd_admission_profile_sampled == true; a census hit in dead/unreachable code with no profile attribution == dead, per §0.4)
simd_admission_caller                      (the runtime_simd::find_… call site in grammars/*/generated.rs; empty == FAIL)
simd_admission_profile_sampled             (true — the runtime_simd entry appears in the css_canon_bench samply sample with non-zero self-time; the addendum-6 profile-first runtime-reachability conjunct, NOT a source grep)
neon_significant_skip_matches_scalar       (PASS | FAIL — guard over the REAL 71KB-495KB corpora, not micro-cases)
checkasm_differential                      (PASS | FAIL — correctness plane; pre-H1 PASS/FAIL only)
css_scan_call_site_singular                (true — exactly ONE generated call site post-P3; re-emit-7-ways == FAIL)
significant_set_is_caller_data             (true — generator emits the CALL; kernel hand-authored once in bbnf-simd)
g6_speedup_median_mbps                     (corpus-in-timer ONLY; null pre-H1 — deferred to the H1 symmetric timer)
json_scan_rs_neutralized                   (retired | neutralized — the zero-sampled json/scan.rs; G5)
json_guard_held                            (51/51 within ±1.0% of SK-V18-open)
```

The `gate-json` consumer REJECTS the G5/G6 row if: `acceleration_at_admission != admission`;
`simd_admission_caller` empty; `simd_admission_profile_sampled != true` (the addendum-6 runtime-reachability
conjunct — a source-census-only PASS without profile attribution FAILS); `neon_significant_skip_matches_scalar
!= PASS`; `checkasm_differential != PASS`; `css_scan_call_site_singular != true`;
`significant_set_is_caller_data != true`; `json_scan_rs_neutralized ∉ {retired, neutralized}`;
`json_guard_held != true`; or a non-null `g6_speedup_median_mbps` NOT sourced from the corpus-in-timer plane
(e.g. sourced off the checkasm differential plane — the addendum-5 plane-mismatch REJECT; the figure is
admissible ONLY from the corpus-in-timer symmetric `css_canon_bench` harness). `g6_speedup_median_mbps` is null pre-H1 and does NOT gate G5/G6 (deferred to
H1); every other emitted column is consumed in the G5/G6 slice (no producer-only field).

**Caps + revert.** MED-HIGH; redress cap 45 min (G5 folds in, LOW). Revert the `runtime_simd` retarget
entry + scalar twin + checkasm extension + the generated call-site swap + the `json/scan.rs`
neutralization as ONE slice; restore `SK-V18-open` RESULTS; add REDRESS naming the seam that failed (the
≤13-byte salvage, the skip-stop boundary, the error-position reproduction, or the caller census).
**Downstream: G5/G6 does NOT block PROVE** (Sheets does not use the CSS NEON — PARALLEL). G5/G6 ∧ PROVE
both gate H1. Outcome `C` (pre-H1) → `A` on the H1 timed figure, `R` on miss.

---

## Section 9 — PROVE: Sheets via the un-forked generator ONLY (R-E-2 — precedence-tower core)

**The negative control.** Generalization is REAL only if a THIRD, structurally-distinct grammar
(`grammar/google-sheets/google-sheets.bbnf`, 185 lines) emits a working parser THROUGH the un-forked G3
generator — not JSON, not CSS, zero hand-authored runtime Rust. **R-E-2** emits the precedence-tower CORE
(the 7-level left-assoc tower `comparison→concat→add→mul→exp→unary→postfix→primary` + cyclic
`paren_expr→expression` + `Nu8` operator rules + `number`/`string`/`boolean`/`error_literal` leaves +
`func_call`'s one `<<`-separated arg list), DEFERRING the `cell_ref`/`range`/`LET`/`LAMBDA` aggregates the
grammar ITSELF leaves as raw `-> input : Span` (TODO AU.6.7, `google-sheets.bbnf:62,73-75`). The
**precedence tower is the SOLE Sheets-distinctive construct** JSON+CSS structurally lack
(`sheets_grammar_shape == pratt-operator`). The `Nu8`-tagged-alt family is NOT the litmus — it is SHARED:
CSS L4 uses `-> Nu8u8` **295×** across its import closure vs Sheets' 21×, so the generator must already
handle it at scale to emit CSS at all. R-E-3 (flattened precedence) is REJECTED as a hollow "third-JSON"
litmus; R-E-1 (maximal) is deferred (highest authoring + regression surface).

**Entry gate.** Per S-P2 §3 PROVE row: **G3 ∧ G4 closed** (transitively G1 ∧ P3). **PARALLEL to G5/G6.
PROVE NEVER admits before G4 closes** (the seq/C6 correction: G4 is PROVE's DIRECT predecessor, NOT
merely a transitive one — the §3 "transitively" phrasing that mis-scoped onto G4 is corrected here; PROVE's
entry is the explicit conjunction `G3 ∧ G4`, and its own exit gate requires the G4 trait instantiation,
so a not-yet-closed G4 BLOCKS PROVE). Concretely: **G3 closed** (the un-forked emitter renders
grammar-DERIVED bodies; it can render recursive `CallRule`/`RepeatLoop` chains from grammar structure;
Sheets is the FIRST grammar whose body CANNOT be a relabeled JSON/CSS courier, so a REDRESSed G3 HALTS
PROVE); **G4 closed** (the shared `Cursor`/`DocumentView` seam exists with the phantom `<G>` resolved by
DELETE; G4 is a DIRECT conjunct because the Sheets value type instantiates the R-D trait — the
phantom-`<G>` resolution made concrete by a third impl); **transitively G1 ∧ P3**. CHALLENGE accepts
(first-of-class, the generality stress): the tower is right-iterated EBNF (`A = B (op B)*`,
`google-sheets.bbnf:109`) lowering to the EXISTING `SinkOnlyExpr` vocabulary
(`Seq`+`RepeatLoop`+`Alt{Dispatch}`+`CallRule`, `lower/sink_only.rs:69-96`) — NO new IR/Pratt primitive;
the stress is on G3's GENERALITY, not a missing construct. The `cell_ref`/`range`/`LET`/`LAMBDA` deferral
is GROUNDED in the grammar's own TODOs, not a dodge.

**Tasks.** (1) Add the Sheets grammar root + xtask target (`.bbnf` referenced, not authored; the generated
runtime FALLS OUT of G3; the 25-LOC `sheets_witness/` stub either becomes the generated output dir or is
deleted; ~+30 LOC skinny grammar-root + xtask; total Sheets adoption ~+200 LOC). (2) Emit the
precedence-tower core THROUGH the un-forked G3 generator (`formula → comparison_expr → … → primary`, the
`Nu8` operator rules, the leaves, `paren_expr` the cyclic recursion, `func_call`; the body comes from the
grammar; NO hand-authored Sheets Rust). (3) Relax the import-closure requirement as DATA, not a branch
(Sheets has NO `@import`, so the RequestFacts contract's `import_closure: true`,
`grammar_provider.rs:263`, rejects it today; the relaxation is `present-iff-grammar-has-imports`, DERIVED
from grammar facts — a `RuntimeFrontendRequirements` data change, `import_closure_relaxation_is_data ==
true`, NEVER a `match grammar { GoogleSheets => … }` arm). (4) Instantiate the G4 shared trait over the
Sheets value type (`sheets_value_instantiates_g4_trait == true`) without LCD-flattening JSON's rich nav.
(5) Add the distinct `RuntimeTarget` row with `grammar_name = "google_sheets"` so the per-`grammar_name`
config-tuple collapse counts a genuine THIRD grammar (`generator_grammar_count == 3`), not a relabeled
CSS row (the row collapses to itself, count==1 per `grammar_name`, under the R16 full-row `PartialEq`).

**Exit gate (MEASURABLE).** `generated_md5_distinct == true` (`md5 -q` over
`grammars/{json,sheets,css_l4}/generated.rs` all distinct; a repeated md5 means a courier was reused —
REJECT); `sheets_verbatim_blob_present == false` (`grep -c 'const.*_RS.*r#' codegen/src` for any Sheets
blob == 0); `sheets_grammar_shape == pratt-operator` — NON-HOLLOW PROVEN BY A CONCRETE STRUCTURAL
FALSIFIER, NOT asserted "by construction": the emitted `grammars/sheets/generated.rs` MUST contain the
7 DISTINCT precedence-level parse fns chained as a recursive `CallRule` tower
(`comparison→concat→add→mul→exp→unary→postfix→primary`), machine-checked by counting the emitted
per-level descent fns (`>= 7` non-terminal levels, each calling the next) AND the cyclic
`paren_expr→expression` back-edge; a `flat-stream` (one linear scan — the R-E-3 flattened-tower REJECT
predicate) or a `tree` (single recursive value match) emission has FEWER than 7 chained level fns and
FAILS the count → `sheets_grammar_shape != pratt-operator` → REJECT; `generator_grammar_branch_count == 0` (the arm-census
`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>'
skinny/crates/codegen/src skinny/xtask/src` → 0; `GoogleSheets` un-abbreviated; Sheets renders from the
SAME `render(program)` path); `generator_grammar_type_count == 0` (`rg
'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap'` → 0; G4 (a CLOSED PROVE predecessor) has
already DELETED the `EventGrammar`/`AnyGrammar` axis entirely, so post-G4 NO `EventGrammar` literal can
be emitted by construction — the residual `rg 'EventGrammar|\*EventGrammar' grammars/sheets/generated.rs`
== 0 is satisfied by the G4 deletion, with the P4 `FORBIDDEN_GENERIC_TOKENS ⊇ {EventGrammar,
*EventGrammar}` extension standing as the defence-in-depth emit-site catch should any axis residue
survive; Sheets is the FIRST grammar that WOULD have exercised the witness-emission coupling had G4 not
deleted the axis); `generator_grammar_count == 3` (json + css_l4 + google_sheets) ∧
`runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`, both nested structs);
`sheets_value_instantiates_g4_trait == true` (PROVEN by a concrete falsifier, NOT asserted: `rg 'impl\s+(Cursor|DocumentView)\b.*\bfor\b' grammars/sheets/` is NON-EMPTY AND the crate compiles with the Sheets value type bound to the G4 seam; an absent impl block or a compile failure → `sheets_value_instantiates_g4_trait == false` → REJECT, isomorphic to the G4.2-conjunct-3 substitution falsifier) ∧ `import_closure_relaxation_is_data == true`; `w5a_sheets`
flips from "fails closed: missing import closure" to "emits a working parser" via the
import-closure-derived-from-facts relaxation; `sheets_emission_path == generator-only` (the BINDING
fallback).

**Falsifiers + the binding fallback (the negative-control teeth).** The litmus FAILS — and each failure
is surfaced HONESTLY as `sheets_emission_path == shim`, the negative-control verdict `N` (generalization
NOT real per the §0.3 enum), NEVER paper-closed — if ANY of: (a)
Sheets `generated.rs` needs a `const SHEETS_GENERATED_RS` courier; (b) G3 routes Sheets via a
`GoogleSheets =>` arm; (c) `sheets_grammar_shape != pratt-operator`; (d) the Sheets value type cannot
instantiate the G4 trait without LCD-flattening JSON's rich nav. **The binding fallback (§5-risk-5):** if
Sheets cannot emit via the generator ONLY, **generalization is NOT real — surface honestly, do NOT
stub-prove, do NOT hand-write a `_GENERATED_RS` Sheets block.** If the precedence tower breaks because G3
cannot render recursive `CallRule`/`RepeatLoop` chains from grammar structure, that becomes a §6
honest-finding: a named, `.bbnf`-invoked, parameterized precedence primitive with a scalar/checkasm
reference — never a silent blob, never a paper-close. PROVE does not paper-close (G1/G3 iterate; PROVE
does not).

**Telemetry (PD-specific columns).**

```text
generated_md5_distinct                     (true — md5 over grammars/{json,sheets,css_l4}/generated.rs all distinct)
sheets_verbatim_blob_present               (false — grep -c 'const.*_RS.*r#' for any Sheets blob == 0)
sheets_grammar_shape                        (pratt-operator — NOT flat-stream/tree)
generator_grammar_branch_count             (0 — no GoogleSheets => arm)
generator_grammar_type_count               (0 — no GoogleSheetsParser/SheetsEventGrammar literal)
generator_grammar_count                    (3 — json + css_l4 + google_sheets)
runtime_target_rows_collapsed              (true — RuntimeTarget: PartialEq full-row, incl. BOTH nested structs)
sheets_value_instantiates_g4_trait         (true — the Sheets value type instantiates the R-D Cursor/DocumentView seam; PROVEN by `rg 'impl\s+(Cursor|DocumentView)\b.*\bfor\b' grammars/sheets/` NON-EMPTY AND the crate compiles with the Sheets value type bound to the G4 seam, NOT asserted — an absent impl or compile failure → false → REJECT, isomorphic to the G4.2-conjunct-3 substitution falsifier)
import_closure_relaxation_is_data          (true — present-iff-grammar-has-imports from facts, NOT a match-grammar arm)
sheets_emission_path                       (generator-only | shim — shim == N negative-control fail, generalization NOT real)
json_css_preservation_held                 (true — JSON 91.5% leaf byte-equal + 51/51 guard within ±1.0% of SK-V18-open AND CSS track1_rich/lcss preserved by byte-equivalence of grammars/css_l4/generated.rs vs the G3-closed shipped file (dirty_generated_state == clean) — NOT a fresh corpus-in-timer re-measure (PROVE runs post-G2, the pre-G2 same-run baseline is gone per close-cond #6; PROVE preserves the CSS leaf by byte-equality, not a ratio bench); the shared render(program) body PROVE re-touches must not perturb the JSON/CSS hot leaves; dirty_generated_state == clean for grammars/{json,css_l4}/generated.rs — binds the §0.5 across-every-generalization-wave floor at the one emit-path-touching wave-slice)
```

The `gate-json` consumer REJECTS the PROVE row if: `generated_md5_distinct != true`;
`sheets_verbatim_blob_present != false`; `sheets_grammar_shape != pratt-operator` (the < 7 chained
level-fn count, the concrete structural falsifier — not asserted by construction);
`generator_grammar_branch_count != 0`; `generator_grammar_type_count != 0`; `generator_grammar_count != 3`;
`runtime_target_rows_collapsed != true`; `sheets_value_instantiates_g4_trait != true`;
`import_closure_relaxation_is_data != true`; `json_css_preservation_held != true`. A `sheets_emission_path == shim` is NOT a gate-REJECT but the
BINDING negative-control verdict (outcome `N`, surfaced honestly — generalization NOT real); the gate does
NOT paper-close a shim as a pass. Every emitted column is consumed in the PROVE slice (no producer-only
field).

**Caps + revert.** MED-HIGH; redress cap 45 min (the un-forked emitter generality stress is the most
likely REDRESS site). Revert the Sheets grammar root + xtask target + the `RuntimeTarget` row + the
import-closure relaxation + the generated Sheets runtime as ONE slice; restore RESULTS; add REDRESS naming
the construct the generator could not lower (the precedence tower is the expected break point).
**Downstream: PROVE ∧ G5/G6 both gate H1.** Outcome `A`/`C`/`G` (generator-only) on close, `N` on the
binding-fallback fail (the shim/`sheets_emission_path == shim` case — generalization NOT real, surfaced
honestly per the §0.3 enum, NEVER paper-closed).

---

## Section 10 — H1: CSS framing honesty + corpus-in-timer + regen --check clean (the honesty close)

**The honesty close.** Lever: disclose the CSS materialization framing, bind the corpus-in-timer
symmetry, produce the deferred G6 speedup figure on the symmetric timer, and prove the regen is clean. H1
is the LAST wave.

**Entry gate.** Per S-P2 §3 H1 row: **G5/G6 ∧ PROVE closed.** G5/G6 closed (`acceleration_at_admission ==
admission`, `checkasm_differential == PASS`, `neon_significant_skip_matches_scalar == PASS` over the real
corpora). PROVE closed (`sheets_emission_path == generator-only`, `generator_grammar_count == 3`, the four
addendum-2 co-gates GREEN).

**Tasks.** (1) **Disclose `materialization_framing == lazy-rich-vs-eager-cssom`** (the honest S-P1
framing; `track1_rich` is LAZY-rich, `css_l4_declaration_values/generated.rs:297-304`, re-deriving every
field from `(source, offset)` spans, writing nothing to the arena — equal-depth typed value work vs the
lightningcss full CSSOM, NOT a count-only structural probe; the materialization-depth asymmetry disclosed
EXPLICITLY; an unqualified "beats CSSOM"/"equal-work" claim behind the lazy-rich re-label is a REJECT,
R-A0-1/R14). (2) **Bind `corpus_in_timer == true`** (the symmetric `css_canon_bench` plane, both sides
equal work on the REAL corpus, COLD, no micro-fixtures, no more-work-competitor). (3) **Produce the
DEFERRED G6 speedup figure** on the symmetric timer (addendum 5 enforced HERE; `g6_speedup_median_mbps`
comes from the corpus-in-timer harness; it inherits §5-risk-7's QUIET-recapture caveat — the S-P1 capture
ran under load loadavg 4.35, so absolute Mbps is DIRECTIONAL until a QUIET re-capture; H1 requires the
quiet re-capture before any ABSOLUTE Mbps claim). (4) **Prove `regen_check_clean == true`** (`cargo xtask
regen --check` exit 0; resolve the git-dirty generated CSS files as clean regen). (5) **Confirm the G2
>SOTA-regression gate held** (same-run `track1_rich/lightningcss > 1.0×` per corpus with no same-run
regression vs the pre-G2 baseline on `css_canon_bench`; the S-P1 absolutes — bootstrap 2.190×,
tailwindcss 3.375×, material-components-web 1.658× min, animate 2.101× — are DIRECTIONAL antecedents the
H1 quiet re-capture re-confirms directionally, NOT the binding floor; oracle parity alone does not prove
throughput preservation).

**Exit gate (MEASURABLE).** `materialization_framing == lazy-rich-vs-eager-cssom` disclosed (the lazy-rich
asymmetry EXPLICIT; no unqualified "beats CSSOM" claim); `corpus_in_timer == true`; `regen_check_clean ==
true`; the G6 speedup figure, if claimed absolute, rides a QUIET re-capture (loadavg-clean), else reported
DIRECTIONAL with the load caveat (outcome `S` — honest residual, not `A`); `json_guard_held == true`
(51/51 within ±1.0% of SK-V18-open — the pinned baseline); CSS >SOTA same-run ratio `> 1.0×` on ≥1
regular corpus (animate OR bootstrap), re-confirmed DIRECTIONALLY against the G2-RECORDED
`track1_rich_over_lcss_ratio_pre_g2` baseline — the pre-G2 code is gone post-G2 so H1 never re-measures
pre-G2 same-run, per close-cond #6 — the SAME binding floor G2 closed on (§0.5); a tailwind miss recorded
as an honest residual at G2 is re-confirmed as a residual here, NOT re-litigated as an H1 block (the S-P1
absolutes are the DIRECTIONAL antecedent, not the binding floor).

**Falsifiers.** Framing: an unqualified "beats CSSOM"/"equal-work" close-report claim behind the lazy-rich
re-label, WITHOUT the materialization-depth asymmetry disclosed → R-A0-1 REJECT. Plane: a speedup figure
NOT from the corpus-in-timer symmetric harness → addendum-5 REJECT. Regen: hand-patch a generated file →
`regen --check` exit ≠ 0 → RED; revert. Load: an ABSOLUTE Mbps claim emitted with the harness-recorded
`host_loadavg` (1m) ≥ 1.0 (the binding quiet bar; the S-P1 capture ran at 4.35, S-P1 §0) without a
quiet re-capture → §5-risk-7 REJECT (directional only). The H1 harness MUST stamp `host_loadavg` in the
report row; an absolute claim with `host_loadavg ≥ 1.0` or no `host_loadavg` stamp is RED.

**Telemetry (H1 columns).**

```text
materialization_framing                    (lazy-rich-vs-eager-cssom — disclosed explicitly; undisclosed turns the gate RED)
corpus_in_timer                            (true)
regen_check_clean                          (true — cargo xtask regen --check exit 0)
host_loadavg                               (the 1m loadavg stamped on the H1 capture; an ABSOLUTE Mbps claim REQUIRES < 1.0)
g6_speedup_median_mbps                     (the DEFERRED G6 figure produced HERE on the corpus-in-timer symmetric timer; if claimed ABSOLUTE it rides host_loadavg < 1.0, else DIRECTIONAL — outcome S)
json_guard_held                            (51/51 within ±1.0% of SK-V18-open — the pinned baseline)
css_sota_ratio_held                        (same-run track1_rich/lightningcss > 1.0× on ≥1 regular corpus (animate OR bootstrap), re-confirmed DIRECTIONALLY against the G2-RECORDED track1_rich_over_lcss_ratio_pre_g2 baseline — the pre-G2 code is gone post-G2 so H1 never re-measures pre-G2 same-run, per close-cond #6; the SAME binding floor G2 closed on (§0.5), a tailwind miss recorded as an honest residual at G2 is re-confirmed as a residual here NOT re-litigated as an H1 block; the S-P1 absolutes DIRECTIONAL, not the floor)
```

The `gate-json` consumer REJECTS the H1 row if: `materialization_framing != lazy-rich-vs-eager-cssom`
(an `undisclosed` or any unqualified "beats CSSOM" re-label is RED); `corpus_in_timer != true`;
`regen_check_clean != true`; `json_guard_held != true`; `css_sota_ratio_held != true`; an absolute
`g6_speedup_median_mbps` claim emitted with `host_loadavg >= 1.0` or no `host_loadavg` stamp (the
§5-risk-7 quiet-bar — such a claim is RED; reporting the figure DIRECTIONAL with the load caveat is
outcome `S`, not a REJECT); or a `g6_speedup_median_mbps` NOT sourced from the corpus-in-timer plane
(e.g. read off the checkasm differential plane — addendum-5 plane-mismatch). Every emitted column is consumed in the H1 slice (no producer-only field).

**Caps + revert.** LOW; standard 30-min redress cap. No source revert by default (documentation +
symmetric re-measure + regen-check). On mismatch, reopen the producing wave (G6 for the speedup figure,
PROVE for the Sheets row) or mark close blocked with a mismatch list naming file paths, rows, and missing
evidence. **Downstream: on H1 close, the SK-V18 generalization closes.** Outcome `A` (absolute, quiet
re-capture) or `S` (directional residual) on close, `R` on miss.

---

## Section 11 — Pre-Blocked Routes (the route ledger, per-wave-attributed)

| Wave | Must NOT re-open (load-bearing) |
|---|---|
| P1 | a deletion list narrower than the verify grep (the RED-by-construction reach hazard); deleting `src/x86_64/` without the same-commit `checkasm_parity.rs` decouple (broken-build state); any x86/AVX/SVE/nasm survivor |
| P2 | deleting `css_canon_bench` or the 9-field oracle with the warm path; an Mbps figure from the warm micro-fixture plane |
| P3 | minting fake `.bbnf` roots to satisfy a distinctness gate (the EXACT overfit addendum 2 forbids); erasing a real `profile` discriminator; a hand-rolled prose-field row-compare (shallow-compare false-green); md5-distinct treated as sufficient |
| P4 | a green-by-exclusion gate; routing the codegen surface through the weaker `SKV15_W2_EXTRA_COVERAGE_ROOTS`; dropping the `diagnostic-x86` exclusion on a still-present tree (commit P1 first/same-wave) |
| P5 | hand-patching the shipped `json/generated.rs` (fix at the SOURCE); a surviving `w[0-9]+`/`sk_v`/corpus tag |
| G1 | a JSON throughput regression on the 91.5% leaf; an LCD-unify of the dispatch triple; a relabeled fragment as a leaf primitive (fails (b)/(d)); trading the SinkOnly blob for the `_RS` blob (only one folded); a courier-swap passing byte-equivalence but failing the `.bbnf`-mutation falsifier |
| G2 | the full grammar-IR tree-walk (R-B Candidate C); a hand-patched scan body passing the oracle but failing the arg-mutation falsifier; a NEUTRALLY-named CSS-only primitive (FORCED to `css_balanced_component_scan` per s6/C4); a god-kernel absorbing the drivers/projection (fails (d)); re-deriving the scan into 7 byte-identical files; any >SOTA admission off the corpus-in-timer plane; a same-run ratio ≤ 1.0× OR a same-run regression vs the pre-G2 baseline reported as a pass; the load-depressed S-P1 absolute treated as a binding floor (it is DIRECTIONAL only) |
| G3 | the relocated seam (un-forking the visible enum while leaving a per-grammar branch in a neutral data table — `target.profile`-selected `ProjectionSpec`); a per-grammar `match grammar_name` arm; md5-distinct as sufficient; a behaviour change to the JSON/CSS bodies masquerading as an un-fork |
| G4 | a tree-shaped `Value` stack (Candidate B); a `DocumentView`+stream-only trait (Candidate C); a degenerate-equal CSS impl; deleting the REAL `K` (Kind) axis; a second substrate; an eager value tree; a `_proof_compiles` test false-greening the phantom grep |
| G5/G6 | x86/AVX/SVE; a kernel without a profile anchor (orphan); re-emitting the call site 7 ways (P3 re-fork); a test-only admission proof; a `g6_speedup_median_mbps` from the checkasm plane (not corpus-in-timer); an orphan JSON classifier (G5 authors NOTHING for JSON) |
| PROVE | a `const SHEETS_GENERATED_RS` courier; a `GoogleSheets =>` arm; a flattened precedence tower (R-E-3); an `import_closure` `match grammar` arm; a stub-prove / hand-written Sheets block on the binding-fallback fail |
| H1 | an unqualified "beats CSSOM"/"equal-work" claim behind the lazy-rich re-label; an absolute Mbps claim without the quiet re-capture; a speedup figure off the corpus-in-timer plane; a hand-patched generated file; a corpus-average substituting for per-corpus ratios |

---

**Close condition restated:** SK-V18 closes when ONE grammar-driven generator emits JSON + CSS + Sheets
from `.bbnf` (each grammar-DERIVED, `verbatim_blob_present==false`, `generator_grammar_count==3`),
through ONE un-forked emitter dispatched on the lowered `BackendShape` (`emitter_fork_present==false`,
`emit_shape_source==lowered_program`, `runtime_target_rows_collapsed==true`), with a shared value-API
trait both JSON and CSS (and Sheets) instantiate ≥2 ways without LCD-flattening
(`json_rich_navigation_preserved==true`), the phantom `<G>` deleted (`phantom_generic_resolved==deleted`),
the >SOTA preserved honestly (CSS same-run `track1_rich/lcss > 1.0×` on ≥1 regular corpus (animate OR
bootstrap), with no same-run regression vs the
pre-G2 baseline, cold corpus-in-timer — the S-P1 absolutes DIRECTIONAL, not the binding floor; JSON 51/51
strict-vs-sonic-rs; `materialization_framing==lazy-rich-vs-eager-cssom` disclosed), x86 gone
(aarch64-only), the Lock-14 gate meaningful (`lock14_gate_scans_codegen==true`), Sheets proving the
generalization is real (`sheets_grammar_shape==pratt-operator`, no `_RS` blob, the binding fallback
honored), the NEON reaching admission (`acceleration_at_admission==admission`), regen clean, and net ≈
−10800 LOC.

**GROUND re-validation disposition (V5-independent, folded):** `sota.md` accept=13/revise=0/reject=0 —
zero SOTA defect; the P5↔G1 call-site sequencing note (the P5 metalang rename touches the JSON hot-leaf
call sites at `json/generated.rs:841`/`:881`) is folded into §3.5 + §4 as the binding P5-before-G1
ordering with G1's identical-call-site byte-equivalence re-asserted on the regenerated file. `seq.md`
accept=8/revise=2 — C6 (PROVE never admits before G4 closes; G4 is PROVE's DIRECT, not merely transitive,
predecessor) and C7 (G5/G6 needs only G3 and runs PARALLEL to G4, NOT under G4) are folded into the §2.1
lattice + §8 + §9 entry gates. `s6.md` accept=6/revise=1 — C4 (the `balanced_component_scan` neutrality
dischargers JSON `{}`/`[]` and Sheets `paren_expr` are parse-with-emit descents structurally incompatible
with the CSS byte-SKIP shell, so the demotion to `css_balanced_component_scan` is the FORCED outcome) is
folded into Section 1 + §5 (G2). No REJECT surfaced; the candidate shortlist and PRUNE→G1..G6→PROVE→H1
lattice stand.

**Next move:** ready-for-S-P3-CHALLENGE (the 7-lens 3Z harden of this manifest) and, on close,
ready-for-wave-implementation (W-PRUNE P1–P5 dispatch-eligible FIRST — pure deletion + gate-tightening,
no entry-gate, P4 carrying the BEFORE-G2/G3 obligation and P5 the BEFORE-G1 obligation). Thereafter the
GENERALIZE/PROVE/HONESTY waves dispatch only as each predecessor closes its exit gate and its entry-gate
predicate holds GREEN. SK-V19 is the totality-fold tranche (`crates/core/` adoption) + BBNF-self as the
fourth-grammar litmus.
