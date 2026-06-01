# SK-V18 S-P3 PLAN PE — Cross-Cutting SPEC Sections 0 / 1 / 2.1 (Close Condition, Non-Negotiables, Generality Gate)

Date: 2026-06-01. Cycle: S-P3 (synthesis-PLAN). This is the cross-cutting spine of the
SK-V18 wave manifest — the GLOBAL close condition, the comparator classes, the outcome enum,
the executable `--skv18-generalization-report` telemetry schema, the standing-law non-negotiables,
and the Generality + Lock-14 gate EVERY wave carries. The per-wave sections (W-PRUNE P1..P5,
G1, G2, G3, G4, G5/G6, PROVE, H1) are authored by PA-PD; the SPEC-assembler folds PA..PE into
`restart/skinny/tranches/sk-v18/SPEC.md`. This file IS Sections 0/1/2.1 of that SPEC.

Authority (every gate here is GROUNDED in these, not invented):

- `restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — the 6 binding
  addenda (§1), the residual census R1..R16 + R-A0-1..3 (§2), the PRUNE-list P1..P5 (§4), the
  sequencing constraints (§5), the CH7 lens (§6), the §6 named-primitive escape (R-A0-3 / §6).
- `restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md` — the candidate shortlist
  (§1), the coupling lattice (§2), the PRUNE→G1..G6→PROVE→H1 sequencing with per-wave exit-gates
  (§3), the §6 findings + the (a)-(d) primitive gate (§4), the residual risks (§5).
- `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md` — the hot leaves (CSS
  `find_component_delim` 79.5% / scalar-scan 94.1%; JSON `parse_object_value_at_direct` +
  `parse_array_element_at_direct` 91.5%; G6=WIRE), the load-honesty caveat (§0), the
  lazy-rich-vs-eager-cssom framing ground (§0/§3).
- `restart/skinny/tranches/sk-v17/SPEC.md` — the STRUCTURE TEMPLATE (Section 0 / 1 / 2.1) mirrored.

Host: aarch64 / Apple M5 Max ONLY. x86 is a PRUNE target (P1), not a measured plane.

Dispatch lock: No SK-V18 implementation wave dispatches from S-P3 itself. W-PRUNE is the only
dispatch-now-eligible cluster on close of S-P3; every GENERALIZE/PROVE/HONESTY wave remains
blocked until its predecessor closes its exit gate AND its entry-gate predicate holds GREEN.

---

## Section 0 — Close Condition And Goalset

### 0.1 — Global Close Condition

SK-V18 is the GENERALIZATION cycle — the inflection backtrack. It closes only when ALL of
these are simultaneously true. Every surface citation is the benched skinny tree
(`skinny/crates/`); each generator/gate is verifiable by grepping `skinny/crates/`, NOT
`crates/core/` (the TOTALITY tree is the SK-V19 adoption target, not the SK-V18 benched
surface). Net LOC ≈ **−10800** (the campaign DELETES far more than it adds; SYNTHESIS-AUDIT §4).

1. **ONE generator emits JSON + CSS + Sheets from `.bbnf`.** A single grammar-driven generator
   exists in `skinny/crates/codegen/`; it consumes the three `.bbnf` roots (JSON, CSS, Sheets)
   and emits three NON-IDENTICAL `generated.rs` parsers, each grammar-DERIVED — NOT a const
   `&str` courier, NOT a fixed-literal `render()`, NOT a relabeled blob. `generator_grammar_count
   == 3` (json + css + sheets — NOT json + 7-css + sheets, the P3 collapse, SYNTHESIS-AUDIT R-A0-2).
   The CSS const courier (`runtime_generator.rs:701 CSS_GENERATED_RS`) and the JSON 7× `push_str`
   fixed-literal (`json_sink_direct.rs`) are both RETIRED; `verbatim_blob_present == false`
   campaign-wide.

2. **One un-forked emitter path, dispatched on the LOWERED program, not a grammar tag.**
   `RuntimeEmitterKind{CompiledLowering,RequestFacts}` (`grammar_provider.rs:40-42`) is DELETED;
   `runtime_generator.rs:16` no longer forks on a grammar-family kind. `render(program)` reads its
   output-shape ONLY from `program.policy_summary.backend_shape` (`sink_only.rs:48`, the
   grammar-NEUTRAL 5-shape `BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`,
   `lower/mod.rs:18`), NEVER from a `RuntimeTarget` field. `emitter_fork_present == false`;
   `generator_grammar_branch_count == 0`; `generator_grammar_type_count == 0`;
   `emit_shape_source == lowered_program`.

3. **The relocated seam is closed structurally, not just by arm-grep.** The 7 byte-identical
   `css_l4_*/generated.rs` replicas (md5 `b654562c…`) collapse to ONE CSS config; the 7 xtask
   `RuntimeTarget` rows collapse via the R16 recipe — `RuntimeTarget: PartialEq` full-row derive
   (+1 line; today `Clone,Copy,Debug` only, `regen.rs:5`) that recurses into BOTH nested structs
   `frontend_requirements` (field #11, `regen.rs:17`) AND `output_labels` (field #12, `regen.rs:18`)
   automatically. `runtime_target_rows_collapsed == true`.

4. **The shared value-API trait — both JSON and CSS (and Sheets) instantiate ONE seam.** A thin
   `Cursor`/`DocumentView` micro-trait over the surviving `ValueRef<K>` is extended to CSS and
   Sheets; it shares ONLY the laziness/cursor contract, NEVER navigation. JSON's rich tree
   (`get`/`pairs`/typed `JsonValue`/recursive visitor) is preserved BY CONSTRUCTION —
   `json_rich_navigation_preserved == true`. ≥2 real impls (JSON `ValueRef<K>`, CSS `CssNode`)
   that CANNOT collapse to the lesser. (The trait may NOT LCD-flatten JSON's rich navigation to a
   CSS-flat sweep; SYNTHESIS-RESEARCH §5-risk-4.)

5. **The phantom `<G>` is resolved by DELETE.** `tape/mod.rs:175 ValueRef<…G: EventGrammar =
   AnyGrammar>` + the latent `DocumentView`/`type Root` (`:227-228`) — zero non-test production
   consumers (the `_proof_compiles` census excluding `_tests.rs` is EMPTY; SYNTHESIS-RESEARCH
   preamble). DELETE the `<G>` axis (preserving the REAL `K=Kind` axis untouched).
   `phantom_generic_resolved == deleted`.

6. **>SOTA preserved HONESTLY — CSS beats lightningcss, JSON beats sonic-rs strict.** On the
   cold, corpus-in-timer canonical harness (`css_canon_bench`, the P2-survivor path), the
   regenerated grammar-driven CSS parser holds `track1_rich/lightningcss >= the S-P1 ratio`
   per corpus (the load-robust S-P1 ratios: bootstrap 2.190, tailwindcss 3.375,
   material-components-web 1.658, animate 2.101). JSON's 51/51 strict-vs-sonic-rs cold rows
   remain admitted, same-plane. The 94.1% CSS scalar scan and the 91.5% JSON sink leaves are
   re-emitted with throughput-equivalent bodies; `corpus_in_timer == true`. Oracle parity
   (9-field cssparser / 51-row JSON strict) holds BEFORE any speed admission.

7. **x86 is gone (aarch64-only).** BOTH x86 surfaces (`src/x86_64/` 24 files + `ext/x86/`
   vendored ASM + the nasm `build.rs` driver) are DELETED crate-wide; `find …/src/x86_64
   …/ext/x86 -type f == 0`; crate-wide aarch64-neutral grep clean; `cargo build` + `cargo test
   --no-run` clean (the falsifier per P1). The single-arch kernel surface is the R-F retarget host.

8. **Lock-14 gate is MEANINGFUL (no green-by-exclusion).** `runtime_generator.rs` + the JSON
   sink/typed/template surfaces are moved from the weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` into
   strict `GENERIC_SCAN_ROOTS`; `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}`;
   the `diagnostic-x86` exclusion is dropped. `lock14_gate_scans_codegen == true`; the
   re-inject-a-`JsonSink`-token falsifier turns the gate RED, then is reverted. P4 lands BEFORE
   G2/G3 (so the un-forked emitter is neutrality-scanned AS it is authored).

9. **Sheets proves the generalization is REAL (the negative control).** `google-sheets.bbnf`
   emits a working parser THROUGH the un-forked G3 generator ONLY — its 7-level precedence tower
   is structurally unlike both JSON and CSS, so it CANNOT be a relabeled JSON/CSS courier.
   `sheets_grammar_shape == pratt-operator` (non-hollow); Sheets `generated.rs` md5-DISTINCT from
   JSON ∧ CSS; no `const.*_RS.*r#` Sheets blob; the Sheets value type instantiates the G4 trait.
   `w5a_sheets` flips from "fails closed: missing import closure" to "emits a working parser" via
   the import-closure-DERIVED-FROM-FACTS relaxation (a frontend-requirements DATA change, NOT a
   `match grammar` arm). **BINDING FALLBACK:** if Sheets cannot emit via the generator ONLY,
   generalization is NOT real — surface HONESTLY, do NOT stub-prove, do NOT hand-write a
   `_GENERATED_RS` Sheets block (SYNTHESIS-RESEARCH §5-risk-5).

10. **NEON acceleration reaches the hot path AT ADMISSION.** The checkasm-gated
    `bbnf-simd::find_ascii_set_member64`/`byte_class_from_eq_set_64` kernel is RETARGETED onto the
    scalar recursive shell of `find_component_delim` (the measured 94.1% CSS hot leaf), landing as
    a SHARED grammar-neutral runtime primitive the (P3-collapsed singular) generated scan CALLS —
    NOT bespoke vector code re-emitted per-grammar. `acceleration_at_admission == admission` proven
    by the generated-`generated.rs` caller census (`rg runtime_simd::find_… …/grammars/*/generated.rs`
    non-empty), NOT a `#[cfg(test)]` caller. The zero-sampled `json/scan.rs` (S-P0 R12) is
    neutralized/retired (cheap; no JSON classifier authored — S-P1 §2 has NO JSON G5 hot leaf).

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
| CSS L4 `track1_rich` (grammar-driven, post-G2/G3) | lightningcss full-CSSOM | lazy-rich typed projection vs eager full CSSOM build | THE CSS >SOTA bar; cold, corpus-in-timer, same-run, N≥50 median; the `track1_rich/lightningcss >= S-P1 ratio` gate (G2 exit) |
| CSS L4 `track1_full_parse` | lightningcss full-CSSOM | recognition-only vs full CSSOM | recognizer floor; A-shaped but 4-field, NOT preserve-rich-ast — does NOT by itself discharge the typed close |
| CSS cssparser 9-field oracle | — | typed equality reference | EXACT correctness anchor; gate BEFORE speed; structurally distinct from `track1_rich` |
| JSON Track 1 (SinkOnly direct, post-G1) | sonic-rs strict | strict-vs-strict, same-plane | THE JSON >SOTA bar; the 51/51 cold strict guard carried from the W0 lock |
| JSON Track 2 / 51-row oracle | — | strict equality reference | per-iter equality anchor; structurally distinct from Track 1 |

**CSS framing honesty (the binding H1 disclosure, SYNTHESIS-AUDIT R14 / R-A0-1).** The CSS bar
is `lazy-rich-vs-eager-cssom`: `track1_rich` (`rich_summary`, 9 materialized fields) is LAZY-RICH —
it re-derives every field from `(source, offset)` spans, writing NOTHING to the arena
(`css_l4_declaration_values/generated.rs:297-304`), comparable to lightningcss's eager full-CSSOM
build (equal-depth typed value work, not a count-only structural probe). The honest framing MUST be
disclosed EXPLICITLY at H1: `materialization_framing == lazy-rich-vs-eager-cssom`. An unqualified
"beats CSSOM" / "equal-work" claim behind a re-label, WITHOUT the materialization-depth asymmetry
disclosed, is a REJECT (R-A0-1, a0 §4). The symmetric-comparator branch is preferred; the re-label
branch is acceptable ONLY with the asymmetry disclosed.

**JSON framing honesty.** JSON is strict-vs-sonic-rs strict, same-plane, per-iter oracle equality;
no framing asymmetry — the existing W0-locked >SOTA proof, carried forward.

**Load-robustness caveat (cross-cutting, S-P1 §0 / SYNTHESIS-RESEARCH §5-risk-7).** The S-P1
absolute Mbps capture ran under concurrent-session load (loadavg 4.35) and is DIRECTIONAL, NOT
re-locked. The load-robust ground-truths are (1) the same-run `track1_rich/lcss` ratios and (2) the
relative hot-leaf rank. A QUIET re-capture is required before ANY absolute Mbps claim in H1; G6 may
report only its checkasm PASS/FAIL pre-H1, and any speedup FIGURE comes from the corpus-in-timer
symmetric harness (same plane both sides), deferring the speedup CLAIM to the H1 symmetric timer.

### 0.3 — Outcome Enum

```text
A   admit-shaped (> the >SOTA bar, on its honest plane)
C   correctness (oracle parity; gate before speed)
G   GO (substrate/gate green, no behavior-regressing change)
L   loss (crosses no bar; honest residual recorded, NOT paper-closed)
N   negative-control fail (Sheets cannot emit via the generator ONLY → generalization NOT real)
P   prune-clean (a P-wave: deletion + gate-tightening verified, zero generalization risk)
R   redress (a wave failed its exit gate; blocks every downstream entry-gated wave)
S   substrate-guard / non-SOTA (admission-capable parse that does not cross the bar)
```

`A`/`C`/`G`/`L`/`S` are valid emitter-wave outcomes. `N` is the BINDING negative-control verdict
reserved for PROVE (Sheets): an `N` means generalization is NOT real and is surfaced honestly,
NEVER stub-proved. A recognition-only `A` (`track1_full_parse`, 4-field, NOT preserve-rich-ast)
does NOT by itself discharge the CSS typed >SOTA close. A `P` wave carries zero generalization
risk (pure deletion + gate-tightening) and deletes no >SOTA-bearing code.

### 0.4 — Required Telemetry (the `--skv18-generalization-report` schema)

The gate consumer is
`(cd skinny && cargo xtask gate-json --check-results --skv18-generalization-report <path>)`.
It REJECTs the run if ANY required column is missing, mis-typed, or a producer-only field (a column
emitted but never consumed FAILS the wave — typed-materialization-invariant). The 13 binding
generalization columns (each grounded in a named addendum / residual):

```text
verbatim_blob_present            (bool; addendum 1 — MUST be false at G1/G2 close; a @generated &str literal in codegen = REJECT)
generator_grammar_branch_count   (int;  addendum 2/3 — MUST be 0 at G3; a generic branch selecting on grammar name/family = REJECT)
generator_grammar_type_count     (int;  addendum 2/3 — MUST be 0 at G3; a grammar-named type discriminating the emit path = REJECT)
runtime_target_rows_collapsed    (bool; addendum 2/R16 — MUST be true at G3/P3; the STRUCTURAL co-gate the arm-grep cannot see; computed via RuntimeTarget: PartialEq full-row over BOTH nested structs)
emit_shape_source                (enum {lowered_program|runtime_target}; addendum 3 — MUST be lowered_program at G3; render(program) reads NO target.profile/target.emitter/target.output_labels/target.profile_contract — the relocated-seam falsifier)
emitter_fork_present             (bool; addendum 3 — MUST be false at G3; RuntimeEmitterKind/CompiledLowering/RequestFacts grep == 0)
phantom_generic_resolved         (enum {deleted|instantiated|present}; addendum 4 — MUST be deleted at G4; the <G>=EventGrammar axis, test-excluded)
json_rich_navigation_preserved   (bool; addendum 4 — MUST be true at G4; makes the ≥2-impl count necessary-not-sufficient; preserve-rich-ast)
acceleration_at_admission        (enum {admission|cfg_test|none}; addendum 6 — MUST be admission at G6; generated-caller census non-empty, NOT #[cfg(test)])
sheets_grammar_shape             (enum {pratt-operator|courier|hollow}; addendum 2 / R-E — MUST be pratt-operator at PROVE; a courier/hollow shape = relabeled, REJECT)
generator_grammar_count          (int;  addendum 2 — MUST be 3 at PROVE (json+css+sheets); 7-css inflation = the P3 overfit, REJECT)
corpus_in_timer                  (bool; addendum 5 — MUST be true at G2/G6/H1; the real corpus inside the timed region, cold, no warm micro-fixtures, no more-work competitor)
materialization_framing          (enum {lazy-rich-vs-eager-cssom|...}; addendum 5/R14 — MUST be disclosed at H1; the honest CSS framing, no unqualified "beats CSSOM" behind a re-label)
```

Supporting columns the gate also consumes (per-wave evidence the above 13 lean on):

```text
css_corpus / css_sample_count(>=50) / css_sample_statistic(median) / css_sample_mode(cold)
css_track1_rich_median_mbps / css_lightningcss_full_cssom_median_mbps / track1_rich_over_lcss_ratio
css_typed_summary_equal(9-field cssparser oracle; gate before speed) / css_rich_ast_preserved
json_strict_rows_admitted(51/51) / json_sonic_rs_strict_delta / json_hot_leaf_preserved
generated_md5_distinct(no byte-identical pair across generated.rs) / dirty_generated_state(clean|retired)
lock14_gate_scans_codegen / forbidden_generic_tokens_extended / named_primitive_falsifier_pass(per (a)-(d))
```

The retained SK-V15/16/17 JSON + CSS schema stays in force for the guard rows. The gate REJECTs:
any CSS row missing `css_sample_count>=50` / `css_sample_statistic==median` / `css_sample_mode==cold`
/ `css_comparator_plane==full-cssom` for the lightningcss bar / `css_typed_summary_equal==true` before
speed / `css_rich_ast_preserved==true`; any row whose corpus is not in the benched set (no phantom
`normalize`); any single-tuple broadcast (`sample_count==1` or one tuple across multiple corpus rows).
Every emitted field is consumed in the same wave.

### 0.5 — Opening Row Goalset

The CSS >SOTA bar is per-corpus, against lightningcss full-CSSOM, cold N≥50 median, corpus-in-timer.
The benched corpus set is fixed (`css_l4_corpus.rs`): `{bootstrap, tailwindcss,
material-components-web, animate}`. `normalize` is NOT in this set. The load-robust S-P1 ratios are
the per-corpus floors the regeneration must HOLD (not regress below):

| Corpus | S-P1 `track1_rich/lcss` floor (hold-not-regress) | Close-state obligation | Fallback if regressed |
|---|---:|---|---|
| bootstrap (regular) | 2.190 | grammar-driven CSS holds ≥ floor, cold N≥50, corpus-in-timer | REDRESS G2; record hot-leaf attribution; do NOT paper-close |
| tailwindcss (hardest) | 3.375 | hold ≥ floor; load-tail dispersion noted (S-P1 stddev 159.8) | honest residual recorded; not tranche-blocking provided ≥1 regular holds |
| material-components-web (full) | 1.658 (smallest margin) | hold ≥ floor; the full-corpus integration check | report median delta honestly |
| animate (regular) | 2.101 | hold ≥ floor | REDRESS G2; record residual |

JSON guard floor: all 51 JSON rows maintain A/GO strict same-plane vs sonic-rs strict, throughput
within ±1.0% of `SK-V18-open`, across every generalization wave (the G1 91.5% hot-leaf MUST-preserve).

Tranche-level success criterion: **the ONE grammar-driven generator emits JSON + CSS + Sheets,
each grammar-DERIVED, with the shared trait instantiated ≥2 ways, the phantom deleted, x86 gone,
the Lock-14 gate meaningful, AND the per-corpus CSS >SOTA ratios held ≥ the S-P1 floors (≥1 regular
corpus mandatory) AND JSON 51/51 held.** If a generalization wave proves a grammar-derived parser
CANNOT preserve the >SOTA without hand-shaping, that is a genuine §6 finding — admitted ONLY as a
named, (a)-(d)-gated, grammar-parameterized primitive (§1), recorded honestly, NEVER a silent blob.
The honest re-capture of absolute Mbps under quiet load is required before any absolute claim (H1).

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
   output-shape from the LOWERED PROGRAM, never from `target.profile`/`target.emitter`/
   `target.output_labels`/`target.profile_contract` — the §5-risk-1 relocated-seam binding).
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
paper-close surface, R-A0-3).** A fully grammar-derived parser that CANNOT preserve the >SOTA
without a hand-shaped core admits that core ONLY as a NAMED, `.bbnf`-INVOKED, grammar-DERIVED-data,
machine-gated primitive — NEVER a silent blob, NEVER a paper-close. Each primitive is admissible
ONLY under ALL FOUR:

- **(a) grammar-INVOKED-by-name** — the `.bbnf` rule names/invokes the primitive (machine-checked).
- **(b) emitted-output-VARIES-under-invoking-rule-mutation** — mutate the invoking `.bbnf` rule ⇒
  the emitted ARG byte-sets / numeric class change (the BYTE-SET/numeric-class mutation; a kernel
  that does NOT vary under its own rule's class mutation is a relabeled fixed courier, even if the
  surrounding skeleton varies — byte-equivalence alone is satisfiable by routing the SAME literal
  through the new walk, so (b) is what distinguishes derived from relabeled).
- **(c) `verbatim_blob_present == false`** — no surviving verbatim literal.
- **(d) PROFILE-PROVEN-NARROW-LEAF** — the primitive covers a SINGLE hot leaf attributable to a
  named S-P1 hot leaf (one scan/classify/emit kernel); the surrounding structural SKELETON MUST be
  walk-derived. A "primitive" spanning a rule's whole body or an unprofiled region is REJECT
  regardless of (a)-(c) (machine-checkable: primitive LOC vs the profiled hot-leaf extent).

A primitive failing ANY of the four is a relabeled hand-written blob = REJECT. The named primitives
this campaign admits: `balanced_component_scan` (CSS, the 94.1% leaf — G2/G6), the JSON
`string`/`number` leaf scanners (the 91.5% leaf — G1). **Neutrality-proof obligation (CH6):**
`balanced_component_scan` is named neutrally but exercised ONLY by CSS in this campaign — its
balanced-recognizer SHELL must be PROVEN neutral by at least one NON-CSS invocation (JSON
object/array `{}`/`[]` nesting OR Sheets `paren_expr` balancing invoking the SAME primitive) ELSE it
is demoted to an honestly CSS-scoped name (`css_balanced_component_scan`). A neutrally-named CSS-only
primitive is an overfit-in-waiting.

**The remaining standing non-negotiables:**

- **Lock 1 — one substrate.** Exactly ONE retained tape (the existing `Tape`/`ValueRef`/
  `PayloadArena`). No second tape, no eager value tree, no parser-owned facts/cursor, no sidecar
  event vector. The unified substrate is the genuine foundation (SYNTHESIS-AUDIT CLEAN list, KEEP).
- **Lock 14 — grammar-neutral.** No JSON/CSS/Sheets policy in generic crates. All routing is DERIVED
  from the `.bbnf`/`BackendShape` shape, never hand-curated; relocating per-rule branching into
  projection DATA or flag form is the Lock-14 re-entry seam and is FORBIDDEN (the §5-risk-1
  relocated seam). Every generic-crate edit carries a non-JSON proof (§2.1).
- **preserve-rich-ast.** The typed CSSOM and JSON rich tree are NEVER flattened for speed. CSS
  `track1_rich` stays lazy `ValueRef`-view projection over the tape (lazy not eager — re-derived
  from spans, nothing eager to the arena); JSON's `get`/`pairs`/typed-`JsonValue`/recursive-visitor
  navigation is preserved by construction (`json_rich_navigation_preserved==true`).
- **dav1d discipline on every primitive.** Scalar reference FIRST + checkasm differential parity
  (the `neon_significant_skip_matches_scalar` guard over the REAL 71KB-495KB corpora) BEFORE wiring;
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

## Section 2.1 — Generality And Lock-14 Gate (every wave carries this)

Every wave has this exit gate, with extra checks when generic crates are edited:

- **Public API scan.** No new public JSON/CSS/Sheets-named API in generic crates.
- **Grammar-branch scan.** No generic branch selects behavior by grammar name, grammar family,
  corpus name, rule role, object/array role, field name, or layout role.
  `generator_grammar_branch_count==0 ∧ generator_grammar_type_count==0` at and after G3.
- **Relocated-seam scan (the STRUCTURAL co-gate the arm-grep cannot see).** `runtime_target_rows_
  collapsed==true` via the R16 recipe — `RuntimeTarget: PartialEq` full-row derive recursing into
  BOTH nested structs `frontend_requirements` (field #11) AND `output_labels` (field #12). A
  hand-rolled prose-field comparison risks a shallow-compare false-green of EITHER nested struct and
  is FORBIDDEN — the full-row derive is the ONLY admitted mechanism (+1 line; both nested structs
  already derive `PartialEq,Eq`). `emit_shape_source==lowered_program` (the emitter body reads NO
  `target.*` field) is the companion falsifier: grep the `render(program)` body for any read of
  `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract` == 0.
- **Primitive/table scan.** No generic primitive, SIMD kernel, or classifier embeds CSS/JSON/Sheets
  structural policy unless it is generated byte-set data plus opaque class ordinals with a scalar
  reference and a same-wave consumer (the §1 (a)-(d) gate). The CSS `balanced_component_scan` and
  the NEON eq-set kernel take grammar-DERIVED byte-set ARGS as caller data; the kernel does not
  author grammar policy. The balanced-recognizer SHELL carries the CH6 neutrality-proof obligation.
- **Role/fact boundary.** Generic code may store/search generated structural class ordinals or
  opaque fact ids, but rule-role/recovery/layout/record-boundary meaning lives ONLY inside generated
  grammar modules keyed by parser state + class/byte.
- **Template/provider boundary.** CSS/Sheets-specific templates/providers remain per-grammar
  surfaces; generic codegen consumes grammar-derived facts, not hand-coded policy under neutral
  names. The import-closure relaxation for a single-file grammar (Sheets) is a
  frontend-requirements DATA change (present-iff-grammar-has-imports, derived from facts), NOT a
  `match grammar` arm.
- **Witness-emission scan-root coupling (P4).** The `JsonEventGrammar`/`SheetsEventGrammar`
  witnesses live in `runtime/`. IF the un-forked generator EMITS a grammar-named `EventGrammar`
  literal into the generated runtime, the `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS`
  must catch it (`EventGrammar`/`*EventGrammar` in the extension) — Sheets is the FIRST grammar to
  exercise the witness-emission coupling.
- **Non-JSON proof (the SK-V18 first-mover triple).** The projection-generality riders exercised are
  JSON + CSS + Sheets (`projection_generality_exercise ∈ {json, css_l4, google_sheets}`); the NEON
  SIMD non-JSON exercise is `css_l4` (`simd_non_json_exercise=css_l4`; S-P1 has NO JSON G5 hot leaf).
  Sheets is the negative-control that proves grammar-DERIVED emission (its precedence tower cannot be
  a relabeled JSON/CSS courier). A wave that lets CSS/JSON/Sheets policy into a generic crate fails
  the generality lens (the CH7 Overfit-Prune lens, binding on every wave).

Allowed grammar-specific surfaces: grammar inputs (`.bbnf`), generated parser output, per-grammar
providers/templates, tests, host/API schema facts. The CH7 lens is binding on every wave: every new
code is grammar-derived (template + grammar metadata + emission command), never hand-written under
`// @generated`; every admit via a real source change, strict same-plane, per-iter oracle; every
generated output passes round-trip; no scaffold-only landing counts as an admit; the §6 escape
qualifies ONLY under (a)-(d).

---

**Close condition restated:** SK-V18 closes when ONE grammar-driven generator emits JSON + CSS +
Sheets from `.bbnf` (each grammar-DERIVED, `verbatim_blob_present==false`,
`generator_grammar_count==3`), through ONE un-forked emitter dispatched on the lowered
`BackendShape` (`emitter_fork_present==false`, `emit_shape_source==lowered_program`,
`runtime_target_rows_collapsed==true`), with a shared value-API trait both JSON and CSS (and Sheets)
instantiate ≥2 ways without LCD-flattening (`json_rich_navigation_preserved==true`), the phantom
`<G>` deleted (`phantom_generic_resolved==deleted`), the >SOTA preserved honestly (CSS
`track1_rich/lcss >= the S-P1 floor` cold corpus-in-timer; JSON 51/51 strict-vs-sonic-rs;
`materialization_framing==lazy-rich-vs-eager-cssom` disclosed), x86 gone (aarch64-only), the Lock-14
gate meaningful (`lock14_gate_scans_codegen==true`), Sheets proving the generalization is real
(`sheets_grammar_shape==pratt-operator`, no `_RS` blob, the binding fallback honored), the NEON
reaching admission (`acceleration_at_admission==admission`), regen clean, and net ≈ −10800 LOC.

**Next move:** ready-for-SPEC-assembly (PE Sections 0/1/2.1 fold with PA-PD per-wave sections into
`restart/skinny/tranches/sk-v18/SPEC.md`); thereafter ready-for-S-P3-CHALLENGE (the 7-lens 3Z
harden) and, on close, ready-for-wave-implementation (W-PRUNE dispatch-eligible first).
