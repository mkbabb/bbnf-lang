# SK-V18 S-P2 — SYNTHESIS-RESEARCH (the binding candidate shortlist for S-P3)

Consolidates the six R-A..R-F class digests (`rA-emitter-unify.md`, `rB-css-lowering.md`,
`rC-json-projection.md`, `rD-value-api-trait.md`, `rE-sheets-proof.md`, `rF-neutral-scan.md`)
into ONE research synthesis: the recommended candidate per class, the cross-class couplings,
the binding PRUNE→G1..G6→PROVE→H1 sequencing with each entry-gate, the section-6 honest-finding
surfaces, and the residual research RISKS S-P3 plans around. RESEARCH only — no code, no cargo.
Bound by the six S-P0 addenda (`SYNTHESIS-AUDIT-OVERFIT.md §1`, `a1-six-addenda-lens-registry.md`)
and the S-P1 profile ground-truth (`SYNTHESIS-PROFILE.md §2/§3`). Host: aarch64 / Apple M5 Max ONLY.

Every load-bearing claim re-grounded on disk this pass at the cited `path:line`:
`grammar_provider.rs:40-42` (`RuntimeEmitterKind{CompiledLowering,RequestFacts}`),
`:32-37` (`RuntimeProfileContract` carries `emitter` + nested `frontend_requirements` (its field #3)
+ `output_labels` (its field #4) — these are field #11/#12 in the SEPARATE `RuntimeTarget` struct,
`regen.rs:17-18`; the two ordinal systems must not be conflated, per the a1 §3 R16 numbering hazard),
`:46-57` (`RuntimeFrontendRequirements{import_closure,…}`, the 10-bool struct);
`runtime_generator.rs:16` (the live fork dispatch) + `grammar_provider.rs:110-111` (the JSON-only `first_unsupported` gate),
`:37` (JSON `json_sink_direct::render`), `:91` (CSS `normalize(CSS_GENERATED_RS)` courier);
`lower/mod.rs:18-25` (the grammar-NEUTRAL 5-shape `select_lowering` over `BackendShape`,
`{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`);
`lower/sink_only.rs:68-96` (the rich `SinkOnlyExpr` IR `render()` discards);
`tape/mod.rs:175` (`ValueRef<…K=AnyKind, G: EventGrammar = AnyGrammar>` + `_grammar: PhantomData`),
`:227-228` (latent `DocumentView`/`type Root`, zero generic consumers);
`xtask/regen.rs:5-18` (`RuntimeTarget` derives only `Clone,Copy,Debug`; carries `emitter`,
`frontend_requirements`, `output_labels` — the R16 +1-line `PartialEq` target). The
`_proof_compiles` G-instantiation census excluding `_tests.rs` returns EMPTY — confirming the
`<G>` axis has zero non-test production animator (R-D DELETE is the grounded default; the
`…_witness.rs` `EventGrammar` impls in `grammars/json/` + `grammars/sheets_witness/` exist as
DEFINITIONS but are consumed ONLY by the `_tests.rs` `_proof_compiles` proof — defined, never
animated, so the DELETE-default cannot be rebutted as ignoring production `EventGrammar` impls).

---

## §1 — Recommended candidate per class (R-A..R-F), one-line justification each

| class | wave | RECOMMENDED candidate | one-line justification |
|---|---|---|---|
| **R-A** un-fork emitter | G3 | **A — DELETE `RuntimeEmitterKind`, dispatch on the lowered `BackendShape`** | reuses the already-Lock-14-clean grammar-NEUTRAL 5-shape discriminator (`lower/mod.rs:18`), keeps the decision grammar-DERIVED (cost-model over rule shapes, not a config field), and makes `runtime_target_rows_collapsed` satisfiable by removing `emitter` from `RuntimeTarget` entirely. |
| **R-B** CSS lowering | G2 | **B wrapping A — balanced-scan as a grammar-parameterized NAMED PRIMITIVE; drivers + lazy rich projection as grammar-fact-keyed emit blocks** | the 94.1% delimiter scan is NOT line-derivable from a generic IR walk (a tree-walk descent = lightningcss's own architecture, regresses >SOTA), but it IS a narrow namable algorithm — name it, parameterize by grammar-derived byte sets, INVOKE it from the emitted scan, and it doubles as the G6 NEON-retarget call site (one seam for G2+G6). |
| **R-C** JSON projection | G1 | **C1 — `SinkOnlyExpr` AST-walk emitter (the `tape_plan.rs`/`json_typed_direct.rs` house pattern), borrowing C2's named-primitive discipline for the string/number leaf scanners** | the ONLY candidate that fully discharges addendum 1 (body grammar-DERIVED, not a courier swap) while structurally preserving the 91.5% hot leaf; the walk emits the same `match byte` + `sink.*` call sites the profile rewards, hot inner kernels stay byte-stable as (a)-(c)-gated grammar-invoked primitives. |
| **R-D** shared value-API trait | G4 | **A — thin `Cursor` micro-trait over the surviving `ValueRef<K>` + extend the existing `DocumentView` to CSS/Sheets; DELETE the phantom `<G>`** | the unique candidate giving ≥2 impls that CANNOT collapse to the lesser — the trait shares the laziness/cursor contract, NEVER navigation, so JSON's rich tree (`get`/`pairs`/typed `JsonValue`/recursive visitor) is preserved by construction (`json_rich_navigation_preserved == true`); resolves the phantom by DELETE, no second substrate, laziness intact. |
| **R-E** Sheets proof | PROVE | **R-E-2 — precedence-tower CORE (7-level left-assoc tower + cyclic `paren_expr→expression` + leaves), DEFERRING the `cell_ref`/`range`/`LET`/`LAMBDA` aggregates the grammar ITSELF leaves as raw `-> input : Span`** | the minimal HONEST proof — the **precedence tower is the SOLE Sheets-distinctive construct** JSON+CSS structurally lack (`sheets_grammar_shape == pratt-operator`, non-hollow; the `Nu8`-tagged-alt family is NOT novel — CSS L4 uses `-> Nu8u8` **295×** across its import closure vs Sheets' **21×**, so it is a SHARED construct the generator must already handle to emit CSS at all, demoted from the litmus), defers only what the grammar's own TODOs defer (AU.6.7, `google-sheets.bbnf:62,73-75`), so no contrivance either direction. |
| **R-F** neutral scan | G5/G6 | **A — "inner-skip vectorize": retarget the existing checkasm-gated `bbnf-simd::find_ascii_set_member64`/`byte_class_from_eq_set_64` onto the scalar recursive shell of `find_component_delim`; vectorize only the inert-run skip, set is caller data** | the kernel ALREADY EXISTS as alphabet-data + is checkasm-gated, so R-F is a retarget (salvage the set-split from the dead `find_css_significant:180-204`) + a generated call-site swap landed WITH its consumer; directly hits the measured 79.5%/94.1% CSS hot leaf; JSON neutrality is honest (same eq-set kernel JSON's `scan_structurals` already rides), NOT fabricated (JSON product path is scan-free). |

Candidate B (R-A `ProjectionSpec` value) is ABSORBED into A's per-shape renderers (each
`BackendShape` renderer declares its own roster, retiring `COMPILED_RUNTIME_FILES`/
`REQUEST_FACTS_RUNTIME_FILES` as per-arm constants); B is REJECTED as primary because a
`ProjectionSpec` IS the relocated-seam shape unless every field is lowering-derived. Candidate C
(R-A "no discriminator at all", one canonical fold) is the correct SK-V19 end-state along A's
per-shape renderer seam — deferred on >SOTA-preservation burden. Candidate C (R-B/R-C full
grammar-IR tree-walk) is REJECTED outright: it inflates the flat scan into the combinator-shaped
descent the scan was built to avoid (the genuine §4 tension). Candidate B (R-D tree-shaped
`Value` stack) is REJECTED: HIGH LCD-flatten hazard or dead degenerate CSS impls for an absent
generic consumer. Candidate C (R-D `DocumentView`+stream-only) under-delivers (shares the stream
not value navigation). R-E-3 (flattened precedence) is REJECTED as a "third-JSON" hollow litmus.
R-F Candidate B (balanced-consume bitmap) is the documented upgrade path (its mask vocabulary IS
the JSON scan vocabulary, the true JSON/CSS convergence point — defer until measured); R-F
Candidate C (table-classifier unify) is REJECTED on lo6-collision hazard + JSON↔CSS coupling.

---

## §2 — Cross-class couplings (the dependency lattice S-P3 wires)

The six classes are NOT independent — they form a coupling lattice the standing order serializes:

1. **R-A (un-fork, G3) depends on BOTH R-B (G2) AND R-C (G1) closing.** A dispatches on the
   lowered `BackendShape`; that requires CSS to actually LOWER (R-B must produce a real lowered
   CSS scan IR — today CSS never reaches `lower_to_rust`, it is a const at `:91`) AND JSON to be a
   `SinkOnlyExpr`-walking projector (R-C, not the fixed-literal `render()`). Without R-B, A has no
   non-const CSS `BackendShape` renderer input; without R-C, A inherits a fixed-literal JSON arm.
   So **G3 entry-gates on G1 ∧ G2**.

2. **R-B (G2) depends on R-C (G1) AND inherits R-C's discipline.** G2 reuses G1's projecting-
   renderer pattern (the facts-bus + emit-block discipline that retires the fixed literals); the
   JSON "analog" R-B leans on is itself only partially derived today, so **G1 must fully
   projectionize JSON FIRST and G2 inherits that discipline** (`a2`/`rB §6`). G2 also dual-entry-
   gates on **P3** (collapse the 7 byte-identical css_l4 replicas + `RuntimeTarget` row-collapse)
   — else G2 re-derives the SAME scan into 7 byte-identical files and re-creates the replica
   overfit (addendum 2).

3. **R-F (G6) depends on P3 collapse AND R-B's named primitive AND R-A's un-fork.** R-B's
   recommended candidate lands the balanced-scan as a grammar-parameterized named primitive that
   IS the G6 NEON-retarget call site — co-locating G2 and G6 in ONE seam (no orphan kernel, no
   per-grammar re-emit). The retargeted CALL SITE must land into the **P3-collapsed SINGLE CSS
   scan** (the kernel stays singular in `bbnf-simd`; the generated call site must be singular too,
   which only P3 guarantees) and must be emitted by the **G3 un-forked emitter** (else it re-forks
   the shape G3 un-forks). So **G6 entry-gates transitively on P1∧P3∧R-B∧G3**.

4. **R-D (G4) resolves the phantom AND depends on R-A's un-fork closing.** G4 entry-gates on
   G1∧G2∧G3 closed — the un-forked G3 emitter must emit BOTH JSON and CSS value-API surfaces
   THROUGH one path before G4 can define the trait both instantiate. The `<G>` DELETE is
   INDEPENDENT of the trait (the two are separable per `a1 §L4`) and may land FIRST within G4 to
   expose the clean `ValueRef<K>` the trait targets. R-D's trait must NOT LCD-flatten JSON's rich
   tree navigation vs CSS's flat sweep — the trait shares only the cursor/laziness contract.

5. **R-E (PROVE) consumes R-A (G3) AND R-D (G4).** Sheets emits THROUGH the un-forked G3 generator
   (its precedence tower is structurally unlike both JSON and CSS, so it CANNOT be
   a relabeled courier — the true test of grammar-DERIVED emission; the `Nu8`-tagged-alt family is
   a SHARED construct, not part of the litmus — see §1 R-E); the Sheets value type
   instantiates the R-D shared trait (the phantom-`<G>` resolution made concrete by a third impl).
   **PROVE entry-gates on G3 ∧ G4** (G4 is a DIRECT conjunct — the Sheets value type instantiates
   the R-D trait), transitively on G1∧P3.

6. **P4 (Lock-14 gate) is the cross-cutting predecessor of every emitter wave.** It MUST land
   BEFORE G2/G3 so the un-forked emitter is neutrality-scanned AS it is authored; its
   `FORBIDDEN_GENERIC_TOKENS` extension (`CSS_`/`_RS`/`EventGrammar`/`*EventGrammar`) catches the
   JSON `_RS` couriers G1 retires, the CSS courier G2 retires, and any Sheets `EventGrammar`
   literal G3 would emit (Sheets is the FIRST grammar to exercise the witness-emission coupling).

7. **R16 (the row-collapse recipe) is the SINGLE structural co-gate threading R-A/R-B/R-E.** A
   `RuntimeTarget: PartialEq` full-row derive (one line; today `Clone,Copy,Debug` only,
   `regen.rs:5`) is the preferred mechanism — it recurses into BOTH nested structs
   (`frontend_requirements` = `RuntimeTarget` field #11 at `regen.rs:17`, AND `output_labels` =
   `RuntimeTarget` field #12 at `regen.rs:18`) automatically and cannot be coupled to a
   hand-rolled field list. (The #11/#12 ordinals are `RuntimeTarget`'s, `regen.rs:6-19`; the same
   two structs recur as `RuntimeProfileContract` fields #3/#4 at `grammar_provider.rs:35-36` — the
   `PartialEq,Eq` derives the recipe leans on are at `grammar_provider.rs:45`/`:91`. Do not
   conflate the ordinal systems, per the a1 §3 R16 numbering hazard.) It is the ONLY check that catches the relocated seam
   (a per-grammar branch moved into a neutral data table) that the arm-census grep is
   syntactically incapable of seeing. R-A's un-fork, R-B's P3 collapse, and R-E's distinct
   `grammar_name="google_sheets"` row all dual-prove through this one derive.

---

## §3 — The binding sequencing (PRUNE → G1..G6 → PROVE → H1), with each entry-gate

The standing order from `SYNTHESIS-AUDIT-OVERFIT.md §5` is binding; a wave failing its exit gate
BLOCKS every downstream wave that entry-gates on it. No wave dispatches over a REDRESSed predecessor.

```
PRUNE  P1 (x86 crate-wide delete) · P2 (warm micro-fixture CSS bench delete) ·
       P3 (collapse 7 css_l4 replicas + RuntimeTarget row-collapse) ·
       P4 (Lock-14 green-by-exclusion fix — MUST land BEFORE G2/G3) ·
       P5 (metalang leak purge)
  └─ G1  JSON projection (R-C C1)        entry: P-cluster closed (P4 live)
       └─ G2  CSS lowering (R-B B⊃A)      entry: G1 ∧ P3 closed ∧ P4 live
             └─ G3  un-fork emitter (R-A A) entry: G1 ∧ G2 closed ∧ P4 live ∧ P3 (row-collapse)
                   └─ G4  shared trait + phantom (R-D A)  entry: G1 ∧ G2 ∧ G3 closed
                         ├─ G5/G6  neutral scan (R-F A)   entry: P1 ∧ P3 ∧ G3 ∧ S-P1 profile (94.1% leaf)
                         └─ PROVE  Sheets (R-E-2)         entry: G3 ∧ G4 closed (transit. G1∧P3) — PARALLEL to G5/G6 (Sheets does not use the CSS NEON)
                               └─ H1  CSS framing honesty + regen --check clean   entry: G5/G6 ∧ PROVE closed
```

Per-wave entry-gate (the binding predicate that must be GREEN before dispatch):

- **P1**: no entry-gate (pure deletion, zero generalization risk). Falsifier: `find …/src/x86_64
  …/ext/x86 -type f = 0` ∧ crate-wide aarch64-neutral grep ∧ `cargo build`/`cargo test --no-run`
  clean. Exit: x86 surface gone, single-arch kernel surface for R-F retarget.
- **P2**: no entry-gate. Falsifier: `grep measure_mbps|lightningcss_facts = 0`; `css_canon_bench`
  KEPT. Exit: only the cold/real-corpus harness remains (R-F/H1 measure honestly).
- **P3**: no entry-gate. Falsifier: md5-distinct (no byte-identical pair across `generated.rs`) ∧
  the structural `runtime_target_rows_collapsed` co-gate (R16, `RuntimeTarget: PartialEq` full-row).
  Exit: ONE CSS config; the singular scan G2 derives + G6 wires.
- **P4** (the cross-cutting predecessor): no entry-gate, but MUST land BEFORE G2/G3. Falsifier:
  re-inject a `JsonSink` token → gate turns RED (proves coverage), revert; `lock14_gate_scans_codegen
  == true`; `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}`. Exit: the new
  emitter is neutrality-scanned as authored.
- **P5**: no entry-gate. Falsifier: `grep -c parse_w11_1_number = 0`; no `w[0-9]+`/corpus/`sk_v`
  tag in shipped runtime; regen clean.
- **G1** (R-C): entry = P-cluster closed (P4 live). Exit = byte-equivalence of regenerated
  `generated.rs` against the `json_templates/` oracle + shipped file BEFORE oracle deletion
  (`EmittedSource::check_dir`, the `emission_is_deterministic`/`direct_parser_is_authored_from_sink_only_lowering`
  tests) + the `.bbnf`-mutation falsifier (drop `bool`, the `b't'`/`b'f'` arms vanish) +
  `parse_object_value_at_direct` re-emitted with identical inline cfg + `sink.object_*` call sites
  (91.5% MUST-preserve). `verbatim_blob_present == false` (both the SinkOnly literals AND the
  parse-only `_RS` courier folded). ±5% line delta = SOFT tripwire only.
- **G2** (R-B): entry = G1 ∧ P3 closed ∧ P4 live (DUAL gate — a P3 failure blocks G2 independent of
  G1). Exit = CSS `generated.rs` grammar-DERIVED (`verbatim_blob_present == false`, `CSS_GENERATED_RS`
  grep == 0); the named `balanced_component_scan` primitive passes the per-primitive (a)-(c)
  mutate-falsifier (mutate the invoking `.bbnf` rule → emitted ARG byte sets change); 9-field
  cssparser oracle CORRECTNESS parity held; AND an EXPLICIT >SOTA-regression gate distinct from
  parity — `track1_rich/lightningcss >= the S-P1 ratio` on `css_canon_bench` (cold, corpus-in-timer,
  absolute figures inheriting §5-risk-7's QUIET-recapture caveat). G2 re-derives the 94.1% scan, so
  oracle parity alone (correct output) does NOT prove throughput preservation — the bench
  re-measurement is the binding regression falsifier.
- **G3** (R-A): entry = G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse. Exit = `emitter_fork_present
  == false` (`RuntimeEmitterKind`/`CompiledLowering`/`RequestFacts` grep == 0); `generator_grammar_branch_count
  == 0`; `generator_grammar_type_count == 0`; `runtime_target_rows_collapsed == true`;
  **`emit_shape_source == lowered_program`** (the un-forked `render(program)` body reads its
  output-shape ONLY from `program.policy_summary.backend_shape` (`sink_only.rs:48`), NEVER from a
  `RuntimeTarget` field — falsifier: grep the `render(program)` body for any read of
  `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract` == 0; without
  this fourth conjunct the §5-risk-1 relocated seam riding the neutral per-profile columns passes
  all three of the above under a green gate); one `render(program)` path serves JSON+CSS dispatched
  on `BackendShape`, not a grammar tag.
- **G4** (R-D): entry = G1 ∧ G2 ∧ G3 closed. Exit = `phantom_generic_resolved == deleted` ∧
  `json_rich_navigation_preserved == true`; ≥2 real impls of the `Cursor`/`DocumentView` seam
  (JSON `ValueRef<K>`, CSS `CssNode`) that cannot collapse to the lesser.
- **G5/G6** (R-F): entry = P1 ∧ P3 ∧ G3 closed ∧ the S-P1 94.1% hot-leaf measurement (no orphan
  kernel). Exit = `acceleration_at_admission == admission` proven by the generated-`generated.rs`
  caller census (`rg runtime_simd::find_…  …/grammars/*/generated.rs` non-empty), NOT a `#[cfg(test)]`
  caller; checkasm differential + the `neon_significant_skip_matches_scalar` guard retargeted to
  the recursive shell over the REAL 71KB-495KB corpora; G5 = neutralize/retire the zero-sampled
  `json/scan.rs` (cheap, no JSON classifier authored). **Timed-plane binding (addendum 5):** the
  checkasm differential is a CORRECTNESS gate only; G6 may report only its PASS/FAIL pre-H1, and
  any Mbps/speedup FIGURE it emits MUST come from the corpus-in-timer symmetric harness (the
  P2-survivor cold/real-corpus `css_canon_bench` path, same plane both sides) and inherits §5-risk-7's
  QUIET-recapture caveat — deferring any speedup CLAIM to the H1 symmetric timer, so addendum 5 is
  not enforced one wave too late.
- **PROVE** (R-E-2): entry = G3 closed (transitively G1 ∧ P3 ∧ G4). Exit = Sheets `generated.rs`
  md5-distinct from JSON ∧ CSS; no `const.*_RS.*r#` Sheets blob; `sheets_grammar_shape ==
  pratt-operator`; distinct `grammar_name="google_sheets"` row (`generator_grammar_count == 3`);
  the Sheets value type instantiates the G4 trait; `w5a_sheets` flips from "fails closed: missing
  import closure" to "emits a working parser" via the import-closure-derived-from-facts relaxation
  (a frontend-requirements data change, NOT a `match grammar` arm).
- **H1**: entry = PROVE closed. Exit = CSS `materialization_framing` disclosed
  (`lazy-rich-vs-eager-cssom` — the honest S-P1 framing); `corpus_in_timer == true`; regen --check clean.

Hard caps (standing `[dispatch-hard-cap]`): research 20 / plan 15 / redress 30 min, "at 0.9N
commit, at N halt"; the Sheets/NEON cluster (PROVE/G6) is MED-HIGH and may carry a documented
larger cap.

---

## §4 — Section-6 findings (grammar-derived parsers that CANNOT preserve >SOTA without a named, gated, grammar-parameterized primitive)

Two classes surface a genuine §6 honest-finding — a place where a fully grammar-derived parser
CANNOT preserve the >SOTA without a hand-shaped core, which the contract admits ONLY as a NAMED,
`.bbnf`-INVOKED, grammar-DERIVED-data, machine-(a)-(b)-(c)-gated primitive (never a silent blob,
never a paper-close). These are the load-bearing §6 surfaces S-P3 must plan as gated primitives:

- **R-B CSS balanced delimiter scan (the PRIMARY §6 finding).** The 94.1% hot leaf
  (`find_component_delim` + `consume_balanced_at`) is a flat balanced-delimiter recognizer whose
  delimiter alphabet (`{}:;`) and structural-byte dispatch (`' " / ( [ {`) are EMERGENT from the
  rule shapes, modeled by NO `SinkOnlyExpr` node. A naive grammar-walk lowering produces the
  combinator-shaped recursive descent (lightningcss's own architecture) that categorically
  regresses >SOTA. The honest path: the balanced scan lands as a grammar-parameterized
  `balanced_component_scan` named primitive, grammar-INVOKED, taking grammar-DERIVED byte-set ARGS,
  with a per-primitive mutate-falsifier (mutate the invoking rule → ARGS change) + a scalar/checkasm
  reference. This primitive is ALSO the G6 NEON-retarget call site — one seam for G2+G6.
  NEUTRALITY-PROOF obligation (CH6): `balanced_component_scan` is named neutrally but is exercised
  ONLY by CSS in this campaign. Its inner alphabet-scan sub-kernel (the `bbnf-simd` eq-set member
  scan) is genuinely neutral (caller-supplied byte set), but the balanced-recognizer SHELL must be
  PROVEN neutral by at least one NON-CSS invocation in this campaign — the JSON object/array balanced
  `{}`/`[]` nesting OR the Sheets `paren_expr` balancing must invoke the SAME primitive — ELSE it is
  demoted to an honestly CSS-scoped name (`css_balanced_component_scan`), not a false neutral. A
  neutrally-named CSS-only primitive is an overfit-in-waiting.
- **R-C JSON string/number leaf scanners (the SECONDARY §6 finding).** The 91.5% hot leaf's inner
  kernels carry micro-opts (`b'-' | b'0'..=b'9'` array fast-path, `match_tiny_plain_string_direct`)
  the AST-walk must reproduce byte-exact. These stay byte-stable as named primitives invoked by the
  `.bbnf` `string`/`number` rules, each carrying its own (a)-(c) machine falsifier — the structural
  SKELETON is walk-derived, only the proven-hot leaf kernels are gated primitives. The (b)
  falsifier for each leaf kernel is specifically the BYTE-SET / numeric-class mutation: widen the
  `number` rule's digit class in the `.bbnf` → the `b'0'..=b'9'` literal in the emitted kernel
  widens; a kernel that does NOT vary under its own rule's class mutation is a relabeled fixed
  courier even though the surrounding skeleton varies (the byte-equivalence gate alone is
  satisfiable by routing the SAME literal through the new walk, so (b) is what distinguishes a
  derived leaf from a relabeled one).
- **R-E Sheets precedence tower (a §6 CANDIDATE, not yet realized).** The 7-level left-assoc tower
  is right-iterated EBNF (`A = B (op B)*`) lowering to the EXISTING `SinkOnlyExpr` vocabulary
  (`Seq`+`RepeatLoop`+`Alt{Dispatch}`+`CallRule`) — so it needs NO new IR primitive; the stress is
  on G3's GENERALITY (does the body come from the grammar?). IF G3 cannot render recursive
  `CallRule`/`RepeatLoop` chains from grammar structure, the tower breaks first = a §6 finding
  surfaces (a named, `.bbnf`-invoked, parameterized precedence primitive with a scalar/checkasm
  reference). This is the PROVE make-or-break.

The §6 escape is "the single largest paper-close surface in the contract" (R-A0-3) — every
primitive above is admissible ONLY under the machine-checked (a) grammar-INVOKED-by-name + (b)
emitted-output-VARIES-under-invoking-rule-mutation + (c) `verbatim_blob_present == false` + (d)
**PROFILE-PROVEN-NARROW-LEAF**: the primitive covers a SINGLE hot leaf attributable to a named
S-P1-profile hot leaf (a single scan / classify / emit kernel), and the surrounding structural
SKELETON MUST be walk-derived — a "primitive" spanning a rule's whole body or an unprofiled region is
a REJECT regardless of (a)-(b)-(c) (machine-checkable as primitive LOC vs the profiled hot-leaf
extent). (a)-(b)-(c) prove the primitive is grammar-coupled; (d) bounds its SIZE so the escape cannot
admit an arbitrarily large relabeled blob that merely varies under mutation. A primitive failing any
of the four is a relabeled hand-written blob = REJECT.

---

## §5 — Residual research RISKS S-P3 must plan around

1. **The relocated seam (R-A, the single sharpest risk).** Un-forking the VISIBLE
   `RuntimeEmitterKind` enum while leaving a per-grammar branch in a neutral-identifier data table
   (the `RuntimeTarget` strategy field, a `profile`-selected `ProjectionSpec`, or the per-profile
   columns). The arm-census grep is syntactically incapable of seeing it; ONLY the structural
   `runtime_target_rows_collapsed` co-gate catches it. MITIGATION: the R16 `RuntimeTarget:
   PartialEq` full-row derive + the binding rule that the un-forked emitter reads output-shape from
   the LOWERED PROGRAM, never from `target.profile`/`target.emitter`/`target.output_labels`.
   md5-distinct is necessary-NOT-sufficient.

2. **The structural-alphabet-derivation gap (R-B, the G2 make-or-break).** The CSS scan's delimiter
   set + structural-byte dispatch are emergent, not literals in any one `BackendRule`; an incomplete
   derivation either diverges from the 9-field cssparser oracle (parity REJECT) or gets hand-patched
   back into a verbatim blob (L1 REJECT). Candidate B narrows this to deriving only the ARG byte
   sets, but the (a)-(b)-(c) mutate-falsifier MUST prove those args vary with the `.bbnf` or it is a
   relabel. This is the single most likely place G2 REDRESSes.

3. **Hot-leaf byte-equivalence under the AST-walk (R-C).** The diff-control gate is byte-equivalence;
   the hand bodies carry micro-opts (the digit fast-path split, the tiny-string inline, the 3
   sink-prefix dispatch variants). If the walk cannot reproduce them byte-for-byte the gate fails
   (REDRESS), OR the team is tempted to relabel a fixed fragment as a "named primitive" (the
   paper-close). The escape MUST be machine-(a)-(b)-(c)-gated per fragment; do NOT LCD-unify the
   value/object/array dispatch triple (regresses the monomorphized-sink leaf).

4. **The "trait too thin to be a generalization" critique (R-D).** Candidate A's `Cursor` micro-trait
   may be challenged as under-delivering. MITIGATION: bind to the two falsifiable telemetry columns
   (`phantom_generic_resolved == deleted` ∧ `json_rich_navigation_preserved == true`); the rebuttal
   is that ANY trait wide enough to satisfy the critic is wide enough to LCD-flatten JSON (the
   REJECT) — the honest generalization is the cursor/laziness contract, not a forced common value shape.

5. **The un-forked G3 emitter does not exist yet (R-E, the transitive risk).** BOTH current emit
   paths are grammar-specialized couriers; Sheets is the FIRST grammar whose body cannot be a
   relabeled JSON/CSS courier. If G3 cannot render recursive `CallRule`/`RepeatLoop` chains from
   grammar structure, the precedence tower breaks first. Plus the import-closure requirement must
   relax for a single-file grammar (present-iff-grammar-has-imports, derived from facts) WITHOUT
   becoming a `match grammar` arm. The fallback is binding: if Sheets cannot emit via the generator
   ONLY, generalization is NOT real — surface honestly, do NOT stub-prove, do NOT hand-write a
   `_GENERATED_RS` Sheets block.

6. **Parity-under-retarget (R-F).** The ≤13-byte significant set exceeds the 8-byte eq-set cap
   (needs the two-fan OR-reduce salvaged from the dead kernel); the vector skip must stop AT
   `([{'"/` so the scalar shell still handles recursion/strings; error positions must come from the
   shell. Gate = checkasm differential + the `neon_significant_skip_matches_scalar` guard retargeted
   to the recursive shell over the REAL corpora. Secondary: realized speedup is bounded by inert-run
   length — a MEASUREMENT to confirm post-wire, not a correctness risk.

7. **Load-depressed absolute Mbps (cross-cutting, from S-P1).** The S-P1 capture ran under
   concurrent-session load (loadavg 4.35); absolute Mbps is DIRECTIONAL and NOT re-locked. The
   load-robust outputs (same-run >SOTA ratios + relative hot-leaf rank) are the ground-truth; a
   QUIET re-capture is required before any absolute claim in H1.

---

Next move: ready-for-S-P3.
