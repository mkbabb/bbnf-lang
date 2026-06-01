# SK-V18 SPEC — PLAN PC: G3 (un-fork emitter) + G4 (shared trait + phantom) Wave Specs

Date: 2026-06-01. S-P3 synthesis-PLAN pass, packet PC. This is NOT an implementation dispatch;
it is the executable wave manifest for the two MIDDLE GENERALIZE waves of SK-V18 (the
backtrack-into-one-generator cycle) — the un-fork that makes the generator singular, and the
shared value-API trait + phantom resolution that makes the runtime substrate singular. It folds
the S-P2 converged candidate shortlist (`research/p2/SYNTHESIS-RESEARCH.md` §1/§2/§3, rA §4,
rD §4), the S-P0 audit-overfit addenda + PRUNE-list + R16 recipe
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§4/§5/§6), and the S-P1 profile ground-truth
(`research/p1/SYNTHESIS-PROFILE.md` §2/§3 — G6=WIRE context only; PC does not touch the NEON).
The SK-V17 SPEC (`tranches/sk-v17/SPEC.md`) is the structure template. The canonical telemetry
column registry is authored by packet PE (`pe-gate-telemetry-close.md` §gate-consumer); PC EMITS
into that one registry, it does NOT fork parallel `g3_*`/`g4_*` columns. Host: aarch64 / Apple M5
Max ONLY. No cargo run this pass.

Authority (binding inputs, re-grounded on disk this pass):
- `research/p2/SYNTHESIS-RESEARCH.md` §1 (R-A A → G3; R-D A → G4), §2 (the coupling lattice —
  couplings 1/4/5/7), §3 (the per-wave entry-gate predicates, lines 176-188), §4 (the §6
  precedence-tower CANDIDATE that G3's generality is the make-or-break for), §5 (residual
  risks 1/4/5).
- `research/p2/rA-emitter-unify.md` (the G3 surface, the DELETE-`RuntimeEmitterKind` candidate A,
  the per-`BackendShape` renderer seam, the relocated-seam risk).
- `research/p2/rD-value-api-trait.md` (the G4 surface, the thin `Cursor` micro-trait candidate A,
  the phantom `<G>` DELETE, the LCD-flatten REJECT).
- `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1 (addenda 3 single-emitter + 4 phantom-generic),
  §2 (R3/R5/R6/R16 residuals), §4 (P3/P4 PRUNE), §5 (sequencing + the R16 full-row recipe
  binding to S-P3), §6 (the (a)-(d) escape).
- `research/p1/SYNTHESIS-PROFILE.md` §2 (JSON 91.5% hot leaf — MUST-preserve through the un-fork),
  §3 (CSS 94.1% scan — G2 derives, G3 emits THROUGH the un-forked path, G6 wires later).

Disk re-verification this pass (every load-bearing surface confirmed at the cited line):
- `crates/codegen/src/runtime_generator.rs:16-25` (the LIVE fork dispatch:
  `match request.profile_contract.emitter { RuntimeEmitterKind::CompiledLowering => …,
  RuntimeEmitterKind::RequestFacts => emit_request_facts(…) }` — S-P0 R3, the visible enum G3
  deletes), `:29-32` (`emit_compiled(profile_id, sink_only)` — the JSON `CompiledLowering` arm),
  `:67-70` (`grammar_profile::validate_generated_roster(profile_id, COMPILED_RUNTIME_FILES, …)` —
  the per-arm roster constant), `:91` (`("generated.rs", normalize(CSS_GENERATED_RS))` — the CSS
  `RequestFacts` arm body, the const courier G2 retired), `:701`
  (`const CSS_GENERATED_RS: &str = r#"` — gone post-G2).
- `crates/codegen/src/grammar_provider.rs:31-37` (`RuntimeProfileContract` carries `emitter`
  (field #1), `expected_files`, `frontend_requirements` (field #3), `output_labels` (field #4) —
  derives `Clone, Debug, PartialEq, Eq` at `:31`), `:39-43`
  (`pub enum RuntimeEmitterKind { CompiledLowering, RequestFacts }` — S-P0 R3), `:45-57`
  (`RuntimeFrontendRequirements` the 10-bool struct, derives `PartialEq, Eq` at `:45`), `:91-96`
  (`RuntimeOutputLabels` the 3-field struct, derives `PartialEq, Eq` at `:91`).
- `crates/codegen/src/lower/mod.rs:18-26` (the grammar-NEUTRAL 5-shape `select_lowering(cost)`
  over `cost.chosen: BackendShape` ∈ `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` — the already-Lock-14-clean discriminator G3 dispatches on instead of the
  `emitter` enum).
- `crates/codegen/src/lower/sink_only.rs:46-53` (`RuntimePolicySummary { backend_shape:
  BackendShape, … }` — the `program.policy_summary.backend_shape` the un-forked `render(program)`
  reads its output-shape from; the §5-risk-1 binding).
- `xtask/src/regen.rs:5-19` (`#[derive(Clone, Copy, Debug)] pub(crate) struct RuntimeTarget` —
  carries `emitter` (field #9, line 15), `frontend_requirements` (field #11, line 17),
  `output_labels` (field #12, line 18); the R16 +1-line `PartialEq` target — today NO `PartialEq`
  derive).
- `crates/runtime/src/tape/mod.rs:175-181` (`pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind,
  G: EventGrammar = AnyGrammar>` with `_kind: PhantomData<fn() -> K>` (the REAL K axis to PRESERVE)
  AND `_grammar: PhantomData<fn() -> G>` (the phantom `<G>` G4 DELETEs); `:202` `erase(self) ->
  ValueRef<…, AnyKind, G>`), `:227-232` (`pub trait DocumentView<'a> { type Root; root_value;
  tape_id; source }` — the latent extension surface, zero non-test consumers).
- `crates/runtime/src/tape/event_grammar.rs:4` (`pub trait EventGrammar: 'static`), `:17-19`
  (`pub enum AnyGrammar {}` + `impl EventGrammar for AnyGrammar` — the zero-variant default),
  `grammars/{json,sheets_witness}/event_grammar_witness.rs` (the `*EventGrammar` impls — DEFINED,
  consumed ONLY by `_tests.rs` `_proof_compiles`; the G-axis non-test instantiation census returns
  EMPTY, confirming the DELETE-default cannot be rebutted).

Dispatch lock: this packet authorizes NO G3/G4 implementation from S-P3 itself. G3 is
conditionally gated on `G1 ∧ G2 closed ∧ P4 live ∧ P3 (row-collapse)`; G4 gates on
`G1 ∧ G2 ∧ G3 closed`. Each wave remains blocked until its predecessors close AND the
orchestrator/user dispatches the wave triumvirate AND the entry gate below holds GREEN.

---

## §PC.0 — The two waves in the binding sequence (context)

Per `SYNTHESIS-RESEARCH.md` §3 and `SYNTHESIS-AUDIT-OVERFIT.md` §5, the standing order is
`PRUNE(P1..P5) → G1 → G2 → G3 → G4 → {G5/G6 ∥ PROVE} → H1`. PC covers the two MIDDLE GENERALIZE
waves — the structural un-fork (G3) and the substrate-singularization (G4):

```
G1  JSON projection (R-C C1)          [packet PB]
  └─ G2  CSS lowering (R-B B⊃A)        [packet PB]
       └─ G3  un-fork emitter (R-A A)  entry: G1 ∧ G2 closed ∧ P4 live ∧ P3 (row-collapse)
            └─ G4  shared trait + phantom (R-D A)  entry: G1 ∧ G2 ∧ G3 closed
                 ├─ G5/G6  neutral scan (R-F A)    [packet PD]
                 └─ PROVE  Sheets (R-E-2)          [packet PD]   — both consume G3∧G4
```

**Why G3 BEFORE G4 (coupling 4, `SYNTHESIS-RESEARCH.md` §2).** G4 cannot define the shared
value-API trait both JSON and CSS instantiate until BOTH JSON and CSS value-API surfaces emit
THROUGH ONE un-forked path (G3). A trait authored over two forked emitters would be a trait over
two different substrates — exactly the LCD-flatten REJECT. So G3's un-fork is the precondition for
G4's trait to be non-vacuous. The `<G>` DELETE is INDEPENDENT of the trait (separable per `a1 §L4`)
and may land FIRST WITHIN G4 to expose the clean `ValueRef<K>` the trait targets.

**Why both feed PROVE + G6 (couplings 3/5, `SYNTHESIS-RESEARCH.md` §2).** PROVE (Sheets) emits
THROUGH the un-forked G3 generator (its precedence tower is structurally unlike JSON+CSS, so it
CANNOT be a relabeled courier — the true test of grammar-DERIVED emission) AND the Sheets value
type instantiates the G4 trait (the phantom-`<G>` resolution made concrete by a third impl). G6
wires the NEON into the `balanced_component_scan` primitive emitted by the G3 un-forked path
(else it re-forks the shape G3 un-forks). So a G3 OR G4 rejection BLOCKS the entire PD cluster.

Per `[dispatch-hard-cap]`: every dispatch carries "HARD CAP: N min. At 0.9N commit, at N halt"
(research 20, plan 15, redress 30). G3 is HIGH-risk (the §5-risk-1 relocated seam is the single
sharpest risk in the campaign); G4 is MED-HIGH (the §5-risk-4 "trait too thin" critique). Both
may carry a documented larger redress cap if the named risk fires — the larger cap is RECORDED in
REDRESS, not silent.

---

## §PC.1 — G3 Wave: un-fork the emitter (R-A candidate A)

DELETE `RuntimeEmitterKind`; dispatch the single `render(program)` body on the lowered
`BackendShape` (`lower/mod.rs:18`), the already-Lock-14-clean grammar-NEUTRAL 5-shape
discriminator. The decision stays grammar-DERIVED (a cost-model over rule shapes, `cost.chosen`,
NOT a config field). Candidate B (`ProjectionSpec` value) is ABSORBED into A's per-`BackendShape`
renderers (each renderer declares its own roster, retiring `COMPILED_RUNTIME_FILES`/
`REQUEST_FACTS_RUNTIME_FILES` as per-arm constants). Candidate C (no discriminator, one canonical
fold) is the SK-V19 end-state, deferred on >SOTA-preservation burden (`SYNTHESIS-RESEARCH.md` §1).

Owner paths (named for the revert slice):
- `crates/codegen/src/runtime_generator.rs:16-25` (DELETE the `match … emitter` fork; replace with
  `match program.policy_summary.backend_shape { … }` per-`BackendShape` renderer dispatch),
  `:29-70` (`emit_compiled` becomes the `SinkOnly`/`CollapsedStage`-shape renderer), the retired
  CSS arm (`:88-95` post-G2, already grammar-derived).
- `crates/codegen/src/grammar_provider.rs:31-43` (DELETE `pub enum RuntimeEmitterKind`; REMOVE the
  `emitter` field from `RuntimeProfileContract` — the shape comes from the lowered program, not the
  contract; the `first_unsupported` gate at `:110-111` that keyed on `emitter != RequestFacts`
  re-keys on the `BackendShape` instead).
- `crates/codegen/src/grammar_profile.rs` (the per-arm roster constants `COMPILED_RUNTIME_FILES`/
  `REQUEST_FACTS_RUNTIME_FILES` → per-`BackendShape`-renderer rosters; B absorbed into A).
- `xtask/src/regen.rs:5-19` (DELETE the `emitter` field #9 from `RuntimeTarget`; ADD the
  `PartialEq` derive — the R16 +1-line recipe, §PC.1.6).
- generated runtime output (`grammars/*/generated.rs`, `config.rs`, `mod.rs` — named,
  diff-audited; byte-equivalent to the G1/G2-closed shipped files, since G3 changes the PATH not
  the OUTPUT).
- `crates/bbnf-bench/`, `skinny/RESULTS.md`; `skinny/REDRESS.md` if rejected.

### G3.1 — Entry gate (the binding predicate; GREEN before dispatch)

Per `SYNTHESIS-RESEARCH.md` §3 line 176 and `SYNTHESIS-AUDIT-OVERFIT.md` §5: **`G1 ∧ G2 closed ∧
P4 live ∧ P3 (row-collapse)`** — a 4-conjunct gate.

- **G1 closed:** JSON emits via the `SinkOnlyExpr` AST-walk projector (`verbatim_blob_present ==
  false` for JSON), so the un-forked `SinkOnly`-shape renderer inherits a DERIVED body, not a
  fixed literal (coupling 1: without G1, A inherits a fixed-literal JSON arm).
- **G2 closed:** CSS actually LOWERS to a real lowered CSS scan IR (`CSS_GENERATED_RS` grep == 0,
  `verbatim_blob_present == false` for CSS), so the un-forked renderer has a non-const
  `BackendShape` renderer input (coupling 1: without G2, A has no non-const CSS renderer input).
- **P4 live:** the Lock-14 gate is meaningful (`lock14_gate_scans_codegen == true`,
  `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_, _RS, EventGrammar, *EventGrammar}`) so the un-forked emitter
  is neutrality-scanned AS it is authored — a grammar-named branch cannot be re-introduced
  undetected (`SYNTHESIS-AUDIT-OVERFIT.md` §5 fact 2: P4 MUST land BEFORE G3).
- **P3 (row-collapse):** `runtime_target_rows_collapsed == true` — the R16 full-row `PartialEq`
  co-gate already holds from P3, so when G3 removes the `emitter` field the structural-row
  invariant is already enforced (the field removal cannot re-introduce a per-grammar row the gate
  no longer sees).

CHALLENGE acceptance required (G3 is first-of-class, structure-touching, HIGH-risk): the un-fork
is NOT a relocation of the fork into a neutral data table (the §5-risk-1 relocated seam); the
per-`BackendShape` renderer reads its shape from the lowered program, never from a `RuntimeTarget`
or `RuntimeProfileContract` field.

### G3.2 — Exit gate (MEASURABLE; the FOUR-conjunct binding proof)

Per `SYNTHESIS-RESEARCH.md` §3 lines 176-185 — `SYNTHESIS-AUDIT-OVERFIT.md` §1 addenda 2+3 — the
exit gate is the CONJUNCTION of FIVE measurable predicates (the four task-named conjuncts + the
addendum-2 row-collapse), emitted into the PE telemetry registry:

1. **`emitter_fork_present == false`** (addendum 3, R3). The `RuntimeEmitterKind` enum and BOTH its
   variants are deleted. Falsifier: grep `RuntimeEmitterKind|CompiledLowering|RequestFacts` over
   `crates/codegen/src/` + `xtask/src/` == 0 (test-excluded). One `render(program)` path serves
   JSON+CSS dispatched on `BackendShape`.
2. **`generator_grammar_branch_count == 0`** (addendum 2 conjunct, R3). No generic branch in the
   un-forked emitter selects behaviour by grammar name/family. Falsifier: the arm-census grep over
   the `render(program)` body for `match grammar`/`if grammar_name ==`/`"json"`/`"css"`/`"google_
   sheets"` literals == 0.
3. **`generator_grammar_type_count == 0`** (addendum 2 conjunct, R3). No grammar-named type
   discriminates the emit path. Falsifier: no `JsonEmitter`/`CssEmitter`/`*Emitter`-per-grammar
   type in the emit path; the only discriminator is the neutral `BackendShape` enum.
4. **`runtime_target_rows_collapsed == true`** (addendum 2 conjunct, R16, the STRUCTURAL co-gate).
   The R16 full-row `PartialEq` derive holds AFTER the `emitter` field is removed from
   `RuntimeTarget`. This is the ONLY check that catches the relocated seam (a per-grammar branch
   moved into a neutral data table) — the arm-census grep is syntactically incapable of seeing it
   (§5-risk-1, `SYNTHESIS-AUDIT-OVERFIT.md` §1 "caught ONLY by the structural row-collapse").
5. **`emit_shape_source == lowered_program`** (addendum 3, the FOURTH conjunct — the §5-risk-1
   binding). The un-forked `render(program)` body reads its output-shape ONLY from
   `program.policy_summary.backend_shape` (`sink_only.rs:48`), NEVER from a `RuntimeTarget` /
   `RuntimeProfileContract` field. **Falsifier:** grep the `render(program)` body for any read of
   `target.profile` / `target.emitter` / `target.output_labels` / `target.profile_contract` /
   `contract.emitter` == 0. **Without this fourth conjunct, the §5-risk-1 relocated seam riding the
   neutral per-profile columns passes all of conjuncts 1-4 under a green gate.** This is the binding
   distinction between an HONEST un-fork and a paper-close that relocates the fork into data.

Plus the cross-cutting MUST-preserves (the un-fork changes the PATH, not the OUTPUT):
6. **Byte-equivalent generated output.** The G3-regenerated `grammars/*/generated.rs` are
   byte-equivalent to the G1/G2-closed shipped files (G3 un-forks the dispatch, it does not
   re-derive the bodies). Falsifier: `cargo xtask regen --check` clean; diff of regenerated vs
   shipped == empty for every grammar.
7. **JSON 91.5% hot leaf preserved (`SYNTHESIS-PROFILE.md` §2).** `parse_object_value_at_direct` +
   `parse_array_element_at_direct` re-emitted with identical `inline(always)` shape + `sink.*` call
   sites through the un-forked `SinkOnly`-shape renderer; the JSON >sonic-rs-strict guard holds.
8. **CSS >SOTA preserved (`SYNTHESIS-PROFILE.md` §3).** `track1_rich/lightningcss >= S-P1 ratio`
   held same-run on the corpus-in-timer harness (the un-fork must not regress the G2-derived scan).

### G3.3 — The R16 row-collapse recipe (the SINGLE structural co-gate, pinned to this packet)

`SYNTHESIS-AUDIT-OVERFIT.md` §5 fact 5 binds R16 to S-P3. The binding recipe, re-grounded at
`regen.rs:5-19`:

- **Today:** `RuntimeTarget` derives `Clone, Copy, Debug` ONLY (`regen.rs:5`) — NO `PartialEq`.
  It carries `emitter` (field #9, `:15`), `frontend_requirements` (field #11, `:17`),
  `output_labels` (field #12, `:18`).
- **Recipe (PREFERRED, +1 line):** add `PartialEq` to the `RuntimeTarget` derive. This is the
  R16 mechanism because it **recurses into BOTH nested structs automatically** —
  `RuntimeFrontendRequirements` (field #11) AND `RuntimeOutputLabels` (field #12, behind the
  `Option`) — and CANNOT be coupled to a hand-rolled field list. Both nested structs ALREADY
  derive `PartialEq, Eq` (`grammar_provider.rs:45` / `:91`), so the recurse is free; only the
  `RuntimeTarget` line changes.
- **Why NOT a hand-rolled prose-field comparison:** the audit names this REJECT explicitly
  (`SYNTHESIS-AUDIT-OVERFIT.md` §5: "a recipe that recurses into `output_labels` only would slip a
  future seam riding `frontend_requirements`"; "a hand-rolled prose-field comparison risks a
  shallow-compare false-green of EITHER nested struct"). The full-row `PartialEq` derive is the
  ONLY recipe that cannot be coupled to a hand-rolled field list.
- **Numbering hazard (do not conflate):** the #11/#12 ordinals are `RuntimeTarget`'s (`regen.rs`
  source lines 17/18). The SAME two structs recur as `RuntimeProfileContract` fields #3/#4
  (`grammar_provider.rs:35-36`). The R16 derive lives on `RuntimeTarget` (`regen.rs:5`), per the
  a1 §3 numbering hazard.
- **Co-gate timing:** P3 lands the row-collapse co-gate; G3 removes the `emitter` field (field #9)
  from `RuntimeTarget`. The `runtime_target_rows_collapsed == true` invariant must hold ACROSS the
  field removal — the removal cannot re-introduce a byte-identical-but-for-emitter row pair.

### G3.4 — The §6 generality stress (the precedence-tower CANDIDATE, deferred to PROVE)

`SYNTHESIS-RESEARCH.md` §4: G3's un-fork is where the Sheets precedence tower's generality is
FIRST stressed, but the §6 finding (if any) surfaces at PROVE, not G3 (Sheets does not emit in
this wave). G3's obligation is to render recursive `CallRule`/`RepeatLoop` chains from grammar
structure (`sink_only.rs:69-96` `SinkOnlyExpr::{CallRule, RepeatLoop, Seq, Alt}`) — IF the
un-forked `render(program)` body cannot, the tower breaks at PROVE and a named, `.bbnf`-invoked,
parameterized precedence primitive surfaces (gated (a)-(d), `SYNTHESIS-RESEARCH.md` §4). G3 must
NOT special-case any recursion depth or rule-name; the recursion comes from `SinkOnlyExpr`
structure, the neutral IR.

### G3.5 — Telemetry, caps, reruns, revert, downstream

**Telemetry (emitted by G3, consumed by `gate-json --skv18-generalization-report` in the G3
slice; the canonical column registry lives in PE `pe-gate-telemetry-close.md` §gate-consumer):**
```
emitter_fork_present             (bool;  MUST be false — RuntimeEmitterKind/CompiledLowering/RequestFacts grep == 0)
generator_grammar_branch_count   (int;   MUST be 0   — no grammar-name branch in the emit path)
generator_grammar_type_count     (int;   MUST be 0   — no grammar-named emit-path type)
runtime_target_rows_collapsed    (bool;  MUST be true — R16 full-row PartialEq over BOTH nested structs)
emit_shape_source                (enum {lowered_program|runtime_target}; MUST be lowered_program — render(program) reads NO target.* field)
dirty_generated_state            (clean; regen --check byte-equivalent vs G1/G2-closed shipped files)
```
Every emitted field is consumed by `gate-json` in the SAME wave (typed-materialization-invariant);
a producer-only field fails the wave.

- **Cap:** ≤90 min wave wall; 30 min per redress dispatch ("HARD CAP: 30 min. At 27 commit, at 30
  halt"). G3 is HIGH-risk (the relocated seam) and may carry a documented larger redress cap if
  the §5-risk-1 seam fires — RECORDED in REDRESS, not silent.
- **Rerun ceiling:** one full gate refresh (the 5-conjunct exit gate + the byte-equivalence regen
  + the JSON/CSS MUST-preserves); a second requires a REDRESS cost note. Extra reruns are REDRESS
  cost evidence, not retry room.
- **Pre-blocked routes:** the relocated seam (un-forking the visible enum while leaving a
  per-grammar branch in a neutral data table — `target.profile`-selected `ProjectionSpec`, the
  per-profile columns); a per-grammar `match grammar_name` arm; md5-distinct treated as sufficient
  (it is necessary-NOT-sufficient — addendum 2); a behaviour change to the JSON/CSS bodies
  masquerading as an un-fork (the output must be byte-equivalent).
- **Revert protocol:** revert the `runtime_generator.rs` + `grammar_provider.rs` +
  `grammar_profile.rs` + `regen.rs` + generated-output commits as ONE slice; restore the opening
  RESULTS; add a REDRESS naming WHICH conjunct failed (fork-present / grammar-branch /
  grammar-type / row-collapse / emit-shape-source) and the relocated-seam witness if conjunct 5
  fired.
- **Downstream effect:** **G3 REJECTION BLOCKS G4, G6, PROVE** (G4 shares the trait over the
  un-forked JSON/CSS surfaces; G6 wires the NEON into the G3-emitted scan; Sheets emits THROUGH
  the un-forked generator). No downstream wave dispatches over a REDRESSed G3.

---

## §PC.2 — G4 Wave: shared value-API trait + phantom resolution (R-D candidate A)

A thin `Cursor` micro-trait over the surviving `ValueRef<K>` + extend the existing `DocumentView`
to CSS/Sheets; DELETE the phantom `<G>`. The unique candidate giving ≥2 impls that CANNOT collapse
to the lesser — the trait shares the laziness/cursor contract, NEVER navigation, so JSON's rich
tree (`get`/`pairs`/typed `JsonValue`/recursive visitor) is preserved by construction
(`json_rich_navigation_preserved == true`). Candidate B (tree-shaped `Value` stack) is REJECTED
(HIGH LCD-flatten hazard or dead degenerate CSS impls for an absent generic consumer); candidate C
(`DocumentView`+stream-only) under-delivers (shares the stream, not value navigation)
(`SYNTHESIS-RESEARCH.md` §1).

**Two separable sub-tasks (coupling 4, `SYNTHESIS-RESEARCH.md` §2; the `<G>` DELETE is INDEPENDENT
of the trait per `a1 §L4`):**
- **G4a — the phantom `<G>` DELETE (may land FIRST within G4).** Remove the
  `G: EventGrammar = AnyGrammar` parameter + `_grammar: PhantomData<fn() -> G>` field from
  `ValueRef` (`tape/mod.rs:175-181`); the K-axis (`_kind: PhantomData<fn() -> K>`) is the REAL
  Kind axis and is PRESERVED. DELETE the `EventGrammar` trait + `AnyGrammar` enum
  (`tape/event_grammar.rs`) + the test-only `*EventGrammar` witnesses
  (`grammars/{json,sheets_witness}/event_grammar_witness.rs`) + their `_proof_compiles` test
  consumers. The non-test instantiation census is EMPTY (re-confirmed on disk this pass:
  `ValueRef<…Grammar>` non-test usage == only the definition site), so the DELETE cannot be
  rebutted as ignoring a production `EventGrammar` impl.
- **G4b — the `Cursor` micro-trait.** Define the thin trait over the now-clean `ValueRef<K>`
  sharing ONLY the cursor/laziness contract (advance, child, offset, kind-tag-read — the lazy
  navigation primitives), extend `DocumentView` (`tape/mod.rs:227-232`) to CSS/Sheets. ≥2 real
  impls: JSON `ValueRef<K>` (rich tree nav) + CSS `CssNode` (flat sweep) that CANNOT collapse to
  the lesser.

Owner paths (named for the revert slice):
- `crates/runtime/src/tape/mod.rs:175-223` (REMOVE the `G` param + `_grammar` field from
  `ValueRef`; `erase` at `:202` loses its `G`), `:227-232` (extend `DocumentView` to the CSS/Sheets
  root types).
- `crates/runtime/src/tape/event_grammar.rs` (DELETE the file — `EventGrammar` trait + `AnyGrammar`
  + the default impl).
- `crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs` (DELETE — the
  test-only witnesses) + their `_tests.rs` `_proof_compiles` consumers.
- `crates/runtime/src/tape/cursor.rs` (NEW — the thin `Cursor` micro-trait; directory-module
  isomorphic, not a flat sibling — `[directory-module-structure]`).
- `crates/runtime/src/grammars/json/value.rs` + `grammars/css_l4_*/` (the ≥2 impls).
- generated runtime output (named, diff-audited — the JSON rich-nav surface must stay byte-equal);
  `crates/bbnf-bench/`, `skinny/RESULTS.md`; `skinny/REDRESS.md` if rejected.

### G4.1 — Entry gate (the binding predicate; GREEN before dispatch)

Per `SYNTHESIS-RESEARCH.md` §3 line 186: **`G1 ∧ G2 ∧ G3 closed`** — a 3-conjunct gate.

- **G1 ∧ G2 closed:** JSON and CSS both emit DERIVED value-API surfaces.
- **G3 closed:** the un-forked emitter emits BOTH JSON and CSS value-API surfaces THROUGH ONE path
  (coupling 4: G4 cannot define a trait both instantiate until both flow through one un-forked
  path; a trait over two forked emitters is a trait over two substrates = the LCD-flatten REJECT).

CHALLENGE acceptance required (G4 is MED-HIGH-risk — the §5-risk-4 "trait too thin" critique): the
`Cursor` trait shares the cursor/laziness contract, NOT a forced common value shape; ANY trait
wide enough to satisfy the "too thin" critic is wide enough to LCD-flatten JSON (the REJECT). The
honest generalization IS the cursor/laziness contract.

### G4.2 — Exit gate (MEASURABLE)

Per `SYNTHESIS-RESEARCH.md` §3 lines 186-188 — `SYNTHESIS-AUDIT-OVERFIT.md` §1 addendum 4 — the
exit gate is the CONJUNCTION of THREE measurable predicates, emitted into the PE registry:

1. **`phantom_generic_resolved == deleted`** (addendum 4, R5). The `<G>` / `EventGrammar` /
   `AnyGrammar` axis is deleted. Falsifier: grep `EventGrammar|AnyGrammar|G: EventGrammar|_grammar:
   PhantomData` over `crates/runtime/src/` == 0 (test-excluded; the grep MUST test-exclude — the
   standing `_proof_compiles::<JsonEventGrammar>` is test-only and must NOT false-green). The enum
   is `{deleted|instantiated|present}`; MUST be `deleted`. NOTE: the grep targets the `G`
   (EventGrammar) axis, NOT the REAL `K` (Kind) axis — `_kind: PhantomData<fn() -> K>` survives
   (preserve-rich-ast).
2. **`json_rich_navigation_preserved == true`** (addendum 4, R6 — makes the ≥2-impl count
   necessary-not-sufficient). JSON's rich tree navigation (`get`/`pairs`/typed `JsonValue`/
   recursive visitor) is byte-equal after the trait extraction. Falsifier: the JSON `value.rs`
   navigation surface diffs empty vs its pre-G4 form; JSON 51/51 (or 91.5%-leaf strict) held;
   value-plane population parity intact. The trait must NOT LCD-flatten JSON's tree toward CSS's
   flat sweep.
3. **≥2 real impls that CANNOT LCD-collapse to the lesser** (addendum 4 + R6). JSON `ValueRef<K>`
   (rich tree) + CSS `CssNode` (flat sweep) — proven non-collapsible: removing either impl's
   navigation surface must NOT compile to the other's (the trait shares cursor/laziness, never
   navigation). Falsifier: a degenerate-equal CSS impl that is just JSON's nav with dead branches =
   REJECT (Candidate B's failure mode); the two impls' navigation surfaces are structurally
   distinct.

Plus the cross-cutting MUST-preserve:
4. **No second substrate (Lock 1).** The `Cursor` trait is a view over the EXISTING
   `Tape`/`ValueRef`/`PayloadArena`; NO second tape, no parallel cursor type, no eager value tree.
   Falsifier: grep for a new `*Tape`/`*Cursor` substrate type == 0; laziness intact (no per-leaf
   `Box::new`).

### G4.3 — The §5-risk-4 mitigation (the "trait too thin" critique, bound to telemetry)

`SYNTHESIS-RESEARCH.md` §5 risk 4: Candidate A's `Cursor` micro-trait may be challenged as
under-delivering. The binding mitigation is to TIE the verdict to the two falsifiable telemetry
columns (`phantom_generic_resolved == deleted` ∧ `json_rich_navigation_preserved == true`), NOT to
a subjective width judgement. The rebuttal is structural: ANY trait wide enough to satisfy the
critic is wide enough to LCD-flatten JSON (the REJECT) — so the honest generalization is the
cursor/laziness contract, and the ≥2-non-collapsible-impl gate (conjunct 3) is what proves it is a
real generalization, not a forced common value shape. The critique cannot reopen the candidate
choice without producing a wider trait that passes conjunct 2 — which the §5-risk-4 analysis shows
is impossible.

### G4.4 — Telemetry, caps, reruns, revert, downstream

**Telemetry (emitted by G4, consumed by `gate-json` in the G4 slice; canonical registry in PE):**
```
phantom_generic_resolved         (enum {deleted|instantiated|present}; MUST be deleted — the <G>=EventGrammar axis, test-excluded)
json_rich_navigation_preserved   (bool; MUST be true — JSON tree nav byte-equal; the ≥2-impl count's necessary-not-sufficient companion)
shared_trait_impl_count          (int; MUST be >= 2 — JSON ValueRef<K> + CSS CssNode, non-collapsible)
```
Every emitted field is consumed by `gate-json` in the SAME wave; a producer-only field fails.

- **Cap:** ≤90 min wave wall; 30 min per redress dispatch ("HARD CAP: 30 min. At 27 commit, at 30
  halt"). G4 is MED-HIGH; a documented larger redress cap is allowed if the "trait too thin"
  critique forces a trait-width REDRESS — RECORDED, not silent.
- **Rerun ceiling:** one full gate refresh (the 3-conjunct exit + the no-second-substrate
  MUST-preserve + JSON nav byte-equality); a second requires a REDRESS cost note.
- **Pre-blocked routes:** a tree-shaped `Value` stack (Candidate B, LCD-flatten hazard); a
  `DocumentView`+stream-only trait (Candidate C, under-delivers); a degenerate-equal CSS impl
  (dead branches over JSON's nav); deleting the REAL `K` (Kind) axis (preserve-rich-ast); a second
  substrate; an eager value tree; a `_proof_compiles` test false-greening the phantom grep.
- **Revert protocol:** revert the `tape/mod.rs` + `tape/event_grammar.rs` (deletion) +
  `tape/cursor.rs` (new) + the impls + generated-output commits as ONE slice; restore RESULTS; add
  a REDRESS naming WHICH conjunct failed (phantom-resolved / json-nav-preserved / impl-count) and
  whether the trait LCD-flattened JSON.
- **Downstream effect:** **G4 REJECTION BLOCKS PROVE** (the Sheets value type instantiates the G4
  trait — the phantom-`<G>` resolution made concrete by a third impl; G4 is a DIRECT conjunct of
  PROVE's entry gate, `SYNTHESIS-RESEARCH.md` §2 coupling 5). G4 does NOT block G6 (G6 wires the
  NEON, independent of the value-API trait). No downstream wave dispatches over a REDRESSed G4.

---

## §PC.3 — The shared (a)-(d) escape + Lock-14 obligation (both waves)

Neither G3 nor G4 lands a §6 named primitive (G3 un-forks the dispatch; G4 extracts a trait) — so
the (a)-(d) escape (`SYNTHESIS-RESEARCH.md` §4) is NOT exercised in PC. But both waves carry the
Lock-14 / addendum obligations:

- **Addendum 2 (3-co-gate conjunction) at and after G3:** `runtime_target_rows_collapsed == true`
  ∧ `generator_grammar_branch_count == 0` ∧ `generator_grammar_type_count == 0` — md5-distinct is
  necessary-NOT-sufficient. G3 establishes these; G4 must NOT re-introduce a grammar-named emit
  branch or type.
- **Addendum 3 (single-emitter) at G3:** the un-forked `render(program)` reads its output-shape
  from the lowered program (`program.policy_summary.backend_shape`, `sink_only.rs:48`), NEVER from
  a `RuntimeTarget`/`RuntimeProfileContract` field (`emit_shape_source == lowered_program`).
- **Addendum 4 (phantom-generic) at G4:** the `<G>` axis is DELETE-resolved; the REAL `K` axis is
  preserved; the grep test-excludes; `json_rich_navigation_preserved == true`.
- **Witness-emission scan-root coupling (P4):** if the un-forked G3 emitter or the G4 trait
  extraction emits any grammar-named `EventGrammar` literal into the generated runtime, the
  `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` (extended by P4 with
  `CSS_`/`_RS`/`EventGrammar`/`*EventGrammar`) catches it at the emit site. (G4 DELETEs the
  `EventGrammar` axis entirely, so post-G4 NO `EventGrammar` literal can be emitted.)
- **R16 (the SINGLE structural co-gate threading R-A/R-B/R-E):** the `RuntimeTarget: PartialEq`
  full-row derive (§PC.1.3) is the ONE check that catches the relocated seam across G3's un-fork.

### §PC.3.1 — Residual-risk → mitigation map (the PC slice of `SYNTHESIS-RESEARCH.md` §5)

| Risk | Wave | Mitigation (binding) |
|---|---|---|
| 1. The relocated seam (single sharpest) | G3 | `runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`) + `emit_shape_source == lowered_program` (render reads NO `target.*` field) — the FOURTH conjunct without which conjuncts 1-3 pass under a green gate (§PC.1.2) |
| 4. "trait too thin to be a generalization" | G4 | bind to `phantom_generic_resolved == deleted` ∧ `json_rich_navigation_preserved == true` ∧ ≥2-non-collapsible-impl; any wider trait LCD-flattens JSON (§PC.2.3) |
| 5. The un-forked G3 emitter does not exist yet (transitive) | G3 | G3 renders recursive `CallRule`/`RepeatLoop` chains from `SinkOnlyExpr` structure (§PC.1.4); IF it cannot, the §6 precedence primitive surfaces at PROVE, not paper-closed (the fallback is binding: surface honestly, never hand-write a `_GENERATED_RS` Sheets block) |

---

## §PC.4 — Summary of this packet

PC binds the two MIDDLE GENERALIZE waves. G3 DELETES `RuntimeEmitterKind` and dispatches one
`render(program)` body on the lowered `BackendShape` (`lower/mod.rs:18`), with a FIVE-conjunct exit
gate — `emitter_fork_present == false ∧ generator_grammar_branch_count == 0 ∧
generator_grammar_type_count == 0 ∧ runtime_target_rows_collapsed == true ∧ emit_shape_source ==
lowered_program` — where the fifth conjunct (render reads NO `target.*` field) is the load-bearing
defence against the §5-risk-1 relocated seam, bound to the R16 +1-line `RuntimeTarget: PartialEq`
full-row recipe that recurses into BOTH `frontend_requirements` (field #11) AND `output_labels`
(field #12). G4 DELETEs the phantom `<G>`/`EventGrammar` axis (preserving the REAL `K` axis) and
extracts a thin `Cursor` micro-trait sharing only the cursor/laziness contract, with a
THREE-conjunct exit — `phantom_generic_resolved == deleted ∧ json_rich_navigation_preserved == true
∧ shared_trait_impl_count >= 2` — where the ≥2-non-collapsible-impl gate proves it is a real
generalization, not an LCD-flatten. Entry-gates: G3 = `G1 ∧ G2 ∧ P4 ∧ P3`; G4 = `G1 ∧ G2 ∧ G3`.
Both feed the PD cluster (G6 wires the G3-emitted scan; PROVE emits Sheets THROUGH the un-forked
generator and instantiates the G4 trait as the third impl). Caps: 30 min/redress, HIGH (G3) /
MED-HIGH (G4); telemetry emits into the canonical PE registry, consumed same-wave by
`gate-json --skv18-generalization-report`. Next packet: PD (G5/G6 + PROVE + H1).
Ready-for-S-P3-consolidation.
