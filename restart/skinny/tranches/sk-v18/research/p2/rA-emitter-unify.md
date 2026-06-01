# SK-V18 S-P2 / R-A — Un-fork the Emitter (research digest)

Class R-A. Addendum 3 (single-emitter-path, `a1-six-addenda-lens-registry.md` §L3),
co-gated by addendum 2 (distinct-grammar-output + row-collapse, §L2) and addendum 1
(verbatim-blob, §L1). Wave G3 (entry-gate: G1 + G2 closed; P4 Lock-14 gate landed BEFORE
the rebuild; P3 collapse before G2). RESEARCH only — no code, no cargo. Every claim grounded
in the live tree at the cited path:line. Host: aarch64 / Apple M5 Max ONLY.

## 0. The grounded fork — what it actually is, measured on disk

The "fork" is NOT a clean strategy split. It is **asymmetric in depth**: two emit paths that
diverge at `runtime_generator::emit_from_request` and never re-converge.

- **The discriminator.** `grammar_provider.rs:40-42` — `pub enum RuntimeEmitterKind {
  CompiledLowering, RequestFacts }` (the `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` is at
  `:39`); selected per-grammar by `RuntimeProfileContract.emitter` (`:33`). Dispatched at `runtime_generator.rs:16` (`match request.profile_contract.emitter`)
  and again at `grammar_provider.rs:110` (`if … emitter != RuntimeEmitterKind::RequestFacts`,
  which gates the `first_unsupported()` fail-closed check — JSON pays it, CSS is exempt). The
  variant names are NEUTRAL, so the L2 arm-census grep (`Json =>`) stays clean — this is
  exactly why L3 is a distinct lens (`a1` §L3: "the fork hides behind the neutral enum names").

- **JSON arm (`CompiledLowering`) — projects, but shallowly.** `runtime_generator.rs:17-24`
  → `crate::emit_from_source` (`lib.rs:107`) → `parse_grammar` → `passes::compile` →
  `lower::lower_to_rust` → `SinkOnlyProgram` (`lower/sink_only.rs:20`) → `emit_compiled`
  (`runtime_generator.rs:29`) → `json_sink_direct::render` (`json_sink_direct.rs:4`). The
  `.bbnf` IS consumed. BUT `render` walks NOTHING of `SinkOnlyExpr` (the real structural IR,
  `sink_only.rs:69-96`): `render_value_dispatch`/`render_container_rules`/`render_string_rule`/
  `render_utility_rules` take only `out: &mut String` and push **fixed** raw-string bodies
  (`json_sink_direct.rs:124,251,326,497`). The hardcoded dispatch bytes `{[",-tfn`
  (`:138-163`) are NOT derived from `value = object|array|string|number|bool|null`
  (`json.bbnf:11`). Only `render_header` (`:68`, uses `program.entry_rule`/`direct_shapes`)
  and `render_number_emitter` (`:457`, parameterizes the sink-prefix + fn-name) touch program
  data. **So JSON is a fixed-literal courier wrapped in render functions** — the same class as
  CSS, just structurally fragmented (this is the L1 D1 finding "the grammar only `validate()`-
  gates emission, does not shape it", `CONSOLIDATED-AUDIT.md:31`). G1 is the predecessor that
  fixes this; R-A inherits G1's projector.

- **CSS arm (`RequestFacts`) — couriers verbatim.** `runtime_generator.rs:25` →
  `emit_request_facts` (`:76`) → `("generated.rs", normalize(CSS_GENERATED_RS))` (`:91`).
  `CSS_GENERATED_RS` (`:701`→`:1611`, 910-LOC raw string) is a hand-written `CssFullParser`
  recursive-descent scanner + lazy `ValueRef` projection (`CssNode`/`CssTypedNode`,
  `:744`/`:815`) that ignores every `->`/`@pretty`/`@ws`/`@token` in `stylesheet.bbnf`. The
  `.bbnf` is parsed ONLY for `facts` (`grammar_provider.rs:108`) which feed
  `render_request_facts_config` (`:105`, request-identity constants) — never the body. G2 is
  the predecessor that retires it.

- **The roster asymmetry.** The two arms emit DIFFERENT file sets:
  `COMPILED_RUNTIME_FILES` = 8 files {config,generated,host,mod,parser,value,view,visitor}
  (`main.rs:175`); `REQUEST_FACTS_RUNTIME_FILES` = 5 files {config,generated,mod,parser,sink}
  (`regen_css.rs:25`). Each arm hard-validates its own roster
  (`runtime_generator.rs:67,96`). A unified emitter must produce a roster as a FUNCTION of
  grammar-derived surface facts, not a per-arm constant.

- **The relocated-seam already exists in the metadata.** The 7 CSS `RuntimeTarget` rows
  (`regen_css.rs:35-162`) are byte-identical except `profile`/`output_dir`/`check_command`/
  `output_labels` — all decorative (same `grammar_name:"css_l4"`, `entry_rule:"stylesheet"`,
  `source_inputs:CSS_L4_SOURCES`, `source_roots:CSS_L4_ROOTS`). They yield 7 byte-identical
  `generated.rs` (md5 `b654562c…`). **A naive un-fork that moves `RuntimeEmitterKind` into a
  per-row strategy field in this table is the textbook relocated-seam** (L2 check (c) / L3
  REJECT). This is the single sharpest risk for R-A.

## 1. What "subsume both" must mean (the binding target, from SYNTHESIS.md:333 / :565-567)

ONE grammar-agnostic emit fn serving JSON+CSS+Sheets, dispatching on grammar-DERIVED DATA
(alphabet, entry rule, rule shapes, projection shapes from the `.bbnf`/lowered IR), NOT on a
grammar-family tag — co-gated by `emitter_fork_present == false`,
`generator_grammar_branch_count == 0`, `generator_grammar_type_count == 0`,
`runtime_target_rows_collapsed == true`. The decision of WHAT to emit (sink-only direct-to-
struct vs lazy-tape rich projection vs Pratt) must fall out of facts the lowering already
computes, exactly as `lower::select_lowering` (`lower/mod.rs:18`) already dispatches on
`CostFacts.chosen: BackendShape` — a grammar-NEUTRAL discriminator (5 shapes, no grammar name).

## 2. Candidate architectures

### Candidate A — DELETE `RuntimeEmitterKind`; dispatch on the lowered `BackendShape` (the existing neutral axis)

The lowering already classifies each grammar onto one of 5 grammar-neutral `BackendShape`s
(`lower/mod.rs:18-25`: EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage) via
`CostFacts.chosen`. JSON lowers to `SinkOnly`; CSS's hand-written tape projection corresponds
to an offset/event-tape shape. **The un-forked emitter is one fn that runs the FULL lowering
for every grammar, reads `program.policy_summary.backend_shape` (already on
`SinkOnlyProgram`, `sink_only.rs:48`; the analogue exists per shape), and projects via the
shape's renderer** — `SinkOnly → json_sink_direct::render`-class projector (post-G1, walking
`SinkOnlyExpr`), tape-shape → a CSS/Sheets tape-projection renderer (post-G2, walking the
lowered tape IR). `RuntimeEmitterKind`, `RuntimeProfileContract.emitter`, and the
`grammar_provider.rs:110` exemption are DELETED; the fail-closed `first_unsupported()` check
runs for every grammar uniformly.
- **Pro:** reuses an EXISTING neutral discriminator (`select_lowering` already proves the
  pattern is legitimate and Lock-14-clean — it's a 5-shape enum with zero grammar names). No
  new abstraction invented. The decision is grammar-derived by construction: the shape comes
  from the cost model over the rule shapes, not from a config field. Collapses the roster
  question into "the shape's renderer declares its file set." Directly satisfies
  `generator_grammar_branch_count == 0` because `BackendShape` carries no grammar token.
- **Con:** requires CSS to actually LOWER (G2 must produce a real lowered tape IR for CSS —
  today CSS never reaches `lower_to_rust`; it's a const). If G2 lands a faithful CSS lowering,
  this is free; if G2 stalls, A has no CSS input. So A is **maximally G2-coupled**.
- **Relocated-seam exposure:** LOW. The discriminator is the cost-model output, not a config
  row; `runtime_target_rows_collapsed` is satisfiable because `emitter` leaves the
  `RuntimeTarget` struct entirely (one fewer divergent column).

### Candidate B — Collapse `RuntimeEmitterKind` into a data-driven `ProjectionSpec` derived from surface facts

Replace the 2-variant grammar-family enum with a `ProjectionSpec` VALUE computed from
`RuntimeSourceFacts` (`grammar_provider.rs:108`) + the lowered program: e.g.
`{ output_shape: enum{DirectToStruct, LazyTapeRich, …}, roster: Vec<FileRole>,
materialization: …, projection_targets: … }` — a struct of grammar-DERIVED data, each field a
fact the lowering/`facts` already expose (the `RuntimeFrontendRequirements` 10-bool struct
`grammar_provider.rs:46-57` is a primitive prototype of this). The single emit fn consumes the
`ProjectionSpec` and emits; no grammar tag anywhere. The 10-bool `frontend_requirements`
(`grammar_provider.rs:46-57`: `import_closure,whitespace_directive,whitespace_modifier,discard_operator,pretty_directive,host_capture,projection,typed_projection,token_directive,comma`) +
`output_labels` + `expected_files` columns of `RuntimeTarget` fold INTO `ProjectionSpec`
(removing the per-row decorative divergence that the 7 CSS rows carry today).
- **Pro:** makes the emit decision a first-class inspectable VALUE (testable in isolation;
  satisfies `[pluggable-components]` — the decision point is a spec, not a branch). Subsumes
  the roster asymmetry cleanly (roster is a spec field). Can express Sheets without touching
  the dispatch.
- **Con:** this is the **highest relocated-seam risk** (L2 check (c) / L3 REJECT). A
  `ProjectionSpec` is a data-table; if `output_shape` ends up SELECTED from the `profile`
  string or hand-set per-grammar in the `RuntimeTarget` row rather than DERIVED from the
  lowering, the fork has merely moved into a neutral-identifier table — exactly the seam
  `runtime_target_rows_collapsed` exists to catch (`SYNTHESIS.md:566`, CH2 V3 §8.1). The spec
  is only honest if every field is a pure function of the lowered grammar, NOT of the config
  row. Requires a discipline gate B cannot self-enforce.
- **Relocated-seam exposure:** HIGH unless `ProjectionSpec` is computed by the lowering
  (`passes::compile` output), never assembled in xtask.

### Candidate C — Make `EmittedSource` purely the lowering's output; emit fn becomes a thin IR-walker (no discriminator at all)

The most aggressive abrogation: there is no emitter-kind decision because there is no
emitter-kind. `passes::compile`/`lower_to_rust` produces a single canonical lowered program
(the `SinkOnlyProgram` generalized to carry tape-projection rules too, or a unifying
`LoweredProgram`), and `render` is a structural fold over that IR — every construct
(`SinkOnlyExpr::Seq/Alt/RepeatLoop/DirectBuild/RegexProgram/CallRule/TapeEmit/ValueProject`,
`sink_only.rs:69`) has ONE rendering, shared by all grammars. JSON's direct-to-struct and
CSS's lazy-tape rich projection both fall out of the SAME fold because they are the same IR
constructs with different rule shapes. `RuntimeEmitterKind` AND `BackendShape`-level dispatch
both vanish from the emitter (shape stays a lowering-internal cost fact).
- **Pro:** the strongest possible answer to addendum 3 — there is literally no fork to hide.
  Maximally honest (`generator_grammar_branch_count` trivially 0; `emitter_fork_present`
  structurally impossible). One IR, one walker, N grammars.
- **Con:** the largest scope and the highest >SOTA risk. The profile (`SYNTHESIS-PROFILE.md`
  §2/§3) shows the JSON win (91.5%, `parse_object_value_at_direct`) and CSS win (94.1%,
  `find_component_delim`) live in SPECIFIC inline-shaped hot bodies. A generic IR-fold that
  does not reproduce those exact `inline(always)` monomorphized-sink shapes regresses the
  >SOTA — the §6 honest-finding hazard. C demands the IR-walker emit grammar-identical hot
  bodies; that is a research question G1/G2 must first prove SOLVABLE before C is admissible.
- **Relocated-seam exposure:** NONE (no table, no spec, no enum). But the risk migrates to
  >SOTA-preservation, not honesty.

## 3. Recommendation — Candidate A (dispatch on the lowered `BackendShape`), evolving toward C

A is the right G3 target: it deletes `RuntimeEmitterKind` outright (abrogate-before-patch,
the L3/L4 default), reuses the ALREADY-PROVEN grammar-neutral `BackendShape` discriminator
that `select_lowering` (`lower/mod.rs:18`) demonstrates is Lock-14-clean, and keeps the
decision grammar-DERIVED (cost-model output over rule shapes, not a config field). It carries
the LOWEST relocated-seam risk because the discriminator leaves the `RuntimeTarget` struct
entirely — `runtime_target_rows_collapsed` becomes satisfiable as a direct consequence. B is
rejected as the PRIMARY architecture precisely because a `ProjectionSpec` IS the relocated-
seam shape the addenda were written to catch; its one good idea (roster + output-shape as
derived data) is ABSORBED into A by having each `BackendShape` renderer declare its own
roster + projection from the lowered program, not from xtask config. C is the correct END
state (no discriminator at all) but its >SOTA-preservation burden makes it a SK-V19 follow-on,
not a G3 deliverable; A's per-shape renderers are the seam along which C later collapses (when
the shapes' folds prove unifiable). The roster asymmetry (8 vs 5 files) is resolved under A by
the renderer-declares-roster rule, retiring `COMPILED_RUNTIME_FILES`/`REQUEST_FACTS_RUNTIME_FILES`
as per-arm constants.

## 4. The key risk — the relocated seam (a grammar-named branch hidden in a data table)

The dominant failure mode for ALL candidates: un-forking the VISIBLE enum while leaving a
per-grammar branch in a neutral-identifier data structure — the `RuntimeTarget` strategy
table (`regen_css.rs`/`main.rs`) or a `ProjectionSpec` selected by `profile` string. The arm-
census grep (`generator_grammar_branch_count`) is **syntactically incapable** of detecting
this (no `Json =>` arm exists in a neutral table, CH2 V3 §8.1). It is caught ONLY by the
STRUCTURAL `runtime_target_rows_collapsed` co-gate: all rows sharing one `grammar_name` must
be byte-identical except `output_dir`/`expected_files`. R16 (`SYNTHESIS-AUDIT-OVERFIT.md`
§5.5) sharpens this — the collapse check must recurse into BOTH nested structs:
`frontend_requirements` (`RuntimeTarget` field #11, `regen.rs:17`) AND `output_labels`
(`RuntimeTarget` field #12, `regen.rs:18`). (The #11/#12 ordinals are the `RuntimeTarget`
struct's, `regen.rs:6-19`; the same two structs appear as fields #3/#4 of `RuntimeProfileContract`
at `grammar_provider.rs:35-36` — do NOT conflate the two ordinal systems, the R16 numbering
hazard a1 §3 names.) The fix is preferably a `RuntimeTarget: PartialEq` full-row derive (today
it derives only `Clone,Copy,Debug`, `regen.rs:5`; +1 line — both nested struct TYPES already
derive `PartialEq,Eq` at `grammar_provider.rs:45`/`:91`). For R-A specifically: the un-forked emitter must
NOT select its output-shape from `target.profile`/`target.emitter`/`target.output_labels`; it
must read it from the lowered program. A relocated seam riding the per-profile columns
(`profile`/`source_inputs`/`fact_schema`/`output_plane`) passes md5-distinctness and the arm
census yet is a REJECT — md5-distinct is necessary-not-sufficient (`a1` §L2).

## 5. Prune / sequencing dependency

G3 entry-gates on **BOTH G1 AND G2 closed** (`SYNTHESIS.md` §5 dependency graph;
`a1` §"Dependency chain"): A consumes G1's `SinkOnlyExpr`-walking JSON projector AND G2's real
CSS lowering — without G2, A has no non-const CSS input and cannot un-fork (the const courier
cannot be a `BackendShape` renderer). **P4 (Lock-14 gate) MUST land BEFORE G2/G3**
(`SYNTHESIS.md` §5 fact 2) so a grammar-named branch cannot be re-introduced into the new
emitter undetected. **P3 (collapse the 7 CSS rows) is a hard predecessor of the
`runtime_target_rows_collapsed` co-gate** — A cannot prove un-fork while 7 decorative CSS rows
remain (G2 itself dual-entry-gates on P3, `SYNTHESIS-AUDIT-OVERFIT.md` §5.3). G3 in turn
BLOCKS PROVE (Sheets emits THROUGH the un-forked path, `SYNTHESIS.md:337`). Standing order:
P1..P5 (P4 before emitter) → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1. R-A is the G3 node; it
dispatches nothing until G1+G2+P3+P4 are green.
