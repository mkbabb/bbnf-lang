# SK-V6 Reinforcement Cohort B6 - Spec Edit Map And Handoff Packet Outline

Date: 2026-05-15
Workspace read: `/Users/mkbabb/Programming/bbnf-lang`
Repo edits made: none
Output artifact: `/tmp/skv6-B6-spec-edit-map.md`

## Sources Read

Reports:

- `/tmp/skv6-A1-asmjson-generalization.md`
- `/tmp/skv6-A2-dav1d-asm-process.md`
- `/tmp/skv6-A3-comparator-planes.md`
- `/tmp/skv6-A4-history-validated-invalidated.md`
- `/tmp/skv6-A5-general-grammar-abstraction.md`
- `/tmp/skv6-A6-host-asm-instruction-map.md`

Current docs:

- `restart/skinny/INDEX.md`
- `restart/skinny/SUBSTRATE.md`
- `restart/skinny/COMPILER.md`
- `restart/skinny/BENCH.md`
- `restart/skinny/WORKSPACE.md`
- `restart/ARCHITECTURE.md`
- `restart/MASTER-PLAN.md`
- `restart/HANDOFF.md`
- `skinny/REDRESS.md`

Working tree note: `git status --short` showed pre-existing modifications in
`skinny/crates/bbnf-bench/src/metadata.rs` and
`skinny/xtask/src/bin/capacity_probe.rs`. This B6 task did not touch them.

## Cohort Synthesis

B6 should fold the six A reports as one reinforcement packet with these binding
themes:

- No new BBNF directives and no new BIR variants. asmjson, dav1d/FFmpeg
  checkasm, and host ISA findings all route through existing facts, side
  tables, `BackendShape`, `DirectBuild`, `SimdScan`, and primitive admission.
- asmjson is not a strict Apple Silicon S anchor. It is an x86 AVX-512
  architecture reference and permissive flaw probe unless a strict,
  same-corpus, same-plane row is implemented.
- SOTA-beat must be same strictness, same output/workload plane, same corpus,
  same hardware class, same benchmark freshness, same ownership disclosure,
  and same measured scope.
- Bench row metadata must become schema v3 with row-level plane, strictness,
  API symbol, ownership, freshness, prevalidation, mutation, and S-anchor
  eligibility. Markdown must render from metadata, not hard-coded prose.
- SIMD/ASM admission must become upstream-style: scalar executable spec,
  forced feature-mask matrix, ABI checked-call shims for raw extern candidates,
  recoverable fault handling, central helpers, and same-wave consumers.
- The arbitrary-grammar route is fact-model cleanup: generated structural class
  tables, recognizer facts, cost facts, and DirectBuild field facts. Delete
  prototype JSON switches such as `shapes_for_json()`, `nominate_json()`,
  `StructuralAlphabet::json()`, and rule-name materialization matches.
- The current M5 Max lane is AArch64 NEON/AdvSIMD plus PMULL, DotProd, and
  possibly CSSC. Ordinary SVE/SVE2 is not exposed locally; SME/SME2 is
  research-only until transition/ABI costs are proven.

## Global Constraints For All Spec Edits

- Keep Lock 1 unchanged: structural projection is the tape when retained; no
  parallel sidecar substrate.
- Keep Lock 10 unchanged: Pratt, SIMD, backend shape, and ASM selection are
  mined/cost-derived, not syntax-directed.
- Keep Lock 14 unchanged: generic crates get grammar-neutral facts and
  generated data, not grammar-name branches.
- Keep Lock 16 consumed-primitive discipline: no orphan primitive lands on
  benchmark potential alone.
- Preserve the visible split between `semantic_full_digest_stressor` and
  `real_typed_struct`. The former is a maximal guard workload; the latter is
  the representative DirectBuild closure row.
- Do not reopen rejected routes without fresh before/after rows: sidecar
  structural parser, byte-class whitespace cursor, parser-local structural-mask
  cursor, sink-local decoded-stat helpers, quote-source streaming hash,
  parser-owned decoded scratch, byte-output `unescape_json_string`, semantic
  string facts for the digest stressor, hand-authored typed sink proof, global
  cap-16 string probe, or Track 2 array next-byte parity transfer.

## Exact Spec Edit Map

### 1. `restart/skinny/INDEX.md`

Edit sections:

- `## Four quadrants`
- `## What the skinny is testing`
- `## Cross-quadrant invariants`
- `## Open contradictions and skinny-specific deviations from V1`
- `## Decision protocol`
- `## Authority cross-references`

Required amendments:

- In `Four quadrants`, update the Bench ownership text from "three competitor
  baselines / three corpora" to the current five-plane split: retained DOM/tape,
  typed direct, semantic full digest stressor, native strict DOM references, and
  advisory/flaw probes.
- In `What the skinny is testing`, replace any stale "5 G / 12 direct miss" or
  triad-centered phrasing with the A4 current binding gate: retained parse has
  13 G rows and four A rows (`canada`, `mesh`, `marine_ik`, `numbers`);
  `semantic_full_digest_stressor` has four pass rows and 13 misses; representative
  `real_typed_struct` has `twitter` and `update_center` passes.
- Add a same-plane SOTA rule: no row can authorize SOTA-beat unless plane,
  strictness, corpus, ownership, freshness, hardware, and measured scope match.
- Add a row to `Cross-quadrant invariants`: grammar-specific behavior enters
  through grammar source, workspace metadata, generated data, recognizer/cost
  facts, or host/API schema facts; never via new directives, BIR variants, or
  generic-crate grammar switches.
- Add a row to `Cross-quadrant invariants`: raw ASM/SIMD can be selected only
  from admitted primitive rows with scalar spec, feature-mask matrix,
  checkasm/ABI coverage, and same-wave consumer.
- Add contradictions/deviations:
  - Comparator-plane drift: current docs mix strict DOM, typed direct, digest,
    On-Demand, SAX, native, and permissive rows in prose.
  - sonic-rs feature drift: BENCH documents `=0.5` / `sort_keys`; local current
    report says `=0.5.8` and warns `utf8_lossy` is ineligible for strict anchors.
  - Profile contradiction: WORKSPACE TOML shows `lto = "fat"` but profile prose
    still says thin LTO is intentional.
  - Lock 16 allowlist over-broadness: old rows must be tightened to admitted
    primitive families and exact feature names, not "AVX-512" as a bucket.
- In `Decision protocol`, insert a pre-implementation spec-fold gate: BENCH
  schema v3 and same-plane S-anchor rules must be folded before any new
  performance intervention is judged.
- In `Authority cross-references`, add the A1-A6 cohort reports as the current
  SK-V6 reinforcement source set and state that B6 is a spec-fold map, not an
  implementation result.

### 2. `restart/skinny/SUBSTRATE.md`

Edit sections:

- `## 0. Scope and stance`
- `### 1.5 Tape == structural projection`
- `### 1.6 Typed event cursor over tape projection`
- `## 3. SIMD scan integration contract`
- `### 3.1 The dispatch table`
- `### 3.2 The structural alphabet feed`
- `### 3.3 Prefilter vs verifier route`
- `### 3.4 Scalar parity hash`
- `### 3.5 Throughput targets`
- `### 3.6 Token-economy materialization gate`
- `## 4. Direct-to-struct overlay`
- `### 4.1 The typed-view shape for JSON`
- `### 4.2 Field-cache decisions`
- `## 8. Hand-coded JSON parity contract`
- `## 9. Module layout for the skinny`
- `## 10. Open questions surfaced for the orchestrator`

Required amendments:

- Update current-state numbers to A4: 13 retained G rows, four retained A rows,
  four digest pass rows, 13 digest misses, two representative typed passes.
- In `1.5`, keep structural projection == tape, but generalize from JSON
  offsets to A5 data products:
  `StructuralClassTable`, `EventProjectionPlan`, `PayloadFlagPlan`, and
  `TapeBuilderPlan`. These are facts/data, not a side substrate.
- In `1.6`, clarify that parser consumption must use the single projection;
  parse-time aux columns and parser-local mask cursors remain rejected unless
  cost-selected `EventTape` pays for compact event cells.
- In `3`, add primitive admission as normative substrate language:
  scalar reference, checkasm/parity, forced feature-mask matrix, ABI checked-call
  row for raw extern/ASM, recoverable fault reporting, destination/source guard
  checks, and same-wave runtime/generated consumer.
- In `3.1`, replace scattered feature detection with table-first dispatch:
  build `PrimitiveKernels` once, start scalar, override by runtime feature group,
  and expose a test-only feature mask hook.
- In `3.2`, replace JSON structural alphabet wording with generated structural
  class tables from grammar facts. JSON's `{ } [ ] , : "` is one generated
  table, not a generic constant.
- In `3.2`/`3.3`, name two scan products:
  - strict structural-only scan for microbench floors;
  - parser-grade parse index/event feed with quote, escape, delimiter, invalid,
    and follow-set facts only when the parse row pays for them.
- In `3.4`, extend scalar parity hash language to include feature tier,
  forced mask, primitive admission status, and checkasm status in metadata.
- In `4`, replace JSON-specific direct-output prose with DirectBuild field-fact
  minimums from A5: source ref, target field id/path/type, cardinality,
  presence/null/default, duplicate/unknown policy, representation, materializer,
  error policy, diagnostic context.
- In `4.1`/`4.2`, keep JSON examples but label `JsonString`/`JsonNumber`
  materializer names as prototype markers; the generic contract is
  `EscapedString`, `NumberScalar`, `Literal`, and `Raw`.
- In `8`, make Track 2's contract explicitly independent but same-substrate:
  it may use BENCH-side JSON code, but it must consume runtime tape/event/sink
  APIs and cannot import generated Track 1 helpers.
- In `9`, add planned test/support layout for `bbnf-simd/tests/` and a
  primitive admission manifest if WORKSPACE owns the file tree there.
- In `10`, add A6 host-priority open questions:
  retained Unicode-escape scalar split before NEON validator; PMULL prefix-XOR
  only with attribution; CSSC bit iteration inspection; DotProd digit chunks;
  no SME/SVE2 parser target on current Apple host.

### 3. `restart/skinny/COMPILER.md`

Edit sections:

- `### 1.1 Skinny grammar text`
- `### 1.2 Regex literals JSON exercises`
- `### 1.3 @host fn decision`
- `### 3.1 Exercised variants`
- `### 3.3 Lowering matrix per LayoutFacts.backend_shape`
- `### 4.4 LayoutFacts in the skinny`
- `## 5. Pipeline Subset`
- `### 5.3 Skipped phases and per-skip impact`
- `### 5.4 Hand-curated recognizer`
- `### 5.5 Hand-curated shapes`
- `## 6. codegen::rust Path`
- `### 6.1 Per-BIR-variant lowering`
- `### 6.2 Emitted file shape`
- `### 6.3 Emitted parser entry sketch`
- `### 6.5 parse-attribution feature flag`
- `## 7. What's Stubbed In The Skinny`
- `## 8. The Compile-And-Test Loop`
- `## 9. Open Questions And Source-Authority Conflicts`
- `## 10. Summary`

Required amendments:

- Keep JSON grammar semantics unchanged, but state that ASM-style whitespace
  and event seeking is a lowering optimization, not a grammar-semantics change.
- In `3.1`, replace the single hard-coded `StructuralAlphabet { ... }`
  description with `StructuralClassTable` / `DelimitedRegionPlan` /
  recognizer facts. JSON punctuation becomes generated data.
- In `3.3`, add chunk-classified automaton lowering:
  classify bytes into grammar-defined layout/terminator/escape/delimiter/invalid
  masks, use `ctz`/equivalent to seek, and compile state transitions from grammar
  facts. This is a backend lowering option, not a BIR variant.
- In `3.3`, add ASM selection rules: `LayoutFacts.backend_shape` and `CostFacts`
  decide legal primitive families; generated hot loops call selected table
  entries; per-call feature detection is forbidden; missing admitted bodies fall
  back to scalar or existing not-viable diagnostics.
- In `4.4`, add or extend `CostFacts` / `CostDecision` evidence shape:
  selected shape, rejected alternatives, dominated alternatives, objective
  vector, scalarization profile, target ISA/profile, extraction method, and
  benchmark/static evidence.
- In `5.3`-`5.5`, mark `shapes_for_json()` and `nominate_json()` as deletion
  targets, not architecture. Required replacements:
  `derive_shape_facts(grammar, schemas)`,
  `nominate_recognizers(grammar, metadata)`, generated structural alphabets, and
  direct field facts resolved to ids.
- In BIR extraction text, replace literal rule-name switches (`object`, `array`,
  `string`, etc.) with resolved facts keyed by `RuleId`, `BindingId`, and
  field/source ids.
- In `6`, replace `emit_json_*` framing with generic `emit_grammar_*` /
  per-grammar generated modules under `runtime/src/grammars/<name>/`; runtime
  does not depend on benchmark structs.
- In `6.1`, extend `DirectBuild` lowering to consume DirectBuild field facts
  and host/API schema facts. `SinkOnlyProgram` must preserve facts through
  codegen.
- In `7`, update the cost-model stub row to say the stub is masking until
  materialization mode, primitive choice, direct field layout, and event
  consumption alternatives are measured.
- In `8`, add future static gates: `cargo xtask lint-grammar-generalization`,
  import-deny for codegen lowerers, BIR snapshot with no new variants, generated
  header hashes for grammar/metadata/schema/recognizer/cost decision.
- In `9`, add conflict rows for sonic-rs strictness/feature drift only if not
  entirely owned by BENCH; otherwise cross-reference BENCH as owner.

### 4. `restart/skinny/BENCH.md`

Edit sections:

- `## §2 Comparator baselines and workload planes`
- `### 2.1 sonic-rs`
- `### 2.2 simd-json`
- `### 2.3 serde_json`
- existing yyjson / asmjson / "not in competitor set" subsections
- competitor configuration table
- `## §5 Reproducibility schema`
- `### 5.1 Required fields`
- `### 5.1.1 Strictness disclosure columns`
- `### 5.2 Capture mechanism`
- `### 5.3 Schema enforcement gate`
- `## §6 Go/no-go threshold matrix`
- `### 6.1 The full matrix`
- `### 6.1.1 G-fusion-quality two-pathology-class taxonomy`
- `### 6.2 Reading the matrix`
- `### 6.2.1 Classification order`
- `### 6.2.2 Workload split for current NO-GO rows`
- `## §7 Criterion harness layout`
- `### 7.1 Bench groups`
- `### 7.6 Cargo.toml entry`
- `### 7.8.2 Alternate-plan probes`
- `### 7.9 Correctness gates`
- `### 7.10 Comparative-profile primitive`
- `## §10 Verdict-writing template`
- `## §11 Crate ownership`
- `## §12 Failure modes and mitigations`
- `## §13 Open questions and contradictions in source authority`
- `## §15 Bench harness scope summary`

Required amendments:

- Replace `§2` opening with the A3 five-plane split:
  Rust retained DOM/tape, Rust typed direct, semantic full digest stressor,
  native strict DOM reference, and advisory/flaw-probe.
- Fix subsection numbering: current `2.4` and `2.5` are duplicated after asmjson.
- Replace sonic-rs block:
  use `sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys"] }`;
  rows with `utf8_lossy`, unchecked APIs, or invalid-input-tolerant paths are
  `lossy_utf8` / `permissive` and `s_anchor_eligible=false` unless row-local
  probes prove strictness.
- Replace simd-json block:
  use `simd-json = { version = "=0.13.11", default-features = false, features = ["serde_impl"] }`;
  keep borrowed and owned rows separate and record in-situ mutation.
- Add simdjson C++ before yyjson:
  DOM is native strict reference when rerun on exact corpora; On-Demand is
  `partial_ondemand` unless explicitly full-walk.
- Replace yyjson block:
  default immutable DOM can be a strict native reference; permissive flags and
  in-situ modes are separate rows; stale profiles cannot classify current gate.
- Replace asmjson block:
  x86 AVX-512 architecture reference / permissive flaw probe; no Apple Silicon
  strict S anchor by default.
- Replace "not in competitor set" with "not a Rust in-process gate row".
- Replace competitor configuration table with A3 row-level columns:
  `Row`, `API symbol`, `Plane`, `Strictness`, `Input ownership`,
  `Output ownership`, `S-anchor eligible`.
- Bump metadata schema to v3 and add required fields:
  `api_symbol`, `plane`, `strictness`, `validation_boundary`,
  `escape_completeness`, `flaw_probe`, `input_ownership`,
  `output_ownership`, `input_mutated`, `clone_charged`,
  `prevalidation_charged`, `sidecar_freshness`, `s_anchor_eligible`,
  `selected_feature_tier`, `forced_feature_mask`,
  `primitive_admission_status`, `checkasm_status`.
- Replace strictness values `strict/permissive/deferred` with A3 taxonomy:
  `strict_bytes`, `strict_after_utf8_view`, `strict_fullwalk_ondemand`,
  `partial_ondemand`, `lossy_utf8`, `permissive`, `unknown`.
- Update `RowMetadata` sketch and gate behavior:
  missing schema v3 fields fail before threshold classification; report renderer
  reads metadata rather than hard-coding strictness/output text.
- In `§6`, compute S only from rows where `s_anchor_eligible=true`, plane
  matches, strictness is compatible, and freshness is current enough. Track 2
  substrate beat is not final SOTA-beat unless Track 1 also beats the same
  eligible anchor.
- Rename or annotate `A - Beat-and-parity` / `Outcome::ABeatAndParity` as
  substrate-beat-with-codegen-parity unless Track 1 independently satisfies
  same-plane SOTA-beat.
- In `6.1.1`, demote old two-pathology kernel prescription from canonical fix
  to diagnostic-only unless fresh `parse-attribution` proves the leaf.
- In `7.1`, extend groups conceptually to 17 corpora and to direct/typed/direct
  workload tables; do not leave the sketch as twitter/citm/canada only.
- In `7.6`, remove `utf8_lossy` from sonic-rs strict rows or label the row
  ineligible.
- In `7.8.2`, add primitive-admission and feature-mask probes as report-only
  diagnostics before end-to-end credit.
- In `7.9`, align bbnf retained rows as `strict_after_utf8_view` unless a timed
  byte-entry UTF-8 row includes validation inside measured scope.
- In `§10`, update RESULTS template to render schema v3 fields and split
  `semantic_full_digest_stressor` from `real_typed_struct`.
- In `§11`, review `metadata.rs`, `report.rs`, and `gate.rs` LOC caps because
  schema v3 and plane rendering will expand those files.
- In `§12`, add failure modes:
  comparator-plane drift, lossy/unchecked comparator API drift, feature-mask
  dispatch drift, raw ASM without ABI checked-call row, and stale native
  sidecar used as current classifier.
- In `§13`, close or rewrite sonic-rs feature flag conflict using A3; update
  simdjson C++ wording from "not in competitor set" to native reference plane.

### 5. `restart/skinny/WORKSPACE.md`

Edit sections:

- `### 0.1 Post-Iteration State`
- `## 1. Skinny Crate Set`
- `## 2. Per-Crate LOC Budget`
- `## 3. Workspace Cargo.toml`
- `### 3.1 Profile Discipline`
- `## 4. Directory Layout`
- `### 4.8 crates/bbnf-simd`
- `### 4.9 crates/bbnf-bench`
- `## 5. Build And Test Commands`
- `## 6. The xtask Runner`
- `## 7. Stub Policy For Skipped Crates`
- `## 8. Migration Parity`
- `### 8.1 Mechanical Closure Of Skinny Deviations`
- `## 9. Build-Time Targets`
- `## 10. What The Skinny Omits`
- `## 11. Closure And Open Contradictions`

Required amendments:

- In `0.1`, update stale measured state to A4:
  13 retained G rows; four retained A rows; four direct digest pass rows; 13
  direct digest misses; representative typed rows pass for `twitter` and
  `update_center`.
- In crate set, update `bbnf-simd` role to table-first primitive kernel
  dispatch plus scalar/SIMD/ASM admission; add forced feature masks and
  checked-call ABI coverage as workspace gates.
- In `bbnf-bench` role, update to schema v3 metadata, row-level plane
  selection, and same-plane S-anchor filtering.
- In LOC budget, review `bbnf-bench` and `bbnf-simd` caps after schema v3 and
  checkasm hardening. If cap stays unchanged, `lint-loc` must report a named
  budget-cliff warning before implementation begins.
- In workspace TOML/profiles, resolve the contradiction:
  TOML currently uses `lto = "fat"` for release/bench, while `3.1` and `9`
  claim thin LTO is intentional. Align with Lock 15 and current TOML, or
  explicitly record a measured exception. Do not leave both statements.
- In `4.8`, expand bbnf-simd layout to include or reference:
  `tests/checkasm_common.rs`, test-only checked-call shims, a dispatch table,
  and a primitive admission manifest. Keep `src/` child count within Lock 13.
- In `4.9`, make `metadata/`, `gates/`, and `report/` own schema v3 fields and
  plane rendering.
- In `5`, add commands:
  `cargo run -p xtask -- primitive-checkasm`,
  `BBNF_SIMD_FORCE=scalar|neon|dotprod|i8mm|sve|sve2|swar|avx2|avx512icl ...`,
  `cargo xtask lint-grammar-generalization`,
  `cargo xtask check-conformance`,
  and a native `RUSTFLAGS="-C target-cpu=native"` bench command for binding
  performance reruns.
- In `6`, update the xtask sketch to include `check-conformance`,
  `primitive-checkasm`, `gate-json`, and schema v3 validation. The current sketch
  lists some subcommands in prose but not in the match arms.
- In `7`, update cost-model shim language: selected-only static choices are
  masking until CostFacts record rejected/dominated alternatives and objective
  vectors.
- In `8.1`, add deviations:
  schema v3 bench metadata, raw ASM admission discipline, table-first dispatch,
  host/API output schema source for DirectBuild, and no-SVE/SME-current-host
  lane.
- In `10`, update host-fn omission: eager parse-time decode is MASKING; V1 JSON
  must keep decode lazy for SOTA rows. CSS/Sheets host-call costs remain separate
  V1 gates.
- In `11`, add contradictions:
  sonic-rs feature drift, comparator-plane drift, fat/thin profile text drift,
  stale G-row counts, and bbnf-simd admission harness still short of
  FFmpeg-equivalent ABI/fault coverage.

### 6. `restart/ARCHITECTURE.md`

Edit sections:

- `## 0. Authority And Conflict Ledger`
- `## 5. Cargo And Workspace Metadata`
- `### 5.6 Declaration-Crate Fence`
- `## 7. IR Contract`
- `### 7.2 Backend IR`
- `### 7.3 Side Tables`
- `### 7.4 SK-V5 / SK-V6 Implementation Status`
- `### 7.5 Diagnostic Vocabulary`
- `### 7.6 Backend Trait`
- `## 9. Runtime Architecture`
- `### 9.1 Tape`
- `### 9.2 Direct-To-Struct Union`
- `## 10. Codegen And Lowerers`
- `### 10.1 Rewrite-Budget Categories And Thresholds`
- `## 11. Performance Targets`
- `## 12. Future Grammar Onboarding Test`
- `### 12.1 YAML Onboarding Walkthrough`
- `## 13. File And Directory Discipline`
- `### 13.1 Lint Manifest`

Required amendments:

- In `0`, add a conflict-ledger row: native/advisory comparator rows cannot
  classify Rust strict gates unless same-plane and current. asmjson is
  permissive/advisory by default.
- In `5`, add host/API output schema facts as allowed metadata/data source for
  DirectBuild when grammar shape alone does not imply target output type.
- In `7.2`, clarify `SimdScan` products:
  transient mask stream, strict structural-only row, and parser-grade parse
  index/event facts are distinct products. Retained scan facts are the tape
  projection when retained, not a sidecar.
- In `7.3`, extend side tables with:
  structural class tables, recognizer verifier routes, direct field facts, and
  CostFacts evidence shape from A5.
- In `7.3 BackendShape`, update `CollapsedStage` selection language:
  it may choose only admitted primitive rows; ISA selection derives from feature
  masks and target planning; missing ASM is scalar fallback or a viability
  diagnostic, not semantic difference.
- In `7.4`, fold A4/A5 current status:
  symbol state exists; remaining blockers are measured cost selection,
  generated/hand Track 2 split, direct field facts, and Lock 14 cleanup. List
  JSON-specific waivers as deletion targets.
- In `7.5`, add diagnostic codes:
  `BBNF-COMPARATOR-PLANE-DRIFT`,
  `BBNF-LOSSY-UTF8-ANCHOR`,
  `BBNF-SIMD-PRIMITIVE-NOT-ADMITTED`,
  `BBNF-SIMD-FEATURE-MASK-DRIFT`,
  `BBNF-ASM-ABI-CHECK-MISSING`,
  `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` if not already fully catalogued,
  and `BBNF-COST-EVIDENCE-INCOMPLETE`.
- In `9.2`, replace/extend direct-to-struct with A5 authority boundary:
  retained direct views, SinkOnly direct outputs, and host/API typed outputs
  all lower from `DirectBuild { shape, fields }` facts over accepted event
  stream. `semantic_full_digest_stressor` remains a guard, not representative
  typed output.
- In `10.1`, make cost-driven rewrites consume CostFacts with selected,
  rejected, dominated, objective vector, scalarization profile, target/profile,
  and extraction method.
- In `11`, replace generic SOTA metadata wording with schema v3 fields and
  same-plane SOTA classification. Native yyjson/simdjson C++ are final native
  reference ceilings only when current same-corpus metadata exists.
- In `12`/`12.1`, require future grammar onboarding to prove generated
  structural/event data and direct schema facts without generic-crate changes.
- In `13.1`, add lint targets for `shapes_for_json`, `nominate_json`,
  `StructuralAlphabet::json`, `emit_json_*` in generic code, and rule-name
  materialization switches outside generated grammar modules.

### 7. `restart/MASTER-PLAN.md`

Edit sections:

- `## 4. Hard Architectural Gates`
- exact SOTA close-row table under `## 4`
- `## 13. Tranche H - Pratt, SIMD, typed-event codegen`
- `### §13.1 Admissible SIMD primitives`
- `## 21. Lock Ownership`
- `## 23. Risk Register`
- `## 24. Carry And Friction Ledger`
- `## 25. Implementation Order`
- `## 27. Phase 8.4 Simplification Fold Ledger`

Required amendments:

- After the SOTA close-row table, add A3 same-plane classification text:
  SOTA-beat requires matching strictness, output/workload plane, corpus,
  hardware class, benchmark freshness, ownership disclosure, and measured
  scope. Track 2 beating S is substrate headroom unless Track 1 also beats the
  eligible anchor.
- Replace `json/direct_to_struct` row with
  `json/semantic_full_digest_stressor` wording from A3: guard workload, not
  representative typed close, with misses requiring falsified REDRESS routes or
  an explicit no-real-consumer decision.
- Replace `json/real_typed_struct` row with representative DirectBuild wording:
  same declared host/API schema, same field ownership policy, Track 1 generated
  DirectBuild output before checksum, Track 2 separate oracle, serde_json Value
  only for proven null-only fields.
- Replace H tranche goal paragraph:
  primary close is arm64 Apple Silicon; x86 AVX-512 is secondary; asmjson is
  architecture/flaw-probe until strict same-plane rows exist.
- Update H.W2 and H.W2.5 with A2/A6:
  table-first dispatch, feature masks, ABI checked-call, recoverable faults,
  primitive admission manifest, same-wave consumers.
- Update H.W4 with A5:
  DirectBuild field facts and host/API schema-source facts, not hand-authored
  sinks or benchmark-private parsers.
- Replace H.W6 row with A3 strict matrix target:
  full 17-corpus matrix with row-level `plane`, `strictness`, `api_symbol`,
  ownership, prevalidation, mutation, freshness, and S-anchor eligibility.
- Rewrite `§13.1` Lock 16 allowlist from "verbatim" into "admissible
  vocabulary plus admission requirements" or add a new subsection immediately
  after it. Correct A6 feature taxonomy:
  `vpshufbitqmb` is AVX512BITALG; `vpermb` is AVX512VBMI; `vpcompressb` is
  AVX512VBMI2; `vpcompressd` is AVX512F; GFNI only when predicate is actually
  affine-encodable; Apple host is NEON/PMULL/DotProd/CSSC, not SVE/SME parser
  target.
- Add Lock 15 and Lock 16 rows to `## 21 Lock Ownership`; current table visibly
  lists only 1-14 before narrative says locks 15-16 exist.
- Add risk rows:
  comparator-plane drift, primitive admission drift, feature-mask drift,
  JSON fact-model overfit, host feature overclaim, and stale sidecar classifier.
- Add carry rows:
  benchmark schema v3, primitive admission manifest, host/API DirectBuild schema
  facts, grammar-neutral recognizer/fact replacement, and AArch64 host-lane
  priority.
- In implementation order, add a spec-fold step before any new implementation
  wave: update docs/schema authority, then code.

### 8. `restart/HANDOFF.md`

Edit sections:

- Header status block
- `Reading order for the next implementation agent`
- `## §3 - Current state`
- `## §4 - Cross-parser landscape on M5 Max`
- `## §5 - The greater-arch generalization`
- `## §6 - Wave dispatch posture (SK-V6)`
- `## §6a - Close condition`
- `## §7 - Verification rituals`
- `## §8 - Voice + discipline locks`
- `## §9 - Closing posture`

Required amendments:

- Update date/status to 2026-05-15 reinforcement fold-back after A1-A6/B6.
- Add reading order entry for A1-A6 reports and this B6 spec edit map. If these
  reports are later copied into `restart/skinny/tranches/sk-v6/research/`, update
  paths from `/tmp`.
- In current state, keep A4's binding gate numbers and add representative typed
  output split.
- In cross-parser landscape, remove language that lets asmjson's 10.93 GiB/s
  act as Apple Silicon S anchor. It is x86 architecture/flaw-probe unless strict
  same-plane row exists.
- In greater-arch generalization, fold A5: all grammar-specific differences are
  generated data/facts; current JSON-specific generic functions are deletion
  targets.
- In wave dispatch, insert a B6 spec-fold wave before further interventions:
  BENCH schema v3 and same-plane S-anchor selection must land before more
  candidate performance claims are classified.
- In close condition, replace "strictness/output-plane columns" with schema v3
  required fields and S-anchor eligibility.
- In verification rituals, add commands/checks for:
  schema v3 metadata validation; no `utf8_lossy` in strict sonic-rs rows;
  `BBNF_SIMD_FORCE` matrix; primitive admission manifest; no JSON names in
  generic crates.
- In closing posture, add the A6 host-lane priority and SME/SVE2 caution.

### 9. `skinny/REDRESS.md`

Edit sections:

- Append after `## SK-V6 Wave 2 Candidate-14 Redress`
- Optionally add a short cross-reference in the top `Current Bench Fact` section

Required amendment:

- Add a new non-implementation redress item, e.g.
  `## SK-V6 Reinforcement Cohort B6 Spec Fold-Back`.
- Record that A1-A6 are spec-authority reinforcements, not measured code
  candidates.
- Summarize:
  - A1: asmjson is architecture/flaw-probe; no directive/BIR addition; strict
    comparator rows required.
  - A2: checkasm/dispatch admission must become FFmpeg/dav1d-style with
    feature masks and ABI shims.
  - A3: BENCH schema v3 and same-plane SOTA selection are required.
  - A4: current gate and rejected-route ledger remain binding.
  - A5: grammar-neutral fact model replaces JSON switches.
  - A6: M5 Max lane is NEON/PMULL/DotProd/CSSC; SVE/SME not current parser
    target.
- State explicitly that this item authorizes docs/schema updates only. It does
  not admit a new primitive, performance candidate, BIR variant, directive, or
  substrate.

## Implementation Handoff Packet Outline

The implementation packet should be a separate file, probably under
`restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V6-B6.md` or the existing
SK-V6 cohort directory once reports leave `/tmp`. It should be organized as
follows.

### 0. Packet Header

- Date and branch.
- Current gate summary from `skinny/RESULTS.md`.
- Authority chain: `skinny/RESULTS.md`, `skinny/REDRESS.md`, A1-A6, B6 edit
  map, SK-V5 packet history.
- Non-goals: no directives, no BIR variants, no side substrate, no orphan
  primitive, no asmjson strict anchor claim.
- Dirty-worktree rule: preserve unrelated staged/dirty files; inspect
  `git status --short` before staging.

### 1. Implementation Wave 0 - Spec Fold

Purpose: update docs and schema authority before code claims.

Files:

- `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE}.md`
- `restart/{ARCHITECTURE,MASTER-PLAN,HANDOFF}.md`
- `skinny/REDRESS.md`

Exit gates:

- Every doc uses the same current gate numbers.
- BENCH names schema v3 and same-plane selection.
- MASTER/ARCH/HANDOFF no longer treat asmjson as Apple Silicon strict S anchor.
- WORKSPACE profile prose matches TOML or records a measured exception.
- No doc says a new directive or BIR variant is needed.

### 2. Implementation Wave 1 - Bench Metadata v3

Purpose: make comparator planes executable.

Code surfaces:

- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/competitors/*`
- `skinny/crates/bbnf-bench/Cargo.toml`

Required work:

- Bump `SCHEMA_VERSION` to `3`.
- Add row fields listed in BENCH edit map.
- Remove `utf8_lossy` from strict sonic-rs rows or mark those rows
  `lossy_utf8` and ineligible.
- Render strictness/output/ownership/freshness from metadata.
- Select S only from eligible same-plane rows.
- Distinguish substrate beat from Track 1 SOTA-beat in gate output.

Tests/gates:

- Unit tests for required-field rejection.
- Rendered report snapshot with retained, typed direct, digest, native reference,
  and advisory/flaw-probe rows.
- Fixture row missing `plane` or `api_symbol` must fail schema before
  classification.

Stop conditions:

- Any hard-coded `strict`, `deferred`, or output-plane string in `report.rs`
  that is not read from metadata.
- Any strict S anchor using `utf8_lossy`, unchecked API, stale native profile,
  partial On-Demand, or permissive asmjson row.

### 3. Implementation Wave 2 - Primitive Admission And Dispatch Hardening

Purpose: upgrade bbnf-simd from parity tests to admission discipline.

Code surfaces:

- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/{scalar,aarch64,x86_64}/`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- new test-only checked-call shim sources if needed
- `skinny/xtask/src/bin/*` or `xtask` subcommands for `primitive-checkasm`

Required work:

- Build `PrimitiveKernels` once; start scalar; override by runtime feature
  group.
- Add `BBNF_SIMD_FORCE` and mask controls for scalar/default/tier testing.
- Centralize guards, seed logging, alignment windows, robust timing, and
  feature-mask runner.
- Replace Rust-closure register sentinels with raw extern checked-call shims.
- Replace panic-from-signal handler with recoverable fault reporting, or mark
  crash isolation partial until implemented.
- Add primitive admission manifest fields.

Tests/gates:

- Forced scalar/default/host-supported tier rows for admitted primitives.
- Destination/source guard tests for mutating outputs.
- ABI checked-call tests for raw ASM/extern candidates.
- Manifest gate rejects admitted primitive without scalar spec, checkasm,
  feature row, and consumer.

Stop conditions:

- Public wrapper chooses AVX-512 or optional AArch64 feature only by compile-time
  target feature.
- A primitive is admitted with no same-wave runtime/generated consumer.
- Register-sentinel closure tests are cited as ASM ABI proof.

### 4. Implementation Wave 3 - Fact Model And Lock 14 Cleanup

Purpose: remove JSON prototype switches from generic compiler crates.

Code surfaces:

- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/passes/src/shapes/*`
- `skinny/crates/passes/src/recognizers/*`
- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/*`

Required work:

- Replace `shapes_for_json()` with `derive_shape_facts(grammar, schemas)`.
- Replace `nominate_json()` with `nominate_recognizers(grammar, metadata)`.
- Replace `StructuralAlphabet::json()` with generated class/alphabet data.
- Replace rule-name materialization switches with resolved ids and direct facts.
- Add generated header hashes for grammar, metadata, schema, recognizers, and
  cost decisions.

Tests/gates:

- `cargo xtask lint-grammar-generalization` rejects JSON names in generic crates.
- BIR snapshots unchanged in variant alphabet.
- Add a tiny non-JSON grammar fixture that exercises metadata-only generated
  structural data without touching generic code.

Stop conditions:

- Any new BIR variant/directive proposed to carry schema or SIMD facts.
- Any new generic crate branch on `json`, `object`, `array`, `string`, or
  comparable grammar names.

### 5. Implementation Wave 4 - DirectBuild Schema Facts

Purpose: make representative typed direct output grammar-general.

Code surfaces:

- `skinny/crates/ir/src/lib.rs` DirectBuild payload facts
- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- metadata/schema loaders for host/API output shape

Required work:

- Add direct field fact payloads in place; do not add a BIR variant.
- Feed host/API schema facts into DirectBuild for `real_typed_struct`.
- Generate typed parser calls/local fields/seen bitsets/repeated/map materializers.
- Keep digest stressor visible and separate.

Tests/gates:

- Track 1 generated typed DirectBuild reaches generated code path.
- Track 2 remains structurally independent.
- sonic-rs typed and serde_json typed rows share schema and ownership policy.
- Broad `serde_json::Value` allowed only for proven null-only fields.

Stop conditions:

- Benchmark-private Track 1 parser.
- Parse-time checksum-only sink used as typed-output proof.
- Digest stressor pass/fail used to hide representative typed-output status.

### 6. Implementation Wave 5 - Host-Local Primitive Probes

Purpose: test A6 priorities only after attribution proves need.

Candidate order:

1. Retained Unicode-escape validation scalar split; only then NEON x4 validator.
2. PMULL prefix-XOR for `bitmap_prefix_xor_64` if scan/string-region attribution
   is visible.
3. CSSC-aware bit iteration and bulk position emit with asm inspection.
4. DotProd digit chunks for number-heavy rows if `match_number_at_digit` or
   direct number arrays own time.
5. New plain-string classification only with a narrower local fact than the
   rejected wide scanner family.

Required evidence before coding:

- Fresh `parse-attribution` profiles on current generated Track 1.
- Same-HEAD baseline/candidate measurement.
- Guard-row stop thresholds named before benchmark.
- Checkasm parity plan per primitive.

Stop conditions:

- Raw UTF-8 SIMD fusion for current retained generated JSON without profile
  evidence.
- SME/SVE2 implementation on this Apple host without a separate transition/ABI
  proof.
- Reopening direct byte-output unescape or parser scratch under the same output
  contract rejected by REDRESS 66-69.

### 7. Implementation Wave 6 - Report, Redress, And Handoff Close

Purpose: make results reviewable and prevent ghost authority.

Required outputs:

- Updated `skinny/RESULTS.md` from schema v3 renderer.
- `skinny/REDRESS.md` item for every admitted/rejected candidate.
- Handoff update with current state and next receiver.
- If commits are requested, use local commit discipline; preserve unrelated
  staged work.

Close gates:

- `git status --short` reviewed before final.
- `cargo xtask gate-json --advisory` or documented reason it was not run.
- Schema v3 rows present for every row in the rendered report.
- No report row claims SOTA-beat from ineligible comparator plane.

## Minimum Acceptance Checklist For The Future Packet

- [ ] Current gate numbers match A4 and latest `skinny/RESULTS.md`.
- [ ] BENCH schema v3 fields are documented before code relies on them.
- [ ] `sonic-rs` strict rows do not use `utf8_lossy`, or are marked ineligible.
- [ ] asmjson rows are advisory/permissive unless strict same-plane data exists.
- [ ] Primitive admission requires scalar spec, forced mask, checkasm, ABI row,
      consumer, and status.
- [ ] Generic crates contain no JSON-named fact derivation after Lock 14 cleanup.
- [ ] DirectBuild uses schema/fact payloads, not new syntax.
- [ ] AArch64 host lane is NEON/PMULL/DotProd/CSSC; SVE/SME is not a current
      parser target.
- [ ] Every rejected SK-V6 candidate remains rejected unless a new measured row
      overturns it.
