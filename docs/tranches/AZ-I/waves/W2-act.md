# AZ-I.W2-act — GESTALT-ACTIVATE (JSON + Sheets + CSS L4 + close)

**Opens after**: AZ-I.W2 substrate (master `6f78c1ef`).
**Agents**: 5 (1 sequential prelude → 3 parallel grammar activators → 1 close).
**Hard gate**: every grammar in JSON / Sheets / CSS L4 emits into struct
builders only; tape path severed on those three grammars' hot paths;
17-entry matrix at AU floor on struct-only path; sonic-rs / simdjson
OnDemand / serde_json / lightningcss / cssparser parity harnesses green;
`crates/tape/` reachable only from the BBNF bootstrap consumer; AZ-I
FINAL.md lands; `docs/benchmarks/post-AZ-I.json` archived.
**Status**: in_progress (2026-04-28)

W2-act is the activation pass that closes AZ-I. Per
`audit/W2-CLOSE-AUDIT.md` §2 the W2 substrate is grammar-general — nine
per-shape struct-direct emitters, the `EmitStrategy` resolver, the
`<'p>`-threaded dispatcher, the JSON runtime, the parity harness
scaffold — so the per-grammar resolver-arm flip activates JSON + Sheets
+ CSS L4 in one wave rather than three. W4 FINAL absorbs into this
wave's close ceremony.

## Scope

1. Hoist `EmitStrategy` from `crates/core/src/backend/rust/emitter/
   strategy.rs` to `crates/ir/src/registry/strategy.rs` per
   `audit/AUDIT-6-ARCHITECTURE.md` §8.1 — the substrate-selection
   decision is backend-shared, not Rust-specific. The variant carries a
   `SubstrateBinding` record so TS / WASM backends consume the same IR-
   level decision.
2. Author the `JsonDocument` view / value / path-prep accessor API
   matching today's `Parsed<JsonGrammar>` surface
   (`view()` / `to_value()` / `get::<T>(path)`). Migrate the three
   broken consumers (`tests/json_slab.rs`,
   `tests/projection_totality.rs`, `tests/typed_accessor_surface.rs`)
   from `parsed.view()` to the `JsonDocument` accessor.
3. Activate JSON: `EmitStrategy::for_grammar` resolves
   `JsonParser`/`JsonGrammar` to `StructDirect`; regen
   `crates/core/src/grammar/generated/json.rs`; recode the sonic-rs +
   simdjson OnDemand + serde_json parity harnesses against
   `JsonDocument`; bench gate (twitter ≥ 1967, canada ≥ 1231,
   citm ≥ 2438) on the struct-only path.
4. Activate Sheets: author `crates/core/src/runtime/google_sheets/`
   (`SheetsValue`, `SheetsCell`, `SheetsFormula`, `SheetsArena`,
   `SheetsStructBuilder`, `SheetsDocument`); resolver-arm extension for
   `GoogleSheetsParser`; regen
   `crates/core/src/grammar/generated/google_sheets.rs`; recode the
   Sheets parity harness; bench gate (parse_simple ≥ 95) on the
   struct-only path.
5. Activate CSS L4: author `crates/core/src/runtime/css_l4/`
   (`CssTypedValue` enum carrying `Length` / `Color` / `Dimension` /
   `Time` / `Resolution` / `Percentage` / `Angle` typed enums;
   `CssRule`, `StyleRule`, `MediaRule`, `KeyframesRule`, `Selector`,
   `Declaration`, `StyleSheet`; `CssArena`, `CssStructBuilder`,
   `CssDocument`); resolver-arm extension for `CssL4Parser`; regen
   `crates/core/src/grammar/generated/css_l4.rs`; lightningcss +
   cssparser parity recode + green node-for-node on the full CSS L4
   corpus; bench gate (normalize ≥ 735, bootstrap ≥ 600,
   tailwind ≥ 500) on the struct-only path.
6. Decay deletions in-wave (per `audit/W2-CLOSE-AUDIT.md` §4):
   - `crates/json-prototype/` retire (workspace-member demotion to
     `crates/core/benches/json-prototype/`).
   - `audit_payload_coverage` wire-or-delete per `AUDIT-2` §6.B (wire
     into `pipeline/compile.rs` post-`project_types` emitting
     `target/audit/<grammar>.json`, OR delete outright if the wire is
     net-cost over `cargo xtask regen --check`).
   - `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs`
     deletion per `AUDIT-2` §6.C.
   - W2.RE `panic!` quartet at `shapes/hregex.rs:285,446,579,718`
     retires by Sheets / CSS L4 hitting HRegex struct-direct (write
     the body, panic dies).
   - `crates/core/src/backend/rust/emitter/grammar.rs` god-module
     split per `audit/AUDIT-6-ARCHITECTURE.md` §8.5
     (orchestrator-direct in W2-act.A or W2-act.C as scope permits).
   - `shapes/{object,arglist}.rs` directory-module conversion per
     §8.6 (W2-act.B-co-located when the agent owns the file).
7. Close ceremony absorbs W4: 17-entry matrix re-run, samply capture
   under `docs/benchmarks/profiles/AZ-I/W2-act/`, parity harness
   summary, AZ-II handoff contract verification (seven-point read of
   AZ-I.md §Handoff contract), AZ-I FINAL.md, `post-AZ-I.json` archive,
   workspace nextest at master HEAD.

## File bounds

| File | Access | Owner |
|---|---|---|
| `crates/ir/src/registry/strategy.rs` | create | W2-act.A |
| `crates/ir/src/registry/mod.rs` | modify | W2-act.A |
| `crates/ir/src/lib.rs` | modify | W2-act.A |
| `crates/core/src/backend/rust/emitter/strategy.rs` | delete | W2-act.A |
| `crates/core/src/backend/rust/emitter/mod.rs` | modify | W2-act.A |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | modify | W2-act.A |
| `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs` | delete | W2-act.A |
| `crates/core/src/backend/rust/emitter/grammar.rs` | modify-carve | W2-act.A |
| `crates/core/src/runtime/json/value.rs` | modify | W2-act.A |
| `crates/core/src/runtime/json/builder.rs` | modify | W2-act.A |
| `crates/core/src/runtime/json/document.rs` | create | W2-act.A |
| `crates/core/src/runtime/json/mod.rs` | modify | W2-act.A |
| `crates/core/src/pipeline/compile.rs` | modify | W2-act.A |
| `crates/core/tests/emitter_registry_read.rs` | delete | W2-act.A |
| `crates/core/tests/emit_strategy.rs` | modify | W2-act.A |
| `crates/core/tests/json_slab.rs` | modify | W2-act.B1 |
| `crates/core/tests/projection_totality.rs` | modify | W2-act.B1 |
| `crates/core/tests/typed_accessor_surface.rs` | modify | W2-act.B1 |
| `crates/core/tests/json_parity.rs` | modify | W2-act.B1 |
| `crates/core/tests/json_parity_struct.rs` | modify | W2-act.B1 |
| `crates/core/tests/json_canonical_parity.rs` | modify | W2-act.B1 |
| `crates/core/tests/json_value_parity.rs` | modify | W2-act.B1 |
| `crates/core/src/grammar/generated/json.rs` | regen | W2-act.B1 (orchestrator) |
| `crates/core/src/runtime/google_sheets/**` | create | W2-act.B2 |
| `crates/core/src/runtime/mod.rs` | modify | W2-act.B2 + W2-act.B3 |
| `crates/core/tests/sheets_parity.rs` | modify | W2-act.B2 |
| `crates/core/tests/sheets_expr_parity.rs` | modify | W2-act.B2 |
| `crates/core/tests/google_sheets_slab.rs` | modify | W2-act.B2 |
| `crates/core/src/grammar/generated/google_sheets.rs` | regen | W2-act.B2 (orchestrator) |
| `crates/core/src/runtime/css_l4/**` | create | W2-act.B3 |
| `crates/core/tests/css_l4_parity.rs` | modify | W2-act.B3 |
| `crates/core/tests/css_l4.rs` | modify | W2-act.B3 |
| `crates/core/tests/css_l4_color_view.rs` | modify | W2-act.B3 |
| `crates/core/tests/css_l4_dimensions.rs` | modify | W2-act.B3 |
| `crates/core/tests/css_l4_named_color_parity.rs` | modify | W2-act.B3 |
| `crates/core/tests/css_l4_canonical_parity.rs` | modify | W2-act.B3 |
| `crates/core/src/grammar/generated/css_l4.rs` | regen | W2-act.B3 (orchestrator) |
| `crates/ir/src/passes/audit/payload_coverage.rs` | modify-carve OR delete | W2-act.A |
| `crates/json-prototype/` | retire | W2-act.A or W2-act.C |
| `crates/core/src/backend/rust/emitter/shapes/hregex.rs` | modify | W2-act.B2 + W2-act.B3 |
| `docs/benchmarks/post-AZ-I.json` | create | W2-act.C |
| `docs/benchmarks/profiles/AZ-I/W2-act/**` | create | W2-act.C |
| `docs/tranches/AZ-I/FINAL.md` | create | W2-act.C |
| `docs/tranches/AZ-I/PROGRESS.md` | modify | each agent |

**Do NOT touch**: BBNF runtime / bootstrap (AZ-II); `crates/tape/`
(remains for BBNF in AZ-I; AZ-II owns deletion); pprint / parse-that
(no touch in AZ-I); BB scaffold (BB.W0 in parallel, separate file
bounds — `crates/egraph/`, `crates/ir/src/rewrites/`).

## Phase sub-items

### AZ-I.W2-act.A — EmitStrategy hoist + JsonDocument accessor API + dead-substrate sweep

Sequential, blocks B1/B2/B3. Cap **90 min**.

Mechanism:
- Move `EmitStrategy` enum + resolver from
  `crates/core/src/backend/rust/emitter/strategy.rs` to
  `crates/ir/src/registry/strategy.rs`. Generalise the variant
  payload to `SubstrateBinding { rust: { builder_path, document_path },
  ts: …, wasm: … }` per `audit/AUDIT-6-ARCHITECTURE.md` §8.1; for AZ-I,
  the TS/WASM fields land as `Option<&'static str>` populated by the
  per-grammar resolver arm but unused at codegen time (forward-compat
  for BA host bindings + AZ-II view recode).
- `for_grammar(grammar_ident, &registry)` becomes the IR-level
  decision point; the Rust backend's `parse_body` reads the strategy
  off `&PreparedGrammar` rather than re-deriving.
- Author `crates/core/src/runtime/json/document.rs` (or extend
  `value.rs`) with the accessor API:
    - `JsonDocument::root(&self) -> JsonValue<'p>`
    - `JsonDocument::array(&self, id: JsonArrayId) -> &[JsonValue<'p>]`
    - `JsonDocument::object(&self, id: JsonObjectId) -> &[JsonPair<'p>]`
    - `JsonDocument::view(&self) -> JsonView<'_>` — a thin newtype
      around `&'a JsonDocument<'a>` exposing the same shape the
      pre-W2-act `Parsed<JsonGrammar>::view()` did.
    - `JsonDocument::to_value(&self) -> &JsonValue<'_>` — borrow form
      of the root value, matching `Parsed::to_value()` semantics.
    - `JsonDocument::get<T>(&self, path: Path<'_>) -> Option<T>` —
      forwarded to the existing path-query infra; impls land
      against `JsonValue<'p>` directly per AY's `PathQuery` trait.
- Resolver arm: `("JsonParser" | "JsonGrammar", true) =>
  EmitStrategy::StructDirect { rust: SubstrateBinding { builder_path:
  "::bbnf::runtime::json::JsonStructBuilder", document_path:
  "::bbnf::runtime::json::JsonDocument" }, … }`. Sheets / CSS L4 add
  their own arms in B2 / B3.
- Delete `crates/core/src/backend/rust/emitter/shapes/
  registry_observer.rs` and `crates/core/tests/emitter_registry_read.rs`
  per `AUDIT-2` §6.C — module's docstring expects deletion at AZ-I
  close; the `record` write-only sink has no production reader.
- Wire `audit_payload_coverage` into `pipeline/compile.rs` post-
  `project_types`, OR delete the audit-pass surface (`AbsentRegistryProbe`,
  `PayloadLayoutsProbe`, `audit_payload_coverage`,
  `write_coverage_report`) per `AUDIT-2` §6.B. The decision rule:
  if the audit pass produces a runtime artefact a future tranche
  consumes (e.g. CI build-fail on coverage regression), wire it; if
  the gate-bearing test (`payload_coverage_audit.rs`) is the only
  consumer the trait surface ever has, delete and let the leaf test
  inline its own probe.
- Update AZ-I.md wave summary table: W2 substrate close commit, W2-act
  in-progress.

Sub-gate (W2-act.A close):
1. `bbnf_ir::registry::EmitStrategy` resolves; the Rust emitter
   reads it through `&PreparedGrammar`.
2. `JsonDocument::view()` / `to_value()` / `get::<T>(path)` exist with
   the documented signatures; the three broken tests
   (`json_slab`, `projection_totality`, `typed_accessor_surface`)
   compile.
3. `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs`
   does not exist; `tests/emitter_registry_read.rs` does not exist.
4. Audit-pass surface either wired or deleted per the decision rule.
5. `cargo nextest run --workspace --profile ax-iter` ≥ 1546 (W2-close
   baseline). The `for_grammar` arm-flip waits for B1 — A's resolver
   still returns `TapeDirect` for `JsonGrammar` until B1 owns regen.

### AZ-I.W2-act.B1 — JSON activation

Parallel after A. Cap **60 min**.

Mechanism:
- Extend the resolver in
  `crates/ir/src/registry/strategy.rs::EmitStrategy::for_grammar` to
  return `StructDirect { … }` for `JsonParser` / `JsonGrammar` when
  `registry_populated`.
- Migrate the three broken test consumers to `JsonDocument`:
  `parsed.view()` → `doc.view()`; `parsed.to_value()` → `*doc.root()`
  (or `doc.to_value()` borrow); offset-cursor sites in `json_decode.rs`
  / `json_parity.rs` resolve via the document's arena handles.
- Recode parity harnesses: `tests/json_parity.rs` (sonic-rs),
  `tests/json_parity_struct.rs` (already against `JsonDocument` —
  promote from W2-act probe to load-bearing harness),
  `tests/json_canonical_parity.rs` (simdjson OnDemand + serde_json),
  `tests/json_value_parity.rs`. Comparisons run struct-vs-native
  node-for-node on the JSON fixture corpus
  (`data/json/{canada,citm,twitter}.json` + smaller fixtures).
- Bench gate: `cargo bench -p bbnf --bench json_monolithic` cold
  per-parse. Twitter ≥ 1967 MB/s, canada ≥ 1231, citm ≥ 2438. Output
  archived to `docs/benchmarks/profiles/AZ-I/W2-act/json/`.

Sub-gate (B1 close):
1. JSON parity harnesses green on the full corpus (sonic-rs + simdjson
   OnDemand + serde_json + canonical).
2. JSON twitter ≥ 1967, canada ≥ 1231, citm ≥ 2438 cold per-parse on
   the struct-only path.
3. `rg 'TapeBuilder|TapeCursor|TapeRec|push_rec' crates/core/src/grammar/generated/json.rs`
   returns zero hits.
4. `cargo nextest run -p bbnf --profile ax-iter` does not regress.

### AZ-I.W2-act.B2 — Sheets activation

Parallel after A. Cap **90 min** (new runtime + 8+ layouts +
HRegex struct-direct body for formula expressions).

Mechanism:
- Author `crates/core/src/runtime/google_sheets/` directory:
  `value.rs` (`SheetsValue<'p>` enum: `SheetsCell`, `SheetsFormula`,
  `SheetsRef`, etc.), `arena.rs` (slab-of-Vec for compound children
  matching JsonArena pattern), `builder.rs` (`SheetsStructBuilder<'p>`
  implementing the trait), `document.rs` (`SheetsDocument<'p>` +
  view / to_value / get accessors).
- Layouts derive from `grammar/google-sheets/google-sheets.bbnf` via
  `populate_struct_registry` (already running); the runtime types
  match the layout shapes.
- Resolver arm: `("GoogleSheetsParser", true) => StructDirect { … }`.
- Land HRegex struct-direct emitter body per shape — Sheets uses
  HRegex for formula tokenisation; replace the W2.RE
  `panic!("…HRegex shape does not support StructDirect…")` at
  `shapes/hregex.rs:285,446,579,718` with the actual struct-direct
  body. The body emits per-token `builder.push_leaf_with_*` calls
  matching the regex-classified token type.
- Recode `tests/sheets_parity.rs`, `tests/sheets_expr_parity.rs`,
  `tests/google_sheets_slab.rs` against `SheetsDocument`.
- Bench gate: parse_simple ≥ 95 MB/s on
  `cargo bench -p bbnf --bench google_sheets_monolithic`.

Sub-gate (B2 close):
1. Sheets parity harness green on the corpus.
2. parse_simple ≥ 95 MB/s cold per-parse on the struct-only path.
3. `rg 'panic!\(.*StructDirect' crates/core/src/backend/rust/emitter/shapes/hregex.rs`
   returns zero hits.
4. `rg 'TapeBuilder|TapeCursor|TapeRec|push_rec' crates/core/src/grammar/generated/google_sheets.rs`
   returns zero hits.

### AZ-I.W2-act.B3 — CSS L4 activation

Parallel after A. Cap **120 min** (largest grammar — 187 layouts +
typed-value enum family + lightningcss parity).

Mechanism:
- Author `crates/core/src/runtime/css_l4/` directory:
  `value.rs` (typed-value enums: `Length`, `Color`, `Dimension`,
  `Time`, `Resolution`, `Percentage`, `Angle`; aggregate enums:
  `CssTypedValue`, `CssRule`, `StyleRule`, `MediaRule`,
  `KeyframesRule`, `Selector`, `Declaration`, `StyleSheet`),
  `arena.rs`, `builder.rs` (`CssStructBuilder<'p>`), `document.rs`
  (`CssDocument<'p>`).
- Layouts cover all 187 Named rules (162 grammar-named + 25
  anonymous continuation rules surfaced by lowering); the runtime
  types follow the layout shapes per
  `feedback_preserve-rich-ast` — every alternation in the grammar
  becomes a typed enum; no flattening for parse speed.
- Resolver arm: `("CssL4Parser", true) => StructDirect { … }`.
- Land Flat / HRegex / ArgList / Unordered struct-direct emitter
  bodies as CSS L4 surfaces them; each replaces a remaining
  W2.RE panic with the actual body.
- Lightningcss parity harness recode at `tests/css_l4_parity.rs`
  (and the colour / dimension / named-color sub-harnesses) compares
  `CssDocument` against `lightningcss::stylesheet::StyleSheet`
  node-for-node on `data/css/{normalize,bootstrap,tailwind}.css`.
- cssparser parity harness similarly compares against the cssparser
  reference output.
- Bench gate: `cargo bench -p bbnf --bench css_l4` cold per-parse.
  normalize ≥ 735, bootstrap ≥ 600, tailwind ≥ 500.

Sub-gate (B3 close):
1. lightningcss + cssparser parity harnesses green on the full
   CSS L4 corpus.
2. normalize ≥ 735, bootstrap ≥ 600, tailwind ≥ 500 cold per-parse.
3. `rg 'panic!\(.*StructDirect' crates/core/src/backend/rust/emitter/shapes/{hregex,flat,arglist,unordered}.rs`
   returns zero hits.
4. `rg 'TapeBuilder|TapeCursor|TapeRec|push_rec' crates/core/src/grammar/generated/css_l4.rs`
   returns zero hits.

### AZ-I.W2-act.C — Close ceremony (W4 absorbed)

Sequential after B*. Cap **90 min**.

Mechanism:
- Run the full 17-entry matrix on the struct-only path for the three
  data grammars + tape path for BBNF. Cold per-parse, sequential,
  per `feedback_no-warm-benches` and `feedback_bench-sequential-regression`.
  `make ay-bench-close WAVE=close` close-gate command surface.
- Capture samply fleet under `docs/benchmarks/profiles/AZ-I/W2-act/`
  for json_monolithic / css_l4 / google_sheets_monolithic /
  bbnf_monolithic / compile_pipeline benches.
- Archive bench JSON at `docs/benchmarks/post-AZ-I.json` (close
  matrix per `docs/instructions/README.md` §Tranche completion).
- Verify AZ-II handoff contract (AZ-I.md §Handoff contract to AZ-II,
  seven points): three data grammars on direct-to-struct;
  `StructRegistry` closed; `crates/tape/` compiles; BBNF unchanged;
  17-entry matrix at AU parity; classifier scoping resolved (locked-
  split per W0); RESEARCH.md cited.
- Author `docs/tranches/AZ-I/FINAL.md` per
  `docs/instructions/README.md` §Tranche completion. Cap 350 LOC per
  `audit/AUDIT-1-PROCESS-RETRO.md` §4 cut #4. Phase recap with commit
  hashes; hard-gate readout; deferred ledger (none); cross-tranche
  debt reconciled (AY-III gates absorbed; tape-substrate prune
  candidates handed to AZ-II.cutover).
- Workspace nextest at master HEAD: `cargo nextest run --workspace
  --profile ax-iter --no-fail-fast` returns 0 failures (≥ 1546).
- Update `docs/tranches/AZ-I/PROGRESS.md` with the close entry +
  master HEAD commit.
- Decay sweep: ensure
  `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs`,
  `tests/emitter_registry_read.rs`, `crates/json-prototype/` (if
  retired) are absent; `audit_payload_coverage` either wired or
  surface deleted.

Sub-gate (C close, AZ-I-final):
1. `docs/benchmarks/post-AZ-I.json` exists and covers the close
   matrix (json × {canada, citm, twitter, data, data_xl} + css_l4 ×
   {normalize, bootstrap, tailwind} + google_sheets × {parse_simple,
   parse_nested, parse_stress} + bbnf × {json, ebnf, css_pretty,
   google_sheets, bbnf_self, css_l4_grammar} + compile_pipeline
   tranche-selected entries).
2. AZ-I FINAL.md committed.
3. AZ-I PROGRESS.md close entry committed with master HEAD.
4. `cargo nextest run --workspace --profile ax-iter --no-fail-fast`
   returns 0 failures.
5. Tape-symbol live-grep returns hits exclusively in the BBNF
   bootstrap consumer set + historical docs.

## Hard gate

1. `EmitStrategy` lives in `bbnf-ir::registry::strategy`; Rust emitter
   reads from there. Verification: `rg 'pub.*enum EmitStrategy'
   crates/` returns one hit at `crates/ir/src/registry/strategy.rs`.
2. JSON / Sheets / CSS L4 generated parsers contain zero
   `TapeBuilder` / `TapeCursor` / `TapeRec` / `push_rec` references;
   the live tape-symbol scan returns hits only in the BBNF
   bootstrap consumer set. Verification: `rg
   'TapeBuilder|TapeCursor|TapeRec|push_rec' crates/core/src/grammar/generated/{json,css_l4,google_sheets}.rs`
   returns zero matches.
3. 17-entry close matrix at AU floor on every entry — JSON twitter
   ≥ 1967, canada ≥ 1231, citm ≥ 2438; CSS normalize ≥ 735,
   bootstrap ≥ 600, tailwind ≥ 500; Sheets parse_simple ≥ 95;
   BBNF entries unchanged from AU. Verification: bench JSON archived
   at `docs/benchmarks/post-AZ-I.json`.
4. Parity harnesses green: sonic-rs / simdjson OnDemand /
   serde_json (JSON), Sheets-native, lightningcss / cssparser
   (CSS L4) — all on the struct-only path.
5. IR audit pass reports 100% `->` coverage on JSON, CSS L4,
   Sheets. Verification: `cargo nextest run -p bbnf-ir --test
   payload_coverage_audit --profile ax-iter` 9/9 green; CSS L4 +
   Sheets coverage tests in `crates/core/tests/project_types_*`
   green.
6. Workspace nextest ≥ 1546 (W2-close baseline) on
   `cargo nextest run --workspace --profile ax-iter --no-fail-fast`.
7. AZ-I FINAL.md + `docs/benchmarks/post-AZ-I.json` exist on master.
8. W2.RE `panic!` quartet retired (Sheets / CSS L4 hit HRegex /
   Flat / ArgList / Unordered struct-direct bodies; no codegen-time
   panic).
9. Dead substrate retired: `registry_observer` deleted;
   `audit_payload_coverage` wired or deleted; `json-prototype`
   retired or demoted to bench adjunct.

## Verification artefacts

- `docs/benchmarks/post-AZ-I.json` — 17-entry close matrix.
- `docs/benchmarks/profiles/AZ-I/W2-act/{json,css_l4,google_sheets,bbnf,compile_pipeline}/`
  — samply captures.
- `docs/tranches/AZ-I/FINAL.md` — close ceremony.
- `docs/tranches/AZ-I/PROGRESS.md` — close entry + master HEAD.
- Per-grammar regen artefacts under `crates/core/src/grammar/generated/`
  (json.rs, google_sheets.rs, css_l4.rs).
- Per-grammar parity test logs (committed on disk under
  `target/criterion/` is divan, archived JSON output is the canonical
  artefact — divan emits JSON to stdout via `--output-format=json`).
- Commit hashes for each milestone in PROGRESS.md.

## Dependencies

- **Depends on**: AZ-I.W2 substrate close (master `6f78c1ef`);
  `crates/ir/src/registry/struct.rs` (StructRegistry); the W2 per-shape
  emitters (Object/Array/Number/String/Scalar/Keyword/Wrap/AltDispatch/
  Flat); the W2 dispatcher signature parameterized by `&EmitStrategy`.
- **Blocks**: AZ-II.cutover (BBNF cutover + tape deletion); BA.W0
  (path IR over the closed struct tree). BB.W0 may open in parallel
  per `audit/W2-CLOSE-AUDIT.md` Proposal E (substrate independence
  on `IrNode`).

## Reversal posture

Per `audit/W2-CLOSE-AUDIT.md` §10:
- Bench-gate miss > 20% on any of the 17 entries reverts the activation
  arm for the failing grammar (resolver returns `TapeDirect` for that
  grammar) + re-plans through W2-act.A research. The substrate stays;
  per-grammar test migrations stay; hygiene cuts stay.
- Parity harness regression on a previously-passing AU-baseline entry
  reverts the responsible substrate immediately.
- No hedging forward: no carry of W2-act.B* misses to AZ-II.cutover;
  `feedback_no-deferrals` is enforced.

## Archaeology

W2-act supersedes the original W2.B / W3 / W4 wave docs at
`docs/tranches/AZ-I/waves/{W3,W4}.md` (carrying supersede notices per
`6f78c1ef`). The W2-EMITTER-REWIRE.md A/B/C/D/E redress mapped to W2's
substrate landing; W2-act re-uses the same disjoint per-shape file
bounds for its activation pass without re-redressing the substrate
itself.

The W2 substrate close at `409b835d` reverted activation per W2.md
§Reversal blockers; W2-act lands those blockers (JsonDocument
accessor API, parity harness recoding, bench gate verification) plus
extends the resolver to Sheets + CSS L4 in the same wave per
`audit/W2-CLOSE-AUDIT.md` §2.
