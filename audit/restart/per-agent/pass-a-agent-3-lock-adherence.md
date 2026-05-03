# Pass A — Agent A.3 — Lock Adherence

Date: 2026-05-03
Lens: the 14 locks at `docs/HARDENING-PLAN-PROMPT.md` applied per file in
Pass A scope.

For each lock, the table cites where the lock is honoured / violated /
silent in Pass A code, with surgery for every violation.

---

## Lock 1 — Tape and its columnar variants are fully dead

**Statement.** No tape crate, no `TapeRec`, no `TapeCursor`, no kind-
partitioned columnar SoA, no variant-strip arenas, no "tape rebranded as
fast-path".

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/lib.rs` | no `pub mod tape`; no tape symbol export |
| `crates/ir/src/lib.rs` | no `pub mod tape`; tape is gone from IR |
| `crates/core/src/grammar/mod.rs` parser entry | parses through `BbnfBootstrap::parse` (struct-direct), not a tape walker |

### Violated (residue)

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/core/src/grammar/mod.rs:3, 7, 17` | 1 | residue (comments) | DELETE the "tape-first bootstrap parser" / "tape walkers" / "walks the tape" comment phrases — substrate is dead. |
| `crates/core/src/grammar/schema/build.rs:26` | 1 | residue (comments) | DELETE "per-rule `<Rule>View` family under the tape-first AC.2" |
| `crates/core/src/grammar/schema/model.rs:20` | 1 | residue (comments) | DELETE "the tape-first AC.2 emitter" |
| `crates/core/src/grammar/schema/emit/rust/shared.rs:3, 17` | 1 | residue (comments) | DELETE "Post-Tranche AC.2 rewrite: schema helpers emit impls on tape-backed records" |
| `crates/core/src/path/ascent.rs:61` | 1 | residue (comments) | DELETE "callback that traverses the tape" |
| `crates/core/src/path/variant_select.rs:21` | 1 | residue (comments) | DELETE "does not consult the runtime tape" |
| `crates/core/src/types.rs:90` | 1 | residue (comments) | DELETE "lowering pipeline now walks the bootstrap tape directly into" |
| `crates/ir/src/types/type_desc.rs:103, 147` | 1 | residue (comments) | DELETE "Span-typed rules already carry their span in `TapeRec`" + "store their span natively in `TapeRec.span_lo/`" |
| `crates/ir/src/types/grammar.rs:310` | 1 | residue (comments) | "FusedBuilder::with_capacity divisor so RawVec::grow_one" — DELETE the FusedBuilder phrase |

The Lock 1 violations in Pass A scope are exclusively narrative residue.
The substrate itself (TapeRec / TapeCursor / FusedBuilder symbols) is gone
from `crates/core/src/{grammar,lower,path,imports}/` and from
`crates/ir/src/`. Verification: `rg -n 'TapeRec|TapeCursor|payload_idx|
OpenFrame|FusedBuilder' crates/core/src/lower crates/core/src/path
crates/core/src/imports crates/ir/src/types crates/ir/src/registry` returns
zero non-comment hits at HEAD.

### Silent

(none)

**Verdict (Pass A, Lock 1)**: substantively-honoured; ~9 narrative-residue
sites require comment scrub at BA-restart. No live tape code in Pass A
scope.

---

## Lock 2 — Layout lowering is the canonical IR pass name

**Statement.** The term replaces *type projection / type collapsing / type
inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema
synthesis* everywhere. The IR module is `bbnf-ir/src/passes/layout/`; the
IR record is `Layout`; the trait that consumes it is `LayoutSink`.

### Honoured

(none — Lock 2 lands as a rename obligation. The current code has not yet
applied it.)

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/ir/src/passes/types/mod.rs` | 2 | violated | RENAME `passes/types/` → `passes/layout/`; mod-rename. The 786-LOC `mod.rs` becomes `layout/{solver.rs, projection.rs, lifetime.rs, registry_glue.rs}`. |
| `crates/ir/src/passes/types/registry.rs` | 2 | violated | RENAME, then SPLIT (510 LOC). |
| `crates/ir/src/passes/types/type_map.rs` | 2 | violated | RENAME the file + the type — `TypeMap` is a Lock-2 retired term. Fold into `Layout`. |
| `crates/ir/src/passes/types/{constraint,obligation,subvariants,generate}.rs` | 2 | violated | RENAME the directory. |
| `crates/ir/src/types/type_desc.rs` | 2 | violated (entire type) | RENAME `TypeDesc` → fold into `Layout` representation. The IR pub re-export at `crates/ir/src/lib.rs` of `TypeDesc, TypeDescId, TypeDescInterner` should become `Layout, LayoutId, LayoutInterner`. |
| `crates/ir/src/registry/struct.rs` | 2 | violated (entire type) | RENAME `StructLayout` → `Layout`; `StructRegistry` → `LayoutRegistry`. |
| `crates/ir/src/registry/mod.rs` | 2 | violated | re-export rename. |
| `crates/ir/src/lib.rs` | 2 | violated | re-export rename. |
| `crates/ir/src/passes/payload/layout.rs` | 2 | partially honoured (named `layout.rs`) — but its content speaks `StructLayout` | RENAME types within. |

Pass A's Lock 2 footprint is large because the IR vocabulary is the entire
naming surface. The synthesizer must batch the rename into a single BA wave
(no incremental rename — one pass per the user's `single-plan-execution`
precept).

**Verdict (Pass A, Lock 2)**: violated. Surgery is a single coordinated
rename pass.

---

## Lock 3 — Cursor-parse + byte-skip unified

**Statement.** One parse implementation. Cursor consultation generates
byte-skip when consult returns `Skip`. The empty-path case
(`__EAGER_EMPTY_PATH`) elides cursor calls entirely.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/path/cursor.rs:115-122` | `PathCursor` defines a single `decide(rule_id) -> Decision` surface — one shape; eager and lazy share it. |
| Generated parser (Pass B scope) | `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json,&str>>` exists at `generated/json.rs:3443` per RESTART-SKETCH §A.1; one parser, eager-empty-path is the trivial cursor. |

### Violated

(none in Pass A scope; the cursor consultation cost on the eager path is a
codegen-side concern — it shows up in `generated/*` (Pass B) per
RESTART-SKETCH §A.4 item 12, but the Pass A surface — `PathCursor` types
themselves — is single-implementation.)

### Silent

(none)

**Verdict (Pass A, Lock 3)**: honoured.

---

## Lock 4 — Per-domain orthogonal optimization

**Statement.** CSP type/layout inference, e-graph rewriting, pattern miners,
shape analysis, and cost model compose by output-piping. No unified
hypergraph.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | CSP-driven strategy synthesis lives here, separate from e-graph |
| `crates/ir/src/egraph/` | e-graph is its own subtree |
| `crates/ir/src/passes/recognizers/` | recogniser pattern miners are sequential pre-passes |

### Violated

(none in Pass A scope.)

### Silent

(none)

**Verdict (Pass A, Lock 4)**: honoured. The decomposition is structural —
each crate / subtree carries one optimisation tier.

---

## Lock 5 — IR + per-backend lower

**Statement.** Codegen emits a backend-agnostic typed IR; per-backend
lowerers (Rust now, TS+WASM at BD+) produce native source. The IR is the
contract.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/ir/src/lib.rs` | `IrNode`, `Layout` (pending rename), `RuleId` are backend-agnostic |
| `crates/ir/src/registry/strategy.rs:103-118` | `EmitStrategy::StructDirect { rust, ts, wasm }` carries per-backend `SubstrateBinding` slots; ts/wasm reserved for BD+ |

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/ir/src/registry/strategy.rs:130-185` | 5 | violated | the `PRODUCTION_MANIFEST_TABLE` carries Rust-specific paths (`crate::runtime::<g>::<G>StructBuilder`). The IR contract is backend-agnostic, but the table mixes IR-level enum-arm declaration with Rust-specific path strings. RESTRUCTURE — the Rust-path mapping moves to `crates/bbnf-codegen/src/rust/` (Pass B scope); the IR-level `EmitStrategy` carries only abstract shape selection (`StructDirect`, etc.). |

### Silent

(none)

**Verdict (Pass A, Lock 5)**: substantively honoured; one structural
redress required (relocate Rust-specific path strings out of IR).

---

## Lock 6 — xtask emits committed source artefacts

**Statement.** No proc-macro façade. css_l4.rs at 107 K LOC is greppable on
disk.

Pass A scope is pre-codegen; Lock 6 is a Pass B concern primarily. The Pass
A intersection is the workspace metadata schema:

| Path:line | Evidence |
|---|---|
| `Cargo.toml:[workspace.metadata.bbnf]` | xtask reads this table; per-grammar regen is committed |
| `crates/core/src/grammar/generated/*.rs` | committed artefact, not proc-macro expansion |

**Verdict (Pass A, Lock 6)**: honoured.

---

## Lock 7 — `crates/path/` is the consolidated path crate

**Statement.** The runtime cursor engine merges INTO it; the existing
`crates/core/src/path/` directory empties. The Rust `pointer!` proc-macro
lives there. The TS proc-macro lives at `crates/path-ts/`. A
`crates/path-core/` (non-proc-macro) crate may exist as the
deduplication mechanism.

### Honoured

(none — Lock 7 names the post-restart shape; today's structure is the
pre-restart shape.)

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/bbnf-path/` (entire crate) | 7 | violated | RENAME to `crates/path/`. The package name remains `bbnf-path` or renames to `path` per Lock 7 — synthesizer adjudicates. |
| `crates/bbnf-path-ts/` (entire crate) | 7 | violated | RENAME to `crates/path-ts/`. |
| `crates/core/src/path/` (entire directory) | 7 | violated | MOVE to `crates/path/src/runtime/` (or split: types into `path-core`, runtime executor into `path`). |
| `crates/bbnf-path/src/path_macro.rs` (639 LOC) duplicated by `crates/bbnf-path-ts/src/compile.rs` (474 LOC) | 7 | violated | EXTRACT validate/lower into a non-proc-macro `path-core` crate per Lock 7's footnote. |
| `crates/bbnf-path/src/registry.rs` (201 LOC) + `crates/bbnf-path-ts/src/fixture.rs` (248 LOC) | 7 | violated (Lock 14 cross-cut) | RETIRE both; finish the T4 closure (per-grammar emitted `pub const REGISTRY: Layout` consumed by both frontends). |

### Silent

(none)

**Verdict (Pass A, Lock 7)**: violated. Surgery is a four-crate restructure
(`bbnf-path` → `path` + `path-core` + `path-ts`; existing `crates/core/src/path/`
absorbs into the consolidated `path` runtime).

---

## Lock 8 — Surpass sonic-rs / simdjson / lightning-css

This is a perf-gate concern (Pass B-side runtime measurement). Pass A
scope is pre-codegen; the lock cuts in only insofar as IR optimisation
output reaches the codegen.

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs:113-115, 359` | 8 | partially-honoured (narrative cites CSS L4 perf) | KEEP narrative; add SOTA citations to recogniser-output rationale where perf matters. |

**Verdict (Pass A, Lock 8)**: honoured (Pass A is pre-perf-gate; the IR
shape itself does not block surpassing SOTA).

---

## Lock 9 — Slice-borrow primary; bumpalo + owned escape hatches

**Statement.** Default API is `&'i str` slices + `Cow<'i, str>` for
transformations. Bumpalo arena is opt-in via `parse_in(input, &bump)`.
Owned (no-borrow) is opt-in via `parse_owned(input)`.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/lower/mod.rs` `LowerCtx<'a>` | the lowering substrate is borrowed-primary (`&'a str` everywhere) |
| `crates/core/src/path/ir.rs:42-65` `PathSegment<'a>` | `Field(&'a str)` is the primary alphabet; `OwnedPathSegment` is the owned escape hatch |
| `crates/core/src/imports/registry.rs` | `ModuleData` borrows from the loader's owned source |

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/core/src/grammar/mod.rs:53-57` | 9 | violated | the parse entry leaks the input string with `Box::leak(source.to_owned().into_boxed_str())` to satisfy `'static` lifetime requirements. The leak forces an *owned* shape into the *borrow* path. The Lock 9 default is borrowing; the leak should be excised by either (a) lifting input ownership to the caller (forcing a `&'p str` API), or (b) introducing a `parse_in(input, &bump)` arena variant per Lock 9. |

### Silent

(none)

**Verdict (Pass A, Lock 9)**: violated at one site (parse-entry leak).
Surgery is a public-API change.

---

## Lock 10 — Pratt + SIMD auto-detected

**Statement.** No `@pratt` or `@simd` directives. Optimizer mines grammar
shape (left-recursive operator chains → Pratt) and leaf-pattern shape
(charclass / keyword set / regex → SIMD scanner) and emits accordingly.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/lower/expression/pratt.rs` | Pratt detection lives in lowering — no grammar-author directive required |
| `crates/ir/src/passes/recognizers/operator_chain.rs` (415 LOC) | mines operator chains automatically |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs` (383 LOC) | mines per-leaf alphabet automatically |

### Violated

(no grammar-author `@pratt` or `@simd` directive observed in Pass A scope.)

### Silent

(none)

**Verdict (Pass A, Lock 10)**: honoured.

---

## Lock 11 — Path-deps for incubating sister crates

**Statement.** egraph + egraph-derive + csp-solver + bbnf-regex + parse-that
as path-deps in workspace until each API stabilises.

### Honoured (partially)

| Path:line | Evidence |
|---|---|
| `crates/core/Cargo.toml` `bbnf-ir = { ..., path = "../ir" }` | bbnf-ir is path-dep |
| `crates/core/Cargo.toml` `simd-scan = { ..., path = "../simd-scan" }` | simd-scan is path-dep |
| `crates/core/Cargo.toml` `egraph = { path = "../egraph" }` | egraph is path-dep |
| `crates/core/Cargo.toml` `csp-solver = "0.1"` | NOT path-dep — versioned dep |
| `crates/ir/Cargo.toml` `bbnf-regex = { version = "0.1", features = ["serde"] }` | NOT path-dep — versioned dep |
| `crates/bbnf-path/Cargo.toml` + `bbnf-path-ts/Cargo.toml` | both reference `bbnf-regex = { version = "0.1" }` — versioned, not path-dep |
| `parse-that` (sibling repo) | NOT a workspace member; consumed via `parse_that = "..."` versioned dep where used |

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/core/Cargo.toml` `csp-solver = "0.1"` | 11 | violated | switch to `csp-solver = { version = "0.1", path = "../csp-solver" }` — the crate exists in the workspace at `crates/csp-solver/` |
| `crates/ir/Cargo.toml` `bbnf-regex = { version = "0.1", features = ["serde"] }` | 11 | violated | promote `bbnf-regex` to workspace path-dep. The crate currently lives at `/Users/mkbabb/Programming/parse-that/rust/regex/` — relocate (or sub-module) it into `crates/bbnf-regex/`. |
| `crates/bbnf-path/Cargo.toml` + `crates/bbnf-path-ts/Cargo.toml` `bbnf-regex = { version = "0.1" }` | 11 | violated | same — promote to workspace path-dep |
| `parse-that` (sibling repo) — NOT in workspace | 11 | violated (not present at all as path-dep) | promote `parse-that` to workspace path-dep at `crates/parse-that/`. The crate currently lives at `/Users/mkbabb/Programming/parse-that/rust/parse_that/`. |

### Silent

(none)

**Verdict (Pass A, Lock 11)**: violated. Surgery: bring `parse-that` and
`bbnf-regex` into the workspace as path-deps; switch `csp-solver` from
versioned to path-dep.

---

## Lock 12 — ser + gorgeous archive BEFORE BA.W0

This is a Pass C concern (archive ceremony). Pass A scope is unaffected.

**Verdict (Pass A, Lock 12)**: silent (out-of-scope).

---

## Lock 13 — No god directories; cohesive encapsulation at every level

**Statement.** Every directory partitions one cohesive concern. Files >500
LOC outside `generated/` are forbidden; directories with >10 immediate
children mixing concerns are forbidden.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/path/` | 11 sub-modules, all path-concerned, one concern (typed-path types + execution) — Lock 13 § subdivision honoured |
| `crates/core/src/imports/` | 5 sub-modules, all import-concerned, ≤ 200 LOC each — clean |
| `crates/core/src/lower/` | sub-divides into expression / value_expr / view_walk / metadata / ... — concerns separated; but several files >500 LOC |
| `crates/core/src/grammar/schema/` | clean three-level split |
| `crates/ir/src/dag/` | clean five-file partition |
| `crates/ir/src/recognizer/` | clean three-file partition |

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/core/src/lib.rs` direct children: `types`, `css_types`, `grammar`, `generate`, `backend`, `graph`, `imports`, `lower`, `path`, `pipeline`, `runtime` (eleven; mixed concerns at the root) | 13 | violated (mixed concerns at top level) | This is the Pass A's intersection with the `crates/core` god-crate split (Agent A.4 territory). The root mixes parser-front (lower, pipeline, imports), middle-pipeline (generate, backend), and runtime (runtime). Surgery is the multi-crate split per Agent A.4. |
| `crates/core/src/grammar/host.rs` (584 LOC) | 13 | violated (>500 LOC) | SPLIT into `host/{mod,observational,pipeline,directives}.rs` |
| `crates/core/src/lower/expression/{mod,wrap}.rs` (539, 731 LOC) | 13 | violated | SPLIT |
| `crates/core/src/lower/value_expr/atom.rs` (590 LOC) | 13 | violated | SPLIT |
| `crates/core/src/css_types.rs` | 13 | violated (grammar-named at root) | RELOCATE per Lock 14 |
| `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/` (file + sibling directory) | 13 | violated (`feedback_directory_modules`) | RESTRUCTURE to `pipeline/mod.rs` |
| `crates/ir/src/types/grammar.rs` (584 LOC) | 13 | violated | SPLIT into `types/grammar/{def,accessors,serde}.rs` |
| `crates/ir/src/registry/struct.rs` (391 LOC) | 13 | clean (under 500 LOC) | n/a (rename per Lock 2) |
| `crates/ir/src/passes/csp_strategy/mod.rs` (1361 LOC) | 13 | violated | SPLIT |
| `crates/ir/src/passes/recognizers/grammar_facts.rs` (1530 LOC) | 13 | violated | SPLIT |
| `crates/ir/src/passes/materialization/classify.rs` (843 LOC) | 13 | violated | SPLIT |
| `crates/ir/src/passes/types/mod.rs` (786 LOC) | 13 | violated | SPLIT (rename + split) |
| `crates/ir/src/passes/types/registry.rs` (510 LOC) | 13 | violated | SPLIT |
| `crates/ir/src/passes/csp_domains.rs` (500 LOC) | 13 | borderline | SPLIT recommended |
| `crates/ir/src/passes/payload/layout.rs` (514 LOC) | 13 | violated | SPLIT |
| `crates/ir/src/passes/audit/payload_coverage.rs` (585 LOC) | 13 | violated | SPLIT |
| `crates/bbnf-path/src/path_macro.rs` (639 LOC) | 13 | violated | SPLIT |
| `crates/bbnf-path-ts/src/compile.rs` (474 LOC) | 13 | borderline | extract shared `path-core` per Lock 7 |

### Silent

(none)

**Verdict (Pass A, Lock 13)**: violated extensively. ~13 SPLIT obligations
across Pass A scope; one library-crate root concern that overlaps Agent
A.4 territory.

---

## Lock 14 — Full grammar generalisation; zero overfitting

**Statement.** Generic crates carry ZERO `match grammar { Json => ..., CssL4
=> ..., ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types
in their public APIs.

### Honoured

| Path:line | Evidence |
|---|---|
| `crates/core/src/imports/` | no grammar-specific code; pure import resolution |
| `crates/core/src/lower/` | walks the BBNF CST generically; no grammar-named arms |
| `crates/core/src/path/{ir,error,type_check,schema,executor,wildcard,variant_select,cursor,ascent}.rs` | typed-path machinery is grammar-marker-parameterised; no per-grammar arms in code |
| `crates/ir/src/types/{node,rule,map_expr,fn_descriptor,recognizer_configs,type_desc_interner}.rs` | grammar-agnostic alphabets |
| `crates/ir/src/dag/*` | DAG types are grammar-agnostic |
| `crates/ir/src/passes/sets/*` | set-analysis is grammar-agnostic |
| `crates/ir/src/passes/transform/*` | transforms are grammar-agnostic |

### Violated

| Path:line | Lock | Status | Surgery |
|---|---|---|---|
| `crates/core/src/css_types.rs` (entire file, 66 LOC, `pub fn parse_hex_color`) | 14 | violated (grammar-specific code at library root) | MOVE-TO per-grammar declaration crate (`crates/<css-grammar>/src/host.rs`) |
| `crates/core/src/path/markers.rs:14-30` (`Json`, `CssL4`, `Sheets`, `Bbnf` ZSTs) | 14 | violated (grammar-named types in generic crate) | RELOCATE markers to per-grammar declaration crates; the path crate carries only the `GrammarMarker` trait. |
| `crates/ir/src/registry/strategy.rs:130-185` (`PRODUCTION_MANIFEST_TABLE`: 9 hardcoded grammar idents + Rust paths) | 14 | violated (hardcoded grammar table in IR) | The IR crate must read from `[workspace.metadata.bbnf-strategy]` at xtask-regen time, NOT a hardcoded `static` table compiled into the IR crate. |
| `crates/ir/src/passes/audit/payload_coverage.rs:67-77` (`enum GrammarAuditTag { Json, CssL4, Sheets, Bbnf, Custom(&'static str) }`) | 14 | violated (named arms per grammar) | MERGE-WITH `Custom(&'static str)` — drop the named arms; every grammar is `Custom` keyed by its identifier. |
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` (192 LOC, entire file) | 14 | violated (grammar-named module in generic crate) | DELETE — generalise as a structural-shape miner with a per-grammar `[recognizer]` config in workspace metadata, OR move to a per-grammar recognizer crate. |
| `crates/ir/src/passes/profile.rs:26, 108` (`bbnf_shape_templates: Vec<BbnfShapeTemplate>` + `mine_bbnf_shape_templates` import) | 14 | violated (grammar-named field on generic struct) | DELETE the field; arrives via the generalised recogniser pipeline. |
| `crates/bbnf-path/src/registry.rs:132-135` (`match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }`) | 14 | violated (match on grammar-name strings) | MERGE-WITH workspace metadata table; the macro consumes a `RegistryDescriptor` per grammar. |
| `Cargo.toml [workspace.metadata.bbnf-strategy] grammars = [...]` | 14 | borderline (per-X table is the Lock-14-conforming shape — but the IR-side `PRODUCTION_MANIFEST_TABLE` must consume it, not mirror it) | KEEP the metadata table; remove its Rust-source mirror. |

### Silent

| Path:line | Lock | Status | Note |
|---|---|---|---|
| (across Pass A scope) | 14 | silent | the per-grammar runtime modules (`crates/core/src/runtime/<g>/`) are Pass B scope but emit-from-template under Lock 14; their template substrate must NOT contain grammar idents. The Pass A's IR side is verifiable; Pass B's runtime side surfaces in the synthesizer. |

**Verdict (Pass A, Lock 14)**: violated at 7 sites in Pass A scope. Each
violation is the prototypical Lock 14 fault (match-arm or named-module or
named-type in a generic crate).

---

## §15 — Cross-lock summary table

| Lock | Pass A status | Major surgery loci |
|---|---|---|
| 1 (tape dead) | substantively honoured; ~9 narrative-residue scrubs | grammar/mod.rs, schema/*, path/{ascent,variant_select}, types.rs, ir/types/{type_desc,grammar} |
| 2 (Layout canon) | violated; rename obligation across IR | ir/passes/types → layout; TypeDesc/StructLayout/TypeMap retire |
| 3 (cursor-parse + byte-skip unified) | honoured | path/cursor.rs |
| 4 (per-domain orthogonal optimization) | honoured | (n/a) |
| 5 (IR + per-backend lower) | substantively honoured; 1 redress (Rust-path strings out of IR) | ir/registry/strategy.rs |
| 6 (xtask emits committed source artefacts) | honoured | (n/a) |
| 7 (consolidated path crate) | violated | bbnf-path + bbnf-path-ts + crates/core/src/path/ → path / path-core / path-ts |
| 8 (surpass SOTA) | honoured (Pass A is pre-codegen) | (n/a) |
| 9 (slice-borrow primary) | violated at 1 site | grammar/mod.rs:57 Box::leak |
| 10 (Pratt + SIMD auto-detected) | honoured | (n/a) |
| 11 (path-deps for incubating sister crates) | violated | parse-that, bbnf-regex, csp-solver dep-form |
| 12 (ser + gorgeous archive) | silent (Pass C scope) | (n/a) |
| 13 (no god directories) | violated extensively (~13 SPLIT obligations) | host.rs, lower/expression/*, lower/value_expr/atom, ir/types/grammar, ir/passes/{csp_strategy,recognizers/grammar_facts,materialization/classify,types/{mod,registry},csp_domains,payload/layout,audit/payload_coverage}, bbnf-path/path_macro, pipeline.rs+pipeline/ |
| 14 (full grammar generalisation) | violated at 7 sites | css_types.rs, path/markers.rs, ir/registry/strategy.rs:130-185, ir/passes/audit/payload_coverage.rs:67-77, ir/passes/recognizers/shape_dict_bbnf.rs, ir/passes/profile.rs:26+108, bbnf-path/registry.rs:132-135 |

---

## §16 — Punch list (ordered for synthesizer)

1. **Lock 14 redresses** (the architectural lock; deferring would
   re-pollute every later edit):
   - DELETE `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`
   - MOVE `crates/core/src/css_types.rs` to per-grammar declaration crate
   - RELOCATE `crates/core/src/path/markers.rs` per-grammar ZSTs
   - RETIRE `PRODUCTION_MANIFEST_TABLE` to workspace-metadata read
   - MERGE `GrammarAuditTag` named arms into `Custom`
   - DROP `bbnf_shape_templates` field
   - REWRITE `bbnf-path/src/registry.rs:132-135` to consume metadata
2. **Lock 7 restructure**: rename + split path crate triplet; absorb
   `crates/core/src/path/` runtime executor.
3. **Lock 11**: bring `parse-that` + `bbnf-regex` into workspace as
   path-deps; switch `csp-solver` to path-dep.
4. **Lock 2 rename**: `passes/types/` → `passes/layout/`; TypeDesc /
   StructLayout / TypeMap retire to `Layout` vocabulary; one coordinated
   pass.
5. **Lock 13 splits**: ~13 god-module SPLIT obligations across Pass A scope.
6. **Lock 9 redress**: address the `Box::leak` in `grammar/mod.rs:57`.
7. **Lock 1 narrative scrub**: ~9 sites where dead-substrate phrases linger.
