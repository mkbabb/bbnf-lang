# Pass A — Agent A.1 — Inventory

Date: 2026-05-03
Lens: Exhaustive catalogue of every file in Pass A scope.
Scope source: `docs/restart/PASS-A-PARSE-FRONT.md` §Pass A Scope.

The inventory carries one row per source file. `Author intent` is taken from
the top-of-file / module docstring; classification (KEEP / KEEP-MODIFY /
ABROGATE) is the synthesis-orchestrator's task and not undertaken here.

`parse-that` and `bbnf-regex` are sibling-repository concerns at
`/Users/mkbabb/Programming/parse-that/rust/{parse_that,regex}`; they enter
Pass A scope as path-dep consumers (today: registry-version deps). Their
file-by-file inventory is referenced as the boundary surface only.

`crates/core/src/source/`, `crates/core/src/parse/`, `crates/core/src/host/`
named in the Pass A directive do not exist on disk at HEAD. The closest
extant siblings are `crates/core/src/imports/` (source acquisition for
`@import`-driven module graphs), `crates/core/src/lower/` (BbnfView →
GrammarIR), and `crates/core/src/grammar/host.rs` (host-fn dispatch fused
into the grammar tree). The directive describes the post-restart shape; the
inventory below describes the as-found shape.

---

## §A — Core parse-front (`crates/core/src/`)

### A.1 — `lib.rs` + leaf modules

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `crates/core/src/lib.rs` | 38 | `types`, `css_types`, `grammar`, `generate`, `backend`, `graph`, `imports`, `lower`, `path`, `pipeline`, `runtime` | every consumer of the `bbnf` crate | re-export `types::*` + `generate::*` + `graph::*`; concrete pubs for the rest | settled aggregator | re-export hub; B5 retired the `extern crate self as bbnf` self-alias |
| `crates/core/src/types.rs` | 136 | `AST`, `RuleEntry`, `ImportDirective`, plus the meta-AST surface | lower, pipeline, analysis | "AST + RuleEntry + ImportDirective"; one file | re-exported via `pub use types::*` | "the canonical CST envelope passed between bbnf-bootstrap and lower" |
| `crates/core/src/css_types.rs` | 66 | `parse_hex_color(&str) -> u32` | CSS L4 generated grammar's `hex` rule map | "single source of truth: one host shim, one resolution path" | grammar-named at the library root | "host shim for CSS L4's `-> parse_hex_color` map" |

### A.2 — `crates/core/src/grammar/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `grammar/mod.rs` | 67 | `parse(source: &str) -> Option<GrammarExtract<'_>>`, sub-modules `generated`, `host`, `schema` | bootstrap, pipeline, analysis | bootstrap-direct `BbnfBootstrap::parse` is the canonical bootstrap | leaks input to satisfy `'static` ownership for observational callers | "BBNF grammar parser. Single-call parse: generated tape-first bootstrap parser + extraction" — narrative is stale (tape is dead) |
| `grammar/host.rs` | 584 | `extract_observational`, `extract_pipeline`, `GrammarSink` trait + 2 impls | `parse`, pipeline | every directive-bearing compound resolves through one of `GrammarSink`'s six emit calls | god module per `feedback_no_god_modules` | "Grammar extraction: BBNF struct-direct document → observational `GrammarExtract` + pipeline-direct walkers" |
| `grammar/schema/mod.rs` | 21 | `pub use model::*`; `pub mod build, emit` | grammar/host, generate | model is the schema's source of truth | re-export hub | "schema description for typed-record materialisation" |
| `grammar/schema/model.rs` | 147 | `Schema`, `SchemaRule`, `<Rule>View` family | schema/build + emit | named `View` per rule | post-W2 schema | "tape-first AC.2 emitter — schema helpers emit impls on tape-backed records" — narrative is stale |
| `grammar/schema/build.rs` | 376 | `build_schema(ir) -> Schema` | grammar/schema/emit, codegen | one schema per IR; cached | per-rule `<Rule>View` emission | "per-rule `<Rule>View` family under the tape-first AC.2" — stale |
| `grammar/schema/emit/mod.rs` | (≤30) | sub-module re-exports | callers in codegen | one emitter per backend | rust-only today | re-export shell |
| `grammar/schema/emit/rust/mod.rs` | 31 | rust schema-emitter aggregator | codegen | rust-emit is the only path | settled | re-export shell |
| `grammar/schema/emit/rust/directives.rs` | 14 | `emit_directives_block` | rust schema emit | one block per grammar | settled | "directive emission helper" |
| `grammar/schema/emit/rust/identifiers.rs` | 14 | `emit_idents_block` | rust schema emit | one block per grammar | settled | "ident-emission helper" |
| `grammar/schema/emit/rust/shared.rs` | 69 | shared rust-emit helpers | rust schema emit | per-rule shape uniform | settled | "Post-Tranche AC.2 rewrite: schema helpers emit impls on tape-backed records" — stale |
| `grammar/generated/mod.rs` | (≈50) | `pub mod <ident>`, `pub use bbnf::*` aggregator | bbnf-bootstrap, pipeline, runtime | xtask-emitted output; never hand-edited | settled per-grammar gates; BBNF aggregator is asymmetric | "Generated parser modules. Output of `cargo xtask regen`." |
| `grammar/generated/bbnf.rs` | (~22 K) | `BbnfBootstrap`, `BbnfParser` types + parse fns | bootstrap, lower | byte-identical to xtask output | settled | xtask emission; bootstrap shape |
| `grammar/generated/json.rs` | (~3.4 K) | `JsonParser`, `JsonGrammar` + parse fns | runtime/json | byte-identical | settled | xtask emission |
| `grammar/generated/css_l4.rs` | (~107 K) | `CssL4Parser` | runtime/css_l4 | byte-identical | settled | xtask emission |
| `grammar/generated/google_sheets.rs` | (~?) | `GoogleSheetsParser`, `GoogleSheetsGrammar` | runtime/google_sheets | byte-identical | settled | xtask emission |
| `grammar/generated/{bnf,csv,ebnf,math,css_pretty}.rs` | (each ~1-3 K) | `<G>Parser`, `<G>Grammar` | per-grammar runtime | byte-identical | settled cohort | xtask emission |

`grammar/generated/` is OUT OF Pass A scope per the directive (Pass B scope).
The five rows above are listed for boundary completeness — Pass A's scope
is `grammar/` source side, not `grammar/generated/`.

### A.3 — `crates/core/src/lower/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `lower/mod.rs` | 356 | `lower_to_ir(ast, scc, directives, closure_defs) -> GrammarIR`, `DirectiveSet`, `LowerCtx` (crate-private) | pipeline | one entry; `LowerCtx` carries closure env + value env | settled | "Lowering pass: BbnfView → GrammarIR. No intermediate Expression AST" |
| `lower/string_interner.rs` | 35 | `StringInterner` | lower | dedup + intern | settled | helper for `LowerCtx` |
| `lower/fn_table.rs` | 20 | `FnTable` | lower | one fn-id allocator per grammar | settled | helper |
| `lower/metadata.rs` | 106 | `build_rule_meta` | lower | rule-meta is built once per rule | settled | metadata extraction helper |
| `lower/expression/mod.rs` | 539 | `lower_rhs`, `lower_term` | lower | every CST descent goes through `BbnfView` accessors | god module per `feedback_no_god_modules` (>500 LOC) | "expression lowering: terms, alternations, repetitions, factors, mappings" |
| `lower/expression/alt.rs` | 184 | alt-branch lowering | expression/mod | branch-tag dispatch | settled | "Alt lowering with branch-tag awareness" |
| `lower/expression/closures.rs` | 91 | closure-call lowering | expression/mod | beta-reduction at compile time | settled | "first-class closure call lowering" |
| `lower/expression/pratt.rs` | 329 | Pratt operator-chain detection + lowering | expression/mod | mined from grammar shape; no `@pratt` directive | settled | "Pratt detection (BA W3 thesis)" |
| `lower/expression/repeat.rs` | 174 | repeat-shape lowering | expression/mod | one form per repeat-kind | settled | "repeat lowering" |
| `lower/expression/wrap.rs` | 731 | wrap-shape detection + MapExpr lowering + payload deduction | expression/mod | three concerns colocated | god module (>500 LOC) | "Wrap lowering. Combines wrap detection, MapExpr lowering, payload deduction" |
| `lower/value_expr/mod.rs` | 178 | `lower_value_expr` | expression, lower | one entry; recursive | settled | "value-expression lowering: the `->` sub-language" |
| `lower/value_expr/atom.rs` | 590 | atom-level lowering | value_expr | atom is the leaf form | god module (>500 LOC) | "atom: literal, projection, type lowering" |
| `lower/value_expr/literals.rs` | 58 | literal lowering | atom | one form per literal kind | settled | "literal lowering helper" |
| `lower/value_expr/precedence.rs` | 340 | operator-precedence lowering | value_expr | precedence ladder | settled | "precedence lowering for value expressions" |
| `lower/value_expr/simple_kinds.rs` | 235 | simple-kind classification | value_expr/atom | every BbnfValue kind has one arm | settled, but L185 carries a "Defensive fallback" comment | "classify simple BbnfValue kinds for atom lowering" |
| `lower/value_expr/unwrap.rs` | 256 | unwrap helpers | value_expr/atom | every Compound kind has one arm | settled | "unwrap helpers for value-expression lowering" |
| `lower/value_expr/view_walk.rs` | 43 | view-walking helpers | value_expr | shared walking patterns | settled | "view-walking helpers for value-expression lowering" |
| `lower/view_walk.rs` | 257 | `find_descendant_by_kind`, `find_rhs_expression_descendant` | grammar/host, lower/value_expr | structural traversal helpers | settled | "BBNF view-walking helpers shared by host extraction + lowering" |

### A.4 — `crates/core/src/path/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `path/mod.rs` | 60 | re-exports of `ascent`, `cursor`, `error`, `executor`, `ir`, `markers`, `schema`, `type_check`, `variant_select`, `wildcard` | runtime/{json,css_l4,bbnf,google_sheets}, bbnf-path | one re-export hub; sub-modules disjoint | settled | "Compile-time-typed path IR. AZ-IV.W2.1 lands the typed-path surface" |
| `path/ir.rs` | 323 | `PathSegment<'a>`, `Path<'a>`, `TypedPath<G, T>`, `OwnedPathSegment`, `IntoPathSegment` | bbnf-path proc-macro, runtime/* | borrowed alphabet primary; owned escape | settled | "Path IR types — borrowed `Path<'a>`, owned `TypedPath<G, T>`" |
| `path/markers.rs` | 30 | `Json`, `CssL4`, `Sheets`, `Bbnf` ZSTs | bbnf-path, runtime | one ZST per grammar | settled — but every existing grammar is hardcoded here | "Grammar markers for `TypedPath`" |
| `path/error.rs` | 143 | `PathError`, `PathErrorReason` | type_check, executor | every error path produces a `PathError` | settled | "Path error types" |
| `path/type_check.rs` | 338 | `check_path`, `check_path_against_registry` | bbnf-path proc-macro, type_check tests | one offline checker | settled | "offline `check_path_against_registry` entry point (W2.1)" |
| `path/schema.rs` | 168 | `PathSchema`, `GrammarMarker` traits | path/cursor, path/executor | grammar-marker trait abstracts over `TypedPath` | settled | "PathSchema trait abstracting over `TypedPath` and future dynamic paths (W3.1)" |
| `path/cursor.rs` | 431 | `PathCursor`, `Decision`, `SegmentKind` | runtime/parse_with, path/executor | cursor decides per-segment | settled | "PathCursor state machine + Decision/SegmentKind alphabet (W3.1)" |
| `path/executor.rs` | 171 | `PathExecutor` | runtime/parse_with | top-level orchestrator | settled | "PathExecutor top-level orchestrator (W3.1)" |
| `path/ascent.rs` | 277 | `AscentStrategy`, `DefaultAscent`, `HybridSidecar`, `InStructPointer`, `RootTraversal` | future ascent consumers | three impls cover the design space | settled — micro-bench picked default | "ascent strategies for parent-pointer queries (W2.5)" |
| `path/variant_select.rs` | 90 | `select_variant` | path/cursor, path/executor | typed-enum variant resolver | settled | "typed-enum variant resolver (W2.5)" |
| `path/wildcard.rs` | 203 | `WildcardConfig`, `WildcardIter`, `WithAnchors`, `ends_with_wildcard`, `DEFAULT_WILDCARD_DEPTH_CAP` | executor | wildcard lazy-iter | settled | "wildcard lazy-iter execution + depth cap (W2.5)" |

### A.5 — `crates/core/src/imports/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `imports/mod.rs` | 14 | `ImportError`, `load_module_graph`, `ImportCycle`, `ModuleData`, `ModuleRegistry`, `ResolvedImport` | pipeline | one module graph per compile | settled | "Import resolution for BBNF grammars" |
| `imports/errors.rs` | 98 | `ImportError` | imports/* | one error type | settled | error-shape for imports |
| `imports/loader.rs` | 186 | `load_module_graph` | pipeline | one entry | settled | "Loader: walks `@import` directives across files" |
| `imports/registry.rs` | 108 | `ImportCycle`, `ModuleData`, `ModuleRegistry`, `ResolvedImport` | imports/loader | one registry per compile | settled | registry shape |
| `imports/resolve.rs` | 160 | per-file resolution helpers | imports/loader | one pass per file | settled | resolve helpers |

### A.6 — Pipeline overlap (Pass C of A scope; `pipeline/` is mostly Pass B)

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `pipeline.rs` | (~105) | `CompileTarget`, `CompileRequest`, `CompileOutput`, `CompileError`, `PipelineOptions` | external API | thin facade | violates `feedback_directory_modules` (file + sibling dir) | "thin facade over pipeline/" |
| `pipeline/compile/` | (multi-file) | `compile_grammar`, internals | external | one compile entry | settled | "compile pipeline" |

`pipeline/` as a whole is Pass B scope; its parser-front overlap (the
`directives::parse_to_pipeline_inputs` call at `pipeline/compile/mod.rs`)
is the boundary.

---

## §B — IR crate (`crates/ir/`)

### B.1 — IR root

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `crates/ir/src/lib.rs` | (~58) | re-exports `cost_config`, `dag`, `egraph`, `passes`, `recognizer`, `registry`, `rewrites`, `types`, `vm`; re-exports from `bbnf_regex::sets::charset::CharSet128`, `bbnf_regex::first::regex_first_chars` | every IR consumer (bbnf, bbnf-path, analysis) | "fully owned (no lifetimes), serializable via MessagePack" | settled aggregator | "Canonical Grammar IR for the BBNF compiler pipeline" |
| `crates/ir/src/cost_config.rs` | 234 | `CostConfig`, `CostConfig::from_env` | every cost-aware pass | env-driven cost knobs | settled | cost-config substrate |

### B.2 — `crates/ir/src/types/` — pure data definitions

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `types/mod.rs` | 59 | re-export hub | external IR consumers | one re-export point | settled | aggregator |
| `types/grammar.rs` | 584 | `GrammarIR` struct, accessors, MessagePack | every IR consumer | one IR shape; serde-stable | god module (>500 LOC) | "GrammarIR — the canonical container" |
| `types/node.rs` | 206 | `IrNode`, `AltBranch`, `AltDispatch`, `TokenDispatchArm`, `GrammarSpan` + walking helpers | every IR pass | enum is closed; walking is uniform | settled | "IR node alphabet + walking helpers" |
| `types/rule.rs` | 179 | `IrRule`, `RuleMeta`, `RuleDirectives`, `MemoStrategy`, `DispatchHint`, `PrettyHints`, `SubVariant` | every IR consumer | one rule per name; meta is rule-private | settled | "Rule + meta shape" |
| `types/map_expr.rs` | 177 | `MapExpr`, `MapBinOp`, `MapUnaryOp` | lower/value_expr, codegen | the user-facing `->` payload | settled | "MapExpr — the user-facing `->` payload" |
| `types/fn_descriptor.rs` | 58 | `FnDescriptor` | lower, codegen, audit | host-fn descriptor enum | settled | "host-fn descriptor enum" |
| `types/type_desc.rs` | 212 | `TypeDesc` | every type-aware consumer | backend-agnostic | retired-term-bearing per Lock 2 | "backend-agnostic type descriptor" |
| `types/type_desc_interner.rs` | 101 | `TypeDescInterner` | lower, codegen | one interner per IR | settled | "TypeDesc interning cache" |
| `types/recognizer_configs.rs` | 101 | `DelimScanConfig`, `KeyDispatchConfig`, `KeyDispatchMatch`, `DetectedBranch`, `KeyClass`, `key_class_regex_pattern` | recognizer pipeline | configurable recognizer alphabet | settled | "recognizer config shapes" |

### B.3 — `crates/ir/src/registry/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `registry/mod.rs` | 39 | `EmitStrategy`, `FieldSource`, `LayoutKind`, `StructField`, `StructLayout`, `StructRegistry`, `SubstrateBinding` | pipeline, codegen, audit | one registry per IR | settled | re-export hub |
| `registry/struct.rs` | 391 | `StructLayout`, `StructRegistry`, `FieldSource`, `LayoutKind`, `StructField` | codegen, audit | typed struct shapes per Named rule | settled but `StructLayout` term is retired per Lock 2 | "StructLayout + StructRegistry — typed struct shapes per Named rule" |
| `registry/strategy.rs` | 334 | `EmitStrategy`, `SubstrateBinding`, `ManifestStrategyEntry`, `PRODUCTION_MANIFEST_TABLE` | codegen | the single backend-shared substrate selector | hardcoded 9-grammar manifest table at L130-185; mirrored in workspace metadata | "AZ-I.W2-act.A — `EmitStrategy` — IR-level codegen-time substrate selector" |

### B.4 — `crates/ir/src/dag/`

| File | LOC | Purpose | Author intent |
|---|---:|---|---|
| `dag/mod.rs` | 186 | `GrammarDag` struct + accessors + `ensure_dag` test helper | DAG aggregator |
| `dag/build.rs` | 143 | `GrammarDagBuilder` — recursive-walk hash-cons builder | "DAG builder" |
| `dag/extract.rs` | 79 | DAG-tree extraction back to `IrNode` for tests | "DAG → IrNode tests helper" |
| `dag/intern.rs` | 54 | Hash-cons interner detail | "interner detail" |
| `dag/node.rs` | 178 | `DagNode` enum + `NodeId(u32)` | "DAG node alphabet" |

### B.5 — `crates/ir/src/passes/` (consolidated row count; per-file metadata follows)

| Pass directory | Files | Top files (LOC) | Notes |
|---|---:|---|---|
| `passes/audit/` | 2 | `payload_coverage.rs` (585) | typed-`->` marker coverage; carries hardcoded `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf,Custom}` enum L67-77 — Lock 14 violation |
| `passes/context/` | 2 | `mod.rs` (~?), `facts.rs` (~?) | context-fact pass |
| `passes/csp_strategy/` | 7 | `mod.rs` (1361), `components.rs` (339) | god module (>500 LOC at `mod.rs`) |
| `passes/csp_strategy/constraints/` | 5 | dispatch / engine / layout / shape | CSP constraint sub-units |
| `passes/facts/` | 1 | `mod.rs` (271) | fact-mining hub |
| `passes/inspect/` | 5 | `mod.rs`, `leading.rs`, `literal.rs`, `resolve.rs`, `unwrap.rs`, `walk.rs` | inspection helpers |
| `passes/materialization/` | 4 | `classify.rs` (843) | god module |
| `passes/patterns/` | 1 | `mod.rs` (201) | pattern miners hub |
| `passes/payload/` | 4 | `layout.rs` (514), `mod.rs`, `named_types.rs`, `scalar_routing.rs` | god module at `layout.rs` |
| `passes/recognizers/` | 18 | `grammar_facts.rs` (1530), `mod.rs` (362), `operator_chain.rs` (415), `pattern_alphabet.rs` (383), `shape_dict_bbnf.rs` (192) | god module at `grammar_facts.rs`; **`shape_dict_bbnf.rs` is grammar-named — Lock 14 violation** |
| `passes/recognizers/shape_dispatch/` | 12 | `mod.rs` (354), `unordered.rs` (224), `flat.rs` (237), `keyword.rs`, `array.rs`, `string.rs`, `number.rs`, `object.rs`, `pratt.rs`, `scalar.rs`, `arglist.rs`, `wrap.rs`, `alt_dispatch.rs`, `hregex.rs` | shape-dispatch sub-units |
| `passes/sets/` | 7 | `mod.rs`, `deps.rs`, `scc.rs`, `first_sets.rs` (431), `follow.rs` (283), `sort.rs` (366), `factor_lookahead.rs` (361), `fingerprint.rs` (259), `structural_alphabet.rs` (437) | set-analysis foundation |
| `passes/sets/dispatch/` | 7 | `mod.rs` (131), `annotate.rs` (165), `build.rs` (158), `constraint.rs` (56), `domain.rs` (82), `eligibility.rs` (158), `first_set.rs` (127) | dispatch-table generation |
| `passes/transform/` | 7 | `alias.rs` (109), `fuse.rs` (388), `inline.rs` (331), `optimize.rs` (238), `pattern_dedup.rs` (439), `prune.rs` (164) | structural normalizer |
| `passes/transform/fuse_token/` | 3 | `mod.rs` (107), `detect.rs` (171), `factor.rs` (339) | fuse-token sub-pass |
| `passes/types/` | 3 | `mod.rs` (786), `generate.rs` (421), `obligation.rs` (204), `registry.rs` (510), `subvariants.rs`, `type_map.rs` (203) | god module at `mod.rs`; god module at `registry.rs`; "TypeMap" carries retired vocabulary (Lock 2) |
| `passes/types/constraint/` | 7 | `mod.rs`, `alt.rs`, `domain.rs`, `grounds.rs`, `operators.rs` (213), `reference.rs`, `revise.rs`, `seq.rs` | type-constraint sub-pass |
| (top-level `passes/*.rs`) | 9 | `csp_domains.rs` (500), `inline_trace.rs` (214), `lr.rs` (321), `metadata.rs`, `path_check.rs` (252), `prefix.rs` (477), `profile.rs` (191), `regex_info.rs`, `span.rs` (247) | cross-pass single-files |

### B.6 — `crates/ir/src/recognizer/`, `egraph/`, `rewrites/`, `vm/`

| File | LOC | Purpose |
|---|---:|---|
| `recognizer/mod.rs` | 303 | `RecognizerInfo` trait + four wrapper impls (Regex, Literal, Token, DispatchGroup, DelimScan) |
| `recognizer/facts.rs` | 94 | `RecognizerKind` + `Width` + `RecognizerInfo` sub-types |
| `recognizer/plans.rs` | 31 | `ExecutionPlan` + `ExecutionPlanKind` |
| `egraph/mod.rs` | 128 | `build_and_saturate` orchestrator |
| `egraph/build_egraph.rs` | 106 | `insert_ir` |
| `egraph/cost.rs` | 164 | `GrammarCostModel` |
| `egraph/interner.rs` | 130 | `SharedStrings` |
| `egraph/node.rs` | 77 | `GrammarENode` enum (with `#[derive(Language)]`) |
| `egraph/write_back.rs` | 378 | `extract_ir_node` + `write_back_optimized` |
| `egraph/analysis/mod.rs` | 290 | `GrammarAnalysis` + `EClassFacts` + `WidthBound` |
| `egraph/analysis/facts.rs` | 274 | `EClassFacts` impl |
| `egraph/rules/mod.rs` | 98 | `default_rules()` factory |
| `egraph/rules/regex.rs` | 434 | regex-related rewrite rules |
| `egraph/rules/suffix.rs` | 146 | `CommonSuffixFactor` rewrite |
| `egraph/rules/universal.rs` | 418 | universal rewrites |
| `rewrites/mod.rs` | 237 | `Rule` + `RuleSet` + `RewriteRuleId` + RON load/save |
| `rewrites/base.rs` | 225 | `Alphabet` + `Atom` + `Pattern` + `PatternRef` + `Witness` |
| `rewrites/path_seed.rs` | 220 | `PATH_SEED_GRAMMAR` |
| `rewrites/rank.rs` | 253 | `RankConfig` + `rank` |
| `rewrites/schema.rs` | 150 | RON schema |
| `rewrites/tiering.rs` | 111 | `RuleClass` + `classify` |
| `vm/*` | (multi-file) | bytecode VM | (12 files; bytecode + compiler + interpreter sub-trees) |

The IR `vm/` sub-tree (bytecode interpreter) overlaps Pass B (runtime
execution) more than Pass A; listed here for completeness because it lives
under `crates/ir/`.

---

## §C — Sister parser crates

### C.1 — `crates/bbnf-path/` (proc-macro)

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `bbnf-path/src/lib.rs` | 78 | `#[proc_macro] pub fn path(...)` | every `path!(...)` invocation | one entry | settled | proc-macro entry |
| `bbnf-path/src/path_macro.rs` | 639 | the `path!` lex/lower/validate body | lib | one cohesive macro body | god module (>500 LOC) | "lex/lower/validate body of the `path!` expansion" |
| `bbnf-path/src/registry.rs` | 201 | compile-time fixture `StructRegistry` lookups | path_macro | T4 deferred — registry is currently fixture | settled — but carries `match grammar { "json" => ..., "css_l4" => ... }` at L132-135 (Lock 14 violation) | "synthetic-grammar fixture registry" |

### C.2 — `crates/bbnf-path-ts/` (cdylib)

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `bbnf-path-ts/src/lib.rs` | 133 | `#[wasm_bindgen]` exports `compile_path`, `execute_path` | TS/wasm callers | TS wire surface | settled | "TS template-tag binding" |
| `bbnf-path-ts/src/compile.rs` | 474 | `compile_path` body | lib | mirror of `bbnf-path/src/path_macro.rs` lex/lower/validate logic | proc-macro vs cdylib mirror — known | TS-side mirror of the Rust macro |
| `bbnf-path-ts/src/fixture.rs` | 248 | `GrammarFixture` registry | compile.rs | mirror of `bbnf-path/src/registry.rs` | mirror | "per-grammar fixture registry — TS" |
| `bbnf-path-ts/src/schema.rs` | 113 | `TypedPathPayload`, `OwnedSegmentPayload`, `PathErrorPayload` | TS | byte-identical wire types | settled | "byte-identical wire types" |
| `bbnf-path-ts/src/template_tag.rs` | 44 | `TEMPLATE_TAG_JS` | TS callers | the JS shim string | settled | "the JS shim string the TS template tag wraps around" |

### C.3 — `crates/bootstrap/`

| File | LOC | Public-API surface | Dependents | Invariants | Current state | Author intent |
|---|---:|---|---|---|---|---|
| `bootstrap/src/lib.rs` | 28 | `pub use bbnf::grammar::generated::BbnfBootstrap` | external bootstrap callers | re-export shim only | settled | "one-line re-export of `BbnfBootstrap`" |
| `bootstrap/src/bin/dump_ir.rs` | 185 | dev binary | dev | dump IR for a grammar source as JSON | settled | "dev binary: dump IR as JSON for diffing" |
| `bootstrap/src/bin/cost_grid_sweep.rs` | 127 | dev binary | dev | sweep `CostConfig` knobs | settled | "dev binary: cost-grid sweep" |
| `bootstrap/src/bin/debug_parse.rs` | 125 | dev binary | dev | parse + pretty-print extract | settled | "dev binary: parse + pretty-print extract" |

### C.4 — Sibling repos (Pass A boundary surface only)

| Repo | Path | Workspace? | Pass A inventory burden |
|---|---|---|---|
| `parse-that` | `/Users/mkbabb/Programming/parse-that/rust/parse_that/` | No — sibling repo, registry-version dep | API surface: `Parser`, `Span`, combinator alphabet. Consumed by `crates/core` indirectly via grammar/host extraction (`use parse_that::Span`) and codegen. |
| `bbnf-regex` | `/Users/mkbabb/Programming/parse-that/rust/regex/` | No — sibling repo, registry-version dep | API surface: `CharSet128`, `regex_first_chars`, `lex_path`, `PathToken`, HIR types. Consumed by `crates/ir` (re-exports `CharSet128`, `regex_first_chars`), `crates/bbnf-path`, `crates/bbnf-path-ts`. |

The two sibling crates are NOT inventoried file-by-file; their relationship
to Pass A is an external boundary surface. The directive's Lock 11
("path-deps for incubating sister crates") demands they become workspace
path-deps until stable.

---

## §D — Grammar source tree (`grammar/`)

| Path | Files | Author intent |
|---|---:|---|
| `grammar/bbnf/bbnf.bbnf` | 1 | BBNF self-host source |
| `grammar/bbnf/expressions.bbnf` | 1 | BBNF expression-sub-grammar (split file) |
| `grammar/bbnf/types.bbnf` | 1 | BBNF type-sub-grammar (split file) |
| `grammar/json/json.bbnf` | 1 | JSON grammar source |
| `grammar/css/pretty.bbnf` | 1 | CSS-pretty grammar source |
| `grammar/css/l4/...` | (multi-file) | CSS L4 grammar source (sub-files; not directly enumerated by file find at `maxdepth 2`) |
| `grammar/google-sheets/google-sheets.bbnf` | 1 | Google Sheets formula grammar source |
| `grammar/ebnf/ebnf.bbnf` | 1 | EBNF grammar source |
| `grammar/bnf/bnf.bbnf` | 1 | BNF grammar source |
| `grammar/misc/csv.bbnf` | 1 | CSV grammar source (named "misc") |
| `grammar/misc/math.bbnf` | 1 | Math grammar source (named "misc") |
| `grammar/misc/math-ambiguous.bbnf` | 1 | Math grammar with ambiguity (test fixture) |
| `grammar/misc/g4.bbnf` | 1 | g4-flavoured grammar (test fixture) |
| `grammar/misc/regex.bbnf` | 1 | Regex grammar (test fixture) |
| `grammar/misc/emoji.bbnf` | 1 | Emoji grammar (test fixture) |
| `grammar/misc/json-commented.bbnf` | 1 | JSON-with-comments fixture |
| `grammar/tests/google-sheets-formula-test-cases.md` | 1 | Sheets test-case markdown |

Layout: per-grammar directories, with a `misc/` catch-all for fixtures.

---

## §E — Workspace metadata (parser-relevant subset)

| TOML key | Where | Purpose |
|---|---|---|
| `[workspace.metadata.bbnf]` `grammars = [{ ident, path, features }, …]` | root `Cargo.toml` L29-37 | per-grammar declaration; xtask reads to know what to regen |
| `[workspace.metadata.bbnf-strategy]` `grammars = [{ idents, rust_builder_path, rust_document_path }, …]` | root `Cargo.toml` L46-58 | per-grammar emit-strategy binding; mirrors `PRODUCTION_MANIFEST_TABLE` |

Mirror: `crates/ir/src/registry/strategy.rs:130-185` carries
`PRODUCTION_MANIFEST_TABLE` — a Rust source-side mirror of the
`[workspace.metadata.bbnf-strategy]` table. The two must agree; the
agreement is enforced at xtask-regen time (the audit doc cites a synthetic-
grammar test).

---

## §F — File counts

| Region | File count |
|---|---:|
| `crates/core/src/{lib,types,css_types}.rs` | 3 |
| `crates/core/src/grammar/` (excl. generated/) | 9 |
| `crates/core/src/lower/` | 18 |
| `crates/core/src/path/` | 11 |
| `crates/core/src/imports/` | 5 |
| `crates/core/src/pipeline.rs` + `pipeline/` boundary | 1 + (multi) |
| `crates/ir/src/` | (~145, per MODULES audit) |
| `crates/bbnf-path/src/` | 3 |
| `crates/bbnf-path-ts/src/` | 5 |
| `crates/bootstrap/src/` | 4 |
| `grammar/` (`.bbnf` files; flat per-grammar layout) | 16 (incl. misc fixtures) |

Pass A scope spans roughly 200 hand-written source files (excluding
generated/). The IR crate carries the largest LOC weight (~17 K hand-
written + ~169 K generated downstream); the parse-front in `crates/core/src/`
contributes ~9 K LOC across the sub-trees enumerated above.
