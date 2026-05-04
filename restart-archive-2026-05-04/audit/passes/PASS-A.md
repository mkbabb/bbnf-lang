# Pass A — Parse Front (Greenfield Restart Synthesis)

Date: 2026-05-03
Pass A scope: source acquisition, parsing, lowering, IR, sister parsers
(parse-that, bbnf-regex), the path crate triplet, the bootstrap crate,
the `grammar/` source tree (per `docs/restart/PASS-A-PARSE-FRONT.md`).

This synthesis consumes the six per-agent reports under
`audit/restart/per-agent/pass-a-agent-{1..6}-*.md`. Where lenses agree, the
synthesis ratifies; where they diverge, the synthesis adjudicates and
cites the deciding lock or precept.

The greenfield mandate is settled — no quick solutions, no workarounds,
idiomatic gestalt approaches, architectural transpositions in the sake of
elegance / simplicity / performance. Pass A ratifies and classifies; it
does not relitigate.

---

## §1 — Pass A verdict ledger

Every file in Pass A scope appears below with a bucket from
{KEEP-OUTRIGHT, KEEP-MODIFY, ABROGATE-DELETE, ABROGATE-MOVE,
ABROGATE-REPLACE}. Source agents listed in parentheses. Successor named
when ABROGATE.

### 1.1 — `crates/core/src/{lib,types,css_types}.rs`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/core/src/lib.rs` | KEEP-MODIFY | A.2, A.3, A.4 | scrub `css_types` narration; narrow `pub use generate::*` glob; post Proposal 1 fracture, lib becomes a thin aggregator | n/a |
| `crates/core/src/types.rs` | KEEP-MODIFY | A.2, A.3 | scrub L90 tape comment; relocate to `bbnf-grammar` per Agent A.5 new facility #2 | post-Proposal-1: `bbnf-grammar/src/types.rs` |
| `crates/core/src/css_types.rs` | ABROGATE-MOVE | A.2, A.3, A.6 | Lock 14 violation (grammar-named host fn at library root); the docstring's "single source of truth" framing is itself the workaround | `crates/bbnf-grammar-css-l4/src/host.rs` (per-grammar declaration crate per Lock 14 footnote) |

### 1.2 — `crates/core/src/grammar/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `grammar/mod.rs` | KEEP-MODIFY | A.2, A.3, A.6 | tape phrases at L3, L7, L17 scrub; `Box::leak` at L57 is a Lock 9 workaround — synthesizer adjudicates between (a) public-API change to force `&'static`-able input and (b) introduce `parse_in(input, &bump)` arena variant | (post-restructure) `bbnf-grammar/src/lib.rs` |
| `grammar/host.rs` | KEEP-MODIFY | A.2, A.3 | SPLIT (584-LOC god module) → `host/{mod, observational, pipeline, directives}.rs`; FAIL-EXPLICIT at L387 wildcard `@debug` strip-prefix; scrub L558 "legacy bootstrap_parser shape" comment | (post-restructure) `bbnf-grammar/src/host/` |
| `grammar/schema/mod.rs` | KEEP-OUTRIGHT | A.1 | clean re-export hub | n/a |
| `grammar/schema/model.rs` | KEEP-MODIFY | A.2, A.3 | scrub L20 "tape-first AC.2" narrative | n/a |
| `grammar/schema/build.rs` | KEEP-MODIFY | A.2, A.3 | scrub L26 tape-first comment | n/a |
| `grammar/schema/emit/mod.rs` | KEEP-OUTRIGHT | A.1 | re-export shell | n/a |
| `grammar/schema/emit/rust/mod.rs` | KEEP-OUTRIGHT | A.1 | re-export shell | n/a |
| `grammar/schema/emit/rust/{directives,identifiers}.rs` | KEEP-OUTRIGHT | A.1 | small helpers | n/a |
| `grammar/schema/emit/rust/shared.rs` | KEEP-MODIFY | A.2, A.3 | scrub L3, L17 "tape-backed records" narrative | n/a |
| `grammar/generated/mod.rs` | KEEP-MODIFY | A.2, A.3 | drop `pub use bbnf::*` aggregator at L35 — Lock 14 asymmetry; BBNF accesses uniformly via `bbnf::grammar::generated::bbnf::BbnfBootstrap` | n/a |
| `grammar/generated/{bbnf,json,css_l4,css_pretty,csv,ebnf,bnf,math,google_sheets}.rs` | (Pass B scope) | A.1 boundary | xtask-emitted; out of Pass A. Listed for boundary completeness only | (Pass B owns) |

### 1.3 — `crates/core/src/lower/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `lower/mod.rs` | KEEP-MODIFY | A.4 | post-Proposal-1, relocates to `bbnf-parse/src/lower/mod.rs`; current 356 LOC is acceptable | (post-restructure) `bbnf-parse/src/lower/mod.rs` |
| `lower/string_interner.rs` | KEEP-OUTRIGHT | A.1, A.2 | small helper | (relocates) |
| `lower/fn_table.rs` | KEEP-OUTRIGHT | A.1, A.2 | small helper | (relocates) |
| `lower/metadata.rs` | KEEP-OUTRIGHT | A.1, A.2 | small helper | (relocates) |
| `lower/expression/mod.rs` | KEEP-MODIFY | A.2, A.3 | SPLIT (539 LOC god module) into `expression/{term, factor, mapping}.rs`; mod.rs ≤ 100 LOC | (relocates) |
| `lower/expression/alt.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/expression/closures.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/expression/pratt.rs` | KEEP-OUTRIGHT | A.1, A.2, A.3 | Lock 10 honoured (auto-detection); clean | (relocates) |
| `lower/expression/repeat.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/expression/wrap.rs` | KEEP-MODIFY | A.2, A.3 | SPLIT (731 LOC god module) into `wrap/{detect, map_expr, payload}.rs` | (relocates) |
| `lower/value_expr/mod.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/value_expr/atom.rs` | KEEP-MODIFY | A.2, A.3 | SPLIT (590 LOC god module) into `atom/{literal, projection, type}.rs` | (relocates) |
| `lower/value_expr/literals.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/value_expr/precedence.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/value_expr/simple_kinds.rs` | KEEP-MODIFY | A.2, A.3 | FAIL-EXPLICIT at L185 — replace defensive fallback with `unreachable!()` or fix upstream | (relocates) |
| `lower/value_expr/unwrap.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/value_expr/view_walk.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `lower/view_walk.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean (shared between grammar/host and lower/value_expr) | (relocates) |

### 1.4 — `crates/core/src/path/`

The path crate triplet (Lock 7 + Proposal 3) means most of `crates/core/src/path/`
relocates to `crates/path/src/runtime/` post-restart. Pass A ratifies the
relocation; the per-file dispositions are KEEP-MODIFY (with relocation as
the modification).

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `path/mod.rs` | ABROGATE-MOVE | A.4 (Lock 7) | re-export hub becomes part of `path-core/src/lib.rs` | `crates/path-core/src/lib.rs` |
| `path/ir.rs` | ABROGATE-MOVE | A.4 (Lock 7) | typed-path types relocate | `crates/path-core/src/ir.rs` |
| `path/markers.rs` | ABROGATE-DELETE | A.2, A.3, A.6 (Lock 14) | per-grammar ZSTs hardcoded — relocate to per-grammar declaration crates; the path crate carries only the `GrammarMarker` trait | per-grammar declaration crates emit `pub struct <G>;` |
| `path/error.rs` | ABROGATE-MOVE | A.4 (Lock 7) | `PathError`, `PathErrorReason` relocate | `crates/path-core/src/error.rs` |
| `path/type_check.rs` | ABROGATE-MOVE | A.4 (Lock 7) | `check_path` relocates | `crates/path-core/src/type_check.rs` |
| `path/schema.rs` | ABROGATE-MOVE | A.4 (Lock 7) | `PathSchema`, `GrammarMarker` trait relocates | `crates/path-core/src/schema.rs` |
| `path/cursor.rs` | ABROGATE-MOVE | A.2, A.3, A.4 | runtime executor relocates; inline test at L313-314 moves to `tests/` | `crates/path-core/src/runtime/cursor.rs` (post Lock 7); test at `crates/path-core/tests/path_cursor.rs` |
| `path/executor.rs` | ABROGATE-MOVE | A.2, A.3, A.4 | runtime executor relocates; inline test at L65-66 moves | `crates/path-core/src/runtime/executor.rs`; test at `tests/path_executor.rs` |
| `path/ascent.rs` | ABROGATE-MOVE | A.2, A.3, A.4 | runtime ascent relocates; scrub L61 tape comment | `crates/path-core/src/runtime/ascent.rs` |
| `path/variant_select.rs` | ABROGATE-MOVE | A.2, A.3, A.4 | runtime variant-select relocates; scrub L21 tape comment | `crates/path-core/src/runtime/variant_select.rs` |
| `path/wildcard.rs` | ABROGATE-MOVE | A.4 | runtime wildcard iter relocates | `crates/path-core/src/runtime/wildcard.rs` |

### 1.5 — `crates/core/src/imports/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `imports/mod.rs` | KEEP-OUTRIGHT | A.2 | clean re-export | (relocates to `bbnf-parse/src/source/`) |
| `imports/errors.rs` | KEEP-OUTRIGHT | A.2 | clean | (relocates) |
| `imports/loader.rs` | KEEP-OUTRIGHT | A.2 | clean | (relocates) |
| `imports/registry.rs` | KEEP-OUTRIGHT | A.2 | clean | (relocates) |
| `imports/resolve.rs` | KEEP-OUTRIGHT | A.2 | clean | (relocates) |

### 1.6 — `crates/core/src/pipeline.rs` + `pipeline/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/core/src/pipeline.rs` | ABROGATE-MOVE | A.2, A.3 | violates `feedback_directory_modules` (flat-file + sibling-dir pair); content merges into `pipeline/mod.rs` | `bbnf-parse/src/pipeline/mod.rs` |
| `crates/core/src/pipeline/` | (largely Pass B scope) | A.1 boundary | parser-front overlap (directives::parse_to_pipeline_inputs) survives; Pass B owns the rest | (Pass B owns) |

### 1.7 — `crates/ir/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/ir/src/lib.rs` | KEEP-MODIFY | A.2, A.3, A.4 | rename re-exports per Lock 2 (`TypeDesc` → `Layout`, etc.); post-Proposal-2 splits into `bbnf-ir/src/lib.rs` | `bbnf-ir/src/lib.rs` |
| `crates/ir/src/cost_config.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/types/mod.rs` | KEEP-MODIFY | A.2 | re-export rename per Lock 2 | (relocates) |
| `crates/ir/src/types/grammar.rs` | KEEP-MODIFY | A.2, A.3 | SPLIT (584 LOC god module) into `types/grammar/{def, accessors, serde}.rs`; scrub L142, 489 narrative; scrub L310 `FusedBuilder` phrase | (relocates) |
| `crates/ir/src/types/node.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `crates/ir/src/types/rule.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `crates/ir/src/types/map_expr.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `crates/ir/src/types/fn_descriptor.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `crates/ir/src/types/type_desc.rs` | ABROGATE-REPLACE | A.2, A.3 (Lock 2) | `TypeDesc` is a Lock 2 retired term; rename + fold into `Layout` representation | `bbnf-ir/src/types/layout.rs` (per Agent A.5 replacement #17) |
| `crates/ir/src/types/type_desc_interner.rs` | KEEP-MODIFY | A.2 | rename to `LayoutInterner` | `bbnf-ir/src/types/layout_interner.rs` |
| `crates/ir/src/types/recognizer_configs.rs` | KEEP-OUTRIGHT | A.1, A.2 | clean | (relocates) |
| `crates/ir/src/registry/mod.rs` | KEEP-MODIFY | A.2 | re-export rename per Lock 2 | (relocates) |
| `crates/ir/src/registry/struct.rs` | ABROGATE-REPLACE | A.2, A.3 (Lock 2) | `StructLayout`, `StructRegistry` are Lock 2 retired terms; rename to `Layout`, `LayoutRegistry` | `bbnf-ir/src/registry/layout.rs` (per Agent A.5 replacement #18) |
| `crates/ir/src/registry/strategy.rs` | KEEP-MODIFY | A.3, A.6 (Lock 14) | retire `PRODUCTION_MANIFEST_TABLE` (L130-185) to xtask-passed `StrategyTable`; relocate Rust-specific path strings out of IR (Lock 5 redress) | (relocates) |
| `crates/ir/src/dag/{mod,build,extract,intern,node}.rs` | KEEP-OUTRIGHT | A.1 | clean DAG cluster | (relocates) |
| `crates/ir/src/egraph/*` | KEEP-OUTRIGHT | A.1 (Pass A overlap; Pass B may have more) | egraph cluster is clean at Pass A scope | (relocates to `bbnf-passes/src/egraph/` or equivalent) |
| `crates/ir/src/recognizer/{mod,facts,plans}.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/rewrites/*` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/audit/payload_coverage.rs` | KEEP-MODIFY | A.2, A.3, A.6 (Lock 14, Lock 13) | merge `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf}` named arms into `Custom`; SPLIT 585-LOC god module | (relocates) |
| `crates/ir/src/passes/audit/mod.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/context/{mod,facts}.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/csp_strategy/mod.rs` | KEEP-MODIFY | A.2, A.3 (Lock 13) | SPLIT (1361 LOC god module) | (relocates) |
| `crates/ir/src/passes/csp_strategy/components.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/csp_strategy/constraints/*` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/facts/mod.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/inspect/{mod,leading,literal,resolve,unwrap,walk}.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/materialization/classify.rs` | KEEP-MODIFY | A.2, A.3 (Lock 13) | SPLIT (843 LOC god module) | (relocates) |
| `crates/ir/src/passes/materialization/{mod,lattice,pin_sweep}.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/patterns/mod.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/payload/layout.rs` | KEEP-MODIFY | A.3 (Lock 13) | SPLIT (514 LOC); per Lock 2 it's already named `layout.rs` (good) | (relocates) |
| `crates/ir/src/passes/payload/{mod,named_types,scalar_routing}.rs` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/recognizers/grammar_facts.rs` | KEEP-MODIFY | A.2, A.3 (Lock 13) | SPLIT (1530 LOC god module) per fact-family | (relocates) |
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` | ABROGATE-DELETE | A.2, A.3, A.6 (Lock 14) | grammar-named module in generic crate; replace with metadata-driven structural-shape miner | (Agent A.5 replacement #2) `bbnf-passes/src/recognizers/structural_shape/` (mechanism) + `[workspace.metadata.bbnf.grammars.<g>.shape-templates]` (data) |
| `crates/ir/src/passes/recognizers/{mod,balanced_wrap,comment_ws,consume_to_next_structural,context_facts_miner,dedup_eligibility,delim_scan,disjoint_first,identifier,kernel_shape,key_dispatch,keyword_stats,list_rules,node_facts,operator_chain,pattern_alphabet,punct_ws_region,quoted_string,separator_list,signature,token_led_branches}.rs` | KEEP-OUTRIGHT | A.1 | clean recogniser ensemble | (relocates) |
| `crates/ir/src/passes/recognizers/shape_dispatch/*` | KEEP-OUTRIGHT | A.1 | 12-file shape-dispatch sub-tree; clean per Lock 13 (cohesive at directory level) | (relocates) |
| `crates/ir/src/passes/sets/*` | KEEP-OUTRIGHT | A.1 | set-analysis foundation; clean | (relocates) |
| `crates/ir/src/passes/sets/dispatch/*` | KEEP-OUTRIGHT | A.1 | dispatch-table generation; clean | (relocates) |
| `crates/ir/src/passes/transform/*` | KEEP-OUTRIGHT | A.1 | structural normaliser; clean | (relocates) |
| `crates/ir/src/passes/transform/fuse_token/*` | KEEP-OUTRIGHT | A.1 | clean | (relocates) |
| `crates/ir/src/passes/types/mod.rs` | ABROGATE-MOVE | A.2, A.3 (Lock 2 + Lock 13) | rename `passes/types/` → `passes/layout/`; SPLIT 786-LOC god module into `layout/{solver, projection, lifetime, registry_glue}.rs` | `bbnf-passes/src/layout/mod.rs` |
| `crates/ir/src/passes/types/registry.rs` | ABROGATE-MOVE | A.2, A.3 | rename + SPLIT (510 LOC) | `bbnf-passes/src/layout/registry_glue.rs` |
| `crates/ir/src/passes/types/type_map.rs` | ABROGATE-REPLACE | A.2, A.3 (Lock 2) | "TypeMap" is a Lock 2 retired term; fold into `Layout` representation | (Agent A.5 replacement) folded into `bbnf-ir/src/types/layout.rs` |
| `crates/ir/src/passes/types/{constraint,obligation,subvariants,generate}.rs` | ABROGATE-MOVE | A.2 (Lock 2) | rename the directory | `bbnf-passes/src/layout/` |
| `crates/ir/src/passes/csp_domains.rs` | KEEP-MODIFY | A.2, A.3 (Lock 13) | SPLIT (500 LOC) per domain-constructor family | (relocates) |
| `crates/ir/src/passes/{inline_trace,lr,metadata,path_check,prefix,profile,regex_info,span}.rs` | KEEP-MODIFY (profile.rs) / KEEP-OUTRIGHT (rest) | A.2 | `profile.rs` L26, L108: drop `bbnf_shape_templates` field (Lock 14 redress); rest are clean | (relocates) |
| `crates/ir/src/vm/*` | (Pass B scope; Pass A boundary) | A.1 boundary | bytecode VM is a runtime concern; out of Pass A. Per Proposal 2, lives in `bbnf-vm/` | (Pass B owns; relocates to `bbnf-vm/`) |

### 1.8 — `crates/bbnf-path/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/bbnf-path/src/lib.rs` | KEEP-MODIFY | A.4 (Lock 7) | proc-macro entry stays; consumes `path-core` | `crates/path/src/lib.rs` (post Proposal 3) |
| `crates/bbnf-path/src/path_macro.rs` | ABROGATE-MOVE | A.2, A.3, A.4 (Lock 7, Lock 13) | SPLIT (639 LOC god module) into per-phase files; relocate to `path-core` | `crates/path-core/src/{lex, lower, validate, emit}.rs` |
| `crates/bbnf-path/src/registry.rs` | ABROGATE-REPLACE | A.2, A.3, A.6 (Lock 14) | retire `match grammar` at L132-135; consume workspace-metadata-driven `RegistryDescriptor`; the synthetic fixture surface retires (Agent A.5 replacement #6) | `crates/path-core/src/registry.rs` (consumes per-grammar emitted `pub const REGISTRY: Layout`) |

### 1.9 — `crates/bbnf-path-ts/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/bbnf-path-ts/src/lib.rs` | KEEP-MODIFY | A.4 (Lock 7) | cdylib entry stays; consumes `path-core` | `crates/path-ts/src/lib.rs` (post Proposal 3) |
| `crates/bbnf-path-ts/src/compile.rs` | ABROGATE-DELETE | A.2, A.6 (DRY, Lock 7) | mirror of `bbnf-path/src/path_macro.rs`; eliminated by Proposal 3 (shared `path-core`) | (deleted; logic at `crates/path-core/src/{lex, lower, validate}.rs`) |
| `crates/bbnf-path-ts/src/fixture.rs` | ABROGATE-DELETE | A.2, A.6 (DRY, Lock 14) | mirror of `bbnf-path/src/registry.rs`; eliminated by per-grammar emitted REGISTRY (Agent A.5 replacement #7) | (deleted; consumed by `crates/path-core/src/registry.rs`) |
| `crates/bbnf-path-ts/src/schema.rs` | KEEP-MODIFY | A.4 | wire types for TS surface; relocate | `crates/path-ts/src/schema.rs` |
| `crates/bbnf-path-ts/src/template_tag.rs` | KEEP-MODIFY | A.4 | TS shim string; relocate | `crates/path-ts/src/template_tag.rs` |

### 1.10 — `crates/bootstrap/`

| File | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `crates/bootstrap/src/lib.rs` | KEEP-MODIFY OR ABROGATE-DELETE | A.4 (Proposal 6) | the 28-LOC re-export shim either renames its single re-export to the new path (post-Proposal-1: `bbnf_codegen::generated::bbnf::BbnfBootstrap`) — which keeps the shim — OR retires entirely (consumers reach the new path directly). Synthesizer adjudicates. | (path adjudicated below) |
| `crates/bootstrap/src/bin/dump_ir.rs` | ABROGATE-MOVE | A.4 (Proposal 6) | dev binary; relocates to xtask | `xtask/src/bin/dump_ir.rs` |
| `crates/bootstrap/src/bin/cost_grid_sweep.rs` | ABROGATE-MOVE | A.4 (Proposal 6) | dev binary; relocates | `xtask/src/bin/cost_grid_sweep.rs` |
| `crates/bootstrap/src/bin/debug_parse.rs` | ABROGATE-MOVE | A.4 (Proposal 6) | dev binary; relocates | `xtask/src/bin/debug_parse.rs` |

**Adjudication on `crates/bootstrap/src/lib.rs`**: KEEP-MODIFY. The shim
preserves the public dependency name `bbnf-bootstrap` for downstream
consumers (per the user's no-backward-compat-but-name-stability pattern).
The 28-LOC overhead is acceptable.

### 1.11 — Sibling repos (Pass A boundary surface)

| Repo | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `parse-that` (`/Users/mkbabb/Programming/parse-that/rust/parse_that/`) | KEEP-MODIFY | A.4 (Proposal 4, Lock 11) | promote to workspace path-dep at `crates/parse-that/` (or sub-module per `docs/precepts/` pattern) | `crates/parse-that/` |
| `bbnf-regex` (`/Users/mkbabb/Programming/parse-that/rust/regex/`) | KEEP-MODIFY | A.4 (Proposal 4, Lock 11) | promote to workspace path-dep at `crates/bbnf-regex/` | `crates/bbnf-regex/` |

### 1.12 — Grammar source tree

| Path | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `grammar/bbnf/{bbnf, expressions, types}.bbnf` | KEEP-OUTRIGHT | A.1 | clean per-grammar dir | n/a |
| `grammar/json/json.bbnf` | KEEP-OUTRIGHT | A.1 | clean | n/a |
| `grammar/css/pretty.bbnf` | KEEP-MODIFY | A.4 (Proposal 5) | relocate to `grammar/css-pretty/pretty.bbnf` for top-level uniformity | `grammar/css-pretty/pretty.bbnf` |
| `grammar/css/l4/*` | KEEP-MODIFY | A.4 (Proposal 5) | relocate to `grammar/css-l4/` for uniformity | `grammar/css-l4/` |
| `grammar/google-sheets/google-sheets.bbnf` | KEEP-OUTRIGHT | A.1 | clean | n/a |
| `grammar/ebnf/ebnf.bbnf` | KEEP-OUTRIGHT | A.1 | clean | n/a |
| `grammar/bnf/bnf.bbnf` | KEEP-OUTRIGHT | A.1 | clean | n/a |
| `grammar/misc/csv.bbnf` | KEEP-MODIFY | A.4 (Proposal 5) | lift to `grammar/csv/csv.bbnf` | `grammar/csv/csv.bbnf` |
| `grammar/misc/math.bbnf` | KEEP-MODIFY | A.4 (Proposal 5) | lift to `grammar/math/math.bbnf` | `grammar/math/math.bbnf` |
| `grammar/misc/{math-ambiguous, g4, regex, emoji, json-commented}.bbnf` | KEEP-MODIFY | A.4 (Proposal 5) | relocate to `grammar/fixtures/` | `grammar/fixtures/` |
| `grammar/tests/google-sheets-formula-test-cases.md` | KEEP-OUTRIGHT | A.1 | clean | n/a |

### 1.13 — Workspace metadata

| Path | Bucket | Source | Rationale | Successor |
|---|---|---|---|---|
| `Cargo.toml [workspace.metadata.bbnf]` | KEEP-MODIFY | A.4 (Proposal 5) | update grammar paths after directory rename | n/a |
| `Cargo.toml [workspace.metadata.bbnf-strategy]` | KEEP-MODIFY | A.3, A.6 (Lock 14) | this metadata table BECOMES the single source of truth (the IR-side `PRODUCTION_MANIFEST_TABLE` retires); xtask reads this directly | n/a |

---

## §2 — Architectural transpositions ratified for Pass A

The synthesis ratifies the following macro-proposals from Agent A.4:

### Proposal 1 (RATIFIED) — Fracture `crates/core/`

**Carry to synthesizer**: yes (largest Pass A surgery).

```
crates/
  bbnf-parse/        ← source acquisition (imports/) + grammar parser entry + lower/ + pipeline/
  bbnf-codegen/      ← codegen + per-backend lowerers (driver/, rust/, ts/, wasm/) + generated/
  bbnf-runtime/      ← per-grammar generic substrate (path/, scan glue)
  bbnf/              ← thin aggregator
  bbnf-grammar-css-l4/  ← per-grammar declaration crate (host fns)
  (other per-grammar declaration crates as needed)
```

**Locks honoured**: 5, 11, 13, 14.

**Cross-tranche carry**: Pass B owns `bbnf-codegen` content; Pass C owns
the workspace toolchain implications (xtask, Cargo.toml, registry-vs-
path-dep policy).

### Proposal 2 (RATIFIED) — Fracture `crates/ir/`

**Carry to synthesizer**: yes.

```
crates/
  bbnf-ir/         ← types/ + dag/ + registry/ + cost_config (Layout-vocab; backend-agnostic)
  bbnf-passes/     ← all transformation passes; the post-Lock-2 layout/ tree
  bbnf-vm/         ← bytecode VM + interpreter
  (optional bbnf-egraph-rules/ for grammar-tier rewrite rules)
```

**Locks honoured**: 2, 4, 5, 13.

### Proposal 3 (RATIFIED) — Path crate triplet

**Carry to synthesizer**: yes.

```
crates/
  path-core/    ← shared types + lex/lower/validate + runtime executor
  path/         ← Rust proc-macro shell
  path-ts/      ← TS cdylib shell
```

The legacy `crates/core/src/runtime/path.rs` (163 LOC duplicate alphabet)
deletes; the four `runtime/<g>/parse_with.rs` legacy lowering paths
(~480 LOC) delete.

**Locks honoured**: 7, 13, 14, KISS.

### Proposal 4 (RATIFIED) — Workspace promotion of `parse-that` + `bbnf-regex`

**Carry to synthesizer**: yes.

Sibling-repo path-dep submodule pinning (per the user's `docs/precepts/`
pattern) is acceptable ALTERNATIVELY to workspace member relocation; the
synthesizer adjudicates the mechanism. Lock 11's "path-deps until stable"
mandate is honoured either way.

**Locks honoured**: 11.

### Proposal 5 (RATIFIED) — Grammar source tree layout

**Carry to synthesizer**: yes.

Per-grammar dir uniformly; `fixtures/` for test sources.

**Locks honoured**: 13, 14.

### Proposal 6 (PARTIALLY RATIFIED) — Bootstrap retirement

**Carry to synthesizer**: partially. Dev binaries relocate to xtask; the
`crates/bootstrap/src/lib.rs` re-export shim KEEPS for public-API stability
(synthesizer adjudication: name-stability outweighs the 28-LOC cost).

**Locks honoured**: 13, KISS.

---

## §3 — New facilities ratified for Pass A scope

The synthesis ratifies all eight new-facility designs from Agent A.5:

| # | Facility | Location | Sketch |
|---|---|---|---|
| 1 | `inverse-layout-audit` IR pass | `bbnf-passes/src/audit/inverse_layout/` | walk every rule's body for typed-`->` `Map` nodes; verify enclosing rule has resolvable `Layout` in `LayoutRegistry`; build-fail on missing or non-admitting layouts. The substantive realisation of `feedback_typed-materialization-invariant`. |
| 2 | `bbnf-grammar` crate | `crates/bbnf-grammar/` | carries `AST<'a>`, `RuleEntry<'a>`, `ImportDirective<'a>`, plus the post-restart `parse_grammar` entry. Untangles "grammar-source types" from "library root". |
| 3 | Workspace-metadata schema validator | `xtask/src/validate/` | xtask command `cargo xtask validate-metadata` checks every `[workspace.metadata.bbnf*]` table is well-formed; CI gate. |
| 4 | `bbnf-error` crate | `crates/bbnf-error/` | unified error trait + canonical wrapper; per-crate error types remain; cross-crate composition has one boundary type. |
| 5 | Cohort-template generator | `xtask/src/template/cohort/` (or proc-macro under `bbnf-codegen/`) | xtask emits the five trivial cohort grammars' (BNF, CSV, EBNF, CSS Pretty, Math) per-grammar runtime modules from one template; ~1500 LOC of mechanical instantiation retires. |
| 6 | Per-grammar declaration crate template | `crates/bbnf-grammar-<g>/` (per grammar that needs host fns) | thin Lock-14 declaration-crate substrate. CSS L4 is the first instantiation. |
| 7 | Path executor surface relocation | `path-core/src/runtime/` | the runtime executor surface lives at the canonical Lock 7 location post-restart. Naming clarity. |
| 8 | `LayoutSink` trait | `bbnf-ir/src/registry/sink.rs` | the trait every backend implements when consuming the Layout; named in Lock 2 as the official consumer trait but currently absent. |

---

## §4 — Cross-cuts ratified

The synthesis ratifies the cross-cut findings from Agent A.6:

### 4.1 — Lock 14 retirement is one BA wave

The seven Lock-14-violating sites in Pass A scope retire in one
coordinated wave:
1. Move `css_types.rs` to `crates/bbnf-grammar-css-l4/src/host.rs`
2. Generalise `shape_dict_bbnf.rs` to metadata-driven (delete file)
3. Drop `bbnf_shape_templates` field on `GrammarProfile`
4. Merge `GrammarAuditTag` named arms into `Custom`
5. Retire `PRODUCTION_MANIFEST_TABLE` to xtask-passed `StrategyTable`
6. Rewrite `bbnf-path/src/registry.rs:132-135` to consume metadata
7. Relocate `path/markers.rs` ZSTs to per-grammar declaration crates

**Receiving locus**: BA W1 (Lock 14 retirement; the architectural lock).

### 4.2 — Workspace promotion is foundational (Lock 11)

Until `parse-that` and `bbnf-regex` are workspace path-deps, every Lock-14
surgery is fragile. **Receiving locus**: BA W0 (alongside other foundational
moves).

### 4.3 — Lock 2 rename + Lock 13 SPLIT are paired on the IR side

The `passes/types/` → `passes/layout/` rename and the 786-LOC `mod.rs`
SPLIT land together. **Receiving locus**: BA W2 (Layout canon + IR
restructure).

### 4.4 — `crates/core/` fracture sequences last

The largest Pass A surgery depends on Lock 11 promotion + Lock 14
retirement + Lock 2 rename + path triplet landing first. **Receiving
locus**: BA W3 (or later) per synthesizer sequencing.

### 4.5 — Pass-B residue: per-grammar runtime template emission

The `crates/core/src/runtime/` god directory by mixed-concern (16
siblings) is technically Pass B scope. Pass A flags it; Pass B owns the
detailed surgery. The Agent A.5 cohort-template generator (new facility
#5) is the substantive resolution mechanism — Pass A authors the metadata
schema; Pass B consumes it.

---

## §5 — Pass-A residues to flag for synthesizer

Cross-pass concerns Pass A surfaces:

1. **`bbnf-error` consolidation** spans all three passes. Pass A names the
   trait + wrapper shape; Pass B's runtime errors and Pass C's tooling
   errors must adopt. Synthesizer reconciles.

2. **`bbnf-ir` → `bbnf-codegen` boundary** is shared with Pass B (codegen
   is Pass B; IR is Pass A). The boundary IS the IR contract; Pass B's
   verdict on the IR shape will need cross-checking against Pass A's
   ratified Lock 5 surgery (Rust-specific path strings out of IR).

3. **Per-grammar runtime template (cohort generator)** is shared with
   Pass B. Pass A authors the metadata schema + xtask command; Pass B's
   runtime side consumes.

4. **`crates/core/` fracture pulls Pass B content** — the `backend/`,
   `generate/`, `runtime/` subtrees (Pass B scope) relocate alongside the
   Pass A subtrees in one coordinated pass. Synthesizer must sequence.

5. **`parse-that` workspace promotion** intersects Pass C (xtask + workspace
   metadata + dep policy). Pass A names the move; Pass C operationalises.

6. **Generated/ tree relocation** (per HARDENING-PLAN-SYNTHESIS punch list
   item #22): `crates/core/src/grammar/generated/` → `crates/bbnf-codegen/src/generated/`
   is a Pass B surgery; Pass A's Proposal 1 mandates it as a sequencing
   prerequisite.

7. **The `Box::leak` at `crates/core/src/grammar/mod.rs:57`** is a Lock 9
   workaround. Pass A flags it; the resolution is a public-API change
   (force `&'static`-able input vs introduce `parse_in(input, &bump)`).
   Synthesizer adjudicates between elegance (force the lifetime) and
   ergonomics (provide both surfaces).

---

## §6 — Lock + precept verdicts at Pass A close

| Lock / precept | Pass A verdict | Cite |
|---|---|---|
| Lock 1 (tape dead) | substantively-honoured; ~9 narrative-residue scrubs | `crates/core/src/{grammar/mod, types, path/{ascent, variant_select}, grammar/schema/*}` + `crates/ir/src/types/{type_desc, grammar}` |
| Lock 2 (Layout canon) | violated; one rename pass | `crates/ir/src/{types/type_desc, registry/struct, passes/types/*}` |
| Lock 3 (cursor-parse + byte-skip unified) | honoured | `crates/core/src/path/cursor.rs` |
| Lock 4 (per-domain orthogonal optimization) | honoured | `crates/ir/src/passes/{csp_strategy, recognizers, egraph}` |
| Lock 5 (IR + per-backend lower) | substantively honoured; one redress | `crates/ir/src/registry/strategy.rs:130-185` Rust path strings out of IR |
| Lock 6 (xtask emits committed source artefacts) | honoured | `Cargo.toml [workspace.metadata.bbnf]` |
| Lock 7 (consolidated path crate) | violated | `crates/{bbnf-path, bbnf-path-ts, core/src/path}` |
| Lock 8 (surpass SOTA) | honoured (Pass A is pre-codegen) | (n/a) |
| Lock 9 (slice-borrow primary) | violated at one site | `crates/core/src/grammar/mod.rs:57` |
| Lock 10 (Pratt + SIMD auto-detected) | honoured | `crates/core/src/lower/expression/pratt.rs` + `crates/ir/src/passes/recognizers/{operator_chain, pattern_alphabet}.rs` |
| Lock 11 (path-deps for incubating sister crates) | violated | `crates/{ir, core, bbnf-path, bbnf-path-ts}/Cargo.toml` (parse-that, bbnf-regex, csp-solver are not workspace path-deps) |
| Lock 12 (ser + gorgeous archive) | silent (Pass C scope) | (n/a) |
| Lock 13 (no god directories) | violated extensively (~13 SPLIT obligations + 1 god directory at `crates/core/src/`) | `crates/core/src/{lib, grammar/host, lower/expression/{mod, wrap}, lower/value_expr/atom}` + `crates/ir/src/{types/grammar, registry/struct, passes/{recognizers/grammar_facts, csp_strategy/mod, materialization/classify, types/{mod, registry}, csp_domains, payload/layout, audit/payload_coverage}}` + `crates/bbnf-path/src/path_macro` |
| Lock 14 (full grammar generalisation) | violated at 7 sites | `crates/core/src/css_types`, `crates/core/src/path/markers`, `crates/ir/src/registry/strategy:130-185`, `crates/ir/src/passes/audit/payload_coverage:67-77`, `crates/ir/src/passes/recognizers/shape_dict_bbnf`, `crates/ir/src/passes/profile:26+108`, `crates/bbnf-path/src/registry:132-135` |
| no-workarounds | violated at ~3 sites + ~9 narrative residues | `crates/core/src/{grammar/host:387, grammar/mod:57 (Box::leak), lower/value_expr/simple_kinds:185}` |
| no-orthogonal-codepaths | honoured (lowering pipeline is structurally singular) | (n/a) |
| KISS / DRY | violated (sister-crate mirror; god modules) | `crates/bbnf-path-ts/src/{compile, fixture}.rs` (mirrors); 13 god modules |
| single-plan-execution | (binds the orchestrator) | (n/a) |
| preserve-rich-AST | honoured | (n/a) |
| direct-to-struct | honoured (substrate is post-AX struct-direct) | (n/a) |
| system-cohesion | violated at 3 sites | `crates/core/src/css_types`, `crates/bbnf-path-ts/src/compile`, `crates/ir/src/passes/recognizers/shape_dict_bbnf` |
| pluggable-components | violated at 4 sites | `crates/core/src/path/markers`, `crates/ir/src/registry/strategy:130-185`, `crates/ir/src/passes/audit/payload_coverage:67-77`, `crates/bbnf-path/src/registry:132-135` |
| gestalt-approach | (binds the orchestrator) | (n/a) |
| feedback_directory_modules | violated at 1 site | `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/` |
| feedback_no_inline_tests | violated at 3 Pass A sites + 5 Pass B sites | `crates/core/src/path/{cursor:313-314, executor:65-66, schema:130-131}.rs` |
| feedback_no_metalanguage_docs | violated at ~9 narrative-residue sites | doc-comment scrub |

---

## §7 — Pass A punch list

Ordered for the synthesizer + tranche-author. Each entry: target / surgery
/ locks honoured / estimated LOC.

### W0 — Foundational (Lock 11; zero-risk)

1. **Promote `parse-that` to workspace path-dep.** Either relocate to
   `crates/parse-that/` or add as git-submodule + workspace member. Update
   `crates/core/Cargo.toml` and any consumer dep declarations to
   `path = "../parse-that"`. **Locks**: 11. **LOC**: ~6 dep entries.

2. **Promote `bbnf-regex` to workspace path-dep.** Relocate to
   `crates/bbnf-regex/`. Update `crates/ir/Cargo.toml`,
   `crates/bbnf-path/Cargo.toml`, `crates/bbnf-path-ts/Cargo.toml`.
   **Locks**: 11. **LOC**: ~3 dep entries.

3. **Promote `csp-solver` from versioned to path-dep.** Update
   `crates/core/Cargo.toml` `csp-solver = "0.1"` → `csp-solver = { path =
   "../csp-solver" }`. The crate already exists at `crates/csp-solver/`.
   **Locks**: 11. **LOC**: 1 dep entry.

4. **Tape narrative scrub.** Remove dead-substrate phrases from ~15 sites:
   `crates/core/src/{grammar/mod:3+7+17, types:90, path/{ascent:61,
   variant_select:21}, grammar/schema/{build:26, model:20,
   emit/rust/shared:3+17}}` + `crates/ir/src/types/{type_desc:103+147,
   grammar:142+310+489}`. **Locks**: 1, no-metalanguage. **LOC**: ~20
   line-deletes.

### W1 — Lock 14 retirement (one coordinated wave)

5. **Move `crates/core/src/css_types.rs` to a per-grammar declaration
   crate.** Create `crates/bbnf-grammar-css-l4/` (Cargo.toml + `src/lib.rs` +
   `src/host.rs` carrying `parse_hex_color`). Update CSS L4 grammar's
   `-> parse_hex_color(...)` map reference and the codegen's host-path
   resolution. **Locks**: 14. **LOC**: +crate (~3 files); −66 LOC at
   library root.

6. **Delete `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`.**
   Replace with metadata-driven structural-shape miner under
   `bbnf-passes/src/recognizers/structural_shape/` (the mechanism) +
   `[workspace.metadata.bbnf.grammars.bbnf.shape-templates]` (the data;
   two rows for `big_comment` and `mapped_factor` empty branch). **Locks**:
   14. **LOC**: −192 LOC; +new mechanism + 2 metadata rows.

7. **Drop `bbnf_shape_templates` field on `GrammarProfile`.** Field at
   `crates/ir/src/passes/profile.rs:26`; import at L108. The structural-
   shape mechanism (W1.6) populates a renamed `structural_shapes:
   Vec<DetectedShape>` field instead. **Locks**: 14. **LOC**: ~6 lines.

8. **Merge `GrammarAuditTag` named arms into `Custom`.** At
   `crates/ir/src/passes/audit/payload_coverage.rs:67-77`, replace the
   enum with `pub struct GrammarAuditTag(pub &'static str);`. Update the
   key() impl, downstream consumers, and the test fixtures
   (`crates/ir/tests/payload_coverage_audit.rs:239+312+376-378+478+486`).
   **Locks**: 14. **LOC**: ~30 lines.

9. **Retire `PRODUCTION_MANIFEST_TABLE`.** At
   `crates/ir/src/registry/strategy.rs:130-185`, delete the static table.
   Update `EmitStrategy::for_grammar` to consume an xtask-passed
   `StrategyTable` parameter. xtask reads
   `[workspace.metadata.bbnf-strategy]` directly. **Locks**: 14, 5 (Lock 5
   redress: Rust-specific path strings move out of the IR crate to
   xtask + bbnf-codegen). **LOC**: −55 LOC at the table; +new xtask path
   reader.

10. **Rewrite `crates/bbnf-path/src/registry.rs:132-135`.** Replace the
    `match grammar { "json" => ..., ... }` with a workspace-metadata-
    driven `RegistryDescriptor` slice consumption. Per Agent A.5
    replacement #6. **Locks**: 14. **LOC**: ~30 lines.

11. **Relocate `crates/core/src/path/markers.rs` ZSTs.** Move per-grammar
    ZSTs (`Json`, `CssL4`, `Sheets`, `Bbnf`) to per-grammar declaration
    crates (or to `crates/bbnf-codegen/src/generated/<g>/marker.rs` as
    xtask emit output). The path crate (post Lock 7) carries only the
    `GrammarMarker` trait. **Locks**: 14, 7. **LOC**: 30 LOC relocate.

### W2 — Lock 2 (Layout canon) + Lock 13 IR splits

12. **Rename `crates/ir/src/passes/types/` → `passes/layout/`.** Move all
    files in the directory (mod.rs, registry.rs, type_map.rs, constraint/,
    obligation.rs, subvariants.rs, generate.rs). **Locks**: 2.
    **LOC**: directory move.

13. **Rename `TypeDesc` → `Layout`** across `crates/ir/src/types/type_desc.rs`,
    re-exports in `lib.rs`, and every consumer (~50 sites). Fold into
    `Layout` representation; eliminate `TypeMap` (at
    `passes/types/type_map.rs`) by folding into `Layout`. **Locks**: 2.
    **LOC**: ~50 sites.

14. **Rename `StructLayout` → `Layout`, `StructRegistry` → `LayoutRegistry`**
    across `crates/ir/src/registry/struct.rs` and consumers. **Locks**: 2.
    **LOC**: ~30 sites.

15. **SPLIT `crates/ir/src/passes/types/mod.rs` → `passes/layout/{solver,
    projection, lifetime, registry_glue}.rs`.** mod.rs becomes ≤ 100 LOC
    re-export hub. **Locks**: 13. **LOC**: 786 LOC redistribute.

16. **SPLIT `crates/ir/src/passes/types/registry.rs`.** 510 LOC into
    `passes/layout/registry_glue.rs` + helpers. **Locks**: 13. **LOC**:
    510 redistribute.

17. **SPLIT `crates/ir/src/passes/csp_strategy/mod.rs`.** 1361 LOC into
    `csp_strategy/{solver_wiring, domains, materialization_glue}.rs`.
    **Locks**: 13. **LOC**: 1361 redistribute.

18. **SPLIT `crates/ir/src/passes/recognizers/grammar_facts.rs`.** 1530
    LOC per fact-family. **Locks**: 13. **LOC**: 1530 redistribute.

19. **SPLIT `crates/ir/src/passes/materialization/classify.rs`.** 843 LOC.
    **Locks**: 13. **LOC**: 843.

20. **SPLIT `crates/ir/src/types/grammar.rs`.** 584 LOC into
    `types/grammar/{def, accessors, serde}.rs`. **Locks**: 13. **LOC**:
    584.

21. **SPLIT `crates/ir/src/passes/audit/payload_coverage.rs`.** 585 LOC
    into `audit/payload_coverage/{classify, walk, report}.rs`. **Locks**:
    13. **LOC**: 585.

22. **SPLIT `crates/ir/src/passes/payload/layout.rs`.** 514 LOC. **Locks**:
    13. **LOC**: 514.

23. **SPLIT `crates/ir/src/passes/csp_domains.rs`.** 500 LOC per domain
    family. **Locks**: 13. **LOC**: 500.

### W3 — Lock 7 path triplet

24. **Restructure path crates per Lock 7.** Create `crates/path-core/`,
    `crates/path/` (rename of `bbnf-path`), `crates/path-ts/` (rename of
    `bbnf-path-ts`). Move `crates/core/src/path/{ir, error, type_check,
    schema, markers}` (markers retire per W1.11) to `path-core/src/`;
    `crates/core/src/path/{cursor, executor, ascent, variant_select,
    wildcard}` to `path-core/src/runtime/`. Extract `bbnf-path/src/path_macro.rs`
    lex/lower/validate logic to `path-core/src/{lex, lower, validate}.rs`
    (split + extract). Delete `bbnf-path-ts/src/{compile, fixture}.rs`
    (mirrored content now lives in path-core). **Locks**: 7, 13, 14, KISS.
    **LOC**: ~−500 LOC mirror + ~−163 LOC legacy alphabet + new path-core
    structure.

25. **Delete `crates/core/src/runtime/path.rs` (legacy alphabet).** 163
    LOC. Adjust the four `runtime/<g>/parse_with.rs` files to consume the
    typed alphabet directly (delete the manual lower-to-legacy code).
    **Locks**: 7, KISS, DRY. **LOC**: −163 + ~−480 in parse_with files.

26. **Move inline tests from `crates/core/src/path/{cursor,executor,
    schema}.rs` to `tests/`.** **Locks**: feedback_no_inline_tests.
    **LOC**: ~30 lines move.

### W4 — `crates/core/` fracture

27. **Create `crates/bbnf-parse/`.** Move `crates/core/src/{lower, imports,
    pipeline, types, grammar/{mod, host, schema}}` into
    `bbnf-parse/src/{lower, source, pipeline, types, grammar}/` (where
    `imports/` becomes `source/` to align with the directive's naming).
    The grammar entry remains here. **Locks**: 5, 11, 13, 14. **LOC**:
    relocations.

28. **Create `crates/bbnf-codegen/`.** Move `crates/core/src/{generate,
    backend, grammar/generated}` into `bbnf-codegen/src/{driver, rust,
    ts, wasm, generated}/`. **Locks**: 5, 13. **LOC**: relocations.
    (Pass B owns the surgery in detail; Pass A names the boundary.)

29. **Create `crates/bbnf-runtime/`.** Move
    `crates/core/src/runtime/{builder, builder_template, arena_template,
    handle, view, mod}` (the generic substrate) into
    `bbnf-runtime/src/`. The per-grammar `runtime/<g>/` directories
    relocate to `bbnf-codegen/src/generated/<g>/` (Pass B owns).
    **Locks**: 13, 14. **LOC**: relocations.

30. **Restructure `bbnf` crate as thin aggregator.** `crates/core/` →
    `crates/bbnf/`. lib.rs becomes `pub use bbnf_parse::*; pub use
    bbnf_codegen::*; pub use bbnf_runtime::*;`. Public-API surface
    survives via re-exports. **Locks**: 13. **LOC**: ~10 lines.

31. **Restructure `crates/core/src/pipeline.rs` + `pipeline/` to
    `pipeline/mod.rs`.** Single directory module. **Locks**:
    feedback_directory_modules. **LOC**: file move (105 LOC absorb).

### W5 — Grammar source tree + bootstrap

32. **Rename grammar source dirs per Proposal 5.**
    - `grammar/css/l4/` → `grammar/css-l4/`
    - `grammar/css/pretty.bbnf` → `grammar/css-pretty/pretty.bbnf`
    - `grammar/misc/csv.bbnf` → `grammar/csv/csv.bbnf`
    - `grammar/misc/math.bbnf` → `grammar/math/math.bbnf`
    - `grammar/misc/{math-ambiguous, g4, regex, emoji, json-commented}.bbnf`
      → `grammar/fixtures/`
    Update `[workspace.metadata.bbnf]` paths accordingly. **Locks**: 13,
    14. **LOC**: 5 path renames.

33. **Move dev binaries from `crates/bootstrap/src/bin/` to xtask.**
    Keep `crates/bootstrap/src/lib.rs` re-export shim (rename re-export
    to `bbnf_codegen::generated::bbnf::BbnfBootstrap` post-W4). **Locks**:
    13, KISS. **LOC**: ~440 LOC move.

### W6 — God-module splits in core / sister crates

34. **SPLIT `bbnf-parse/src/grammar/host.rs`** (584 LOC) into
    `host/{mod, observational, pipeline, directives}.rs`. FAIL-EXPLICIT
    at the wildcard `@debug` strip-prefix (currently L387). **Locks**:
    13, no-workarounds. **LOC**: 584.

35. **SPLIT `bbnf-parse/src/lower/expression/mod.rs`** (539 LOC) into
    `expression/{term, factor, mapping}.rs`. **Locks**: 13. **LOC**: 539.

36. **SPLIT `bbnf-parse/src/lower/expression/wrap.rs`** (731 LOC) into
    `wrap/{detect, map_expr, payload}.rs`. **Locks**: 13. **LOC**: 731.

37. **SPLIT `bbnf-parse/src/lower/value_expr/atom.rs`** (590 LOC) into
    `atom/{literal, projection, type}.rs`. **Locks**: 13. **LOC**: 590.

38. **FAIL-EXPLICIT at `bbnf-parse/src/lower/value_expr/simple_kinds.rs:185`.**
    Replace defensive fallback with `unreachable!()` or fix upstream
    classifier. **Locks**: no-workarounds. **LOC**: ~3 lines.

39. **SPLIT `path-core/src/{lex, lower, validate}.rs` from the extracted
    639 LOC of `bbnf-path/src/path_macro.rs`** (already extracted in W3.24
    — verify the per-phase split is clean). **Locks**: 13. **LOC**:
    verification.

### W7 — New facilities

40. **Create `bbnf-passes/src/audit/inverse_layout/`** — the
    inverse-layout-audit pass (new facility #1). Build-fail on missing
    `Layout` for compound-typed rules. **Locks**:
    feedback_typed-materialization-invariant, 2. **LOC**: ~150 LOC new
    code + audit wiring.

41. **Create `crates/bbnf-error/`** — unified error trait + canonical
    wrapper (new facility #4). Per-crate error types impl `BbnfError`.
    **Locks**: system-cohesion. **LOC**: ~80 LOC new + per-crate impl.

42. **Create `xtask/src/validate/`** — workspace-metadata schema
    validator (new facility #3). CI gate: `cargo xtask validate-metadata
    --check`. **Locks**: system-cohesion, no-workarounds. **LOC**:
    ~150 LOC new.

43. **Create `xtask/src/template/cohort/`** — cohort-template generator
    (new facility #5). Emits the five trivial cohort grammars' runtime
    modules from one template. **Locks**: 13, 14, KISS, DRY. **LOC**:
    ~250 LOC new + ~−1500 LOC retired hand-written cohort runtime (Pass
    B's ~1500 LOC of mechanical instantiation).

44. **Create `bbnf-ir/src/registry/sink.rs`** — `LayoutSink` trait (new
    facility #8). Backend-agnostic Layout-consumer trait. **Locks**: 2,
    5. **LOC**: ~50 LOC.

### W8 — Lock 9 + miscellany

45. **Adjudicate `Box::leak` at `bbnf-parse/src/grammar/mod.rs:57`.**
    Synthesizer chooses: (a) public-API change forcing `&'static`-able
    input, OR (b) introduce `parse_grammar_in(input, &bump)` arena
    variant per Lock 9. **Locks**: 9, no-workarounds. **LOC**: API
    change.

46. **Drop BBNF aggregator `pub use bbnf::*`** at
    `bbnf-codegen/src/generated/mod.rs:35` (post W4 relocation; current
    location is `crates/core/src/grammar/generated/mod.rs:35`).
    BBNF accesses uniformly via the namespaced path. **Locks**: 14,
    system-cohesion. **LOC**: 1 line + downstream consumer rename.

---

## §8 — Greenfield commitments from Pass A

The new tranche set (per `docs/restart/README.md` §10 — A through J or
further) inherits the following commitments:

### 8.1 — New crate creation list

| Crate | Source | Justification |
|---|---|---|
| `crates/bbnf-parse/` | A.4 Proposal 1 | parser-front split (Lock 13) |
| `crates/bbnf-codegen/` | A.4 Proposal 1 | codegen + per-backend lower (Lock 5) |
| `crates/bbnf-runtime/` | A.4 Proposal 1 | grammar-agnostic runtime substrate (Lock 14) |
| `crates/bbnf/` | A.4 Proposal 1 | thin aggregator |
| `crates/bbnf-grammar/` | A.5 new facility #2 | grammar-source types + parser entry |
| `crates/bbnf-grammar-css-l4/` | A.5 replacement #1 + new facility #6 | per-grammar declaration crate (CSS L4 host fns) |
| `crates/bbnf-error/` | A.5 new facility #4 | unified error trait + wrapper |
| `crates/bbnf-ir/` | A.4 Proposal 2 | IR types only (Layout vocabulary) |
| `crates/bbnf-passes/` | A.4 Proposal 2 | every IR transformation pass (post-Lock-2 layout/ tree) |
| `crates/bbnf-vm/` | A.4 Proposal 2 | bytecode VM + interpreter (relocated from `crates/ir/src/vm/`) |
| `crates/path-core/` | A.4 Proposal 3 (Lock 7) | shared path types + lex/lower/validate + runtime executor |
| `crates/path/` | A.4 Proposal 3 (Lock 7) | Rust proc-macro shell |
| `crates/path-ts/` | A.4 Proposal 3 (Lock 7) | TS cdylib shell |
| `crates/parse-that/` | A.4 Proposal 4 (Lock 11) | sibling-repo workspace promotion (or git-submodule) |
| `crates/bbnf-regex/` | A.4 Proposal 4 (Lock 11) | sibling-repo workspace promotion |

Total new crates: 15. Existing crates that retire wholesale (after
content relocates): `crates/bbnf-path/` → renamed to `crates/path/`;
`crates/bbnf-path-ts/` → renamed to `crates/path-ts/`; `crates/core/` →
fractures into bbnf-parse + bbnf-codegen + bbnf-runtime + bbnf aggregator;
`crates/ir/` → fractures into bbnf-ir + bbnf-passes + bbnf-vm.

### 8.2 — File migration list

| From | To | LOC |
|---|---|---|
| `crates/core/src/imports/` | `bbnf-parse/src/source/` | ~570 LOC |
| `crates/core/src/lower/` | `bbnf-parse/src/lower/` | ~3.7 K LOC |
| `crates/core/src/path/{ir,error,type_check,schema,markers}.rs` | `path-core/src/` | ~700 LOC |
| `crates/core/src/path/{cursor,executor,ascent,variant_select,wildcard}.rs` | `path-core/src/runtime/` | ~1100 LOC |
| `crates/core/src/grammar/{mod,host,schema}.rs` | `bbnf-parse/src/grammar/` | ~1100 LOC |
| `crates/core/src/grammar/generated/` | `bbnf-codegen/src/generated/` | ~169 K LOC (Pass B owns) |
| `crates/core/src/{generate,backend}/` | `bbnf-codegen/src/` | (Pass B owns) |
| `crates/core/src/runtime/{generic substrate}` | `bbnf-runtime/src/` | (Pass B owns) |
| `crates/core/src/runtime/<g>/` | `bbnf-codegen/src/generated/<g>/` | (Pass B owns) |
| `crates/ir/src/types/` | `bbnf-ir/src/types/` (post Lock 2 rename) | ~2 K LOC |
| `crates/ir/src/passes/` | `bbnf-passes/src/` (post Lock 2 rename) | ~12 K LOC |
| `crates/ir/src/vm/` | `bbnf-vm/src/` | (Pass B owns) |
| `crates/bbnf-path/src/path_macro.rs` | `path-core/src/{lex,lower,validate}.rs` | ~639 LOC split |
| `crates/bbnf-path-ts/src/{compile,fixture}.rs` | (deleted; logic at path-core) | ~−720 LOC |
| `crates/core/src/runtime/path.rs` | (deleted; alphabet unified to path-core) | ~−163 LOC |
| `crates/core/src/runtime/<g>/parse_with.rs` legacy lowering | (deleted; typed alphabet direct) | ~−480 LOC |
| `crates/core/src/css_types.rs` | `bbnf-grammar-css-l4/src/host.rs` | 66 LOC |
| `grammar/css/l4/`, `grammar/css/pretty.bbnf`, `grammar/misc/{csv, math, fixtures}.bbnf` | per-grammar dir uniform layout | (renames) |

### 8.3 — Surgery list (summary; full detail at §7 punch list)

| # | Surgery | Wave |
|---|---|---|
| 1-4 | W0 — Lock 11 path-deps + tape narrative scrub | BA W0 |
| 5-11 | W1 — Lock 14 retirement (7 sites in one wave) | BA W1 |
| 12-23 | W2 — Lock 2 rename + Lock 13 IR splits (12 surgeries) | BA W2 |
| 24-26 | W3 — Lock 7 path triplet | BA W3 |
| 27-31 | W4 — `crates/core/` fracture | BA W4 |
| 32-33 | W5 — Grammar source tree + bootstrap dev-binary moves | BA W5 |
| 34-39 | W6 — God-module splits in core / sister crates | BA W6 |
| 40-44 | W7 — New facilities (inverse-layout-audit, bbnf-error, validators, cohort-template, LayoutSink) | BA W7 |
| 45-46 | W8 — Lock 9 + BBNF aggregator | BA W8 |

### 8.4 — LOC budget projections

| Wave | Net LOC delta | Reason |
|---|---|---|
| BA W0 | ~−20 | tape narrative scrub |
| BA W1 | ~−250 | shape_dict_bbnf delete + GrammarAuditTag merge + manifest table retire + markers relocate |
| BA W2 | ~+0 | rename + SPLIT (god modules redistribute) |
| BA W3 | ~−640 | path triplet eliminates ~500 mirror + ~163 legacy alphabet |
| BA W4 | ~+0 | crate fracture (relocations) |
| BA W5 | ~+0 | dir renames |
| BA W6 | ~+0 | god-module SPLITs (redistribute) |
| BA W7 | ~+700 | new facilities (audit, bbnf-error, validators, cohort-template, sink); offset by ~−1500 LOC retirement of hand-written cohort runtime (Pass B carry) |
| BA W8 | ~+0 | API change |

Aggregate Pass A LOC delta: ~−210 (bigger relocations in Pass B-side
deletions per cohort templating). Hand-written-source LOC retired: ~3000
LOC (per CENSUS §10.5). Hand-written-source LOC added: ~700 LOC (new
facilities).

### 8.5 — Hard gates for the new tranche set

| Gate | Wave | Verification |
|---|---|---|
| `rg -nE 'TapeRec\|TapeCursor\|payload_idx\|OpenFrame\|FusedBuilder' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core,path,path-ts}/src/` returns 0 (excluding archived narrative) | BA W0 close | Lock 1 |
| `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core,path,path-ts,bbnf-codegen,bbnf-runtime}/src/` returns 0 | BA W1 close | Lock 14 |
| `rg -nE 'TypeDesc\|StructLayout\|TypeMap' crates/bbnf-ir/src/ crates/bbnf-passes/src/` returns 0 outside doc-archived contexts | BA W2 close | Lock 2 |
| `find crates/{path,path-core,path-ts}/src/ -name "*.rs" \| xargs wc -l \| sort -n` shows no file >500 LOC | BA W3 close | Lock 13 + Lock 7 |
| `cargo check --workspace` passes after `crates/core/` fracture | BA W4 close | Locks 5, 11, 13 |
| `cargo xtask validate-metadata --check` passes | BA W7 close | system-cohesion |
| `inverse-layout-audit` build gate passes for every grammar in workspace metadata | BA W7 close | feedback_typed-materialization-invariant |
| `find crates/ -name "*.rs" -not -path "*generated*" \| xargs wc -l \| awk '$1 > 500'` returns 0 | BA W6 close | Lock 13 |
| `cargo nextest run --workspace` passes | each wave close | regression |

---

## §9 — Closing posture

Pass A's ratification of the parser-front audit lands the architectural
direction for the new tranche set. The verdict ledger (§1) classifies
~200 hand-written source files in scope; the architectural transpositions
(§2) ratify six macro-proposals; the new facilities (§3) name eight
items the absence of which is felt; the cross-cuts (§4) name the
sequencing dependencies; the residues (§5) flag for the synthesizer-
orchestrator; the lock + precept verdicts (§6) settle the lock surface for
Pass A; the punch list (§7) sequences the BA tranche's W0..W8; the
greenfield commitments (§8) form the substrate the synthesizer composes
into the master plan.

Pass A surfaces no relitigation of the locks, no soft verdicts, no
hedging. Every concrete claim cites `path:line`. The greenfield mandate
holds: no quick solutions, no workarounds, gestalt approaches over patches,
architectural transpositions for elegance / simplicity / performance.

The plan from here is the synthesizer's; the substrate is settled.
