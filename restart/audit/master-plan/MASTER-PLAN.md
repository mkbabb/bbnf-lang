# Master Plan — Greenfield Restart Synthesis (2026-05-03)

Synthesizer: master-plan orchestrator. Inputs: Pass A (`audit/restart/PASS-A-2026-05-03.md`, 829 lines), Pass B (`audit/restart/PASS-B-2026-05-03.md`, 548 lines), Pass C (`audit/restart/PASS-C-2026-05-03.md`, 486 lines), 18 per-agent reports under `audit/restart/per-agent/`, and the anchor corpora (`audit/CENSUS-2026-05-03.md`, `audit/MODULES-2026-05-03.md`, `audit/RESTART-SKETCH-2026-05-03.md`, `audit/SOTA-2026-05-03.md`).

This master plan ratifies, reconciles, and composes the three pass syntheses into a single greenfield restart blueprint. It does not relitigate the 14 locks; it does not relitigate the precepts; it does not relitigate the greenfield mandate. The substrate is settled — the master plan's task is sequencing, allocation, and disposition.

The plan is ratified-pending-hardening. After commit, the user invokes `docs/restart/HARDENING.md` against this master plan; if hardening returns *ready to execute*, per-tranche execution agents draft full waves under each `docs/tranches/{X}/{X}.md` stub.

---

## §1 — Executive Summary

The bbnf-lang corpus, surveyed across three independent passes, returns a verdict of **architectural transposition required, not patch**. Pass A's parse front audit (829 lines) identifies fifteen new crates the absence of which is felt, ratifies six macro-proposals (the largest being the `crates/core/` fracture), and surfaces 13 god-module split obligations on the IR side alone. Pass B's codegen + runtime + optimisers audit (548 lines) returns the convergent pivot — Lock 1 (tape dead), Lock 13 (no god directories), and Lock 14 (full grammar generalisation) retire as one architectural movement: per-grammar declaration crates, template-emitted runtimes, direct-projection emit, and a reshaped Emitter trait. Pass C's periphery + tooling + docs + commit-chain audit (486 lines) ratifies Option 3 (keep verbatim + branch reset) for the 2,621-commit chain, executes the Lock 12 archive ceremony as a blocking precondition, and returns a six-wave docs re-do plan covering ~8-13 days of sustained rewriting.

What survives, in essence, is the architecture's cogito — the typed enum + slice-borrow substrate (Lock 9), the IR + per-backend lower contract (Lock 5), the per-domain orthogonal optimization (Lock 4), and the discipline of the precepts. What is reborn is the workspace shape: a consolidated path crate triplet (Lock 7); per-grammar declaration crates carrying generated parser + template-emitted runtime + host functions; sister optimiser crates promoted to either crates.io publication (egraph, csp-solver) or workspace-internal honour (simd-scan); a `bbnf-runtime-template` substrate that emits typed Rust from a single grammar-agnostic generator. What is buried is OpenFrame's heap-stack (the 86.07% samply share), the per-grammar runtime hand-written files (~13,000 LOC), the bbnf-path-ts mirror (`compile.rs` + `fixture.rs`, ~720 LOC), the thirty-method Emitter trait collapsed to eight to ten methods, and the `crates/core/` god directory itself.

The 14-lock honoured posture at greenfield completion is total: every lock either retires through the tranche set or is honoured by construction (Lock 8's perf gates land per-tranche; Locks 12 + 14 retire in tranche A; Lock 1's residue scrubs through tranche A's narrative-scrub pass; Lock 13's god directories dissolve through tranche A and tranche C). The estimated calendar is six to twelve months: tranche A (workspace genesis + Lock 12 ceremony) is the gating precondition, completing in approximately three to four weeks; tranches B through D land the foundational substrate (errors, pipeline, parse, IR, codegen IR contract) over approximately two months; tranche E (the convergent pivot — runtime template + per-grammar declaration crates) is the substrate centerpiece, completing in approximately four to six weeks; tranches F through J distribute optimiser pipeline, slice-borrow API, TS/WASM emitters, sister-crate publication, and cross-backend parity over the remaining calendar.

The greenfield mandate carries through: no quick solutions, no workarounds, idiomatic gestalt approaches, architectural transpositions in the sake of elegance / simplicity / performance, no legacy code surviving uncontested. Every file that appears in the post-restart workspace either appears verbatim from the prior corpus (KEEP-OUTRIGHT bucket), appears with surgical modification (KEEP-MODIFY), appears at a relocated path (ABROGATE-MOVE), or does not appear because its successor is named in the master plan (ABROGATE-DELETE / ABROGATE-REPLACE). Hereupon the synthesizer hands off to hardening.

---

## §2 — Verdict Ledger by Pass

The three passes classified every file in scope into one of five buckets. The aggregated ledger:

| Pass | Files in scope | KEEP-OUTRIGHT | KEEP-MODIFY | ABROGATE-DELETE | ABROGATE-MOVE | ABROGATE-REPLACE |
|---|---:|---:|---:|---:|---:|---:|
| A — parse front | ~200 | ~80 | ~85 | ~6 | ~24 | ~5 |
| B — codegen mid | ~290 | 11 | ~120 | ~6 | ~9 | ~140 |
| C — periphery | ~80 | ~22 | ~38 | ~9 | ~10 | ~1 |
| **Total** | **~570** | **~113** | **~243** | **~21** | **~43** | **~146** |

Commentary per row.

**Pass A (~200 files in scope).** The parse-front substrate is mostly intact at the production level — the LARGE retirement work is god-module SPLITs (13 obligations) plus the seven Lock 14 violation sites plus the path-crate triplet consolidation. The KEEP-OUTRIGHT bucket dominates because the IR's pass ensemble (recognisers, sets, dispatch tables, transform passes) is well-factored at the file level. The KEEP-MODIFY bucket carries the rename obligations (Lock 2: `TypeDesc` → `Layout`), the narrative scrubs (Lock 1: ~9 sites), and the relocation-as-modification carries (every `crates/core/src/path/*.rs` migrates to `crates/path-core/src/`). ABROGATE-DELETE in Pass A is small — `shape_dict_bbnf.rs` (per-grammar mining file), `bbnf-path-ts/src/{compile,fixture}.rs` (the 720-LOC mirror), `runtime/path.rs` (legacy alphabet, 163 LOC). ABROGATE-MOVE dominates the path crate triplet — every `crates/core/src/path/*` relocates. ABROGATE-REPLACE captures the small designed-replacements: `TypeDesc` representation folds into `Layout`; `bbnf-path/src/registry.rs:132-135` retires for metadata-driven `RegistryDescriptor`; `bbnf-path-ts/src/{compile,fixture}.rs` retires for shared `path-core`.

**Pass B (~290 files in scope).** This is where the largest restart-LOC mass concentrates. The 119-file `crates/core/src/backend/` god directory plus the 75-file `crates/core/src/runtime/` god directory plus the 168,750-LOC `crates/core/src/grammar/generated/` tree distribute across new crates. The KEEP-OUTRIGHT bucket is small (11 files; mostly clean optimiser sister files plus the negative-assertion regression gate at `struct_direct_snapshots.rs`). The KEEP-MODIFY bucket carries ~120 files — every backend driver/kernel/strategy/types file relocates to `bbnf-codegen` with narrative scrub and trait-reshape modifications. The ABROGATE-DELETE bucket captures `runtime/path.rs` (duplicate alphabet), `runtime/<g>/parse_with.rs` (legacy lowering, 4 files × ~120 LOC), `backend/rust/emitter/shapes/substrate.rs` (vestigial substrate-selection), and a small pipeline-flat-form file. The ABROGATE-MOVE bucket captures the 9-file generated tree (relocates per-grammar). The ABROGATE-REPLACE bucket — the largest in the entire pass set at ~140 files — captures the 9 per-grammar runtime directories that retire for template emission (one per grammar × ~7 hand-written files), plus the Emitter trait's 30-method shape collapsed to 8-10 methods, plus the struct_direct sub-modules (~2500 LOC across 4 files), plus `builder_template.rs` + `arena_template.rs` (~420 LOC subsumed by `bbnf-runtime-template`), plus xtask's `regen.rs` god module split.

**Pass C (~80 files in scope).** The smallest pass by file count but the largest pass by operational impact — the commit-chain disposition decision and the docs re-do plan together govern the entire restart sequence. The KEEP-OUTRIGHT bucket captures the precepts submodule (untouched), the audit anchors (preserve), and the recently-archived pre-restart-{BA,BB,BC} tranches. The KEEP-MODIFY bucket carries every user-facing doc that survives the rewrite (most of `docs/lang/`, `docs/perf/`, `docs/howto/`), every workspace top-level file (Cargo.toml, README.md, Makefile, .gitignore), and the `analysis` + `lsp` crates that consolidate. The ABROGATE-DELETE bucket captures `server/bbnf-lsp` (committed binary), `*.vsix` (committed releases), `wasm/pkg*/` (build outputs), and `docs/codegen-paths.md` (subsumed by `docs/spec/architecture.md`). The ABROGATE-MOVE bucket captures `crates/{ser, gorgeous}` (Lock 12 archive ceremony), the legacy tranche tree (relocates to `docs/tranches/archive/legacy-Y-BD/`), and the `docs/` subtrees (relocate to lang/perf/howto/process/spec layout). The ABROGATE-REPLACE bucket is small in Pass C because most surgery is move-then-rewrite rather than wholesale replacement.

The consolidated total — ~146 ABROGATE-REPLACE files — confirms that the restart's substrate identity is *replacement by template*, not patching. The per-grammar runtime substrate dissolves wholesale; what stands in its place is `bbnf-runtime-template` consuming `[workspace.metadata.bbnf]` and emitting typed Rust per grammar.

---

## §3 — New Workspace Shape

### §3.1 — Reconciled crate set

The three passes proposed crate sets that overlap considerably. The reconciled target list — adjudicated per the locks and the convergent pivot — comprises **23 crates** at greenfield steady-state. Every crate either appears in this list or moves to `archive/`.

| Crate | Role | Public API surface | Private internals | Dependencies | LOC budget | Migration source |
|---|---|---|---|---|---:|---|
| `crates/bbnf/` | thin user-facing aggregator | `pub use bbnf_parse::*; pub use bbnf_codegen::*; pub use bbnf_runtime::*;` | re-export shell | bbnf-parse, bbnf-codegen, bbnf-runtime, bbnf-grammar-* | ~50 | `crates/core/` re-export carve |
| `crates/bbnf-error/` | unified error trait + canonical wrapper | `BbnfError` trait + `BbnfErrorKind` enum | per-source-crate `From` impls | std | ~150 | new (Pass A facility #4) |
| `crates/bbnf-pipeline/` | pipeline coordinator + compile driver | `Pipeline::compile(grammar, opts) -> CompiledFleet` | compile/{audit, closure_partition, target, timer, pipeline} | bbnf-error, bbnf-parse, bbnf-ir, bbnf-passes, bbnf-codegen | ~600 | `crates/core/src/pipeline.rs` + `pipeline/` consolidate |
| `crates/bbnf-grammar/` | grammar source AST + parse_grammar entry | `AST<'a>`, `RuleEntry<'a>`, `parse_grammar(input)` | source/, lower/ helpers | bbnf-error, parse-that, bbnf-regex | ~1,500 | `crates/core/src/{types, grammar/{mod, host, schema}}` |
| `crates/bbnf-parse/` | source acquisition + parser + lower passes | `parse_grammar`, `lower_grammar` | source/, scanner glue, lower/ | bbnf-error, bbnf-grammar, bbnf-ir, parse-that | ~4,500 | `crates/core/src/{imports, lower}` |
| `crates/bbnf-ir/` | IR types + Layout vocabulary + DAG + registry | `Layout`, `LayoutRegistry`, `LayoutSink`, `RuleType`, etc. | types/, dag/, registry/, cost_config | bbnf-error, indexmap, smallvec | ~2,500 | `crates/ir/src/{types, dag, registry, cost_config}/` |
| `crates/bbnf-passes/` | every IR transformation pass | `lower_grammar_passes`, `Pass` trait + impls | layout/, recognizers/, csp_strategy/, sets/, transform/, materialization/, audit/, etc. | bbnf-error, bbnf-ir, egraph, csp-solver, bbnf-regex | ~14,000 | `crates/ir/src/passes/` (post Lock 2 rename) |
| `crates/bbnf-vm/` | bytecode VM + interpreter | `Vm`, `VmProgram` | passes/codegen, executor | bbnf-error, bbnf-ir | ~600 | `crates/ir/src/vm/` |
| `crates/bbnf-codegen-ir/` | typed 22-variant codegen IR + Emitter trait | `CodegenIR`, `Emitter` (8-10 methods), `LayoutSink` impls | typed_ir/, lower_layout_to_ir | bbnf-error, bbnf-ir | ~1,200 | new (BC.W0 phase-4 derivative) |
| `crates/bbnf-codegen/` | per-backend lowerers + driver + kernels + emit | `lower_to_rust`, `lower_to_ts`, `lower_to_wasm` | driver/, kernels/, strategy/, types/, prettify/, rust/, ts/, wasm/, regen | bbnf-error, bbnf-codegen-ir, bbnf-runtime-template, proc-macro2, quote | ~10,000 | `crates/core/src/{backend, generate}/` |
| `crates/bbnf-runtime-template/` | grammar-agnostic runtime emitter | `emit_runtime(grammar_meta, registry) -> TokenStream` | runtime template fragments | bbnf-error, bbnf-codegen-ir, proc-macro2, quote | ~1,500 | new (Pass B Agent B.5 §1 facility) |
| `crates/bbnf-runtime/` | runtime substrate (mechanism only) | `StructBuilder`, `Arena`, `Handle`, `View` traits | builder/, handle/, view/, error | bbnf-error, smallvec, bumpalo | ~800 | `crates/core/src/runtime/{builder, handle, view, error, mod}` |
| `crates/bbnf-host/` | host fn dispatch substrate (generic) | `HostFn` trait + dispatch table | dispatch/, registry | bbnf-error | ~200 | new (extracted from `crates/core/src/grammar/host.rs` after split) |
| `crates/bbnf-test-fixtures/` | workspace-internal fixture crate | `fixture::twitter_json`, `fixture::bootstrap_css`, etc. | fixtures/{json, css, sheets, ...} | bbnf-error | ~600 src + ~5-10 MB data | new (Pass C facility §4.4); absorbs `data/` |
| `crates/bbnf-bench/` | vitest-style bench harness | bench harness + per-crate bench discovery | harness/, sota_anchors | criterion (or alternative) | ~400 | new (`feedback_vitest-bench`) |
| `crates/path-core/` | shared path types + lex/lower/validate + runtime executor | `Path<'a>`, `PathSchema`, `GrammarMarker`, `runtime::execute` | lex/, lower/, validate/, runtime/{cursor, executor, ascent, variant_select, wildcard} | bbnf-error, bbnf-ir, smallvec | ~2,000 | `crates/core/src/path/*` + `crates/bbnf-path/src/path_macro.rs` (split) |
| `crates/path/` | Rust proc-macro shell | `pointer![...]` macro | thin wrapper over path-core | path-core, proc-macro2, quote, syn | ~150 | `crates/bbnf-path/` rename + shrinkage |
| `crates/path-ts/` | TS cdylib shell | exposes path lowering to TS callers | thin wrapper over path-core | path-core, wasm-bindgen | ~150 | `crates/bbnf-path-ts/` rename + shrinkage |
| `crates/bbnf-language-server/` | LSP server (BBNF source files) | LSP entry + capabilities | analysis/, lsp/, dap/ | bbnf-error, bbnf-parse, bbnf-passes, tower-lsp | ~2,500 | `crates/{analysis, lsp}` consolidate (Pass C §3.1) |
| `crates/bbnf-cli/` | user-facing CLI (deferred to 1.0) | `bbnf {parse, regen, bench, lint}` | clap entrypoint | bbnf, clap | ~1,000 | new (Pass C §4.5; DEFERRED) |
| `crates/parse-that/` | combinator library (path-dep / submodule) | `Parser<'a, T>`, `chain!`, `alt!` | combinators/, parser_impl/ | std | ~2,500 | sibling repo `/Users/mkbabb/Programming/parse-that/rust/parse_that/` |
| `crates/bbnf-regex/` | bespoke regex HIR + DFA + scanner generator | `Regex`, `Hir`, `compile_to_dfa`, scanner emit | hir/, dfa/, scanner/ | std | ~3,500 | sibling repo `/Users/mkbabb/Programming/parse-that/rust/regex/` |
| `crates/egraph/` + `crates/egraph-derive/` | sister optimiser (e-graph + derive macro) | `EGraph<L>`, `Language` trait, derive(Language) | egraph/, derive impl | std (egraph), proc-macro2 (derive) | ~2,400 src + ~340 derive | path-deps until stable; promote to crates.io |
| `crates/csp-solver/` | sister optimiser (CSP + propagation) | `Solver`, `Variable`, `Constraint` trait | solver/, propagation/, scheduler/ | std | ~6,500 | path-dep until stable; promote to crates.io |
| `crates/simd-scan/` | SIMD scanner kernels (workspace-internal) | `scan_until`, `validate_utf8`, etc. | kernels/{x86, aarch64, scalar} | std | ~3,400 | workspace-internal per Lock 11 |
| `crates/bbnf-meta/` | meta-grammar declaration crate | `BbnfMetaParser::parse`, host fns | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime, bbnf-grammar | ~22,000 | per-grammar declaration crate (one per grammar) |
| `crates/json/` | JSON declaration crate | `JsonParser::parse`, `JsonDocument`, `JsonValue` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~3,500 | per-grammar declaration crate |
| `crates/css-l4/` | CSS Level 4 declaration crate | `CssL4Parser::parse`, `CssL4Document`, `CssL4Value` | generated.rs, runtime/, host.rs (`parse_hex_color`), specialised/ | bbnf, bbnf-runtime | ~107,000 | per-grammar declaration crate |
| `crates/google-sheets/` | Google Sheets declaration crate | `SheetsParser::parse`, `SheetsDocument` | generated.rs, runtime/, host.rs, specialised/ | bbnf, bbnf-runtime | ~14,000 | per-grammar declaration crate |
| `crates/bnf/` | BNF declaration crate | `BnfParser::parse` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~3,300 | per-grammar declaration crate (trivial cohort) |
| `crates/csv/` | CSV declaration crate | `CsvParser::parse` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~1,700 | per-grammar declaration crate (trivial cohort) |
| `crates/ebnf/` | EBNF declaration crate | `EbnfParser::parse` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~7,650 | per-grammar declaration crate (trivial cohort) |
| `crates/css-pretty/` | CSS Pretty declaration crate | `CssPrettyParser::parse` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~9,000 | per-grammar declaration crate (trivial cohort) |
| `crates/math/` | Math declaration crate | `MathParser::parse` | generated.rs, runtime/, host.rs | bbnf, bbnf-runtime | ~870 | per-grammar declaration crate (trivial cohort) |

**Aggregate**: 33 workspace members. Of these, 9 are per-grammar declaration crates (one per grammar in `[workspace.metadata.bbnf]`); 8 are core substrate crates (`bbnf-error`, `bbnf-pipeline`, `bbnf-grammar`, `bbnf-parse`, `bbnf-ir`, `bbnf-passes`, `bbnf-vm`, `bbnf-codegen-ir`); 5 are codegen / runtime substrate (`bbnf-codegen`, `bbnf-runtime-template`, `bbnf-runtime`, `bbnf-host`, `bbnf-test-fixtures`); 3 are path crates (`path-core`, `path`, `path-ts`); 1 is the bench harness (`bbnf-bench`); 1 is the language server (`bbnf-language-server`); 1 is the deferred CLI (`bbnf-cli`); 5 are sister / optimiser crates (`parse-that`, `bbnf-regex`, `egraph` + `egraph-derive`, `csp-solver`, `simd-scan`); 1 is the user-facing aggregator (`bbnf`).

### §3.2 — Reconciliation of cross-pass crate proposals

Pass A names 15 new crates. Pass B names 6 new crates plus per-grammar declaration crates. Pass C names 4 new crates (bbnf-language-server, bbnf-test-fixtures, bbnf-cli, bbnf-py [deferred]). The reconciled set differs from any single pass's proposal in the following ways:

1. **`bbnf-grammar` is split from `bbnf-parse`** (per Pass A facility #2). Pass A names `bbnf-grammar` for the source-AST types + parser-entry-point; `bbnf-parse` carries the lower passes. Pass B implicitly assumes a single parse crate. Reconciliation: Pass A is correct — separating grammar-source-types from parse-machinery honours Lock 13's cohesion mandate. The two crates path-dep on each other in a clean direction (bbnf-parse depends on bbnf-grammar; bbnf-grammar does not depend on bbnf-parse).

2. **`bbnf-codegen-ir` is named separately from `bbnf-codegen`** (per Phase-4 BC.W0 derivative). Pass A's Proposal 2 names `bbnf-ir` + `bbnf-passes` + `bbnf-vm`; Pass B names `bbnf-codegen` as a single crate. Reconciliation: the typed 22-variant codegen IR (Phase-4 BC.W0 substrate) is its own crate so the IR contract sits independent of the per-backend lowerers. `bbnf-codegen-ir` carries the typed IR + the `Emitter` trait + the `LayoutSink` impls; `bbnf-codegen` carries the per-backend lowerers (driver, kernels, strategy, prettify, rust, ts, wasm) consuming the IR.

3. **`bbnf-host` extracted from `bbnf-grammar`** (per Pass A's host module split + Lock 14 generality). Pass A's Agent A.5 names a per-grammar declaration crate template; the host substrate (the dispatch + registry mechanism) is generic, so it lives at `crates/bbnf-host/`. Per-grammar host functions live in per-grammar declaration crates.

4. **`bbnf-language-server` consolidates `analysis` + `lsp`** (per Pass C §3.1). Both Pass A and Pass C surface the analysis/lsp grammar-coupling problem; Pass C ratifies the consolidation. The reconciled name is `bbnf-language-server` (verbose, but Lock 14-honest: this crate is BBNF-grammar-specific and names that fact).

5. **`bbnf-test-fixtures` consolidates `data/` + per-crate fixture concerns** (per Pass C §4.4). `data/` at the workspace root relocates here; per-test-crate `tests/fixtures/` ad-hoc dirs converge.

6. **`bbnf-cli` is named but DEFERRED** (per Pass C §4.5). Until 1.0, `cargo xtask regen` + LSP-via-extension cover the dev-loop; the CLI lands as a tranche-J deliverable or post-restart.

7. **`bbnf-py` is named but DEFERRED post-1.0** (per Pass C §4.6). No Python consumer materialised; speculative.

8. **`crates/parse-that/` and `crates/bbnf-regex/`**: Pass A proposes either workspace-member relocation or git-submodule + workspace-member. Reconciliation: the synthesizer adjudicates **submodule + workspace-member** is preferred because it preserves the sibling-repo provenance (per `docs/precepts/CONSUMING.md` precedent). Tranche A's Lock 11 wave executes this.

9. **Per-grammar declaration crate names**: Pass A's footnote names `crates/bbnf-grammar-css-l4/`; Pass B's verdict ledger names `crates/<grammar>/` directly. Reconciliation: **direct name without `bbnf-grammar-` prefix**. The convention `crates/json/`, `crates/css-l4/`, `crates/bbnf-meta/` (the BBNF meta-grammar takes the `bbnf-` prefix because `bbnf` is reserved for the aggregator), `crates/google-sheets/`, etc. Honours Lock 13's directory-cohesion mandate (each directory names a concern, here a grammar). The aggregator at `crates/bbnf/` reserves the unqualified name.

### §3.3 — Workspace member ordering

The `[workspace] members` list orders by dependency depth (shallow first):

```toml
[workspace]
resolver = "2"
members = [
    # Foundation (depth 0)
    "crates/bbnf-error",

    # Sister crates (depth 0 or 1)
    "crates/parse-that",
    "crates/bbnf-regex",
    "crates/egraph",
    "crates/egraph-derive",
    "crates/csp-solver",
    "crates/simd-scan",

    # Core substrate (depth 1-3)
    "crates/bbnf-grammar",
    "crates/bbnf-parse",
    "crates/bbnf-ir",
    "crates/bbnf-passes",
    "crates/bbnf-vm",
    "crates/bbnf-codegen-ir",
    "crates/bbnf-runtime-template",
    "crates/bbnf-runtime",
    "crates/bbnf-host",
    "crates/bbnf-codegen",
    "crates/bbnf-pipeline",

    # Path crate triplet (Lock 7)
    "crates/path-core",
    "crates/path",
    "crates/path-ts",

    # Aggregator + tooling
    "crates/bbnf",
    "crates/bbnf-test-fixtures",
    "crates/bbnf-bench",
    "crates/bbnf-language-server",
    # crates/bbnf-cli — DEFERRED to 1.0

    # Per-grammar declaration crates (one per grammar)
    "crates/bbnf-meta",
    "crates/json",
    "crates/css-l4",
    "crates/google-sheets",
    "crates/bnf",
    "crates/csv",
    "crates/ebnf",
    "crates/css-pretty",
    "crates/math",

    # Build tooling
    "xtask",
]
```

The above lists 33 members + xtask = 34 entries. `archive/` is NOT a workspace member (per Lock 12). External sibling repos (`csc411`, `bbnf-buddy`, `gorgeous-external`, `pprint-external`) remain external.

---

## §4 — New Per-Crate src/ Tree

Per Lock 13, every directory carries 4-10 cohesive children expressing one concern. No file >500 LOC outside `generated/`. Sibling APIs uniform.

### §4.1 — `bbnf-error/src/`

```
bbnf-error/src/
  trait.rs            ← `BbnfError` trait
  kind.rs             ← `BbnfErrorKind` enum + variants
  context.rs          ← `BbnfContext` (path:line + source span helpers)
  span.rs             ← `Span`, `SourceLoc` shared types
  display.rs          ← `Display` + `Debug` impl helpers
  lib.rs              ← re-export shell
```

### §4.2 — `bbnf-pipeline/src/`

```
bbnf-pipeline/src/
  pipeline/
    mod.rs            ← `Pipeline` struct + entry
    audit.rs          ← pipeline-level audit step
    closure_partition.rs  ← grammar closure partition
    target.rs         ← target backend selection
    timer.rs          ← timing instrumentation
  directives.rs       ← @host / @debug directive parsing
  validate.rs         ← pipeline contract validation
  lib.rs
```

### §4.3 — `bbnf-grammar/src/`

```
bbnf-grammar/src/
  ast/
    mod.rs            ← AST types (`AST<'a>`, `RuleEntry<'a>`)
    rule.rs           ← `RuleEntry`, `RuleBody`
    expr.rs           ← `Expr` AST nodes
    map.rs            ← `MapExpr` + value-AST nodes
    types.rs          ← grammar-level type expressions
    serde.rs          ← `Serialize` / `Deserialize` impls
  parse/
    mod.rs            ← `parse_grammar(input) -> AST<'a>`
    entry.rs          ← top-level parse entry
    expression.rs     ← expression-level parse
    value.rs          ← value-AST parse
  imports/
    mod.rs            ← @import directive resolution
    loader.rs         ← file loader
    registry.rs       ← import registry
    resolve.rs        ← path resolution
    errors.rs
  lib.rs
```

### §4.4 — `bbnf-parse/src/`

```
bbnf-parse/src/
  source/             ← input acquisition (relocated `imports/`)
    mod.rs
    loader.rs
    registry.rs
    errors.rs
  scanner/
    mod.rs            ← scanner integration
    glue.rs
  lower/
    mod.rs            ← `lower_grammar(ast) -> ir::Grammar`
    string_interner.rs
    fn_table.rs
    metadata.rs
    expression/
      mod.rs          ← ≤100 LOC re-export
      term.rs
      factor.rs
      mapping.rs
      alt.rs
      closures.rs
      pratt.rs
      repeat.rs
      wrap/
        mod.rs        ← ≤100 LOC re-export
        detect.rs
        map_expr.rs
        payload.rs
    value_expr/
      mod.rs
      atom/
        mod.rs        ← ≤100 LOC re-export
        literal.rs
        projection.rs
        type.rs
      literals.rs
      precedence.rs
      simple_kinds.rs
      unwrap.rs
      view_walk.rs
    view_walk.rs
  errors.rs
  lib.rs
```

### §4.5 — `bbnf-ir/src/`

```
bbnf-ir/src/
  types/
    mod.rs
    grammar/
      mod.rs          ← ≤100 LOC re-export
      def.rs          ← grammar definition
      accessors.rs    ← field accessors
      serde.rs        ← serialize/deserialize
    layout.rs         ← `Layout` (replaces `TypeDesc`)
    layout_interner.rs ← `LayoutInterner` (replaces `TypeDescInterner`)
    node.rs           ← IR node types
    rule.rs           ← rule types
    map_expr.rs       ← MapExpr IR
    fn_descriptor.rs  ← host fn descriptor
    recognizer_configs.rs
  registry/
    mod.rs
    layout.rs         ← `Layout`, `LayoutRegistry` (replaces `StructLayout`, `StructRegistry`)
    sink.rs           ← `LayoutSink` trait (Pass A facility #8)
    strategy.rs       ← `EmitStrategy` (post-Lock-14: consumes xtask-passed `StrategyTable`)
  dag/
    mod.rs
    build.rs
    extract.rs
    intern.rs
    node.rs
  cost_config.rs
  lib.rs
```

### §4.6 — `bbnf-passes/src/`

```
bbnf-passes/src/
  layout/             ← (post Lock-2 rename of `passes/types/`)
    mod.rs            ← ≤100 LOC re-export
    solver.rs         ← layout solver
    projection.rs
    lifetime.rs
    registry_glue.rs
    constraint/
      mod.rs
      ...
    obligation.rs
    subvariants.rs
    generate.rs
  recognizers/
    mod.rs
    structural_shape/  ← (replaces `shape_dict_bbnf.rs`)
      mod.rs
      miner.rs
      template.rs
    grammar_facts/
      mod.rs          ← ≤100 LOC re-export (was 1530 LOC)
      def_facts.rs
      use_facts.rs
      shape_facts.rs
      kind_facts.rs
    balanced_wrap.rs
    comment_ws.rs
    consume_to_next_structural.rs
    context_facts_miner.rs
    dedup_eligibility.rs
    delim_scan.rs
    disjoint_first.rs
    identifier.rs
    kernel_shape.rs
    key_dispatch.rs
    keyword_stats.rs
    list_rules.rs
    node_facts.rs
    operator_chain.rs
    pattern_alphabet.rs
    punct_ws_region.rs
    quoted_string.rs
    separator_list.rs
    signature.rs
    token_led_branches.rs
    shape_dispatch/
      mod.rs
      ... (12-file existing tree)
  csp_strategy/
    mod.rs            ← ≤100 LOC re-export
    solver_wiring.rs
    domains.rs
    materialization_glue.rs
    components.rs
    constraints/
      mod.rs
      ...
  csp_domains/
    mod.rs
    ... (per-domain split; was 500 LOC)
  materialization/
    mod.rs
    classify/         ← (was 843 LOC)
      mod.rs
      ...
    lattice.rs
    pin_sweep.rs
  payload/
    mod.rs
    layout/           ← (was 514 LOC)
      mod.rs
      ...
    named_types.rs
    scalar_routing.rs
  audit/
    mod.rs
    payload_coverage/  ← (was 585 LOC)
      mod.rs
      classify.rs
      walk.rs
      report.rs
    inverse_layout/    ← (Pass A facility #1)
      mod.rs
      walker.rs
      validator.rs
  context/
    mod.rs
    facts.rs
  facts/
    mod.rs
  inspect/
    mod.rs
    leading.rs
    literal.rs
    resolve.rs
    unwrap.rs
    walk.rs
  patterns/
    mod.rs
  sets/
    mod.rs
    ... (existing tree)
    dispatch/
      mod.rs
      ... (existing tree)
  transform/
    mod.rs
    ... (existing tree)
    fuse_token/
      mod.rs
      ...
  egraph/
    mod.rs
    ... (existing egraph passes)
  rewrites/
    mod.rs
    ...
  recognizer/
    mod.rs
    facts.rs
    plans.rs
  inline_trace.rs
  lr.rs
  metadata.rs
  path_check.rs
  prefix.rs
  profile.rs           ← (post-Lock-14: drop `bbnf_shape_templates` field)
  regex_info.rs
  span.rs
  lib.rs
```

### §4.7 — `bbnf-vm/src/`

```
bbnf-vm/src/
  vm.rs               ← VM core
  program.rs          ← VM program type
  executor.rs         ← step-execution
  passes/
    mod.rs
    codegen.rs        ← bytecode codegen pass
  lib.rs
```

### §4.8 — `bbnf-codegen-ir/src/`

```
bbnf-codegen-ir/src/
  ir/
    mod.rs            ← `CodegenIR` (22-variant typed IR per Phase-4 BC.W0)
    variants.rs       ← variant definitions
    lower.rs          ← `lower_layout_to_ir(layout, registry) -> CodegenIR`
  emitter/
    mod.rs            ← `Emitter` trait (8-10 methods)
    sink.rs           ← `LayoutSink` impl bridge
  contracts.rs        ← invariants + verifier
  lib.rs
```

### §4.9 — `bbnf-codegen/src/`

```
bbnf-codegen/src/
  driver/             ← per-shape walking driver
    mod.rs
    alt.rs
    seq.rs
    repeat.rs
    wrap.rs
    reference.rs
    map.rs
    node.rs
    analysis.rs
    prettify.rs
  kernels/
    mod.rs
    charclass.rs
    charset_shapes.rs
    identifier.rs
    number.rs
    balanced_wrap.rs
    prefix_class.rs
    punct_ws_region.rs
  strategy/
    mod.rs
    alt_strategy.rs
    seq_strategy.rs
    ref_strategy.rs
    repeat_strategy.rs
  types/
    mod.rs
    decisions.rs
  prettify/
    mod.rs
    types.rs
    plan.rs
    sep_rewrite.rs
    analysis.rs
  rust/                ← Rust backend
    mod.rs
    lower.rs          ← `lower_to_rust`
    emitter/
      mod.rs
      grammar.rs
      shapes/         ← per-shape emit (post-trait-collapse)
        mod.rs
        alt.rs
        seq.rs
        repeat.rs
        wrap.rs
        reference.rs
        map.rs
        scalar.rs
        string.rs
        number.rs
        object.rs
        ...
    keyword_dispatch.rs
    path_plan.rs
    precedence.rs
    profile.rs
    regex_scan_adapter/  ← (was 786 LOC; split god module)
      mod.rs
      ...
    registry_emit.rs
    prettify/
      mod.rs
      ...
    analysis/
      mod.rs
      inline.rs
      visit.rs
      constraints.rs
    view/
      mod.rs
      named_types.rs
  ts/                  ← TypeScript backend (deferred to tranche H)
    mod.rs
    lower.rs
    emitter.rs
    ...
  wasm/                ← WebAssembly backend (deferred to tranche H)
    mod.rs
    lower.rs
    emitter.rs
    ...
  regen/               ← (was 849 LOC; split god module)
    mod.rs
    manifest.rs
    pipeline.rs
    emit.rs
    check.rs
    staged.rs
  lib.rs
```

### §4.10 — `bbnf-runtime-template/src/`

```
bbnf-runtime-template/src/
  emit/
    mod.rs            ← `emit_runtime(meta, registry) -> TokenStream`
    value.rs          ← per-grammar `<G>Value` enum emit
    document.rs       ← per-grammar `<G>Document` emit
    view.rs           ← per-grammar `<G>View` emit
    kind.rs           ← per-grammar `<G>Kind` enum emit
    arena.rs          ← per-grammar arena impl emit
    builder.rs        ← per-grammar StructBuilder impl emit
  template/
    mod.rs
    fragments.rs      ← shared fragment generators
  contracts.rs        ← invariant checks per emitted module
  lib.rs
```

### §4.11 — `bbnf-runtime/src/`

```
bbnf-runtime/src/
  builder/
    mod.rs            ← `StructBuilder` trait
    arena.rs          ← `Arena` trait
  handle.rs           ← `Handle<T>` type
  view.rs             ← `View<T>` type
  errors.rs
  lib.rs
```

### §4.12 — `bbnf-host/src/`

```
bbnf-host/src/
  dispatch.rs         ← host fn dispatch
  registry.rs         ← host fn registry
  fn_descriptor.rs    ← `HostFn` trait + descriptor
  lib.rs
```

### §4.13 — `path-core/src/`

```
path-core/src/
  ast/
    mod.rs            ← `Path<'a>` AST
    segment.rs        ← `PathSegment<'a>`
    schema.rs         ← `PathSchema`, `GrammarMarker`
  lex.rs              ← (extracted from old `path_macro.rs`)
  lower.rs            ← (extracted)
  validate.rs         ← (extracted)
  emit.rs             ← (extracted)
  type_check.rs       ← `check_path`
  registry.rs         ← (consumes per-grammar emitted REGISTRY const; metadata-driven)
  runtime/
    cursor.rs
    executor.rs
    ascent.rs
    variant_select.rs
    wildcard.rs
  errors.rs
  lib.rs
```

### §4.14 — `path/src/`

```
path/src/
  lib.rs              ← `pointer![...]` proc-macro entry
```

### §4.15 — `path-ts/src/`

```
path-ts/src/
  lib.rs              ← cdylib entry
  schema.rs           ← TS-side schema
  template_tag.rs     ← TS template tag string
```

### §4.16 — `bbnf-language-server/src/`

```
bbnf-language-server/src/
  analysis/           ← (consolidated from `crates/analysis/`)
    mod.rs
    directives/
      mod.rs
      hints.rs        ← (Lock 10 audit: ZERO @pratt/@simd entries)
    diagnostics.rs
    completion.rs
    hover.rs
  lsp/                ← (consolidated from `crates/lsp/`)
    mod.rs
    server.rs
    handlers.rs
    capabilities.rs
  dap.rs              ← (debug adapter; if present)
  lib.rs
  bin/
    bbnf-lsp.rs       ← server binary entry
```

### §4.17 — `bbnf/src/`

```
bbnf/src/
  lib.rs              ← `pub use bbnf_parse::*; pub use bbnf_codegen::*; pub use bbnf_runtime::*; pub use bbnf_grammar::*;`
```

### §4.18 — `bbnf-test-fixtures/src/`

```
bbnf-test-fixtures/
  src/
    fixtures/
      mod.rs
      json.rs         ← `pub fn twitter_json() -> &'static str`
      css.rs          ← `pub fn bootstrap_css() -> &'static str`
      sheets.rs
      ...
    lib.rs
  data/               ← raw fixture data files
    json/
      twitter.json
      citm_catalog.json
      canada.json
    css/
      bootstrap.css
    ...
```

### §4.19 — `bbnf-bench/src/`

```
bbnf-bench/
  src/
    harness/
      mod.rs
      runner.rs
      report.rs
    sota_anchors.rs   ← per-Lock-8 baselines
    lib.rs
  benches/            ← (per-grammar benches live in per-grammar declaration crates)
```

### §4.20 — Per-grammar declaration crate skeleton (`crates/<grammar>/`)

```
crates/<grammar>/
  src/
    generated.rs      ← (xtask-emitted; per-grammar parser; ~3K-107K LOC depending on grammar)
    runtime/          ← (xtask-emitted by `bbnf-runtime-template`)
      mod.rs
      value.rs
      document.rs
      view.rs
      kind.rs
      arena.rs
      builder.rs
    host.rs           ← (hand-written host fns; e.g., css-l4 carries `parse_hex_color`)
    specialised/      ← (only for specialised cohort: bbnf, json, css-l4, sheets)
      mod.rs
      ...
    lib.rs
  tests/
    parse.rs
    runtime_invariants.rs  ← (Pass B Agent B.6 §6 contract test)
    ...
  benches/
    parse.rs          ← (per-Lock-8 anchored benches)
  Cargo.toml
```

The trivial cohort (bnf, csv, ebnf, css-pretty, math) emits 100% from template; `specialised/` is absent. The specialised cohort (bbnf-meta, json, css-l4, google-sheets) carries `specialised/` for hand-written extensions (CSS L4 colour functions, BBNF aggregator overrides, Sheets path-query canonical wiring).

---

## §5 — Tranche Set

The fresh tranche set comprises **ten tranches A through J**, each bounded around one architectural movement. The set is sequenced so each tranche lands a substrate or transposition with a same-wave or next-wave consumer (per `feedback_substrate-without-consumer` and Era V's lesson).

### §5.1 — Master tranche table

| Tranche | Name | Gestalt | Wave count | Calendar | Carry FROM | Carry TO |
|---|---|---|---:|---|---|---|
| A | Workspace genesis | Lock 12 ceremony, commit-chain disposition, crate skeletons land, sister-crate path-deps | 6 | 3-4 weeks | (none) | B, C |
| B | bbnf-error + bbnf-pipeline | unified error types, pipeline coordinator, narrative scrub, naming canon | 4 | 2-3 weeks | A | C, D |
| C | Parse + IR foundation | bbnf-grammar + bbnf-parse + bbnf-ir + bbnf-passes; Lock 2 rename; Lock 13 IR splits | 7 | 4-5 weeks | A, B | D, E |
| D | Codegen IR contract | bbnf-codegen-ir; 22-variant typed IR; Emitter trait reshape; Rust lowerer smoke | 5 | 3-4 weeks | C | E, F |
| E | Per-grammar declaration crates + runtime template | bbnf-runtime-template, 9 declaration crates, direct-projection emit (the convergent pivot) | 8 | 5-6 weeks | C, D | F, G, H |
| F | Optimiser pipeline | egraph + csp-solver + miners + cost-model output-piping; Pratt + SIMD auto-detection | 6 | 3-4 weeks | C, D, E | G, J |
| G | Slice-borrow API + pointer macro + visitor surface | bbnf-runtime user-facing API; parse / parse_in / parse_owned; pointer![] | 5 | 3 weeks | E, F | H, J |
| H | TS + WASM emitters | bbnf-codegen TS + WASM activation; cross-backend smoke | 4 | 3 weeks | D, E, G | I, J |
| I | Sister-crate publication | egraph / csp-solver / bbnf-regex publish prep; parse-that disposition | 3 | 2 weeks | A, F | J |
| J | Cross-backend parity + close | parity matrix; final perf gates against sonic-rs / simdjson / lightning-css; SOTA validation | 5 | 3-4 weeks | All | (close) |

**Aggregate**: 53 waves across 10 tranches. Estimated calendar 30-37 weeks (~7-9 months) at sustained execution; ~6-12 month band per the suite mandate, accounting for hardening passes and triumvirate redress between tranches.

### §5.2 — Per-tranche gestalt commentary

**Tranche A — Workspace genesis.** This is the precondition tranche. Lock 12 archive ceremony lands first (per Pass C §8.1.1; blocking). The commit-chain disposition executes per Pass C §7.5 (Option 3: keep verbatim + branch reset). The empty crate skeletons land per §3.1. Sister-crate path-deps register (Pass A facility — `parse-that`, `bbnf-regex`, `csp-solver` move to path-dep). The narrative-scrub pass eliminates ~50 tape residue sites. The IR Lock-14 retirement (the seven sites; per Pass A §7 W1) lands here because they block the IR fracture. Tranche A closes with `cargo check --workspace` green on the new crate set with skeletal contents.

**Tranche B — bbnf-error + bbnf-pipeline foundation.** The unified error trait + canonical wrapper land first (per Pass A facility #4). Per-crate `Error` types adopt `BbnfError` impl. The pipeline coordinator consolidates `crates/core/src/pipeline.rs` + `pipeline/`. The Lock 2 renaming is partially staged here (the `passes/types/` → `passes/layout/` directory rename is mechanical and lands in B's W3); the substantive `TypeDesc` → `Layout` representation fold happens in tranche C. Naming canon (the post-Lock-2 vocabulary) audits across the workspace.

**Tranche C — Parse + IR foundation.** The parser-front substrate consolidates: `bbnf-grammar` (source AST + parse_grammar), `bbnf-parse` (lower passes), `bbnf-ir` (Layout vocabulary + DAG + registry), `bbnf-passes` (every transformation pass). The Lock 2 substantive fold lands (`TypeDesc` → `Layout`; `StructLayout` → `Layout`; `TypeMap` retires; `LayoutSink` trait lands). The 13 god-module SPLIT obligations land (Pass A §7 W2 + W6). The `bbnf-vm/` extraction lands. The `bbnf-host/` extraction from `grammar/host.rs` lands. The `path-core/` substrate begins consolidation.

**Tranche D — Codegen IR contract.** The 22-variant typed codegen IR (per Phase-4 BC.W0 derivative) lands at `crates/bbnf-codegen-ir/`. The `Emitter` trait reshape (30 methods → 8-10) lands per Pass B §2.c. The Rust lowerer smoke test passes — at least one grammar (BBNF or JSON) round-trips through `bbnf-codegen-ir` → `bbnf-codegen::rust::lower` → emitted source compiling. The `LayoutSink` trait wires per-backend.

**Tranche E — Per-grammar declaration crates + runtime template.** The convergent pivot — Lock 1 + Lock 13 + Lock 14 retire as one architectural movement. `bbnf-runtime-template` lands at `crates/bbnf-runtime-template/`. The 9 per-grammar declaration crates scaffold and adopt template-emitted runtimes. Direct-projection emit retires OpenFrame across all grammars. The 86.07% samply share (per RESTART-SKETCH §A.7) collapses by mechanism. The hand-written 13K-LOC per-grammar runtime files retire (5 trivial cohort × ~480 LOC = ~2400 LOC immediate; 4 specialised cohort retain `specialised/` for extensions only). Tranche E is the substrate centerpiece — the largest single-tranche surface in the restart.

**Tranche F — Optimiser pipeline.** Per Lock 4, the per-domain orthogonal optimisation: CSP, e-graph, pattern miners, cost model compose by output-piping. The cost-model output-piping lands (per Pass B Agent B.5 §6: cost-model stays in egraph per `feedback_kiss-perf-bias`). Pratt + SIMD auto-detection lands per Lock 10 (no `@pratt`/`@simd` directives). Same-wave consumer for each rewrite tier ensures Era V's failure mode does not repeat.

**Tranche G — Slice-borrow API + pointer macro + visitor surface.** The bbnf-runtime user-facing API lands per Lock 9. `parse(input)` (slice-borrow), `parse_in(input, &bump)` (arena), `parse_owned(input)` (owned) — three surfaces over one parse impl, lifetime-discriminated. The `pointer![...]` macro per Lock 7 (sonic-rs convention). The `Visitor<'i, T>` + `VisitTypes` per lightning-css convention.

**Tranche H — TS + WASM emitters.** The `bbnf-codegen::ts` and `bbnf-codegen::wasm` activate. Cross-backend smoke: at least one grammar emits valid TS + WASM. The post-tranche-D Emitter trait collapse means TS + WASM share the per-shape walking pattern.

**Tranche I — Sister-crate publication.** Per Lock 11, the path-dep crates promote: egraph + egraph-derive + csp-solver to crates.io (API freeze + version + publish). bbnf-regex publishes alongside. parse-that disposition per Pass A Proposal 4 (submodule-as-workspace-member is the synthesizer's adjudication). simd-scan stays workspace-internal explicitly.

**Tranche J — Cross-backend parity + close.** Cross-backend parity matrix (Rust ↔ TS ↔ WASM emit equivalence on every grammar). Final perf gates per Lock 8: sonic-rs M1 Pro twitter ≤ 436 µs; lightning-css Bootstrap ≤ 4.16 ms; simdjson On-Demand 7 GB/s. The close ceremony lands `FINAL.md` per tranche; the master plan's hardening close fires; the restart suite closes.

### §5.3 — Stub creation

For each tranche A through J, a stub at `docs/tranches/{X}/{X}.md` lands per the master plan's commit. The stubs have ~150-300 lines each, covering: gestalt, hard gates, wave summary table, carry-tags FROM, carry-tags TO, 14-lock honoured cell map, risks, build/iter time gate, voice locks, closing posture. The waves themselves are drafted by per-tranche execution agents post-hardening.

---

## §6 — Workspace + Cargo.toml Schema

### §6.1 — Top-level `Cargo.toml`

```toml
[workspace]
resolver = "2"
members = [
    # ... per §3.3 ordering ...
]

[workspace.package]
edition = "2021"
rust-version = "1.78"
license = "MIT OR Apache-2.0"
authors = ["Mike Babb <mbabb@ncsu.edu>"]
repository = "https://github.com/mkbabb/bbnf-lang"

[workspace.dependencies]
# Foundation
bbnf-error = { path = "crates/bbnf-error", version = "0.1" }
# Core substrate
bbnf-grammar = { path = "crates/bbnf-grammar", version = "0.1" }
bbnf-parse = { path = "crates/bbnf-parse", version = "0.1" }
bbnf-ir = { path = "crates/bbnf-ir", version = "0.1" }
bbnf-passes = { path = "crates/bbnf-passes", version = "0.1" }
bbnf-vm = { path = "crates/bbnf-vm", version = "0.1" }
bbnf-codegen-ir = { path = "crates/bbnf-codegen-ir", version = "0.1" }
bbnf-codegen = { path = "crates/bbnf-codegen", version = "0.1" }
bbnf-runtime-template = { path = "crates/bbnf-runtime-template", version = "0.1" }
bbnf-runtime = { path = "crates/bbnf-runtime", version = "0.1" }
bbnf-host = { path = "crates/bbnf-host", version = "0.1" }
bbnf-pipeline = { path = "crates/bbnf-pipeline", version = "0.1" }
# Path triplet
path-core = { path = "crates/path-core", version = "0.1" }
path = { path = "crates/path", version = "0.1" }
path-ts = { path = "crates/path-ts", version = "0.1" }
# Aggregator + tooling
bbnf = { path = "crates/bbnf", version = "0.1" }
bbnf-test-fixtures = { path = "crates/bbnf-test-fixtures", version = "0.1" }
bbnf-bench = { path = "crates/bbnf-bench", version = "0.1" }
bbnf-language-server = { path = "crates/bbnf-language-server", version = "0.1" }
# Sister crates (path-dep until stable)
parse-that = { path = "crates/parse-that", version = "0.1" }
bbnf-regex = { path = "crates/bbnf-regex", version = "0.1" }
egraph = { path = "crates/egraph", version = "0.1" }
egraph-derive = { path = "crates/egraph-derive", version = "0.1" }
csp-solver = { path = "crates/csp-solver", version = "0.1" }
simd-scan = { path = "crates/simd-scan", version = "0.1" }

# External dependencies
indexmap = "2"
smallvec = "1"
bumpalo = "3"
proc-macro2 = "1"
quote = "1"
syn = "2"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
toml = "0.8"

[workspace.metadata.bbnf]
# Per-grammar manifest; consumed by xtask + bbnf-codegen at regen time.
# Per Lock 14: this is the dispatch surface; ZERO grammar-specific code in any generic crate.
# Per Lock 6: xtask reads this directly to emit committed source artefacts.

[workspace.metadata.bbnf.grammars.bbnf-meta]
source_path = "grammar/bbnf/bbnf.bbnf"
declaration_crate = "crates/bbnf-meta"
recognisers = ["structural_shape:big_comment", "structural_shape:mapped_factor"]
host_fns = []
pratt_eligibility = "force"
simd_eligibility = "auto"
output_dir = "crates/bbnf-meta/src"
features = ["serialize", "prettify"]
cohort = "specialised"

[workspace.metadata.bbnf.grammars.json]
source_path = "grammar/json/json.bbnf"
declaration_crate = "crates/json"
recognisers = []
host_fns = []
pratt_eligibility = "auto"
simd_eligibility = "force"
output_dir = "crates/json/src"
features = ["serialize", "prettify", "structural"]
cohort = "specialised"

[workspace.metadata.bbnf.grammars.css-l4]
source_path = "grammar/css-l4/css-l4.bbnf"
declaration_crate = "crates/css-l4"
recognisers = []
host_fns = ["parse_hex_color"]
pratt_eligibility = "auto"
simd_eligibility = "auto"
output_dir = "crates/css-l4/src"
features = ["serialize", "prettify"]
cohort = "specialised"

[workspace.metadata.bbnf.grammars.google-sheets]
source_path = "grammar/google-sheets/google-sheets.bbnf"
declaration_crate = "crates/google-sheets"
recognisers = []
host_fns = []
pratt_eligibility = "force"
simd_eligibility = "auto"
output_dir = "crates/google-sheets/src"
features = ["serialize"]
cohort = "specialised"

[workspace.metadata.bbnf.grammars.bnf]
source_path = "grammar/bnf/bnf.bbnf"
declaration_crate = "crates/bnf"
recognisers = []
host_fns = []
pratt_eligibility = "skip"
simd_eligibility = "skip"
output_dir = "crates/bnf/src"
features = []
cohort = "trivial"

[workspace.metadata.bbnf.grammars.csv]
source_path = "grammar/csv/csv.bbnf"
declaration_crate = "crates/csv"
recognisers = []
host_fns = []
pratt_eligibility = "skip"
simd_eligibility = "auto"
output_dir = "crates/csv/src"
features = []
cohort = "trivial"

[workspace.metadata.bbnf.grammars.ebnf]
source_path = "grammar/ebnf/ebnf.bbnf"
declaration_crate = "crates/ebnf"
recognisers = []
host_fns = []
pratt_eligibility = "skip"
simd_eligibility = "skip"
output_dir = "crates/ebnf/src"
features = []
cohort = "trivial"

[workspace.metadata.bbnf.grammars.css-pretty]
source_path = "grammar/css-pretty/css-pretty.bbnf"
declaration_crate = "crates/css-pretty"
recognisers = []
host_fns = []
pratt_eligibility = "skip"
simd_eligibility = "skip"
output_dir = "crates/css-pretty/src"
features = ["prettify"]
cohort = "trivial"

[workspace.metadata.bbnf.grammars.math]
source_path = "grammar/math/math.bbnf"
declaration_crate = "crates/math"
recognisers = []
host_fns = []
pratt_eligibility = "force"
simd_eligibility = "skip"
output_dir = "crates/math/src"
features = []
cohort = "trivial"
```

The per-grammar table replaces the prior `[workspace.metadata.bbnf-strategy]` (which Pass A §1.13 marked KEEP-MODIFY). Lock 14's verification command — `cargo xtask validate-metadata --check` — confirms every key is present and well-formed.

### §6.2 — Per-crate Cargo.toml schema

Each per-grammar declaration crate carries a uniform Cargo.toml:

```toml
[package]
name = "<grammar>"
edition.workspace = true
rust-version.workspace = true
license.workspace = true
authors.workspace = true
repository.workspace = true
version = "0.1.0"

[dependencies]
bbnf.workspace = true
bbnf-runtime.workspace = true
bbnf-error.workspace = true
# (specialised cohort only)
# bumpalo.workspace = true (if used)

[dev-dependencies]
bbnf-test-fixtures.workspace = true

[[bench]]
name = "parse"
harness = false
required-features = []
```

### §6.3 — xtask Cargo.toml additions

```toml
[package]
name = "xtask"
# ...

[dependencies]
bbnf-codegen.workspace = true
bbnf-runtime-template.workspace = true
bbnf-error.workspace = true
clap = "4"
toml.workspace = true

[[bin]]
name = "xtask"
path = "src/main.rs"

[[bin]]
name = "dump_ir"
path = "src/bin/dump_ir.rs"

[[bin]]
name = "cost_grid_sweep"
path = "src/bin/cost_grid_sweep.rs"

[[bin]]
name = "debug_parse"
path = "src/bin/debug_parse.rs"
```

The three bins relocate from `crates/bootstrap/src/bin/` per Pass A §7 W5.33.

---

## §7 — Commit-Chain Disposition

### §7.1 — Ratification of Pass C's recommendation

Pass C ratifies **Option 3: keep verbatim + branch reset** (per Pass C §7.4, §7.5). The synthesizer **ratifies Pass C's recommendation outright**.

Deciding rationale (as Pass C presents and the synthesizer affirms):

1. **Provenance preservation is non-negotiable.** Memory items `accurate-perf-narrative` and `perf-breakthrough-accuracy` cite specific commit SHAs. The archaeology document references commit-by-commit history. Squashing breaks the project's own attribution.

2. **The commits ARE the lessons.** Era V's failure-mode anatomy (substrate-then-substrate-then-ship) is *commitable* — each commit carries the per-substrate-build reasoning. `git log -- crates/tape/src/dta.rs` produces the DTA arc's full archaeology.

3. **Operational cost is manageable.** 2,621 commits clones in seconds. GitHub renders the chain. `git log --oneline | head/tail` operates instantly.

4. **Branch reset is cheap.** Tag preserves the prior chain; new branch carries the prelude.

5. **Future commit discipline absorbs failure modes.** Per `feedback_clean-instrumentation`, `dispatch-hard-cap`, `triumvirate-discipline`, `no-metalanguage-docs` (commit subjects), the post-restart commits are cleaner.

### §7.2 — Operational sequence (ratified verbatim from Pass C §7.5)

```bash
# Pre-flight
git status                                               # clean working tree
cd docs/precepts && git status && git rev-parse HEAD     # submodule clean + pinned
cd ../..

# 1. Tag current HEAD as the pre-restart provenance anchor.
git tag pre-restart-2026-05-03 master

# 2. Push everything — closes the 1,724-unpushed gap; preserves provenance.
git push origin master
git push origin pre-restart-2026-05-03

# 3. Open a new branch starting at current master.
git checkout -b master-greenfield-2026-05-03 master

# 4. Land the restart prelude as ~8 focused commits:
#    Commit 1 — Lock 12 archive ceremony (ser + gorgeous → archive/).
#    Commit 2 — crates/{analysis, lsp} consolidation → crates/bbnf-language-server/.
#    Commit 3 — docs/ tree restructure (mechanical relocation).
#    Commit 4 — docs/tranches/ archive (legacy Y-BD relocate).
#    Commit 5 — README + GESTALT rewrite per restart vocabulary.
#    Commit 6 — .gitignore additions; delete committed build artefacts.
#    Commit 7 — SPEC.md + architecture.md + migration record + tranche A-J stubs.
#    Commit 8 — Master Plan + per-pass restart audit synthesis.

# 5. Push the new branch.
git push -u origin master-greenfield-2026-05-03

# 6. Hardening pass per docs/restart/HARDENING.md.

# 7. Cutover decision (USER):
#    - Option A: master-greenfield-2026-05-03 → master.
#      git branch -m master-greenfield-2026-05-03 master
#      git push --force-with-lease origin master
#      (pre-restart-2026-05-03 tag preserves the prior chain.)
#    - Option B: keep both; master continues; greenfield evolves separately.
```

### §7.3 — Estimated time

Operational sequence (steps 1-5): ~1 hour mechanical (the 8 prelude commits include the Lock 12 ceremony, the analysis+lsp consolidation, the docs restructure — most of which is `git mv` plus Cargo.toml edits plus README rewrite). The Lock 12 ceremony alone is ~15 minutes. The docs restructure mechanical sweep is ~30 minutes. The 8 prelude commits land in ~3-4 hours over a single working session.

Hardening pass (step 6): per `docs/restart/HARDENING.md`, ~45 min per target × ~3 targets (master plan + 1-2 specific tranche stubs if drafted) = ~2 hours.

Cutover decision (step 7): user-gated. The synthesizer recommends **Option A** (force-push the greenfield branch to master, with the pre-restart tag preserving prior chain) at the moment hardening returns *ready to execute* across the master plan + tranche A. Option B (keep both branches) introduces split-brain governance and is dispreferred.

### §7.4 — Reversibility analysis

The pre-restart-2026-05-03 tag is the safety net. Every legacy commit is reachable via `git checkout pre-restart-2026-05-03`. If the greenfield branch develops a fault (architectural, hardening returns *requires re-draft*), the user reverts master to the tag (`git reset --hard pre-restart-2026-05-03`) and the prior chain is restored.

The greenfield branch's prelude (8 commits) is small enough that a per-commit revert is also trivial. The risk surface is minimal.

---

## §8 — Docs Re-Do Plan

### §8.1 — Ratification of Pass C's structure

Pass C names the post-restart `docs/` shape (per §3.3):

```
docs/
  GESTALT.md
  README.md (project navigation)
  lang/{bbnf, parse-that, pprint, gorgeous}/
  perf/
  howto/{cookbook, optimizer, migration}/
  process/{precepts, restart, instructions, tranches}/
  spec/{SPEC.md, architecture.md, codegen.md}
  audit → workspace-root /audit/
```

The synthesizer **ratifies Pass C's structure outright**. Five immediate children (`lang/`, `perf/`, `howto/`, `process/`, `spec/`) plus `GESTALT.md` + `README.md`. Each child names one concern. Cohesion clean.

### §8.2 — Master tables

#### §8.2.1 — Docs to keep verbatim

| Doc | Path | Reason |
|---|---|---|
| Precepts submodule | `docs/precepts/` | Pinned by SHA; out of restart scope |
| Recently archived pre-restart tranches | `docs/tranches/archive/pre-restart-{BA,BB,BC}/` | Already archived |

#### §8.2.2 — Docs to rewrite

| Doc | From | To | Wave | Cost |
|---|---|---|---|---|
| `docs/bbnf/*` | `docs/bbnf/` | `docs/lang/bbnf/` | C.W3 | 1-2 days |
| `docs/parse-that/*` | `docs/parse-that/` | `docs/lang/parse-that/` | C.W3 | 0.5 day |
| `docs/pprint/*` | `docs/pprint/` | `docs/lang/pprint/` | C.W3 | 0.5 day |
| `docs/gorgeous/*` | `docs/gorgeous/` | `docs/lang/gorgeous/` | C.W3 | 0.5 day |
| `docs/performance/*` | `docs/performance/` | `docs/perf/` (full Lock 8 rewrite; AU silent) | C.W4 | 2-3 days |
| `docs/cookbook/*` | `docs/cookbook/` | `docs/howto/cookbook/` (mostly-honoured; minor metalanguage strip) | C.W4 | 1 day |
| `docs/optimizer/*` | `docs/optimizer/` | `docs/howto/optimizer/` | C.W4 | 0.5 day |
| `docs/migration/*` | `docs/migration/` | `docs/howto/migration/` | C.W4 | 1 day |
| `README.md` | repo root | repo root (full rewrite per restart vocabulary) | A.W6 | 0.5 day |
| `docs/GESTALT.md` | `docs/GESTALT.md` | `docs/GESTALT.md` (rewrite to reflect post-restart shape) | A.W6 | 0.5 day |

#### §8.2.3 — Docs to delete

| Doc | Path | Reason |
|---|---|---|
| `docs/codegen-paths.md` | `docs/codegen-paths.md` | Subsumed by `docs/spec/architecture.md` |
| `server/bbnf-lsp` | repo binary | Build artefact; .gitignore `server/` |
| `extension/bbnf-language-support-1.0.{3,5}.vsix` | `extension/` | Committed releases; .gitignore `*.vsix` |
| `wasm/{pkg, pkg-node, pkg-node-debug}/` | `wasm/` | Build outputs; .gitignore `wasm/pkg*/` |

#### §8.2.4 — Docs to relocate

| Doc | From | To |
|---|---|---|
| Workspace top-level `audit/` partitions | `audit/` | `audit/{codebase-2026-05-03, plan-2026-05-03, restart-2026-05-03}/` |
| `docs/instructions/` | `docs/instructions/` | `docs/process/instructions/` |
| `docs/restart/` | `docs/restart/` | `docs/process/restart/` (or kept if user prefers; Pass C is silent on this) |
| `docs/tranches/` | `docs/tranches/` | `docs/process/tranches/` (the legacy Y-BD letters relocate under `archive/legacy-Y-BD/`; the new A-J set lives at `docs/process/tranches/A/` etc.) |
| `docs/PHASE-4-DIRECTIVE-2026-05-03.md` | `docs/` | `audit/plan-2026-05-03/` |
| `data/` | repo root | `crates/bbnf-test-fixtures/data/` |

#### §8.2.5 — New docs to create

| Doc | Path | Wave | Cost | Source |
|---|---|---|---|---|
| `docs/spec/SPEC.md` | new | C.W7 | 5 days | Pass C facility §4.1 |
| `docs/spec/architecture.md` | new | C.W7 | 3 days | Pass C facility §4.2 |
| `docs/spec/codegen.md` | new | C.W7 | 1 day | new (post-Phase-4 codegen IR contract) |
| `docs/howto/migration/2026-restart.md` | new | C.W4 | 1 day | Pass C facility §4.3 |
| `docs/process/tranches/{A..J}/{A..J}.md` | new (this synthesis) | this commit | (in-flight) | master plan |

### §8.3 — Sequencing per tranche

The docs work folds into tranche A's W6 (README + GESTALT rewrite + commit-prelude #5) and tranche C's W3-W4-W7 (per-doc rewrite + new spec docs). A few late-arriving items (cross-backend parity doc) land in tranche J.

| Tranche | Wave | Doc work |
|---|---|---|
| A | A.W3 (mechanical) | docs tree relocate per §8.2.4 |
| A | A.W3 (mechanical) | legacy tranches archive per §8.2.4 |
| A | A.W6 | README + GESTALT rewrite |
| C | C.W3 | `docs/lang/*` rewrites |
| C | C.W4 | `docs/perf/*` full rewrite + `docs/howto/*` polish |
| C | C.W7 | `docs/spec/SPEC.md` + `architecture.md` + `codegen.md` lands |
| C | C.W4 | `docs/howto/migration/2026-restart.md` lands |
| J | J.W4 | cross-backend parity doc + final SOTA validation reports |

The legacy tranche tree archive (Pass C's recommendation: relocate every letter from `docs/tranches/{Y..BD}/` to `docs/tranches/archive/legacy-Y-BD/`) lands in **A.W3** as a `git mv` operation. The synthesizer **ratifies Pass C's recommendation outright** — no fresh adjudication required.

---

## §9 — Migration Timeline

### §9.1 — Phase summary

| Phase | Scope | Calendar | Notes |
|---|---|---|---|
| Pre-A operational sequence | Commit-chain disposition execution + Lock 12 ceremony + 8 prelude commits | 1 working day (~3-4 hours) | Per §7.2 |
| Tranche A | Workspace genesis (skeleton + Lock 14 retirement + path-deps + narrative scrub) | 3-4 weeks | 6 waves; closes with `cargo check --workspace` green on skeletal contents |
| Tranche B | bbnf-error + bbnf-pipeline foundation | 2-3 weeks | 4 waves |
| Tranche C | Parse + IR foundation | 4-5 weeks | 7 waves |
| Tranche D | Codegen IR contract | 3-4 weeks | 5 waves |
| Tranche E | Per-grammar declaration crates + runtime template | 5-6 weeks | 8 waves; the substrate centerpiece |
| Tranche F | Optimiser pipeline | 3-4 weeks | 6 waves |
| Tranche G | Slice-borrow API + pointer macro + visitor surface | 3 weeks | 5 waves |
| Tranche H | TS + WASM emitters | 3 weeks | 4 waves |
| Tranche I | Sister-crate publication | 2 weeks | 3 waves |
| Tranche J | Cross-backend parity + close | 3-4 weeks | 5 waves |
| Post-J close | Hardening + 1.0 release prep | 1-2 weeks | tag + release ceremony |

**Aggregate calendar**: 33-43 weeks (~7-10 months).

### §9.2 — Critical-path identification

The critical path runs **A → B → C → D → E**. Tranches F, G, H land in compatible parallel after E closes (F and G can run side-by-side because their write scopes do not overlap; H depends on D's Emitter trait reshape but otherwise lands in parallel with G). Tranche I depends on F (sister-crate API freeze comes from F's optimiser pipeline shape). Tranche J synchronises all upstream tranches before close.

```
A ──► B ──► C ──► D ──► E ──┬──► F ──► I ──┐
                            ├──► G ────────┼──► J ──► close
                            └──► H ────────┘
```

### §9.3 — Risk-driven schedule slack

Each tranche carries a hardening pass (per `docs/restart/HARDENING.md`, ~2 days). A triumvirate redress (per `feedback_triumvirate-discipline`) for any tranche adds ~3 days. The schedule above does not account for redress; an honest projection adds ~10-15% to the calendar (~1 month) for redress + hardening cycles.

A pessimistic upper bound: 12 months (the suite mandate ceiling).
An optimistic lower bound: 6 months (no redress, no hardening overruns).
Realistic central estimate: 8-9 months.

---

## §10 — Archive Disposition

### §10.1 — Legacy tranche tree

**Ratified recommendation (Pass C §3.4 + §8.2.5)**: move all letter-tranches from `docs/tranches/{Y..BD}/` (also W, X, AA-AZ-IV, B0-B7, BA-BD) to `docs/tranches/archive/legacy-Y-BD/`.

The synthesizer **ratifies Pass C's recommendation outright**. Execution lands in tranche A.W3 as a `git mv` operation. The restart's fresh tranche set occupies `docs/tranches/A/`, `B/`, ..., `J/` (relocated to `docs/process/tranches/A/` etc. if Pass C's docs restructure is honoured fully; otherwise stays at `docs/tranches/A/`).

The synthesizer's tranche stubs (created by this synthesis) land at `docs/tranches/{A..J}/{A..J}.md` per the SYNTHESIZER.md contract. Whether they relocate to `docs/process/tranches/{A..J}/{A..J}.md` is a Pass C secondary decision; the synthesizer does NOT execute the docs restructure, only ratifies it. The relocation lands in tranche A.W3.

### §10.2 — Workspace `archive/` directory

**Ratified recommendation (Pass C §3.2 + §8.1.1)**: `crates/ser/` and `crates/gorgeous/` move to `archive/ser/` and `archive/gorgeous/` per Lock 12. The directory `archive/` is created on first ceremony.

The synthesizer **ratifies Pass C's recommendation outright**. The Lock 12 ceremony is the BLOCKING precondition for tranche A. Disposition: keep-as-historical archive (Pass C Agent 4 §2.3); future maintainers may delete at 1.0 cut.

### §10.3 — Pre-restart-{BA,BB,BC} tranches

Already archived per commit `9dde66ab chore(tranches): archive pre-restart BA/BB/BC; clean slate for re-draft`. No action required.

### §10.4 — Sibling repos disposition

The five sibling repos (`parse-that`, `csc411`, `bbnf-buddy`, `gorgeous-external`, `pprint-external`) remain external. Two are workspace path-dep'd: `parse-that` (→ `crates/parse-that/` as submodule + workspace-member); `bbnf-regex` (→ `crates/bbnf-regex/` as submodule + workspace-member).

`csc411` carries the generalised CSP solver per memory item `csp-solver-crate`; the `csp-solver` workspace crate is the consumer-side patch. No relocation needed — `csc411` is the canonical home; `crates/csp-solver/` path-deps it via submodule.

`bbnf-buddy` (procedural SVG mascot per memory item `bbnf-buddy`) is unrelated to the parser fleet; no restart-level coupling.

`gorgeous-external` and `pprint-external` are the external archives of the in-workspace `gorgeous/` and `pprint/` (per Lock 12 archive ceremony, both relocate to `archive/`). The external sibling repos are the canonical homes; the workspace archive is the local snapshot.

---

## §11 — 14-Lock Honoured Table at Greenfield Completion

Per Lock 14's verification mandate, every lock's honour-state at greenfield steady-state:

| Lock | Tranche owning honour | Wave honouring | Verification |
|---|---|---|---|
| 1 — Tape + columnar dead | A, C | A.W4 (narrative scrub), C.W7 (typed-IR consolidation) | `rg -nE 'TapeRec\|TapeCursor\|payload_idx\|OpenFrame\|FusedBuilder' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core,path,path-ts,bbnf-codegen,bbnf-codegen-ir,bbnf-runtime,bbnf-runtime-template}/src/` returns 0 (excluding `archive/`) |
| 2 — Layout lowering canon | C | C.W2 (Lock 2 substantive fold) | `rg -nE 'TypeDesc\|StructLayout\|TypeMap\|type_projection\|type_collapsing\|schema synthesis\|LayoutDesc' crates/bbnf-ir/src/ crates/bbnf-passes/src/` returns 0 outside doc-archived contexts |
| 3 — Cursor + byte-skip unified | C, E | C.W6 (path-core consolidation), E.W4 (cursor consult on EMPTY_PATH binding) | `rg -nE 'fn parse_with_cursor.*fn parse_eager' crates/` returns 0 (no dual implementations); empty-path elision invariant codified in `docs/howto/cookbook/path-macro.md` |
| 4 — Per-domain orthogonal optimisation | F | F.W2 (output-piping verification) | dependency-DAG verification: `cargo tree -p bbnf-passes` shows egraph + csp-solver as orthogonal sub-deps; no fused-hypergraph crate exists |
| 5 — IR + per-backend lower | C, D | C.W4 (bbnf-codegen-ir typed IR lands), D.W3 (per-backend lower split) | IR contract document at `docs/spec/codegen.md`; `rg 'fn emit_for_rust\|fn emit_for_ts' crates/bbnf-codegen/src/rust/ crates/bbnf-codegen/src/ts/` produces matching trait method counts |
| 6 — xtask emits committed source | A, E | A.W2 (xtask Cargo.toml additions), E.W6 (runtime template emit gate) | `cargo xtask regen --check` produces zero source diff post-regen; generated artefacts greppable on disk per per-grammar declaration crate `src/generated.rs` |
| 7 — `crates/path/` consolidated | A, C | A.W4 (path-core skeleton lands), C.W6 (full path consolidation) | `find crates/{path,path-core,path-ts}/src/ -name "*.rs" \| xargs wc -l \| sort -n` shows no file >500 LOC; `rg 'crates/bbnf-path' crates/` returns 0 (renamed) |
| 8 — Surpass SOTA | F, G, H, J | All perf-anchored gates | competitor numbers cited per gate: sonic-rs M1 Pro twitter ≤ 436 µs; lightning-css Bootstrap ≤ 4.16 ms; simdjson On-Demand 7 GB/s; per benchmark commit |
| 9 — Slice-borrow primary | G | G.W2 (parse / parse_in / parse_owned API) | `rg 'pub fn parse_in\|pub fn parse_owned' crates/bbnf/src/lib.rs` matches; default `parse(input)` emits `&'i str` |
| 10 — Pratt + SIMD auto-detected | F | F.W3 (auto-detection lands) | `rg '@pratt\|@simd' grammar/` returns 0; `bbnf-passes::recognizers::operator_chain` + `pattern_alphabet` consume cost-model output |
| 11 — Path-deps for sister crates | A, I | A.W2 (path-deps register), I.W2 (publication of egraph + csp-solver) | `cargo tree -p bbnf-passes` shows path-dep markers; egraph + csp-solver published on crates.io with stable 1.0 versions |
| 12 — ser + gorgeous archive ceremony | A | A.W0 (precondition; lands first commit) | `find archive/{ser,gorgeous}/Cargo.toml` exists; `find crates/{ser,gorgeous}/Cargo.toml` returns nothing; `rg 'crates/ser\|crates/gorgeous\|bbnf-ser\|bbnf-gorgeous' crates/` returns 0 |
| 13 — No god directories | A, C | A.W4 (initial cleanup), C.W6 (final SPLITs) | `find crates/ -name "*.rs" -not -path "*generated*" \| xargs wc -l \| awk '$1 > 500'` returns 0; `find crates/ -mindepth 2 -maxdepth 2 -type d \| xargs -I {} bash -c 'cnt=$(find "{}" -mindepth 1 -maxdepth 1 \| wc -l); echo "$cnt {}"' \| awk '$1 > 10'` returns 0 |
| 14 — Full grammar generalisation | A, E | A.W3 (Lock 14 retirement; 7 sites), E.W6 (per-grammar declaration crates land) | `rg -nE 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core,path,path-ts,bbnf-codegen,bbnf-codegen-ir,bbnf-runtime,bbnf-runtime-template,bbnf-host,bbnf-error,bbnf-pipeline,bbnf-vm,bbnf-grammar}/src/` returns 0; `find crates/bbnf-runtime/src -mindepth 1 -maxdepth 1 -type d -name '<grammar>'` returns 0 (no per-grammar dirs in generic crate); future-grammar onboarding test: adding a new grammar to `[workspace.metadata.bbnf.grammars]` + creating its declaration crate + running `cargo xtask regen` produces a working parser with ZERO code change in any other crate |

The 14 locks honour at completion. Lock 8 honours per-tranche (every tranche with a perf gate cites a SOTA number); the others honour at the named-tranche close.

---

## §12 — Generated-LOC Trajectory

### §12.1 — Pre-restart baseline

Per CENSUS §10.5: 168,750 LOC across `crates/core/src/grammar/generated/` (9 grammars).

| Grammar | LOC |
|---|---:|
| bbnf | 21,503 |
| bnf | 3,290 |
| csv | 1,693 |
| css_l4 | 107,138 |
| css_pretty | 9,021 |
| ebnf | 7,646 |
| google_sheets | 14,088 |
| json | 3,500 |
| math | 871 |
| **Total** | **168,750** |

### §12.2 — Tranche-by-tranche projection

| Tranche | Entry LOC (generated) | Net delta | Exit LOC | Notes |
|---|---:|---:|---:|---|
| A | 168,750 | 0 | 168,750 | Workspace restructure shouldn't change generated content; per-grammar declaration crate skeletons receive `generated.rs` as moved-not-modified |
| B | 168,750 | 0 | 168,750 | Naming canon (Lock 2) + bbnf-error don't touch generated |
| C | 168,750 | -2,000 | 166,750 | god-module SPLITs in IR may reduce some generated wrapping; Layout-vocab fold may shrink some redundant emit |
| D | 166,750 | -8,000 | 158,750 | Codegen IR contract + Emitter trait reshape; struct_direct sub-modules retire; substrate.rs retires; some per-shape code reduces |
| E | 158,750 | -13,000 | 145,750 | Per-grammar runtime template-emit retires hand-written runtime files (~13K LOC across 5 trivial cohort grammars); direct-projection emit retires OpenFrame machinery (~5K LOC of stack-build code per CSS L4 + ~2K elsewhere) |
| F | 145,750 | -2,500 | 143,250 | Optimiser pipeline (Pratt + SIMD auto-detection) may consolidate some emit; cost-model output-piping retires some heuristic code |
| G | 143,250 | +500 | 143,750 | Slice-borrow API adds parse_in/parse_owned variants per grammar (~50 LOC × 9 = ~500) |
| H | 143,750 | +30,000 | 173,750 | TS + WASM emitters land; per grammar emits TS source (~3K-50K LOC per grammar depending on grammar size; CSS L4 dominates) |
| I | 173,750 | 0 | 173,750 | Sister-crate publication doesn't touch generated |
| J | 173,750 | 0 | 173,750 | Parity matrix gates lock the trajectory; no further generated changes |

**End-state**: ~173,750 LOC generated, distributed:
- ~143,750 LOC Rust (~15% reduction from baseline; the runtime template-emit + direct-projection emit + Lock 14 retirement)
- ~30,000 LOC TS (post-tranche-H, projecting CSS L4 dominance)

The 15% Rust-side reduction is the substrate-identity win — the OpenFrame machinery + per-grammar runtime hand-written files retire; what stands is template-emitted typed Rust.

### §12.3 — Per-tranche generator regression budget (per `feedback_generated-size-budget`)

Each tranche that touches a code generator declares a per-grammar output line-count window. Overflow blocks the wave until the regression is traced and either fixed or deliberately accepted.

| Tranche | Generator touched | Per-grammar window | Total window |
|---|---|---|---|
| D | bbnf-codegen-ir + Emitter trait | ±10% per grammar | ±15K LOC |
| E | bbnf-runtime-template | ±15% per grammar | ±25K LOC |
| F | Pratt + SIMD auto-detection | ±5% per grammar | ±8K LOC |
| H | bbnf-codegen TS + WASM | (new emission; no prior baseline) | bounded by per-grammar declared budget |

The per-grammar budget table is honoured per wave's `Verification Artefacts` section.

---

## §13 — Risks + Mitigations Across Tranches

The consolidated risk table — every cross-pass risk surfaced by the three syntheses, with the tranche owning mitigation.

| # | Risk | Tranche owning mitigation | Mitigation |
|---|---|---|---|
| R1 | Lock 12 archive ceremony silently breaks workspace `members` consumers | A | A.W0 explicit pre-flight: `rg 'bbnf-ser\|bbnf-gorgeous' crates/` confirms zero internal references before relocation; `cargo check --workspace` post-ceremony confirms green |
| R2 | Commit-chain disposition (Option 3) breaks user's GitHub fork integration | A | A.W0 verifies the prior chain is reachable via tag; pre-restart-2026-05-03 tag preserved; user retains rollback capability via `git reset --hard pre-restart-2026-05-03` |
| R3 | `crates/core/` fracture leaves orphan re-exports breaking downstream consumers | A | A.W4 sequenced after substantive content moves; `cargo doc` smoke confirms public-API surface re-exports through `crates/bbnf/src/lib.rs` aggregator |
| R4 | Lock 14 retirement (7 sites) misses one site, allowing grammar-named code to persist in generic crate | A | A.W3 final gate runs the verification command per-crate; CI gate post-A.close fires the same command on every PR |
| R5 | Lock 2 rename (`TypeDesc` → `Layout`) breaks downstream compile due to missed reference | C | C.W2 staged rename: directory rename first (mechanical), then symbol rename (rustc errors guide), then verification scan |
| R6 | god-module SPLIT mid-tranche introduces compilation breakage | C | C.W6 per-SPLIT: pre-split `cargo check`, split file at module-boundary commit, post-split `cargo check`; rejection triggers triumvirate |
| R7 | bbnf-runtime-template emits incorrect typed Rust for one grammar, breaking it | E | E.W2 smoke gate per grammar: emitted runtime parses test fixture per `bbnf-test-fixtures::fixture::<grammar>`; per-grammar parity matrix |
| R8 | Direct-projection emit retires OpenFrame but introduces correctness regression | E | E.W4 per-grammar regression suite vs prior tape-era output; nested-depth fixtures (citm_catalog), array-heavy fixtures (canada), per-grammar `tests/parse.rs` |
| R9 | Per-grammar declaration crate Cargo.toml missing `[workspace.metadata.bbnf]` reference | E | E.W3 `cargo xtask validate-metadata --check` CI gate; rejects PRs missing metadata |
| R10 | Optimiser pipeline (egraph + csp-solver) output-piping fuses by accident | F | F.W2 dependency-DAG audit: `cargo tree -p bbnf-passes` confirms orthogonal sub-deps; per-domain test isolation |
| R11 | Pratt auto-detection misfires on a grammar shape, breaking parse | F | F.W4 per-grammar Pratt-eligibility inspection: emit Pratt-eligible rules to log; user confirms per grammar; `pratt_eligibility = "auto" \| "force" \| "skip"` per workspace metadata |
| R12 | SIMD auto-detection misfires on small leaves, blowing dispatch overhead | F | F.W4 cost-model gate: SIMD only emits when expected dispatch overhead < SIMD payoff; per-grammar `simd_eligibility` knob escapes if misclassification |
| R13 | Slice-borrow + arena + owned (Lock 9) surface drift between backends | G, H | G.W2 unified API surface; H.W3 cross-backend equivalence test (one input, three backends, identical typed-tree) |
| R14 | TS + WASM emitters drift from Rust emit due to backend-specific divergence | H | H.W2 cross-backend parity matrix; per-shape walking pattern enforced via Emitter trait constraint |
| R15 | Sister-crate publication breaks workspace path-dep consumers when API freezes | I | I.W1 API freeze audit: cargo-semver-checks + manual review; downstream consumers (per-grammar declaration crates) pinned to specific versions |
| R16 | parse-that disposition (submodule vs workspace-member) introduces git-submodule complexity | A | A.W2 ratified disposition: submodule-as-workspace-member (per `docs/precepts/CONSUMING.md` precedent); operational sequence documented |
| R17 | bbnf-language-server consolidation breaks editor extension | A | A.W1 prelude commit verifies the LSP binary builds + extension loads; post-A.close per-tranche LSP smoke |
| R18 | docs/ tree restructure breaks cross-references | A, C | A.W3 mechanical relocate + grep-based cross-reference fix; C.W3 substantive rewrite cycles |
| R19 | Hardening cycle returns *requires re-draft* on master plan | (orchestrator) | Triumvirate per `docs/restart/HARDENING.md`; redress lands; second hardening fires |
| R20 | Tranche execution agent (per-tranche) introduces scope-pivot mid-tranche | (orchestrator) | Per `tranche/SPEC.md` §Scope Reveal: open `{LETTER}-II.md` if absorption ceiling exceeded; never absorb silently |
| R21 | Generated-LOC budget regression in tranche D or E | D, E | Per-tranche `generated-size-budget` table; overflow blocks wave until regression traced |
| R22 | Build/iter time regression mid-tranche (cargo check >60s on incremental) | (orchestrator) | Per `feedback_build-infra-first`: dev iteration time is a Wave 0 concern in any tranche where it's a bottleneck |
| R23 | Triumvirate auto-trigger (JSONL quiet >15 min) escalates wrong cohort | (orchestrator) | Per `feedback_triumvirate-auto-trigger`: research + plan + redress dispatched in proper sequence; no user prompt required |
| R24 | Cross-tranche debt accumulates (deferred items) | All tranches | Per tranche `FINAL.md`: every deferred item names the receiving tranche + the gate; J.close fires final reconciliation |

---

## §14 — Voice Locks

Per `docs/precepts/instructions/STYLE.md` and `docs/restart/README.md` §Voice. The master plan + tranche stubs honour:

- **Calibrated, archaic-permissive register**. "Hereupon", "thereof", "appurtenant", "begotten", "redress", "transposition" deployed where befitting; never forced.
- **No corporate hedging**. Direct assertions. No "might want to consider"; no "could be tightened"; no "we should think about". Either the plan is right on a dimension or it is wrong.
- **No metalanguage**. Cite path:line and proceed. No "earlier this tranche", no "as discussed". The master plan reads as standalone prose.
- **No epanorthosis**. No "not just X, but Y"; no "rather, Y". State Y directly.
- **Sparing unspaced em-dashes**. One per paragraph max. Spaced em-dashes are an AI-writing sign and are forbidden.
- **No banned words**. Per STYLE.md: no `delve`, `tapestry`, `testament`, `underscore`, `pivotal`, `robust`, `leverage`, `navigate`, `unleash`, `foster`, `align with`, `ever-evolving`, `bustling`, `showcase`, `landscape`, `intricate`, `in conclusion`, `in the realm of`, `it's worth noting`. ("Leverage" allowed only in mechanical sense.)
- **Mild poetic lilt at ~5%**. Domain verbiage from grammar / compiler theory / Romantic-era musical idiom / Memphis Group / botany / Medieval poetics deployed where the material invites; never decoratively.
- **Path:line citations on every concrete claim**. Every architectural assertion grounds in a file path or audit reference.
- **Per-X tables for every "all-X" claim**. The verdict ledger (§2), the lock table (§11), the risk table (§13) — every "all-locks-honoured" or "all-risks-mitigated" claim materialises as a per-row table.

The voice locks bind every tranche stub. The per-tranche execution agents inherit them.

---

## §15 — Closing Posture

Hereupon the master plan is ratified-pending-hardening. The synthesizer's commitment, in two paragraphs:

The bbnf-lang restart is, at its substrate, a transposition — the corpus reshapes around the locks rather than absorbing them as patches. The 23-crate workspace at greenfield steady-state is the architectural instance of the locks; the 10-tranche sequence is the operational shape of the substrate's coming-into-being; the docs re-do is the parallel discourse the project speaks about itself. Pass A's parser-front audit, Pass B's codegen + runtime + optimisers audit, and Pass C's periphery + tooling + docs + commit-chain audit converge here without contradiction — the convergent pivot (Lock 1 + Lock 13 + Lock 14 retiring as one architectural movement, per Pass B §6) drives tranche E; the commit-chain disposition (Option 3, per Pass C §7.5) drives the pre-A operational sequence; the workspace shape (per Pass A §8.1, reconciled here in §3) drives every tranche thereafter. None of the three passes proposes a substrate the master plan cannot accommodate; none of the locks demands a sequencing the master plan cannot honour.

The next step is hardening. The user invokes `docs/restart/HARDENING.md` against this master plan; if hardening returns *ready to execute*, tranche A opens — the Lock 12 ceremony fires, the commit-chain branch reset commences, the eight prelude commits land, and the per-tranche execution agents draft full waves under each `docs/tranches/{X}/{X}.md` stub. If hardening returns *requires re-draft*, the synthesizer absorbs the redress and re-issues. If hardening returns *requires amendment*, the affected sections re-draft surgically. The plan from here is the orchestrator's; the substrate is settled; the greenfield mandate carries through.

---

*Master plan synthesised 2026-05-03. Inputs: PASS-A-2026-05-03.md (829 lines), PASS-B-2026-05-03.md (548 lines), PASS-C-2026-05-03.md (486 lines), 18 per-agent reports, 4 anchor corpora. Outputs: this master plan + 10 tranche stubs at `docs/tranches/{A..J}/{A..J}.md`.*
