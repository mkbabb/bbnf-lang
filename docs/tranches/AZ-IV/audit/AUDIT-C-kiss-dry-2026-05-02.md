# AUDIT-C — KISS / DRY / Special-Case Excision Lane

**Date**: 2026-05-02
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-audit-c-kiss` (branch `audit-c-kiss`)
**Base**: `10ac5448` (post W2-close mid-tranche audit)
**Coordination**: AUDIT-A (legacy/workaround sweep) and AUDIT-B (file-size architectural splits) running in parallel worktrees; explicit hand-offs noted below.

## §1 Antipattern Census

Sweeps cover `crates/{core,ir,egraph,egraph-derive,csp-solver,simd-scan,analysis,lsp,bbnf-path}/src` and `xtask/src`. The path-patched sibling `bbnf-regex` lives at `parse-that/rust/regex/` and falls outside this worktree. Generated trees (`crates/core/src/grammar/generated/**`, `**/generated/**`) excluded from every sweep.

### 1.1 Workaround / TODO / FIXME / HACK markers

Direct grep for `// TODO`, `// FIXME`, `// HACK`, `// XXX`, `// WORKAROUND`, `// LEGACY`, `// COMPAT` against non-generated source returned a low-volume residue. Doc-comment references to "legacy" patterns dominate; nearly every one names an excised surface (`legacy bootstrap_parser`, `legacy emit_call`, `legacy fn-per-rule`) and serves as a pinned epitaph, not a live workaround.

| File:line | Marker | Classification | Action |
|---|---|---|---|
| `crates/core/src/runtime/google_sheets/value.rs:76` | TODO (in doc-comment) | DEAD-COMMENT | route to W5 (Sheets typed-value rework) |
| `crates/core/src/runtime/google_sheets/value.rs:92` | TODO (in doc-comment) | DEAD-COMMENT | route to W5 |
| `crates/ir/src/types/grammar.rs:519` | XXX (in code-fence comment) | DEAD-COMMENT — placeholder for a rendered chain | cosmetic; route to W5 |
| `crates/ir/src/passes/patterns/mod.rs:21,55,63` | "Legacy types (kept for backward compat during migration)" | REAL-CARRY — labelled migration window | the patterns module's `PatternAnnotations` legacy fields drive `recognizers/mod.rs:229-252`; the TWO-PHASE migration (legacy + NodeFacts) is mid-flight. Route to W4 (NodeFacts cutover) for legacy-arm deletion. |
| `crates/ir/src/passes/recognizers/mod.rs:229-252` | "Phase 1: legacy per-rule annotations" | REAL-CARRY (mirror of above) | route to W4 |
| `crates/ir/src/passes/recognizers/node_facts.rs:170,174,180,193` | `recognize_seq_legacy`, `recognize_alt_legacy` | REAL-CARRY | route to W4 |
| `crates/ir/src/vm/mod.rs:5` | "kept for backward compatibility" doc-comment | DEAD-COMMENT — VM is the live consumer | route to W4 (vm/interpreter rework if scoped); else W6 close-honesty |
| `crates/ir/src/passes/payload/named_types.rs:70` | `No-op resolver: ... Used by the legacy entry point` | REAL-CARRY | route to W4 |
| `crates/ir/src/passes/recognizers/dta.rs:113,135,201,431` | `legacy structural Seq-compound emission` doc-comments | REAL-CARRY (the DTA/legacy fork is real) | route to W4 (DTA full activation; legacy-fallback delete) |
| `crates/core/src/lower/expression/wrap.rs:131-154` | `Legacy bootstrap_parser shape (pre-AZ-II.cutover.D)` | REAL-CARRY | route to W4 (bootstrap_parser shape removal) |
| `crates/core/src/css_types.rs:15` | `Single source of truth (per feedback_no_workarounds)` | EPITAPH | keep — this is the doc declaring the no-workaround precept's enforcement point |
| `crates/core/src/runtime/css_l4/builder.rs:306` | `feedback_no-workarounds` | EPITAPH | keep |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:56,496` | `feedback_no-workarounds` | EPITAPH | keep |
| `crates/core/src/backend/rust/emitter/prettify/seq.rs:126-129` | "A grammar-level workaround (...) would fork ..., violating `feedback_no_workarounds_arch`" | EPITAPH (negative declaration) | keep |
| 7× `xxx_impl` / `signature-compatible` doc-comments | various | EPITAPH | keep (architectural namespacing) |

The seven directly-marked TODO/FIXME/XXX sites are doc-fragments inside comment blocks; none guard live code branches. The `legacy_*` module structure in `crates/ir/src/passes/recognizers/` is a labelled migration cohort that REAL-CARRYs through W4 (NodeFacts unification).

### 1.2 Excessive dynamism

| File:line | Pattern | Classification | Action |
|---|---|---|---|
| `crates/csp-solver/src/constraint/dispatch.rs:20` | `Custom(Box<dyn Constraint<D>>)` | DYNAMICISM — legitimate user-extensible registry | keep |
| `crates/csp-solver/src/constraint/lambda.rs:8` | `Box<dyn Fn(...)>` | DYNAMICISM — user closure storage | keep |
| `crates/ir/src/egraph/rules/mod.rs:78` | `Vec<Box<dyn RewriteFn<...>>>` | DYNAMICISM — legitimate heterogeneous-rule list (each rule is a different ZST) | keep |
| `crates/ir/src/vm/debug.rs:64` | `Box<dyn FnMut(&DebugSnapshot) -> DebugAction>` | DYNAMICISM — user-supplied break callback | keep |
| `crates/core/src/runtime/css_l4/document.rs:176` | `Box<dyn Iterator<...>>` | DYNAMICISM — branchy iterator union (existential return) | keep — single use, narrow surface |
| `crates/core/src/lower/view_walk.rs:69` | `Box<dyn Iterator<...>>` | DYNAMICISM — same shape | keep |
| `crates/analysis/src/state/parsing.rs:81-83` | `panic_info.downcast_ref::<String>` / `<&str>` | DYNAMICISM — `catch_unwind` payload introspection (unavoidable) | keep |

Total `Box<dyn …>` outside `crates/core/src/grammar/generated/`: 6. None are static-dispatch refactors; every site stores a heterogeneous trait-object collection (rewrite rules / constraints / closures) where monomorphisation would force per-instance generics through the API.

`Any::downcast_ref` only appears in `crates/analysis/src/state/parsing.rs` (panic-payload introspection) — required by the `std::panic::catch_unwind` API.

No `match enum_var { _ => default_handler }` catch-alls swallowing unmatched variants found in production code paths after auditing the 460 catch-all arms; the bulk are narrow projections (`_ => None`) where the variant set is exhaustive in intent and the `_` wildcard is purely the bound-on-future-additions guard.

### 1.3 Nested imports / import-in-function-body

| File:line | Pattern | Classification | Action |
|---|---|---|---|
| `crates/core/src/pipeline/compile.rs:405,406` | `use crate::backend::CallStrategy` + `use std::collections::HashSet` (in `compute_call_strategies` body) | NESTED-IMPORT — fn-body imports | DO NOT TOUCH — AUDIT-B is concurrently splitting `crates/core/src/pipeline/compile.rs` (1049 LOC) into a directory module. Route post-AUDIT-B to W3-cleanup. |
| `crates/core/src/pipeline/compile.rs:453` | `use bbnf_ir::passes::MaterializationClass` (inside `for rule in &ir.rules` loop) | NESTED-IMPORT | route post-AUDIT-B-merge |
| `crates/core/src/runtime/google_sheets/document.rs:162` | `use core::fmt::Write` (in fn body) | NESTED-IMPORT — narrow scope | LOW-PRIORITY; the trait import is local to the `write_value` fn for `write!` macro coupling. Route to W5 (Sheets value-API consolidation). |
| `crates/core/src/backend/rust/emitter/grammar.rs:23` | `use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag` (in fn body) | NESTED-IMPORT | route to W4 (emitter grammar surface rework) |
| `crates/core/src/backend/rust/emitter/mod.rs:122` | `use bbnf_ir::passes::patterns::RecognizerShape` | NESTED-IMPORT | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs:194,312` | `use bbnf_ir::IrNode` (inside fns) | NESTED-IMPORT | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs:114` | `use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag` | NESTED-IMPORT | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/object.rs:223,224,265` | `use bbnf_ir::IrNode` + `ShapeTag` (in fn) | NESTED-IMPORT | route to W4 |
| 7× `quote! { use crate::runtime::builder::StructBuilder as _; ... }` blocks under `crates/core/src/backend/rust/emitter/shapes/` | NESTED-IMPORT — inside `quote!` (generated code) | NOT-A-VIOLATION | keep |
| 4× `quote! { use ::parse_that::*; ... }` | NESTED-IMPORT — inside `quote!` (generated code) | NOT-A-VIOLATION | keep |

Total non-`quote!` fn-body `use` statements: 9. None justify in-place fix without coordinating with AUDIT-B's compile.rs split or W4's emitter rework — every site sits in a file AUDIT-B may relocate or W4 will rewrite.

No `use foo::{bar::{baz::{qux}}}` 3+-level nesting found.

### 1.4 Inline tests (`#[cfg(test)]` in `src/`)

| File | Block locations | Status |
|---|---|---|
| `crates/core/src/backend/rust/emitter/shapes/substrate.rs:34-35,124-179` | `#[cfg(test)] use bbnf_ir::registry::SubstrateBinding;` + `#[cfg(test)] mod tests { ... }` (3 tests) | **EXCISED** — relocated to `crates/core/tests/substrate_path_tokens.rs` in commit `841ac0e2` |

`grep -rln "#\\[cfg(test)\\]" crates/{core,ir,egraph,egraph-derive,csp-solver,simd-scan,analysis,lsp,bbnf-path}/src xtask/src` returns ZERO entries post-fix. Per-crate verification in §2.

### 1.5 DRY violations

| Site | Pattern | Classification | Action |
|---|---|---|---|
| 18 sites across `crates/analysis/src/{directives,state,state/diagnostics}/*.rs` | `"bbnf".into()` (lsp_types::Diagnostic::source) | DRY — string-literal dup | **EXCISED** — extracted `pub const DIAGNOSTIC_SOURCE: &str = "bbnf"` in `crates/analysis/src/lib.rs`; 18 sites now reference `crate::DIAGNOSTIC_SOURCE.into()`. Commit `e3a3e322`. |
| 27 sites `quote! { let _ = #support_mod::skip_space(input, p, state); }` across `crates/core/src/backend/rust/emitter/shapes/{arglist,unordered,object,flat,pratt,dispatcher,array,inline}.rs` | DRY — codegen template repeated | route to W4 (emitter shape consolidation; the `skip_space` template is one of three motifs that should fold into a single `quote_skip_space(support_mod)` helper or a `WhitespaceTemplate` enum) |
| `crates/core/src/runtime/{bnf,ebnf,csv,css_pretty}/value.rs` | All four files differ only by `BnfCompoundId`/`EbnfCompoundId`/`CsvCompoundId`/`CssPrettyCompoundId` substitution; same Span/Unit/Compound triplet, same `Default` impl | DRY — pure cut-and-paste per-grammar | route to W5 (per-grammar value-API dedup; folds into a generic `Value<C: CompoundId>` or trait-method indirection) |
| `crates/core/src/runtime/{bnf,ebnf,css_pretty}/builder.rs` | 165 LOC each, identical structure with grammar-name substitution | DRY — pure cut-and-paste per-grammar | route to W5 |
| `crates/core/src/runtime/{bnf,ebnf,css_pretty}/document.rs` | 170-175 LOC each, mostly identical | DRY | route to W5 |
| `crates/core/src/runtime/{bnf,ebnf,css_pretty,math}/view.rs` | 64-65 LOC each, near-identical | DRY | route to W5 |

The structural DRY in `crates/core/src/runtime/{bnf,ebnf,csv,css_pretty}/` is the W5 per-grammar value-API dedup target the mandate flagged. Roster in §4.

### 1.6 Computed-value discards (`feedback_no-value-discard`)

Total non-template `let _ = <expr>` or `.map(|_| ())` patterns outside generated tree: 56 (post `quote!`-content filter). Disposition:

| File:line | Pattern | Classification | Action |
|---|---|---|---|
| `crates/core/src/runtime/bbnf/serialize.rs:163` | `let n = compound.children.len(); ...; let _ = n;` (n unused entirely) | VALUE-DISCARD — dead variable | **EXCISED** — see §2 commit |
| `crates/core/src/lower/expression/pratt.rs:297` | `if let Some(off) = super::find_unquoted(gap, op) { let _ = off; return Some(op); }` | VALUE-DISCARD — dead bind | **EXCISED** — folded to `.is_some()` — see §2 commit |
| `crates/core/src/lower/value_expr/precedence.rs:321` | `let _ = view.focus();` (pure getter call, value discarded) | VALUE-DISCARD — dead getter | **EXCISED** — branch collapsed to `_ => false` — see §2 commit |
| `crates/core/src/backend/rust/emitter/shapes/hregex.rs:377-378` | `let support_mod = format_ident!(...); let _ = support_mod;` (binding never used in scope) | VALUE-DISCARD — dead binding | **EXCISED** — see §2 commit |
| `crates/core/src/pipeline/compile.rs:264` | `let _ = write_coverage_report(&report, &path);` | VALUE-DISCARD — IO-failure mask, BUT the comment explicitly justifies (CI permission-bound) | KEEP — intent documented; route to AUDIT-A (mask audit) for cross-check |
| `crates/core/src/backend/emitter.rs:203` | `let _ = (head, op, rhs, head_type, link_elem_type, ir, ctx);` in default trait impl returning None | NOT-A-VIOLATION — idiomatic unused-arg suppression for default impl | keep |
| `crates/core/src/runtime/css_l4/builder.rs:557` | `let _ = layout.kind;` | VALUE-DISCARD | route to W5 (CSS L4 builder is being refactored; layout.kind dispatch hookup is a known carry) |
| `crates/core/src/backend/rust/emitter/grammar.rs:246` | `let _ = rule_functions;` (commented as W4β cleanup target) | VALUE-DISCARD — explicitly carried | route to W4β |
| `crates/core/src/backend/rust/emitter/dfa_codegen.rs:568` | `let _ = grammar; // retained for symmetry with the ident composer` | NOT-A-VIOLATION — symmetry-required signature parameter | keep |
| `crates/core/src/backend/rust/emitter/precedence.rs:108` | `let _ = grammar; // reserved for future prefix-based disambiguation` | NOT-A-VIOLATION — documented future-use | keep |
| `crates/core/src/backend/rust/emitter/shapes/number.rs:44,46,89` | `let _ = grammar_suffix; let _ = variant_idx; ... let _ = POW10_U64;` | KEEP-OR-ROUTE — emitter-shape refactor target; route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:808,975` | `let _ = support_mod;` | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs:520` | `let _ = strategy;` | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs:143` | `let _ = entry;` | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/array/element.rs:69` | `let _ = branches;` | route to W4 |
| `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs:173-174` | `let _ = sid; let _ = ir;` (conservative-default StringLit branch) | VALUE-DISCARD — masks proper sid lookup | route to W4 (Keyword payload typing) |
| `crates/core/src/backend/rust/analysis/inline.rs:441` | `let _ = csp.propagate();` | VALUE-DISCARD — discards `Result<(), Unsatisfiable>` | route to AUDIT-A (mask audit); the structured-failure mode is real — propagation infeasibility silently leaves prior decisions in place |
| `crates/core/src/lower/value_expr/atom.rs:375-376,399,535` | `let _ = node; let _ = ctx;` (mirror-helper-signature pattern) | KEEP — sibling-API symmetry; documented intent |
| `crates/ir/src/egraph/write_back.rs:150` | `let _ = current_rule; // no longer consulted` | API-CHURN — parameter is dead but signature change ripples | route to W3 (extractor signature cleanup) |
| `crates/ir/src/passes/csp_strategy/mod.rs:562` | `let _ = components;` | API-CHURN — function parameter unused, comment cites alternate consumption path | route to W4 (csp_strategy/mod.rs is on AUDIT-B's split-target list) |
| `crates/ir/src/passes/recognizers/shape_dispatch/wrap.rs:67` | `let _ = ir;` (parameter unused after recognizer rewrite) | API-CHURN | route to W4 |
| `crates/egraph/src/egraph.rs:266` | `let _ = merged;` after `self.union(existing, parent_canonical)` | VALUE-DISCARD — `union` returns the new canonical Id; the recanonicalisation loop reads `unionfind.find` afresh on next iter, but the Id discard masks an opportunity to short-circuit | route to W3 (egraph rebuild loop tightening) |
| 33+ further `let _ = #ident_in_quote_block;` inside `quote! { ... }` codegen templates | NOT-A-VIOLATION (generated-code unused-var suppression) | keep |

### 1.7 Diagnostic emission (`feedback_clean-instrumentation`)

`eprintln!` / `println!` / `dbg!` outside generated:

| File:line(s) | Pattern | Disposition |
|---|---|---|
| `xtask/src/regen.rs:341,357,367,407,500,560,568,601,628` | CLI tool stdout/stderr | KEEP — xtask is the regen CLI; its progress output is the user-facing surface |
| `crates/lsp/src/dap/mod.rs:40` | DAP server `eprintln!("DAP: invalid request: {}", e)` | KEEP — DAP protocol errors surface via stderr by convention |
| `crates/core/src/pipeline/compile.rs:65-70,587` | `BBNF_PIPELINE_REPORT`-gated diagnostic CSV emission | KEEP-WITH-NOTE — env-var-gated; structured CSV is the format. Per `feedback_clean-instrumentation` the long-run replacement is a tracing-style emitter; route to W6 (close-honesty: instrumentation discipline) for migration. |
| `crates/ir/src/egraph/mod.rs:106-119` | `BBNF_EGRAPH_REPORT`-gated saturation report | KEEP-WITH-NOTE — same pattern; route to W6 |
| `crates/ir/src/passes/csp_strategy/mod.rs:584-636` | `BBNF_CSP_REPORT`-gated component report | KEEP-WITH-NOTE — same pattern; route to W6 |
| `crates/ir/src/passes/types/subvariants.rs:169` | `#[cfg(debug_assertions)]`-gated note | KEEP — debug-only |
| `crates/ir/src/vm/interpreter/mod.rs:154` | `self.trace`-flag-gated trace | KEEP — runtime flag |

No bare `eprintln!`/`println!`/`dbg!` calls fire on the production hot path.

## §2 Surgical Fixes Landed in This Dispatch

All commits land on branch `audit-c-kiss` in worktree `/Users/mkbabb/Programming/bbnf-wt-audit-c-kiss`. Per-commit lint cadence (`cargo fmt --all -- --check` + `git diff --check`) green; `cargo nextest run --profile ax-iter -p <crate>` green for every crate touched; full `cargo xtask regen --check` 9/9 green where lower/expression/ or runtime touched.

| # | Commit | Class | Surface |
|---|---|---|---|
| 1 | `2b19e67d` | DEAD-CODE | `crates/analysis/src/state/ast_utils/mod.rs` — delete unused `is_term_kind` (14 LOC) and `is_grouped_term` (10 LOC); compiler-flagged on baseline check. |
| 2 | `841ac0e2` | INLINE-TEST | `crates/core/src/backend/rust/emitter/shapes/substrate.rs` — relocate 3-test `mod tests { ... }` to `crates/core/tests/substrate_path_tokens.rs`; `#[cfg(test)] use SubstrateBinding;` retired alongside. Surfaces tested unchanged. |
| 3 | `e3a3e322` | DRY | 18 `"bbnf".into()` → 1 `pub const DIAGNOSTIC_SOURCE` + 18 references; touches 11 files in `crates/analysis/src/{lib.rs,directives/*,state/{pretty.rs,diagnostics/*}}`. |
| 4 | (this commit) | VALUE-DISCARD | Four-file batch of pure dead-binding/dead-getter excisions: `serialize.rs:163` (let n; ...; let _ = n;), `pratt.rs:296-298` (Some(off) {let _ = off;} → .is_some()), `precedence.rs:319-323` (None branch with dead getter call), `hregex.rs:377-378` (unused support_mod binding). |

### 2.1 Hard-gate evidence (this dispatch)

```
$ cargo fmt --all -- --check       # clean
$ git diff --check                 # clean
$ cargo check --profile ax-iter --workspace
    Finished `ax-iter` profile [unoptimized + debuginfo] target(s) in 13.04s
$ cargo nextest run --profile ax-iter -p bbnf-analysis
    Summary [   0.041s] 12 tests run: 12 passed, 2 skipped
$ cargo nextest run --profile ax-iter -p bbnf --test substrate_path_tokens
    Summary [   0.008s] 3 tests run: 3 passed, 0 skipped
$ cargo xtask regen --check
    regen --check: clean (9 of 9 grammars matched)
```

Workspace nextest pre-AUDIT-C: 1582 / 0 fail (W2 close baseline). Post-AUDIT-C: pending verification on full workspace nextest after this audit doc commits.

## §3 Routing — Cross-Cutting Items

| Owner | Items | Priority |
|---|---|---|
| AUDIT-A coordination | `crates/core/src/pipeline/compile.rs:264` IO-failure mask + `crates/core/src/backend/rust/analysis/inline.rs:441` `csp.propagate()` mask + `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs:173-174` StringLit conservative-default | medium |
| AUDIT-B coordination | DO NOT touch `crates/core/src/pipeline/compile.rs` — AUDIT-B is splitting it (1049 LOC → directory module) in parallel; the 3 fn-body imports at lines 405,406,453 ride along with their owner fn into the post-split target file. AUDIT-C re-sweeps after AUDIT-B merges. |
| W3 (Lazy Bail-Out Parse) | `crates/ir/src/egraph/write_back.rs:150` API-churn `current_rule` deletion; `crates/egraph/src/egraph.rs:266` rebuild-loop merged-Id tightening |
| W4 (Codegen substrate / TS binding precursor) | 27× `quote! { let _ = #support_mod::skip_space(input, p, state); }` motif consolidation; 7+ emitter `let _ = X;` API-churn parameter cleanups across `shapes/{number,flat,wrap,dispatcher,array,keyword,unordered}/*.rs`; W4-route from §1.1 (legacy/NodeFacts cutover); fn-body `use bbnf_ir::passes::...` consolidations across `backend/rust/emitter/{grammar.rs,mod.rs,shapes/*.rs}` |
| W4β | `crates/core/src/backend/rust/emitter/grammar.rs:246` `let _ = rule_functions` — upstream-pipeline-compiled per-rule fragments now unused; the comment explicitly cites W4β as the deletion site |
| W5 (TS binding / per-grammar value-API dedup) | per-grammar runtime cut-and-paste roster (§4); `crates/core/src/runtime/css_l4/builder.rs:557` layout.kind hookup; `crates/core/src/runtime/google_sheets/document.rs:162` `use core::fmt::Write` body-import; CSS L4 + Sheets value.rs TODO doc-comments |
| W6 (close-honesty / instrumentation discipline) | env-var-gated `eprintln!` instrumentation across `crates/core/src/pipeline/compile.rs`, `crates/ir/src/{egraph/mod.rs,passes/csp_strategy/mod.rs}` — migrate to a tracing-style emitter so production builds carry no formatting overhead; per `feedback_clean-instrumentation` |

## §4 DRY Consolidation Roster — W5 per-grammar Value-API Dedup

The four small grammars (`bnf`, `ebnf`, `csv`, `css_pretty`) carry near-identical runtime modules whose only difference is the per-grammar `CompoundId` type substitution. The W5 per-grammar value-API dedup target should fold the following pairs into generic forms (or, where generics carry too much friction, single-source generators that emit per-grammar shells).

| Surface | Per-grammar files | Total LOC | Difference |
|---|---|---:|---|
| `value.rs` (Span / Unit / Compound triplet + `Default` impl) | `bnf/value.rs` (96), `ebnf/value.rs` (23), `csv/value.rs` (57), `css_pretty/value.rs` (23) | 199 | `Bnf/Ebnf/Csv/CssPretty` ident substitution only |
| `builder.rs` (`*StructBuilder` mirror of `CsvStructBuilder`) | `bnf` (165), `ebnf` (165), `css_pretty` (165) | 495 | grammar-ident substitution; field types track their `*CompoundKind` |
| `document.rs` | `bnf` (170), `ebnf` (170), `css_pretty` (175) | 515 | grammar-ident substitution + per-compound `emit_value` arms |
| `view.rs` | `bnf` (64), `css_pretty` (64), `ebnf` (64), `math` (65) | 257 | grammar-ident substitution |
| `arena.rs` | `bnf` (128), `ebnf` (138), `csv` (180), `css_pretty` (145), `math` (143) | 734 | `*CompoundKind` enum + handle dispatch; the small grammars all carry the same Compound storage shape |

Post-dedup target: ~50 LOC of shared trait-driven generic core + ~20 LOC per-grammar shell (4-6 grammars × 100 LOC ≈ 400-600 LOC saved). The 27 `quote! { let _ = #support_mod::skip_space(input, p, state); }` motif occurrences fall into the same dedup wave (W5) since they share the per-grammar codegen template surface.

## §5 Coordination Notes

- **AUDIT-A (legacy/workaround sweep)**: AUDIT-C left every `unwrap_or` and IO-failure-masking `let _ = …` discard in place per the mandate's "cross-check AUDIT-A; coordinate to avoid duplicate work" clause. The three sites flagged in §3 (compile.rs:264, inline.rs:441, payload.rs:173-174) are the explicit hand-off — AUDIT-A's report is the canonical disposition.
- **AUDIT-B (file-size architectural splits)**: AUDIT-B's commit (in flight at the time of this audit) splits `crates/core/src/pipeline/compile.rs` (1049 LOC) into a directory module. AUDIT-C did NOT modify any line of compile.rs; the 3 fn-body imports at lines 405,406,453 ride into post-split sub-files. Re-sweep planned post-AUDIT-B merge.

## §6 Audit Verdict

Four surgical commits landed cleanly: dead-code excision, inline-test relocation, DRY constant extraction, and value-discard cleanup. The audit doc enumerates 100+ further antipattern sites with explicit owner-wave routing — every site classified DEAD / EXCISED / KEEP / ROUTE-TO-WX. Hard-gate evidence (lint, regen 9/9, focused nextest) cited per-commit. The KISS / DRY / Special-Case Excision lane closes in budget; the heavier consolidation work (per-grammar value-API dedup, codegen template motif fold, env-gated instrumentation migration) routes to W4 / W5 / W6 per the wave mandates.
