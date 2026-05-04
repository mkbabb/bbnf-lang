# Pass A — Agent A.2 — Idiomaticity (Precepts Adherence)

Date: 2026-05-03
Lens: every precept under `docs/precepts/` applied to every file in Pass A
scope. Per-file violations enumerated; each row carries surgery.

The precepts applied (per the Pass-A directive):

- **no-workarounds** — no defensive fallbacks, no shims, no "for now"
- **no-orthogonal-codepaths** — single substrate; no conditional Vec-vs-
  scratch branches
- **KISS / DRY** — no kitchen sinks; no copy-paste mirrors
- **single-plan-execution** — execute everything in one pass; no deferrals
- **preserve-rich-AST** — no flattening of typed grammar rules for speed
- **direct-to-struct** — typed records, not tagged buffers
- **system-cohesion** — fold optimizations into existing systems
- **pluggable-components** — decision points must be pluggable, not
  hardcoded match arms
- **gestalt-approach** — no quick solutions; root-cause fixes

Categorisation per file:
- **KEEP-OUTRIGHT** — no precept violations
- **KEEP-MODIFY** — named surgery closes the violations
- **ABROGATE** — violations exceed the surgery threshold

---

## §1 — Per-file violations

### 1.1 — `crates/core/src/lib.rs`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/core/src/lib.rs:14-23` | no-workarounds | comment narrates why `mod css_types;` lives at the library crate root — "some test crates still inline a `mod css_types { ... }` shadow for hermetic-compilation reasons (tracked by REAUDIT-2026-04-30 lane 3 §5.1 as a pending mechanical-fix item)" | DELETE the css_types narration; relocate `parse_hex_color` to a per-grammar host module under `crates/<css-grammar>/src/host.rs` (per Lock 14 fn-namespace rule). Library root carries no grammar-specific shim. |
| `crates/core/src/lib.rs:26` | no-orthogonal-codepaths | `pub use generate::*` glob re-export of codegen module surface | INVESTIGATE then namespace; `bbnf::generate::*` is wide-glob. Replace with explicit re-exports of consumed items only. |

**Categorisation**: KEEP-MODIFY — surgery is mechanical (move css_types,
narrow the glob).

### 1.2 — `crates/core/src/css_types.rs`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| (whole file) | no-workarounds + system-cohesion + pluggable-components + Lock 14 | A 66-LOC grammar-specific host fn lives in the library crate root because a previous workaround threaded the test-side resolution path through `crate::css_types::parse_hex_color`. | MOVE-TO `crates/<css-grammar>/src/host.rs` (per-grammar declaration crate). The CSS L4 grammar's `crate::css_types::parse_hex_color` reference must rewrite to that crate's path; codegen emits absolute paths into the per-grammar host module. |

**Categorisation**: ABROGATE-MOVE.

### 1.3 — `crates/core/src/types.rs`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/core/src/types.rs:90` | no-workarounds (meta-language) | doc comment "lowering pipeline now walks the bootstrap tape directly into …" — narrates a substrate that is dead | DELETE the meta-narration; tape is buried (Lock 1). |

**Categorisation**: KEEP-MODIFY — single-line scrub.

### 1.4 — `crates/core/src/grammar/mod.rs`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/core/src/grammar/mod.rs:3` | no-workarounds (meta-language) | "tape-first bootstrap parser" — tape substrate is dead | DELETE the tape phrase. |
| `crates/core/src/grammar/mod.rs:7` | no-workarounds (meta-language) | "host — tape walkers" — same | DELETE the tape phrase. |
| `crates/core/src/grammar/mod.rs:17` | no-workarounds (meta-language) | "walks the tape straight into" — same | REWRITE to "walks the document straight into". |
| `crates/core/src/grammar/mod.rs:53-57` | no-workarounds | `Box::leak(source.to_owned().into_boxed_str())` to satisfy `'static`-flavoured lifetime assumptions of observational callers | INVESTIGATE — the leak is justified per the doc comment; either lift the leak boundary out of the `parse` entry (force callers to provide a `&'static`-able input) or accept the leak with a louder comment. The current `Box::leak` IS a workaround in the technical sense (per Lock § no-workarounds) — at minimum, surface it as `pub fn parse_with_leaked_input` and let callers see the cost. |

**Categorisation**: KEEP-MODIFY — narrative scrub plus one API-level decision.

### 1.5 — `crates/core/src/grammar/host.rs` (584 LOC)

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| (whole file) | no-god-modules + KISS | 584 LOC mixing GrammarSink trait + 2 impls + per-directive extractor functions + view-walking helpers | SPLIT into a directory module: `host/mod.rs` (trait + dispatch), `host/observational.rs` (`ExtractSink`), `host/pipeline.rs` (`PipelineSink`), `host/directives.rs` (per-directive extractors). |
| `crates/core/src/grammar/host.rs:387` | no-workarounds | "keyword-strip wildcard fallback (`text.strip_prefix("@debug")`)" — the wildcard catch-all is silently swallowing unrecognised keywords | FAIL-EXPLICIT — match every recognised keyword; unmatched-keyword case panics with a structured diagnostic. |
| `crates/core/src/grammar/host.rs:558` | no-workarounds (meta-language) | "dedicated child compound (legacy bootstrap_parser shape)" | DELETE the legacy marker; verify post-bootstrap shape is the only path. |

**Categorisation**: KEEP-MODIFY — mechanical split + one fail-explicit + one
narrative scrub.

### 1.6 — `crates/core/src/grammar/schema/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `grammar/schema/build.rs:26` | no-workarounds (meta-language) | "per-rule `<Rule>View` family under the tape-first AC.2" | DELETE — schema is grammar-substrate-agnostic. |
| `grammar/schema/model.rs:20` | no-workarounds (meta-language) | "the tape-first AC.2 emitter — it's the root grammar marker" | DELETE the tape-first phrase. |
| `grammar/schema/emit/rust/shared.rs:3, 17` | no-workarounds (meta-language) | "Post-Tranche AC.2 rewrite: schema helpers emit impls on tape-backed records" | DELETE — schema does not target the tape. |

**Categorisation**: KEEP-MODIFY — narrative scrub across three files.

### 1.7 — `crates/core/src/lower/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `lower/expression/mod.rs` (539 LOC) | no-god-modules | mixed concerns: term lowering, alt lowering, repeat lowering, factor lowering, mapping lowering | SPLIT — the directory `expression/` already exists; further split `mod.rs` into `expression/term.rs` + `expression/factor.rs` + `expression/mapping.rs`; `mod.rs` becomes a re-export hub ≤ 100 LOC. |
| `lower/expression/wrap.rs` (731 LOC) | no-god-modules | wrap-shape detection + MapExpr lowering + payload deduction colocated | SPLIT into `wrap/{detect.rs, map_expr.rs, payload.rs}`. |
| `lower/value_expr/atom.rs` (590 LOC) | no-god-modules | literal lowering + projection lowering + type lowering colocated | SPLIT into `atom/{literal.rs, projection.rs, type.rs}`. |
| `lower/value_expr/simple_kinds.rs:185` | no-workarounds | "Defensive fallback — descend through any value-layer" | FAIL-EXPLICIT. Defensive fallbacks are workarounds. Either prove the case is unreachable and `unreachable!()`, or fix the upstream pass. |

**Categorisation**: KEEP-MODIFY — three splits + one fail-explicit.

### 1.8 — `crates/core/src/path/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `path/markers.rs:14-30` | pluggable-components + Lock 14 | hardcoded ZSTs `Json`, `CssL4`, `Sheets`, `Bbnf` | RELOCATE markers to the per-grammar declaration crate (each grammar emits its own `pub struct <G>;` ZST); the path crate carries only the `GrammarMarker` trait. |
| `path/ascent.rs:61` | no-workarounds (meta-language) | "callback that traverses the tape, while the W2.5 bench harness" | DELETE the tape phrase. |
| `path/cursor.rs:313-314` | no-inline-tests | inline `#[cfg(test)]` block | MOVE-TO `crates/core/tests/path_cursor.rs`. |
| `path/executor.rs:65-66` | no-inline-tests | inline `#[cfg(test)]` block | MOVE-TO `crates/core/tests/path_executor.rs`. |
| `path/schema.rs:130-131` | no-inline-tests | inline `#[cfg(test)]` block | MOVE-TO `crates/core/tests/path_schema.rs`. |
| `path/variant_select.rs:21` | no-workarounds (meta-language) | "The resolver is layout-only — it does not consult the runtime tape" | DELETE the tape disclaimer. |

**Categorisation**: KEEP-MODIFY — markers relocation + three test moves +
narrative scrub.

### 1.9 — `crates/core/src/imports/`

No precept violations on inspection. The imports directory is a small,
cohesive set of files (errors / loader / registry / resolve) with single-
purpose modules and a one-paragraph mod.rs. **Categorisation**: KEEP-OUTRIGHT.

### 1.10 — `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/mod.rs` (or absent) | feedback_directory_modules | a flat sibling file `pipeline.rs` co-exists with the `pipeline/` directory; the directory has no `mod.rs` | RESTRUCTURE — convert `pipeline.rs` into `pipeline/mod.rs`. The existing directory + flat-file pair violates `feedback_directory-module-structure`. |

**Categorisation**: KEEP-MODIFY — mechanical merge.

(Note: `pipeline/` is mostly Pass B scope; the surgery itself is Pass A.)

---

## §2 — `crates/ir/` violations

### 2.1 — `crates/ir/src/lib.rs` + types

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/ir/src/types/grammar.rs` (584 LOC) | no-god-modules | `GrammarIR` definition + accessors + serde/MessagePack glue + ~40 distinct fields colocated | SPLIT into `types/grammar/{def.rs, accessors.rs, serde.rs}`. |
| `crates/ir/src/types/grammar.rs:142, 489` | no-workarounds (meta-language) | "classify_regex survives only as the fall-through" | KEEP code, scrub comment. |
| `crates/ir/src/types/type_desc.rs:103, 147` | no-workarounds (meta-language) + Lock 2 | "Span-typed rules already carry their span in `TapeRec`" + "store their span natively in `TapeRec.span_lo/`" | DELETE TapeRec mentions. The substrate is the typed-enum (Lock 1). |
| `crates/ir/src/types/type_desc.rs` (212 LOC) | no-workarounds + Lock 2 | the TYPE itself is named `TypeDesc` — a Lock 2 retired term | RENAME `TypeDesc` → fold into `Layout`. The IR pass is `bbnf-ir/src/passes/layout/`; the IR record is `Layout`; `TypeDesc` becomes a private interior detail (or a `Layout::Atom` variant) — not a public type. |

### 2.2 — `crates/ir/src/registry/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/ir/src/registry/strategy.rs:130-185` | pluggable-components + Lock 14 | `PRODUCTION_MANIFEST_TABLE` hardcodes 9 grammar idents in source — `JsonParser`, `JsonGrammar`, `GoogleSheetsParser`, `CssL4Parser`, `BbnfBootstrap`, `BbnfParser`, `CsvParser`, `MathParser`, `BnfParser`, `EbnfParser`, `CssPrettyParser`, plus `crate::runtime::<g>::<G>StructBuilder`/`<G>Document` strings | The IR crate must read from `[workspace.metadata.bbnf-strategy]` at xtask-regen time, NOT a hardcoded `static GRAMMARS` table compiled into the IR crate. The IR crate cannot know any grammar's name. The current "mirror" mechanism violates Lock 14. |
| `crates/ir/src/registry/struct.rs` (391 LOC) + (whole file) | Lock 2 | the canonical record is `StructLayout` — a Lock 2 retired term | RENAME `StructLayout` → `Layout`; `StructRegistry` → `LayoutRegistry`. |

### 2.3 — `crates/ir/src/passes/`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `crates/ir/src/passes/audit/payload_coverage.rs:67-77` | pluggable-components + Lock 14 | `enum GrammarAuditTag { Json, CssL4, Sheets, Bbnf, Custom(&'static str) }` — named arms PER GRAMMAR | MERGE-WITH `Custom(&'static str)` — drop the named arms; every grammar is `Custom` keyed by its identifier from the workspace metadata. |
| `crates/ir/src/passes/audit/payload_coverage.rs` (585 LOC) | no-god-modules | classifier + walker + report aggregator colocated | SPLIT into `audit/payload_coverage/{classify.rs, walk.rs, report.rs}`. |
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` | Lock 14 + KISS + pluggable-components | the entire file is grammar-named ("bbnf") and explicitly mines `big_comment` + `mapped_factor` patterns from `grammar/bbnf/bbnf.bbnf` | DELETE — generalise as a structural-shape miner with a per-grammar `[recognizer]` config in workspace metadata, OR move to a per-grammar recognizer crate. |
| `crates/ir/src/passes/recognizers/grammar_facts.rs` (1530 LOC) | no-god-modules | mixed: alt-classifier, chain-facts, branch-uniqueness, etc. | SPLIT into `recognizers/grammar_facts/{alt_classifier.rs, chain.rs, branch_uniqueness.rs, ...}`. |
| `crates/ir/src/passes/csp_strategy/mod.rs` (1361 LOC) | no-god-modules | mixed: solver wiring, domain construction, materialisation tie-in | the directory `csp_strategy/constraints/` exists; split mod.rs into `csp_strategy/{solver_wiring.rs, domains.rs, materialization_glue.rs}`. |
| `crates/ir/src/passes/materialization/classify.rs` (843 LOC) | no-god-modules | per-classifier-rule code colocated | SPLIT per classifier rule. |
| `crates/ir/src/passes/types/mod.rs` (786 LOC) | no-god-modules + Lock 2 | mixed type-vars solver + projection + lifetime calculus + registry mediation; PLUS the directory is named `types/` (Lock 2 retired term) | (a) RENAME `passes/types/` → `passes/layout/`; (b) SPLIT mod.rs into `layout/{solver.rs, projection.rs, lifetime.rs, registry_glue.rs}`. |
| `crates/ir/src/passes/types/registry.rs` (510 LOC) | no-god-modules | registry probes + layout admission + named-resolution colocated | SPLIT. |
| `crates/ir/src/passes/types/type_map.rs` (203 LOC) | Lock 2 | "TypeMap" is a Lock 2 retired term | RENAME `TypeMap` → fold into `Layout` representation; eliminate the term. |
| `crates/ir/src/passes/csp_domains.rs` (500 LOC) | no-god-modules | per-domain-constructor code colocated | SPLIT per domain. |
| `crates/ir/src/passes/profile.rs:26, 108` | Lock 14 | `bbnf_shape_templates: Vec<BbnfShapeTemplate>` field on the universal `GrammarProfile` struct + `mine_bbnf_shape_templates` import | DELETE the field; arrives via the generalised recogniser pipeline. |

### 2.4 — `crates/ir/src/passes/csp_strategy/mod.rs:113-115, 359`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `csp_strategy/mod.rs:113-115` | no-workarounds (meta-language) | doc comment names CSS L4 as the perf-sensitive grammar | KEEP — narrative is informative, not policy. |
| `csp_strategy/mod.rs:359` | (same) | same | KEEP. |

---

## §3 — Sister crate violations

### 3.1 — `crates/bbnf-path/src/path_macro.rs` (639 LOC)

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| (whole file) | no-god-modules | proc-macro IO + lex + lower + validate + emit colocated | SPLIT into `path_macro/{lex.rs, lower.rs, validate.rs, emit.rs}` so each phase ≤ 200 LOC. |

### 3.2 — `crates/bbnf-path/src/registry.rs:132-135`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| `registry.rs:132-135` | pluggable-components + Lock 14 | `match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }` | the proc-macro must consume a `RegistryDescriptor` per grammar from a workspace-metadata-driven table. Per-grammar arms are forbidden under Lock 14. |

### 3.3 — `crates/bbnf-path-ts/src/compile.rs` (474 LOC) + `fixture.rs` (248 LOC)

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| (compile.rs whole file) | DRY + system-cohesion | mirror of `bbnf-path/src/path_macro.rs` validate/lower logic | EXTRACT shared logic into a non-proc-macro crate (`bbnf-path-core` per Lock 7); both `bbnf-path` (proc-macro) and `bbnf-path-ts` (cdylib) path-dep on it. |
| (fixture.rs whole file) | DRY + Lock 14 | per-grammar fixture registry — synthetic mirror of the proc-macro's registry | DELETE — finish the T4 closure: emit `pub const REGISTRY: <Layout>` per grammar in the xtask-emitted source; both frontends consume the production const. |

### 3.4 — `crates/bootstrap/src/lib.rs`

| File:line | Precept | Violation | Surgery |
|---|---|---|---|
| (whole file) | KISS | one-line re-export shim (28 LOC including comments) | KEEP — the shim is the entire library; it is what BBNF self-host consumers reach for. The comment is historical and could be tightened. |

**Categorisation**: KEEP-OUTRIGHT (narrative could be slightly tightened).

---

## §4 — Per-file roll-up

### 4.1 — KEEP-OUTRIGHT (no violations)

| File |
|---|
| `crates/core/src/imports/{mod,errors,loader,registry,resolve}.rs` |
| `crates/core/src/lower/{string_interner,fn_table,metadata}.rs` |
| `crates/core/src/lower/expression/{closures,repeat,alt}.rs` |
| `crates/core/src/lower/value_expr/{mod,literals,view_walk,unwrap,precedence}.rs` |
| `crates/core/src/lower/view_walk.rs` |
| `crates/core/src/path/{ir,error,type_check,schema,executor,wildcard,variant_select}.rs` (modulo Lock 14 on markers) |
| `crates/core/src/grammar/schema/{mod}.rs` (clean re-export) |
| `crates/core/src/grammar/schema/emit/rust/{directives,identifiers}.rs` |
| `crates/ir/src/{lib,cost_config}.rs` |
| `crates/ir/src/types/{mod,node,rule,map_expr,fn_descriptor,recognizer_configs,type_desc_interner}.rs` |
| `crates/ir/src/registry/mod.rs` |
| `crates/ir/src/dag/*` (DAG cluster — clean) |
| `crates/ir/src/recognizer/{plans,facts}.rs` |
| `crates/ir/src/egraph/*` (egraph cluster — clean per Pass A scope; Pass B may have more) |
| `crates/ir/src/rewrites/{mod,base,schema,tiering,rank,path_seed}.rs` |
| `crates/bootstrap/src/lib.rs` |
| `crates/bootstrap/src/bin/{dump_ir,cost_grid_sweep,debug_parse}.rs` |
| `crates/bbnf-path/src/lib.rs` |
| `crates/bbnf-path-ts/src/{lib,schema,template_tag}.rs` |
| `grammar/*` (source-tree files; Pass A scope is layout, not content) |

### 4.2 — KEEP-MODIFY (named surgery)

| File | Surgery summary |
|---|---|
| `crates/core/src/lib.rs` | scrub css_types narration + narrow generate-glob |
| `crates/core/src/types.rs` | scrub tape comment at L90 |
| `crates/core/src/grammar/mod.rs` | scrub tape phrases (L3, L7, L17); reconsider `Box::leak` at L57 |
| `crates/core/src/grammar/host.rs` | SPLIT (god module) + fail-explicit at L387 + scrub L558 |
| `crates/core/src/grammar/schema/{build,model,emit/rust/shared}.rs` | scrub tape narrative |
| `crates/core/src/grammar/generated/mod.rs:35` | drop BBNF aggregator `pub use bbnf::*` (asymmetry) |
| `crates/core/src/lower/expression/{mod,wrap}.rs` | SPLIT (god modules) |
| `crates/core/src/lower/value_expr/{atom,simple_kinds}.rs` | SPLIT atom; fail-explicit at simple_kinds.rs:185 |
| `crates/core/src/path/markers.rs` | RELOCATE per-grammar ZSTs to per-grammar declaration crates (Lock 14) |
| `crates/core/src/path/{cursor,executor,schema}.rs` | move inline tests to `tests/` |
| `crates/core/src/path/{ascent,variant_select}.rs` | scrub tape phrases |
| `crates/core/src/pipeline.rs` + `pipeline/` | RESTRUCTURE to directory module |
| `crates/ir/src/types/{grammar,type_desc}.rs` | SPLIT god module; rename TypeDesc; scrub TapeRec phrases |
| `crates/ir/src/registry/{strategy,struct}.rs` | retire `PRODUCTION_MANIFEST_TABLE` to workspace metadata; rename StructLayout → Layout |
| `crates/ir/src/passes/audit/payload_coverage.rs` | merge `GrammarAuditTag` named arms into `Custom`; SPLIT god module |
| `crates/ir/src/passes/recognizers/grammar_facts.rs` | SPLIT (1530-LOC god module) |
| `crates/ir/src/passes/csp_strategy/mod.rs` | SPLIT (1361-LOC god module) |
| `crates/ir/src/passes/materialization/classify.rs` | SPLIT (843-LOC god module) |
| `crates/ir/src/passes/types/{mod,registry,type_map}.rs` | RENAME `passes/types/` → `passes/layout/`; rename `TypeMap` → fold into Layout |
| `crates/ir/src/passes/csp_domains.rs` | SPLIT |
| `crates/ir/src/passes/profile.rs` | DROP `bbnf_shape_templates` field |
| `crates/bbnf-path/src/path_macro.rs` | SPLIT |
| `crates/bbnf-path/src/registry.rs` | retire match-on-grammar (Lock 14) |
| `crates/bbnf-path-ts/src/{compile,fixture}.rs` | extract shared logic to `bbnf-path-core` |

### 4.3 — ABROGATE (precept-violations exceed surgery threshold)

| File | Disposition |
|---|---|
| `crates/core/src/css_types.rs` | ABROGATE-MOVE (relocate to per-grammar declaration crate per Lock 14) |
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` | ABROGATE-DELETE (grammar-named in generic crate; replace with metadata-driven recogniser registry) |

---

## §5 — Cross-precept verdict roll-up

| Precept | KEEP-OUTRIGHT | KEEP-MODIFY | ABROGATE | Total Pass A footprint |
|---|---:|---:|---:|---:|
| no-workarounds | majority | ~12 narrative-scrub sites + `simple_kinds.rs:185` + `host.rs:387` | (none unique to this precept) | every doc comment that names "tape" |
| no-orthogonal-codepaths | majority | `lib.rs:26` (glob narrowing) | (none unique) | substrate is fundamentally singular post-AX |
| KISS / DRY | majority | god-module SPLITs above + `bbnf-path-ts` mirror extraction | (none unique) | 23 files >500 LOC outside `generated/` per CENSUS §5 |
| single-plan-execution | majority | (no specific surgeries; this precept binds the orchestrator) | (none) | n/a |
| preserve-rich-AST | majority | (no surgeries; the AST IS the typed CST today) | (none) | n/a |
| direct-to-struct | majority | (no surgeries; structural-direct is the substrate post-AX) | (none) | n/a |
| system-cohesion | majority | `bbnf-path-ts` mirror; `css_types` relocation; `passes/types/` rename | `shape_dict_bbnf.rs` (orthogonal subsystem) | 3 sites |
| pluggable-components | majority | `path/markers.rs` relocate; `registry/strategy.rs` retire hardcoded table | `passes/audit/payload_coverage.rs` enum (named-arm rebellion); `bbnf-path/src/registry.rs:132-135` (match on grammar) | 4 sites |
| gestalt-approach | majority | (no specific surgeries; binds the orchestrator) | (none) | n/a |

---

## §6 — Notes for synthesizer

1. The css_types relocation is a Lock 14 + no-workarounds intersection — the
   single-source-of-truth narrative cited in the file's docstring is a
   workaround for a generic-crate placement. Pass A names it ABROGATE-MOVE.

2. The `shape_dict_bbnf.rs` deletion is the cleanest Lock-14 redress in Pass
   A scope. The replacement facility (a metadata-driven structural-shape
   miner) is Agent A.5's territory.

3. The 23 god modules outside `generated/` (per CENSUS §5) include 9 in Pass
   A scope (`crates/core/src/{grammar/host,lower/expression/{mod,wrap},
   lower/value_expr/atom}.rs` + `crates/ir/src/{types/grammar,registry/struct,
   passes/{recognizers/grammar_facts,csp_strategy/mod,materialization/classify,
   types/mod,types/registry,csp_domains,audit/payload_coverage}}.rs` +
   `crates/bbnf-path/src/path_macro.rs` + `crates/bbnf-path-ts/src/compile.rs`).
   Each is a SPLIT obligation per `feedback_no_god_modules`.

4. Inline-tests in src/ (8 across Pass A scope per CENSUS §7) are mechanical
   moves — no test logic changes; only file location. The synthesizer can
   bundle the moves into one BA wave.

5. The `Box::leak` in `crates/core/src/grammar/mod.rs:57` is a deliberate
   workaround that the doc comment defends. It is a workaround per the
   precept; Pass A flags it for synthesizer adjudication. The alternative
   is forcing callers to provide `&'static`-able input — a public-API
   change.
