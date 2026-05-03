# Pass A — Agent A.6 — Cross-Cut Analysis

Date: 2026-05-03
Lens: concerns spanning multiple files. The lane surfaces cross-file
dependencies, hidden coupling, accidental complexity. No per-file
classification.

---

## §1 — Hidden grammar coupling

### 1.1 — Grammar-name match arms in Pass A scope

The Lock 14 verification command is
`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>'
crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,bbnf-regex,parse-that,simd-scan,analysis,lsp}/src/`

In Pass A scope (the corpus of generic crates above), the live hits are:

| Path:line | Match form | Producer | Consumer |
|---|---|---|---|
| `crates/ir/src/passes/audit/payload_coverage.rs:67-77` | `enum GrammarAuditTag { Json, CssL4, Sheets, Bbnf, Custom(&'static str) }` + `match` on the enum at L82-89 (key()) | `audit_payload_coverage` callers tag their input with `GrammarAuditTag::Json` etc. | `payload_coverage`'s JSON-output keying logic |
| `crates/bbnf-path/src/registry.rs:132-135` | `match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }` (string-name match) | proc-macro-time grammar identifier from `path!(<G>, …)` invocations | per-grammar `RegistryDescriptor` lookup |
| `crates/ir/src/registry/strategy.rs:130-185` (table data, not match arm — but the same Lock 14 fault) | `PRODUCTION_MANIFEST_TABLE` — 9 hardcoded grammar idents | xtask reads workspace metadata; `for_grammar(grammar_ident, …)` consults this table | Rust emitter splices builder/document paths from the table into `parse()` body |

### 1.2 — Grammar-named modules in Pass A scope

| Path:line | Module | Why it lives here |
|---|---|---|
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` | the entire file is named for one grammar | BBNF-specific shape-template mining (big_comment, mapped_factor empty branch); the recogniser pipeline at `recognizers/mod.rs:193` consumes a slice of `&dyn RecognizerMiner` impls and the BBNF miner is hardcoded among them |
| `crates/core/src/css_types.rs` | grammar-named host fn at library root | CSS L4's `-> parse_hex_color(...)` map reference reaches `crate::css_types::parse_hex_color`; the symbol hosts itself at the library root because the test-side resolution path needs an unambiguous absolute path |
| `crates/core/src/path/markers.rs:14-30` | per-grammar marker ZSTs (`Json`, `CssL4`, `Sheets`, `Bbnf`) | every grammar that participates in `TypedPath<G, T>` declares its `G` here; the path crate hardcodes the four extant grammars |

### 1.3 — Grammar-named field on universal struct

| Path:line | Field | Universal struct |
|---|---|---|
| `crates/ir/src/passes/profile.rs:26, 108` | `bbnf_shape_templates: Vec<BbnfShapeTemplate>` | `GrammarProfile` (the universal per-grammar profile; has BBNF-prefixed field) |

### 1.4 — Grammar-named test in IR (boundary surface)

| Path:line | Issue |
|---|---|
| `crates/ir/tests/structural_alphabet_extended.rs` (multiple `fn css_l4_fixture()` etc.) | tests are allowed to instantiate concrete grammars; CENSUS §2.2 marks these DEFER-WITH-RATIONALE; Lock 14's surface is generic-crate src/, not generic-crate tests/ |
| `crates/ir/tests/payload_coverage_audit.rs` (`GrammarAuditTag::CssL4` references) | downstream of `payload_coverage` enum; cleanup follows replacement #3 |

### 1.5 — Cross-cut: who consumes per-grammar code?

| Producer | Consumer | Surface |
|---|---|---|
| `css_types.rs:20` `parse_hex_color` | `grammar/css/l4/*.bbnf` `-> parse_hex_color(...)` map reference; CSS L4 generated parser at `crates/core/src/grammar/generated/css_l4.rs` | host-fn dispatch |
| `path/markers.rs` ZSTs | `bbnf-path` proc-macro arms; `runtime/<g>` per-grammar Document `*PathQuery` impls | typed-path machinery |
| `registry/strategy.rs:130-185` table | xtask-regen + Rust emitter | substrate selection |
| `audit/payload_coverage.rs` enum | tests + audit JSON output | coverage reporting |
| `passes/recognizers/shape_dict_bbnf.rs` | profile.rs's `bbnf_shape_templates` field; emitter consumption per CENSUS §2.2 | shape collapse |
| `passes/profile.rs:26` field | emitter or downstream consumer | (ratifying replacement #5: deletion is safe) |

### 1.6 — Surgery (cross-cut)

A single coordinated retirement of all five sites resolves Lock 14 in Pass
A scope:

1. Move `css_types.rs` to per-grammar declaration crate
2. Generalise `shape_dict_bbnf.rs` to metadata-driven structural-shape
   miner; delete the file
3. Drop `bbnf_shape_templates` field on `GrammarProfile`
4. Merge `GrammarAuditTag` named arms into `Custom`
5. Retire `PRODUCTION_MANIFEST_TABLE` to xtask-passed `StrategyTable`
6. Rewrite `bbnf-path/src/registry.rs:132-135` to consume metadata
7. Relocate `path/markers.rs` ZSTs to per-grammar declaration crates

The seven steps land in one BA wave; deferring leaves Lock 14 violated
for the rest of the surgery.

---

## §2 — Shared substrate (types or functions used by ≥3 modules)

### 2.1 — `parse_that::Span`

| Consumer | Path |
|---|---|
| `crates/core/src/grammar/host.rs` | `use parse_that::Span;` (L22) |
| `crates/core/src/types.rs` | (transitively, via `RuleEntry<'a>`'s span fields) |
| `crates/ir/src/types/grammar.rs` | (potentially; uses `GrammarSpan` which mirrors but is not Span) |
| Other callers | indirect via `BbnfView` / `runtime/<g>::View` types |

`Span` is the lifetime-borrowed source-region alphabet. It comes from a
sibling repo. Per Lock 11, `parse-that` should be a workspace path-dep so
the `Span` type is greppable in-tree.

### 2.2 — `CharSet128`, `regex_first_chars` (from `bbnf-regex`)

| Consumer | Path |
|---|---|
| `crates/ir/src/lib.rs` | `pub use bbnf_regex::sets::charset::CharSet128;` (re-export L33) |
| `crates/ir/src/lib.rs` | `pub mod regex_first { pub use bbnf_regex::first::regex_first_chars; }` (re-export L35-37) |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs` (likely) | uses `CharSet128` for byte-set computation |
| `crates/ir/src/passes/sets/structural_alphabet.rs` (likely) | uses `CharSet128` |

The IR crate explicitly re-exports `CharSet128` so its consumers don't
need a direct `bbnf-regex` dep. Per Lock 11, `bbnf-regex` should be a
workspace path-dep.

### 2.3 — `bbnf_ir::dag::NodeId`

| Consumer | Path |
|---|---|
| `crates/core/src/path/ascent.rs:29` | `use bbnf_ir::dag::NodeId;` — re-uses the IR's NodeId as the parsed-document node identifier |
| `crates/ir/src/dag/*` | producer |
| `crates/ir/src/egraph/*` | consumer |

`NodeId` is shared between the IR DAG (compile-time) and the path-ascent
substrate (runtime). The Lock 7 path-crate consolidation should preserve
this shared type or rename if confusion arises.

### 2.4 — `BbnfView` / `BbnfDocument` / `BbnfCompoundKind`

| Consumer | Path |
|---|---|
| `crates/core/src/lower/mod.rs` | bbnf lowering walks BbnfView |
| `crates/core/src/lower/expression/*.rs` | bbnf-rule lowering |
| `crates/core/src/lower/value_expr/*.rs` | bbnf value-expression lowering |
| `crates/core/src/grammar/mod.rs` | bbnf parse entry returns `BbnfDocument` |
| `crates/core/src/grammar/host.rs` | bbnf grammar-extraction walks BbnfView |
| `crates/analysis/` (Pass C boundary) | LSP analysis walks BbnfView |
| `crates/gorgeous/` (Pass C boundary) | prettifier walks BbnfView |

`BbnfView` is shared substrate across `lower/`, `grammar/`, `analysis/`,
`gorgeous/`. It is a per-grammar runtime type (lives at `crates/core/src/runtime/bbnf/`
which is Pass B scope) — but it IS the canonical CST surface every Pass A
consumer reaches for. Cross-cut: the BBNF self-host's own runtime types
are the lingua franca for analysis-of-other-grammar-sources.

### 2.5 — Surgery (shared substrate)

The shared substrates above are HONEST sharing — no relocation needed.
The only relocation candidate is `parse-that::Span` and `bbnf-regex::*`
into the workspace per Lock 11.

---

## §3 — Accidental complexity in the parse driver

### 3.1 — How many code paths exist for "parse a leaf"?

The Pass A scope's lowering side (the producer of GrammarIR; the parser
itself is in generated/, Pass B scope):

| Leaf form | Lowering path | LOC |
|---|---|---|
| Literal string | `lower/expression/mod.rs::lower_term` → `IrNode::Lit` | bounded |
| Regex literal | `lower/expression/mod.rs::lower_term` → `IrNode::Regex` | bounded |
| Reference | `lower/expression/mod.rs::lower_term` → `IrNode::Ref` | bounded |
| Mapped factor | `lower/expression/wrap.rs::lower_wrap` → `IrNode::Map` | 731 LOC (god module) |
| Closure call | `lower/expression/closures.rs::lower_call` → beta-reduce + body lower | 91 LOC |

The leaf-lowering paths are SINGULAR per leaf form — one fn per leaf form
in the lowering module. The wrap-lowering path is the god module
(`wrap.rs`); it mixes wrap-shape detection + MapExpr lowering + payload
deduction. Per `feedback_no_orthogonal_codepaths`, the SPLIT obligation
does not introduce orthogonality — it separates concerns within one path.

### 3.2 — How many code paths for "parse a compound"?

Lowering side:

| Compound form | Path |
|---|---|
| Seq | `lower/expression/mod.rs::lower_term` → `IrNode::Seq` |
| Alt | `lower/expression/alt.rs::lower_alt` → `IrNode::Alt` |
| Repeat | `lower/expression/repeat.rs::lower_repeat` → `IrNode::Repeat` |
| Wrap | `lower/expression/wrap.rs::lower_wrap` → `IrNode::Wrap` |
| Pratt | `lower/expression/pratt.rs::lower_pratt` (auto-detected) |

Each compound form has one path — singular per Lock 3. The split-by-form
is structural, not orthogonal.

### 3.3 — How many code paths for "parse a repeat"?

| Repeat form | Path |
|---|---|
| All repeat shapes | `lower/expression/repeat.rs::lower_repeat` (one fn) |

Singular.

### 3.4 — How many code paths for the wrap shape?

| Wrap detection | Path | LOC |
|---|---|---:|
| Wrap shape detection | `lower/expression/wrap.rs::detect_wrap_shape` | (sub-section) |
| MapExpr lowering | `lower/expression/wrap.rs::lower_map_expr` | (sub-section) |
| Payload deduction | `lower/expression/wrap.rs::deduce_payload` | (sub-section) |

Three concerns colocated in one 731-LOC file. The SPLIT (per Agent A.2 +
A.3) is mechanical — separate concerns, single fn per concern.

### 3.5 — Orthogonality verdict

Pass A's lowering pipeline is structurally singular per Lock 3 +
`feedback_no_orthogonal_codepaths`. The god-module concern is a Lock 13
size violation, NOT an orthogonality violation.

---

## §4 — God directories in Pass A scope

### 4.1 — Per Lock 13 ("> 10 immediate children mixing concerns")

| Directory | Children | Concerns mixed? |
|---|---:|---|
| `crates/core/src/` (the lib root) | 11 (`backend`, `css_types`, `generate`, `grammar`, `graph`, `imports`, `lib`, `lower`, `path`, `pipeline`, `runtime`, `types`) | **YES** — parser-front (lower, pipeline, imports) + middle-pipeline (generate, backend) + runtime (runtime). **God directory by mixed concern.** |
| `crates/ir/src/passes/` | (~15 sub-directories + ~9 single-files) | **NO** — every child is a "pass" (one cohesive concern at this level). The mixing is INSIDE a child directory (e.g., csp_strategy/mod.rs's mixed concerns). |
| `crates/ir/src/passes/recognizers/` | 18 sub-files | **borderline** — every child IS a recogniser; cohesive at this level. The 1530-LOC `grammar_facts.rs` is the LOC violation, not directory mixing. |
| `crates/ir/src/passes/recognizers/shape_dispatch/` | 12 sub-files | **NO** — every child is a shape dispatch; cohesive. |

### 4.2 — Per `feedback_no_god_modules` ("'utils' / 'helpers' / 'common' kitchen sinks are god modules in gestation")

| Path | Issue |
|---|---|
| (none in Pass A scope at HEAD) | — |

There are NO `utils`, `helpers`, or `common` directories in Pass A scope.
The `view_walk.rs` shared-helpers file in `lower/` is appropriately named
(it walks views, period); not a kitchen sink.

### 4.3 — Surgery

The single god directory (`crates/core/src/`) is Agent A.4's Proposal 1
territory (fracture into bbnf-parse + bbnf-codegen + bbnf-runtime + bbnf
aggregator). Other directories are clean at the directory level; their
size violations are at the file level (Lock 13 inner clause).

---

## §5 — Hidden dependents on dead substrate

### 5.1 — Tape residue per CENSUS §1

| Path:line | Residue type | Dependent count |
|---|---|---:|
| Comment-only references to `Tape*` symbols | narrative meta-language | ~15 in Pass A scope (per Agent A.3 §Lock 1 violations) |
| `crates/core/src/path/ascent.rs:61` | doc-comment "callback that traverses the tape" | 1 |
| `crates/core/src/path/variant_select.rs:21` | doc-comment | 1 |
| `crates/core/src/grammar/{mod,schema/build,schema/model,schema/emit/rust/shared}.rs` | doc-comment cluster | 4 |
| `crates/core/src/types.rs:90` | doc-comment | 1 |
| `crates/ir/src/types/{type_desc,grammar}.rs` | doc-comment cluster | ~3 |

No live tape symbol exists in Pass A scope source; only comments. The
residue is mechanical scrub.

### 5.2 — `OpenFrame` residue

| Path:line | Issue |
|---|---|
| `crates/core/src/grammar/generated/*.rs` (multiple) | live `OpenFrame` enums emitted into generated parsers (Pass B scope; out of Pass A) |
| `crates/ir/src/types/grammar.rs:310` | comment "FusedBuilder::with_capacity divisor so RawVec::grow_one" — narrative residue (Pass A scope) |

### 5.3 — `EmissionTier` residue

| Search | Result |
|---|---|
| `rg -n 'EmissionTier' crates/core/src crates/ir/src` | (search yields no hits in Pass A scope at HEAD) |

`EmissionTier` is dead in Pass A scope.

### 5.4 — Surgery

The tape + OpenFrame + EmissionTier residues in Pass A scope are pure
narrative. The synthesizer can batch the scrub into a BA W0 wave. No code
change risk.

---

## §6 — Cyclic dependency risk

### 6.1 — `bbnf-ir` depends on what?

`crates/ir/Cargo.toml` deps:

```
serde, rmp-serde, bbnf-regex, rustc-hash, rayon, smallvec, csp-solver,
egraph, egraph-derive, ron, syn (build), quote, regex, tempfile,
prettyplease, pprint, divan, mimalloc, sonic-rs, bbnf-path (dev),
trybuild, serde, simd-json, jiter, nom
```

Among Pass A scope: `bbnf-regex`, `csp-solver`, `egraph`, `egraph-derive`.
None of these reverse-depend on `bbnf-ir` per their Cargo.toml inspection.

### 6.2 — Should `bbnf-ir` depend on anything in `crates/core/`?

| Direction | Status |
|---|---|
| `bbnf-ir` ← `crates/core` (core depends on ir) | YES — `crates/core/Cargo.toml` has `bbnf-ir = { ..., path = "../ir" }` |
| `bbnf-ir` → `crates/core` | NO — `crates/ir/Cargo.toml` has no `bbnf = ...` dep |

Direction is correct: core depends on ir; not the reverse. No cycle.

### 6.3 — `bbnf-path` depends on what?

`crates/bbnf-path/Cargo.toml` deps: `syn`, `quote`, `proc-macro2`,
`bbnf-regex`, `bbnf-ir`, `serde`, `serde_json`. The proc-macro consumes
`bbnf-ir` directly to look up registry data; `bbnf-regex` for path-string
lex.

| Direction | Status |
|---|---|
| `bbnf-path` ← `crates/core` (core depends on bbnf-path) | YES |
| `bbnf-path` → `bbnf-ir` | YES (proc-macro time) |
| Cycle? | NO (bbnf-ir doesn't depend on bbnf-path) |

### 6.4 — Cycle verdict

No cycles in Pass A scope at HEAD. The proposed Proposal 3 path-crate
triplet (`path-core`, `path`, `path-ts`) introduces:

| Direction |
|---|
| `path` (proc-macro) → `path-core` |
| `path-ts` (cdylib) → `path-core` |
| `path-core` → `bbnf-ir` (consumes Layout) |
| `bbnf-runtime` (post-Proposal-1) → `path-core` (consumes runtime executor) |

No cycle.

---

## §7 — Per-grammar runtime directory god-by-mixed-concern

The directive's Lock 13 sample names "today's `crates/core/src/runtime/`"
as a god directory by mixed-concern (16 siblings mixing per-grammar
subdirs with generic mechanism files). Pass A scope is parser-front, NOT
the runtime side; the runtime god-directory is technically Pass B scope.
But the pattern is foundational to the Lock 14 violation surface; the
synthesizer must reconcile.

| Sub-tree | Per-grammar? | Generic mechanism? |
|---|---|---|
| `runtime/bbnf/` | YES | (per-grammar) |
| `runtime/bnf/` | YES | (per-grammar) |
| `runtime/csv/` | YES | (per-grammar) |
| `runtime/css_l4/` | YES | (per-grammar) |
| `runtime/css_pretty/` | YES | (per-grammar) |
| `runtime/ebnf/` | YES | (per-grammar) |
| `runtime/google_sheets/` | YES | (per-grammar) |
| `runtime/json/` | YES | (per-grammar) |
| `runtime/math/` | YES | (per-grammar) |
| `runtime/arena_template.rs` | NO | YES (template substrate) |
| `runtime/builder.rs` | NO | YES (StructBuilder trait) |
| `runtime/builder_template.rs` | NO | YES (template substrate) |
| `runtime/handle.rs` | NO | YES (CompoundHandle, StringHandle) |
| `runtime/path.rs` | NO | YES (legacy borrowed path alphabet) |
| `runtime/view.rs` | NO | YES (RuntimeView) |
| `runtime/mod.rs` | NO | YES (re-exports) |

16 immediate children, mixed per-grammar + generic mechanism. **God
directory.** Lock 13 + Lock 14 cross-cut. Surgery: per Lock 14, the
per-grammar runtime dirs become emit output of a single template (Agent
A.5 replacement #5); the generic mechanism files relocate to
`bbnf-runtime/` (Agent A.4 Proposal 1).

The Pass A surface flags this as a residue for synthesizer pickup; Pass B
owns the surgery in detail.

---

## §8 — Cross-cut surgery roll-up

| Cross-cut | Surgery | Receiving locus |
|---|---|---|
| Lock 14 grammar coupling (7 sites in Pass A scope) | retire all 7 sites in one BA wave | Agent A.5 §1 replacements #1, #2, #3, #4, #5, #6, #8 |
| Workspace promotion of `parse-that` + `bbnf-regex` (Lock 11 + shared substrate cleanup) | path-dep + workspace member | Agent A.4 Proposal 4 |
| Shared NodeId between IR DAG + path-ascent | preserve through Lock 7 consolidation | Agent A.4 Proposal 3 |
| BbnfView shared substrate across `lower/` + `analysis/` + `gorgeous/` | preserve; no relocation | (no surgery; this is honest sharing) |
| Lower-pipeline orthogonality | none required (singular paths) | (no surgery) |
| God directory at `crates/core/src/` | fracture per Proposal 1 | Agent A.4 Proposal 1 |
| God modules (>500 LOC) — 13 in Pass A scope | SPLIT each | Agent A.2 §1 + A.3 §13 |
| Tape narrative residue — ~15 sites | comment scrub | BA W0 wave |
| `runtime/` god directory by mixed-concern | template-emit per-grammar; relocate generic substrate | Pass B residue (Agent A.6 §7) |

---

## §9 — Cross-cut residues for synthesizer

Cross-tranche concerns Pass A surfaces:

1. **Lock 14 retirement is one BA wave**, not seven separate waves. The
   seven sites identified in §1 share the same fault pattern (per-grammar
   code in a generic crate) and should retire in one coordinated pass to
   avoid leaving the lock partially honoured between waves.

2. **Workspace promotion (Lock 11) is foundational**. Until `parse-that`
   and `bbnf-regex` are workspace path-deps, every Lock 14 surgery in
   Pass A is fragile to dep-form changes.

3. **Lock 2 rename + Lock 13 SPLIT pair on the IR side** — `passes/types/`
   →  `passes/layout/` (rename) + `passes/types/mod.rs` (786 LOC split)
   should land together; doing the rename without the split leaves a
   god module under a new name.

4. **`crates/core/` fracture is the largest Pass A surgery** and depends
   on the IR rename + path triplet landing first. Synthesizer must
   sequence: Lock 11 promotion → Lock 14 retirement → Lock 2 rename + Lock
   13 split → path triplet → core fracture.

5. **Pass B residues**: the per-grammar runtime cohort (5 trivial grammars
   ×7 files each) wants the cohort-template generator (Agent A.5 new
   facility #5); Pass B owns the runtime-side surgery; Pass A's contri-
   bution is the metadata schema.
