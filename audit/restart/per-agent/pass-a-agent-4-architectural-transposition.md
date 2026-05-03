# Pass A — Agent A.4 — Architectural Transposition

Date: 2026-05-03
Lens: macro-level restructuring proposals where elegance / simplicity /
performance suffer under the current shape. No per-file classification;
this lane proposes whole-system rewrites.

---

## Proposal 1 — Fracture `crates/core/` into a parser-front family

### Current state

`crates/core/` is the workspace's largest non-generated crate by an order
of magnitude. Its src/ tree carries (file count from §A.1, total LOC ~9 K
hand-written + ~169 K generated):

```
crates/core/src/
  lib.rs                          ← public hub
  types.rs                        ← AST + RuleEntry surface
  css_types.rs                    ← grammar-named host fn at root (Lock 14)
  imports/                        ← @import resolution
  lower/                          ← BbnfView → GrammarIR
  path/                           ← typed-path machinery
  grammar/                        ← BBNF parser entry + schema + generated/
  pipeline.rs + pipeline/         ← compile pipeline (directory + sibling file)
  generate/                       ← codegen entry
  backend/                        ← per-backend emitters
  graph/                          ← SCC + deps graph
  runtime/                        ← per-grammar Document/Value/View/Builder/...
```

The crate root mixes parser-front (lower / pipeline / imports), middle-
pipeline (generate / backend), and runtime (runtime). Cargo.toml describes
it as "BBNF grammar parser and code generator built on parse_that"; in
practice the crate carries every compile-time artefact AND every runtime
artefact in a single workspace member.

`crates/core/Cargo.toml`:
```toml
[package]
name = "bbnf"
description = "BBNF grammar parser and code generator built on parse_that"
```

The crate is named `bbnf` (the public library name); its src/ surfaces
sub-modules that are themselves separable libraries.

### Proposed shape

Three sibling workspace crates, all consumed by a thin `bbnf` aggregator:

```
crates/
  bbnf-parse/        ← source acquisition + parsing + lowering
    src/
      lib.rs         ← public hub
      source/        ← source acquisition (today's imports/ + new "rope/streaming abstraction" hook)
      grammar/       ← BBNF parser entry + schema (today's grammar/ source side)
      lower/         ← BbnfView → bbnf-ir GrammarIR
      pipeline/      ← compile entry; consumes lower's GrammarIR; produces ready-to-emit
  bbnf-codegen/      ← codegen + per-backend lowerers
    src/
      lib.rs
      driver/        ← shared codegen driver (today's backend/driver/)
      rust/          ← Rust emitter (today's backend/rust/)
      ts/            ← TS emitter (today's backend/ts/)
      wasm/          ← WASM emitter (today's backend/wasm/)
      generated/     ← committed xtask output (today's grammar/generated/, relocated)
  bbnf-runtime/      ← per-grammar generic substrate (path + scan support glue)
    src/
      lib.rs
      path/          ← path runtime executor (consumes path crate)
      scan/          ← simd-scan glue
  bbnf/              ← thin aggregator re-exporting parse + codegen + runtime
    src/lib.rs       ← `pub use bbnf_parse::*; pub use bbnf_codegen::*; pub use bbnf_runtime::*;`
```

Per-grammar runtime (Documents, Values, Builders) emits into separate
generated modules under `bbnf-codegen/src/generated/<g>/` (Pass B scope);
the runtime crate carries only the grammar-agnostic substrate (path
runtime, scan glue, error types).

Plus one workspace member per grammar that needs a host fn:

```
crates/
  bbnf-grammar-css-l4/    ← CSS L4 host fn (today's css_types::parse_hex_color)
                            optional decl crate per Lock 14 footnote
```

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 13 (no god directories) | the 11-child crate root partitions into three concern-disjoint crates; each new crate carries 4-6 sub-modules at top level |
| Lock 14 (full grammar generalisation) | per-grammar host code lives in per-grammar declaration crates; the parser/codegen/runtime crates carry zero grammar-named code |
| Lock 5 (IR + per-backend lower) | `bbnf-parse` produces the IR; `bbnf-codegen` consumes it; the IR contract is explicit (path-dep boundary makes it greppable) |
| Lock 11 (path-deps for incubating sister crates) | the workspace boundary is the path-dep boundary; bringing `parse-that` and `bbnf-regex` into the workspace as path-deps becomes natural |
| Lock 7 (consolidated path crate) | `bbnf-runtime` consumes `crates/path/` rather than carrying `crates/core/src/path/` |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| Crate split | ~0 (relocations, not deletes) | YES — the public API surface fragments. Consumers must update `bbnf::*` paths to `bbnf_parse::*`, `bbnf_codegen::*`, `bbnf_runtime::*` (or keep the `bbnf` aggregator). |
| `crates/core/src/grammar/generated/` relocation to `bbnf-codegen/src/generated/` | move ~169 K LOC | regen invariant survives; xtask-emitted byte-identical output |
| Per-grammar declaration crates (only css-l4 needs one today) | +1 crate | no — additive |

Sister tranche carries:
- Pass B's codegen lives in `bbnf-codegen` (relocates from `crates/core/src/backend/`)
- Pass B's runtime per-grammar emit lives under `bbnf-codegen/src/generated/<g>/` per-grammar sub-modules (relocates from `crates/core/src/runtime/<g>/`)
- Pass C's xtask + workspace metadata survive

This is the largest single-pass surgery in the restart. It is a Lock 13 +
Lock 14 prerequisite for every later step.

---

## Proposal 2 — Fracture `crates/ir/` into IR-types / IR-passes / IR-vm

### Current state

`crates/ir/` carries (~145 files, ~17 K LOC):

```
crates/ir/src/
  lib.rs                  ← re-export hub + bbnf-regex re-exports
  cost_config.rs          ← cost knobs
  types/                  ← IR data definitions (GrammarIR, IrNode, ...)
  registry/               ← StructLayout + StructRegistry + EmitStrategy
  dag/                    ← hash-cons DAG over IR
  egraph/                 ← grammar-tier e-graph (rules, analysis, write-back)
  recognizer/             ← unified recognizer trait
  rewrites/               ← rewrite-rule storage (RuleSet, schema, rank, tiering)
  passes/                 ← every IR transformation pass (the largest sub-tree)
  vm/                     ← bytecode VM + interpreter
```

Two tensions:

1. The crate carries IR data + IR passes + a bytecode VM. The VM is a
   runtime concern — execution of programs against an input — orthogonal
   to the grammar-IR-the-data and the IR-pass-pipeline that produces it.
2. Per Lock 14, the IR types must be backend-agnostic; the registry's
   `PRODUCTION_MANIFEST_TABLE` and the `EmitStrategy::StructDirect.rust`
   field both leak Rust-specific paths into the IR crate. The IR crate's
   public surface should be backend-agnostic.

### Proposed shape

Three (or four) sibling workspace crates:

```
crates/
  bbnf-ir/               ← IR data definitions only
    src/
      lib.rs
      types/             ← GrammarIR, IrNode, IrRule, MapExpr, FnDescriptor, Layout (renamed from TypeDesc)
      dag/               ← hash-cons DAG
      registry/          ← LayoutRegistry (renamed from StructRegistry), abstract EmitStrategy
      cost_config.rs     ← cost knobs
  bbnf-passes/           ← every transformation pass that consumes/produces GrammarIR
    src/
      lib.rs
      sets/              ← FIRST/FOLLOW/SCC/dispatch-table generation
      transform/         ← inline/fuse/prune/fuse_token/optimize/pattern_dedup
      recognizers/       ← pattern miners (grammar_facts, operator_chain, ...)
      layout/            ← (renamed from types/) — the canonical IR pass per Lock 2
      payload/           ← payload-layout calculation
      materialization/   ← classify + lattice + pin_sweep
      audit/             ← payload-coverage audit (renamed; minus the named-arm enum)
      egraph/            ← grammar-tier e-graph (consumes egraph crate; emits IR rewrites)
      patterns/          ← pattern-miner aggregators
      csp_strategy/      ← CSP-driven strategy synthesis
      csp_domains.rs
      facts/             ← fact-mining hub
      inspect/           ← read-only inspectors
      profile.rs
  bbnf-vm/               ← bytecode VM + interpreter (today's crates/ir/src/vm/)
    src/
      lib.rs
      bytecode.rs
      compiler/
      interpreter/
      debug.rs
```

Optional fourth crate:

```
crates/
  bbnf-egraph-rules/     ← grammar-tier e-graph rewrite rules (today's crates/ir/src/egraph/rules/, ~1.1 K LOC)
                            consumes the generic `egraph` crate; emits IrNode rewrites
                            rationale: rules are extensible substrate per Lock 4
```

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 4 (per-domain orthogonal optimization) | the bbnf-passes crate is the explicit pass-pipeline substrate; e-graph and CSP each occupy their own sub-tree, output-piped per Lock 4 |
| Lock 2 (Layout canon) | `bbnf-ir/src/types/` carries `Layout`, not `TypeDesc`; `bbnf-passes/src/layout/` is the canonical pass |
| Lock 5 (IR + per-backend lower) | `bbnf-ir` carries no Rust-specific paths; the path strings move out (relocation per Proposal 1) |
| Lock 13 (no god directories) | the largest god module today (`passes/csp_strategy/mod.rs`, 1361 LOC) splits into `csp_strategy/{solver_wiring, domains, materialization_glue}.rs`; `recognizers/grammar_facts.rs` (1530 LOC) splits per fact-family |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| `crates/ir/` → `bbnf-ir` + `bbnf-passes` + `bbnf-vm` | ~0 (relocations) | YES — public API fragments. The `bbnf-ir` aggregator could re-export bbnf-passes and bbnf-vm during the transition. |
| Lock 2 vocabulary rename | ~50 sites | YES — `TypeDesc` becomes `Layout`; `StructLayout` becomes `Layout`; `TypeMap` retires. Every consumer (codegen, lower, audit, recognizer) renames. |
| God-module splits | ~0 (file shuffling) | no — internal |

Sister tranche carries:
- Pass B's codegen consumes `bbnf-ir` instead of `crates/ir/`
- Pass C's xtask consumes the new manifest format

---

## Proposal 3 — Path crate triplet shape

### Current state

Three places hold path-related code (per CENSUS §4.1):

| Crate / Module | LOC | Purpose |
|---|---:|---|
| `crates/bbnf-path/` (proc-macro) | 918 | `path!(...)` macro lifting path literal into `TypedPath<G, T>` |
| `crates/bbnf-path-ts/` (cdylib) | 1012 | TS/wasm twin; lex/lower/validate logic mirrored verbatim from `bbnf-path/src/path_macro.rs` |
| `crates/core/src/path/` | 1300+ | runtime typed-path types + executor + cursor + ascent + wildcard + ... |

`crates/core/src/runtime/path.rs` (legacy, 163 LOC) holds the older
borrowed `PathSegment<'a> { Field(&'a str), Index(usize) }` alphabet that
runtime documents' `*PathQuery` traits consume. The `parse_with.rs` files
manually lower the typed alphabet down to the legacy alphabet — a known
duplicate per the BA W3 plan.

### Proposed shape (Lock 7 explicitly names it)

```
crates/
  path-core/             ← non-proc-macro shared types + lex/lower/validate
    src/
      lib.rs
      ir.rs              ← PathSegment, Path, TypedPath, OwnedPathSegment, IntoPathSegment
      schema.rs          ← PathSchema, GrammarMarker
      type_check.rs      ← check_path, check_path_against_registry
      error.rs           ← PathError, PathErrorReason
      lex.rs             ← path-string lexer (today's bbnf-path/src/path_macro.rs lex section)
      lower.rs           ← typed-path lowering (today's lower section)
      validate.rs        ← typed-path validation (today's validate section)
      runtime/           ← runtime executor (cursor + executor + ascent + wildcard + variant_select)
                            (relocates from crates/core/src/path/)
  path/                  ← Rust proc-macro shell (path-dep on path-core)
    src/
      lib.rs             ← #[proc_macro] pub fn path(...) → consumes path-core
      registry.rs        ← workspace-metadata-driven RegistryDescriptor lookup (Lock 14)
  path-ts/               ← TS cdylib shell (path-dep on path-core)
    src/
      lib.rs             ← #[wasm_bindgen] compile_path / execute_path → consumes path-core
      schema.rs          ← TypedPathPayload / OwnedSegmentPayload / PathErrorPayload (wire types)
      template_tag.rs    ← TS shim string
```

This is the Lock 7 proposed shape verbatim; it eliminates:
- the ~500 LOC mirror between `bbnf-path/src/path_macro.rs` and
  `bbnf-path-ts/src/compile.rs`
- the synthetic fixture registries in `bbnf-path/src/registry.rs` and
  `bbnf-path-ts/src/fixture.rs`
- the duplicate `PathSegment<'a>` alphabet at `crates/core/src/runtime/path.rs`
  (legacy) vs `crates/core/src/path/ir.rs` (typed)
- the manual typed-path → legacy-path lowering in
  `crates/core/src/runtime/<g>/parse_with.rs` (4 files, ~480 LOC)

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 7 (consolidated path crate) | the proposed triplet IS Lock 7's named shape |
| Lock 13 (no god directories) | the 639-LOC `path_macro.rs` splits into per-phase files (lex / lower / validate / emit) |
| Lock 14 (full grammar generalisation) | the registry consumes workspace metadata; no per-grammar match-arms; markers relocate to per-grammar declaration crates |
| KISS / DRY | one source of truth for lex / lower / validate; no proc-macro vs cdylib mirror |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| Crate restructure | ~−500 LOC mirror eliminated; ~+200 LOC of plumbing | YES — `bbnf::path::*` reaches consumer code; the move to `path::*` re-exported from `bbnf::path` keeps the consumer surface |
| Markers relocation | ~30 LOC moves to per-grammar crates | no — additive |
| Legacy alphabet deletion | ~163 LOC at `crates/core/src/runtime/path.rs` deleted | no — runtime documents adopt the typed alphabet |

---

## Proposal 4 — `parse-that` workspace promotion

### Current state

`parse-that` lives at `/Users/mkbabb/Programming/parse-that/rust/parse_that/`
as a sibling repo. The bbnf-lang workspace consumes it via versioned dep
where used (most consumption is via grammar/host extraction's `use
parse_that::Span` and codegen-internal); `parse-that` is NOT a workspace
member.

The sibling repo carries:
```
parse-that/rust/
  parse_that/         ← combinator library (the named consumer)
  regex/              ← bbnf-regex (NFA→DFA + HIR; its own crate)
  bootstrap/          ← parse_that's own dev binaries
  src/                ← (legacy / ?)
```

Lock 11: "egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as
path-deps in workspace until each API stabilises."

### Proposed shape

Bring `parse-that` and `bbnf-regex` into the workspace as path-deps:

```
crates/
  parse-that/           ← relocated from /Users/mkbabb/Programming/parse-that/rust/parse_that/
                          (or sub-module / git-submodule reference)
  bbnf-regex/           ← relocated from /Users/mkbabb/Programming/parse-that/rust/regex/
                          (sibling of parse-that within the same source repo today; co-relocates)
```

Then update consumer Cargo.toml entries:
- `crates/ir/Cargo.toml` → `bbnf-regex = { version = "0.1", path = "../bbnf-regex", features = ["serde"] }`
- `crates/bbnf-path/Cargo.toml` + `crates/bbnf-path-ts/Cargo.toml` → same
- `crates/core/Cargo.toml` (where it consumes `parse_that::Span`) → `parse-that = { version = "0.4", path = "../parse-that" }`

Once each API stabilises, restore registry-version dep + remove from
workspace per Lock 11's "promote to registry once stable" clause.

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 11 (path-deps for incubating sister crates) | exactly this lock's mandate |
| Pass B's runtime + codegen | gain greppable, in-tree access to `parse-that` Span types and `bbnf-regex` HIR |
| KISS | one workspace covers the parse-front toolchain |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| Workspace move (or git-submodule) | LOC stays where it lives; add workspace member declaration | no |
| Cargo.toml dep-form switches | ~6 dep entries change | no — same crate name + version |

The sibling-repo retention of `parse-that` is acceptable IF a git-submodule
pinning is added; the user's working pattern with `docs/precepts/` is
exactly this — sibling-repo submodule pinned by SHA. Recommend: same
pattern for `parse-that` + `bbnf-regex` if relocating in-tree is too
invasive.

---

## Proposal 5 — Grammar source tree layout

### Current state

```
grammar/
  bbnf/
    bbnf.bbnf
    expressions.bbnf       ← split file (sub-grammar)
    types.bbnf             ← split file (sub-grammar)
  json/
    json.bbnf
  css/
    pretty.bbnf
    l4/                    ← sub-directory for CSS L4 (multi-file grammar)
      stylesheet.bbnf      ← cited in workspace metadata as the entry
      ...
  google-sheets/
    google-sheets.bbnf
  ebnf/
    ebnf.bbnf
  bnf/
    bnf.bbnf
  misc/
    csv.bbnf
    math.bbnf
    math-ambiguous.bbnf    ← test fixture (ambiguous form)
    g4.bbnf                ← test fixture
    regex.bbnf             ← test fixture
    emoji.bbnf             ← test fixture
    json-commented.bbnf    ← test fixture
  tests/
    google-sheets-formula-test-cases.md
```

Two tensions:

1. The `misc/` directory mixes production grammars (csv, math) with test
   fixtures (g4, math-ambiguous, regex, emoji, json-commented). The
   workspace metadata cites `grammar/misc/csv.bbnf` and `grammar/misc/math.bbnf`
   as production paths.
2. CSS L4 has its own subdirectory because it is a multi-file grammar (per
   `grammar/bbnf/`'s pattern, but on a larger scale). Other multi-file
   grammars (BBNF) sit at the per-grammar dir level. The pattern is
   inconsistent.

### Proposed shape

```
grammar/
  bbnf/
    bbnf.bbnf
    expressions.bbnf
    types.bbnf
  json/
    json.bbnf
  css-l4/                  ← rename from css/l4/ for top-level uniformity
    stylesheet.bbnf
    ...
  css-pretty/              ← rename from css/pretty.bbnf to dedicated dir
    pretty.bbnf
  google-sheets/
    google-sheets.bbnf
  ebnf/
    ebnf.bbnf
  bnf/
    bnf.bbnf
  csv/                     ← lift from misc/
    csv.bbnf
  math/                    ← lift from misc/
    math.bbnf
  fixtures/                ← rename from misc/; pure test fixtures
    g4.bbnf
    math-ambiguous.bbnf
    regex.bbnf
    emoji.bbnf
    json-commented.bbnf
  tests/                   ← keep as-is
    google-sheets-formula-test-cases.md
```

One per-grammar directory per grammar; one `fixtures/` directory for test
sources only.

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 13 (no god directories; cohesive encapsulation) | every directory partitions one cohesive concern; the production-vs-fixture distinction is structural, not lexical |
| Lock 14 (full grammar generalisation) | per-grammar dir is the per-grammar declaration boundary; matches the per-grammar declaration-crate pattern |
| KISS | one rule for layout — one dir per grammar |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| Directory renames + lifts | 5 paths change in workspace metadata | no — workspace metadata updates with the renames |
| Test fixtures relocate | ~5 paths change in test files | no |

This is the smallest-surgery proposal in the lane. Recommend: ratify.

---

## Proposal 6 — `crates/bootstrap/` retirement

### Current state

`crates/bootstrap/src/lib.rs` is a 28-LOC re-export shim:
```rust
pub use bbnf::grammar::generated::BbnfBootstrap;
```

Plus three dev binaries (`dump_ir`, `cost_grid_sweep`, `debug_parse`)
totalling ~440 LOC.

Per Lock 14 + the proposed crate split (Proposal 1), `bbnf::grammar::generated::BbnfBootstrap`
relocates to `bbnf-codegen::generated::bbnf::BbnfBootstrap` (or its post-
rename equivalent). The bootstrap crate becomes either:

(a) **Pure re-export shim**: lib.rs renames its single re-export to the
new path. The 28-LOC shim survives.

(b) **Dev-binary host**: lib.rs disappears; the three binaries move to a
workspace `xtask`-style crate or `bbnf-tools`.

### Proposed shape

Recommend **(b)**: retire `crates/bootstrap/` as a published library;
relocate the dev binaries to `xtask/` (where they fit thematically — they
already sit alongside `xtask regen`). The single re-export becomes
unnecessary if consumer code consumes `bbnf-codegen::generated::bbnf::BbnfBootstrap`
directly.

### Justification (locks honoured)

| Lock | How the proposal honours it |
|---|---|
| Lock 13 (no god directories) | one fewer crate; the dev binaries colocate with their build-tooling sibling |
| KISS | a 28-LOC shim crate is overhead; the rename either retires the shim or absorbs it |

### Migration cost

| Surface | LOC delta | Backward-incompat? |
|---|---|---|
| `crates/bootstrap/src/lib.rs` deletion | −28 LOC | YES — consumers of `bbnf_bootstrap::BbnfBootstrap` rename to the new path. Per the user's "no-backward-compat" precept, this is acceptable. |
| Bin-binary moves | 3 files change crate ownership | no — no public API |

Trade-off: keeping the shim crate as a pure re-export is the smaller
surgery. Synthesizer adjudicates.

---

## §7 — Roll-up of macro proposals

| # | Proposal | Locks honoured | LOC delta | Risk |
|---|---|---|---:|---|
| 1 | Fracture `crates/core/` into `bbnf-parse` + `bbnf-codegen` + `bbnf-runtime` + `bbnf` aggregator + per-grammar declaration crates | 5, 11, 13, 14 | ~0 (relocations) | high (largest surgery) |
| 2 | Fracture `crates/ir/` into `bbnf-ir` + `bbnf-passes` + `bbnf-vm` + (optional `bbnf-egraph-rules`) | 2, 4, 5, 13 | ~0 | medium (Lock 2 rename is large but mechanical) |
| 3 | Path crate triplet: `path-core` + `path` + `path-ts`; absorb `crates/core/src/path/` runtime; delete legacy alphabet | 7, 13, 14, KISS | −500 LOC mirror; −163 LOC legacy | low (Lock 7 is explicit) |
| 4 | Workspace promotion of `parse-that` + `bbnf-regex` (path-deps; submodule pinning) | 11 | no LOC change | low |
| 5 | Grammar source tree: per-grammar dirs uniformly; `fixtures/` for test sources | 13, 14 | no LOC change | low (smallest surgery) |
| 6 | Retire `crates/bootstrap/` (move dev binaries to xtask; eliminate re-export shim or fold into bbnf-codegen) | 13, KISS | −28 LOC | low |

### Recommended ordering

The synthesizer should execute (and sequence into BA waves):

1. **Proposal 5** (grammar source tree) — smallest, lowest-risk, foundational
2. **Proposal 4** (parse-that / bbnf-regex workspace promotion) — Lock 11 mandate; foundational dep-form
3. **Proposal 3** (path crate triplet) — Lock 7 mandate; eliminates 500-LOC mirror
4. **Proposal 6** (bootstrap retirement) — clean-up; small
5. **Proposal 2** (IR crate fracture + Lock 2 rename) — large mechanical pass
6. **Proposal 1** (core crate fracture) — largest; depends on 2 (IR rename) + 3 (path consolidation)

Steps 1–4 are independent; steps 5–6 sequence on the IR rename landing.
Step 6 is the foundation of the new tranche set's W0 wave.

---

## §8 — What the proposals do NOT touch

The lane intentionally does NOT propose:

- **A unified hypergraph** (CSP + e-graph fused) — Lock 4 explicitly forbids
- **A new IR layer for TS/WASM** beyond the existing per-backend lower —
  Lock 5 honours the current shape
- **A combinator-layer rewrite of the hot parse path** — Lock 1 + the
  generated/* substrate is the post-AX shape; combinators stay as
  codegen-time tooling per the SOTA survey
- **Re-introducing tape under any name** — Lock 1 forbids; sonic-rs and
  lightning-css both win without it

These are out-of-scope for Pass A; they are settled.
