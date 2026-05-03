# Pass B — Agent B.5 — Replacement Design

Date: 2026-05-03
Lens: For every ABROGATE-REPLACE item, design the new facility. Plus brand-new items.
Source: `docs/restart/PASS-B-CODEGEN-MID.md` §Methodology — Agent B.5.

The directive lists 8 candidate brand-new items; each is treated below
with the rubric **name | location | API sketch | locks honoured |
migration sequence**. Plus the ABROGATE-REPLACE candidates from the
inventory + lock-adherence + idiomaticity passes.

---

## §1 — `bbnf-runtime-template` — per-grammar runtime emission

**Name**: `bbnf-runtime-template` (or sub-module of `xtask` per Q4 of
Agent B.4 §Q5).

**Location**: `xtask/src/runtime_template/` — sibling of
`xtask/src/regen/`. The runtime template runs at the same regen
trigger; outputs land at `crates/<grammar>/src/runtime/` per Lock 14.

Alternative: `crates/bbnf-runtime-template/` — separate generator
library that xtask path-deps. Cleaner separation; allows third-party
grammars to share. Default: in xtask for fewer crates.

**API sketch**:

```rust
// xtask/src/runtime_template/mod.rs
use bbnf_ir::{GrammarIR, registry::StructRegistry};

pub struct RuntimeTemplateRequest<'a> {
    pub ir: &'a GrammarIR,
    pub registry: &'a StructRegistry,
    pub grammar_ident: &'a str,        // e.g. "json"
    pub marker_struct: &'a str,        // e.g. "JsonParser"
    pub output_path: &'a Path,         // crate's src/runtime/
    pub specialisation: SpecialisationLevel,
}

pub enum SpecialisationLevel {
    /// Trivial cohort: template emits everything (BNF, CSV, EBNF, CSS Pretty, Math)
    Canonical,
    /// Specialised cohort: template emits canonical surface; declaration crate
    /// extends via Rust trait impls or per-grammar `<g>::specialised` module
    /// (BBNF, CSS L4, Sheets, JSON)
    CanonicalPlusExtension,
}

pub struct RuntimeTemplateOutput {
    /// `<g>Value` enum — typed projection of the grammar's typed leaves
    pub value_module: String,
    /// `<g>Document` struct + `<g>PathQuery` impl
    pub document_module: String,
    /// `<g>View` cursor
    pub view_module: String,
    /// `<g>Kind` enum (compound kinds)
    pub kind_module: String,
    /// `<g>Arena` slab arena
    pub arena_module: String,
    /// Concrete `<g>Builder` — direct-projection (no OpenFrame stack per Lock 1)
    pub builder_module: String,
    /// `<g>::mod` re-export hub
    pub mod_module: String,
}

pub fn emit(req: RuntimeTemplateRequest<'_>) -> RuntimeTemplateOutput;
```

The template walks the IR's StructRegistry. For each rule:
- `LayoutKind::Struct` → emit a struct-shape projection
- `LayoutKind::TaggedEnum` → emit an enum with per-variant payloads
- `LayoutKind::Newtype` → emit a transparent wrapper
- `LayoutKind::Scalar` → contribute to the `<g>Value` typed-enum

The builder module uses **direct-projection** per RESTART-SKETCH §B.2:
parent struct fields filled from child parses; no `OpenFrame` heap
stack; the call stack is the depth.

**Locks honoured**:
- **Lock 14** — zero hand-written per-grammar runtime files in generic substrate
- **Lock 1** — direct-projection emit (no OpenFrame, no checkpoint clone)
- **Lock 9** — slice-borrow primary; bumpalo via opt-in
- **Lock 6** — output committed under per-grammar declaration crates

**Migration sequence**:
1. Land template skeleton (proc-macro2 + quote, ~400 LOC)
2. Land trivial cohort emit (BNF, CSV, EBNF, CSS Pretty, Math); validate against existing tests
3. Land specialised-cohort emit canonical surface; declaration crates re-export
4. Audit specialised-cohort extensions against template canonical
5. Retire `crates/core/src/runtime/<g>/` directories
6. Retire `runtime/builder_template.rs`, `runtime/arena_template.rs` (subsumed)
7. Update consumers (`bbnf-bootstrap`, `bbnf-parse` re-exports)

---

## §2 — `bbnf-codegen-IR` — typed IR shared across Rust + TS + WASM

**Name**: `bbnf-codegen-IR` — but per Phase-4 BC.W0 22-variant table,
this is more naturally **`bbnf-ir`'s post-Layout-lowering surface**
which is already shared.

The codegen-IR distinction is between:
- **Source-IR** — `IrNode` (Alt, Seq, Repeat, Ref, Lit, Regex, Wrap,
  Map, Host) — what `lower/` emits; what optimisation passes consume
- **Codegen-IR** — typed IR after layout lowering — what `bbnf-codegen`
  consumes

Today the bridge is `bbnf_ir::types::TypeDesc` + `StructRegistry` +
`StructLayout`. The 22-variant table at `docs/tranches/BC/audit/W0-typed-ir-variant-table.md`
specifies the typed IR shape post-lowering.

**Location**: lives within `bbnf-ir`; the codegen consumes it via the
typed sub-API.

**API sketch**:

```rust
// crates/ir/src/typed_ir.rs (or post-lowering pass's output)
pub enum TypedIrNode {
    Scalar { ty: TypeDesc, source: ScalarSource },
    Struct { fields: Vec<TypedField>, source: StructSource },
    Enum { variants: Vec<TypedVariant>, source: EnumSource },
    Sequence { element: Box<TypedIrNode>, repeat: RepeatKind },
    Reference { rule_id: RuleId, ty: TypeDesc },
    Map { input: Box<TypedIrNode>, fn_ref: HostFnRef, output: TypeDesc },
    // ... per Phase-4 BC.W0 22-variant table
}

pub trait TypedIrConsumer {
    fn consume(&mut self, node: &TypedIrNode, ctx: &mut Self::Ctx);
}
```

Each emitter (`RustEmitter`, `TsEmitter`, `WasmEmitter`) implements
`TypedIrConsumer`.

**Locks honoured**:
- **Lock 5** — codegen emits backend-agnostic typed IR; per-backend lowerers consume it
- **Lock 14** — the typed IR carries no grammar-named variants

**Migration sequence**:
1. Land Layout lowering pass (Pass A scope)
2. Land typed IR variant table per BC.W0
3. Migrate Rust codegen to consume typed IR (today consumes IrNode + types side-by-side)
4. Migrate TS + WASM codegen
5. Retire any direct `IrNode` consumption in `bbnf-codegen`

---

## §3 — `Emitter` trait — `RustEmitter` / `TsEmitter` / `WasmEmitter`

**Name**: `Emitter` (already exists at `backend/emitter.rs:31`).

**Location**: `crates/bbnf-codegen/src/emitter.rs` post-extraction
per Agent B.4 §Q1.

**API sketch** (reshaped per Agent B.4 §Q7 option (a) — per-IrNode walk):

```rust
pub trait Emitter {
    type Output: Default;
    type Ctx;

    fn emit_grammar(&mut self, ir: &GrammarIR, ctx: &mut Self::Ctx) -> Self::Output;

    fn emit_rule(&mut self, rule: &IrRule, ctx: &mut Self::Ctx) -> Self::Output;

    fn emit_node(&mut self, node: &TypedIrNode, ctx: &mut Self::Ctx) -> Self::Output;

    // per-shape leaves (driver pre-resolves shape, calls one of these)
    fn emit_alt(&mut self, branches: Vec<Self::Output>, dispatch: AltDispatch, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_seq(&mut self, parts: Vec<Self::Output>, ty: TypeDesc, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_repeat(&mut self, body: Self::Output, kind: RepeatKind, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_ref(&mut self, rid: RuleId, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_lit(&mut self, value: &str, guaranteed_byte: Option<u8>, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_regex(&mut self, regex_id: usize, ir: &GrammarIR, ctx: &mut Self::Ctx) -> Self::Output;
    fn emit_map(&mut self, input: Self::Output, fn_ref: HostFnRef, ctx: &mut Self::Ctx) -> Self::Output;
}
```

The current Emitter trait at 30 methods reduces to 8-10 by collapsing
default-impl-then-override patterns; the shape-dispatcher's logic
moves *into* the per-shape methods.

**Locks honoured**:
- **Lock 5** — IR + per-backend lower; one walking pattern
- **Lock-system-cohesion** — backends share one walking shape

**Migration sequence**:
1. Reshape Emitter trait per Agent B.4 §Q7
2. Migrate Rust shape-dispatcher logic into per-shape methods
3. Verify TS + WASM path through new methods
4. Retire `backend/rust/emitter/shapes/` sub-tree as per-shape methods replace it

---

## §4 — Cohort template generator

**Name**: subsumed by §1 — `bbnf-runtime-template`. Per Phase-4 gap D
the cohort template generator is the proc-macro2 + quote mechanism
that emits per-grammar runtime modules.

No separate facility; folded into §1.

---

## §5 — `bbnf-bench` — vitest-style bench harness separate from xtask

**Name**: `bbnf-bench`.

**Location**: `crates/bbnf-bench/` (workspace member; per `feedback_vitest-bench`
in tree).

**API sketch**:

```rust
// crates/bbnf-bench/src/lib.rs
pub fn bench<F: Fn() -> R, R>(name: &str, f: F);

// per-grammar bench:
// crates/json/benches/parse.rs
fn main() {
    bbnf_bench::bench("json::parse_twitter", || {
        JsonParser::parse(TWITTER_JSON)
    });
    bbnf_bench::bench("json::parse_twitter_get_text", || {
        JsonParser::parse_with(TWITTER_JSON, &PATH_STATUSES_0_TEXT)
    });
}
```

The bench harness wraps `criterion` or rolls a small SOTA-comparable
harness. Per Lock 8 + `feedback_vitest-bench`, the harness is uniform
across grammars; bench definitions live per-grammar.

**Locks honoured**:
- **Lock 8** — bench gates name SOTA competitors per dataset
- **Lock 13** — benches live per-grammar; no central god directory

**Migration sequence**:
1. Land `bbnf-bench` crate skeleton
2. Migrate existing benches (per `crates/lsp/benches/bench_lsp.rs`, etc.)
3. Add per-grammar SOTA bench rows (sonic-rs twitter parity, etc.)

---

## §6 — `bbnf-cost-model` — cost-model crate

**Name**: `bbnf-cost-model`.

**Location**: subsumed by `egraph` per Lock 4 + Lock 11.

The directive's question is whether cost model lives in egraph
(extraction-side) or as separate crate. Today it lives in `egraph/src/cost_config.rs`,
`cost_weights.rs`, `extract.rs` — total ~400 LOC.

Per `feedback_general-infra-crates` "general-purpose constructs
(e-graphs, cost models) in own crate(s)", the cost model COULD split
into `bbnf-cost-model`. But:

- the cost model's dependencies are domain-specific (StructDirect,
  SubstrateBinding, AltStrategy, etc.); the separation is artificial
- per `feedback_kiss-perf-bias`, smallest set of changes wins; keep
  cost model in egraph

**Default disposition**: KEEP cost model in egraph.

The synthesis revisits only if a non-egraph consumer arrives.

---

## §7 — `bbnf-pratt` — Pratt LUT propagation + auto-detection

**Name**: `bbnf-pratt`.

**Location**: subsumed by `bbnf-codegen` per Lock 10 + cohesion.

Today Pratt logic lives in:
- `crates/core/src/backend/rust/emitter/precedence.rs` (274 LOC)
- `crates/core/src/backend/rust/emitter/shapes/pratt/{mod,struct_direct}.rs` (411 LOC)
- pattern miners in `crates/ir/src/passes/recognizers/operator_chain.rs` (Pass A)

Per Lock 10: "Optimizer mines grammar shape (left-recursive operator
chains → Pratt) ... Cost model decides when SIMD overhead is worth
the dispatch cost." Pratt is one fork of the auto-detection; SIMD is
the other.

The Pratt logic is well-cohered with the codegen substrate; extracting
into `bbnf-pratt` adds a crate boundary without subtraction. Default:
KEEP in `bbnf-codegen` per `feedback_kiss-perf-bias`.

---

## §8 — `bbnf-simd-detect` — SIMD-eligible-leaf auto-detection

**Name**: `bbnf-simd-detect`.

**Location**: subsumed by `simd-scan` + `bbnf-codegen` per Lock 10.

The SIMD-eligibility detection lives in `crates/ir/src/passes/recognizers/`
(Pass A — pattern_alphabet, charset, etc.) and `crates/simd-scan/src/alphabet.rs`
(KernelShape selector). The codegen consumes the recogniser's output
via `crates/core/src/backend/rust/emitter/regex_scan_adapter.rs` (786 LOC).

Per Lock 10, no separate detection crate is needed — the existing
substrate already auto-detects via grammar shape mining.

Default: NO new crate; KEEP existing substrate.

---

## §9 — Replace `OpenFrame` + `JsonStructCheckpoint` with direct-projection

**Name**: direct-projection emit.

**Location**: `bbnf-runtime-template` (per §1) emits the new builder
shape; the existing `OpenFrame` stacks retire.

**API sketch** — per RESTART-SKETCH §B.2:

```rust
// EMITTED per-grammar (no hand-written builder.rs):
pub fn parse_object<'p>(
    input: &'p [u8],
    p: &mut usize,
    arena: &mut JsonArena<'p>,
    state: &mut ScanState,
    cursor: &mut PathCursor<'_>,
) -> Result<JsonObjectId, ParseErr> {
    // byte-disjoint dispatch; no checkpoint
    if input.get(*p).copied() != Some(b'{') {
        return Err(ParseErr::syntax(*p));
    }
    *p += 1;
    skip_space(input, p, state);
    if input.get(*p).copied() == Some(b'}') {
        *p += 1;
        return Ok(JsonObjectId::EMPTY);
    }
    let mut pairs: SmallVec<[JsonPair<'p>; 8]> = SmallVec::new();
    loop {
        let key = parse_string_borrowed(input, p)?;
        skip_space(input, p, state);
        if input.get(*p).copied() != Some(b':') { return Err(ParseErr::syntax(*p)); }
        *p += 1;
        skip_space(input, p, state);
        let value = parse_value(input, p, arena, state, cursor)?;
        pairs.push(JsonPair { key, value });
        skip_space(input, p, state);
        match input.get(*p).copied() {
            Some(b',') => { *p += 1; skip_space(input, p, state); }
            Some(b'}') => { *p += 1; return Ok(arena.intern_object(pairs.into_vec())); }
            _ => return Err(ParseErr::syntax(*p)),
        }
    }
}
```

No `OpenFrame`, no `StructBuilder` trait surface, no `Checkpoint`,
no `Vec<OpenFrame>::clone` at every speculative entry. The call
stack carries depth; `SmallVec` carries pairs (capacity hint mineable
from grammar profile); the arena owns interned compound IDs.

**Checkpoint** for the speculative cases (Alt-of-non-disjoint, Repeat-with-fail):
```rust
struct Checkpoint {
    pos: usize,
    arena_arrays: u32,
    arena_objects: u32,
}
// O(1) capture; rollback truncates arena, rewinds pos
```

**Locks honoured**:
- **Lock 1** — typed-enum is the substrate; no runtime trait ceremony
- **Lock 9** — slice-borrow `&'p str` keys + values
- **Lock-perf** — RESTART-SKETCH §A.7 86.07% samply share retires

**Migration sequence**:
1. Land §1 (template) + §3 (Emitter reshape)
2. Emit direct-projection per-grammar via template
3. Validate behaviour parity against existing test suite
4. Retire `runtime/builder.rs` trait, `runtime/builder_template.rs`,
   per-grammar builders

---

## §10 — Replace `pipeline.rs` facade + `pipeline/` directory

**Name**: `pipeline/mod.rs` (single directory module).

**Location**: `crates/core/src/pipeline/mod.rs` (or `crates/bbnf-codegen/src/pipeline/mod.rs`
post-extraction).

**API sketch** — collapse the facade types + sub-module exports into
one mod.rs:

```rust
// crates/core/src/pipeline/mod.rs
pub mod compile;
pub mod directives;
pub mod validate;

pub use compile::{CompileTarget, CompileRequest, CompileOutput, CompileError, PipelineOptions};
pub use compile::compile_paths_request;
```

The current `pipeline.rs` (103 LOC) facade types fold into `compile/mod.rs`
or get exported via mod.rs. No new facility; structural fix.

**Locks honoured**:
- **Lock-feedback_directory_modules** — directory module not file-form sibling

**Migration sequence**:
1. Move `pipeline.rs` content into `pipeline/mod.rs`
2. Delete `pipeline.rs` file
3. Update consumers (xtask, lib.rs)

---

## §11 — Replace duplicate `PathSegment` definitions

**Name**: `crates/path/` consolidated path crate per Lock 7.

**Location**: `crates/path/` carries `PathSegment`, `Path`, `TypedPath`.
`crates/path-core/` carries shared lex/lower/validate logic between
the proc-macro and cdylib shells per CENSUS §4.1.

**API sketch** — single PathSegment alphabet across runtime + macro:

```rust
// crates/path/src/lib.rs
pub mod ir;
pub mod cursor;
pub mod executor;
pub mod schema;
pub mod ascent;
pub mod wildcard;

pub use ir::{PathSegment, Path, TypedPath};
```

`PathSegment` carries the typed alphabet (Field, Index, Wildcard,
VariantName per `crates/core/src/path/ir.rs`); `runtime/path.rs`
deletes; per-grammar `parse_with.rs` legacy lowering retires.

**Locks honoured**:
- **Lock 7** — consolidated path crate
- **Lock-system-cohesion** — single PathSegment alphabet

**Migration sequence**:
1. Land `crates/path/` with content from `crates/core/src/path/`
2. Land `crates/path-core/` factoring lex/lower/validate from `bbnf-path` + `bbnf-path-ts`
3. Update `bbnf-path` (proc-macro shell) + `bbnf-path-ts` (cdylib shell) to path-dep on path-core
4. Delete `runtime/path.rs`
5. Update per-grammar `parse_with.rs` to consume typed alphabet directly
6. Retire `LegacyPath` / `LegacySegment` lowering

---

## §12 — Replace `crates/css_types.rs` host shim

**Name**: per-grammar host module within `crates/<grammar>/`.

**Location**: `crates/css-l4/src/host.rs` carries `parse_hex_color`
(and other per-CSS-grammar host fns).

**API sketch**:

```rust
// crates/css-l4/src/host.rs
pub fn parse_hex_color(input: &str) -> u32 { /* ... */ }
```

Grammar source `grammar/css/l4/stylesheet.bbnf` declares:
```
@host parse_hex_color(input: &str) -> u32 from crate::host::parse_hex_color
```

The codegen splices `crate::host::parse_hex_color` into the per-grammar
parse fn; the per-grammar declaration crate provides it.

**Locks honoured**:
- **Lock 14** — zero grammar-specific host fns in `crates/core/src/`

**Migration sequence**:
1. Create `crates/css-l4/` declaration crate
2. Move `parse_hex_color` from `crates/core/src/css_types.rs` to `crates/css-l4/src/host.rs`
3. Update grammar source's `-> parse_hex_color` map annotation
4. Update codegen's host-fn resolution path

---

## §13 — Aggregate of brand-new + ABROGATE-REPLACE items

| Item | Type | Location |
|---|---|---|
| `bbnf-runtime-template` | brand-new | `xtask/src/runtime_template/` (subsumed) |
| `Emitter` trait reshape | ABROGATE-REPLACE | `bbnf-codegen/src/emitter.rs` (post-extract) |
| Direct-projection emit | ABROGATE-REPLACE | per-grammar declaration crates (template emit) |
| `bbnf-bench` | brand-new | `crates/bbnf-bench/` |
| `pipeline/mod.rs` collapse | ABROGATE-REPLACE | `bbnf-codegen/src/pipeline/mod.rs` |
| `crates/path/` + `crates/path-core/` | ABROGATE-REPLACE | new crate; `runtime/path.rs` retires |
| `crates/<grammar>/src/host.rs` | brand-new | per-grammar declaration crates |
| Per-grammar declaration crates | brand-new | `crates/<json,css-l4,bbnf,sheets,bnf,csv,ebnf,css-pretty,math>/` |
| `crates/bbnf-codegen/` | brand-new (extracted) | from `crates/core/src/backend/` |
| `crates/bbnf-runtime/` | brand-new (extracted) | from `crates/core/src/runtime/` mechanism files |
| Promote egraph + egraph-derive + csp-solver | KEEP-MODIFY | crates.io publish |
| Split `xtask/src/regen.rs` god module | KEEP-MODIFY | per Agent B.4 §Q5 |

The 6 brand-new items + 6 ABROGATE-REPLACE items represent the full
scope of Pass-B's structural redress. No items beyond these emerge
from the inventory + idiomaticity + lock-adherence + architectural-transposition
passes.

The synthesis sequences these per dependency:
1. Layout lowering (Pass A) lands first
2. `crates/bbnf-codegen/` extracts
3. `bbnf-runtime-template` lands within xtask
4. Per-grammar declaration crates scaffold
5. Direct-projection emit replaces OpenFrame across all 9 grammars
6. `crates/path/` + `crates/path-core/` consolidate
7. egraph + csp-solver promote to crates.io
8. Pipeline + xtask god-module splits

The dependency arrow guarantees no half-state — each step's output
is the next step's input.
