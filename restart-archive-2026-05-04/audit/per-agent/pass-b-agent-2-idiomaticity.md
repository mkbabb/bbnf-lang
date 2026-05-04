# Pass B — Agent B.2 — Idiomaticity (precepts adherence)

Date: 2026-05-03
Lens: Apply every precept under `docs/precepts/` to every Pass-B file.
Scope source: `docs/restart/PASS-B-CODEGEN-MID.md` §Pass B Scope.

The catalog below names violations by precept; per-file rows surface
the line-witness; the verdict column is left absent (synthesis-orchestrator's
task).

Critical foci per the directive: direct-to-struct, no-orthogonal-codepaths,
system-cohesion, gestalt approach, single-codegen-path.

---

## §1 — direct-to-struct (every emit shape MUST produce typed enums + parse fns; Tape / OpenFrame / variant-strip residue is fault)

### 1.1 — OpenFrame is alive in 6 files; the central pathology

`crates/core/src/runtime/json/builder.rs:61-87` defines `OpenFrame<'p>`
with four heap-bearing variants (Array, Object, Pair, Wrap) and is the
86.07% samply-share root cause per RESTART-SKETCH §A.4. The pathology
recurs in:

| File:line | Symbol | Fault |
|---|---|---|
| `crates/core/src/runtime/json/builder.rs:60-87` | `enum OpenFrame<'p> { Array, Object, Pair, Wrap }` | open-frame stack with `Vec`-bearing variants; `JsonStructCheckpoint::stack: Vec<OpenFrame>::clone()` at :243 deep-clones on every speculative entry |
| `crates/core/src/runtime/css_l4/builder.rs` | 14-variant `OpenFrame` (Declaration, Color, ColorFunction, ColorMix, SelectorList, HexColor, etc.) | god module per CENSUS §5; OpenFrame variant explosion is the layout-projection deferred-to-runtime fault |
| `crates/core/src/runtime/google_sheets/builder.rs` | `OpenFrame` | same shape |
| `crates/core/src/runtime/bbnf/builder.rs` | `OpenFrame` + bounds-recording extension | same shape with W1.9 compound-bounds tweak |
| `crates/core/src/runtime/builder_template.rs:97` | template's `layout: StructLayout` field | the trivial cohort uses the same template; OpenFrame is the leaf shape across all 9 grammars |
| `crates/core/src/runtime/google_sheets/arena.rs` | OpenFrame mention | per-grammar OpenFrame |

The direct-to-struct invariant says: every typed-record write at
codegen lands directly into the parent's typed field. There is no
heap stack of "open compounds awaiting completion" — the call stack
holds the partial state per RESTART-SKETCH §B.1 Layer 3. OpenFrame's
existence proves the codegen has *not* projected types directly; it
defers the typed materialisation to runtime via a runtime trait
surface (`StructBuilder`).

### 1.2 — StructLayout is a runtime literal reconstruction

`crates/core/src/grammar/generated/json.rs:1512-1518` (and 42 sites
across all grammars) reconstructs the `__layout` literal at every
parse-fn entry — `String::from("object")` + `Vec::new()` for fields
+ `TypeDesc::Span`. Per RESTART-SKETCH §A.4 #1, the type info is
known at codegen and is being re-built at runtime. `feedback_no-value-discard`
plus `feedback_typed-materialization-invariant` say every `->` reaches
the typed emitter; here the codegen *projects* the `->` but then
*re-builds* the projection at runtime through a trait method.

### 1.3 — `EmissionTier` / "fast path" / "scalar fallback" residue

| File:line | Match | Fault |
|---|---|---|
| `backend/driver/alt.rs:242` | `let fb_branches: Vec<_> = fallback_indices` | "fallback indices" is alt-dispatch fallback; legitimate compiler decision per CENSUS §3 — not fault |
| `backend/rust/emitter/shapes/dispatcher/support.rs:15` | `(NEON / AVX2 / scalar fallback)` | doc-comment narrative; KEEP |
| `backend/rust/emitter/shapes/wrap/struct_direct.rs:198` | `linear-try fallback` | one of two emit-path branches in wrap; `feedback_no-orthogonal-codepaths` violation |
| `crates/core/src/lower/value_expr/simple_kinds.rs:185` | "Defensive fallback" | Pass-A scope but worth carrying — fault per CENSUS §10.4 |
| `crates/core/src/backend/rust/emitter/shapes/unordered.rs:288` | "Defensive fallback" | fault per CENSUS §10.4 |
| `crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` | repeated "fallback" comments | INVESTIGATE per CENSUS §3 |

---

## §2 — no-orthogonal-codepaths (multiple code paths for "emit an Alt" / "emit a Repeat" are fault; the cost-model decides between paths but they share substrate)

### 2.1 — The struct_direct sub-modules are an orthogonal codepath

`feedback_no-orthogonal-codepaths` says ONE substrate; pluggable
decisions inside it. The current shape has:

| File | Shape |
|---|---|
| `backend/rust/emitter/shapes/keyword/struct_direct.rs` | 534 LOC of "struct-direct keyword emit" |
| `backend/rust/emitter/shapes/flat/struct_direct.rs` | 1033 LOC of "struct-direct flat emit" |
| `backend/rust/emitter/shapes/wrap/struct_direct.rs` | 622 LOC of "struct-direct wrap emit" |
| `backend/rust/emitter/shapes/pratt/struct_direct.rs` | 364 LOC of "struct-direct pratt emit" |

The naming alone (`struct_direct.rs` sub-module) implies a separate
codepath from a non-struct-direct emit. Lock 1 says tape is dead;
struct-direct is the only path. The `struct_direct.rs` sub-modules
should fold into their parent (`keyword/mod.rs`, `flat/mod.rs`, etc.)
as the single emit path.

### 2.2 — `parse_with` legacy-path lowering

CENSUS §4.1 documents the legacy-path lowering at four parse_with
files: `runtime/{bbnf, json, css_l4, google_sheets}/parse_with.rs`.
Each manually lowers the typed `path::ir::Path` down to `runtime::path::Path`
because the runtime documents' path-query traits consume the legacy
alphabet. Two `PathSegment<'a>` definitions co-exist — fault.

### 2.3 — Pipeline.rs file-form + pipeline/ directory

`crates/core/src/pipeline.rs` (103 LOC facade) + `crates/core/src/pipeline/`
(directory). `feedback_directory_modules` says splits use directory
modules, not flat siblings. The current shape has both — `pipeline.rs`
is the file-form module exporting facade types, while `pipeline/` is
the actual pipeline. Fault per CENSUS §4.3.

### 2.4 — Linear-try fallback inside wrap shape

`backend/rust/emitter/shapes/wrap/struct_direct.rs:198` "linear-try
fallback (the byte-dispatch arms are still" — the wrap shape has two
emit paths inside one file: byte-dispatch + linear-try-fallback. The
cost-model decision is *which path to take*; the substrate underneath
should be one. Fault.

### 2.5 — Alt-dispatch cross-shape fallback

`backend/rust/emitter/shapes/dispatcher/cross_shape.rs:118` "legacy
Alt-dispatch body (pre-W4 pattern preserved for" — two Alt-dispatch
emit paths inside the same dispatcher; the legacy pattern was meant
to retire at W4 but persists. Fault.

---

## §3 — system-cohesion (codegen + runtime + optimisers must share types; multiple `Layout` definitions are fault)

### 3.1 — Two `PathSegment<'a>` definitions

| File | Definition |
|---|---|
| `crates/core/src/runtime/path.rs:32-42` | `pub enum PathSegment<'a> { Field(&'a str), Index(usize) }` |
| `crates/core/src/path/ir.rs:42-57` | `pub enum PathSegment<'a> { Field(&'a str), Index(usize), Wildcard, VariantName(...) }` |

Two definitions; runtime/<g>/parse_with.rs mechanically lowers the
typed alphabet to the legacy alphabet. Fault per CENSUS §4.1.

### 3.2 — `StructLayout` import via `bbnf_ir::registry`

`crates/core/src/runtime/builder.rs:32` and 9 per-grammar `builder.rs`
files import `bbnf_ir::registry::StructLayout`. The codegen consumes
the same type via `crates/core/src/backend/rust/emitter/registry_emit.rs`.
No duplicate definition — but the runtime's `StructBuilder` trait
takes `&StructLayout` references at runtime entry, while the codegen
embeds the registry as static data. The cohesion is partial: the type
is shared, but the runtime treats it as runtime data while codegen
treats it as compile-time data. Per RESTART-SKETCH §A.4 #1, this is
the "type info known at codegen, re-built at runtime" pathology.

### 3.3 — `Emitter::Output` default vs override asymmetry

`backend/emitter.rs` defines `Emitter::Output: Default` (line 31) so
"per-node `emit_*` methods covering Leaves / Seq / Alt / Repeat / Ref
/ Binary / Value-manipulation / Ws-trim / Token-dispatch carry default
impls returning `Self::Output::default()` for backends whose parse-emission
path is dead. The Rust backend routes `parse()` through `dta_run`
wholesale and discards per-rule bodies at `emit_rule_function_impl`".

This is `feedback_no-orthogonal-codepaths` × `feedback_kiss-perf-bias`:
the trait carries 30+ methods, of which 27 are unimplemented for Rust
because the Rust path doesn't walk the IR per-node — it walks at the
shape-dispatcher level. TS + WASM walk per-node. **Three different
walking strategies sharing one trait surface is system-cohesion fault.**

### 3.4 — Dual `EmitStrategy` mirror

`crates/ir/src/registry/strategy.rs:130-185` `PRODUCTION_MANIFEST_TABLE`
mirrors `[workspace.metadata.bbnf-strategy]` in workspace `Cargo.toml`.
The xtask reads the TOML at regen time; the IR also carries it as a
hardcoded const. Two sources of truth, kept in sync by a synthetic
test. Per `feedback_one-codegen-path` and `feedback_no-orthogonal-codepaths`,
this should be xtask-only (read manifest, splice into emitter) — not
also baked into IR.

---

## §4 — gestalt approach (workarounds, fallbacks, "scalar fallback for SIMD", "Vec fallback for arena" are fault unless gated by the cost-model)

### 4.1 — The "scalar fallback" in dispatcher/support.rs

`backend/rust/emitter/shapes/dispatcher/support.rs:15` "(NEON / AVX2
/ scalar fallback)" — the SIMD detection is correct (Lock 10 honoured)
but the framing is "fallback". Per `feedback_no-workarounds`, the
scalar path is the *correctness oracle* per simd-scan's posture; the
naming should reflect that.

### 4.2 — `parse_number_fallback` in generated CSS L4 + JSON

`crates/core/src/grammar/generated/json.rs:667, 1357, 1365` —
`fn parse_number_fallback(bytes: &[u8]) -> f64` — fallback path for
edge-case numbers `lexical-core` rejects. CENSUS §3.2 calls KEEP but
demands rename to `parse_number_lexical_overflow`. The fallback isn't
a workaround; it's an alternate decoder for a domain `lexical-core`
explicitly doesn't cover.

### 4.3 — `_fallback` underscored params in Emitter trait

`backend/emitter.rs:96, 125, 332, 469` carry `_fallback: ...`
underscored params on emitter trait methods. CENSUS §3.1 calls them
out as fault: "the underscore prefix says they're unused. Either
consume the fallback or remove the parameter". Fault.

### 4.4 — `Defensive fallback` in shapes/unordered.rs

`backend/rust/emitter/shapes/unordered.rs:288` "Defensive fallback: a
malformed Unordered rule under" — `feedback_no-workarounds` says
defensive fallbacks are workarounds. CENSUS §10.4 calls FAIL-EXPLICIT.
Fault.

### 4.5 — `feedback_no-silent-epsilon` violations in pipeline

Per CENSUS §3, `crates/core/src/grammar/host.rs:387` "keyword-strip
wildcard fallback" violates `feedback_no-silent-epsilon`. The grammar
host is Pass A scope, but the pattern recurs in Pass B at:

- `runtime/google_sheets/arena.rs:38, 40, 103, 153` — fallback arena
  arms; INVESTIGATE per CENSUS §3.1
- `runtime/css_l4/builder.rs:713` "without a parsed unit fall through
  to unitless" — silent unitless fallback; INVESTIGATE per CENSUS

---

## §5 — single-codegen-path (ONE codegen path; Lock 1: direct-to-struct only; combinator fallback / tape fallback / EmissionTier residue are fault)

### 5.1 — The Emitter trait carries 27 unused methods

Per §3.3 above. The Rust backend routes parse() through one path
(driver::analysis::prepare_grammar → emit_grammar → shape-dispatcher)
and uses default impls for 27 of 30 trait methods. The trait is
designed for *three* walking strategies (per-node IR walk, shape-dispatcher
walk, full driver walk). Per `feedback_no-orthogonal-codepaths`, this
is fault — one walker, one trait, one substrate.

### 5.2 — Substrate selection in shapes/substrate.rs

`backend/rust/emitter/shapes/substrate.rs` (119 LOC) — explicit
"substrate selection (struct-direct vs combinator)" code path. Per
Lock 1 + `feedback_no-orthogonal-codepaths`, struct-direct is the
only path; substrate selection should not exist. The 119 LOC encodes
a vestigial decision.

The doc-comment cite at :70-73 "Lock 13 (No silent fallback), the
resolver does NOT route a parse..." is the correct anti-fallback
narrative. The substrate "decision" exists only to assert struct-direct;
the existence of the decision file is the fault.

### 5.3 — `feedback_one-codegen-path` violations across emitter

Per the precept: ONE monolithic codegen path; no combinator fallback;
ONE regex system. The grep evidence:

| File | Violation |
|---|---|
| `backend/rust/emitter/shapes/wrap/struct_direct.rs:198` | "linear-try fallback" — two paths inside wrap |
| `backend/rust/emitter/shapes/array/mod.rs:35` | "legacy record stream fallback" — two paths inside array |
| `backend/rust/emitter/shapes/dispatcher/cross_shape.rs:118` | "legacy Alt-dispatch body (pre-W4 pattern preserved for" — two paths inside cross-shape dispatch |

### 5.4 — `runtime/builder.rs` doc-comment carries "selection between tape and struct" narrative

`runtime/builder.rs:7` "selection between tape and struct happens at
codegen time" — narrative is stale per CENSUS §1.2. Tape is dead per
Lock 1; the selection no longer happens. Yet the doc-string asserts
the bifurcation, framing the trait as *pluggable between two substrates*
when only one survives. Fault.

---

## §6 — `feedback_no-god-modules`

Per CENSUS §5 the >500-LOC files outside generated/ are 23. Pass-B
god modules (subset of CENSUS §5):

| File | LOC | Suggested split |
|---|---:|---|
| `backend/rust/emitter/shapes/flat/struct_direct.rs` | 1033 | per leaf-projection family |
| `backend/rust/emitter/shapes/dispatcher/support.rs` | 902 | per dispatch-helper family |
| `backend/rust/emitter/regex_scan_adapter.rs` | 786 | HIR-to-DFA / transition table / byte-class hoisting |
| `backend/rust/emitter/shapes/wrap/struct_direct.rs` | 622 | byte-dispatch / linear-try / MapExpr projection |
| `runtime/css_l4/builder.rs` | 1014 | per-OpenFrame-variant module under `builder/<variant>.rs` |
| `runtime/css_l4/value.rs` | 852 | per typed-value family (color, length, selector, declaration, function-call, at-rule) |
| `runtime/css_l4/document.rs` | 541 | typed-value walking / child-iteration |
| `backend/emitter.rs` | 566 | trait surface / default-impl glue / dispatch helpers |
| `backend/rust/emitter/shapes/keyword/struct_direct.rs` | 534 | per keyword-payload variant |
| `backend/rust/emitter/shapes/array/mod.rs` | 514 | prefix-classifier / per-element loop |
| `xtask/src/regen.rs` | 849 | manifest-read / IR-pipeline-runner / emit-formatter / file-write |

---

## §7 — `feedback_no-god-directories` (Lock 13)

### 7.1 — `crates/core/src/runtime/` is the archetype god directory

11 immediate children (counting trailing `/`):

```
crates/core/src/runtime/
├── arena_template.rs       (generic mechanism)
├── bbnf/                   (per-grammar)
├── bnf/                    (per-grammar)
├── builder.rs              (generic mechanism — trait)
├── builder_template.rs     (generic mechanism — template)
├── css_l4/                 (per-grammar)
├── css_pretty/             (per-grammar)
├── csv/                    (per-grammar)
├── ebnf/                   (per-grammar)
├── error.rs                (generic mechanism)
├── google_sheets/          (per-grammar)
├── handle.rs               (generic mechanism)
├── json/                   (per-grammar)
├── math/                   (per-grammar)
├── mod.rs                  (aggregator)
├── path.rs                 (generic mechanism — path alphabet)
└── view.rs                 (generic mechanism)
```

Lock 13 explicit text: "A 16-sibling directory mixing per-grammar
subdirs with generic mechanism files (e.g., today's
`crates/core/src/runtime/`) is a god directory and is a fault." This
is the cited archetype; 9 per-grammar subdirs + 8 mechanism files.

### 7.2 — `crates/core/src/backend/rust/emitter/shapes/` is sibling-API divergent

Per §B.1.h Notes — the shape modules carry mixed sub-API. This is
Lock 13 sibling-API divergence.

### 7.3 — `crates/core/src/grammar/generated/` is data, not god directory

10 files (9 grammars + mod.rs); each is xtask-emitted. Per `feedback_clean-regen-discipline`,
generated files are clean output. Not a god directory — but the
asymmetric `pub use bbnf::*` aggregator at `mod.rs:35` is fault per
CENSUS §3.1.

---

## §8 — `feedback_no-workarounds`

Across the Pass-B substrate, the catalog of workaround markers (CENSUS
§3 aggregates):

- `fall through` / `fallthrough` / `fall-through` — 60 hits in src/
- `legacy` — 12 hits in src/
- `shim` — 8 hits in src/
- `fallback` — 70 hits (mixed: some legitimate, some fault)
- `for now` / `temporary` — 3 hits
- `TODO` / `FIXME` / `HACK` (non-generated) — 5 hits

The **shim** mentions in Pass-B (per CENSUS §3.1):
- `crates/core/src/backend/ts/projection.rs:113` "`declare function …`
  shim emitted at the top" — INVESTIGATE
- `crates/core/src/backend/rust/emitter/grammar.rs:4` "`emit_rule_function_impl`
  is retained as an empty shim" — DELETE
- `crates/core/src/backend/types/mod.rs:5-7` "lone non-shim survivor"
  — DELETE meta-language
- `crates/core/src/backend/mod.rs:4-8` "the only non-shim file" —
  DELETE meta-language

The **legacy** mentions in Pass-B:
- `backend/kernels/charclass.rs:32` "legacy `emit_call`" — DELETE
- `backend/kernels/prefix_class.rs:21-23, 42` "legacy `emit_call`...
  fall through to Unrecognized" — DELETE
- `backend/rust/emitter/shapes/dispatcher/cross_shape.rs:118` "legacy
  Alt-dispatch body" — DELETE
- `backend/rust/emitter/shapes/keyword/struct_direct.rs:85` "legacy
  `push_leaf_with_unit()`" — DELETE
- `runtime/bbnf/arena.rs:220` "legacy emission paths" — INVESTIGATE
- `runtime/bbnf/view.rs:206` "legacy emission paths" — DELETE

---

## §9 — `feedback_clean-instrumentation`

Pass-B substrate carries no `eprintln!` macros for instrumentation
(verified by ~grep). The trace recording pattern via `&mut dyn TraceSink`
in `crates/ir/src/passes/transform/{inline,fuse}.rs` is the canonical
shape — not Pass B scope.

xtask's `regen.rs` (849 LOC) carries `println!` for status output but
no `eprintln!` — KEEP.

---

## §10 — `feedback_clean-regen-discipline`

`crates/core/src/grammar/generated/` is xtask emit; no hand-edits.
Regen discipline: `cargo xtask regen` writes per-grammar files; `--check`
diffs against tempdir.

CENSUS §10.4 catches several emitter-source patterns that survive
into generated files (`linear-try fallback (the byte-dispatch arms
are still` — 8 generated files; `parse_number_fallback` — 5 generated
files). These are emitter-source TODOs that landed in generated files
verbatim. The discipline is honoured *mechanically* (no hand-edits)
but the source comments riding through to generated files violate
the *spirit* of `feedback_no-metalanguage-docs` — the generated file
should not carry the emitter-source's TODO narrative.

---

## §11 — `feedback_directory-module-structure`

Per CENSUS §4.3: `pipeline.rs` (file-form) + `pipeline/` (directory)
co-exist. Fault.

The runtime per-grammar dirs (e.g. `runtime/json/`, `runtime/google_sheets/`)
carry `mod.rs` per directory module discipline — honoured.

The `backend/rust/emitter/shapes/` sub-modules carry mixed shapes
(some directory-form with `mod.rs` + `struct_direct.rs`, some single-file
flat siblings). Fault per Lock 13.

---

## §12 — `feedback_no-inline-tests`

Per CENSUS §7, eight inline `#[cfg(test)]` blocks survive in src/:

| Path:line | Pass scope |
|---|---|
| `crates/core/src/path/cursor.rs:313-314` | Pass B (path is shared but the runtime-cursor-engine spec at PASS-B §B.2 covers it) |
| `crates/core/src/path/executor.rs:65-66` | Pass B (per-path executor) |
| `crates/core/src/path/schema.rs:130-131` | Pass B |
| `crates/core/src/runtime/google_sheets/parse_with.rs:83-84` | Pass B |
| `crates/core/src/runtime/css_l4/parse_with.rs:84-85` | Pass B |
| `crates/core/src/runtime/bbnf/parse_with.rs:99-100` | Pass B |
| `crates/core/src/runtime/json/parse_with.rs:105-106` | Pass B |
| `crates/core/src/backend/rust/analysis/inline/mod.rs:37` | Pass B |

All eight in Pass-B scope. Mechanical move per CENSUS §7.

---

## §13 — `feedback_no-backward-compat`

Several "back-compat" mentions in Pass-B substrate, each fault per
the precept:

- `crates/core/src/grammar/generated/mod.rs:14-23` — "BBNF aggregator
  for back-compat with consumers" — CENSUS §3.1 calls FAIL-EXPLICIT.
- `runtime/bbnf/view.rs:28-33` — "discriminator (replaces the tape-era
  `variant_idx`)" — narrative cites tape; KEEP code, scrub doc.

---

## §14 — `feedback_pluggable-components`

The `StructBuilder` trait (`runtime/builder.rs`) IS pluggable: trait +
template + per-grammar instance. Per CENSUS §4.2: "Three layers is
JUSTIFIED — the trait + template + per-grammar instance is the right
shape". KEEP the architecture.

The `Emitter` trait (`backend/emitter.rs`) is *over*-pluggable: 30
methods, of which 27 are unused for Rust. Fault per `feedback_kiss-perf-bias`:
plug only what *needs* plugging.

---

## §15 — `feedback_kiss-perf-bias`

Per the precept: "Optimization plans propose the smallest set of
changes that achieve elegance + performance; reject sprawling 4-lever
/ 8-PR sweeps".

The Pass-B substrate violates this in several places:

- `OpenFrame` + `StructBuilder` trait + `JsonStructCheckpoint`
  + `<g>StructCheckpoint` × 9 is a 4-layer machine to do what
  RESTART-SKETCH §B.2 shows in a 25-line `parse_object` function.
- `Emitter` trait with 30 methods + `EmitStrategy::for_grammar` +
  manifest-mirror + sub-trait `Output: Default` is over-pluggable
  for one in-tree consumer.
- `pipeline.rs` facade over `pipeline/` directory is two layers
  for a single linear pipeline.

---

## §16 — `feedback_isomorphic-api` and language-cohesion locks

The Rust + TS + WASM emitters share the driver but emit independently
(per CENSUS §B.1.g/h/i). Per `feedback_isomorphic-api`, "Rust PyO3
bindings must mirror Python API signatures exactly". The Pass-B
parallel is "TS / WASM emitters must mirror Rust emitter signatures
exactly". The current shape has each backend with its own emit_<shape>
sub-modules (e.g. `backend/ts/alt.rs`, `backend/wasm/alt.rs`) — uniformity
is *partial*; some emit functions are mirrored verbatim, some diverge
in arity.

The grep evidence: TS emit-shape files total ~1719 LOC; WASM ~1408
LOC. The Rust shape-emitter total is ~9000 LOC. The 5× asymmetry
implies decisions are being made in Rust that TS and WASM either
don't make or make differently.

---

## §17 — Summary table by precept

| Precept | Fault count in Pass B | Severity |
|---|---:|---|
| direct-to-struct (Lock 1, no OpenFrame) | 6 builder files, 9 per-grammar runtimes | **critical** |
| no-orthogonal-codepaths | 4 emit-path bifurcations, 1 pipeline split | **critical** |
| system-cohesion (single Layout, single PathSegment) | 2 PathSegment defs, 1 manifest mirror | high |
| gestalt approach (no fallback) | 8 fallback-narrative sites | medium |
| single-codegen-path (Lock 1) | substrate.rs + 4 substrate-selection sites | high |
| no-god-modules (>500 LOC) | 11 god modules in Pass B | high |
| no-god-directories (Lock 13) | 1 archetype (runtime/) + 1 shape sub-API divergence | **critical** |
| no-workarounds | ~85 marker hits | medium (most legitimate) |
| clean-instrumentation | clean | — |
| clean-regen-discipline | honoured mechanically; emitter-source TODOs leak | low |
| directory-module-structure | 1 violation (pipeline.rs) | medium |
| no-inline-tests | 8 violations | medium (mechanical) |
| no-backward-compat | 1 BBNF aggregator asymmetry | medium |
| pluggable-components | over-plug on Emitter trait | medium |
| KISS perf-bias | OpenFrame + builder trait + checkpoint stack | high |
| isomorphic-API across backends | partial uniformity | medium |

The most consequential idiomaticity faults — **OpenFrame, StructBuilder
trait surface, runtime/ god directory, struct_direct sub-module
proliferation, pipeline.rs/pipeline split** — all converge on one
question: **does direct-to-struct land as the codegen output, or as
a runtime substrate?** Today, codegen emits a runtime trait surface
that *simulates* direct-to-struct by deferring typed materialisation
to a runtime builder; this is the residue Lock 1 was meant to eliminate.
