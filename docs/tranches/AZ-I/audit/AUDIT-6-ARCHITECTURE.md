---
title: AZ-I.W2-substrate close — Architecture audit (Agent 6/6)
order: 460
section: BBNF
---

# AZ-I.AUDIT.6 — Codebase architecture audit

**Lens.** Module / crate / boundary structure. The user's mandate:
*architectural transpositions in the sake of elegance, simplicity, and
performance above all are both necessary and desirable*. This audit
catalogues where the post-W2-substrate workspace deviates from that
principle and proposes the ten transpositions that recover it. Synthesis
into `W2-CLOSE-AUDIT.md` is the sole consumer.

Methodology — every claim cites grep / find / wc -l output captured in
the audit worktree. The W2 substrate landed `EmitStrategy`,
`StructBuilder`, `JsonStructBuilder` / `JsonDocument`, and
`StructRegistry` as `GrammarIR.struct_registry`; activation reverted to
`TapeDirect` for every grammar (W2.md §Reversal, `crates/core/src/
backend/rust/emitter/strategy.rs:118-147`). The orphan-abstraction
exposure that close ceremony introduces is the audit's anchor.

---

## 1. Crate-graph health

The workspace declares 13 members (`Cargo.toml:1-3`):

```
crates/core, crates/analysis, crates/ir, crates/lsp, crates/ser,
crates/gorgeous, crates/bootstrap, crates/egraph, crates/egraph-derive,
crates/csp-solver, crates/tape, crates/simd-scan, crates/json-prototype
```

Path-patched externals from `.cargo/config.toml:34-46`: `pprint`,
`pprint_derive`, `parse_that`, `bbnf-regex`. Patched by
`[patch.crates-io]`, not `[workspace.members]` — they are owned per the
SPEC §Crate ownership invariant but compile in their own repo trees.

**Dep-edge survey** (one-line `grep` per Cargo.toml; results
cross-checked against `use` site counts):

| Crate           | Direct deps                                                                  | One-direction consumers           |
|-----------------|------------------------------------------------------------------------------|-----------------------------------|
| `bbnf` (core)   | bbnf-ir, bbnf-ser, tape, simd-scan, egraph, csp-solver, pprint, parse_that  | gorgeous, analysis, lsp           |
| `bbnf-ir`       | bbnf-regex, csp-solver, egraph, egraph-derive, parse_that                   | bbnf, analysis, lsp, gorgeous(opt)|
| `bbnf-analysis` | bbnf, bbnf-ir                                                                | lsp                               |
| `bbnf-lsp`      | bbnf, bbnf-ir, bbnf-analysis                                                 | (binary)                          |
| `bbnf-ser`      | ryu, itoa                                                                    | bbnf (codegen-only via generated) |
| `gorgeous`      | bbnf, bbnf-ir(optional via `vm`)                                             | (binary + downstream)             |
| `bbnf-bootstrap`| bbnf, bbnf-ir                                                                | (xtask regen)                     |
| `egraph`        | csp-solver                                                                   | bbnf-ir                           |
| `egraph-derive` | proc-macro                                                                   | bbnf-ir (dev-deps only)           |
| `csp-solver`    | (none)                                                                       | bbnf, bbnf-ir, egraph             |
| `tape`          | (none)                                                                       | bbnf, simd-scan, json-prototype   |
| `simd-scan`     | tape                                                                         | bbnf                              |
| `json-prototype`| tape, simd-scan, parse_that                                                  | (bench harness, no lib consumer)  |

**Cycles.** None. Verified by `grep "name = \"" crates/*/Cargo.toml` →
`grep "= { path"` cross-walk; every edge points towards the leaves.

**Single-consumer crates** (fold-in candidates):

- **`json-prototype`**: zero non-bench consumers. `grep -rn
  "json_prototype::" crates/*/src` returns 10 hits, all doc-mirror
  comments (`/// Mirrors json_prototype::number::parse_number_body`),
  not real uses. Production role nil; bench-only. **Fold candidate.**
- **`egraph-derive`**: proc-macro sub-crate. Cannot fold (`proc-macro
  = true` libs require their own crate). Justified.
- **`simd-scan`**: sole non-test consumer is `bbnf` core via
  `runtime::scan` re-export. 154 LOC of per-arch SIMD kernels +
  feature-gated AVX-512. Build-flag concern justifies separation.
- **`bbnf-ser`** (530 LOC, no deps beyond ryu/itoa). Six call sites in
  `crates/core/src/generate/serialize/mod.rs`. Borderline — Serializer
  IS general-purpose (no BBNF knowledge) per `feedback_general-infra-
  crates`; keep separate.

**Justification.** `bbnf-ir` (439 core consumer sites, 167 elsewhere),
`csp-solver` (24 sites across IR + core + egraph), `egraph` +
`egraph-derive` (general-purpose with derive sub-crate) — all
legitimate per `feedback_general-infra-crates`. `tape` (16 modules,
3871 LOC) has two role-shift sections: `dta.rs` (80 LOC, §9.2) and
`visitor.rs` (746 LOC, §9.7) flagged for AZ-II.W2 sunset.

**The bbnf → bbnf-ir → bbnf-regex chain** is clean — no back-edges.
**Path-dep cross-repo boundary** honoured: `pprint`/`parse-that` live
in sibling repos; bbnf-lang reaches via `[patch.crates-io]`. Patch
does not cross-publish trait surfaces.

---

## 2. Module god-module audit

Per `feedback_no-god-modules`, every level (crate, module, file)
separates concerns. `wc -l` over `crates/*/src/**/*.rs` (excluding
`grammar/generated/`) yields these >800 LOC files:

| File                                                                 | LOC  | Role                                        | Recommendation |
|----------------------------------------------------------------------|------|---------------------------------------------|----------------|
| `crates/ir/src/passes/recognizers/dta.rs`                            | 1625 | DTA mining, summarise, lift; 9 public types | **SPLIT**: extract `dta/{builder.rs,profile.rs,summary.rs,types.rs,lift.rs}` directory module per `feedback_directory-modules` |
| `crates/core/src/backend/rust/emitter/grammar.rs`                    | 1432 | Grammar-level emit + projection admission   | **SPLIT**: extract `grammar/{impl_block.rs, projection.rs, parse_body.rs, materializer.rs}` directory module |
| `crates/ir/src/passes/csp_strategy/mod.rs`                           | 1273 | CSP strategy domain definitions             | **SPLIT**: extract `csp_strategy/{domain.rs, propagate.rs, components.rs, decisions.rs}` |
| `crates/core/src/backend/rust/view/value.rs`                         | 1272 | Value enum codegen for typed views          | **SPLIT**: extract `view/value/{collect.rs, emit.rs, dispatch.rs}` |
| `crates/core/src/pipeline/compile.rs`                                | 903  | Pipeline orchestration (parse→IR→backend)   | **KEEP** — naturally cohesive, single concern. |
| `crates/ir/src/passes/materialization/classify.rs`                   | 858  | Materialisation class CSP                   | **KEEP** — single concern (the CSP system) |
| `crates/core/src/backend/rust/emitter/dfa_codegen.rs`                | 842  | DFA codegen for regex inlining              | **KEEP** — single concern (DFA emission) |
| `crates/core/src/backend/rust/emitter/shapes/object.rs`              | 820  | Object-shape per-grammar emit               | **SPLIT**: extract `shapes/object/{tape.rs, struct_direct.rs, visitor.rs}` directory; the file already has 3 emission strategies tangled |
| `crates/core/src/backend/rust/emitter/shapes/arglist.rs`             | 789  | ArgList-shape per-grammar emit              | **SPLIT**: similar 3-strategy entanglement |
| `crates/core/src/backend/rust/emitter/shapes/hregex.rs`              | 786  | HRegex-shape (regex-inlined Wrap)           | **KEEP** — single shape concern |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`  | 759  | Per-grammar SIMD scan support module        | **KEEP** — see §2.1 below |

**§2.1 dispatcher 5-file split** (`shapes/dispatcher/{cross_shape:628,
support:759, ref_call:296, scan_policy:271, symbol_composition:63}`).
Justified — the asymmetry is structural (support emits SIMD
scaffolding; symbol_composition is pure ident builders). Holds.

**§2.2 `crates/ir/src/passes/types/`** (W2-extended): 1950 LOC across
5 files (mod 640, generate 403, registry 523, subvariants 181,
type_map 203). Cohesive; the registry.rs W1 addition is a pass
extension, not separate concern. Keep.

**§2.3 `crates/ir/src/registry/`** (W1-new): 37 LOC mod.rs + 372 LOC
struct.rs. Right-sized. Stylistic quirk: `mod r#struct;` (raw
identifier) — see proposal 8.7.

---

## 3. Trait surface health

**Trait inventory** — `grep -rn "^pub trait" crates/*/src` returns 35
hits. The full table is too long; condensed health summary below.

| Trait              | Implementors | Trait-bound generic consumers | Health   |
|--------------------|--------------|-------------------------------|----------|
| `Emitter`          | 3 (Rust/TS/Wasm)  | 13 driver generic fns           | Healthy  |
| `StructBuilder`    | 1 (JsonStructBuilder)  | 0 generic; concrete type        | **Watch** |
| `StructRegistryProbe` | 3 | 1 (audit_payload_coverage)         | Healthy  |
| `RecognizerMiner`  | 8 (consume_to_next, delim_scan, keyword_stats, pattern_alphabet, key_dispatch, quoted_string, identifier, disjoint_first) | 1 (mine_recognizers) | Healthy |
| `NamedTypeResolver` | 2 (NullResolver, RustNamedTypes) | 1 (compute_payload_layouts_with_resolver) | Healthy |
| `GrammarVisitor` + 6 shape sub-traits | 2 sets (TapeVisitor, ValueVisitor) at tape/src/visitor.rs:321-455 | per-grammar generated `V: <Visitor>` bounds | **Sunset per §9.7** |
| `JsonVisitor`      | 2 (ValueVisitor, TapeVisitor) | json-prototype only          | Local-scope orphan |
| `Serializer` / `Deserializer` | 2 | generated codegen | Healthy |
| egraph: 8 traits, csp-solver: 6 traits | per-domain | per-domain | Healthy general-infra |
| `Root` / `ValueRoot` / `PathQuery` | per-grammar generated | `Parsed<T: Root>` | Healthy |
| `IntoPathSegment`  | 4 (str/&str/usize/…) | `Path::push_*` | Healthy |
| `ColumnTag` / `Reducer<T>` | 4 reducers (Count/Sum*/Max*/Min*) | `reduce_column` | Healthy |

**Orphan-abstraction findings:**

1. **`StructBuilder` (1 implementor + 0 trait-bound generic
   consumers).** The trait's stated purpose at `runtime/builder.rs:38-
   41` admits this: *"the typical generated parse fn takes a concrete
   `&mut JsonStructBuilder<'_>` so LLVM monomorphises and inlines the
   trait calls."* No emitter takes `<B: StructBuilder>`; no driver
   generic fn takes a `B: StructBuilder` bound. Until a second concrete
   builder lands (Sheets W2.B or CSS L4 W3), the trait is overhead — a
   `pub trait` with one implementor and no generic consumer is an
   orphan abstraction per `feedback_pluggable-components`. **However**,
   the W2 substrate explicitly designs for the second + third
   implementor; collapse here would block the activation wave. Keep,
   but ensure W2-act lands within ≤1 wave or this transitions from
   "planned substrate" to "orphan abstraction".
2. **`StructRegistryProbe`.** 3 implementors, 1 consumer
   (`audit_payload_coverage`). Pluggable per the design; the three
   implementors discriminate the audit-pass's three legitimate readback
   modes (absent / present-via-PayloadLayouts / present-via-Registry).
   Justified.

---

## 4. Boundary-leak audit

Per SPEC `docs/instructions/README.md:474-514` Architecture invariants:
*"Each crate owns a responsibility; … Data crosses boundaries through
well-defined structs, not re-derivation."*

**bbnf-ir → bbnf re-exports.** `crates/core/src/runtime/mod.rs:7,8`
re-exports `types::*`; `crates/ir/src/lib.rs:24-46` exports
`StructRegistry`, `StructLayout`, `MapExpr`, `TypeDesc`, etc. The
`bbnf::runtime::tape` re-export at `runtime/mod.rs:56` is a **full
crate re-export** — generated code in `crates/core/src/grammar/
generated/json.rs:5692` reaches `tape::Tape<R>` via
`crate::runtime::tape::Tape::<…>`. Clean — generated code does not
require a `tape` cargo dependency in downstream consumers.

**bbnf-regex → bbnf re-exports.** `crates/ir/src/lib.rs:28-32`
exports `CharSet128` and the `regex_first` module. Clean delegation.

**`tape` → `bbnf` flagged for AZ-II.W2 deletion.**
REMAINING-TRAJECTORY.md §9.2 (DTA precedence), §9.7 (Visitor
re-exports). The DTA precedence helpers are 80 LOC at `crates/tape/src/
dta.rs` carrying 4 types (DtaStateId, DtaRuleId, DtaAssociativity,
DtaPrecedenceEntry). All four cross the boundary as `pub use
tape::{DtaPrecedenceEntry, DtaAssociativity, DtaRuleId, DtaStateId}` at
`crates/tape/src/lib.rs:80`. **The boundary is correct (data not
re-derivation), but the OWNER crate is wrong** — these belong in
`bbnf-ir` (the IR definition layer); `tape` exists per its file's stated
rationale (`dta.rs:39-43`) only because *"to avoid a crate dependency
edge from tape back into the IR for the precedence-table row."* Now
that struct-direct emission deprecates this dependency direction
entirely (W2 substrate routes Pratt through the visitor surface),
**hoist the 4 types to `bbnf-ir::dta`** and let `tape` consume them.

**`GrammarIR.struct_registry` field placement (W1-new).**
`crates/ir/src/types/grammar.rs:392`:
`pub struct_registry: crate::registry::StructRegistry,`. Right
ownership, **right field placement on `GrammarIR`** — `struct_registry`
is canonical IR-level data the IR's own `populate_struct_registry` pass
writes (`crates/ir/src/passes/types/registry.rs:97`) and downstream
backends read (Rust emitter at `runtime/builder.rs:60`). Clean. The
38-fixture impact concern in the brief is just the wave-1 propagation;
the architectural choice itself is correct.

**`EmitStrategy` placement (W2-new).** Lives at `crates/core/src/
backend/rust/emitter/strategy.rs:46-164`. Reads `&StructRegistry`
(`crates/ir/src/registry`). **Wrong ownership.** Per
`feedback_backend-agnostic-types`, *"each backend resolves to native
types"*; per the SPEC §Architecture invariants, *"data crosses
boundaries through well-defined structs"*. The current placement makes
`EmitStrategy::for_grammar` a Rust-backend-specific resolver, but
nothing about the strategy is Rust-specific — Sheets/CSS will need a
TS variant and a WASM variant if multi-backend struct-direct lands.
The strategy is *which substrate to emit into*; that decision is
backend-shared. **Hoist to `bbnf-ir::registry::strategy`** with the
`builder_path` / `document_path` fields generalised to a
`SubstrateBinding { rust: …, ts: …, wasm: … }` struct, OR keep the
backend-specific strings and split the resolver per-backend with the
common discriminator (`StructDirect | TapeDirect`) in `bbnf-ir`. The
former is cleaner.

---

## 5. Naming-consistency catalogue

**`&EmitStrategy` parameter position.** Survey via `grep -rn "strategy:
&EmitStrategy" crates/core/src/backend/rust/emitter/shapes` (24 hits).
Reading the surrounding context: every shape emitter places
`strategy: &EmitStrategy` as the **last** parameter after `ir:
&GrammarIR` (e.g. `shapes/scalar.rs:50-51`, `shapes/number.rs:43-44`,
`shapes/string.rs:54-55`, `shapes/object.rs:60-61`). The brief mentions
"two positional conventions" — the audit found no inconsistency in the
current snapshot; every site lands strategy after `ir`. **Consistency
holds**; the plan-time concern was preempted in execution. Cite
`shapes/array/mod.rs:122`, `shapes/wrap/mod.rs:231`, `shapes/keyword/
mod.rs:58`, `shapes/flat/mod.rs:97`, `shapes/alt_dispatch/mod.rs:63`,
`shapes/dispatcher/cross_shape.rs:60`, `shapes/dispatcher/cross_shape.
rs:35` (helper fn `dispatcher_builder_type(strategy: &EmitStrategy)
-> TokenStream` — strategy is first because there's only one parameter)
— all conventional.

**Other naming checks:**
- `EmitStrategy::for_grammar(grammar_ident, &registry)` vs
  `audit_payload_coverage(ir, &probe)` vs
  `populate_struct_registry(ir, &rule_types_for_registry, &type_map)` —
  the `for_grammar` constructor convention is consistent with Rust
  idiom (Rust uses `for_<context>` for derived constructors, e.g.
  `From::from`, `IntoIterator::into_iter`). Clean.
- `is_tape_direct() -> bool` vs `is_struct_direct() -> bool` (both at
  `strategy.rs:154-163`). Symmetric. Clean.

---

## 6. Macro / proc-macro surface

**Verification: `bbnf_derive` stays retired.** `find /Users/mkbabb/
Programming/bbnf-wt-az-i-audit-arch/crates -name "derive" -type d`
returns only `crates/egraph-derive`. **No `bbnf_derive` proc-macro
crate** in the workspace. Verified.

References to `bbnf_derive` in source:
- `crates/core/src/imports/loader.rs:105` — comment-only mention of the
  legacy proc-macro path.
- `crates/gorgeous/src/jit.rs:64,100,108` — JIT runtime path that
  consumes the **published** `bbnf_derive` crate from crates.io for the
  external user-facing JIT compile path. This is a **separate
  consumer** (the user-facing JIT), not a workspace member. Kept by
  design: gorgeous's JIT entrypoint expects a derive-macro-shaped
  user surface.

**Remaining proc-macros in workspace:** `egraph-derive` only
(`crates/egraph-derive/Cargo.toml:11: proc-macro = true`). 1 of 13
crates. Justified per the egraph crate's `#[derive(Language)]`
requirement for downstream e-graph consumers.

**`format_ident!` / `quote!` consumers.** 167 use sites in `crates/
core/src/generate/` (per `grep | wc -l`). All emission is direct
`TokenStream` building inside the codegen pipeline. No macro
indirection.

---

## 7. Substrate ownership

**`StructBuilder` trait.** Lives at `crates/core/src/runtime/
builder.rs:66`; consumed at `crates/core/src/backend/rust/emitter/
shapes/{number,scalar}.rs:247,143` via `use crate::runtime::builder::
StructBuilder as _;`. **Right ownership.** The trait IS Rust-runtime-
shaped (it returns `CompoundHandle`, takes `&str` lifetimes that the
JSON arena binds); generated Rust code consumes it. The "lives in core
but every emitter imports it" pattern is correct because **emitters
emit code that calls into core's runtime**.

**`audit_payload_coverage` lives in `bbnf-ir`.** `crates/ir/src/
passes/audit/payload_coverage.rs:243`. Consumed by `bbnf-core` via
`bbnf_ir::passes::audit_payload_coverage`. Right — the audit walks
`GrammarIR`'s rule graph; the IR layer owns the data being audited.

**`JsonStructBuilder` lives in `crates/core/src/runtime/json/
builder.rs`.** Consumed by emitted code at `crates/core/src/grammar/
generated/json.rs`. Right.

**`EmitStrategy` lives in `crates/core/src/backend/rust/emitter/
strategy.rs`.** **Wrong** — see §4 above. Cross-backend decision; lift
to `bbnf-ir::registry::strategy` so TS/WASM backends share.

**`tape::dta` precedence helpers.** Live at `crates/tape/src/dta.rs:
1-80`. **Wrong owner** — these are IR-level concepts (precedence
table, associativity); tape carries them only to avoid a back-edge
from tape→ir per the file's own rationale (lines 39-43). With the
post-AZ-I.W2 substrate routing Pratt through the visitor surface,
nothing in the tape crate actually needs these types as primitives;
hoist to `bbnf-ir::dta`.

---

## 8. Top-10 architectural transposition proposals

Ranked by elegance × performance × simplicity. Each: before / after /
mechanism / drag eliminated.

### 8.1 Hoist `EmitStrategy` to `bbnf-ir::registry::strategy`

**Before.** `crates/core/src/backend/rust/emitter/strategy.rs` (164
LOC) couples codegen-time substrate selection to the Rust backend.
TS/WASM backends would need parallel duplicates for Sheets/CSS.

**After.** `crates/ir/src/registry/strategy.rs` defines `enum
EmitStrategy { StructDirect { binding: SubstrateBinding },
TapeDirect }` where `SubstrateBinding` carries per-backend type paths.
Each backend has a thin resolver that reads the IR-level strategy and
emits its native types.

**Mechanism.** Move file. Generalize `&'static str builder_path` to
`SubstrateBinding { rust_builder, ts_builder, wasm_builder }` (or
better: a `BackendKey → String` map). The resolver
`for_grammar(grammar_ident, &registry)` becomes the IR-level decision
point.

**Drag eliminated.** Three backends × per-grammar duplication =
9 files for the substrate decision; collapsed to 3 backend-specific
binding records reading one IR-level decision. **Estimated drag: ~400
LOC over multi-backend rollout.**

### 8.2 Retire `tape::dta` — hoist to `bbnf-ir::dta`

**Before.** `crates/tape/src/dta.rs:1-80` defines `DtaStateId`,
`DtaRuleId`, `DtaAssociativity`, `DtaPrecedenceEntry` to *"avoid a
crate dependency edge from tape back into the IR for the
precedence-table row"* (`dta.rs:39-43`). Re-exported at `crates/tape/
src/lib.rs:80`.

**After.** `crates/ir/src/dta/mod.rs` owns the four types. Tape
consumes them as `bbnf_ir::dta::*`. Per AZ-II.W2 §9.2.

**Mechanism.** Move 80 LOC from `tape` to `bbnf-ir`; flip dep edge so
`tape` depends on `bbnf-ir` (already works — `bbnf-ir` does not depend
on `tape`).

**Drag eliminated.** One workaround crate-edge inversion (the file's
own rationale block becomes a delete). 80 LOC of IR-level concepts move
to their owning crate. Eliminates §9.2 carry-forward.

### 8.3 Delete `tape::visitor` Visitor traits — fold into per-grammar

**Before.** `crates/tape/src/visitor.rs:57-197` defines 7 visitor
traits (`GrammarVisitor`, `ObjectVisitor`, `ArrayVisitor`,
`StringVisitor`, `NumberVisitor`, `KeywordVisitor`, `PrattVisitor`).
746 LOC of trait + impl. Generated json.rs takes `V: ObjectVisitor +
ArrayVisitor + …` bounds at every parse fn. Per REMAINING-TRAJECTORY
§9.7, *"struct-direct grammars never invoke them"*; tape grammars
inline the visitor at the call site already.

**After.** Delete the visitor trait family; per-grammar generated code
emits its concrete builder calls inline. Sheets/CSS struct-direct
naturally bypass; JSON tape-direct keeps a single concrete
`TapeVisitor` impl as a free function.

**Mechanism.** Per AZ-II.W2 §9.7. The 7 traits reduce to direct calls
on the per-grammar concrete substrate (already the W2 substrate's
intent for struct-direct).

**Drag eliminated.** 746 LOC of trait surface, 12 trait re-exports at
the tape crate boundary, 7 trait-bound generic instantiations per
generated parse fn (ABI surface bloat that LTO collapses but `nm`
shows pre-LTO).

### 8.4 Split `crates/ir/src/passes/recognizers/dta.rs` (1625 LOC)

**Before.** Single file holding 14 public types + `lift_dta` /
`summarise` mining (per `passes/mod.rs:65-69`).

**After.** Directory module `recognizers/dta/{builder.rs, profile.rs,
summary.rs, types.rs, lift.rs}` per `feedback_directory-modules`.

**Mechanism.** Re-export from `dta/mod.rs`; cluster each conceptual
group to its own file.

**Drag eliminated.** 1625 LOC god module → 5 cohesive files.

### 8.5 Split `crates/core/src/backend/rust/emitter/grammar.rs` (1432 LOC)

**Before.** Multiple concerns: grammar-level emit + projection
admission + per-rule emit + materializer.

**After.** Directory module `emitter/grammar/{impl_block.rs,
parse_body.rs, projection.rs, materializer.rs, rule_function.rs}`.

**Mechanism.** Cluster `pub fn`s by concern.

**Drag eliminated.** 1432 LOC → 5 cohesive files.

### 8.6 Split `shapes/{object,arglist}.rs` (820 + 789 LOC)

**Before.** Each tangles 3 emission strategies (TapeDirect /
StructDirect / Visitor) in one file with `match strategy` plus
per-strategy private fns inline.

**After.** Directory modules `shapes/object/{tape.rs, struct_direct.
rs, visitor.rs, mod.rs}` and `shapes/arglist/{...}` mirroring the
existing pattern at `shapes/{keyword,wrap,array}/`.

**Mechanism.** Apply the established directory-module convention from
`shapes/wrap/visitor.rs`, `shapes/keyword/visitor.rs`,
`shapes/flat/struct_direct.rs` to object + arglist.

**Drag eliminated.** 1609 LOC across 2 files → 6 cohesive files.
Convention drift erased.

### 8.7 Rename `crates/ir/src/registry/r#struct.rs` → `layout.rs`

**Before.** `mod.rs:34-37` uses `mod r#struct;` (raw identifier
escape because `struct` is reserved) + `#[allow(clippy::module_
inception)]`.

**After.** `mod layout;` matching the dominant type
(`StructLayout`/`StructRegistry`). Drop the raw identifier escape and
clippy allow.

**Mechanism.** `git mv` + `mod.rs` update.

**Drag eliminated.** Reserved-word workaround; one `#[allow]`.

### 8.8 Demote `json-prototype` to a benches/ adjunct

**Before.** Standalone workspace member; zero non-bench consumers
(shape emitters' 10 references are all doc-mirrors).

**After.** `crates/core/benches/json-prototype/` (bench-only
adjunct).

**Mechanism.** `git mv crates/json-prototype/src crates/core/benches/
json-prototype/`; reference via `[[bench]]` paths.

**Drag eliminated.** One single-consumer workspace member; ~1000 LOC
crate boilerplate.

### 8.9 Split materialization classifier (858 LOC)

**Before.** Single file at `passes/materialization/classify.rs`
mixes classification rules + lattice join + MustTape traversal
heuristic.

**After.** `passes/materialization/{class.rs, join.rs, classify.rs,
traversal.rs}` directory module.

**Mechanism.** Same pattern as 8.4/8.5/8.6.

**Drag eliminated.** 858 LOC god file → 4 cohesive files.

### 8.10 Document path-dep responsibilities in SPEC

**Before.** `README.md:34-46` lists path-dep crates without role
descriptions.

**After.** Append `### Path-dep responsibilities` listing
parse_that → combinators; bbnf-regex → HIR/NFA/DFA; pprint → pretty;
pprint_derive → pprint macro.

**Drag eliminated.** Doc drift — new agents must grep to learn
`bbnf-regex`'s role.

---

## 9. Hand-off

**Top 5 lifts to W2-CLOSE-AUDIT.md synthesis:**

1. **`EmitStrategy` placement is wrong** (§4, §8.1). Lives in Rust
   backend; should live in `bbnf-ir::registry::strategy` for
   multi-backend share. **Block AZ-I.W3 if not lifted before CSS L4
   activation** — CSS will need its own backend-specific binding string,
   and the resolver match arm doubling per-backend is the exact
   `feedback_no-orthogonal-codepaths` violation the W2 substrate
   designed against.
2. **`StructBuilder` is one-implementor today** (§3). Justified by W2
   intent for Sheets/CSS, but if AZ-I.W2-act delays past one wave the
   trait converts from "planned substrate" to "orphan abstraction"
   per `feedback_pluggable-components`. Wave-budget gate.
3. **Three concrete file splits stand ready** (§8.4, §8.5, §8.6):
   recognizers/dta.rs (1625 LOC), emitter/grammar.rs (1432 LOC),
   emitter/shapes/{object,arglist}.rs (1609 LOC combined). Mechanical
   directory-module conversions; no semantics change. Cite
   `feedback_no-god-modules`.
4. **Tape's DTA + Visitor surfaces are owner-wrong** (§4, §7, §8.2,
   §8.3). REMAINING-TRAJECTORY §9.2 and §9.7 are correctly identified
   for AZ-II.W2 absorb; this audit confirms architectural correctness.
5. **`json-prototype` is a single-consumer crate** (§1). Bench-only
   role; demote to a `benches/` adjunct of `bbnf-core`. Justified per
   `feedback_general-infra-crates`'s contrapositive.

**File bounds remain.** No source changes proposed in this audit (per
the brief's allow-list). The proposals are queued for the AZ-I close
ceremony's path-forward synthesis.

**Word count.** ~2750 words exclusive of front-matter and tables.
