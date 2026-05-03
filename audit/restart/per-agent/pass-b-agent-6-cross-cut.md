# Pass B — Agent B.6 — Cross-cut Analysis

Date: 2026-05-03
Lens: Cross-file concerns spanning Pass-B substrate.
Source: `docs/restart/PASS-B-CODEGEN-MID.md` §Methodology — Agent B.6.

The directive flags seven cross-cuts; each is treated below.

---

## §1 — Codegen-runtime coupling

### 1.1 Surface enumeration

The `bbnf-codegen` (post-extract) emits source code that depends on
runtime types. The contract surface:

| Codegen-emitted symbol | Runtime type referenced |
|---|---|
| `crate::runtime::<g>::<G>StructBuilder::new()` | `runtime::<g>::builder::<G>StructBuilder` |
| `crate::runtime::<g>::<G>Document::new(arena, root, input)` | `runtime::<g>::document::<G>Document` |
| `crate::runtime::<g>::<G>Value::*` | `runtime::<g>::value::<G>Value` |
| `crate::runtime::<g>::<G>Arena::new()` | `runtime::<g>::arena::<G>Arena` |
| `<G>StructBuilder::begin_compound(&__layout)` | `runtime::builder::StructBuilder` trait |
| `<G>StructBuilder::push_leaf_with_*` | `runtime::builder::StructBuilder` trait methods |
| `crate::path::ir::PathSegment::*` | `path::ir::PathSegment` |
| `crate::path::cursor::PathCursor::new` | `path::cursor::PathCursor` |
| `crate::path::ascent::ParseFully` | `path::ascent::Decision` (and variants) |
| `crate::runtime::error::ParseErr` | `runtime::error::ParseErr` |

The `EmitStrategy::for_grammar` resolver at `bbnf-ir::registry::strategy::PRODUCTION_MANIFEST_TABLE`
splices the per-grammar `<G>StructBuilder` + `<G>Document` paths into
the emitted parse fn body.

### 1.2 Contract under crate split

When `bbnf-codegen` and `bbnf-runtime` separate:

- The codegen emits `pub use bbnf_runtime::path::*;` not `crate::path::*;`
- The codegen emits `pub use crate_<grammar>::*;` for per-grammar
  declaration crate types (e.g. `pub use json::JsonStructBuilder;`)
- The `[workspace.metadata.bbnf-strategy]` table in workspace `Cargo.toml`
  encodes the per-grammar-declaration-crate paths

Today: the manifest carries `rust_builder_path: "crate::runtime::json::JsonStructBuilder"`.
Post-split: `rust_builder_path: "::json::JsonStructBuilder"`.

### 1.3 Coupling minimisation

Per `feedback_one-codegen-path` and `feedback_no-orthogonal-codepaths`,
the codegen-runtime coupling minimises by:

- ALL runtime types live in `bbnf-runtime` (mechanism) + per-grammar
  declaration crates (typed types)
- The codegen has ZERO knowledge of grammar-named types; the contract
  is the trait surface (`StructBuilder`, `PathQuery`)
- Per Lock 14, the per-grammar runtime modules are emitted from a
  template (per Agent B.5 §1) — codegen's emission of `<G>StructBuilder`
  references is straightforward because the template emits the type

The contract is simpler post-split: codegen emits *trait calls* +
*per-grammar declaration crate references*. Runtime has *no codegen
references*; codegen has *no runtime implementation knowledge* beyond
the trait interface.

---

## §2 — Optimiser fan-in / fan-out

### 2.1 Optimiser inputs (consumed)

Egraph + csp-solver consume:

| Input | Source crate | Path |
|---|---|---|
| `IrNode` typed enum | `bbnf-ir` | `crates/ir/src/types/dag.rs` |
| `RuleId`, `NodeId` | `bbnf-ir` | same |
| `TypeDesc` | `bbnf-ir` | `crates/ir/src/types/type_desc.rs` |
| Grammar facts | `bbnf-ir::passes::recognizers` | facts mining |
| Cost weights | `egraph::cost_weights` | self |
| CSP domain types | `csp-solver` | self |

### 2.2 Optimiser outputs (produced)

Egraph + csp-solver produce:

| Output | Consumed by |
|---|---|
| Saturated e-graph | `egraph::extract` |
| Best-cost extraction | `bbnf-ir::passes::optimize` |
| CSP solution | `bbnf-ir::passes::csp_strategy` |
| Strategy bindings (per-rule) | `bbnf-codegen::backend::strategy` |
| AltDispatch / SeqStrategy / RepeatStrategy decisions | per-shape emit |

### 2.3 Fan-in cleanliness

The optimiser crates are *consumed* by `bbnf-ir` + `bbnf-codegen`;
they don't fan-in across other domain crates. The contract is one-way:
- `bbnf-ir` → optimiser → `bbnf-ir` (saturation + extract)
- optimiser → `bbnf-codegen` (strategy decisions)

Per Lock 4 (orthogonal optimisation by output-piping; no unified
hypergraph): honoured.

### 2.4 Fan-out cleanliness

Egraph publishes `Language` trait + e-graph types. csp-solver publishes
constraint kit + solver. `egraph-derive` publishes proc-macro derive.

Per Lock 11 promotion candidates (Agent B.4 §Q3): all three are
crates.io-ready.

---

## §3 — Tape residue across crates

Per CENSUS §1 + Agent B.3 §Lock 1: ~50 doc-comment residue sites
across Pass-B substrate. Production tape symbols are dead.

The residue cuts across:
- runtime/<g>/mod.rs — boilerplate substrate-departure assertion (9 files)
- runtime/<g>/document.rs — narrative on tape-walker retirement
- runtime/builder.rs — "selection between tape and struct" narrative
- pipeline/compile/{mod,pipeline}.rs — "Tape-direct ingress"
- backend/driver/{alt,seq,analysis}.rs — "Under tape-first emission" narratives
- backend/types/mod.rs — "AM.3 per-branch tape surgery"
- backend/rust/ir_types.rs — "tape-first rule emission"
- backend/rust/emitter/grammar.rs — "TapeVisitor" reference
- backend/rust/emitter/profile.rs, emitter_types.rs, ir_enums.rs — tape mentions
- backend/rust/emitter/shapes/{number,arglist,object,flat,alt_dispatch}.rs — tape narratives
- generated/{json,css_l4}.rs — emitter-source TODOs surviving regen

Per `feedback_no-metalanguage-docs`: docs must never reference plans,
commits, conversation history. The tape residue is meta-language
narrative — it documents what was, not what is.

The synthesis remedies:
- regen-clean: scrub emitter-source TODOs that produce generated tape mentions
- doc-clean: remove tape-narrative paragraphs from runtime, pipeline, backend
- KEEP only the tape-departure regression gate at `crates/core/tests/struct_direct_snapshots.rs:45-53`
  — that's the strict negative-assertion invariant per CENSUS §1.3

---

## §4 — OpenFrame migration completeness

### 4.1 Live OpenFrame symbols

Per grep evidence (Agent B.3 §Lock 1), OpenFrame is alive in 6 files:

| File | OpenFrame variant count |
|---|---:|
| `runtime/json/builder.rs` | 4 (Array, Object, Pair, Wrap) |
| `runtime/css_l4/builder.rs` | 14 |
| `runtime/google_sheets/builder.rs` | (varies) |
| `runtime/bbnf/builder.rs` | (varies; bounds-recording extension) |
| `runtime/builder_template.rs` | template-typed |
| `runtime/google_sheets/arena.rs` | OpenFrame mention |

Plus 109 textual mentions across runtime/ files.

### 4.2 Phase-4 BA option-(a) status

Per the directive: "post-Phase-4 BA option-(a), all 9 grammars retire
OpenFrame. Verify no residue remains in Pass-B scope."

Verification: OpenFrame residue **does** remain in Pass-B scope across
6 files + 109 mentions. Phase-4 BA option-(a) has *not* landed across
all 9 grammars; today's substrate is OpenFrame-bearing.

### 4.3 Migration mechanism

Per Agent B.5 §9: OpenFrame retires via direct-projection emit. The
emitted parse fns hold partial state on the call stack + `SmallVec`
locals; no runtime trait surface; no heap-stack of partial compounds.

The migration completeness gate: **post-restart, OpenFrame appears
ONLY in `archive/`**. Production substrate carries zero OpenFrame
mentions. The `crates/core/tests/struct_direct_snapshots.rs` negative
gate extends to assert OpenFrame-departure parallel to TapeRec-departure.

---

## §5 — Cost-model-egraph coupling

Per the directive: "should the cost model live in egraph (extraction-side)
or as a separate crate?"

### 5.1 Current state

`crates/egraph/src/cost_config.rs` (≤80 LOC) — `CostConfig`
`crates/egraph/src/cost_weights.rs` (191 LOC) — `CALIBRATED_WEIGHTS`,
`CostWeights`
`crates/egraph/src/extract.rs` (215 LOC) — extract pass uses cost model

The cost model lives inside egraph. It's tightly coupled to e-graph
extraction. Per `feedback_general-infra-crates`, "general-purpose
constructs (e-graphs, cost models) in own crate(s)" — but the cost
model in this repo is e-graph-extraction-specific (`SubstrateBinding`,
`StructDirect`, `AltStrategy` cost weights).

### 5.2 Disposition

KEEP cost model in egraph per Agent B.5 §6.

The synthesis revisits if a non-egraph consumer arrives (e.g. CSP-only
optimisation that needs the cost model independently).

---

## §6 — Generated-output as substrate

Per the directive: "168,750 LOC of generated/ files have implicit
invariants (trait conformance, visibility, span resolution). Surface
the invariants the codegen path silently relies on."

### 6.1 Implicit invariants

The generated files implicitly assume:

| Invariant | Description |
|---|---|
| `<G>StructBuilder: StructBuilder` | The per-grammar builder implements the trait |
| `<G>Document: <G>PathQuery` | The per-grammar document implements its path-query trait |
| `<G>Value: Copy + 'static` | Value enum is a tag-and-payload with static-lived variants (slice-borrow lifetime is parametric) |
| `<G>Arena::new() -> <G>Arena<'p>` | Arena constructor returns lifetime-parametric value |
| Structural alphabet | `__shape_support_<G>::skip_space`, `first_quote_or_backslash` etc. emit per-grammar |
| Path cursor | `__EAGER_EMPTY_PATH: LazyLock<TypedPath<G, &str>>` per-grammar |
| Marker struct | `pub struct <G>Parser` exists at the generated module's root |
| `parse(input)` entry | `impl <G>Parser { pub fn parse(input: &str) -> Result<<G>Document, ParseErr> }` |
| `parse_with(input, &TypedPath)` entry | Lazy-parse surface |
| Span resolution | Generated parse fns thread `*p: usize` cursor; positions resolve to byte offsets within `input: &[u8]` |
| `crate::host::*` resolves | Host fns referenced by `-> fn_name(...)` map annotations resolve at the generated file's `use` site |

### 6.2 Invariant fragility

The invariants are NOT enforced by the type system at codegen-emission
time; the generated output's `cargo check` is the enforcement. If the
runtime types drift (e.g. `<G>StructBuilder` adds a new trait method
the codegen doesn't emit), the generated file fails to compile.

Per `feedback_typed-materialization-invariant`, every typed `->`
must reach the tape emitter. The post-restart equivalent: every
typed `->` must reach the typed-projection in the emitted parse fn.

### 6.3 Invariant audit

Per Lock-14 verification commands the synthesis renders, the audit
extends to:

```bash
# Every per-grammar declaration crate must export <G>StructBuilder
$ cargo check --workspace
# Every per-grammar generated parse fn must compile against runtime trait
# Every typed path in the IR must reach a typed-projection in emitted code
```

---

## §7 — xtask cross-cut

### 7.1 xtask dependency graph

Per `xtask/Cargo.toml`:

```
xtask
├── path-deps:
│   ├── bbnf-ir   (read IR + run pipeline)
│   ├── bbnf      (lib-only — read public API)
│   ├── parse_that  (registry-version path-dep)
│   └── pprint    (registry-version path-dep)
├── codegen toolkit:
│   ├── proc-macro2
│   ├── quote
│   ├── syn (with full + extra-traits)
│   └── prettyplease
├── cli + meta:
│   ├── clap
│   ├── cargo_metadata
│   └── anyhow
└── i/o:
    ├── serde + serde_json + toml
    └── tempfile
```

Acyclic. xtask depends on bbnf-ir + bbnf; neither depends back on
xtask.

### 7.2 Post-split dependency graph

When bbnf-codegen + bbnf-runtime extract:

```
xtask
├── path-deps:
│   ├── bbnf-ir
│   ├── bbnf-codegen   (run codegen)
│   ├── bbnf           (lib-only — public API)
│   └── ...
```

bbnf-codegen path-deps on bbnf-ir; bbnf-runtime path-deps on path
+ path-core; per-grammar declaration crates path-dep on bbnf-runtime
+ bbnf-codegen-emitted-via-include.

The dependency graph remains acyclic.

### 7.3 Per-grammar template invocation

Per Agent B.5 §1: xtask carries a `runtime_template` sub-command
(or sub-module of `regen`). xtask runs:

1. Read `[workspace.metadata.bbnf.grammars]` from workspace `Cargo.toml`
2. For each grammar:
   a. Run `bbnf::pipeline::compile_paths_request(grammar source)` →
      compiled IR
   b. Run `bbnf-codegen::generate_all(ir)` → per-grammar generated.rs
   c. Run `xtask::runtime_template::emit(ir, registry)` → per-grammar
      runtime modules
3. Format output via `prettyplease::unparse`
4. Write per-grammar declaration crate's `src/runtime/{value,document,
   view,kind,arena,builder,mod}.rs`
5. Write generated parse fn at `src/generated.rs` (per-grammar
   declaration crate)
6. `--check` mode diffs against committed output

### 7.4 Bench cross-cut

`bbnf-bench` (per Agent B.5 §5) runs separately from xtask; xtask
carries no bench logic.

Per Lock-feedback_vitest-bench: vitest-style `bench()` API; per-grammar
bench files live under `crates/<grammar>/benches/` not under xtask.

---

## §8 — Lock-1 + Lock-14 + Lock-13 convergence

The three most-violated Pass-B locks converge on one architectural
claim: **every per-grammar concern lives in its own declaration crate;
the generic substrate is grammar-agnostic**.

| Concern | Today | Post-restart |
|---|---|---|
| Per-grammar value enum | `crates/core/src/runtime/<g>/value.rs` (hand-written) | `crates/<grammar>/src/runtime/value.rs` (template-emitted) |
| Per-grammar builder | `crates/core/src/runtime/<g>/builder.rs` (hand-written incl. OpenFrame) | `crates/<grammar>/src/runtime/builder.rs` (template-emitted; direct-projection) |
| Per-grammar host fns | `crates/core/src/css_types.rs` (CSS-named in core) | `crates/<grammar>/src/host.rs` |
| Per-grammar generated parse fn | `crates/core/src/grammar/generated/<g>.rs` | `crates/<grammar>/src/generated.rs` |
| Per-grammar prettify | `crates/gorgeous/src/<g>.rs` (CENSUS §2.5) | `crates/<grammar>/src/prettify.rs` (template-emitted from grammar's `@pretty` annotations) |
| Per-grammar tests | `crates/core/tests/<g>_*.rs` | `crates/<grammar>/tests/*.rs` |
| Per-grammar registry sidecar | `crates/core/src/grammar/generated/<g>.registry.json` | `crates/<grammar>/src/registry.json` (or template-emitted const) |

Lock 1 says no tape; Lock 14 says no per-grammar code in generic
crates; Lock 13 says no god directories. All three retire when each
grammar lives in its own declaration crate with template-emitted
runtime, with the generic crates (`bbnf-codegen`, `bbnf-runtime`,
`bbnf-ir`, `path`) carrying ZERO grammar-named code.

---

## §9 — Cross-cut summary

| Cross-cut | Verdict | Synthesis carry |
|---|---|---|
| Codegen-runtime coupling | Contract simpler post-split | per Agent B.4 §Q1 + B.5 §3 |
| Optimiser fan-in/fan-out | Honoured (Lock 4) | promote per Lock 11 |
| Tape residue across crates | ~50 sites in Pass-B; scrub during regen | doc-clean + emitter-source-TODO clean |
| OpenFrame migration completeness | NOT complete; 6 files + 109 mentions | direct-projection emit per Agent B.5 §9 |
| Cost-model-egraph coupling | Tight; KEEP per Lock 11 default | none |
| Generated-output as substrate | 11 implicit invariants; type-system enforced post-emission | invariant-audit per §6 |
| xtask cross-cut | Acyclic; remains so post-split | template invocation per Agent B.5 §1 |

The convergent finding is §8: Lock 1 + Lock 13 + Lock 14 retire
*together* via per-grammar declaration crates + template-emitted
runtimes + direct-projection emit. No partial fix; the synthesis must
land all three or land none.
