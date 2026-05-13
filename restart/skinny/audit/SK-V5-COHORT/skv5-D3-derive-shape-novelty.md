# SK-V5 D3 — derive_backend_shape Novelty Verdict

Interrogation of A5 cohort claims regarding the 5-shape `BackendShape` and `derive_backend_shape`. Citations are file:line, current as of master HEAD `1519cf16`.

## §1 Per-claim verdict

| # | A5 claim | Verdict |
|---|---|---|
| 1 | "Zero of five BackendShape variants implemented in Rust" | **NEW** (verified; enum is not declared in any Rust source) |
| 2 | "`derive_backend_shape` does not exist; only `shapes_for_json` does" | **NEW** (verified; no `derive_backend_shape` symbol in `skinny/crates/`) |
| 3 | "`shapes_for_json` is a *different* `ShapeFacts` for output types" | **VERIFIED** (the existing `ShapeFacts` is a typed-view shape catalogue, not the BIR per-rule lowering selector) |
| 4 | "codegen is decorative: `parser_rs` / `generated_rs` take `BackendIr`, write `let _ = backend`, `include_str!` verbatim" | **VERIFIED** (literally `let _ = backend;` followed by `include_str!`) |
| 5 | "passes hardcodes `shapes_for_json()` / `nominate_json()` regardless of grammar" | **VERIFIED** (`compile()` at `passes/src/lib.rs:28-29` calls them unconditionally; `nominate_json` body ignores its `_grammar` argument) |

All five claims hold. None are MASTER-PLAN-scoped as already-in-flight — the implementation work is **NEW** for SK-V5 unless re-attributed to MASTER-PLAN §13 H.W1 / H.W4 (see §5 below).

## §2 Actual current backend-selection path

Trace from grammar source bytes to emitted Rust file set.

```
emit_json_from_source(src)                          codegen/src/lib.rs:60-64
 ├── grammar::parse_json_grammar(src)              -> GrammarIr
 ├── passes::compile(&grammar)                      passes/src/lib.rs:24-36
 │    ├── normalize -> validated GrammarIr
 │    ├── layout::types::infer  -> TypeFacts (HM)
 │    ├── layout::run            -> LayoutFacts {
 │    │                              rule_types, node_types,
 │    │                              layout_policies: empty,
 │    │                              hot_call_graph: derive_hot_path(..)
 │    │                            }                passes/src/lib.rs:46-51
 │    ├── shapes::shapes_for_json()                 passes/src/lib.rs:198-226
 │    │     -> hardcoded ShapeFacts with 9 entries:
 │    │        JsonRoot/JsonValue/JsonObject/JsonArray/JsonPair/
 │    │        JsonString/JsonNumber/JsonBool/JsonNull
 │    ├── recognizers::nominate_json(&_grammar)     passes/src/lib.rs:232-238
 │    │     -> vec![Recognizer::SimdScan {
 │    │            mode: Exact, alphabet: json(), site: PreEntry
 │    │        }]
 │    │     (the `_grammar` parameter is unused)
 │    └── extract::single_plan                      passes/src/lib.rs:334-365
 │          -> BackendIr { rules: lower_expr + materialize_rule }
 │             where materialize_rule emits TapeEmit+DirectBuild
 │             iff the rule name matches one of seven hardcoded
 │             JSON names (materialization_for_rule, lib.rs:423-434)
 └── codegen::emit_json(&backend_ir)                codegen/src/lib.rs:66-76
      ├── generated.rs <- generated_rs(backend)     lib.rs:115-118
      │                    : `let _ = backend;`
      │                    : include_str!("json_templates/generated.rs")
      ├── parser.rs    <- parser_rs(backend)        lib.rs:110-113
      │                    : `let _ = backend;`
      │                    : include_str!("json_templates/parser.rs")
      ├── host.rs      <- static string
      ├── mod.rs       <- static string
      ├── value.rs     <- include_str!(...)
      ├── view.rs      <- include_str!(...)
      └── visitor.rs   <- include_str!(...)
```

Net consequence: the `BackendIr` is constructed, threaded through `emit_json`, accepted by `parser_rs` / `generated_rs`, then discarded with `let _ = backend;`. The "generated" parser is `include_str!` of a hand-written template at `skinny/crates/codegen/src/json_templates/{parser,generated}.rs` (87 + 304 LOC).

The grammar parse → BIR build → emit pipeline is real for the determinism / shape-mining / hot-path sides (`shape_facts` ends up serialized into `BackendIr`, `hot_call_graph` is computed from `derive_hot_path` over real grammar refs), but the codegen text output is **shape-independent**: every `(grammar) -> codegen` call returns byte-identical files modulo the grammar source file (`emission_is_deterministic` test at `codegen/src/lib.rs:179-189` asserts this).

## §3 BackendShape enum status

`BackendShape` enum:
- **Declared** in spec: `restart/ARCHITECTURE.md:1048-1072` (5 variants: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`).
- **NOT declared** in any Rust source under `skinny/crates/`. `grep -rn "BackendShape" skinny/crates/` returns zero hits (the only matches are inside `bbnf-simd/ext/x86/bbnf.asm` comments and `target/` debug yml).
- **NOT referenced** by any pass. `LayoutFacts` at `passes/src/lib.rs:46-51` has fields `rule_types`, `node_types`, `layout_policies`, `hot_call_graph` — the spec-prescribed `backend_shape: HashMap<RuleId, BackendShape>` field (per `ARCHITECTURE.md:1034`) is absent.

`derive_backend_shape`:
- **Declared** in spec: `restart/ARCHITECTURE.md:1075-1083` (8-step priority algorithm).
- **Re-specified** at `restart/skinny/COMPILER.md:241-336` (lowering matrix per shape) and `restart/skinny/audit/SOTA-BEAT-DESIGN.md:228-273, 351-361`.
- **NOT implemented**: no Rust symbol named `derive_backend_shape`, `derive_shape`, or `shape_for_rule` exists. `passes::recognizers` (`passes/src/lib.rs:229-329`) contains only `nominate_json` + the `hot_path` submodule.

`shapes_for_json` vs spec `ShapeFacts`:
- The existing `ShapeFacts` (`ir/src/lib.rs:436-467`) is a **typed-view shape catalogue** — a `Vec<Shape>` where `Shape::{Struct, Enum}` carries named Rust types (`JsonRoot { value: JsonValue<'i> }`, etc.) used for the `view.rs` direct-builder surface. It is the SK-V3-style `ShapeFacts` for output type emission.
- The spec's `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` is a **per-rule lowering-mode selector**. Same name "shape", entirely different role: one names types in the rendered AST, the other names a generation strategy for `Alt { mode: Dispatch }`.
- A5's claim that they are different is correct. Confusable naming, distinct concerns.

Status summary: `BackendShape` is **spec-declared, Rust-undeclared, dead** (no producer, no consumer, no field on `LayoutFacts`).

## §4 The "decorative codegen" claim

Verification of `codegen/src/lib.rs:110-118`:

```rust
fn parser_rs(backend: &BackendIr) -> String {
    let _ = backend;
    include_str!("json_templates/parser.rs").to_string()
}

fn generated_rs(backend: &BackendIr) -> String {
    let _ = backend;
    include_str!("json_templates/generated.rs").to_string()
}
```

The body of `parser_rs` and `generated_rs` does not branch on `backend`, does not read any field of `BackendIr`, does not call `lower_*` to materialize Rust tokens. The `_ = backend` is a literal lint-silencer for the unused parameter. `include_str!` is a compile-time string embed of the hand-written template files at `skinny/crates/codegen/src/json_templates/{parser,generated}.rs`. There is no subtle BIR-gated branching; `emits_expected_file_set_in_order` and `emission_is_deterministic` (`codegen/src/lib.rs:160-189`) assert the file set is constant and the contents are byte-identical across invocations.

Two qualifications on "decorative":
1. The **BIR construction itself is non-decorative**: `extract::single_plan` (`passes/src/lib.rs:334-365`) walks the real grammar, calls `lower_expr` over every expression, projects `materialize_rule` per rule, and produces `BackendIr` with correct `recognizers` + `rules` + `shape_facts` payloads. The tests at `codegen/src/lib.rs:454-481` validate that `output.backend_ru les.len() == 15` and that the `object` rule's BIR contains `TapeEmit` + `DirectBuild`. So BIR is *built* honestly.
2. The decorative part is the **BIR → Rust source step**: that step is a noop pass-through to a hand-written template. The BIR data structure is computed but never consulted during text emission.

Therefore: "decorative codegen" is accurate as applied to the Rust-source emission step (`parser_rs` / `generated_rs`), and is **mis-stated** if it implied the BIR build itself is decorative. The BIR build is correct; only the lowering step is degenerate.

Two other shapes are visible in the passes layer (`materialize_rule` at `passes/src/lib.rs:401-434`):
- **Hardcoded grammar coupling**: `materialization_for_rule` matches on the literal strings `"object"`, `"array"`, `"pair"`, `"string"`, `"number"`, `"bool"`, `"null"`. These are JSON rule names embedded in a generic-purpose pass. Per Lock 14, this would emit `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` if `xtask lint-no-hardcoded-grammars` were run against `passes`.
- **Recognizer mining is grammar-blind**: `nominate_json` (`passes/src/lib.rs:232-238`) returns a single `SimdScan` over the JSON structural alphabet regardless of the grammar's actual alphabet — `_grammar` is unused.

## §5 Final novelty verdict

| Item | Status |
|---|---|
| `BackendShape` enum (5 variants) | NEW Rust state required |
| `derive_backend_shape(grammar, rule_id) -> BackendShape` | NEW |
| `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` field | NEW |
| Per-shape lowering at `crates/codegen/src/lower/rust.rs` | NEW (the `codegen/src/lower/` directory does not exist; only `codegen/src/json_templates/`) |
| `BBNF-BACKEND-SHAPE-INCONSISTENT` / `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostics | NEW (catalogued at `ARCHITECTURE.md:1155-1156`; no producer in Rust) |
| MASTER-PLAN coverage | **EXTANT-PLANNED**, not yet executed |

MASTER-PLAN §13 explicitly scopes the implementation:
- **H.W1** (`restart/MASTER-PLAN.md:504`): "Cost model in `passes::recognizers` derives `LayoutFacts.backend_shape[rule_id]` from existing Grammar IR facts (first-set disjointness, output mode, recovery, host decode, layout scope) per Lock 10 auto-detection mandate. Lowerer at `crates/codegen/src/lower/rust.rs` emits typed `EventCursor` consumption for `OffsetTape`/`EventTape` rules…"
- **H.W4** (`restart/MASTER-PLAN.md:508`): "Cost-model `derive_backend_shape` exercises all five `BackendShape` variants across the expanded corpora… 5-shape per-rule selection emits to `LayoutFacts.backend_shape` with `BBNF-BACKEND-SHAPE-INCONSISTENT` covering rejected configurations."

Both waves are spec-scoped; no Rust commit yet lands the enum, the algorithm, the field, or the per-shape lowering. The most recent commit `1519cf16` (SK-V4 redress) confirms `SinkOnly` mandate as docs but does not introduce the Rust state. Commit `8fa51245` (2026-05-12) added the spec for `derive_backend_shape` to ARCHITECTURE.md without implementation.

**Conclusion.** A5's diagnosis stands. Every claim verifies. The 5-shape `BackendShape` and `derive_backend_shape` are **EXTANT-PLANNED but Rust-NEW**: the MASTER-PLAN H.W1/H.W4 wave gates already commit to authoring them, but no Rust line has been written. SK-V5 scopes the implementation work, not a fresh spec author. Authoring sequence implied by the existing pipeline:

1. Add `BackendShape` enum to `ir` crate (mirroring `ARCHITECTURE.md:1048-1072`).
2. Add `backend_shape: HashMap<RuleId, BackendShape>` field to `LayoutFacts` (`passes/src/lib.rs:46-51`).
3. Implement `passes::recognizers::derive_backend_shape` per the 8-step priority algorithm (`ARCHITECTURE.md:1075-1083`).
4. Wire `compile()` (`passes/src/lib.rs:24-36`) to populate the field for each rule after `layout::run`.
5. Create `skinny/crates/codegen/src/lower/rust.rs` with per-shape emission for `Alt { mode: Dispatch }` (matches the SOTA-BEAT-DESIGN.md §4 sketch at lines 236-272).
6. Replace `parser_rs` / `generated_rs` `include_str!` returns with a function that reads `backend.rules`, looks up the shape per rule from `layout_facts.backend_shape`, and emits via the new lowerer.
7. Emit `BBNF-BACKEND-SHAPE-INCONSISTENT` / `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostics from `derive_backend_shape` per `ARCHITECTURE.md:1155-1156`.

Same-wave-consumer rule binds steps 5–6 together: per-shape lowerer cannot land without the consumer call from `parser_rs` / `generated_rs`, and the diagnostic cannot land without the producer in step 3.
