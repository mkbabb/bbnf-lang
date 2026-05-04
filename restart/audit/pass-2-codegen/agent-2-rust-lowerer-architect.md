# PASS-2 Agent 2: Rust Lowerer Architect

## §1 Scope + Framing

Lens: specify Rust V1 lowering from Backend IR to template-emitted Rust parser/runtime modules. PASS-2 owns Rust V1 lowerer and runtime template (`restart/prompts/PASS-2-CODEGEN.md:3`). The current source has a Rust emitter, but it is not the greenfield lowerer shape. It sits behind a broad `Emitter` trait (`crates/core/src/backend/emitter.rs:31-566`), and Rust grammar emission contains strategy dispatch, PHF tables, Pratt LUTs, path plans, parser bodies, and registry emission in one area (`crates/core/src/backend/rust/emitter/grammar.rs:119-221`). PASS-2 must split these responsibilities around Backend IR and Tape.

The Rust lowerer must retire the OpenFrame checkpoint pathology. The restart sketch records `Vec<OpenFrame>::clone` as 86.07% of samples in the old substrate (`restart/corpora/RESTART-SKETCH.md:154-184`), and the current builder template still deep-clones stack state in `checkpoint` (`crates/core/src/runtime/builder_template.rs:203-210`). Lock 1 names this exact class of parallel substrate as a fault and mandates Tape with no OpenFrame ladder (`restart/locks/14-LOCKS.md:34`).

## §2 Per-Item Table

| Construct | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| `Rule` | Current generated parse body already has private/public parser split hooks (`crates/core/src/backend/rust/emitter/grammar.rs:298-363`). | It is coupled to strategy registries and old path plans in one source area (`crates/core/src/backend/rust/emitter/grammar.rs:155-221`). | Emit `fn parse_<rule>(&mut Cursor, &mut TapeBuilder) -> Result<NodeIdx, Error>`, then generate typed accessors. | Rule functions cannot read Grammar IR. | KEEP-REINVENT. |
| `Seq` | Direct field writes match sonic-rs direct-struct lesson (`restart/corpora/SOTA.md:30-44`). | Direct-only output conflicts with Tape authority. | Emit child calls, push a tape node with `first_child` and `sib_skip`, and generate a struct view over child indices. | No second builder path. | REINVENT. |
| `AltDispatch` | Current rust emitter has dispatcher/PHF concepts (`crates/core/src/backend/rust/emitter/grammar.rs:155-163`). | Byte-disjoint alts previously still paid checkpoint costs (`restart/corpora/RESTART-SKETCH.md:201-217`). | Cost model selects byte dispatch, PHF keyword, or structural scan; byte-disjoint dispatch does not checkpoint each arm. | Every speculative alt records why dispatch was not legal. | KEEP-MODIFY. |
| `AltSpeculative` | Necessary for overlapping grammar shapes. | Old checkpoints cloned frame vectors (`crates/core/src/runtime/builder_template.rs:203-210`). | Checkpoint is `{ cursor, tape_len, payload_len, error_len }`; rollback truncates all arenas. | No `Clone` over child/value stacks on hot path. | REINVENT. |
| `Repeat` | Current trait has repeat hooks (`crates/core/src/backend/emitter.rs:134-164`). | Current hook shape is grammar-walk-facing. | Emit loop with progress check, optional separator edge, and bounded vector payload. | Infinite-loop gate on zero-width body. | KEEP-REINVENT. |
| `Optional` | BC table models optional as codegen variant (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:88-98`). | Optional cannot allocate an OpenFrame. | Emit branch returning `Option<NodeIdx>` plus tape omission when absent. | Absence must be distinguishable from empty span. | KEEP. |
| `Lookbehind` | README keeps lookbehind as grammar-level surface (`restart/README.md:125-129`). | It is absent from the BC variant table. | Emit bounded reverse predicate over input bytes or regex side table; no tape node. | PASS-1 must reject unbounded lookbehind. | REINVENT. |
| `RegexDfa` | README assigns Unicode to regex layer (`restart/README.md:131-143`). | Prompt stale lens mentions Unicode-set as construct (`restart/prompts/PASS-2-CODEGEN.md:33`). | Rust lowerer calls regex engine with compiled Unicode property table; no grammar-level Unicode node. | Regex payload must cite Unicode version from regex crate metadata. | KEEP-MODIFY. |
| `HostCall` | README includes `@host fn` and multi-function chaining (`restart/README.md:145-166`). | Per-grammar declaration crates are not default (`restart/README.md:13-25`). | Rust lowerer emits calls through `host::Registry` and generic host primitives; chains compose typed steps. | No `match grammar` in generic code. | REINVENT. |
| `ErrorRecovery` | `@error` is in V1 (`restart/README.md:172-174`). | Error APIs are PASS-3-facing. | Rust lowerer emits structured diagnostic records keyed by generic grammar id data. | Error types cannot be per-grammar Rust types in generic crates. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **Rust lowering consumes Backend IR only.** The current driver/trait design is replaced because it says the driver walks `GrammarIR` (`crates/core/src/backend/driver/mod.rs:1-6`). The new driver walks `BackendModule`, and Rust lowering is a consumer of already-selected strategies.

2. **TapeBuilder is the only mutable materialiser.** `TapeBuilder` owns node, payload, diagnostic, and optional side-table arenas. Checkpoint/rollback is length-truncation. This is the same-wave consumer wiring demanded by lessons on substrate plus consumer gates (`docs/precepts/instructions/LESSONS-LEARNED.md:17-26`, `docs/precepts/instructions/LESSONS-LEARNED.md:74-80`).

3. **Typed values are views.** The generated `<Grammar>Document<'i>` stores `&'i Tape<'i>` and a root node id. Generated `<Rule>View<'i>` or value enums borrow the tape and project fields. This matches Lock 1's typed-value-borrow shape (`restart/locks/14-LOCKS.md:34`).

4. **Emitter trait collapses to a small Rust lowerer API.** PASS-B already identified the 30-method trait collapse to 8-10 methods (`restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). Rust lowerer methods are grouped by module, type, rule, node, scanner, host table, registry, finish.

5. **No generated Rust path resurrects `runtime/path.rs`.** CENSUS marks runtime/path duplication and legacy `parse_with` lower as delete surfaces (`restart/corpora/CENSUS.md:237-265`). PASS-2 Rust lowering emits path/view accessors through Tape-backed views; PASS-3 owns ergonomic selectors.

## §4 New Facilities Proposed

```rust
pub struct RustLowerer<'m> {
    module: &'m BackendModule,
    out: RustModuleBuilder,
}

impl<'m> RustLowerer<'m> {
    pub fn emit_module(&mut self) -> Result<RustFiles>;
    fn emit_types(&mut self) -> Result<()>;
    fn emit_tape_kinds(&mut self) -> Result<()>;
    fn emit_rule(&mut self, rule: RuleId) -> Result<()>;
    fn emit_node(&mut self, node: NodeId) -> Result<Expr>;
    fn emit_scanner_tables(&mut self) -> Result<()>;
    fn emit_host_registry(&mut self) -> Result<()>;
    fn finish(self) -> Result<RustFiles>;
}
```

Sample emission for an alt:

```rust
let mark = tape.checkpoint(cursor);
if let Ok(node) = parse_arm_a(cursor, tape) {
    return Ok(node);
}
tape.rollback(mark);
parse_arm_b(cursor, tape)
```

The checkpoint is small because it stores lengths, not cloned frames. The old deep clone in `builder_template.rs` is explicitly discarded (`crates/core/src/runtime/builder_template.rs:203-210`).

Sample emission for host chains:

```rust
let span = input.slice(start, end);
let value = host.step::<Trim>(span)
    .and_then(|v| host.step::<ParseHexColor>(v))
    .map_err(|e| error.host(rule_id, span, e))?;
```

The chain model honors `@host fn` and multi-function chaining while keeping host logic generic (`restart/README.md:145-166`).

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1 must provide: `ValueShape`, `TapeKind`, fixed-width lookbehind analysis, scanner alphabet, Pratt operator table, host signatures, layout policy, and cost-plan annotations. README's pass order places cost extraction before Backend IR lower (`restart/README.md:188-217`).

PASS-3 receives: parse signatures, document/view names, error shape, visitor hooks, and materialisation cost table. Runtime API stability belongs to PASS-3, but PASS-2 must emit enough structure for PASS-3 to wrap without re-parsing or walking generated internals (`restart/prompts/PASS-2-CODEGEN.md:54`).

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| Rust emitter keeps direct Grammar IR reads | Backend IR boundary fails | Add an assertion test that `codegen/src/lower/rust` does not import grammar IR modules. |
| Checkpoint truncation forgets a side table | Rollback corrupts typed views | `TapeCheckpoint` records lengths for every arena and side table in one struct. |
| Host chains allocate boxed dynamic calls | Host path becomes slow and opaque | Monomorphize common generic primitives; dynamic registry only for rare external `@host fn`. |
| Lookbehind accepts unbounded patterns | Parser can become non-local | PASS-1 width analysis must encode `Bounded(n)` or reject. |
| Error recovery changes parse result shape | PASS-3 API churn | Error recovery emits diagnostics and recovery nodes through a stable `ErrorRecovery` BIR payload. |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| Current Rust emitter | PHF/Pratt/table knowledge in `grammar.rs` (`crates/core/src/backend/rust/emitter/grammar.rs:155-202`). | Split by BIR consumers and runtime template files. | Monolithic emission body and hardwired path plans. |
| Builder template | Arena/checkpoint vocabulary (`crates/core/src/runtime/builder_template.rs:121-210`). | Convert to TapeBuilder length checkpoints. | OpenFrame stack and `Clone` checkpoints. |
| sonic-rs | Direct struct and lazy value lessons (`restart/corpora/SOTA.md:30-44`). | Direct views borrow into Tape. | Direct-only substrate that bypasses Tape. |
| simdjson | Tape and structural scan lessons (`restart/corpora/SOTA.md:73-89`). | Use tape with typed views, not untyped DOM only. | Separate tape from typed values. |

## Wave 2 correction note

This agent's Rust V1 lowerer obligation (agent-2 §6 risk mitigation, line 84 cited by HARDENING-PASS-2 punch item 9) is augmented by PASS-2's PASS-3 consumer acceptance gates at the close of PASS-2.md §4. The emitted parse signatures must compile under PASS-3 API wrappers; document/view metadata must feed visitor + selectors; the materialisation cost table must be generated and documented. The named verification commands at PASS-2.md §4 close are the binding gate; this agent's lowerer obligation tree feeds those gates.
