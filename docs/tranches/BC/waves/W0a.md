# BC.W0a — IR Contract Specification (Spec + Variant Table)

**Name**: W0a — IR Contract Specification
**Opens after**: BB.W6 close (BB tranche complete; carry-tags BB→BC.C1, BB→BC.C2 consumed; BA→BC.C1, BA→BC.C2 consumed)
**Hard gate**: `docs/codegen-IR-CONTRACT.md` lands; the typed IR alphabet at `crates/ir/src/typed_ir/` exists with all 22 variants from `audit/W0-typed-ir-variant-table.md`; the contract is the spec, not the consumer
**Status**: planned

## §1 Deliverable

Per BC02-1 (`audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:59`), W0 splits into three sub-waves: W0a is **contract spec only**, W0b is **smoke test for the contract**, W0c is **sibling baseline + AscentStrategy disposition**. W0a does NOT refactor the Rust emitter; that ownership belongs to BC.W1a per surgery 8 (BC.W1 owns the full Rust emitter refactor).

W0a publishes `docs/codegen-IR-CONTRACT.md` containing the 22-variant alphabet from `audit/W0-typed-ir-variant-table.md`, the Lifetime / Layout resolution rules per variant, and the per-backend lowering rules (Rust / TS / WASM columns). The typed IR module at `crates/ir/src/typed_ir/` lands as the normative type-level specification; lowering from `IrNode` to `TypedIRNode` is implemented; the round-trip property holds for the existing nine grammars at smoke level (BC.W0b verifies).

The contract is the BC.W1a refactor's input, not its output. The contract is a publication.

## §2 Milestones

### §2.1 Contract document drafted

Mechanism: draft `docs/codegen-IR-CONTRACT.md` reproducing `audit/W0-typed-ir-variant-table.md`'s 22-variant table with cross-references to the lowering pass sites. The document includes: (a) a §1 Cardinality justification citing prior-art per `audit/research-anchors.md:§1`; (b) a §2 Per-variant table with every column from W0-typed-ir-variant-table.md; (c) a §3 Layout resolution rules naming the resolution per variant; (d) a §4 Per-backend lowering rules (Rust / TS / WASM); (e) a §5 Errors and friction section per Lane 7.

Files: `docs/codegen-IR-CONTRACT.md` (create).

Sub-gate: every variant from `audit/W0-typed-ir-variant-table.md` appears in the contract document with all columns; cross-reference to W0-typed-ir-variant-table.md verified; voice is archaic-permissive per V1.

### §2.2 Typed IR module structure

Mechanism: create `crates/ir/src/typed_ir/{lib,node,lower,layout_resolve}.rs` (post-BA.W2 directory module pattern per `feedback_directory_modules`). `node.rs` defines the `TypedIRNode` enum with all 22 variants per `audit/W0-typed-ir-variant-table.md:§2`. `lower.rs` defines `IrNode → TypedIRNode` lowering. `layout_resolve.rs` defines the layout resolution rules per variant.

Files: `crates/ir/src/typed_ir/{mod,node,lower,layout_resolve}.rs` (create).

Sub-gate: `cargo check -p bbnf-ir` green with the new `typed_ir/` module; the 22-variant `TypedIRNode` enum compiles; each variant carries its declared payload type per the table.

### §2.3 Layout pass anchor

Mechanism: per `audit/W0-typed-ir-variant-table.md` Layout variant, `Layout` is a *pass anchor* not a runtime variant; the layout-lowering pass at `crates/ir/src/passes/layout/` (post-surgery 4 BA.W2 rename from `passes/types/`) consumes Layout markers and emits resolved layouts on every other variant. W0a verifies the anchor's role through the type system: a typed IR with a remaining Layout marker fails compilation in a debug-assertion guard.

Files: `crates/ir/src/typed_ir/layout_resolve.rs` (defines the resolution function), `crates/ir/src/passes/layout/mod.rs` (consumer; updated to consume the marker; this is part of BA.W2's relocation).

Sub-gate: `cargo check -p bbnf-ir` green; the layout-lowering pass eliminates Layout markers post-resolve; round-trip test (W0b) verifies no markers remain in lowered output.

### §2.4 No `TypeDesc` separate term

Mechanism: per surgery 4, `TypeDesc` does not appear as a separate canonical IR term. If a type descriptor remains, it is a *field* of `Layout`. The contract spec at `docs/codegen-IR-CONTRACT.md` uses `Layout` vocabulary throughout; `TypeDesc` appears only as a field name within Layout's enum variants, not as a separate type.

Files: `docs/codegen-IR-CONTRACT.md` (verify), `crates/ir/src/types/layout.rs` (verify).

Sub-gate: `rg -nE 'TypeDesc' docs/codegen-IR-CONTRACT.md crates/ir/src/typed_ir/` returns matches only as field names within `Layout` definitions, never as a separate canonical term.

### §2.5 Host-fn metadata anchor (per G05-4)

Mechanism: per G05-4 (`audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:27`), the contract spec names that host mappings are read from per-grammar host metadata emitted by `bbnf-parse`; `bbnf-codegen` never hardcodes grammar names. The HostCall and MapExpr variants reference per-grammar metadata at lower time.

Files: `docs/codegen-IR-CONTRACT.md` (verify the §HostCall and §MapExpr sub-sections cite per-grammar metadata; specifically `bbnf-parse::generated::<g>::host_table`).

Sub-gate: the contract spec includes a §HostCall.metadata-resolution section naming `grammar/<g>/host/` as the per-grammar metadata source; `bbnf-codegen` consumes this through a generic registry interface.

### §2.6 Friction-forecast for IR contract syntax

Mechanism: per Lane 7, the contract spec must anticipate user friction. Forecast points: (1) `TypedIRNode` vs `IrNode` distinction; (2) when each is used; (3) `Layout` markers vs resolved Layout; (4) per-backend lowering idiom variation.

Files: `docs/codegen-IR-CONTRACT.md` (§5 Errors and friction).

Sub-gate: §5 includes at least three verbatim error messages the lowerer emits; cross-reference to `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md` table F07-E4 honoured.

### §2.7 LOC budget gate

Mechanism: per Lane 6, W0a is spec-only; generated LOC must NOT change. Verify pre-W0a generated LOC matches post-W0a generated LOC (zero delta).

Files: `docs/tranches/BC/audit/W0a-generated-loc.txt` (create — captured at W0a open vs close).

Sub-gate: `wc -l crates/core/src/grammar/generated/*.rs` (or post-BA.W3 path) at W0a open == at W0a close; net delta = 0.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W0a-G1 | Contract document lands with all 22 variants | `test -f docs/codegen-IR-CONTRACT.md && grep -c 'Rule\|Seq\|Optional\|Ref\|AltDispatch\|AltSpeculative\|Repeat\|CharClass\|Keyword\|Lit\|Scanner\|HostCall\|MapExpr\|FoldResult\|Span\|Layout\|PrattSpine\|SimdScan\|ErrorRecovery\|DebugMarker\|RegexDfa\|EnumDiscriminator' docs/codegen-IR-CONTRACT.md` ≥ 22 |
| W0a-G2 | `crates/ir/src/typed_ir/` module compiles | `cargo check -p bbnf-ir` green |
| W0a-G3 | TypedIRNode enum has all 22 variants | `rg -c 'pub enum TypedIRNode\|^\s+\w+\s*\{' crates/ir/src/typed_ir/node.rs` reports 22 variants |
| W0a-G4 | No separate `TypeDesc` canonical term | `rg -nE 'pub (enum\|struct) TypeDesc\b' crates/ir/src/typed_ir/` returns zero (TypeDesc may appear as a field name within Layout, never as a separate term) |
| W0a-G5 | LOC budget unchanged | `docs/tranches/BC/audit/W0a-generated-loc.txt` shows zero delta |
| W0a-G6 | Friction-forecast section complete | §5 of contract has ≥ 3 verbatim error messages |

## §4 Invariants

§I1. **Lock 5 (IR contract)**: the contract is published in W0a; the consumer (Rust emitter refactor) does NOT land here per BC02-1.

§I2. **Lock 2 (Layout canon)**: the contract uses `Layout`/`LayoutSink` exclusively; no `TypeDesc` as separate canonical term per surgery 4.

§I3. **Same-wave consumer**: the consumer of W0a is W0b's smoke test; per BC02-1 the full Rust emitter refactor consumer is W1a, but W0b's smoke test is the same-wave consumer of the contract spec, structurally precluding Era V.

§I4. **No metalanguage** in the contract document.

§I5. **Voice**: archaic-permissive per §V1.

§I6. **Per-grammar host metadata**: contract spec names `grammar/<g>/host/` as per-grammar host source per G05-4.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| The 22-variant alphabet is incomplete (a real grammar feature has no variant) | Low | Variant table defended against rustc HIR ExprKind (35) and Cranelift InstructionData (40) per `audit/research-anchors.md:§1`; nine production grammars cover the variant set |
| The Layout marker / resolution discipline is too subtle for consumers | Medium | §5 friction-forecast has verbatim error messages; the typed IR's compile-time guard catches missing resolution in debug builds |
| Contract document references retired terms (`TypeDesc`, `type_projection`) | Low | W0a-G4 grep ensures zero retired terms; cross-reference to BA.W2 layout canon per L2 |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BA→BC.C1 | BA.W2 | Layout-lowering canon supports the contract spec |
| BA→BC.C2 | BA.W5 | Direct-to-struct emitter pattern (one IR walker, leaf emission per backend) is the precursor to the contract |
| BB→BC.C1 | BB.W3 | Optimiser composition is output-piped; the contract specifies the optimiser-output / lowerer-input boundary |
| BB→BC.C2 | BB.W1 | Direct-to-struct emit shape is grammar-agnostic; the contract formalises the IR-input / typed-IR-output |

### BC-G gates closed

| Gate | Closure |
|---|---|
| BC-G4 (partial) | The contract is documented at `docs/codegen-IR-CONTRACT.md` (W0a-G1); the Rust emitter consumption gate is W1a's responsibility; W0a closes the document side of BC-G4 |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W0a→BC.W0b | BC.W0b | The contract is the input to the smoke test |
| BC.W0a→BC.W1a | BC.W1a | The contract is the input to the full Rust emitter refactor |

## §7 Iter-time check

| Activity | Pre-W0a wall | Post-W0a wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf-ir` | ~8 s | ~10 s | new typed_ir/ module compile cost |
| `cargo check -p bbnf` | ~22 s | ~22 s | unchanged (no consumer refactor here) |
| `cargo xtask regen --check` | ≤ 23 s | ≤ 23 s | unchanged |

## §8 Dependencies

- **Depends on**: BB.W6 close; BA.W2 + BA.W5 closures (Layout canon, direct-to-struct pattern).
- **Blocks**: BC.W0b (smoke test consumer); BC.W1a (full emitter refactor consumer).

## §9 Closing posture

W0a publishes the contract, lands the typed IR module, and stops. The consumer is W0b (smoke) and W1a (full refactor); ownership splits per BC02-1. The contract is a publication; the refactor is the next wave.
