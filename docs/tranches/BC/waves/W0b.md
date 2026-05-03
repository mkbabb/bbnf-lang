# BC.W0b — Rust Lowerer Smoke Test

**Name**: W0b — Rust Lowerer Smoke Test
**Opens after**: BC.W0a close (contract spec + typed IR module landed)
**Hard gate**: a smoke-level Rust lowerer consumes typed IR and produces byte-identical output for one canonical grammar (CSS L4) against the BB close artefact; the smoke test is the same-wave consumer of W0a's contract
**Status**: planned

## §1 Deliverable

W0b lands a *minimal* Rust lowerer that consumes `TypedIRNode` and emits Rust source for ONE grammar (CSS L4 — the most complex grammar per `audit/MODULES-2026-05-03.md:622`). The smoke is structural: it proves the contract supports a real grammar's lowering by mechanism, not by spec only. Per BC02-1, the *full* Rust emitter refactor across all nine grammars is BC.W1a; W0b is the contract's smoke-level proof.

This sub-wave structurally precludes Era V's substrate-first/consumer-later anti-pattern: the contract from W0a has its consumer (smoke lowerer) here, in the next wave.

## §2 Milestones

### §2.1 CSS L4 round-trip fixture capture

Mechanism: capture the BB-close generated `crates/core/src/grammar/generated/css_l4.rs` (or post-BA.W3 path) as a snapshot fixture. The fixture is the input for the round-trip test.

Files: `crates/core/tests/fixtures/bb-close-css-l4-snapshot.rs` (create), `crates/core/tests/typed_ir_round_trip.rs` (create).

Sub-gate: the fixture file is byte-identical to BB close artefact at W0b open; captured before any W0b refactoring.

### §2.2 IrNode → TypedIRNode lowering for CSS L4

Mechanism: implement `bbnf_ir::typed_ir::lower::lower_grammar_ir(&grammar_ir, &layout) -> TypedGrammarIR` for the CSS L4 grammar IR. The lowering is one-to-one for most variants per `audit/W0-typed-ir-variant-table.md:§6`; layout markers are eliminated by `bbnf_ir::passes::layout::resolve` post-lowering.

Files: `crates/ir/src/typed_ir/lower.rs` (modify-carve to handle CSS L4's variants).

Sub-gate: `cargo check -p bbnf-ir` green; the lowering function compiles; CSS L4 grammar IR lowers to typed IR without panic.

### §2.3 Smoke Rust lowerer for CSS L4

Mechanism: implement `bbnf_codegen::rust::SmokeRustLowerer` (a minimal lowerer; not the production refactor) that consumes typed IR for CSS L4 and emits Rust source. The smoke lowerer is intentionally narrow — it covers the variants CSS L4 uses (Rule, Seq, AltDispatch, Repeat, Optional, CharClass, Keyword, Lit, Scanner, Ref, HostCall, MapExpr, EnumDiscriminator, Layout, Span). The smoke output is byte-identical to BB close artefact for CSS L4.

Files: `crates/core/src/codegen/rust/smoke_lower.rs` (create — or post-W3c path `crates/bbnf-codegen/src/rust/smoke_lower.rs`).

Sub-gate: `cargo check -p bbnf` green; the smoke lowerer compiles; calling `SmokeRustLowerer::lower(typed_grammar_ir)` for CSS L4 produces a `TokenStream` whose pretty-printed form is byte-identical to the W0b §2.1 fixture.

### §2.4 Round-trip equality test

Mechanism: implement `crates/core/tests/typed_ir_round_trip.rs` for CSS L4. The test asserts: `lower_grammar_ir(&grammar_ir, &layout) → typed_ir → SmokeRustLowerer::lower(&typed_ir) → tokens` produces byte-identical output to BB close artefact.

Files: `crates/core/tests/typed_ir_round_trip.rs` (modify-carve to add CSS L4 case).

Sub-gate: `cargo nextest run --test typed_ir_round_trip` 100% pass for CSS L4; no other grammars covered (those are W1a).

### §2.5 Era V disposition record

Mechanism: per `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:7-10`, document the W0b same-wave-consumer mechanism explicitly. The W0a contract has its smoke consumer in W0b; the W0a substrate cannot land without a W0b consumer. The Era V anti-pattern is structurally precluded.

Files: `docs/tranches/BC/audit/W0b-era-v-disposition.md` (create).

Sub-gate: the document names the W0a → W0b same-wave-consumer mechanism; cross-reference verified.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W0b-G1 | CSS L4 round-trip fixture captured | `test -f crates/core/tests/fixtures/bb-close-css-l4-snapshot.rs` |
| W0b-G2 | Smoke Rust lowerer compiles | `cargo check -p bbnf` green |
| W0b-G3 | Round-trip equality for CSS L4 | `cargo nextest run --test typed_ir_round_trip` 100% pass |
| W0b-G4 | Era V disposition recorded | `docs/tranches/BC/audit/W0b-era-v-disposition.md` exists |
| W0b-G5 | LOC unchanged | `wc -l crates/core/src/grammar/generated/css_l4.rs` matches BB close |

## §4 Invariants

§I1. **Same-wave consumer**: W0b is W0a's same-wave consumer; Era V structurally precluded.

§I2. **Lock 5 (IR + per-backend lower)**: the smoke lowerer consumes typed IR; the contract supports CSS L4 by mechanism.

§I3. **Smoke not production**: the smoke lowerer is *minimal*; the production lowerer (full nine grammars; the rename from `emitter.rs` to `lower.rs`; the per-shape consumer refactor) is W1a.

§I4. **Regen-equality preserved**: byte-identical to BB close artefact for CSS L4; no behavioural drift.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| The smoke lowerer is too minimal and misses a CSS L4 variant | Medium | Walk the CSS L4 grammar IR pre-W0b; enumerate every variant; W0b §2.3 covers all observed variants |
| The IrNode → TypedIRNode lowering at `lower.rs` introduces structural drift | Medium | Round-trip test catches drift; layout-pass debug-assert catches missing resolution |
| The fixture diverges between W0b and W1a (BB close changes between waves) | Low | Fixture is captured at W0b open; the W1a regen-equality gate uses the same BB close snapshot |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W0a→BC.W0b | BC.W0a | The typed IR alphabet is the input |

### BC-G gates closed

| Gate | Closure |
|---|---|
| BC-G4 (smoke) | The Rust emitter consumes typed IR for CSS L4; the contract supports a real grammar by mechanism (smoke level; full coverage at W1a) |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W0b→BC.W1a | BC.W1a | The smoke lowerer pattern is the reference for the full nine-grammar refactor |

## §7 Iter-time check

| Activity | Pre-W0b wall | Post-W0b wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf` | ~22 s | ~22 s | smoke lowerer adds < 1 s |
| `cargo nextest run --test typed_ir_round_trip` | n/a | ~3 s | new round-trip test |
| `cargo xtask regen --check` | ≤ 23 s | ≤ 23 s | unchanged (W0b does not regen all grammars) |

## §8 Dependencies

- **Depends on**: BC.W0a close (typed IR module + contract spec).
- **Blocks**: BC.W0c (sibling baseline; same wave as W0b but with disjoint touchpoints), BC.W1a (full Rust emitter refactor; W0b's smoke lowerer pattern is the reference).

## §9 Closing posture

W0b proves the contract by mechanism for CSS L4. The smoke lowerer is intentionally minimal; W1a does the production refactor. Era V's substrate-without-consumer anti-pattern is structurally precluded by the W0a → W0b same-wave-consumer relationship.
