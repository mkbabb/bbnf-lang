# BC.W1a — Full Rust Emitter Refactor

**Name**: W1a — Full Rust Emitter Refactor (All Nine Grammars)
**Opens after**: BC.W0c close (sibling baseline; AscentStrategy excised; smoke lowerer for CSS L4 proven)
**Hard gate**: per BC02-1 surgery, W1a OWNS the full Rust emitter refactor; the `crates/core/src/codegen/rust/emitter.rs` becomes `lower.rs` consuming `TypedIRNode`; per-shape consumers (struct_direct, dispatcher, alt_dispatch, pratt) consume typed IR; behaviour preserved
**Status**: planned

## §1 Deliverable

Per BC02-1 (`audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:59`), W1 splits into W1a (full refactor) + W1b (regen-equality). W1a is the full Rust emitter refactor across all nine production grammars; W1b is the byte-identical regen-equality verification.

The wave renames `crates/core/src/codegen/rust/emitter.rs` to `lower.rs` consuming `TypedIRNode` per W0a's contract. Existing emitter shapes — struct_direct, dispatcher, alt_dispatch, pratt — refactor to consume typed IR's pre-resolved `Layout` and `TypeDesc` fields. The smoke lowerer from W0b's CSS L4 case extends to all nine grammars.

The W1a refactor is a *rename* + *interface narrowing*, not a behaviour change. The behavioural invariant: every Rust source byte the emitter produces in W1a close must match BB close artefact byte-for-byte. The performance invariant: per-grammar parity tests against sonic-rs / lightningcss / simdjson / cssparser remain green.

## §2 Milestones

### §2.1 Emitter rename

Mechanism: rename `crates/core/src/codegen/rust/emitter.rs` to `lower.rs`. Per `feedback_doc_alongside_code`, update doc references throughout the codegen module.

Files: `crates/core/src/codegen/rust/emitter.rs` → `crates/core/src/codegen/rust/lower.rs` (rename).

Sub-gate: `test -f crates/core/src/codegen/rust/lower.rs && ! test -f crates/core/src/codegen/rust/emitter.rs`.

### §2.2 struct_direct shape consumes typed IR

Mechanism: refactor the struct_direct shape consumer to read `Layout` + `TypeDesc` from typed IR; no re-derivation at emit time.

Files: `crates/core/src/codegen/rust/shapes/struct_direct/{header,body,fields,finalize}.rs` (modify-carve).

Sub-gate: `rg -nE 'use bbnf_ir::types::grammar::IrNode' crates/core/src/codegen/rust/shapes/struct_direct/` returns zero; the shape consumer compiles.

### §2.3 dispatcher shape consumes typed IR

Mechanism: refactor the dispatcher shape to consume `TypedIRNode::AltDispatch::dispatch` (with `AltDispatchKind` variants per `audit/W0-typed-ir-variant-table.md:AltDispatch`); the per-shape emit consumes the dispatch tag from typed IR.

Files: `crates/core/src/codegen/rust/shapes/dispatcher/` (modify-carve).

Sub-gate: dispatcher shape compiles; samply trace shows zero grammar-IR access from the dispatcher emit site.

### §2.4 alt_dispatch shape consumes typed IR

Mechanism: refactor the alt_dispatch shape to consume `TypedIRNode::AltSpeculative::branches`; the per-branch emit consumes typed IR per child.

Files: `crates/core/src/codegen/rust/shapes/alt_dispatch/` (modify-carve).

Sub-gate: CSS L4's largest typed-Alt (14 variants for `Property`) regen-equal to BB close.

### §2.5 Pratt + structural-alphabet shapes consume typed IR

Mechanism: refactor `crates/core/src/codegen/rust/shapes/pratt/` to consume `TypedIRNode::PrattSpine`; refactor structural-alphabet emit to consume `TypedIRNode::SimdScan`.

Files: `crates/core/src/codegen/rust/shapes/pratt/` (modify-carve), `crates/core/src/codegen/rust/shapes/structural_alphabet.rs` (modify-carve).

Sub-gate: BBNF's `binary_factor` (PrattSpine) and JSON's structural alphabet emit (SimdScan) regen-equal to BB close.

### §2.6 EnumDiscriminator emission

Mechanism: per `audit/W0-typed-ir-variant-table.md:EnumDiscriminator` and `feedback_typed_materialization_invariant`, every `->` reaches the typed-emit through EnumDiscriminator. Refactor the typed-enum constructor emit to consume EnumDiscriminator's discriminator kind.

Files: `crates/core/src/codegen/rust/shapes/enum_constructor.rs` (or analogous; modify-carve).

Sub-gate: every `->` arrow in every grammar reaches a tagged emit; the inverse-layout-audit invariant from G05-7 holds.

### §2.7 HostCall emission per per-grammar metadata

Mechanism: per G05-4, host-fn references resolve through per-grammar host metadata at `grammar/<g>/host/`. The HostCall shape emit consumes `TypedIRNode::HostCall::fn_id` and resolves through the registry, never hardcoding grammar names.

Files: `crates/core/src/codegen/rust/shapes/host_call.rs` (modify-carve).

Sub-gate: `rg -nE 'json|css_l4|google_sheets|bbnf' crates/core/src/codegen/rust/shapes/host_call.rs` returns zero literal grammar names; resolution flows through registry only.

### §2.8 Per-grammar smoke regen

Mechanism: for each of the nine grammars, run `cargo xtask regen --grammar=<g> --check`. Each grammar's regen output must be byte-identical to BB close.

Files: per-grammar regen smoke tests at `docs/tranches/BC/audit/W1a-per-grammar-regen.md`.

Sub-gate: nine per-grammar smoke tests pass; the document records each grammar's pre/post checksums.

### §2.9 Per-grammar parity tests

Mechanism: per-grammar parity tests against sonic-rs (JSON), lightningcss (CSS L4 + CSS Pretty), cssparser (Sheets), serde_json (JSON canonical) must remain green.

Files: `crates/core/tests/json_parity.rs`, `crates/core/tests/css_l4_parity.rs`, `crates/core/tests/sheets_parity.rs`, `crates/core/tests/css_pretty_parity.rs`, `crates/core/tests/bbnf_parity.rs` (no modification; assert pass).

Sub-gate: `cargo nextest run -p bbnf --test json_parity --test css_l4_parity --test sheets_parity --test css_pretty_parity --test bbnf_parity` 100% pass.

### §2.10 Performance trajectory check

Mechanism: per BC perf gates, the refactor must not regress JSON twitter, CSS L4 bootstrap, JSON canada timings.

Files: `docs/tranches/BC/audit/W1a-bench-snapshot.json` (create).

Sub-gate: `JsonParser::parse(twitter.json)` ≤ 400 µs (BA-G1 still met); `parse(bootstrap.css)` ≤ 3.5 ms (BB-G1 still met); `parse(canada.json)` ≤ 3.0 ms (BB-G4 still met).

### §2.11 Tape-residue scrub continuity

Mechanism: BA.W0's tape-residue scrub eliminated tape mentions per `audit/CENSUS-2026-05-03.md:38-84`. W1a must not reintroduce tape mentions.

Files: `docs/tranches/BC/audit/W1a-tape-scrub.txt` (create).

Sub-gate: `rg -n 'TapeRec\|TapeCursor\|TapeBuilder\|TapeOffset' crates/core/src/codegen/` returns zero post-refactor.

### §2.12 LOC budget gate

Mechanism: W1a is a refactor; generated LOC must remain BB-close-identical (zero delta). The LOC ceiling is W1b's regen-equality byte gate.

Files: `docs/tranches/BC/audit/W1a-generated-loc.txt` (create).

Sub-gate: per-grammar LOC at W1a close == BB close; net delta = 0%.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W1a-G1 | `emitter.rs` renamed to `lower.rs` | `test -f crates/core/src/codegen/rust/lower.rs && ! test -f crates/core/src/codegen/rust/emitter.rs` |
| W1a-G2 | All shape consumers consume typed IR | `rg -nE 'use bbnf_ir::types::grammar::IrNode' crates/core/src/codegen/rust/shapes/` returns zero |
| W1a-G3 | All nine grammars regen byte-identical | per-grammar W1a §2.8 smoke test passes for each grammar |
| W1a-G4 | All per-grammar parity tests pass | `cargo nextest run -p bbnf --test json_parity --test css_l4_parity --test sheets_parity --test css_pretty_parity --test bbnf_parity` 100% pass |
| W1a-G5 | Performance trajectory met | bench snapshot at `docs/tranches/BC/audit/W1a-bench-snapshot.json` |
| W1a-G6 | No grammar names in codegen | `rg -nE 'json\|css_l4\|google_sheets\|bbnf' crates/core/src/codegen/rust/shapes/` returns zero literal grammar idents (per G05-4) |
| W1a-G7 | LOC delta zero | `docs/tranches/BC/audit/W1a-generated-loc.txt` per-grammar LOC == BB close |

## §4 Invariants

§I1. **Lock 5 (IR + per-backend lower)**: Rust emitter consumes typed IR exclusively at W1a close.

§I2. **Lock 1 (direct-to-struct)**: refactor preserves direct-to-struct emit; no `OpenFrame` resurrection.

§I3. **Lock 9 (slice-borrow primary)**: three-surface API preserved.

§I4. **Behavioural invariance**: byte-for-byte equality with BB close (verified at W1b).

§I5. **No grammar names in codegen** per G05-4.

§I6. **No tape resurrection** (BA.W0 retiral preserved).

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Per-shape consumer refactor introduces structural drift | Medium | Round-trip test from W0b extends to nine grammars; W1a §2.8 per-grammar smoke catches drift |
| CSS L4 14-variant Alt regresses | Medium | W0b's CSS L4 fixture is the worst-case; W1a §2.4 alt_dispatch shape consumer is tested first |
| Pratt emit (BBNF binary_factor) regresses | Low | PrattSpine variant from W0a covers; W1a §2.5 verifies |
| EnumDiscriminator emission misses a `->` | Medium | inverse-layout-audit invariant from G05-7 catches; W1a §2.6 verifies |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W0a→BC.W1a | BC.W0a | Typed IR alphabet + contract spec |
| BC.W0b→BC.W1a | BC.W0b | Smoke lowerer pattern (CSS L4) extends to nine grammars |
| BB→BC.C2 | BB.W1 | Direct-to-struct emit shape grammar-agnostic |

### BC-G gates closed

| Gate | Closure |
|---|---|
| BC-G4 (full) | Rust emitter consumes only typed IR (W1a-G2); the contract is the boundary by mechanism |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W1a→BC.W1b | BC.W1b | Refactored lowerer is the input to regen-equality verification |
| BC.W1a→BC.W2 | BC.W2 | Per-shape consumer pattern is the reference for TS + WASM scaffolds |

## §7 Iter-time check

| Activity | Pre-W1a wall | Post-W1a wall | Notes |
|---|---:|---:|---|
| `cargo check -p bbnf` | ~22 s | ~21 s | typed-IR consumers compile faster (pre-resolved fields) |
| `cargo xtask regen --check` | ≤ 23 s | ≤ 23 s | unchanged at byte level |
| `cargo nextest run -p bbnf` | ~38 s | ~38 s | parity tests unchanged |

## §8 Dependencies

- **Depends on**: BC.W0c close (clean baseline; AscentStrategy excised).
- **Blocks**: BC.W1b (regen-equality byte verification), BC.W2 (TS + WASM scaffolds).

## §9 Closing posture

W1a is the rename + narrow + verify across all nine grammars. The per-shape consumer refactor is disjoint per shape (struct_direct, dispatcher, alt_dispatch, pratt) and tested per grammar. The IR contract from W0a is the boundary; the Rust emitter consumes typed IR exclusively at W1a close.
