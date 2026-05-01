# AX.W0a — Gate Repair + Routing

**Opens after**: AW-V.W6 close
**Agents**: 2 serial (gate narrowing, then routing + wire-contract)
**Hard gate**: Every grammar's `parse()` routes through shape dispatcher; `gate_predicate_wire_contract.rs` asserts per-grammar predicate outcomes.

## Scope

1. Narrow `has_w4_classified(ir)` to return true only for grammars with `ShapeTag::Pratt | ShapeTag::Unordered` rules (not Flat/Wrap/ArgList/HRegex). Rationale: `V-audit-overfit.md` §Gate-pathology — Flat/Wrap/ArgList/HRegex bodies invoke only W3-bound visitor methods (`.string()`, `.number()`, `.begin_object()` etc.); only Pratt/Unordered need W4-bound traits (`PrattVisitor`, bespoke).
2. Emit entry-shape `parse()` routing for non-Alt-rooted grammars. CSS `stylesheet` (OW-wrapped Array), Sheets `formula` (Seq), BBNF `grammar` (Repeat) each need `parse()` to tail-call the per-shape emitted function directly, bypassing `dta_run_<grammar>`.
3. Land `gate_predicate_wire_contract.rs` — a harness that asserts, for every grammar × every gate predicate, the predicate's boolean output. Freeze the contract per invariant 9.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/backend/rust/emitter/grammar.rs` | modify (narrow `has_w4_classified` at line 718; emit non-Alt-rooted routing at line 515 branch) |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs` | modify (line 836 gate usage) |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | modify (line 149 gate usage) |
| `crates/core/tests/gate_predicate_wire_contract.rs` | create |
| `crates/core/src/grammar/generated.rs` | regen (bootstrap output) |

Do NOT touch: any file under `crates/core/src/backend/rust/emitter/dta_walker/`, `dta.rs`, or `bbnf-tape/src/driver.rs` — those deletions belong to W0b.

## Phase sub-items

### W0a.1 Narrow `has_w4_classified`

Change from "any rule with any W4-shape" to `ir.rules.iter().any(|r| matches!(shape, Pratt | Unordered))`. Verify via `cargo expand -p bbnf --bin json_monolithic_value` that the visitor-path emission re-activates for JSON.

### W0a.2 Emit non-Alt-rooted `parse()` routing

For each of CSS L4, Sheets, BBNF: detect entry-rule shape at `grammar.rs:515`. When `has_shape_dispatcher_entrypoint(ir)` returns true and entry is non-Alt-rooted (`Seq` / `Repeat` / `OptionalWhitespace`), emit `parse()` body that calls `parse_<entry_shape>_<grammar>_<entry_rule>` directly — not `dta_run_<grammar>`. Bootstrap regen idempotent.

### W0a.3 `gate_predicate_wire_contract.rs`

For each gate predicate (`has_w4_classified`, `has_full_shape_coverage`, `has_shape_dispatcher_entrypoint`), assert its output per grammar (JSON / CSS L4 / Sheets / BBNF / EBNF / BNF / BbnfBootstrap). Tests encode the FROZEN outputs — any subsequent wave that widens a predicate must amend this test, which forces visibility.

## Hard gate

1. `cargo bench -p bbnf --bench json_monolithic_value` compiles and reaches ≥ 0.98× `json-prototype` (per `AW-V-W2-close.md` reference numbers).
2. `nm target/release/deps/{css_l4,google_sheets,bbnf}_monolithic-*` shows no `parse()` entry path reaching `__dta_walker_inline::run`.
3. `gate_predicate_wire_contract.rs` lands with per-grammar × per-predicate assertions; `cargo test --workspace` green.
4. Bootstrap regen idempotent; diff empty on second run.

## Verification artefacts

- `nm` output proving walker not reached from any `parse()` entry.
- `cargo expand` diff showing `parse_with_visitor` re-emitted for JSON.
- Wire-contract test passes asserting per-grammar gate outcomes.
- Bench bench artefact `docs/benchmarks/archive/post-AX-W0a-close.json` showing JSON entries recovered to prototype range and non-JSON grammars unchanged (walker still routed but via shape-dispatcher entry, not `parse()` → `dta_run`).

## Dependencies

- Depends on: AW-V.W6 close (the diagnosed regression).
- Blocks: **every other AX wave.** Deleting the walker at W0b before W0a closes fails `cargo test` immediately because the walker is currently the only path 12/17 entries reach.

## Archaeology

V's single diagnosed regression per `V-audit-overfit.md`. The `has_w4_classified` gate was introduced in AW-V W3-fix-bench for visitor-path emission; widened silently by W4-fix-rest (commits `569c17e4` / `ce2fd9f6`) admitting Flat/Wrap for JSON, collapsing the W3-close bench to non-compile by W6. This wave closes the diagnostic; no speculative scope.
