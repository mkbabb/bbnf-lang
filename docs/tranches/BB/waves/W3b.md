# BB.W3b — E-graph + Miners

**Thesis** Hereupon the e-graph saturation stage (path-dep'd `crates/egraph/`) and the recogniser miners (`recognizers/operator_chain.rs`, `passes/sets/structural_alphabet.rs`) feed the cost model; output piped to W3c's rank/tier rewrites + Pratt/SIMD detection. **Closer-gate** `cargo nextest run -p bbnf-ir --test miner_facts` 100% pass; `crates/egraph/src/` is path-dep'd (verified by `cargo metadata`).

## §1 Deliverable

W3b is the second W3 sub-wave. The e-graph saturation + miner facts production land here; the rewrite-rule application + cost-model integration land at W3c.

Per `audit/MODULES-2026-05-03.md:1218`, the miners exist as files: `crates/ir/src/passes/recognizers/operator_chain.rs`, `crates/ir/src/passes/sets/structural_alphabet.rs`, `crates/ir/src/passes/recognizers/delim_scan.rs`, `crates/ir/src/passes/recognizers/key_dispatch.rs`. W3b wires them as facts producers feeding the cost model:

- `OperatorChainFacts { precedence, associativity, op_set, chain_depth }` — input to Pratt detection at W3c.
- `StructuralAlphabetFacts { alphabet, cardinality, density }` — input to SIMD detection at W3c.
- `DelimScanFacts { delim, range }` — input to delim-skip dispatch.
- `KeyDispatchFacts { phf_table, dispatch_size }` — input to keyword strategy.

The e-graph saturation runs at W3b without rank.rs/tiering.rs (which land at W3c with consumer); W3b establishes the saturation infrastructure for W3c's rewrite-rule landing.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W3b verification | Verify W3a layout-inference output piped | `cargo nextest run -p bbnf-ir --test layout_pipe` passes | W3a baseline holds. |
| M1 | E-graph wiring | The e-graph saturation reads layout output; runs over the EXISTING rewrite-rule set (excludes rank.rs/tiering.rs) | `cargo nextest run -p bbnf-ir --test egraph_saturation_excluding_rank_tier` passes | E-graph operates without rank/tier. |
| M2 | Operator-chain miner | Wire `recognizers/operator_chain.rs` to produce `OperatorChainFacts` for each grammar's IR | `cargo nextest run -p bbnf-ir --test operator_chain_facts` passes | Miner facts produced. |
| M3 | Structural-alphabet miner | Wire `passes/sets/structural_alphabet.rs` to produce `StructuralAlphabetFacts` per grammar | `cargo nextest run -p bbnf-ir --test structural_alphabet_facts` passes | Miner facts produced. |
| M4 | Miner-facts artefact | Land `docs/tranches/BB/audit/W3b-miner-facts.md` recording per-grammar miner facts (cardinality, density, op_set per rule) | `test -f docs/tranches/BB/audit/W3b-miner-facts.md` | W3c reads the facts as input. |

## §3 Closer gate

```sh
cargo metadata --format-version 1 | jq '.workspace_members' | grep -v 'crates/egraph'   # egraph is path-dep
cargo nextest run -p bbnf-ir --test egraph_saturation_excluding_rank_tier --profile ax-iter
cargo nextest run -p bbnf-ir --test operator_chain_facts --test structural_alphabet_facts --profile ax-iter
test -f docs/tranches/BB/audit/W3b-miner-facts.md
```

## §4 Invariants

§I1. **Lock 4** — output-piping continues (e-graph + miners as own crates).
§I2. **Lock 11** — egraph path-dep is exercised.
§I3. **Era V abrogation precondition** — rank.rs / tiering.rs absent at W3b close (W3c creates with consumer).

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| E-graph saturation does not converge without rank/tier rewrites | Low | The existing rewrite-rule set (excluding rank/tier) produces a viable saturation; W3c adds rank/tier as additional rules, not as substitutes. |

## §6 Cross-references

- **Preceding wave**: BB.W3a.
- **Following wave**: BB.W3c.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf-ir --profile ax-iter` | ≤ 50 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W3b-miner-facts.md` | `docs/tranches/BB/audit/` | Per-grammar miner facts for W3c input |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L4, L11 honoured |
| Lane 2 | E-graph + miners are W3c precursors; same-wave (W3c) consumer |
