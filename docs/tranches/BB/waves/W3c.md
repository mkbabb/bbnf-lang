# BB.W3c — Rank/Tier Rewrites + Pratt/SIMD Detection With Same-Wave Consumer

**Thesis** Hereupon `crates/ir/src/rewrites/{rank.rs, tiering.rs}` create here with the consumer (cost-model + CSP-strategy pipeline at `crates/ir/src/passes/csp_strategy/mod.rs`) wired in the SAME commit per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:118-127`; Pratt detection at `recognizers/operator_chain.rs` and SIMD detection at `passes/sets/structural_alphabet.rs` extend with cost-model integration; perf gates BB-G1, BB-G2, BB-G3, BB-G4 close. **Closer-gate** BB-G6 + BB-G1 + BB-G3 + BB-G4 met; BB-G2 met OR routed-to-BC.W5 with named hypothesis; `git log -1 --stat -- crates/ir/src/rewrites/rank.rs` shows consumer in SAME commit; substrate_audit green.

## §1 Deliverable

W3c is the third W3 sub-wave and the dominant perf-gate closure for BB. The rank/tier rewrites land here with same-wave consumer per the Era V abrogation; Pratt + SIMD detection wire to the cost model; the four parse-throughput gates close.

Per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-127` and the artefact at `docs/tranches/BB/audit/W3-rank-tier-with-consumer.md`, rank.rs + tiering.rs creation MUST land in the same commit as the consumer wiring at `crates/ir/src/passes/csp_strategy/mod.rs`. The atomicity gate is mechanical: `git log -1 --stat -- crates/ir/src/rewrites/rank.rs` shows the consumer modified in the SAME commit; pre-commit verification rejects any commit that does not contain both files.

The Pratt + SIMD detection per `docs/optimizer/pratt-simd-detection.md`:
- Pratt detection at `recognizers/operator_chain.rs` walks IR for left-recursive operator-chain shape; emits `OperatorChainFacts`; cost model decides Pratt vs recursive-descent.
- SIMD detection at `passes/sets/structural_alphabet.rs` mines structural alphabet; emits `StructuralAlphabetFacts`; cost model decides SIMD vs scalar.

The perf gates:
- BB-G1: `parse(bootstrap.css)` ≤ 3.5 ms (lightningcss 4.16 ms × 0.85; parse-only re-measurement at W0a).
- BB-G2: `parse(tailwind.css)` ≤ 35 ms (lightningcss 43.37 ms × 0.82).
- BB-G3: `parse(citm_catalog.json)` ≤ 800 µs (sonic-rs 854 µs × 0.94).
- BB-G4: `parse(canada.json)` ≤ 3.0 ms (sonic-rs 3.144 ms × 0.955).

The F4 Tailwind disposition per surgery 32 + D08-13: if BB-G2 misses, the gap routes to BC.W5 with named path-shape rewrite hypothesis — NOT a silent carry. The artefact `W3c-perf-routing.md` records the hypothesis if routing fires.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W3c substrate audit | Verify W0a rank/tier absence held until now; the W3b miner facts are produced | `test ! -f crates/ir/src/rewrites/rank.rs && test ! -f crates/ir/src/rewrites/tiering.rs && cargo nextest run -p bbnf-ir --test miner_facts` passes | Substrate precondition holds. |
| M1 | rank.rs creation + consumer (atomic commit) | Create `crates/ir/src/rewrites/rank.rs` (~250 LOC); in the SAME commit, wire `crates/ir/src/passes/csp_strategy/mod.rs` to read `compute_rank_facts` | `git log -1 --stat -- crates/ir/src/rewrites/rank.rs` shows csp_strategy modified in SAME commit; `cargo nextest run -p bbnf-ir --test rank_facts` passes | rank.rs + consumer atomic. |
| M2 | tiering.rs creation + consumer (atomic commit) | Create `crates/ir/src/rewrites/tiering.rs` (~200 LOC); same commit consumer | same atomicity gate | tiering.rs + consumer atomic. |
| M3 | Pratt detection + emission | Wire `recognizers/operator_chain.rs` to feed cost model; extend `crates/core/src/codegen/rust/emitter/shapes/pratt/struct_direct.rs` to emit Pratt fns | `cargo nextest run -p bbnf-ir --test pratt_auto_detection` passes; BBNF's `binary_factor` routes to Pratt | Pratt emerges from grammar shape. |
| M4 | SIMD detection + emission | Wire `passes/sets/structural_alphabet.rs` mining output to simd-scan kernel selector; cost model `simd_threshold_bytes` is grammar-derived | `cargo nextest run -p bbnf-ir --test simd_auto_detection` passes; JSON drives SIMD; CSV routes to scalar | SIMD emerges from grammar shape. |
| M5 | Pipeline output-piping verification | The pipeline at step [8]→[9]→[10]→[11]→[12] composes by output-piping; trait-surface coupling only | `cargo nextest run -p bbnf-ir --test pipeline_output_piping` passes | Lock 4 honoured by mechanism. |
| M6 | Perf gate verification | Run benchmarks against lightningcss + sonic-rs anchors | BB-G1 ≤ 3.5 ms; BB-G2 ≤ 35 ms (or routed); BB-G3 ≤ 800 µs; BB-G4 ≤ 3.0 ms | Perf gates close. |
| M7 | substrate_audit green | `crates/ir/src/passes/tests/substrate_audit.rs` reports zero zero-caller substrates | `cargo nextest run -p bbnf-ir --test substrate_audit` passes | Era V abrogation gate green. |
| M8 | F4 Tailwind disposition | If BB-G2 met: F4 closed at W3c. If BB-G2 missed: route-to-BC.W5 with named path-shape rewrite hypothesis at `docs/tranches/BB/audit/W3c-tailwind-routing.md` | Either gate met OR routing artefact lands | Lane 8 carry discipline honoured. |
| M9 | Rank/tier+consumer artefact | Land `docs/tranches/BB/audit/W3c-rank-tier-with-consumer.md` recording the atomic-commit history | `test -f docs/tranches/BB/audit/W3c-rank-tier-with-consumer.md` | Era V abrogation evidence lands. |

## §3 Closer gate

```sh
test -f crates/ir/src/rewrites/rank.rs                                              # rank.rs exists
test -f crates/ir/src/rewrites/tiering.rs                                           # tiering.rs exists
git log --oneline --diff-filter=A crates/ir/src/rewrites/rank.rs | head -1          # commit also touches consumer
git log --oneline --diff-filter=A crates/ir/src/rewrites/tiering.rs | head -1       # same
rg -n '@pratt' grammar/                                                              # zero hits
rg -n '@simd' grammar/                                                               # zero hits
cargo nextest run -p bbnf-ir --profile ax-iter                                      # 100% pass
cargo nextest run -p bbnf-ir --test substrate_audit                                 # green
cargo bench -p bbnf -- css_l4_bootstrap                                              # ≤ 3.5 ms (BB-G1)
cargo bench -p bbnf -- css_l4_tailwind                                               # ≤ 35 ms (BB-G2 or routed)
cargo bench -p bbnf -- json_citm                                                     # ≤ 800 µs (BB-G3)
cargo bench -p bbnf -- json_canada                                                   # ≤ 3.0 ms (BB-G4)
test -f docs/tranches/BB/audit/W3c-rank-tier-with-consumer.md                       # artefact lands
```

## §4 Invariants

§I1. **Lock 4** — per-domain orthogonal optimisation; no fusion.
§I2. **Lock 10** — Pratt + SIMD auto-detected; no `@pratt`, `@simd` directives.
§I3. **Lock 8** — surpass sonic-rs / simdjson / lightningcss; gates name competitor + dataset + platform.
§I4. **Lock 5** — IR + per-backend lower; rank/tier are IR-level concerns consumed by codegen.
§I5. **Era V abrogation** — atomic-commit discipline for rank.rs + tiering.rs creation alongside consumer.
§I6. **Lock 11** — sister crates (egraph, csp-solver, bbnf-regex) consumed by the pipeline.

## §5 Risks

Per BB.md `## Risks + mitigations` table — Pratt false-positive, SIMD overhead, atomicity violation, fusion temptation, BB-G2 miss.

Atomicity violation mitigation: pre-commit gate runs `git diff --cached --stat` and asserts both files appear; the W3c commit body MUST list rank.rs + consumer + tiering.rs + consumer in the same commit.

## §6 Cross-references

- **BB-G gates closing**: BB-G1, BB-G2, BB-G3, BB-G4, BB-G6, BB-G10.
- **Carry-tags consumed**: BA→BB.C2, BA→BB.C5.
- **Carry-tags produced**: BB→BC.C1.
- **Preceding wave**: BB.W3b.
- **Following wave**: BB.W4a.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf-ir --profile ax-iter` | ≤ 50 s |
| `cargo bench -p bbnf -- css_l4_bootstrap css_l4_tailwind json_citm json_canada` | ≤ 12 s |
| `cargo nextest run -p bbnf-ir --test substrate_audit --profile ax-iter` | ≤ 6 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W3c-rank-tier-with-consumer.md` | `docs/tranches/BB/audit/` | Atomic-commit history of rank.rs + tiering.rs creation alongside consumer |
| `W3c-pratt-detection.md` | same | Per-grammar enumeration of operator chains; positive (BBNF binary_factor) + negative cases |
| `W3c-simd-detection.md` | same | Per-grammar structural alphabet + cardinality; threshold computation |
| `W3c-perf-gate-measurements.json` | same | The four BB-G perf gate measurements + SOTA anchor reference numbers |
| `W3c-substrate-audit.md` | same | The substrate_audit output post-W3c (zero zero-caller substrates) |
| `W3c-cost-model-pipe.md` | same | Output-piping documentation between CSP → e-graph → miners → CSP strategy → cost model |
| `W3c-tailwind-routing.md` | same (conditional) | If BB-G2 misses: named path-shape rewrite hypothesis for BC.W5 |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L4, L8, L10 honoured |
| Lane 2 | M1 + M2 atomic commits close Era V abrogation |
| Lane 4 | BB-G1 + BB-G3 + BB-G4 close at SOTA-anchored targets; BB-G2 close OR routed |
| Lane 6 | Specialised grammars +1-3% from W3a (Pratt + SIMD dispatch emit) per surgery 21 |
| Lane 8 | Tailwind routing per F4 disposition; no silent carry |
