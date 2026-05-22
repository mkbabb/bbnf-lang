# SK-V13 W6 Research - E-Graph + Active Cost

Cycle: W6 Research. Scope: read-only research for SPEC Section 9.

## Authority

W6 is `Decision Fold B: E-Graph + Active Cost`. Entry gate: W5 admitted.

Owner paths named by SPEC Section 9:

- `skinny/crates/ir/src/cost.rs`.
- `skinny/crates/passes/src/`.
- codegen lowering paths named by plan.

The exit gate requires bounded e-graph telemetry, deterministic active-cost
selection, stale cost rate <=30%, JSON/CSS guard maintenance, and a same-wave
generated backend-selection consumer. E-graph/cost telemetry alone rejects.

## Six-Agent Fan-Out

Six read-only agents inspected disjoint surfaces:

1. Root `crates/egraph` feasibility and skinny coupling.
2. Backend expression rewrite candidates.
3. Active-cost stale-rate and deterministic extraction.
4. Generated backend-selection consumer surface.
5. Gate/report/Lock 14/REDRESS constraints.
6. Abrogate thresholds and guard commands.

All six converged on the same main fact: W6 can replace the current passive
decision point with a bounded active selector, but current generated JSON/CSS
runtime emission does not materially render an extracted candidate into row
code. A redress that lands only telemetry or passive facts must therefore
record a measured architectural block rather than a row admit.

## Existing E-Graph Surface

`crates/egraph/` is a generic root-workspace crate. It exports `Language`,
`EGraph`, `Rewrite`, `RewriteFn`, `BackoffScheduler`, `RunReport`, `Extractor`,
and `CostModel`. This is the local equivalent of the W6 active-cost surface;
the crate is not `egg`, and its extraction trait is `CostModel`, not
`egg::CostFunction`.

Useful telemetry already exists:

- `RunReport` exposes iterations, applied rewrites, final node/class counts,
  saturation/limit status, and per-rule work.
- `CostConfig` exposes node and iteration caps.
- Memory is not directly exposed and must be estimated or measured by the W6
  report.

The root `crates/ir/src/egraph/` grammar wrapper is pattern evidence only. W6
must not import the root IR into skinny. If redress uses the generic egraph
crate, it should do so through a direct skinny path dependency and a skinny-local
backend expression language.

Coupling risks:

- `crates/egraph` is edition 2024 while skinny declares edition 2021 and
  `rust-version = "1.78"`.
- `crates/egraph` depends on `csp-solver = "0.1"`; W6 should make that path or
  inherited patch behavior explicit if it imports the crate.

## Current Decision Seam

The narrow W6 seam is in `passes`, not generated runtime code:

- `derive_backend_shape_with_diagnostics` creates `layout_facts.backend_shape`
  and `layout_facts.cost_facts`.
- `choose_backend_shape` is the current hardcoded priority selector.
- `CostFacts.chosen` is passed through codegen lowering.

This is the right place for active cost because it preserves the existing
`BackendShapePlan` / `CostFacts` surface and leaves W7 to own CSP/cascade
deletion.

The blocking consumer gap is downstream:

- `codegen::emit_with_layout` lowers using `backend_shape` and `cost_facts`, but
  JSON emission still uses the existing sink/direct templates.
- `codegen/src/lower/rust.rs` can build rule plans from `CostFacts`, but the
  emitted sink-only program is generated directly from BIR.
- `json_sink_direct.rs` hardcodes JSON value dispatch arms instead of rendering
  the selected candidate.
- CSS rows are served by static provider templates before JSON lowering is
  reached.

Therefore, replacing the selector can be gate-consumed, but it will not move a
row unless W6 also lands a generated runtime path that materially consumes the
candidate. If that path is not present, the correct block id is:

`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME`.

## Conservative Rewrite Set

Safe W6 rewrites are grammar-neutral and fact based:

- sequence flattening and identity removal.
- singleton `Seq` / `Alt` collapse.
- duplicate alternative removal.
- same-mode alternative flattening.
- dispatch branch canonical ordering only when W5 FIRST facts prove exact,
  disjoint, non-nullable branches.
- speculative-to-dispatch promotion only when W5 facts prove disjointness.

More aggressive prefix factoring, optional/repeat idempotence, and control-flow
reshaping need interpreter coverage and should not be selected without explicit
plan acceptance.

Shape candidates should remain within the existing `BackendShape` enum:

- `OffsetTape -> SinkOnly` when the IR proves direct-only sinkability.
- `OffsetTape -> EventTape` for high branch density.
- `OffsetTape -> CollapsedStage` only when existing target support and author
  declarations prove it.

No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
stream, or hidden CSP path is authorized by W6.

## Active Cost And Stale Rate

The smallest valid active-cost slice ranks deduplicated backend-shape candidate
expressions and writes the winner into `CostFacts.chosen`.

Use an integer total order:

1. freshness rank.
2. measured performance cost.
3. capacity-policy cost.
4. static-size cost.
5. fixed shape rank.
6. candidate SHA-256 tie-breaker.

Stale-rate denominator: every deduplicated candidate that reaches active cost
ranking, keyed by rule id, canonical expression hash, backend shape, and
capacity policy. Hard-pruned candidates are counted separately.

Stale-rate numerator: ranked candidates whose primary evidence is missing,
historical, host/build/feature-mask mismatched, or author-declared without a
same-run artifact. Same-commit static analysis can be fresh only when explicitly
tagged as a static proxy and hash-bound to the source artifact.

Gate formula:

`candidate_stale_count * 10 <= candidate_ranked_count * 3`.

The selected winner itself must not use stale cost evidence unless the wave
abrogates.

## Gate And Report Requirements

`gate-json --with-cost-facts` is not enough for W6: it emits old SK-V8 cost-fact
shape and does not validate e-graph node/iteration/memory telemetry,
stale-rate evidence, deterministic replay, or same-wave generated consumption.

W6 needs a companion report:

- schema: `sk-v13-decision-active-cost-v1`.
- wave: `SK-V13-W6`.
- required provenance: run id, source commit, host, build flags, feature mask,
  G-Omega status, W5 regex fact artifact path/SHA, material differential, and
  REDRESS entry.
- required e-graph facts: language status, rewrite set id, node count, e-class
  count, iteration count, memory peak/estimate, budget status, and rewrite-order
  variance.
- required cost facts: cost function source/status, candidate totals, ranked
  count, hard-pruned count, stale count, stale rate, selected candidate id,
  selected rule, selected shape, freshness, capacity-policy status, trace SHA,
  and cost-facts artifact hash.
- required consumer facts: generated selection path, same-wave consumer path,
  cascade fallback status, row-move/admit/block status, block id, and abrogate
  status.

Reject states include `support_only`, `gate_only`, `telemetry_only`,
`scaffold_only`, empty generated/consumer paths, stale-rate >30% without
abrogation, nondeterministic winner, hidden fused solver, and old P1-P8 silent
fallback admission.

Lock 14 needs W6 owner paths for `ir/src/cost.rs`, `passes/src/`, any new
passes e-graph module, named codegen lowering files if consumed, report/gate,
xtask passthrough, and `lock14_baseline.rs`.

## Bounds And Abrogate Criteria

Recommended W6 bounds:

- final egraph nodes <= 100,000.
- final nodes / initial nodes <= 16.0.
- default iterations <=64, hard ceiling <=100.
- memory estimate or RSS <1 GiB.
- stale cost rate <=30%.
- rewrite-order extraction cost variance <=10%.

Abrogate before patching around:

- egraph OOM or memory budget hit.
- stale/missing cost evidence over 30%.
- rewrite-order variance over 10%.
- support-only egraph/cost scaffold with no generated selection consumer.
- any JSON/CSS guard regression.

## Likely Measurement Rows

If W6 genuinely affects generated selection, the closest JSON direct rows are:

- `json/numbers/direct_to_struct/main`.
- `json/instruments/direct_to_struct/main`.
- `json/random/direct_to_struct/main`.
- `json/mesh/direct_to_struct/main`.
- `json/canada/direct_to_struct/main`.

The admitted W10 CSS rows should be treated as guards, not primary movement
targets, because they have large positive margins and their static providers
currently bypass the JSON lowerer/cost seam.

## Recommendation

Plan W6 as a bounded active selector at the `passes` decision seam. Use the
local egraph crate only if CHALLENGE accepts the `CostModel`/edition/path-dep
surface; otherwise use a challenge-accepted equivalent representation with the
same report fields. The redress must either prove generated runtime candidate
consumption with a row move or record
`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` as a
measured architectural block. W6 must not claim G2 completion by itself; W7 owns
CSP and cascade fail-closed.
