# CH5 HIDDEN COUPLING

Disposition: ACCEPT

Lens: CH5 HIDDEN COUPLING - no retained sidecars, parallel substrates,
CSS/provider side channels, public substrate leaks, generic crate grammar
switches, new BIR variants, expanded `BackendShape` claims, or masked
broadcast admission under NEW-CH5-V5-02.

## Critical Findings

| id | severity | finding | evidence | fold requirement |
|---|---:|---|---|---|
| CH5-V1-00 | none | No blocking CH5 defect in the T-P2 V1 dossiers. The dossiers identify and reject the known hidden-coupling routes rather than admitting them. | Sidecars rejected in 2A (`restart/audit/totality/p2/2A-sota-landscape.md:48`, `:84`, `:107`); Layer 0/Layer 1 kept one-way in 2B (`restart/audit/totality/p2/2B-primitive-vocabulary.md:51`-`:52`, `:81`-`:88`); CSS sidecar and grammar switches rejected in 2C (`restart/audit/totality/p2/2C-grammar-neutrality.md:62`-`:63`, `:116`-`:120`); BackendShape fixed to five in 2D (`restart/audit/totality/p2/2D-cost-model.md:44`-`:50`, `:73`, `:105`); runtime regex import rejected in 2F (`restart/audit/totality/p2/2F-parse-that-gaps.md:71`-`:78`, `:98`, `:118`). | None for V1. |

## Evidence Inspected

- Required governance: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:1`-`:39`, `restart/prompts/totality/PASS-2-RESEARCH.md:89`-`:138`, `restart/prompts/ORCHESTRATOR.md:74`-`:127`, `restart/prompts/ORCHESTRATOR.md:198`-`:205`, and `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`:151`.
- All six T-P2 V1 dossiers: `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`, `2D-cost-model.md`, `2E-host-arch-esoterica.md`, and `2F-parse-that-gaps.md`.
- Broadcast-admission handling: 2A rejects the 24-row CSS broadcast and requires row-local command/input/equality/timing (`restart/audit/totality/p2/2A-sota-landscape.md:56`, `:76`, `:105`); 2B rejects repeated throughput tuples for primitive/parser rows (`restart/audit/totality/p2/2B-primitive-vocabulary.md:154`); 2C requires duplicate-tuple rejection (`restart/audit/totality/p2/2C-grammar-neutrality.md:61`, `:118`, `:139`); 2D blocks shape-consumer evidence from CSS broadcast rows (`restart/audit/totality/p2/2D-cost-model.md:86`); 2E prevents PMU laundering of broadcast CSS rows (`restart/audit/totality/p2/2E-host-arch-esoterica.md:100`-`:105`, `:120`); 2F blocks CSS row movement while broadcast telemetry is unresolved (`restart/audit/totality/p2/2F-parse-that-gaps.md:78`, `:101`, `:120`).
- Public substrate / BIR / shape surface spot-check: current `BackendShape` is exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` at `skinny/crates/ir/src/lib.rs:340`-`:345`, and `all_backend_shapes()` returns exactly those five at `skinny/crates/ir/src/cost.rs:333`-`:340`. `BackendExpr::RegexProgram` exists at `skinny/crates/ir/src/lib.rs:355`-`:371`, but 2F treats it as an existing non-row-moving gap, not as a new admitted runtime substrate (`restart/audit/totality/p2/2F-parse-that-gaps.md:72`, `:77`).

## Fold Requirements

None. V1 does not require CH5 fold changes. Preserve the dossier constraints in
V2: sidecars remain transient or diagnostic only, CSS rows stay non-admit until
row-local typed measurement exists, generic grammar switches must be replaced by
generated/provider-owned inputs, and the five-shape BackendShape canon must not
expand.

## Convergence Impact

Does not block T-P2 V1 convergence for CH5. Any future T-P2/T-P3 fold that
reintroduces a retained sidecar, runtime regex substrate, generic grammar switch,
sixth `BackendShape`, or broadcast-admitted row should reopen CH5 as REVISE or
REJECT.
