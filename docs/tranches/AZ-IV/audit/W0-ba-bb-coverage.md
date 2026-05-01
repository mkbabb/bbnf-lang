# AZ-IV W0 BA/BB Coverage Ledger

**Status**: planning seed. W0 - Truth And Canonical Regen must refresh this
ledger before W1 opens, replacing planned proof with landed evidence,
retirement evidence, or an exact routed successor. Rows may not close as
`unknown`, `later`, or ownerless.

## Rule

AZ-IV is allowed to reject stale BA/BB mechanisms. It is not allowed to drop
their functional requirements silently.

- **Absorbed** means the requirement lands inside AZ-IV's grammar-derived
  StructDirect route.
- **Retired** means the requirement is incompatible with the AZ-IV thesis and
  has an artefact-backed decision.
- **Routed** means the requirement is explicitly outside AZ-IV, with a named
  successor and blocker proof. Routing is invalid when the item can fit AZ-IV
  without creating a second parser/path/optimizer system.

## BA Coverage

| Source | Requirement | AZ-IV disposition | Owner | Required evidence |
|---|---|---|---|---|
| BA.W-1 | prove AZ direct-struct output, no tape path, no stale fallback resolver | absorbed | W0 - Truth And Canonical Regen | strict regen, tempdir parity, no active tape/bootstrap/derive fallback |
| BA.W0 | `Path`, `PathSegment`, typed path construction | absorbed through existing runtime path surface | W1 - Runtime Surface And Semantic Parity | `runtime::path`, `path!`, and per-document `*PathQuery` are type-inference checked |
| BA.W0 | `TypedPath<G,T>` semantics | absorbed as compile-time/type-inference path proof, not a new public path crate | W1 - Runtime Surface And Semantic Parity | invalid/valid path tests prove result types and grammar-aware diagnostics |
| BA.W0 | path type checker against `StructRegistry` | absorbed | W1 - Runtime Surface And Semantic Parity | generated/type facts define path segment alternatives; no untyped resolver |
| BA.W0 | invalid path diagnostics name segment, struct, and valid alternatives | absorbed | W1 - Runtime Surface And Semantic Parity | focused tests for JSON and CSS L4 malformed paths |
| BA.W0 | parent-pointer/ascent strategy micro-bench | retired unless zero-allocation traversal proves no sidecar is needed | W1 - Runtime Surface And Semantic Parity | explicit no-sidecar decision or measured in-place ascent strategy |
| BA.W0 | IR `path_check` pass over `path!` sites | absorbed as type-inference path check in existing compile/runtime surface | W1 - Runtime Surface And Semantic Parity | `path!` sites fail closed without a second parser or shadow path crate |
| BA.W1 | lazy traversal executor | absorbed | W1 - Runtime Surface And Semantic Parity | traversal over generated struct documents is zero-allocation |
| BA.W1 | `NodeView<'p,T>` semantics | absorbed through runtime document/view surface | W1 - Runtime Surface And Semantic Parity | typed borrowed views expose values without heap allocation |
| BA.W1 | `path!` macro | absorbed if existing macro can own semantics; otherwise macro route must be retired by artefact | W1 - Runtime Surface And Semantic Parity | expansion/usage tests prove compile-time path typing |
| BA.W1 | wildcard/no-collect traversal | absorbed if existing path surface supports it; otherwise retire exact wildcard semantics | W1 - Runtime Surface And Semantic Parity | allocation proof and parity examples |
| BA.W1 | 3-field JSON path extraction >=20% faster than sonic-rs pointer | absorbed | W1 and W3 | same-harness path/value bench row with competitor ratio |
| BA.W1 | 30-field JSON access parity-or-better | absorbed | W1 and W3 | same-harness path/value bench row with competitor ratio |
| BA.W1 | sonic-rs/simdjson/cssparser/lightningcss parity for path output | absorbed where comparable | W1 - Runtime Surface And Semantic Parity | regenerated-output parity matrix and explicit no-surface decisions |
| BA.W2 | TS template-literal tag semantics | absorbed only if generated TS backend supports it; otherwise exact no-surface decision | W1 - Runtime Surface And Semantic Parity | Node-executed TS proof and host signature/error ledger |
| BA.W2 | Python callable path surface | routed or absorbed by explicit host-binding decision | W1 - Runtime Surface And Semantic Parity | Python status row with exact signature/errors or no-surface decision |
| BA.W2 | host-binding isomorphism across errors and values | absorbed | W1 - Runtime Surface And Semantic Parity | host-binding ledger covering TS and every declared host |
| BA.W3 | full measurement matrix | absorbed | W3 - Measurement And Close | `post-AZ-IV.json`, path/value rows, profiles for misses |
| BA.W3 | extended parity harness | absorbed | W1 and W3 | regenerated-output parity plus host/path matrix |
| BA.W3 | handoff to BB | superseded | W0 - Truth And Canonical Regen | AZ-IV parent plan declares BA and BB folded into one route |
| BA critical files | `crates/core/src/path/*` and `crates/bbnf-path*` file layout | retired as mechanism | W1 - Runtime Surface And Semantic Parity | no new path crate; existing runtime surface owns semantics |

## BB Coverage

| Source | Requirement | AZ-IV disposition | Owner | Required evidence |
|---|---|---|---|---|
| BB preflight | AZ-III carries: regen drift, `Map`, Tailwind, watchdogs | absorbed | W0, W2, W3 | carry ledger rows close with artefacts |
| BB thesis | egraph-first rewrite inference, VM residue second | absorbed | W2 - Optimization Substrate Activation | rewrite-chain artefact splits egraph-proved and VM-residue cases |
| BB.W0 | `crates/ir/src/rewrites` schema, rank, tiering | absorbed if present; created/refactored only inside current IR route | W2 - Optimization Substrate Activation | schema/provenance/rank/tier consumed by production pipeline |
| BB.W0 | Ruler CVC enumeration over `IrNode` | absorbed | W2 - Optimization Substrate Activation | enumerator output with declared size bound |
| BB.W0 | egraph residue split | absorbed | W2 - Optimization Substrate Activation | residue count and oracle count in `W2-ruler-oracle-ranker.json` |
| BB.W0 | VM oracle wrapper | absorbed | W2 - Optimization Substrate Activation | VM checks only residue classes, not the main equivalence path |
| BB.W0 | base fleet-wide rules in RON | absorbed or retired per rule | W2 - Optimization Substrate Activation | every loaded non-empty ruleset has load/apply/writeback/generated proof |
| BB.W0 | grammar rewrite discovery by `xtask regen` | absorbed | W2 - Optimization Substrate Activation | `grammar/<name>/rewrites/*.ron` discovery is fail-closed and evidenced |
| BB.W0 | Tranche H rediscovery >=80% | absorbed | W2 - Optimization Substrate Activation | rediscovery report meets floor or blocks close |
| BB.W0 | retained-rule hit-rate >=0.1 | absorbed | W2 - Optimization Substrate Activation | hit-rate report per production grammar |
| BB.W1 | JSON enumeration run, >=5 accepted sound rules | absorbed | W2 - Optimization Substrate Activation | accepted-rule ledger and generated diff |
| BB.W1 | Sheets enumeration run, >=5 accepted sound rules | absorbed | W2 - Optimization Substrate Activation | accepted-rule ledger and parity proof |
| BB.W1 | generated.rs re-emission and shrink verification | absorbed | W2 and W3 | generated diff, parity, and hot-path bench result |
| BB.W1 | review ledger | absorbed or retired with CI/review reason | W2 - Optimization Substrate Activation | review ledger status in denominator artefact |
| BB.W2 | CSS L4 wide alphabet target N=50, >=5 accepted sound rules | absorbed | W2 - Optimization Substrate Activation | CSS rule report plus Tailwind non-timeout bench/parity |
| BB.W2 | BBNF wide alphabet target N=40, >=5 accepted sound rules | absorbed | W2 - Optimization Substrate Activation | BBNF rule report plus self-host parity |
| BB.W2 | node ceiling and rejection-rate control | absorbed | W2 - Optimization Substrate Activation | rejection <=50% after ranker calibration |
| BB.W3 | every production grammar has rewrite-dir decision | absorbed | W2 - Optimization Substrate Activation | directory census and per-grammar status row |
| BB.W3 | grammar-colocated authoring | absorbed | W2 - Optimization Substrate Activation | RON provenance names grammar, source, proof, and owner |
| BB.W3 | synthetic grammar extensibility with zero core/emitter edits | absorbed | W2 - Optimization Substrate Activation | synthetic grammar contributes a rule without core/emitter changes |
| BB.W4 | cost-model integration | absorbed | W2 and W3 | rank/tier feeds extraction/cost; benchmark non-regression |
| BB.W4 | CI auto-accept job | absorbed or retired with explicit process decision | W2 and W3 | workflow/status row or retirement artefact |
| BB.W4 | ranker calibration | absorbed | W2 - Optimization Substrate Activation | >=90% accepted rules are Class 1/2 |
| BB.W4 | review ledger close and BB FINAL | superseded | W3 - Measurement And Close | AZ-IV FINAL cites BB coverage and residual routes |
| BB.W5/BC.W5 | debug, inspect, and minimise tooling | routed only if W3 proves a blocker needing it | W3 - Measurement And Close | named successor route with blocker profile/test proof |
| BB.W6/BC.W6 | replay, resume, incremental, recovery closure | routed only if W3 proves a blocker needing it | W3 - Measurement And Close | named successor route with blocker profile/test proof |
| BB hard gates | rejection <=50%, accepted Class 1/2 >=90%, generated shrink, throughput gain, parity after each accepted rule | absorbed | W2 and W3 | numeric rule report, generated diff, parity, and benchmark evidence |

## W0 Refresh Checklist

- Read `docs/tranches/BA/BA.md`, `docs/tranches/BA/waves/*.md`,
  `docs/tranches/BB/BB.md`, and `docs/tranches/BB/waves/*.md`.
- Replace each `absorbed`, `retired`, or `routed` claim above with artefact
  links once W0 source/doc work lands.
- Add rows for any BA/BB requirement omitted from this seed.
- Fail W0 if any row remains ambiguous, ownerless, or contradicted by source.
