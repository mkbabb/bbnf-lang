# PASS-1 Sub-Agent 1: IR Architect

## §1 Scope + Framing

Scope: Grammar IR, Backend IR, side-table optimized IR, source maps, lower boundary, and IR ownership across PASS-1 crates.

Verdict: keep two IRs. Grammar IR is a compact semantic grammar representation; Backend IR is the executable plan chosen after type, recognizer, CSP, e-graph, and cost passes. Optimized IR is side tables keyed by stable ids, not a third AST.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| Two IRs | Matches `restart/README.md:104`-`restart/README.md:118`. | Requires a strict lower boundary. | Grammar IR holds user semantics; Backend IR holds execution. | Backend hints must not leak upstream. | KEEP |
| Grammar IR 12-15 variants | Keeps semantics inspectable. | Current `IrNode` already mixes semantic and backend hints. | `crates/ir/src/types/node.rs:30`-`crates/ir/src/types/node.rs:98` includes `AltDispatch` and `TokenDispatch`, which should lower later. | Define final variant fields before code migration. | REINVENT |
| Backend IR about 22 variants | Old typed table is a useful execution catalog. | Some legacy variants encode stale terms. | The BC table includes `HostCall`, `Layout`, `PrattSpine`, `SimdScan`, and `ErrorRecovery` (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:160`-`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:254`). | Reclassify table entries into backend op, side table, or deletion. | KEEP AS CATALOG |
| Optimized side tables | Fits existing `GrammarIR` sidecar practice. | Side tables can hide invariants. | Current grammar data carries regex info, facts, recognizer decisions, configs, and cost config (`crates/ir/src/types/grammar.rs:70`-`crates/ir/src/types/grammar.rs:168`). | Every side table needs producer and consumer gates. | KEEP |
| Tape/direct value substrate | Supports both simdjson-style tape and sonic-rs-style direct build. | Requires one API over two materializations. | Tape is locked as substrate and unioned with direct-to-struct (`restart/README.md:285`-`restart/README.md:315`, `restart/locks/LOCKS.md:34`). | Do not rename to ParseStream. | KEEP |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | Two IRs; stable ids; source map table; side-table optimization; tape/direct value union; explicit lower boundary. |
| REINVENT | BC 22-variant table as Backend IR inventory; current `IrNode` backend hints as extraction products; map expressions as typed expressions. |
| DISCARD | One super-IR; optimized AST clone; ParseStream rename; direct-only substrate; grammar-specific backend branches in generic IR crates. |

## §4 New Facilities Proposed

| Proposed path | Purpose | Notes |
|---|---|---|
| `restart/specs/pass-1/grammar-ir.md` | Canonical 12-15 variant Grammar IR schema. | Names fields, ids, spans, directive nodes. |
| `restart/specs/pass-1/backend-ir.md` | Canonical Backend IR instruction inventory. | Uses the BC table as source material, not authority. |
| `restart/specs/pass-1/side-tables.md` | Producer/consumer contract for optimized side tables. | Required by `docs/precepts/instructions/LESSONS-LEARNED.md:74`-`docs/precepts/instructions/LESSONS-LEARNED.md:80`. |
| `restart/specs/pass-1/lower-boundary.md` | Exact Grammar IR to Backend IR boundary. | Prevents backend decisions from entering semantic IR. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | Module layout must give `ir`, `passes`, `vm`, `host`, `cost-model`, `egraph`, and `csp-solver` sibling APIs without god directories; Lock 13 applies (`restart/locks/LOCKS.md:58`). |
| PASS-2 | Any proposed `source` module may own normalization and spans, but it must not rename tape to ParseStream. |
| PASS-3 | Backend/VM/debug consumers take Backend IR plus side tables; they do not walk Grammar IR directly. |
| PASS-3 | Host-call lowering consumes typed host metadata and `HostChain` backend operations, not grammar-specific registries. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| Backend details creep into Grammar IR. | Enforce a lower-boundary spec and make `AltDispatch`, scanner, Pratt, SIMD, layout push/pop, and recovery Backend IR only. |
| Side-table drift. | Version side tables and name producer/consumer gates. |
| Old ParseStream wording returns. | Mark `restart/inheritance/INDEX.md:66`, `restart/README.md:391`, and `restart/README.md:473` as stale for naming. |
| Generic IR crate grows grammar switches. | Replace `PRODUCTION_MANIFEST_TABLE`-style registries (`crates/ir/src/registry/strategy.rs:134`-`crates/ir/src/registry/strategy.rs:189`) with metadata and generated code. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| BA.W2 layout/god-module discipline | Cohesive module splits and consumer-coupled layout work survive (`docs/tranches/BA/waves/W2.md:1`-`docs/tranches/BA/waves/W2.md:17`). | Full term purge details are not directly inherited. | PASS-1 names semantic type, layout, and backend materialization separately. |
| BB optimizer layering | Output-piped CSP/e-graph/miner/cost structure survives (`docs/tranches/BB/BB.md:5`-`docs/tranches/BB/BB.md:9`). | Old “tape dead” assumption dissolves. | Backend IR extraction consumes facts from those stages. |
| BC typed IR contract | Backend-agnostic typed IR premise survives (`docs/tranches/BC/BC.md:5`-`docs/tranches/BC/BC.md:24`). | Exact old crate split does not bind this greenfield plan. | PASS-1 separates Grammar IR and Backend IR. |
| BD multi-backend activation | TS/WASM/Rust parity expectation survives (`docs/tranches/BD/BD.md:31`-`docs/tranches/BD/BD.md:36`). | BD publication mechanics are outside PASS-1. | Backend IR must be language-neutral enough for later backends. |
