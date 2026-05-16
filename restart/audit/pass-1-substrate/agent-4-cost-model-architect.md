# PASS-1 Sub-Agent 4: Cost Model Architect

## §1 Scope + Framing

Scope: cost trait, extraction scoring, SOTA gates, generated-code budgets, materialization choice, and evidence recorded for benchmark triage.

Verdict: define a reusable `AnalysisCost`-style trait and domain weights, but keep profiles metadata-driven. Cost chooses among legal alternatives; it does not create semantic rewrites or branch on grammar identifiers.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| Trait cost model | Matches `restart/README.md:211`-`restart/README.md:218`. | Weak defaults can pick poor plans. | Terminal, sequence, alt, repeat, host, layout, materialization, SIMD, and Pratt weights become scored inputs. | Record alternatives and selected score. | KEEP |
| SOTA gates | Keeps performance claims concrete. | Too many gates can slow iteration. | SOTA targets are required by `restart/README.md:322`-`restart/README.md:349` and Lock 8 (`restart/locks/LOCKS.md:48`). | Separate throughput, allocation, LOC, and parity gates. | KEEP |
| Tape/direct cost | Covers simdjson and sonic-rs patterns. | Requires one value API. | sonic-rs supports direct/lazy evidence (`restart/corpora/SOTA.md:28`-`restart/corpora/SOTA.md:58`); simdjson supports tape/on-demand evidence (`restart/corpora/SOTA.md:62`-`restart/corpora/SOTA.md:89`). | Do not regress to direct-only. | KEEP |
| Metadata profiles | Avoids grammar switches. | Metadata schema must be strong. | Current hard-coded registry rows show the anti-pattern (`crates/ir/src/registry/strategy.rs:134`-`crates/ir/src/registry/strategy.rs:189`). | Replace with generated facts. | REINVENT |
| Cost evidence | Makes extraction debuggable. | Adds output artifacts. | Every selected Backend IR strategy should say why it won. | Keep evidence compact. | KEEP |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | Trait-based scoring; SOTA gates; tape/direct materialization candidates; SIMD/Pratt costs; extraction evidence. |
| REINVENT | Grammar profiles as metadata; generated LOC as a scored pressure; scanner cost using generic `simd-scan`. |
| DISCARD | Grammar id switches; hidden pass-local scoring; direct-only or tape-only dogma; cost without reproducible evidence. |

## §4 New Facilities Proposed

| Proposed path | Purpose |
|---|---|
| `restart/specs/pass-1/cost-model.md` | Public cost trait and weight categories. |
| `restart/specs/pass-1/extraction-evidence.md` | Side-table schema for selected/rejected alternatives. |
| `restart/specs/pass-1/sota-gates.md` | Minimal PASS-1 gate taxonomy and benchmark anchors. |
| `restart/specs/pass-1/materialization-costs.md` | Tape/direct scoring inputs and consumer requirements. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | `cost-model` must be a cohesive crate/module with no grammar ids. |
| PASS-2 | Generated-code budget data belongs in metadata or side tables, not source comments. |
| PASS-3 | VM/debug hooks should expose cost evidence for selected plans. |
| PASS-3 | Backend emitters must accept cost-selected Backend IR rather than re-score grammar structure. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| SOTA corpus stale tape rejection misleads implementers. | Call out `restart/corpora/SOTA.md:198`-`restart/corpora/SOTA.md:215` and `restart/corpora/SOTA.md:276`-`restart/corpora/SOTA.md:284` as stale research. |
| Benchmark gates use vague baselines. | Require named competitor, dataset, platform, and measurement surface. |
| Cost picks plan but no one can inspect why. | Emit extraction evidence side table. |
| Generic cost code branches on grammar name. | Use facts and metadata only. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| BB hard gates | Specific SOTA-gate discipline survives (`docs/tranches/BB/BB.md:11`-`docs/tranches/BB/BB.md:24`). | Old exact numeric gates are not PASS-1 requirements. | PASS-1 defines gate taxonomy. |
| BB.W3c cost consumer | Same-commit consumer discipline survives (`docs/tranches/BB/waves/W3c.md:7`-`docs/tranches/BB/waves/W3c.md:13`). | Rank/tier timing dissolves. | Cost evidence must have an immediate extractor consumer. |
| BC final perf gates | Cross-tranche performance pressure survives (`docs/tranches/BC/BC.md:15`-`docs/tranches/BC/BC.md:24`). | BC exact code paths are later. | Backend IR extraction records benchmark-relevant decisions. |
| BD TS/WASM gates | Cross-backend performance pressure survives (`docs/tranches/BD/BD.md:31`-`docs/tranches/BD/BD.md:36`). | NAPI/WASM packaging is outside PASS-1. | Cost traits should not be Rust-backend-only. |
