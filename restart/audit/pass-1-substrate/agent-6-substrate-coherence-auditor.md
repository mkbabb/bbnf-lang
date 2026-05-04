# PASS-1 Sub-Agent 6: Substrate Coherence Auditor

## §1 Scope + Framing

Scope: tape, direct-to-struct, value API, source spans, path/value API, mutation visitor, runtime builders, and stale inheritance conflicts.

Verdict: tape is the substrate term. It is implemented as a union with direct-to-struct materialization. Columnar SoA, parallel substrates, direct-only materialization, and the ParseStream rename are dead for PASS-1.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| Tape | Matches lock and dispatch authority. | Old docs claim tape died. | Lock 1 says tape is substrate and columnar/parallel substrates are dead (`restart/locks/14-LOCKS.md:34`). | Ensure old research is not treated as current plan. | KEEP |
| Direct-to-struct | SOTA evidence supports it. | Direct-only loses on-demand value benefits. | README unions direct-to-struct with tape (`restart/README.md:285`-`restart/README.md:315`). | One path/value API over both. | KEEP |
| ParseStream rename | Could clarify source stream duties. | Conflicts with lock/current authority. | Inheritance says tape name dies (`restart/inheritance/INDEX.md:66`), but lock keeps tape. | Mark stale. | DISCARD |
| Read-write visitor | Controls mutation. | Requires generated visitor surface. | README says mutation is a read-write visitor (`restart/README.md:316`-`restart/README.md:319`). | Keep mutation out of parse stack. | KEEP |
| Runtime builders | Existing OpenFrame shows old substrate leakage. | Useful as backend-internal stack detail. | JSON and CSS builders still expose `OpenFrame` (`crates/core/src/runtime/json/builder.rs:61`, `crates/core/src/runtime/css_l4/builder.rs:55`). | Do not expose as public substrate. | REINVENT |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | Tape; direct-to-struct union; source maps; path/value API; slice-borrow primary; read-write visitor; generated consumers. |
| REINVENT | Runtime builder stacks as Backend IR internals; path API over tape/direct; source normalization without ParseStream naming. |
| DISCARD | ParseStream rename; columnar SoA; parallel substrates; direct-only substrate; generic-runtime grammar switches. |

## §4 New Facilities Proposed

| Proposed path | Purpose |
|---|---|
| `restart/specs/pass-1/tape-value-api.md` | One public value API over tape and direct structs. |
| `restart/specs/pass-1/source-spans.md` | Source normalization, encoding, and span map without ParseStream rename. |
| `restart/specs/pass-1/path-api.md` | Path/value API over lazy and direct materialization. |
| `restart/specs/pass-1/mutation-visitor.md` | Read-write visitor surface and side-effect ownership. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | Module layout may include `source`, `runtime`, and `value`, but tape remains the substrate term. |
| PASS-2 | Runtime builders should split generic builder logic from generated grammar-specific code. |
| PASS-3 | VM/debug paths must emit tape/direct events through one value interface. |
| PASS-3 | Mutation and visitor APIs are consumers of materialized documents, not parse-time stacks. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| Vocabulary drift to ParseStream. | Cite `restart/inheritance/INDEX.md:66`, `restart/README.md:391`, and `restart/README.md:473` as stale naming. |
| Old tape-rejection research drives deletion. | Treat `restart/corpora/SOTA.md:198`-`restart/corpora/SOTA.md:215` and `restart/corpora/RESTART-SKETCH.md:289`-`restart/corpora/RESTART-SKETCH.md:321` as failure evidence only. |
| Value API splits into parallel worlds. | Define a single `tape_value_api` contract. |
| Grammar-specific runtime code enters generic crates. | Move variation to generated parser/runtime code or metadata. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| BA substrate cleanup | OpenFrame/tape failure evidence survives. | Tape burial dissolves. | Tape is fixed as union with direct-to-struct. |
| BB path/lazy value surface | Slice-borrow and lazy path surface survive (`docs/tranches/BB/BB.md:5`-`docs/tranches/BB/BB.md:7`). | Direct-only premise dissolves. | Path API reads one value substrate. |
| BC visitor/pointer formalization | Pointer and visitor pressure survives (`docs/tranches/BC/BC.md:23`). | Old crate split is not binding. | PASS-1 names value and visitor contracts. |
| BD parity fixtures | Cross-backend parity pressure survives (`docs/tranches/BD/BD.md:35`-`docs/tranches/BD/BD.md:37`). | Fixture implementation is later. | Tape/direct values must serialize consistently across backends. |
