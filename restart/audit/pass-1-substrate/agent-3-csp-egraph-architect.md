# PASS-1 Sub-Agent 3: CSP And E-Graph Architect

## §1 Scope + Framing

Scope: CSP/e-graph composition, rewrite domains, recognizer mining, finite-choice extraction, and optimization side-table flow.

Verdict: bridge CSP and e-graph; do not fuse them. E-graph owns equivalence and rewrite saturation. CSP owns finite domains, legality, and extraction choices. Grammar, regex, layout, and backend remain distinct domains.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| Bridged CSP/e-graph | Matches `restart/README.md:219`-`restart/README.md:228`. | Needs stable exchange ids. | E-class analysis feeds CSP; CSP decisions feed extraction. | Keep exchange schema small. | KEEP |
| No unified hypergraph | Prevents domain collapse. | More adapters. | Lock 4 says per-domain optimization and no unified hypergraph (`restart/locks/14-LOCKS.md:40`). | Make adapters explicit. | KEEP |
| Recognizer mining | Enables Pratt/SIMD without user directives. | Facts can go stale after rewrites. | BB says Pratt/SIMD auto-detect from grammar shape (`docs/tranches/BB/BB.md:5`-`docs/tranches/BB/BB.md:9`). | Regenerate or invalidate facts after saturation. | KEEP |
| E-graph generic engine | Current crate is domain-agnostic. | Domain languages still need clear modules. | `crates/egraph/src/lib.rs:1`-`crates/egraph/src/lib.rs:10` shares infra across grammar and regex. | Avoid grammar-specific engine code. | KEEP |
| CSP solver | Already supports variables, constraints, propagation. | Needs deterministic tie-breaks. | `crates/csp-solver/src/lib.rs:120`-`crates/csp-solver/src/lib.rs:184` defines variables/constraints; propagation is at `crates/csp-solver/src/lib.rs:250`-`crates/csp-solver/src/lib.rs:260`. | Deterministic extraction must be locked. | REINVENT |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | Domain-specific e-graph languages; CSP finite-choice solver; recognizer facts; output-piped optimizer; extraction side tables. |
| REINVENT | Dirty-domain scheduling; grammar/regex bridge facts; CSP-guided extraction evidence. |
| DISCARD | Global hypergraph; hidden pass-local optimizer; backend decisions in Grammar IR; cost model as semantic rewriter. |

## §4 New Facilities Proposed

| Proposed path | Purpose |
|---|---|
| `restart/specs/pass-1/csp-egraph-bridge.md` | Facts exchanged between e-graph analysis, CSP, and extraction. |
| `restart/specs/pass-1/rewrite-domains.md` | Grammar, regex, layout, and backend rewrite-domain separation. |
| `restart/specs/pass-1/recognizer-facts.md` | Pratt, SIMD, dispatch, and structural alphabet fact schema. |
| `restart/specs/pass-1/extraction-order.md` | Deterministic tie-break and side-table emission rules. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | Crate/module split must preserve `egraph`, `csp-solver`, `cost-model`, and `passes` as separate concerns. |
| PASS-2 | Any module tree that merges optimizer domains into one god crate conflicts with Lock 4. |
| PASS-3 | VM/backend consumes extracted Backend IR plus recognizer/cost evidence. |
| PASS-3 | Debug hooks should show selected alternatives and rejected candidates. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| Optimizer pipeline becomes order-dependent folklore. | Write `extraction-order.md` and make each side table versioned. |
| E-graph language gets grammar-specific host branches. | Keep host facts in metadata and type side tables. |
| CSP choices are not reproducible. | Require stable variable ordering and tie-breaks. |
| Recognizer facts stale after e-graph saturation. | Facts are produced after the final grammar/regex saturation step. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| BB.W3 optimizer pipeline | Output piping survives (`docs/tranches/BB/waves/W3a.md:7`-`docs/tranches/BB/waves/W3a.md:11`, `docs/tranches/BB/waves/W3c.md:7`-`docs/tranches/BB/waves/W3c.md:13`). | Rank/tier file timing does not bind PASS-1. | PASS-1 names bridge schema first. |
| BC sister-crate freeze | API boundary pressure survives (`docs/tranches/BC/BC.md:21`-`docs/tranches/BC/BC.md:24`). | Publication candidacy is later. | Keep reusable `egraph` and `csp-solver` APIs. |
| BD publication/parity | Cross-backend evidence survives (`docs/tranches/BD/BD.md:34`-`docs/tranches/BD/BD.md:36`). | NPM/crates.io workflow is out of scope. | Extraction evidence should be backend-neutral. |
| BA consumer discipline | Same-wave consumer principle survives (`docs/tranches/BA/waves/W2.md:13`). | Specific old path names dissolve. | Every optimizer fact names its consumer. |
