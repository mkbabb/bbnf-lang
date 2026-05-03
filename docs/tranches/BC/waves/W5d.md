# BC.W5d — Worktree Fixture Closure + Sibling Reconciliation

**Name**: W5d — Worktree Fixture Closure
**Opens after**: BC.W5c close
**Hard gate**: `xtask worktree-init` materialises `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` for every grammar; sibling baseline reconciled per `audit/W0-sibling-baseline.txt`
**Status**: planned

## §1 Deliverable

Per surgery 26 + D08-3 (`audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:12`), the fleet-wide fixture receiver is BC.W5d. The previous draft routed the receiver to three different waves (BA.W0, BB.W0, BC.W2, BC.W5); per Operational Rule 2 the receiver normalises here.

W5d also reconciles the sibling baseline captured at BC.W0c. Five siblings (parse-that, pprint, csc411/csp-solver, bbnf-buddy, ffuzzy) — only parse-that and csc411 should differ from baseline; the rest match.

## §2 Milestones

### §2.1 xtask worktree-init implementation
Mechanism: `xtask/src/worktree_init.rs` materialises:
- `data/{json,css,bbnf,sheets}/` — test datasets per grammar (BA.W0 partial closure)
- `grammar/<name>/rewrites/*.ron` — rewrite rule fixtures per grammar
- sibling sym-links (`parse-that/`, `pprint/`, `csc411/`, `bbnf-buddy/`, `ffuzzy/`) — sibling repo discovery
Files: `xtask/src/worktree_init.rs` (modify-carve), `grammar/<name>/rewrites/*.ron` (create per-grammar).
Sub-gate: `xtask worktree-init` runs cleanly; sample worktree boots with materialised fixtures.

### §2.2 Sibling baseline reconciliation
Mechanism: walk the W0c sibling baseline; for each sibling, capture current SHA + status; record delta.
Files: `docs/tranches/BC/audit/W5d-sibling-reconciliation.md` (create).
Sub-gate: only parse-that (W5b rename) and csc411 (W5a diff-equality verification) differ from W0c baseline; pprint, bbnf-buddy, ffuzzy match (or differ by upstream pull).

### §2.3 Per-grammar rewrites/*.ron creation
Mechanism: each grammar gets a `rewrites/` directory with at least a placeholder `.ron` file. The rewrite fixtures support BD's eventual fleet-wide closure for parallel-agent dispatch.
Files: `grammar/<g>/rewrites/<rule>.ron` per grammar (create).
Sub-gate: every grammar has a `rewrites/` dir with at least one .ron file (placeholder acceptable; populated by future work).

### §2.4 Worktree boot smoke test
Mechanism: a fresh worktree clone runs `xtask worktree-init && cargo check --workspace` clean. The smoke test verifies the fixture materialisation supports BD's parallel-agent infrastructure.
Files: `docs/tranches/BC/audit/W5d-worktree-boot.md` (create).
Sub-gate: smoke test passes; the worktree fixture contract holds.

### §2.5 BD parallel-agent dispatch contract record
Mechanism: per `audit/W6-bd-carry-contract.md:§3 BC→BD.C3`, document the worktree contract for BD's parallel-agent dispatch.
Sub-gate: W6 carry contract BC→BD.C3 row references this wave's deliverable.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W5d-G1 | xtask worktree-init runs clean | `cargo run --bin xtask -- worktree-init` returns clean exit |
| W5d-G2 | data/ fixtures materialised | `find data -type d -name 'json' -o -name 'css' -o -name 'bbnf' -o -name 'sheets'` returns 4 |
| W5d-G3 | rewrites/*.ron per grammar | `find grammar -type d -name 'rewrites' \| wc -l` returns ≥ 9 (one per grammar) |
| W5d-G4 | Sibling reconciliation document | `test -f docs/tranches/BC/audit/W5d-sibling-reconciliation.md` |
| W5d-G5 | Worktree boot smoke test | `xtask worktree-init && cargo check --workspace` clean |

## §4 Invariants

§I1. **Single-receiver rule**: per Operational Rule 2 + surgery 26, W5d is the singular receiver for fleet-wide fixture closure.

§I2. **Worktree contract**: BD's parallel-agent dispatch consumes this contract.

§I3. **Sibling baseline integrity**: only BC-edited siblings differ; others at baseline.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Some grammars don't have natural rewrite rules | Medium | Empty placeholder .ron acceptable; BD-eventual closure populates |
| Sibling drift due to upstream pulls | Low | Reconciliation document records deltas; non-edited siblings may differ by upstream pull (acceptable) |

## §6 Cross-references

### Carry-tags consumed
| Tag | From | Description |
|---|---|---|
| BC.W0c→BC.W5d | BC.W0c | Sibling baseline grounds reconciliation |
| BC.W5b→BC.W5d | BC.W5b | parse-that rename diff |
| BC.W5c→BC.W5d | BC.W5c | parse-that disposition ratified |

### Carry-tags produced
| Tag | To | Description |
|---|---|---|
| BC→BD.C3 | BD.W0 | Worktree fixture contract for parallel-agent dispatch |

## §7 Iter-time check

| Activity | Pre-W5d wall | Post-W5d wall | Notes |
|---|---:|---:|---|
| `xtask worktree-init` | n/a | ~5 s | new iter-loop |

## §8 Dependencies

- **Depends on**: BC.W5c close.
- **Blocks**: BC.W6 (carry ledger to BD).

## §9 Closing posture

The worktree contract closes. The sibling baseline reconciles. BD's parallel-agent infrastructure inherits the contract.
