# BA Progress (Recycled)

**Status**: planned (opens after AZ-IV close)
**Letter recycled at**: master c2a1c39e (2026-05-01) — old BA preserved at `docs/tranches/BA/historical/`
**Subsumes**: tranche BB rule-discovery scope (BB.W0 through BB.W4 absorbed verbatim; BB perf items absorbed into AZ-IV)
**Brittleness window**: none

## Wave Status

| Wave | Status | Evidence | Notes |
|---|---|---|---|
| W0 - Enumerator + VM Oracle + Ranker Scaffold | planned | pending | Ruler CVC enumerator, e-graph residue split, VM oracle wrapper, ranker, tiering, `crates/ir/src/rewrites/` scaffold; Tranche H soundness rediscovery; corpus-wide rule hit-rate measurement |
| W1 - First Enumeration: JSON + Sheets | planned | pending | curated Class-1/2 batch; ≥ 20 JSON candidates; ≥ 5 accepted JSON rules; codegen shrink ≥ 10 LOC; oracle rejection ≤ 50 % |
| W2 - Wide Alphabet: CSS L4 + BBNF | planned | pending | ≥ 50 CSS + BBNF candidates; ≥ 5 accepted CSS rules; ≥ 5 accepted BBNF rules; rejection ≤ 50 %; e-graph node-count ceiling held |
| W3 - Grammar-Specific Discovery | planned | pending | per-grammar `rewrites/*.ron` authoring; grammar-colocated storage compiles via `cargo xtask regen` |
| W4 - FINAL — Cost Integration + CI Auto-Accept + Review-Ledger Close | planned | pending | cost-model integration; CI auto-accept job; review-ledger close; FINAL.md cites every rule |

## Opening Checklist (runs at AZ-IV close)

- [ ] AZ-IV close commit identified; BA opens against this commit.
- [ ] `cargo xtask regen --check` passes 9/9 (AZ-IV §Hard Gates 1).
- [ ] `crates/ir/src/rewrites/` does NOT exist (AZ-IV.W4 deleted the unconsumed `RuleSet` field).
- [ ] `egraph::ruler::*` does NOT exist (AZ-IV.W4 deleted the unconsumed skeleton).
- [ ] Permanent `substrate_audit.rs` test (AZ-IV.W5) passes against the AZ-IV close HEAD.
- [ ] Workspace nextest is 100 % pass at AZ-IV close.
- [ ] `StructRegistry` populated for JSON / CSS L4 / Sheets / BBNF (AZ-IV §Hard Gates 4).
- [ ] `TypedPath<G, T>` + `path!` macro live (AZ-IV.W2 close).
- [ ] Lazy bail-out parse on 4 production grammars (AZ-IV.W3 close).
- [ ] Record `git status --short --branch`, base commit, `git worktree list`.
- [ ] Create sibling worktrees and unique `CARGO_TARGET_DIR` values for parallel writers.
- [ ] Dispatch BA.W0 - Enumerator + VM Oracle + Ranker Scaffold.

## Running Evidence Ledger

| Date | Wave | Artefact | Result |
|---|---|---|---|
| 2026-05-01 | planning | letter recycle from old BA (lazy typed pointer-path queries) | absorbed into AZ-IV waves W2/W3/W5; old BA preserved at `docs/tranches/BA/historical/` |
| 2026-05-01 | planning | BB subsumption | rule-discovery scope absorbed verbatim from old BB (Ruler CVC enumerator, VM oracle, ranker, Class-1/2/3 tiering, `crates/ir/src/rewrites/`, grammar-colocated rewrite dirs); BB perf/value/struct-projection items absorbed into AZ-IV; BB tranche closed |

## Close-Honesty Parking Lot

Items must close inside BA. None route to a successor letter without a triumvirate scope-reveal review of the BA thesis itself.

| Item | Owner wave | Disposition |
|---|---|---|
| `crates/ir/src/rewrites/` schema + RuleSet registry + provenance types | W0 | planned (non-routable) |
| `crates/egraph/src/ruler/{enumerate,oracle,residue}.rs` | W0 | planned (non-routable) |
| Tranche H soundness rediscovery ≥ 80 % | W0 | planned (non-routable) |
| Corpus-wide rule hit-rate measurement (≥ 0.1 firings/parse on 4 primary grammars) | W0 | planned (non-routable) |
| Automatic ranker with Class-1/2/3 tiering | W0 | planned (non-routable) |
| ≥ 5 accepted JSON rules + Class-1/2 batch | W1 | planned (non-routable) |
| ≥ 5 accepted Sheets rules | W1 | planned (non-routable) |
| ≥ 5 accepted CSS L4 rules + wide-alphabet enumeration | W2 | planned (non-routable) |
| ≥ 5 accepted BBNF rules | W2 | planned (non-routable) |
| Per-grammar `grammar/<name>/rewrites/*.ron` authoring | W3 | planned (non-routable) |
| `cargo xtask regen` discovers + compiles grammar-colocated rule files | W3 | planned (non-routable) |
| Cost-model integration (path-aware cost dimension; ranker weights) | W4 | planned (non-routable) |
| CI auto-accept job for Class-1 rules | W4 | planned (non-routable) |
| Review-ledger close + FINAL.md | W4 | planned (non-routable) |
| ≥ 10 LOC `generated.rs` shrink on at least one grammar | W4 | planned (non-routable) |
| Throughput gain on `post-AZ-IV.json` close matrix on at least one grammar | W4 | planned (non-routable) |
| Workspace nextest 100 % pass at every wave close | every wave | planned (non-routable) |
| Permanent `substrate_audit.rs` test green at every wave close | every wave | planned (non-routable) |
