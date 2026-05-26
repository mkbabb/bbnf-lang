# Pass Omega V7 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 hidden coupling.
Disposition: ACCEPT after fold.

## V1 Finding

Initial CH5 returned REVISE. The parser/frontend gap, provider-reachability gap,
`@ws` caveat, and no substrate/BackendShape drift were covered, but Lock 14
owner-path and parent-diff routing for W5B-FRONTEND and W5C-GEN was not
concrete enough.

## Fold

The V7 fold now requires executable Lock 14 routing before W5B-FRONTEND or
W5C-GEN source redress:

- Named owner rosters in `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- Exact parent-diff subject routing for `sk-v14-waveW5B-FRONTEND` and
  `sk-v14-waveW5C-GEN`.
- Unit tests analogous to the current W5A parent-diff test.
- Initial owner-path lists for W5B-FRONTEND and W5C-GEN.
- No implicit authorization for new neutral module paths.

## Verdict

ACCEPT. The prior REVISE is fixed.
