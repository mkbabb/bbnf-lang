# SK-V15 Skinny Implementation Overfit Audit - Consolidated

Date: 2026-05-28. Evidence HEAD before close packet: `8bada626a`.
Cycle: V2. Audit dispatch: 6 parallel read-only agents plus W11 command replay.

## Verdict

PASS-IMPL V2 verdict: ACCEPT-SK-V15-CLOSE-WITH-ROUTED-BLOCKS.

SK-V15 closes as a prune/rebuild implementation cycle. It does not claim CSS L4
SOTA and does not claim the grammar-driven inflection point.

| axis | verdict | close meaning |
|---|---|---|
| JSON-specific hardcoding | ACCEPT | 51 / 51 strict measured JSON rows sustained. |
| CSS-L4-specific hardcoding | ROUTE | No CSS admission; typed same-workload retime rejects with `admitted_rows=0`. |
| Pattern H runtime | ACCEPT-FOR-PROVENANCE | 67 files and line-1 provenance pass; full collapse routed. |
| Codegen / xtask leaks | ACCEPT-WITH-ROUTED-BROAD-CHECKS | Owner gates pass; dirty generated broad checks routed. |
| Bench / test contrivances | ACCEPT-FOR-QUARANTINE | FNV bench metadata quarantined; CSS broadcast non-admitted. |
| Substrate / backend specialisation | ACCEPT | Decision spine and all five BackendShape lowerers have executable proof. |

## Dispositive CSS Finding

CSS remains open. The current close packet rejects any CSS admission claim:

- W1 made the 24 W8R CSS rows diagnostic/non-admitted.
- W6 retired the old live proof classes.
- W11 re-ran typed same-workload retime and got Track 1 `2/4` parses,
  cssparser `4/4`, unequal typed summaries, Track 1 `3.426 Mbps`,
  cssparser `1995.168 Mbps`, and `admitted_rows=0`.

## Close Evidence

Passed:

- `cargo xtask check-json`
- `cargo xtask gate-json --check-results`
- `cargo test --manifest-path skinny/Cargo.toml -p passes decision_ -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen decision_spine_changes_generated_selection_fixture -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_ -- --nocapture`
- `cargo xtask gate-json --check-results --skv15-backend-lowerers-report ../restart/skinny/tranches/sk-v15/research/w9/skv15-W9-backend-lowerers-report.json`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests:: -- --nocapture`
- `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report ../restart/skinny/tranches/sk-v15/research/w10/skv15-W10-fnv-quarantine-report.json`
- `SKV15_W6_REPORT_OUT=/tmp/skv15-W6-css-typed-retime.json RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf --test css_l4_w6_typed_retime --release -- --nocapture`

Invariants:

- Lock count: `16`.
- Pattern H runtime file count: `67`.
- Pattern H line-1 provenance scan: no bad rows.
- BackendShape canon: 5 shapes only.

## Routed Remainder

SK-V16 starts from routed implementation remainder:

1. Grammar-derived CSS L4 provider and typed same-workload equality.
2. Full Pattern H grammar-id parameterized collapse beyond provenance.
3. Dirty generated CSS state retirement before broad generated checks become
   close gates.
4. FNV production migration remains blocked unless future work proves typed
   semantics independently of hash sidecars.
