# SK-V15 Wave W6 Plan: Typed CSS Same-Workload Retime

Inputs: `skv15-W6-research.md`, W5 redress `3ac131c45`, W6 agent inventory.
Intervention: add a gate-consumed typed CSS retime report that compares root
`CssL4Parser::parse` against `cssparser` on the same corpus and same command,
then retire W8R from live admission whether the typed row admits or routes.

Owner paths:

- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/src/{lib.rs,css_l4_w6.rs,report.rs}`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v15/research/w6/*`

Falsifiability gate:

- command: `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench css_l4_w6_typed_same_workload_retime --release -- --nocapture`
- Track 1 workload: root `CssL4Parser::parse` producing `CssDocument` plus
  visitor traversal over typed document/value nodes.
- Comparator workload: `cssparser` full stylesheet parse plus callback summary
  over the same source set.
- Admit only if Track 1 Mbps is greater than same-run `cssparser` Mbps and the
  typed summary guard passes. Otherwise W6 routes CSS as measured rejection and
  admits no CSS floor.

Hard cap: 75 minutes redress.

Revert protocol: revert the W6 bench/report/gate implementation and keep the
research/plan/challenge docs; record a routed W6 reject if compile, gate, or
measurement proof cannot close inside cap.

Same-wave consumer: the `bbnf-bench` unit test consumes the report, and the
gate validates the generated JSON report with `--skv15-w6-css-typed-report`.

Pre-blocked routes: W8R floors, broadcast rows, `lightningcss` live admission,
fact-stream string admission, `CssFullParseSummary`, brace-counter proof,
warm benches, x86 anchors, and unstaged unrelated dirty files.

Expected result: no CSS admission is assumed. If Track 1 loses, W6 still
retires old proof from live admission and records the row as routed/rejected
with fresh typed evidence.
