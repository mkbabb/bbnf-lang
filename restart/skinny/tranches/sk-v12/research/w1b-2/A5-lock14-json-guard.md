# SK-V12 W1b-2 A5 - Lock 14 And JSON Guard

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: ownership, Lock 14, and JSON guard handling.

## Findings

- Adding lightningcss to the W1b-2 bench dependency surface does not require a
  Lock 14 allowlist change if it stays in `skinny/crates/bbnf-bench/Cargo.toml`
  and is consumed only by `nonjson_css_l4.rs` / the CSS L4 bench.
- Extending `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`, and
  `skinny/crates/bbnf-bench/src/bin/gate.rs` does not require new Lock 14
  allowlist entries. `report.rs`, `gate.rs`, and `src/bin/gate.rs` are already
  bench gate/schema surfaces; `nonjson_css_l4.rs` is outside the frozen roots.
- Do not edit `lock14_baseline.rs` for W1b-2 unless implementation touches a
  frozen/generic root. SPEC Section 7 does not name it as an owner path.
- If lightningcss must be added through `skinny/Cargo.toml` workspace
  dependencies, that is a plan/owner amendment issue. Prefer a direct
  `bbnf-bench` dependency for W1b-2.

## RESULTS.md Rule

Keep `skinny/RESULTS.md` unchanged for W1b-2 measurement-only evidence. The CSS
L4 lightningcss row belongs in the W1b-2 companion report unless W1b-2 records
an actual ADMIT disposition or a measured JSON guard demotion. Do not add
placeholder CSS SOTA rows, main JSON columns, or speculative `lightningcss_mbps`
entries to `RESULTS.md`.

## No-Write JSON Guard

Run from `skinny/`:

```sh
CRITERION_HOME=/tmp/skv12-w1b-2-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

This validates the companion report, runs the JSON result check, and does not
write `RESULTS.md`.
