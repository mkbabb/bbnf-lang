# SK-V14 W5B-FRONTEND A5: JSON/Non-JSON Proof Carry

Date: 2026-05-26.
Scope: read-only inspection for JSON unchanged-output proof plus Sheets/BBNF-self fail-closed/generated-role proof surfaces relevant to W5A/W5B-FRONTEND.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5B-FRONTEND is a preservation wave, not a deletion/generator-body wave. SPEC
§8B requires frontend/import/IR closure, Lock 14 owner-path routing before
source redress, JSON unchanged-output preservation, Sheets/BBNF-self proof
carry, unchanged provider/template topology, and no provider/template deletion
or provider-free generator replacement (`restart/skinny/tranches/sk-v14/SPEC.md:710`).

JSON unchanged-output currently proves through the W5A request path.
`regen-json` and `check-json` call `emit_runtime_from_request(runtime_request(...))`,
not the old direct source path, at `skinny/xtask/src/main.rs:152`.
`runtime_request` reads grammar sources, workspace metadata, source roots,
output dir, and expected files at `skinny/xtask/src/regen.rs:48`. JSON request
emission delegates back to `emit_from_source` only after request/fact validation
at `skinny/crates/codegen/src/grammar_provider.rs:31`.

The executable JSON equality test exists:
`w5a_json_request_matches_emit_from_source` compares old
`emit_from_source("json", ...)` output to `emit_runtime_from_request(...)` at
`skinny/crates/codegen/src/lib.rs:862`. W5A redress records this test,
`cargo xtask check-json`, `cargo xtask gate-json --check-results --skv14-existing-results-capture`,
and no-diff gates as passed at
`restart/skinny/tranches/sk-v14/research/skv14-W5A-redress.md:52`.

Sheets/BBNF-self proof carry is currently fail-closed through the same request
path. Runtime source facts scan imports, `@ws`, `@pretty`, `?w`, `>>`, `<<`,
projections, typed projections, comma, and host capture at
`skinny/crates/grammar/src/lib.rs:141`. Unsupported constructs map to named
codes with path, offset, and source hash at `skinny/crates/grammar/src/lib.rs:93`.
The codegen test `w5a_sheets_bbnf_fail_closed_through_runtime_contract` asserts
Sheets/BBNF-style requests fail with `BBNF-UNSUPPORTED-PROJECTION` at
`skinny/crates/codegen/src/lib.rs:886`.

Non-JSON proof cannot be status-only. The report gate requires witness
command/path/hash and rejects `status-only`, `support_only`, `gate_only`,
`telemetry_only`, and `future_consumer` statuses at
`skinny/crates/bbnf-bench/src/report.rs:1094` and
`skinny/crates/bbnf-bench/src/report.rs:1169`. The generated-role/fail-closed
witness label surface is tested at `skinny/crates/bbnf-bench/src/report.rs:8937`.

W5B-FRONTEND Lock 14 routing is not present yet. Current Lock 14 owner paths
include W5A only at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`, and
the W5A parent-diff test is at
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2053`. Read-only grep found no
`SK_V14_W5B_FRONTEND` / `sk-v14-waveW5B-FRONTEND` routing.

Read-only checks: no `emit_runtime_profile(target.profile)` remains in
`skinny/xtask/src/regen.rs`; provider count is 8; CSS L4 template dir count is
7; `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` are unchanged
from `HEAD`.

## §2 — Recommendations (named falsifiability gates)

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture
cargo xtask check-json
cargo xtask gate-json --check-results --skv14-existing-results-capture
cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture
cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture
cargo xtask regen-css
cargo xtask check-css-l4-at-rules-and-media
cargo xtask check-css-l4-declaration-values
cargo xtask check-css-l4-declaration-values-extended
cargo xtask check-css-l4-nested-layout
cargo xtask check-css-l4-stylesheet-selectors
cargo xtask check-css-l4-vendor-and-custom-atrules
cargo xtask check-css-l4-visual-functions
```

Provider/template topology gate: provider count remains 8, CSS template count
remains 7, and no provider/template add/delete/rename appears in
`git diff --name-status`.

## §3 — Risks (REDRESS entries to pre-block)

- Touching frontend/codegen source before adding W5B-FRONTEND Lock 14 routing
  violates SPEC §8B entry.
- JSON proof can regress if W5B moves JSON off the request-equality path or
  treats `check-json` as sufficient without the equality test.
- Sheets/BBNF-self proof becomes paper-close if it reuses old witness artifacts
  or reports generic parser errors instead of named, source-located unsupported
  constructs.
- Provider/template deletion or provider-free generator-body replacement
  belongs to W5C-GEN/W5D-DELETE, not W5B-FRONTEND.

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
