# SK-V15 Wave W1 Plan: CSS Broadcast Admission Hard Demotion

Inputs:

- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-A-results-manifest.md`
- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-B-gate-report-surfaces.md`
- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-C-ledger-surfaces.md`
- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-D-provider-boundaries.md`
- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-E-falsifiability.md`
- `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-F-dirty-tree.md`
- `restart/skinny/tranches/sk-v15/SPEC.md:264` through `restart/skinny/tranches/sk-v15/SPEC.md:281`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:130` through `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:142`

Intervention: Make every W8R CSS L4 full-parse broadcast path diagnostic-only
by demoting rolling-delta CSS rows to `OPEN`, rejecting audit-falsified CSS
rolling rows marked `ADMITTED`, removing W8R CSS from legacy sustained-admit
validators, and changing the W8 harness disposition to zero admitted rows.

## Owner Paths

- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `skinny/xtask/tests/skv15_w0.rs` only if an integration fixture is needed

No generated CSS runtime, CSS provider/template, `regen_css`, root
`crates/core/src/runtime/css_l4/**`, or `skinny/crates/codegen/**` paths are
authorized. Provider deletion is forbidden in W1 because typed replacement
proof belongs to W5/W6.

## Falsifiability Gate

- `restart/skinny/ROLLING-SOTA-DELTA.md` has 24 CSS rows and every CSS row is
  `OPEN`, not `ADMITTED`.
- `validate_skv13_rolling_delta` rejects a CSS row marked `ADMITTED` when
  RESULTS marks that CSS metric as `not_admitted` and `AUDIT-FALSIFIED`.
- `validate_skv14_sustained_row` in both report and xtask validator paths no
  longer admits SK-V14 W8R CSS rows as sustained authority.
- `css_l4_w8::run_production_attempt` returns zero admitted rows and a
  diagnostic W1 disposition even when the old W8R numeric predicate is true.
- `gate-json --check-results` continues to accept the committed W0/W1 RESULTS
  posture and preserves JSON 51/51 guard rows.

Required commands from the skinny workspace:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv13_rolling_delta
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench css_l4_w8
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
```

Global invariant checks:

```sh
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

## Budget And Trigger

SPEC W1 envelope: risk Medium, manual source/test LOC 80-200, generated output
none, docs/ledger LOC 80-180, entry gate W0 admitted, exit gate no W8R live
admit. Redress hard cap: 30 minutes. If implementation exceeds the envelope or
requires provider deletion, abort redress and record an intrinsic block or
REDRESS-class route instead of broadening into W5/W6 work.

CHALLENGE is not mandatory for W1 under `DISPATCH-PROMPT.md:110`; this is a
ledger/gate diagnostic demotion with no provider deletion, no substrate change,
no SIMD/ASM primitive, and no generated-output claim.

## Revert Protocol

- Revert edits to the five owner paths above.
- Restore `restart/skinny/ROLLING-SOTA-DELTA.md` CSS statuses only from the
  pre-W1 committed state if the gate cannot be made internally consistent.
- Preserve unrelated dirty runtime/generated work; do not use broad restore,
  checkout, or `git add -u`.
- If the W8 harness cannot be made diagnostic-only inside budget, commit a
  REDRESS rejection artifact and leave W2 blocked.

## Same-Wave Consumer

The same-wave consumer is `cargo xtask gate-json --check-results`, which calls
`validate_skv13_rolling_delta` through `validate_w0_results_snapshot`, plus the
`bbnf-bench css_l4_w8` test harness proving the historical W8R producer cannot
emit live admitted rows.

## Pre-Blocked Routes

- Do not delete `CSS_GENERATED_RS`, generated CSS modules, parser providers, or
  root CSS runtime files.
- Do not set CSS metrics floors from W8R numeric output.
- Do not convert W8R diagnostic proof into independent per-feature rows.
- Do not change the JSON 51-row admission posture except to verify it remains
  accepted by the gate.
