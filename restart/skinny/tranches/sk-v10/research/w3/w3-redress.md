# SK-V10 W3 Redress - Parse-Only Firewall

Pass: Wave Redress.
Cycle: W3.
Date: 2026-05-19.
Gate: `G-W3-PARSE-FIREWALL`.
Disposition: PASS.

## Patch

W3 is closed as a proof-only governance firewall. It changes no parser/runtime
behavior, no generated code, no benchmark body, and no `RESULTS.md` row.

The close documents now keep these facts aligned:

- W3 is closed under REDRESS 102 as a firewall only.
- W4 is the next live wave.
- W3/union/event substrate, retained class column, `UnionTape`, structural or
  streaming cursor, class-lane-only route, parser-owned projection, and
  W4-through-W3 cascade-lock remain pre-blocked.
- `parse_only` stays diagnostic `S / NO-GO` and cannot close SOTA.

## Audit Evidence

Active packet audit:

```text
rg -n 'UnionTape|class column|class-column|streaming cursor|structural cursor|W4 cascade-lock|cascade-lock|parse-only SOTA|parse_only.*A / GO|parse_only.*GO' \
  restart/skinny/tranches/sk-v10/SPEC.md \
  restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md \
  restart/skinny/tranches/sk-v10/HANDOFF.md \
  restart/skinny/tranches/sk-v10/SYNTHESIS.md
```

Result: PASS. Hits are refusal, pre-block, or diagnostic references only.

Result table audit:

```text
parse_only rows=17; bad=none
```

## Gate Evidence

Parse-admission rejection:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture
```

Result: PASS. The test includes a parse-row `A / GO` mutation and expects
report validation to fail.

Frozen report and cost-facts consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Result: PASS. JSON report saved at `/tmp/skv10-w3-cost-facts.json`.

## Gate Accounting

- No row moved.
- `parse_only` remains 17 `S / NO-GO`.
- `direct_to_struct` remains 5 `A / GO` and 12 `N-direct / NO-GO`.
- `real_typed_struct` remains 6 `A / GO`.
- W4 may now dispatch, but W4 is typed-product work and cannot name W3 as a
  consumer or substrate dependency.
