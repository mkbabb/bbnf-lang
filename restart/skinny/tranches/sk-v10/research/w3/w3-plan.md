# SK-V10 W3 Plan - Parse-Only Firewall

Pass: Wave Plan.
Cycle: W3.
Date: 2026-05-19.
Scope: proof-only closure plan for `G-W3-PARSE-FIREWALL`.

## Entry Gate

PASS.

- W2 closed under REDRESS 101.
- The W1 direct movement contract remains live.
- Current `skinny/RESULTS.md` has 17 `parse_only S / NO-GO` rows and no
  parse-only admission.

## Selected Intervention

Close W3 as a governance firewall, not an implementation wave.

Owner paths:

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

No source behavior, generated output, benchmark body, or row table movement is
planned. `skinny/crates/bbnf-bench/src/bin/gate.rs` is not edited because the
existing `Report::validate_sk_v8_w0` consumer already rejects parse-only
admission and W3 alias reopening is a dispatch/refusal contract, not telemetry
data.

## Exit Gate

`G-W3-PARSE-FIREWALL` from SPEC Section 6.

Required evidence:

```text
rg -n 'UnionTape|class column|class-column|streaming cursor|structural cursor|W4 cascade-lock|cascade-lock|parse-only SOTA|parse_only.*A / GO|parse_only.*GO' \
  restart/skinny/tranches/sk-v10/SPEC.md \
  restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md \
  restart/skinny/tranches/sk-v10/HANDOFF.md \
  restart/skinny/tranches/sk-v10/SYNTHESIS.md
```

Expected: only refusal, pre-block, or diagnostic references.

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture
```

Expected: PASS. This test mutates a parse row to `A / GO` and expects
`Report::validate_sk_v8_w0` to reject it.

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Expected: PASS against the frozen W2-rendered table.

## CHALLENGE Disposition

CHALLENGE is skipped under the dispatch contract's optional W3 clause. The plan
is proof-only, stays inside accepted gate semantics, changes no source behavior,
and moves no rows.

## LOC Budget And Risk

Budget: 80-160 docs/gate LOC. Expected redress is documentation-only.

Risk: LOW. The practical risk is stale status text that accidentally revives W3
as a dependency or consumer; the research audit and close-doc update address
that directly.

## Revert Protocol

Revert W3 close-doc and REDRESS edits. If an alias is found, record it in
REDRESS with the exact file and phrase that kept the route live.

## Same-Wave Consumer

The same-wave consumer is the close packet itself: SPEC, DISPATCH, HANDOFF, and
SYNTHESIS must agree that W3 is closed as a firewall and W4 is next. The
parse-only row claim is consumed by `gate-json`/`Report::validate_sk_v8_w0`.

## Pre-Blocked Routes

- REDRESS 96/97/98 W3 shapes.
- W3/union/event substrate, retained class column, `UnionTape`, structural
  cursor, class-lane-only route, streaming cursor, parser-owned projection.
- W4 cascade-lock through W3.
- Parse-only SOTA close.
- Sidecar or parallel substrate producer.
