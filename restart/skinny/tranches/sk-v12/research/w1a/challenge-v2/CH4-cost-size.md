# SK-V12 W1a CHALLENGE V2 - CH4 Cost And Size

Date: 2026-05-20.
Lens: CH4 cost / size / generated roster / redress cap fit.
Disposition: ACCEPT - redress authorized with cap-stop enforcement.

## Verdict

ACCEPT. V2 resolves the CH4 V1 blockers enough to authorize redress.

The baseline-failing `lint-loc` gate is removed. Verified baseline still fails
unrelated ceilings: `crates/bbnf-bench` is `12143/3300` LOC and `xtask` is
`1402/650` LOC, so excluding `lint-loc` from W1a PASS is correct.

V2 also removes the broad optional plumbing CH4 rejected: `report.rs`,
`crates/bbnf-bench/src/bin/gate.rs`, and `skinny/xtask/src/main.rs` are
explicitly not owned. The remaining plan is narrow enough for a 30-minute
redress attempt only because it is path-bound, JSON-only, and has a hard
cap-stop rule.

## Size Evidence

Current generated/runtime sizing is safe:

- Current V2 generated JSON roster, excluding `scan.rs` / `sink.rs` and before
  new `config.rs`: `1599` LOC / `51594` bytes.
- `scan.rs` + `sink.rs`, now treated as JSON-owned source rather than generated
  output: `395` LOC / `11858` bytes.
- `generated_real_typed.rs`: `1846` LOC / `62599` bytes.
- `skinny/grammars/json.bbnf`: `18` LOC / `492` bytes.
- Existing generated runtime JSON lint bucket: `1819/4000` LOC.

Generated-size accounting is present in V2: REDRESS 121 must record hand LOC
delta, generated LOC delta for the eight-file roster, optional
`generated_real_typed.rs` movement, generated module bytes, grammar bytes, and
whether growth is O(1) metadata/config plumbing or O(N) per grammar.

## CH4 Boundaries

Redress must not broaden into report, xtask, bin-gate, IR, runtime tape, public
runtime API, CSS, Sheets, or BBNF-self paths. If the exact roster proves
insufficient, W1a returns to plan instead of expanding in-flight.

`scan.rs` and `sink.rs` are acceptable as JSON-owned source for CH4 cost/size
only if they stay out of the generated roster and any header/comment movement is
counted as hand/source change, not generated output.

The native JSON guard refresh remains real cost, but V2 handles it honestly:
because JSON-producing paths move, PASS requires refreshed guard state. If the
30-minute redress cap is reached before proof completes, record the state and
reject or re-plan.

## CH4 Disposition

ACCEPT.
