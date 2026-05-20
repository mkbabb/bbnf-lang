# SK-V12 W1a CHALLENGE V3 - CH4 Cost And Size

Date: 2026-05-20.
Lens: CH4 cost / size / generated roster / redress cap fit.
Disposition: ACCEPT - redress authorized with cap-stop enforcement.

## Verdict

ACCEPT. V3 does not reopen the CH4 blockers. Adding `scan.rs` / `sink.rs` as
JSON-owned source and narrowly owning `passes/src/lib.rs` still fits the
30-minute redress cap because the work is path-bound, JSON-only, and explicitly
cap-stopped.

## Evidence

- `lint-loc` remains out of W1a PASS; V2 already established unrelated
  baseline failures.
- Broad plumbing remains excluded: `report.rs`, `crates/bbnf-bench/src/bin/gate.rs`,
  and `xtask/src/main.rs` are explicitly not owned.
- Current generated JSON roster before new `config.rs`, excluding `scan.rs` /
  `sink.rs`: `1599` LOC / `51594` bytes.
- `scan.rs` + `sink.rs`: `395` LOC / `11858` bytes, now counted as
  source/provenance delta, not generated output.
- `passes/src/lib.rs`: `1751` LOC, but V3 limits ownership to the small
  `recognizers::derive_recognizers` structural alphabet derivation.
- `lock14_baseline.rs`: `806` LOC and already hosts the validation path, so
  adding the W1a generic-root scan there is acceptable if it stays
  path/test-exclusion bound.
- `generated_real_typed.rs`: `1846` LOC / `62599` bytes, counted only if
  deterministic regen moves it.
- `skinny/grammars/json.bbnf`: `18` LOC / `492` bytes.

## Conditions

REDRESS 121 must record hand LOC delta, generated LOC delta for the eight-file
generated JSON roster, `scan.rs` / `sink.rs` source LOC delta, optional
`generated_real_typed.rs` delta, generated module byte totals, grammar byte
count, and O(1) vs O(N) growth classification.

If redress needs paths outside the V3 owner roster, or if the 30-minute cap
expires before proof, W1a must record state and reject or re-plan rather than
broaden the wave.

## CH4 Disposition

ACCEPT.
