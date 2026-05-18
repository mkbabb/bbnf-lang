# SK-V8 W0 Hardening V9 CH6

Date: 2026-05-18.

Target: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Verdict

ACCEPT.

Confidence: 94%.

This is an anti-paper-close accept for the V9 fold only. It is not W0 closure.
V8 consolidated as REJECT and reset the consecutive-ACCEPT counter; under
`restart/prompts/ORCHESTRATOR.md` Section 3Z, V9 can be only the first
qualifying challenge cycle after that reset. W0 still needs a second consecutive
qualifying ACCEPT cycle with zero open critical defects and no unresolved
REVISE before W0 can close or W1-W6 can dispatch.

## Reviewed Surfaces

- CH6 and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:88`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:120`.
- W0 strictness, sidecar, telemetry, cap, behavior-freeze, and closure gates:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:203`,
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:335`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`.
- Dispatch and handoff locks:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:60`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`;
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:154`.
- V8 rejection and required V9 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md`.
- V9 source fold:
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/RESULTS.md`.

## Evidence

- `git rev-parse HEAD`: `00c3485a8774296e796c2f68b74fd3d559627f0a`.
- `git show --stat --oneline 00c3485a`: only
  `skinny/crates/bbnf-bench/src/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs` changed; 61 insertions and 64
  deletions.
- The V8 blocker is folded in source. `validate_strict_admission()` now parses
  the outcome and rejects any outcome whose verdict is not `GO` before checking
  comparator evidence (`skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/gate.rs:139`). The focused test covers `D`,
  `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`, and
  `S` as non-strict-admission outcomes
  (`skinny/crates/bbnf-bench/src/gate.rs:460`).
- W0 row semantics are now frozen before strict-admission evidence can matter:
  strictness must be `deferred`, validation path `view-boundary`, `parse_utf8`
  `view-boundary`, and `escape_complete=yes`
  (`skinny/crates/bbnf-bench/src/report.rs:1012`). The Canada `L / NO-GO`
  hard-failure relabel repro is covered in the full-baseline test
  (`skinny/crates/bbnf-bench/src/report.rs:1954`).
- `skinny/RESULTS.md` stayed unchanged by the V9 fold and still has
  `main_rows=38`, `manifest_rows=38`; the Canada parse row remains
  `L / NO-GO`, `deferred`, `view-boundary`, `yes`, with the frozen W0 run id.
- Frozen behavior-surface diff remained empty:
  `git diff --name-only 0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/generated-json skinny/crates/test-fixtures skinny/crates/bbnf-bench/benches skinny/crates/bbnf-bench/src/direct.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2.rs skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/materialization.rs`
  returned no paths.
- `git diff --check HEAD --`: PASS.

## Commands Run

All successful cargo evidence below was run from
`/Users/mkbabb/Programming/bbnf-lang/skinny`. Running the same package names
from repository root is not valid local reproduction: root `cargo test -p
bbnf-bench` does not find that package, and root `cargo xtask gate-json` invokes
the wrong xtask surface.

- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v9-target cargo test -p bbnf-bench w0_ -- --nocapture`:
  PASS; 12 report W0 tests and 8 gate-bin W0 tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v9-target cargo test -p bbnf-bench strict -- --nocapture`:
  PASS; 5 strict-admission tests passed, including the new non-GO outcome
  rejection.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v9-target cargo test -p bbnf-bench sidecar_same_run -- --nocapture`:
  PASS; sidecar same-run without structured manifest rejected.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v9-target cargo test -p bbnf-bench`:
  PASS; 52 library tests, 8 gate-bin tests, and doc tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  PASS against committed `skinny/RESULTS.md`; output retained overall
  `N-direct / NoGo` and W0 telemetry notes.

## Findings

1. No blocker: the V8 CH1 strict hard-failure hole is closed by executable gate
   behavior. The helper-level path now rejects every non-GO outcome, and the W0
   report path rejects a strict/measured relabel of the current Canada
   `L / NO-GO` row before any strict comparator can admit it.

2. No blocker: the V9 fold did not alter `RESULTS.md`, SPEC, HANDOFF, dispatch
   prompt, parser/runtime/SIMD/codegen/generated/product/Track 2/parity/scan/
   materialization surfaces, or benchmark rows. It is a report/gate-source fold
   only.

3. No blocker: local gate reproduction is green when run from the skinny
   workspace. The committed evidence should continue to state that cwd
   explicitly, because the same bare cargo commands fail from repository root.
   I do not classify this as a W0 source blocker, but it is the exact
   reproduction path CH6 should preserve in consolidation.

4. Closure readiness: not yet. V9 acceptance would restart the qualifying
   counter after V8 REJECT, not close W0. W1-W6 remain blocked until a second
   consecutive qualifying ACCEPT cycle and closure commit.

## Material Blockers

None found for CH6.

## Required Fold If Rejecting

Not applicable; CH6 accepts this fold. The next consolidation should preserve
the exact skinny-workspace commands above and must not describe V9 as W0
closure.
