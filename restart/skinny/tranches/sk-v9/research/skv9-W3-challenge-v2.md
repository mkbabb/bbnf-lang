# SK-V9 Wave W3 CHALLENGE V2: Union Event-Model Class Column

Disposition: ACCEPT.

Inputs: revised `restart/skinny/tranches/sk-v9/research/skv9-W3-plan.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-challenge.md`;
revised `restart/skinny/tranches/sk-v9/SPEC.md` Section 6.

The revised plan resolves the harness crate-boundary rejection. W3 is
authorized for redress under the revised owner paths and the granted
≤110-minute redress cap.

## CH1 Correctness

Accepted. The parser consumes the positions-only `StructuralIndex` by move and
writes parser-event class bytes into the retained tape. The retained view reads
`class_at(cursor)` and no longer rediscovers event class from
`source[offset]`.

## CH2 Generality And Lock 14

Accepted. The SIMD structural alphabet remains `{ } [ ] , : "`. Scalar event
classes live only in the retained JSON tape class column and Track 2 parity
oracle; no generic SIMD JSON policy is introduced.

## CH3 Regression And REDRESS

Accepted. `bbnf-bench/src/track2/json.rs` is an owned redress path because it
constructs benchmark-oracle `JsonRoot` values. `bbnf-bench/src/parity.rs` must
compare classes as well as offsets/flags.

## CH4 Cost

Accepted. The previously granted W3 redress extension stands: redress cap
≤110 minutes.

## CH5 Hidden Coupling

Accepted. JSON-aware scan parity now lives in
`runtime/tests/checkasm_scan_structurals.rs`, so there is no runtime↔bbnf-simd
dependency cycle and no JSON-specific API in `bbnf-simd`.

## CH6 Anti-Paper-Close

Accepted with evidence requirements:

- `rg -n 'consume_structural' skinny/crates/runtime/src skinny/crates/codegen/src`
  returns zero after redress.
- `cargo test -p runtime --test checkasm_scan_structurals -- --nocapture`
  passes.
- Runtime, bbnf-bench parity/materialization, and native Criterion gates named
  by the plan pass.
- The W10b six-row maintain block remains binding; any one row below floor
  rejects the wave.

## Redress Authorization

Proceed to W3 source redress under the revised SPEC Section 6 owner table. Any
need to edit outside those owners returns REVISE before touching source.
