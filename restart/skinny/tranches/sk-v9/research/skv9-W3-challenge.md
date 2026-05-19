# SK-V9 Wave W3 CHALLENGE: Union Event-Model Class Column

Disposition: REJECT.

Inputs: `restart/skinny/tranches/sk-v9/research/skv9-W3-plan.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research.md`;
`restart/skinny/tranches/sk-v9/SPEC.md` Section 6; current crate graph.

The plan selects the right substrate shape, accepts the needed Track 2 owner
correction, and correctly requests the W3 redress extension. It is rejected on
the checkasm harness location before redress.

## CH1 Correctness

Accept the positions-only `StructuralIndex` route. The parser may consume
positions by move and read the structural byte at those positions; the retained
view consumer must read the tape class column. This avoids adding a second
structural-class vector to the producer.

## CH2 Generality And Lock 14

Accept the retained-event/structural-alphabet clarification. Number, true,
false, and null classes belong to the retained tape event column. They must not
enter the SIMD structural alphabet or any generic `bbnf-simd` JSON-named API.

## CH3 Regression And REDRESS

Accept the `bbnf-bench/src/track2/json.rs` owner-table correction. Without it,
Track 2 would either break under `JsonNodeKind::at_cursor` class reads or force
a source-byte fallback that keeps the rejected rediscovery path alive.

## CH4 Cost

Accept the redress extension request. W3 touches tape layout, parser state,
generated output, templates, parity, Track 2, and scan parity. The CHALLENGE
grants the SPEC §6 extension to a ≤110-minute redress cap.

## CH5 Hidden Coupling

Reject the plan's `bbnf-simd/tests/checkasm_scan_structurals.rs` location for
the JSON-aware structural scan harness. The string-aware JSON scanner lives in
`runtime/src/grammars/json/scan.rs`, and `runtime` depends on `bbnf-simd`.
Putting the JSON scanner harness inside `bbnf-simd` would require either a
dev-dependency cycle or moving JSON policy into the generic SIMD crate. Both
violate the plan.

The lower aarch64 primitives may stay covered by existing `bbnf-simd`
checkasm tests. The end-to-end JSON scan parity harness must live on the
runtime side, for example as `skinny/crates/runtime/tests/checkasm_scan_structurals.rs`,
where it can compare `runtime::grammars::json::scan::scan_structurals` against
`scan_structurals_scalar` across alignment and corpus fixtures.

## CH6 Anti-Paper-Close

The wave still cannot close without both layers of evidence:

- lower primitive parity remains in `bbnf-simd` tests already present in the
  crate;
- JSON scanner parity is added under `runtime`, not in `bbnf-simd`;
- parser/tape class parity is added under `bbnf-bench`.

## Required Revision

Return to plan with these binding edits:

- Replace the new `bbnf-simd/tests/checkasm_scan_structurals.rs` owner with
  `skinny/crates/runtime/tests/checkasm_scan_structurals.rs`.
- Keep `bbnf-simd/src/lib.rs` and `bbnf-simd/src/aarch64/` owned only for
  source changes needed by the already-existing primitive chain; do not add
  JSON policy to those paths.
- Update the W3 gate command to
  `cargo test -p runtime --test checkasm_scan_structurals -- --nocapture`.
- Add a second CHALLENGE pass after the revision before source redress.
