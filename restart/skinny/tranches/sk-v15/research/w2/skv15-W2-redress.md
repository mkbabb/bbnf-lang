# SK-V15 Wave W2 Redress: Lock 14 / Lock 16 Gate Restoration

Status: redress applied at HEAD.

## Scope

W2 restored gate consumption without touching CSS providers, CSS generated
runtime output, root Pattern H runtime files, Decision Engine scaffold, or
benchmark result ledgers.

Owner paths changed:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`

Manual source/test diff: 276 inserted / 12 removed lines across the three W2
owner paths, inside the W2 120-280 LOC envelope by net source/test movement.

## Implementation

- `lock14_baseline::validate` now consumes SK-V15 W2 coverage:
  - required report columns are parsed and duplicate/empty columns fail;
  - required Lock 14 roots are checked for existence, including
    `runtime_generator.rs`, `grammar_provider.rs`, JSON-owned generated
    surfaces, `report.rs`, `bin/gate.rs`, `lock14_baseline.rs`, and
    `xtask/src/{main,skv15_w0}.rs`;
  - root bindings route provider/template findings to W3/W6, gate/report
    findings to W2, and CostFacts/Decision findings to
    `DEP-W7-DECISION-SPINE`;
  - `self-exempting` and `diagnostic:pre-W2-incomplete` values reject W2
    coverage.
- Lock 16 coverage now validates source inventory from `aarch64/mod.rs`,
  dispatch `PrimitiveKernels`, public `prim` wrappers, native token hits, and
  `xtask primitive-checkasm`.
- `xtask primitive-checkasm` now includes `checkasm_escape_mask_64`.
- Bench gate has a named `--skv15-w2-lock-gates-only` mode that runs
  `lock14_baseline::validate` and returns before Criterion/report generation.
- `cargo xtask gate-json --check-results` invokes the lock-only consumer
  before returning; `--with-cost-facts` invokes the same consumer before
  emitting CostFacts.

## Verification

Passed:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench lock14_baseline
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench --bin gate
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results --skv14-existing-results-capture
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --with-cost-facts --check-results
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- primitive-checkasm
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check
```

Invariant outputs:

- Lock count: `16`.
- Pattern H file count: `67`.

Blocked outside W2:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench gate
```

This broad filter selects seven pre-existing `nonjson_css_l4` generated-output
tests (`writes_gate_consumed_*`). They fail on CSS track1/golden byte
mismatches while the worktree already has dirty
`skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` files. W2 does not
own CSS generated runtime repair, and the scoped bench gate binary tests passed.

## Disposition

W2 gate restoration is admitted for its owner paths. Known provider/template,
CSS generated-output, CSS old-proof, and Decision/CostFacts findings remain
routed to W3, W5/W6, and W7 per the SK-V15 dependency ledger.
