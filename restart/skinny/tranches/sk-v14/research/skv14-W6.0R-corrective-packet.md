# SK-V14 W6.0R Corrective Packet: Runtime Projection Source Before CSS Root Collapse

Date: 2026-05-26.

## Problem

W6.0 cannot honestly collapse `crates/core/src/runtime/css_l4/` under the
current implementation surface. Root `cargo xtask regen` regenerates parser and
registry files only, while the CSS L4 root runtime depends on semantic
projection data that the registry does not encode.

## Proposed Amendment Shape

Split the current W6.0 expectation into two implementation obligations before
the destructive CSS root-runtime gate:

1. Add a root runtime-projection source and emitter.
   - Inputs: CSS L4 grammar source, workspace metadata, generated registry
     sidecar, and a declarative runtime projection file.
   - Output: the seven files under `crates/core/src/runtime/css_l4/`.
   - Prohibited: copied Rust bodies, `include_str!` of committed runtime files,
     and central string-template relocation.
2. Re-run W6.0 CSS L4 root-runtime collapse.
   - `cargo xtask regen-css`
   - `git diff --exit-code -- crates/core/src/runtime/css_l4`
   - destructive delete/regen/diff clean
   - focused CSS parser/runtime/path tests

## Required Projection Fields

The runtime projection source must carry:

- module/export roster;
- CSS value enum and struct constructors;
- arena family declarations and rollback counters;
- builder frame kinds;
- rule-id/rule-name frame mapping;
- leaf and branch-tag routing;
- recursive color-reference storage;
- document walkers and path-query focus rules;
- runtime-view child traversal.

## Risk Class

High, but local to W6.0 root CSS runtime generation. The change does not require
altering Locks 1-16 if it keeps the existing public CSS runtime API and Lock 14
generic-crate constraints intact.

## Exit Gate

W6.0R is not admitted until:

```sh
cargo xtask regen-css
git diff --exit-code -- crates/core/src/runtime/css_l4
rm -rf crates/core/src/runtime/css_l4
cargo xtask regen-css
git diff --exit-code -- crates/core/src/runtime/css_l4
cargo xtask regen --grammar css_l4 --check
cargo test -p bbnf --profile ax-iter --test css_l4_substrate
cargo test -p bbnf --profile ax-iter --test parse_with_css_l4
cargo test -p bbnf --profile ax-iter --test runtime_root
cargo test -p bbnf --profile ax-iter --test typed_accessor_surface
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
```

## Downstream Ownership

W6.1 remains blocked until W6.0R and W6.0 admit. W6.1..W6.8 should receive
their own narrow Lock 14 routes only after the CSS root-runtime generator proves
the projection-source pattern is real.
