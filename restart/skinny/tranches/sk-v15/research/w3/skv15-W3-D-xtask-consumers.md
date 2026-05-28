# SK-V15 W3-D Research: Xtask Consumers And Dirty Generated Output

Scope: `skinny/xtask/src/{main.rs,regen.rs,regen_css.rs}`.

Status: read-only research.

## Findings

`skinny/xtask/src/main.rs:10` exposes `regen-json`, `check-json`,
`regen-css`, seven `check-css-l4-*` commands, `regen-real-typed`, and
`check-real-typed`. Dispatch is at `main.rs:34-54`.

JSON uses `JSON_TARGET` at `main.rs:166-178`; `check_json()` at
`main.rs:186-191` is non-writing and checks
`crates/runtime/src/grammars/json`.

CSS target data lives in `regen_css.rs:5-97`. The seven targets all share the
same fifteen CSS source inputs and `css_l4` grammar name, with separate
profile/output/check strings. `regen_css()` at `regen_css.rs:99-101` writes all
seven targets through `regen::write_targets()`. `check_*` wrappers at
`regen_css.rs:103-139` are non-writing.

`regen.rs:17-31` writes every target passed to it. `regen.rs:34-46` is the
non-writing generated-output check. `regen.rs:48-83` constructs
`RuntimeGenerationRequest` from `RuntimeTarget`; this is the xtask bridge that
can carry W3 metadata.

## Dirty State

Pre-existing dirty generated files block clean CSS check evidence:

```text
skinny/crates/runtime/src/grammars/css_l4_*/generated.rs
skinny/crates/bbnf-bench/src/generated_real_typed.rs
```

Current direct probe result from the research agent:

- all seven `check-css-l4-*` commands fail with `generated file generated.rs differs`;
- `check-json` passes;
- `check-real-typed` fails because `generated_real_typed.rs` is already dirty.

## W3 Boundary

W3 should prefer non-writing checks. `regen-css` is too broad unless the plan
explicitly owns every generated CSS output diff. W3 must not repair unrelated
dirty CSS generated files by hand.

If W3 changes only codegen profile/request metadata and emitted CSS output
remains byte-equivalent, `codegen` unit tests can be the generated-output proof
while CSS xtask checks are recorded as pre-existing dirty blockers.

## Legal Commands

Run from the repo root with the skinny manifest:

```sh
cargo run --manifest-path skinny/Cargo.toml --profile ax-iter -p xtask -- check-json
cargo run --manifest-path skinny/Cargo.toml --profile ax-iter -p xtask -- check-css-l4-declaration-values
cargo run --manifest-path skinny/Cargo.toml --profile ax-iter -p xtask -- check-real-typed
```
