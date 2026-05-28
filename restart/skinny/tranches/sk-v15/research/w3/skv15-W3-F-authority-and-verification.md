# SK-V15 W3-F Research: Authority And Verification

Scope: W3 authority, receiver obligations, dirty-tree preservation.

Status: read-only research.

## Dispatch Authority

W3 entry gate is W2 admitted. W2 is admitted by
`restart/skinny/tranches/sk-v15/research/w2/skv15-W2-challenge-v2.md` and
`restart/skinny/tranches/sk-v15/research/w2/skv15-W2-redress.md`.

SPEC W3 envelope: risk High, 150-320 manual source/test LOC, regen/check
evidence, docs 80-180 LOC, entry W2 admitted, exit one coherent generic leak
family removed with same-wave generator consumer.

`DEP-W3-W6-CSS-PROVIDER-TEMPLATE` authorizes W3 neutralization of CSS
provider/template/static profile roster and runtime family fanout. Deletion of
provider/template proof waits for W6. `DEP-W6-CSS-GENERATED-RS` and
`DEP-W6-CSS-SUMMARY-FACT-STREAM` block old CSS parser/fact-stream/full-parse
retirement before W6.

## Receiver Obligations

For `grammar_provider.rs`, SPEC requires CSS L4 plus Sheets or BBNF-self.
For `runtime_generator.rs`, SPEC requires CSS L4 plus Sheets, BBNF-self, CSV,
or math. For xtask regen/check, SPEC requires CSS L4 plus at least one non-CSS
generated receiver.

Current commands expose CSS and JSON/real-typed, but no Sheets/BBNF-self/CSV/math
generated receiver. W3 must either add a real non-CSS receiver proof within
scope or record a row-level intrinsic block. A synthetic metadata-driven
non-CSS frontend profile test can prove provider generality only if it does not
become a paper receiver detached from executable codegen.

## Challenge Trigger

W3 is a mandatory seven-lens CHALLENGE candidate because it changes generic
generation/provider behavior. The plan must reject grammar-family branches,
parallel source passes, second tape, Track 1 == Track 2 sidecars, and CSS proof
retirement before typed provider evidence.

## Dirty Tree

Current W3 target paths are clean. Unrelated dirty paths include root
`crates/core/src/runtime/**`, `docs/precepts`, old SK-V12/SK-V13 research JSONs,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs`, seven
`skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`, and root
`xtask/src/{main.rs,regen_simple_runtime.rs}`.

## Verification Matrix

```sh
git status --short
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p codegen
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check
```

Add CSS `check-css-l4-*` commands only if W3 owns the generated output
predicate or isolates the pre-existing dirty blockers.
