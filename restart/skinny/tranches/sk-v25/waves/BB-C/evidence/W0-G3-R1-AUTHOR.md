# BB-C.W0 G3 deletion R1 — author record

Status: **IMPLEMENTED NARROW SLICE; ZERO WAVE OR MOVEMENT CREDIT; HOSTILE RED.**

Model: `gpt-5.6-sol`  
Reasoning: `ultra`  
Workflow: `v2`

## Scope

Only the unsound production equality `Alt([x, epsilon]) -> x` was removed. This
record claims neither BB-C.W0 completion nor optimizer, graph, compiler, package,
consumer, performance, or movement authority.

The implementation:

- deleted `WrapOfEpsilonScalar`, its match and side-table machinery;
- removed its public export and default-rule registration;
- deleted the four positive mechanics tests that asserted the unsound equality;
- added one direct-pipeline witness for `start = "x" | epsilon` over `""` and
  `"x"`.

## Frozen subject

| Path | SHA-256 |
|---|---|
| `crates/ir/src/egraph/rules/universal.rs` | `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b` |
| `crates/ir/src/egraph/rules/mod.rs` | `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4` |
| `crates/ir/tests/egraph/egraph_universal.rs` | `72a5ee30742d0bc5647b68db83fc19faf306223d0bb37dcf6e98ecd9f6394070` |
| `crates/core/tests/egraph_semantic.rs` | `dbd658dc672acc362b990b070a964466e10e2d100b81320c6b408fa9519c4d5c` |

## Replayed facts

Root independently replayed on Apple ARM with `Cargo.lock` byte-identical before
and after at
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`:

```text
cargo test --offline --locked -p bbnf-ir --test egraph egraph_universal::
7 passed; 0 failed; 26 filtered

cargo test --offline --locked -p bbnf --test egraph_semantic \
  egraph_semantic_nullable_alt_preserves_empty_branch
1 passed; 0 failed; 7 filtered
```

`git diff --check` passed. The author agent reported the same focused semantic
witness RED before deletion and GREEN after deletion, but no immutable pre-fix
payload was retained; that report is not acceptance evidence.

