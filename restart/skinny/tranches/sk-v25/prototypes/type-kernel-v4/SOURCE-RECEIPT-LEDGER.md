# V4 source and receipt ledger

Status: **INCOMPLETE_ASSAY / awaiting two fresh hostile gestalt passes**.

## Source ownership

| Authority | Owned invariant | External assay |
| --- | --- | --- |
| `type_kernel.rs` | checked budget, origins, proof slices, typed failures | zero-budget and attributable failure tests via mirrored modules |
| `store.rs` | private draft arenas, duplicate labels, explicit recursion, consumed close | compile-fail draft crater; escape, alpha, nesting, vacuity |
| `infer.rs` | immutable generic snapshot and whole-grammar constraint generation | deep boundary, whole vocabulary, recursive JSON, host signature, CSS-shape no-overfit |
| `solve.rs` | transactional coordination and content semantic digest | independent-store identity, proof reachability, no-publication failure |
| `solve/unify.rs` | iterative type equality, type occurs, complete lowering | row-field type occurs and endpoint symmetry |
| `solve/row.rs` | one record/variant row substitution/lacks authority | two-kind law matrix and 2×1,024 distinct permutations |
| `solve/zonk.rs` | root-local flat normal form and canonical bytes | sibling/repeat identity, deep flat publication |
| `scheme.rs` | batch flat qualified schemes and reusable rigids | multi-root recursion, lacks/freshness, value restriction, deep budget |
| `layout.rs` | semantic/layout profile split and narrow explicit carriers | carrier dimensions, target rejection, toolchain mutation |
| `project.rs` | independent precedence-aware target emitters and symbol interning | rustc/tsc hostile value craters, clean byte identity, open rejection |

## Determinism boundaries

Included in semantic identity: constructor tags, canonical child edges, ordered
tuple/function positions, canonical row labels, open-tail predicates/levels,
root-local recursive binders, export keys, and immutable input semantic bytes.

Excluded: draft/store scope, validation revision, pointer/address, absolute path,
hash iteration, process allocation order, compiler identity, and physical layout
measurement identity. Toolchain identity is instead explicit in artifact/layout
provenance.

## Final receipt hashes

The final author fills these with the direct `shasum -a 256` results after the
two clean replays; reviewers must reproduce them rather than trust this ledger:

```text
V4 Cargo.lock: 6de0490fde327c06b8ebac5ae797cb3570aa33841fa80b8e8e12211debd355bc
repository Cargo.lock: a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908
source/test tree receipt: 0df94e3316df5b262b4e24dfb96f7af828c67542169a3b41ba52bb77474408e1
```

The tree receipt command is deterministic:

```sh
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src \
     restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests \
     -type f -print0 | sort -z | xargs -0 shasum -a 256 | shasum -a 256
```

No author status in this file upgrades the assay disposition.
