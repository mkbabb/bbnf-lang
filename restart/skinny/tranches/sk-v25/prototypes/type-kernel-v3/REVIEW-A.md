# Type-kernel v3 compiler-library review A

Status: **reject as a completed kill-carrier; retain as quarantined partial evidence only**. This review authorizes neither production adoption nor a compatibility path.

## Command replay

From the repository root, I ran each documented check twice:

- `cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --check`: exit 0 twice.
- `cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --all-targets -- -D warnings`: exit 0 twice. Cargo printed only the repository's unused-patch notices; the crate emitted no denied warning.
- `cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml`: exit 0 twice, with 32 integration tests passed, 0 failed, 0 ignored, 0 source-unit tests, and 0 doc tests.

The external tree mirrors every source module, including `solve/{unify,row,zonk}`; repository search found no `#[test]` or `#[cfg(test)]` in `src`. A search for the v3 package/module names under `crates` and the root `Cargo.toml` found no production connection.

Disposable probes lived at `/tmp/bbnf-type-kernel-v3-review-a.CJM7bj`. The first `cargo test --manifest-path /tmp/bbnf-type-kernel-v3-review-a.CJM7bj/Cargo.toml -- --nocapture` reproduced four discrepancies before a fifth probe aborted the process with SIGABRT/stack overflow. A preservation replay with the depth witness reduced completed the remaining cases as 7 passed/1 harness assertion failure. The failing harness had checked the wrong diagnostic stream: its `tsc` subprocess itself exited 2 with TS2322, reproducing the intended `null`-assignment defect.

## Correctness findings

### 1. A retained free recursive ID escapes `finish`

**Contradicted.** `StoreBuilder::finish` checks only the IDs supplied in its `roots` iterator, then drops `free_rec` when it constructs `TypeStore` (`src/type_kernel/store.rs`, `StoreBuilder::finish` and the `TypeStore` fields). Other public `TypeId` values returned before `finish` remain usable by `fingerprint` and `solve`.

The disposable probe created a free `RecVar` term, finished the store with an unrelated `I64` root, and retained the free ID. `fingerprint` returned `free-rec-0`; `solve` accepted that ID through ingestion and later returned `InternalCycle`. Thus the store is not sealed at its public boundary, and escaping recursion is diagnosed late and inaccurately. The nested-use, named-vacuity, prebuilt-composition, alpha-fingerprint, and panic tests remain useful, but they do not establish the claimed closed-store invariant.

### 2. Recursive reification is not alpha-local per root and breaks schemes

**Contradicted.** `Zonker` owns one `binders` map and one `next_binder` counter for the entire root set, and `State::reify_roots` does not reset them between top-level roots (`src/type_kernel/solve/zonk.rs`, `Zonker`, `reify_roots`, and the `Node::Mu` arm). `Scheme::instantiate` starts its own binder numbering at zero and requires every `PolyType::Rec(index)` to find that locally created index (`src/type_kernel/scheme.rs`, `InstantiationState` and `instantiate_type`).

The probe solved two separately constructed alpha-equivalent `mu a. Option<a>` roots in one solution. They reified as `Mu(Option(Rec(0)))` and `Mu(Option(Rec(1)))`, compared unequal, and generalizing then instantiating the second root failed with `InvalidRecursiveReference`. The checked-in scheme round trip covers only a root whose numbering starts at zero.

### 3. The operation budget is not total and can still permit process abort

**Contradicted.** `infer` charges enter/exit work, but `type_desc` recursively walks nested real `TypeDesc` values without an `OperationContext::step` (`src/type_kernel/infer.rs`, `infer_target` and `type_desc`). The depth of this boundary input therefore has no effect on the visible inference charge.

The disposable probe passed an `IrNode::Map` whose real `FnDescriptor::Expr` returned a 1,024-deep `TypeDesc::Option`, with an operation limit of four. The four IR enter/exit steps were admitted; descriptor recursion then overflowed the test-thread stack and aborted the process. This directly contradicts the total-budget and typed-exhaustion claims. The checked-in 20,000-option test is still valid evidence for early bounded exhaustion along one solver path, not for total inference/reification safety.

The preservation replay also confirmed that depth-1 and depth-128 descriptors both consume exactly four inference operations, and that projection of a million-byte requested identifier plus its digest consumes the same three operations as a one-byte identifier. The source has the same accounting granularity weakness in `solve::stable_identity` and `project::digest`: each phase receives a tick, but formatting/hashing work is proportional to uncharged material size.

### 4. The live enum bridge is exhaustive but not the real production mapping

**Contradicted.** The wildcard-free matches do compile against all current enum variants, and every constructed node gets a `NodeTyping` and equality constraint. However, exhaustiveness is vocabulary coverage, not semantic agreement.

`host_function` maps `FnDescriptor::EnumWrap { variant }` to the type name `bbnf::Enum::{variant}` (`src/type_kernel/infer.rs`, `host_function`). The real production generator maps that descriptor to `TypeDesc::Enum` (`crates/ir/src/passes/types/generate.rs`, the `IrNode::Map` arm). The probe observed `Named("bbnf::Enum::Variant")` for `EnumWrap` but `Named("bbnf::Enum")` for a declared `TypeDesc::Enum`.

Likewise, v3 maps `IrNode::Ref` directly from the supplied rule `TypeDesc`; the probe mapped a `Vec<U8>` rule to `List(U8)`. Production's `RefConstraint` propagates only scalar types and maps compound references to `BoxedEnum` while recording an `UnresolvedCompoundRef` obligation (`crates/ir/src/passes/types/constraint/reference.rs`). `HostEnvironment::with_rule` also bypasses `GrammarIR`, rule metadata, vector context, sequence span-collapse policy, and obligation publication. The checked-in scalar `U16` reference fixture does not exercise these differences.

### 5. `TypeSolution::identity` is not stable across clean equivalent stores

**Contradicted.** `stable_identity` hashes `revision`, while the store revision comes from process-global `NEXT_SCOPE` allocation (`src/type_kernel/solve.rs`, `stable_identity`; `src/type_kernel/store.rs`, `NEXT_SCOPE` and `StoreBuilder::new`). The probe built and solved the same `I64` store twice and obtained different identities: `fnv1a64:9b6e2eb59f2ee176` and `fnv1a64:e940afd009341b30`.

The checked-in artifact test does establish byte identity for two calls using the same `SolvedType`, requested name, toolchain strings, and reproduction command. It does not establish a reproducible solution identity across clean construction.

### 6. Row evidence is useful but does not establish the full named algebra gate

**Not established.** Source inspection found one flexible substitution authority, field-recursive row occurs, representative lacks propagation, path-local row visitation, and public `FreeRowVar` reification. The checked-in counterexamples for field recursion, compatible open/open joins, sibling zonk, lacks conflict, and 1,024 distinct input permutations pass.

The test named for identity, commutativity, associativity, and idempotence compares equality-constraint closures; its “associativity” comparison is transitive equality over `a`, `b`, and `c`, not associativity of a row composition operator. There is no corresponding variant-row law matrix or monotonicity witness. No new row counterexample was established in this review, but the evidence is narrower than the GREEN claim.

### 7. Projection and physical layout have narrower support than the manifest wording

**Layout supported for the explicit carrier scope.** `lower_physical` records size, alignment, fields, offsets, padding, tag kind, target, representation, and modeled/measured status; opaque Rust containers are rejected (`src/type_kernel/layout.rs`). The checked-in native tests passed on the recorded Apple arm64 host for `Span`, the three-word header, a padded struct, explicit tagged `U64`, and the null-pointer niche observation. This is not a layout contract for Rust `String`, `Vec`, or general semantic types. `LayoutProvenance` and `NativeMeasurement` do not themselves store the exact toolchain identity advertised in the evidence narrative.

**Projection contradicted for additional ordinary values.** The checked-in real-toolchain test proves a closed top-level record containing primitive fields and a singleton tuple under the tested hostile labels. `tests/type_kernel/project.rs::hostile_record` contains no nested record, variant, function, option-of-function, repeated named type, or string-literal escape case, so the manifest's “nested values” and full hostile matrix claims exceed that fixture.

`IdentifierAllocator::allocate` increments on every occurrence rather than memoizing a source identifier, while both emitters route every `SolvedType::Named` occurrence through that allocator (`src/type_kernel/project.rs`, `IdentifierAllocator::allocate`, `RustEmitter::ty`, and `TypeScriptEmitter::ty`). The preservation replay generated a Rust tuple from two occurrences of `Named("Shared")` as `(Shared, Shared_2)`; with only the intended `Shared` declaration, `rustc -D warnings` rejected the nonexistent renamed type. The TypeScript emitter also renders `Option<Function>` without grouping the function type, producing `(arg0: number) => boolean | null` rather than `((arg0: number) => boolean) | null`; `tsc --strict` exited 2 with TS2322 when a valid null option value was assigned. The one failed probe assertion concerned where that diagnostic was captured, not whether the compiler rejected the value.

## Major-claim disposition

| Claim | Disposition | Evidence |
| --- | --- | --- |
| Explicit recursive identities and sealed recursive terms | **Contradicted** | Retained free ID passed `finish`; two-root reification broke alpha equality and scheme instantiation. |
| Flexible rows, lacks, occurs, and zonk | **Supported for checked counterexamples; full algebra not established** | Two clean 32-test runs; `solve/row.rs` and `solve/zonk.rs`; missing genuine variant/algebra breadth. |
| Qualified schemes and value restriction | **Contradicted as a general claim** | Lacks/freshness/rigid tests pass, but the second recursive root instantiates as `InvalidRecursiveReference`. |
| Total operation accounting and typed exhaustion | **Contradicted** | Four-budget, 1,024-deep real `TypeDesc` probe aborted with stack overflow. |
| Minimal solver provenance | **Supported for the checked failures only** | Contradiction, type-occurs, row-occurs, lacks, and zero-budget tests pass and exclude the unrelated tautology where asserted. |
| Real `IrNode`/`FnDescriptor`/`TypeDesc` semantics | **Contradicted** | Exhaustive vocabulary match exists, but `EnumWrap` and compound `Ref` differ from production mapping. |
| Apple arm64 explicit-carrier layout | **Supported in carrier scope** | Native layout tests pass; opaque library containers reject. |
| Generated Rust/TypeScript values and deterministic output | **Contradicted as a general claim** | Tested record values compile and same-input artifacts match, but repeated named Rust types and TypeScript option-of-function values fail; clean solution identities drift. |
| External test structure | **Supported** | Exact source/test module mirror, 32 external tests, zero source tests, zero ignored tests. |
| Production integration | **Absent by design** | No v3 references under production `crates`; old projector, sidecars, and consumers remain. |

## Predecessor gates 1–9

1. **RED — store/binder.** Exact construction witnesses pass, but retained escaping IDs and non-local recursive reification violate the public recursive-type contract.
2. **RED with substantial partial evidence — rows/zonk.** The v2 counterexamples pass; the complete named law/variant gate is not established.
3. **RED — schemes.** Ordinary lacks, freshness, levels, value restriction, and rigid sharing pass; multi-root recursive schemes fail.
4. **RED — budget/proof.** Focused causal proofs pass, but a bounded real boundary input still aborts the process.
5. **RED — production semantics.** Enum vocabulary coverage is exhaustive, but important `FnDescriptor` and compound-reference outcomes disagree with production and no `GrammarIR` comparison exists.
6. **GREEN only for the quarantined explicit-carrier model — layout.** The measured host probes and opaque-container rejection pass. This grants no general Rust or production ABI authority.
7. **RED with partial compiler evidence — projection.** The checked record/singleton values compile and repeat generation is byte-identical, but the required nested/variant/function/named matrix is absent.
8. **RED — atomic production cut.** Deliberately untouched; ignored propagation results, masking fallbacks, old sidecars, and `project_types` remain.
9. **RED — consumers/performance/audits.** No shared sealed production solution, corpus/runtime consumers, benchmarks, production transposition, or two clean adoption audits exist.

## Smallest next implementation step

Repair the recursive boundary before extending rows or projection. Persist the declared closed export set in `TypeStore` and reject any public fingerprint, constraint endpoint, or solve root outside it; then reset recursive binder numbering for each top-level reified type. Add external regressions for a retained free `TypeId`, two alpha-equivalent recursive roots in one solution, and generalize/instantiate of the second root. Only after that slice is green should the uncharged iterative `TypeDesc` bridge be addressed. No production file should consume this carrier meanwhile.
