# Type-kernel V3 fresh adjudication

Status: **V3 is superseded as a completed kill-carrier. It remains quarantined
partial evidence and is not production authority.**

This ruling supersedes the judgments in `REVIEW-A.md` and `REVIEW-B.md`; it does
not edit or erase either review. Both reviews were treated as allegations, not
authority. The adjudication was performed against repository HEAD
`af15f63e0d2d3d719938c13b906a50acbb92ea3b` on 2026-07-18.

I read completely:

- V3 `README.md`, `EVIDENCE.md`, `REVIEW-A.md`, and `REVIEW-B.md`;
- V2 `ADJUDICATION.md`;
- `waves/BB-B/W0.md` through `W3.md`;
- every V3 source file and every V3 external test;
- only the live `bbnf-ir` files required to adjudicate the type vocabulary and
  projection semantics: `types/{type_desc,fn_descriptor,node,rule,grammar}.rs`,
  `passes/types/{generate,mod}.rs`, and
  `passes/types/constraint/{operators,reference,seq,alt,revise}.rs`.

Fresh counterexamples were created only under
`/tmp/bbnf-type-kernel-v3-adjudication.UCKy5h`. No durable change was made to
V3 source/tests, production, reviews, ledgers, or locks. The documented Cargo replay
temporarily generated the prototype's ignored `Cargo.lock`; it was removed
after the two runs, and the repository root lock remained at SHA-256
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.
This adjudication file is the only durable write.

Ruling terms:

- **CORRECT**: independently reproduced by a fresh command/probe or compelled by
  the current source.
- **INCORRECT**: a material assertion is disproved or a named test does not
  establish the law attributed to it.
- **UNPROVEN**: no counterexample was found, but the evidence does not cover the
  asserted domain.

## Verdict

**Do not repair V3 in place and do not transpose it into production. Build a
clean-break V4 closure carrier.**

V3 contains real advances over V2: explicit recursive identities, one flexible
row-substitution authority, field-recursive row occurs, representative lacks,
path-local public row reification, qualified schemes with reusable rigids, a
parent-reachable proof DAG, explicit Apple-arm64 carrier layouts, target-aware
identifier normalization, and a path-isomorphic external test tree. The clean
suite and those organs are genuine evidence.

They coexist with six ordinary public counterexamples:

1. an unclosed recursive `TypeId` remains usable after `finish`;
2. recursive numbering is global across solution roots, breaking alpha equality
   and scheme instantiation for the second root;
3. binding an old row to a young field fails to lower the field's type level;
4. a live deeply nested `TypeDesc` bypasses the budget and aborts the process;
5. repeated semantic names and TypeScript option-of-function values generate
   invalid target programs;
6. solution identity hashes process-global store scope rather than semantic
   content.

The inference boundary also consumes one tree whose rule references have already
been assigned `TypeDesc` values. It is exhaustive vocabulary coverage, not a
whole-grammar type solution. Its Repeat, compound Ref, and EnumWrap projections
disagree with the live projector. These are not isolated missing assertions: the
required corrections change public handle types, the solved representation,
operation-bearing method signatures, whole-grammar ownership, scheme traversal,
semantic identity, and target symbol allocation. Calling that result V3 repair
would conceal a second clean break inside the same carrier.

Under the predecessor's binary closure standard, gates **1–5 and 7–9 are RED**.
Gate 2 has substantial positive record-row evidence but remains RED because its
required variant and monotonicity matrix is absent and the alleged associativity
test is not an associativity law. Gate 6 alone is GREEN, and only for the
explicit, quarantined Apple-arm64 carriers actually modeled and measured.

## Exact fresh probe results

The disposable probe crate passed formatting, strict clippy, and all eight
grouped adjudication tests. Its load-bearing output was:

```text
escaped-rec-fingerprint=free-rec-0
escaped-rec-solve=InternalCycle

multi-root-first=Mu(Option(Rec(0)))
multi-root-second=Mu(Option(Rec(1)))
multi-root-equal=false
multi-root-second-scheme=InvalidRecursiveReference

captured-young-type-quantifiers=1

descriptor-depth-1-operations=4
descriptor-depth-128-operations=4
projection-name-1-operations=3
projection-name-1000000-operations=3

identity-1=fnv1a64:e67af3903c1b0d3e
identity-2=fnv1a64:c39b3441c89fc0cc
identities-equal=false

v3-optional-span=Option(Primitive(Span))
v3-many-span=List(Primitive(Span))
v3-compound-ref=List(Primitive(U8))
v3-enum-wrap=Named("bbnf::Enum::Variant")
production-repeat-types=[(0, Span), (1, Span)]
production-compound-ref-types=[(0, Vec(U8)), (1, BoxedEnum)]
production-compound-ref-obligations=1
production-enum-wrap-types=[(0, Enum)]

generated-rust-named=pub type Payload = (External, External_2);
rustc-named-exit=1
rustc-named-stdout-bytes=0
rustc: E0425, External_2 not found

generated-ts-named=export type Payload = readonly [External, External_2];
tsc-named-exit=2
tsc-named-stdout=TS2552, External_2 not found
tsc-named-stderr=

generated-ts-option-function=
  export type MaybeFn = (arg0: number) => boolean | null;
tsc-option-function-exit=2
tsc-option-function-stdout=
  TS2322: Type 'null' is not assignable to type 'MaybeFn'.
tsc-option-function-stderr=
```

The separately built live-descriptor binary ran on an explicitly 512-KiB thread:

```text
command: /tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/target/debug/deep_type_desc
exit: 134

thread '<unknown>' (...) has overflowed its stack
fatal runtime error: stack overflow, aborting
```

Its input was `IrNode::Ref(0)` plus a real 20,000-deep
`TypeDesc::Option(...TypeDesc::U8)`, with `OperationContext::new(256)`.

The layout probe returned:

```text
layout-span=8/4
layout-header=24/8
layout-padded=16/8;word-offset=8;first-padding=7
layout-tagged=16/8;tag=Explicit { offset: 0, bytes: 1, none: 0, some: 1 }
layout-null-niche=8/8;tag=NullPointerNiche
native-std::string::String=24/8
native-std::vec::Vec<u8>=24/8
native-Option<String>=24/8
native-Option<NonNull<u8>> null niche=8/8
layout-opaque=OpaqueRustContainer("String") at layout.opaque
```

### The Review-A harness mismatch is not a product exoneration

Review A accurately disclosed one bad assertion in its preservation harness.
Fresh reproduction identifies it precisely: `npx tsc` writes TS2322 to **stdout**
and leaves stderr empty. An assertion that searched only stderr therefore fails
even though `tsc` exits 2. The harness assertion is wrong; the generated type is
also wrong. The product defect is the missing parentheses around a function type
under a union. These are separate facts.

The fresh named-type TypeScript failure likewise appeared on stdout. Rust's
E0425 appeared on stderr. All adjudication assertions used exit status plus the
appropriate stream or a combined stream.

## Review A claim-by-claim ruling

| Claim | Ruling | Independent adjudication |
| --- | --- | --- |
| A1. A retained free recursive ID escapes `finish`. | **CORRECT.** | `StoreBuilder::finish` validates only supplied roots (`store.rs:483-507`), while `TypeStore::fingerprint` accepts any scope-valid stored slot (`:206-220`). The public-only witness printed `free-rec-0`; `solve` accepted the retained handle and failed later as `InternalCycle`, which is both late and semantically inaccurate. |
| A2. Recursive reification is not alpha-local per root and breaks schemes. | **CORRECT.** | One `Zonker` owns one `binders` map and `next_binder` across all roots (`solve/zonk.rs:14-50,145-147`). Two alpha-equivalent roots became `Rec(0)` and `Rec(1)`; the latter's scheme instantiated as `InvalidRecursiveReference`. |
| A3. The operation budget is not total and can permit process abort. | **CORRECT.** | `infer::type_desc` recursively visits Option/Vec/Tuple/join descriptors without a step (`infer.rs:467-521`). The 20,000-deep real descriptor exited 134 under limit 256. Depth 1 and 128 Map descriptors both used exactly four inference operations. One-byte and one-million-byte projection names both used three; digest and normalization work are proportional but uncharged (`project.rs:190-200,620-627`). |
| A4. Exhaustive live-enum coverage is not production semantic agreement. | **CORRECT.** | Wildcard-free matches cover the current 14 `IrNode`, 6 `FnDescriptor`, and 18 `TypeDesc` variants. Fresh V3/production probes nevertheless reproduced EnumWrap `Named("bbnf::Enum::Variant")` versus `Enum`, and compound Ref `List(U8)` versus `BoxedEnum` plus one obligation. The V3 API accepts one node and pretyped rule descriptors (`infer.rs:68-89,140-225`), not a `GrammarIR` solution keyed by stable node/rule identity. |
| A5. `TypeSolution::identity` is not stable across equivalent stores. | **CORRECT.** | Store revision comes from `NEXT_SCOPE` (`store.rs:9,358`) and `stable_identity` hashes that revision (`solve.rs:406-417`). Two fresh isomorphic I64 stores produced different hashes. Revision is valid for handle checking; it is invalid as semantic content identity. |
| A6. Row evidence is useful but does not establish the full named algebra gate. | **CORRECT.** | The V2 field-occurs, open/open residual, lacks, sibling-zonk, repeated-zonk, order, and 1,024-distinct-permutation witnesses pass. The alleged associativity assertion compares two equivalent transitive equality bases (`{ab,bc}` and `{bc,ac}`); no row-composition operator is defined. There is no variant-row matrix or named monotonicity witness. |
| A7a. Explicit-carrier Apple-arm64 layout is supported at narrow scope. | **CORRECT.** | Fresh sizes, alignments, offsets, padding, explicit tag, null niche, measured library containers, and opaque-container rejection matched. This is not a semantic-to-layout lowering contract. `LayoutProvenance` and `NativeMeasurement` do not carry the exact rustc identity internally. |
| A7b. Projection claims exceed the checked matrix; repeated Named and TS option-function fail. | **CORRECT.** | Real `rustc -D warnings` and `tsc --strict` rejected the independently generated values. The checked hostile fixture contains a primitive record plus singleton tuple, not Named, variant, function, nested record/variant, or option-of-function coverage. “Nested values” is true only in the narrow singleton-tuple sense. |
| A8. External topology is mirrored and production is disconnected. | **CORRECT.** | Ten authority files have ten path-isomorphic external files; there are 32 external tests, zero source tests, and zero ignored tests. Repository search outside the prototype found no V3 consumer. Metadata reports one dependency arrow, V3 to local `bbnf-ir`. |

Review A's major-claim and gate table follows from these facts. Its conclusion
that V3 is quarantined partial evidence is correct. Its proposed first repair is
too small as a final disposition because it does not address the simultaneous
public-boundary, whole-grammar, budget, row-level, identity, and emitter breaks.

## Review B claim-by-claim ruling

| Claim | Ruling | Independent adjudication |
| --- | --- | --- |
| B1. `finish` does not seal public recursive handles. | **CORRECT.** | Same independent witness and source ruling as A1. No forged ID or unsafe code was used. |
| B2. A deep real `TypeDesc` bypasses the total budget and aborts. | **CORRECT.** | Fresh 20,000-depth, 512-KiB process exited 134 under a 256-operation limit. Scheme generalization/instantiation also has no operation-context parameter and recursively materializes `PolyType` (`scheme.rs:90-202,346-441,479-568`); total scheme work is therefore structurally impossible in this API. |
| B3. Row binding fails to lower levels reachable through fields. | **CORRECT.** | `bind_meta` calls `lower_levels` (`solve/unify.rs:225-244`); `bind_row` lowers only its residual tail row (`solve/row.rs:252-299`). The old-row/young-field witness generalized one type variable; the required result is zero. |
| B4. Coverage is exhaustive but Repeat, Ref, and EnumWrap disagree with production. | **CORRECT.** | Fresh differential output is recorded above. Production Optional/Repeat collapse Span (`constraint/operators.rs:49-55,98-104`), compound Ref becomes `BoxedEnum` with `UnresolvedCompoundRef` (`constraint/reference.rs:89-130`), and EnumWrap becomes `Enum` (`generate.rs:360-377`). |
| B5. Repeated Named types generate invalid Rust and TypeScript. | **CORRECT.** | The allocator counts occurrences rather than interning semantic symbols (`project.rs:180-200`); both emitters allocate every Named occurrence (`:320-364,422-466`). Rust failed E0425; TypeScript failed TS2552 at invented `External_2`. |
| B6. Solution identity is revision identity, not semantic identity. | **CORRECT.** | Same independent witness and source ruling as A5. Exact digest values are process-order dependent, but inequality is deterministic while revisions differ. |
| B7. Rows/zonk are supported for exercised records but not the whole gate. | **CORRECT WITH ONE MATERIAL CORRECTION.** | The positive inventory and the fact that all 1,024 orders are distinct are correct. The statement that the checked test covers four row-algebra laws is **INCORRECT** for associativity: it covers equivalence of transitive equality closures, not association of row composition. B's final conclusion that variant and monotonicity evidence is missing is correct. |
| B8. Apple-arm64 explicit layout and checked proof slices are supported narrowly. | **CORRECT.** | Fresh layout results match. Checked zero-budget, mismatch, type-occurs, row-occurs, and lacks slices pass; `OperationContext::reachable` follows parent edges. This does not restore totality. |
| B9. Goldilocks split is better but the public surface is broad. | **CORRECT.** | The ten-file authority split and mirror are factual. A reproducible textual count over the ten authority files gives 183 `pub fn/struct/enum/mod/type` declarations/accessors when the exported generator constant is excluded; the precise count is less important than the factual exposure of proof nodes, allocator internals, measurement helpers, rigid tokens, and phase-specific wrappers. |
| B10. Production remains disconnected and retains masking paths. | **CORRECT.** | `project_types` still returns `()`; both propagation results are ignored (`passes/types/mod.rs:67,127`); three extraction paths use `unwrap_or(TypeDesc::Span)` (`:192,220,226`); and `GrammarIR` still owns `types`, `type_map`, `payload_layouts`, `struct_registry`, and `type_obligations`. |

Review B's final rejection is correct. Its V4 direction is substantially right,
but the smallest coherent V4 must also make solved types and schemes flat/arena
backed: merely making traversal iterative while returning/dropping a
20,000-deep recursive `SolvedType` does not close the successful-depth half of
BB-B.W3.

## Independent domain findings

### I1 — closure certification must be a type boundary

Persisting a set of “declared closed IDs” in the current `TypeStore` would make
runtime checks possible, but it would leave the same raw handle type on both
sides of certification. The clean closure law is stronger:

```text
DraftTypeId / DraftRowId / DraftRecVar
    -- ProblemBuilder::close(exports + constraint endpoints) -->
ClosedProblem + RootId
```

No post-close public function accepts `DraftTypeId`. A copied free draft handle
can exist as inert data, but cannot be fingerprinted, constrained, solved,
generalized, or projected. `close` proves exact free-recursion closure for every
export and constraint endpoint and consumes the builder.

### I2 — recursive solved trees are themselves a depth hazard

V3's boundary bridge, fingerprint, zonk, `poly_type`, and instantiation are
recursive. The checked 20,000-depth solver test exhausts before reification; it
does not prove a successful 20,000-depth result. V4 must store semantic,
polytype, and solved terms as flat arenas addressed by opaque IDs. Traversal and
drop must be iterative; raising the budget must not turn a typed exhaustion into
a stack abort.

### I3 — production is an oracle, not normative permission for its fallbacks

The live projector is needed for differential agreement on successful fixtures,
but it is not an authority for ignored errors or missing host signatures.
V4 must match every explicitly agreed projection row below and record every
intentional deviation. `Expr { return_type: None } -> MissingHostSignature` is a
required clean-break deviation from production's current `Span` fallback.
Gate 8 separately deletes the incumbent ignored/masking paths; V4 must not copy
them to make a differential test green.

## Gates 1–9

| Gate | Disposition | Binding reason |
| --- | --- | --- |
| 1. Store/binder | **RED** | Nested outer use, vacuity, prebuilt composition, panic recovery, and single-root alpha fingerprints pass. Public closure is false and multi-root alpha locality is false. |
| 2. Rows/zonk | **RED with substantial retained evidence** | V2's three decisive record-row counterexamples are repaired; lacks, type/row occurs, sibling zonk, canonical ordering, and distinct permutations pass. Required variant, true extension/composition, and monotonicity matrices are absent. Binary closure does not admit AMBER. |
| 3. Schemes | **RED** | Ordinary lacks/freshness/rigids/value restriction pass. The second recursive root cannot instantiate, and an old row incorrectly generalizes a captured young field. Scheme work is also outside the shared operation context. |
| 4. Budget/proof | **RED** | Checked causal slices are useful. A live descriptor still aborts at exit 134 under a finite budget; descriptor bytes/nodes, large identifiers/digests, and all scheme traversals are not total-accounted. |
| 5. Real IR/host semantics | **RED** | Vocabulary coverage is exhaustive. Whole-grammar ownership, stable rule/node results, vec context, Seq policy, rule metadata, Repeat, compound Ref, and EnumWrap agreement are missing or contradicted. |
| 6. Apple-arm64 physical layout | **GREEN only for the quarantined explicit carriers** | The exact modeled/measured probes and opaque-container rejection pass. This grants no Rust-library ABI, general semantic lowering, or production authority. V4 still needs exact toolchain provenance in returned evidence and a checked semantic-to-carrier boundary. |
| 7. Rust/TypeScript projection | **RED** | The primitive record/singleton fixture and same-input byte identity pass. Repeated Named and TS option-function fail real compilers; the required full target matrix is absent; semantic solution identity drifts. |
| 8. Atomic production cut | **RED and out of V4 carrier scope** | No V3 production consumer exists. The void projector, ignored solver results, Span masking, sidecars, and old pass remain. |
| 9. Consumers/performance/audits | **RED and out of V4 carrier scope** | No shared production solution/plan, corpus runtime values, cold compile/allocation/byte baseline, production transposition, or two clean adoption audits exist. |

## Organ disposition

| V3 organ | Disposition for V4 |
| --- | --- |
| Ten-file Goldilocks topology and path-isomorphic external tree | **Retain.** It has clear invariant ownership and no V2 solver god-module. |
| Explicit store-scoped `RecVar` and exact free-recursion subtraction at `mu` | **Retain internally.** Replace raw post-finish `TypeId` access with draft-to-closed capability conversion. |
| Immutable interning | **Retain.** Canonical semantic identity must be based on canonical content, not allocation scope or slot order. |
| One flexible row-substitution authority, representative lacks, field-recursive occurs | **Retain and repair.** Row binding must lower levels through all fields/reachable rows before install. |
| Path-local row visitation and public-row reification | **Retain.** Extend to flat canonical solution arenas and both row kinds. |
| Iterative unification worklist | **Retain.** All adjacent traversals must become iterative too. |
| Qualified schemes, reusable rigid context, levels, syntactic-value restriction | **Retain policy; replace representation/traversal.** Polytypes become flat, alpha-local, and operation-bearing. |
| Parent-reachable proof DAG | **Retain.** Add total charge units and deterministic proof compression for bulk byte work. |
| Explicit Apple-arm64 carrier model and opaque-container rejection | **Retain narrowly.** Do not expose manual `StorageType` as an alternate semantic authority. |
| Target-specific normalization, keywords, quoted TS labels, singleton tuple, artifact manifest | **Retain policy; replace allocator/emitter core.** Intern semantic symbols separately from spelling collisions and use a precedence-aware type printer. |
| Exhaustive live enum matches and `MissingHostSignature` | **Retain.** Move them behind a whole-grammar immutable snapshot boundary. |
| V3 `TypeStore`, recursive `SolvedType`, recursive `PolyType`, `HostEnvironment::with_rule`, revision-hashed identity | **Delete, do not adapt.** These are the falsified closure carrier. |

## Bound V4 closure-carrier topology

V4 is a new sibling proof carrier, not a V3 branch, patch, feature flag, or
compatibility mode. Its source/test topology stays exact:

```text
prototypes/type-kernel-v4/src/type_kernel.rs
prototypes/type-kernel-v4/src/type_kernel/store.rs
prototypes/type-kernel-v4/src/type_kernel/infer.rs
prototypes/type-kernel-v4/src/type_kernel/solve.rs
prototypes/type-kernel-v4/src/type_kernel/solve/unify.rs
prototypes/type-kernel-v4/src/type_kernel/solve/row.rs
prototypes/type-kernel-v4/src/type_kernel/solve/zonk.rs
prototypes/type-kernel-v4/src/type_kernel/scheme.rs
prototypes/type-kernel-v4/src/type_kernel/layout.rs
prototypes/type-kernel-v4/src/type_kernel/project.rs

prototypes/type-kernel-v4/tests/type_kernel.rs
prototypes/type-kernel-v4/tests/type_kernel/store.rs
prototypes/type-kernel-v4/tests/type_kernel/infer.rs
prototypes/type-kernel-v4/tests/type_kernel/solve.rs
prototypes/type-kernel-v4/tests/type_kernel/solve/unify.rs
prototypes/type-kernel-v4/tests/type_kernel/solve/row.rs
prototypes/type-kernel-v4/tests/type_kernel/solve/zonk.rs
prototypes/type-kernel-v4/tests/type_kernel/scheme.rs
prototypes/type-kernel-v4/tests/type_kernel/layout.rs
prototypes/type-kernel-v4/tests/type_kernel/project.rs
```

No extra “oracle” source authority is added. Production-oracle helpers live in
the relevant external test file. `src/lib.rs` only re-exports the bounded public
facade.

The ownership flow is:

```text
read-only live GrammarIR semantic fields + live FnDescriptor/TypeDesc vocabulary
    -> KernelInput (validated canonical snapshot + InputDigest)
    -> infer (private ProblemBuilder with draft IDs)
    -> close (ClosedProblem; all constraint endpoints and roots certified)
    -> solve (flat immutable TypeSolution keyed by InputDigest/RuleId/NodeId)
    -> lower(target) (flat immutable LoweringPlan)
    -> project Rust / TypeScript artifacts
```

`KernelInput` must ignore incumbent result sidecars (`types`, `type_map`,
`payload_layouts`, `struct_registry`, `type_obligations`) as inputs. During the
carrier stage it may read the live durable DAG only to bind current `NodeId`
fixtures; it does not claim BB-B.W1 `Program` authority. `InputDigest` is derived
from canonical semantic bytes, strings, functions, rule metadata that affects
typing, and declared source inputs—not store scope, pointer, absolute path,
insertion slot, or hash iteration.

### Public boundary

The production-intended public surface is limited to:

```text
OperationContext
Origin / TypeError / TypeErrorKind / ProofSlice (read-only accessors)
KernelInput / InputDigest
ProblemBuilder / DraftTypeId / DraftRowId / DraftRecVar
ExportKey / ExportMap / ClosedProblem / RootId
TypeSolution / SolvedTypeId / SolvedTypeView / SemanticDigest
BindingForm / Scheme / RigidContext / InstantiatedRoot
TargetProfile / LoweringPlan / GeneratedArtifact / ArtifactProvenance

KernelInput::from_ir(&GrammarIR, &mut OperationContext) -> Result<KernelInput, TypeError>
infer(&KernelInput, &mut OperationContext) -> Result<ClosedProblem, TypeError>
ProblemBuilder::close(self, named_exports, &mut OperationContext)
    -> Result<(ClosedProblem, ExportMap), TypeError>
solve(ClosedProblem, &mut OperationContext) -> Result<TypeSolution, TypeError>
TypeSolution::rule_root(RuleId) -> Option<RootId>
TypeSolution::node_root(NodeId) -> Option<RootId>
TypeSolution::export_root(&ExportKey) -> Option<RootId>
generalize(&TypeSolution, RootId, env_level, BindingForm, &mut OperationContext)
    -> Result<Scheme, TypeError>
instantiate(&Scheme, &mut ProblemBuilder, &mut RigidContext, level,
            Origin, &mut OperationContext)
    -> Result<InstantiatedRoot, TypeError>
lower(&TypeSolution, TargetProfile, &mut OperationContext)
    -> Result<LoweringPlan, TypeError>
project(&LoweringPlan, TargetLanguage, ToolchainIdentity,
        &mut OperationContext)
    -> Result<GeneratedArtifact, TypeError>
```

All ID fields, arenas, substitutions, proof-graph mutation, identifier allocators,
native measurement helpers, and phase coordinators remain private. External tests
exercise measurement through returned evidence, not a general production helper.

`DraftTypeId` is valid only with `ProblemBuilder`. `ProblemBuilder::close`
consumes all draft constraints and an explicit named export set, validates every
endpoint and exact free-recursion set, and returns new `RootId` values. No
`ClosedProblem`, fingerprint, solve, scheme, layout, or projection API accepts a
draft ID. This compile-time separation is the closure carrier's defining law.

`TypeSolution`, `Scheme`, and `LoweringPlan` are flat arenas. Recursive binding is
root-local de Bruijn identity or an equivalent root-local canonical encoding.
Store revision remains private handle-validation data and is excluded from
`SemanticDigest`.

## Born-RED external tests

Every row below must be checked in RED before its implementation lands. A test
passes only for the named outcome; panic, abort, fallback, missing assertion, or
compiler rejection for a supposedly valid value is failure.

| External file | Born-RED witness | Required GREEN outcome |
| --- | --- | --- |
| `store.rs` | retain a free draft Rec handle, close an unrelated root, then attempt public use | A compile-fail consumer proves no post-close API accepts the draft handle; exporting the free draft returns `EscapingRecursion` with site. |
| `store.rs` | two separately built alpha-equivalent `mu a. Option<a>` exports in one problem | Equal solved views, equal per-root canonical bytes, both scheme-round-trip successfully. |
| `store.rs` | nested outer use, named inner vacuity, prebuilt composition, panic recovery | Preserve V3's good outcomes without mutable binder state. |
| `infer.rs` | live 20,000-deep Option `TypeDesc`, 512-KiB stack, limit 256 | Typed `Exhausted`, non-empty boundary proof, process exit 0. |
| `infer.rs` | same descriptor with a sufficient measured budget | Successful flat result on 512 KiB, iterative drop, process exit 0. |
| `infer.rs` | complete live grammar containing every IrNode/FnDescriptor/TypeDesc shape | Whole-grammar rule/node totality and the semantic agreement table below; added enum variants fail compilation. |
| `solve/row.rs` | level-0 row equals `{x: level-2 meta}` | Generalization at level 0 quantifies zero captured variables; two instances share the environment meta. |
| `solve/row.rs` | full row-law matrix below, for Record and Variant | All success/failure/proof/normal-form cells match exactly. |
| `solve/zonk.rs` | sibling shared rows, two recursive roots, repeated reification | Complete path-local flat normal form; no meta/internal residual; repeated semantic bytes identical. |
| `scheme.rs` | second recursive root, deep scheme, lacks, rigid reuse, level cutoff, both binding forms | Alpha-local successful instantiation; iterative charged traversal; predicates re-emitted; named value restriction. |
| `solve.rs` | equivalent stores built after unrelated allocations | Same semantic digest, different private validation revisions. |
| `solve.rs` | contradiction plus unrelated tautology; zero budget; deep success/exhaustion | Minimal reachable causal slice; unrelated origin absent; every boundary error attributed. |
| `layout.rs` | existing explicit carrier probes plus toolchain mutation | Same physical results; returned provenance contains exact target/toolchain; changing toolchain evidence changes measurement identity, not modeled semantic identity. |
| `project.rs` | repeated Named, two distinct colliding names, TS option-function | Repeated symbols reuse one spelling; distinct symbols suffix; valid null value compiles with grouped arrow type. |
| `project.rs` | complete target matrix below and million-byte hostile names | Real compilers accept/reject exactly as specified; charge grows with input/emitted bytes; two clean artifacts are byte-identical. |

## V4 row-law matrix

Run every applicable row cell twice, once as `Record` and once as `Variant`.
Field insertion order, constraint order, root order, and hash seed are independent
dimensions. “Canonical equal” means identical flat solved bytes and semantic
digest, not merely `PartialEq` after one traversal.

| Law/case | Required result |
| --- | --- |
| Closed reflexivity `{a:T} = {a:T}` | Success; identical canonical row; no tail. |
| Closed permutation `{a:T,b:U}` versus `{b:U,a:T}` | Success; labels in canonical order. |
| Closed missing/extra label | `ClosedRow` naming the exact sorted difference and both equation causes. |
| Duplicate label at construction | `DuplicateRowLabel` before solve with boundary origin. |
| Open/closed extension `{a:T \| r} = {a:T,b:U}` | Success; `r = {b:U}` closed; prefix `a` remains lacking from `r`. |
| Closed/open symmetric form | Same canonical result/digest as open/closed. |
| Open/open, distinct tails, disjoint residuals | Success through one fresh flexible shared tail; no solver residual number escapes. |
| Open/open, equal prefixes | Canonically union tails; orientation independent. |
| Open/open, same tail with unequal prefixes | Typed failure; a row cannot contain each side's prefix through its own tail. |
| Lacks identity and union | Repeating one lacks is idempotent; merging compatible lacks yields sorted set union. |
| Lacks conflict | `RowLacks` contains both predicate and field/equation causes; unrelated facts absent. |
| Row occurs, direct tail | `RowOccurs`. |
| Row occurs through field type | `RowOccurs` for nested Record and nested Variant. |
| Row occurs through prior substitutions | `RowOccurs`; order independent. |
| Type occurs through row field/tail substitution | `TypeOccurs`; order independent. |
| Level capture through every field/reachable row | All reachable type/row levels lower to the bound row level before substitution install. |
| Equality symmetry | Swapping equation endpoints preserves canonical bytes/digest/proof-origin set. |
| Equality transitive bases | `{a=b,b=c}` and `{b=c,a=c}` yield the same closure; name this transitivity, not associativity. |
| Disjoint extension identity | Extending by the empty field set is identity. |
| Disjoint extension commutativity | Extending by disjoint A then B equals B then A. |
| Disjoint extension associativity | `(r + A) + (B + C)` equals `((r + A) + B) + C` for pairwise-disjoint sets. |
| Extension non-idempotence on duplicate labels | A second same-label extension rejects; do not misname duplicate rejection as algebraic idempotence. |
| Monotonic compatible constraint addition | Adding a compatible equality/lacks fact preserves prior solved fields and may only refine tails/levels/predicates. |
| Monotonic conflicting constraint addition | Typed failure and no partial `TypeSolution` publication. |
| Zonk sibling/path locality | Shared substitutions complete in every sibling; visiting one root cannot suppress another. |
| Repeated zonk | Canonical bytes/digest identical; no meta, draft ID, solver row, or internal residual appears. |
| Permutations | At least 1,024 genuinely distinct permutations over a row-heavy mixed equality/lacks system for each row kind. |

This matrix replaces the ambiguous V3 “identity/commutativity/associativity/
idempotence” test name with explicit operators and domains.

## V4 semantic agreement table

The gate compares both semantic result and derived projection where production
currently distinguishes collapsed and structural forms. “Oracle” means the live
projector on successful fixtures. Named deviations are not accidental drift.

| Boundary case | Live production oracle | V3 result | Bound V4 result |
| --- | --- | --- | --- |
| Literal / Regex / Epsilon | `Span` | `Span` | `Span`; agree. |
| Empty Seq | `Tuple([])` | `Tuple([])` | `Tuple([])`; agree. |
| One-child Seq | child type | one-item tuple | Match production result; retain structural arity in `LoweringPlan` only where a consumer requires it. |
| Multi-child Seq | policy-sensitive span compression, pretty preservation, sp-method override/revert, pair flattening | unconditional tuple | Differentially match `collapse_simple_spans`, `@pretty`, `has_sp_method`, transparency, and flattening for both node result and structural plan. |
| Homogeneous Alt | common/effective payload type | exact common type | Match production including factored `Tuple([Span,T]) -> T` payload equivalence. |
| Heterogeneous Alt | `HeterogeneousAltJoin` plus named obligation | anonymous branch-index Variant | Semantic variant is explicit; projection is the exact heterogeneous obligation with stable branch evidence and site. No silent BoxedEnum. |
| Optional Span | result `Span`; structural node `Option<Span>` | `Option<Span>` only | Public projected result `Span` plus explicit structural `Option<Span>` in the plan. |
| Many/Many1 Span | `Span` | `List<Span>` | `Span` unless a BB-B.W0 runtime-value oracle proves a different semantic value; record any deviation before implementation. |
| Optional non-Span | `Option<T>`; transparent Ref has `Option<Enum>` policy | `Option<T>` without rule metadata | Match oracle including transparency. |
| Repeat non-Span | `Vec<vec-context(T)>` | `List<T>` without vec context | Match normal and vec-context node maps. |
| Scalar Ref | target scalar | target scalar | Agree and retain target provenance. |
| Compound Ref | `BoxedEnum` plus `UnresolvedCompoundRef` structural obligation | direct target compound | Semantic target remains visible; projected node is `BoxedEnum` and carries the exact named obligation. |
| Ref in vec context | `Enum` | no distinct context | `Enum`; publish both normal and vec-context result. |
| EnumWrap | `Enum` | variant-specific `Named` | Enum type, with variant identity as constructor provenance rather than a distinct type name. |
| BoxWrap / NumberConvert / HexConvert / SpanCapture | `BoxedEnum` / F64 / U32 / Span | same | Agree. |
| Expr with declared return | exact `TypeDesc` | bridged recursively | Exact iterative bridge; Named identity preserved; every descriptor node charged. |
| Expr without declared return | current fallback `Span` | `MissingHostSignature` | **Named deviation:** `MissingHostSignature`; never copy the fallback. |
| Skip / Next / Minus | kept child | kept child | Agree in normal and vec contexts; discarded/predicate child still classified and diagnosed. |
| Negate | `Tuple([])` | `Tuple([])` | Agree. |
| OptionalWhitespace | transparent | transparent | Agree while preserving rule policy/provenance. |
| TokenDispatch | join mapped continuations + fallback; token does not determine result | token classified, same broad join | Token gets a stable fact/discriminator classification but does not enter the result join; arm/fallback and map semantics match oracle. |

The agreement tests must construct a real whole `GrammarIR`, run both V4 and the
live projector, and compare stable rule/node outcomes. Merely constructing every
enum variant is a separate drift test, not agreement evidence.

## V4 Rust/TypeScript compilation matrix

Each accepted row generates a real value consumer for both targets and runs
`rustc --edition=2024 -D warnings` and
`npx --yes tsc --strict --noEmit --lib es2022,dom`. Each rejected row must fail
inside V4 with a typed projection error before invoking a compiler.

| Matrix row | Required assertions |
| --- | --- |
| Every primitive | Definition and real value compile; numeric widths retain Rust spelling and use the declared TS number policy. |
| Empty, singleton, and N-tuples | Rust singleton has comma; TS uses readonly tuples; arity preserved. |
| List and nested List/Option | Generic/union precedence correct; real empty/non-empty/null values compile. |
| Function, function returning union, option-of-function, list-of-function | Precedence-aware printer groups arrow/union/generic positions; null option value compiles. |
| Repeated `Named("Shared")` | One semantic symbol and one target spelling at every occurrence; one external declaration satisfies all uses. |
| Distinct names with the same normalized spelling | Stable `_2`, `_3` allocation by semantic symbol, not occurrence. |
| Closed record and nested record | Real nested values compile; repeated field names in different scopes do not spuriously collide. |
| Closed variant, nested variant, and empty variant | Real constructors/discriminants compile; empty variant maps to uninhabited `enum`/`never`. |
| Hostile type names | Rust and TS keywords, punctuation, Unicode policy, empty, leading digit, normalized collision. |
| Hostile field/variant labels | Keywords, punctuation, quote, slash, backslash, newline, tab, control scalar, Unicode; TS labels/literals escaped, Rust normalized with collision table. |
| Source/semantic/toolchain manifests | Source digest covers emitted body; semantic digest is canonical and scope-free; exact toolchain and reproduction command present. |
| Two fresh generations | Byte-identical definitions and manifests from equivalent independently built solutions. |
| Million-byte requested name/label | Typed exhaustion at small budget; charged work grows with scalars/bytes; no fixed three-step accounting. |
| Free type, open row, recursive type without explicit carrier | Typed `OpenTypeVariable`, `OpenRow`, or `RecursiveTypeRequiresCarrier`; compiler not invoked. |

Identifier allocation has two layers:

1. a semantic symbol interner maps the same source symbol to the same target
   symbol forever within an artifact;
2. a spelling allocator resolves collisions only between distinct semantic
   symbols after target normalization/keyword escaping.

The type printer is precedence-aware; string concatenation of child type text is
not an admissible implementation.

## Total operation accounting contract

One mutable `OperationContext` is passed through snapshot, close, infer, solve,
scheme, identity, layout, and projection. No public arbitrary-input traversal
omits it. A charge is proportional to work, not one tick per phase.

| Phase | Minimum charged units |
| --- | --- |
| Kernel input | Every rule/node/edge, function descriptor, `TypeDesc` constructor, metadata field inspected, string scalar and byte hashed. |
| Closure/store | Every constructor/child edge, scope/dangling check, free-rec set visit, export, and constraint endpoint. |
| Ordering | Every comparison/radix byte and moved item used to canonicalize constraints, labels, roots, predicates, and proof parents. |
| Unification | Every pending equation, prune hop, decomposition edge, orientation, type/row binding, residual creation, and representative update. |
| Rows | Every prefix label, lacks fact/merge, flattened tail hop, field/tail occurs visit, and level-lowering visit through fields and reachable rows. |
| Zonk/reification | Every arena node/edge/field/root, row memo hit/install, public-variable allocation, binder enter/exit, and canonical byte emitted. |
| Schemes | Every free-variable/row visit, quantifier/predicate, poly arena node/edge, rigid lookup/insert, binder operation, and instantiated constraint. |
| Proof | Every proof node/edge/origin retained or traversed; bulk charges may compress ranges but must preserve deterministic causal reachability. |
| Identity | Every canonical semantic byte hashed. Private store revision is not input. |
| Layout | Every storage node/field, alignment/padding/tag calculation, arithmetic check, target/toolchain provenance byte. |
| Projection | Every semantic node, symbol lookup, Unicode scalar, normalized/emitted byte, escape, manifest byte, and digest byte. External compiler execution is recorded evidence, not hidden inside the kernel budget. |

`charge(units, operation, parents, origins)` uses checked arithmetic and fails
before starting work it cannot cover. Both 20,000-depth tests are mandatory:
finite-small budget returns typed exhaustion; sufficient budget succeeds and
drops on 512 KiB. Operation counts for a depth-128 descriptor and a million-byte
name must be strictly greater than their depth-1/one-byte counterparts.

## Explicit V4 exclusions

V4 closes only the quarantined carrier gates 1–7. It explicitly excludes:

1. any production edit, feature flag, alias, shim, dual runtime path, or write to
   incumbent `GrammarIR` type sidecars;
2. deletion or replacement of production `project_types`—that is gate 8's later
   atomic cut;
3. production corpus/runtime adoption, Rust/TS/VM shared execution, performance
   freeze, and adoption audits—those are gate 9;
4. a claim that current mutable `GrammarIR` is BB-B.W1's immutable `Program`;
   V4 uses a validated read-only snapshot boundary until Program exists;
5. subtyping, effect rows, or a second shape algebra absent a named consumer that
   equality plus record/variant rows cannot express;
6. a general Rust ABI contract, or layout authority for `String`, `Vec`, Rust
   `Option`, functions, arbitrary recursion, or opaque containers;
7. recursive target code generation without a separately explicit, tested
   carrier;
8. `unsafe`, source-unit tests, ignored tests, compatibility code, and unchecked
   “unlimited” traversal at public arbitrary-input boundaries;
9. benchmark or convergence claims based on green tests while any born-RED
   semantic row above remains open.

Production gates 8 and 9 remain separate even after V4 gates 1–7 pass. They may
be attempted only after two fresh hostile V4 reviews and a fresh adjudication.

## Commands and exact outcomes

All repository commands ran from
`/Users/mkbabb/Programming/bbnf-lang`.

### Checked-in V3

```sh
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
```

Outcomes: format exit 0; clippy exit 0 with warnings denied and only the three
workspace unused-patch notices; both test runs exit 0 with `32 passed; 0 failed;
0 ignored`; source unit tests and doc tests each report zero. The external real
Rust/TypeScript consumer passed inside both runs.

### Disposable adjudication probe

```sh
cargo fmt --manifest-path /tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/Cargo.toml --check
cargo clippy --manifest-path /tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path /tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/Cargo.toml \
  --test adjudication -- --nocapture --test-threads=1
cargo build --manifest-path /tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/Cargo.toml \
  --bin deep_type_desc
/tmp/bbnf-type-kernel-v3-adjudication.UCKy5h/probe/target/debug/deep_type_desc
```

Outcomes: format exit 0; clippy exit 0 with warnings denied; grouped probe exit 0
with `8 passed; 0 failed`; deep binary build exit 0; deep binary exit 134 with
fatal stack overflow. The grouped probe itself invoked:

```sh
rustc --edition=2024 -D warnings --crate-type=lib named.rs -o libnamed.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom named.ts
npx --yes tsc --strict --noEmit --lib es2022,dom maybe_fn.ts
```

Outcomes: Rust repeated-Named exit 1/E0425; TypeScript repeated-Named exit
2/TS2552; TypeScript option-of-function exit 2/TS2322. The TypeScript diagnostics
were on stdout with empty stderr.

### Structure, connectivity, and environment

Repository searches found no source test/ignore attribute under V3 `src` and no
V3 production reference under `crates` or root `Cargo.toml`. `cargo metadata
--no-deps` reports `bbnf-type-kernel-v3` with one local dependency,
`bbnf-ir=/Users/mkbabb/Programming/bbnf-lang/crates/ir`.

The live environment independently matches `EVIDENCE.md`:

```text
arm64
macOS 26.4.1 (25E253)
rustc 1.96.0-nightly (02c7f9bec 2026-04-10)
host aarch64-apple-darwin; LLVM 22.1.2
cargo 1.96.0-nightly (eb94155a9 2026-04-09)
Node v26.0.0
TypeScript 5.9.3
```

## Final disposition

Freeze V3 exactly as falsified evidence plus this adjudication. Do not add a
compatibility route and do not let production consume it. Start V4 with the
closure capability boundary, flat arenas, total operation-bearing API, and the
born-RED witnesses above. Retain the proved organs, not V3's public identity.

Only after V4 closes gates 1–7 under two new hostile reviews and a new
adjudicator may gate 8 be designed as the predecessor's one atomic production
transposition. Gate 9 remains a separate consumer/performance/audit closure.
