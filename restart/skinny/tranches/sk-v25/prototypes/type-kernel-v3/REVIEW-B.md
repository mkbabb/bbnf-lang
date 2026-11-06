# Type-kernel v3 independent review B

Status: **REJECT production adoption; retain as useful but falsified proof-carrier evidence**.

Review date: **2026-07-18**. I reviewed `README.md`, `EVIDENCE.md`, the v2
`ADJUDICATION.md`, every v3 source file, every v3 external test, and only the
live `bbnf-ir` node/descriptor/type-generation files needed for comparison. I
did not read the other v3 review. Disposable counterexamples live under the
fresh directory `/tmp/bbnf-type-kernel-v3-review-b.h24sD6`; no probe was added
to the repository.

## Verdict

The checked-in claims are not fabricated: the documented format, clippy, and
two test runs are clean; all 32 tests are external; the row solver repairs the
three decisive v2 row defects on the exercised record systems; explicit
recursive identities repair v2's active-stack construction model; the narrow
Apple-arm64 carrier layouts match native probes; and primitive-only hostile
Rust/TypeScript artifacts compile reproducibly.

Those facts do not close the predecessor gates. Small public-API witnesses
contradict recursive closure, total bounded inference, row-level
generalization, real production semantic agreement, target-safe projection,
and content-stable solution identity. The carrier also accepts one `IrNode`
tree whose referenced rules are already supplied as production `TypeDesc`
values; it does not infer a grammar, own stable production `NodeId`/`RuleId`
results, or produce a lowering plan. Production remains wholly disconnected,
as the manifest correctly admits.

Using the predecessor's binary closure standard, only gate 6 closes in its
explicitly narrow carrier scope. Gate 2 has substantial positive evidence but
is not established in full because the required variant and monotonicity laws
are absent. Gates 1, 3, 4, 5, and 7 are contradicted. Gates 8 and 9 remain RED
exactly as the manifest says.

Classification below is deliberate:

- **SUPPORTED** means I reproduced the stated behavior at its named scope.
- **CONTRADICTED** means an ordinary counterexample or direct live-source
  comparison disproves the claim.
- **NOT ESTABLISHED** means the checked evidence is narrower than the gate and I
  found no deciding counterexample for the remaining portion.

## Load-bearing findings

### B1 — `finish` does not seal public recursive handles

**CONTRADICTED; gate 1 remains RED.** The explicit-identity model correctly
supports nested outer use, rejects a vacuous inner binder, composes prebuilt
references, and has no mutable binder state for panic recovery. I reproduced
all four checked tests.

The stronger closure claim is false. `StoreBuilder::finish` checks only the IDs
passed in `roots` (`store.rs:483-501`) and then retains every builder node in the
public `TypeStore` (`:502-507`). `TypeStore::fingerprint` accepts any
scope-valid retained `TypeId`, not only a root proven closed (`:206-220`). A
caller can therefore retain an unclosed ID, finish the store using an unrelated
closed root, and use the unclosed ID afterward:

```text
escaped-rec-fingerprint=free-rec-0
```

The witness uses only `fresh_rec_var`, `rec_var`, `primitive`,
`finish([closed])`, and `fingerprint(free)`. It does not forge an ID. The API
comment says `finish` proves every exported root is closed, but a copied public
`TypeId` is itself an export path. The README's stronger statement that finish
rejects every remaining free identity is therefore contradicted.

### B2 — a real deep `TypeDesc` bypasses the total budget and aborts

**CONTRADICTED; gate 4 remains RED.** The checked iterative solve witness is
real: 20,000 nested kernel `Option` nodes on a 512-KiB thread return
`Exhausted { limit: 256 }`, and zero-budget ingestion plus the checked proof
slices behave as advertised.

Inference's real host bridge is not total. `infer` charges enter and emit
(`infer.rs:155-203`), but `type_desc` receives only `&OperationContext` and
recurses through `Option`, `Vec`, `Tuple`, and heterogeneous joins without a
step (`:467-521`). On a 512-KiB thread, `IrNode::Ref(0)` with a 20,000-deep
`TypeDesc::Option` descriptor and `OperationContext::new(256)` aborts the
process:

```text
deep-type-desc-exit=134

thread '<unknown>' (...) has overflowed its stack
fatal runtime error: stack overflow, aborting
```

This is an actual live boundary type, not a fabricated kernel node. Scheme
generalization/instantiation is also recursive and takes no operation context
(`scheme.rs:90-202`, `:346-441`, `:479-568`), so bounded scheme work is not
established even apart from the reproduced host-boundary abort.

### B3 — row binding fails to lower levels reachable through fields

**CONTRADICTED; gate 3 remains RED.** Lacks predicates, fresh quantified
instantiations, reusable rigid contexts, direct level cutoff, the named value
restriction, and recursive scheme round-tripping all pass their checked tests.

The level invariant fails when an old row captures a young field type. Type-meta
binding explicitly calls `lower_levels` before installing a substitution
(`solve/unify.rs:225-244`). Row binding checks lacks, propagates facts to a tail,
and lowers only the tail row level (`solve/row.rs:252-299`); it never lowers
metas or rows reachable through `expr.fields`. Generalization then quantifies
every free type whose level remains greater than the environment level
(`scheme.rs:304-343`).

The disposable witness creates an environment row at level 0, unifies it with
`{ x: a }` where `a` is level 2, solves the environment record, and generalizes
at environment level 0. Result:

```text
captured-young-type-quantifiers=1
```

That count must be zero: the level-0 environment row made the captured field
monomorphic. As written, separate instantiations can freshen a variable that
should remain shared with the environment.

### B4 — exhaustive enum coverage is real, production semantic agreement is not

**CONTRADICTED as a gate-5 agreement claim.** `infer.rs` exhaustively matches all
14 current `IrNode` variants, all 6 `FnDescriptor` arms, and all 18 current
`TypeDesc` shapes without wildcards. Adding a live enum variant will fail this
module's compilation. Every visited node receives a meta, a path fact, and one
equality constraint. Those are supported vocabulary/coverage facts.

They are not result agreement with the live projector:

- V3 maps an optional literal to `Option<Span>` and a repeated literal to
  `List<Span>` (`infer.rs:380-386`). The live `OptionalConstraint` and
  `RepeatConstraint` both collapse a `Span` element to `Span`
  (`crates/ir/src/passes/types/constraint/operators.rs:49-55` and `:98-104`).
  Reproduced V3 outputs:

  ```text
  v3-optional-span=Option(Primitive(Span))
  v3-many-span=List(Primitive(Span))
  ```

- V3 maps `FnDescriptor::EnumWrap { variant: "Variant" }` to
  `Named("bbnf::Enum::Variant")` (`infer.rs:434-440`). Production maps the same
  descriptor to `TypeDesc::Enum` (`generate.rs:360-377`). Reproduced V3 output:

  ```text
  v3-enum-wrap=Named("bbnf::Enum::Variant")
  ```

- V3 maps `Ref` directly from the host-supplied target descriptor
  (`infer.rs:388-397`). A target `Vec<U8>` therefore becomes `List<U8>`.
  Production's `RefConstraint` propagates only scalars and maps compound
  targets to `BoxedEnum` with a named obligation
  (`constraint/reference.rs:89-130`). Reproduced V3 output:

  ```text
  v3-compound-ref=List(Primitive(U8))
  ```

The semantic mismatch is also architectural. The public entry point accepts one
`&IrNode` plus a `HostEnvironment` whose referenced rules are already
`TypeDesc`s (`infer.rs:68-89`, `:140-225`). It cannot infer mutually recursive
rules, preserve production vec context, rule transparency, pretty-span policy,
or `collapse_simple_spans`. `NodeTyping` owns synthesized string paths, not the
live DAG's stable `NodeId`; `TypeSolution` maps ephemeral `TypeId` roots, not
rules/nodes (`solve.rs:137-171`). The all-variant test proves traversal drift
protection, not a production-equivalent grammar solution.

The deliberate `MissingHostSignature` result for an unannotated `Expr` is a
supported clean-break policy and is preferable to production's current Span
fallback. It does not repair the unrelated Repeat, Ref, and EnumWrap
disagreements.

### B5 — repeated named types generate invalid Rust and TypeScript

**CONTRADICTED; gate 7 remains RED.** Keyword handling, primitive records,
punctuation/Unicode field policy, normalized field collisions, and Rust
singleton tuple syntax compile under the recorded real toolchains. Two calls
with the same `SolvedType`, requested name, toolchain text, and reproduction
command are byte-identical, and their recorded manifest fields are populated.

The emitter is not target-safe over its own public `SolvedType` vocabulary.
Every `SolvedType::Named(name)` calls the collision allocator afresh
(`project.rs:320-364`, `:422-466`). The allocator counts uses of the normalized
target spelling but does not intern source semantic names (`:180-200`). Thus a
tuple containing the same named type twice becomes two different target types:

```rust
pub struct External;
pub type Payload = (External, External_2);
```

```ts
type External = number;
export type Payload = readonly [External, External_2];
```

Real compiler results:

```text
rustc-exit=1
error[E0425]: cannot find type `External_2` in this scope

tsc-exit=2
error TS2552: Cannot find name 'External_2'. Did you mean 'External'?
```

This is not merely a missing user import: the witness defines the one semantic
external type. The generator invents a second name for its second occurrence.
The production bridge routinely produces `Named` values for custom return
types and V3 also produces named Enum/Box carriers, so this is a live boundary,
not a dormant constructor.

The external hostile suite never projects a `Named`, variant, function, empty
tuple value, nested record/variant, or quoted/control-character label. Its root
is one primitive record with a singleton tuple
(`tests/type_kernel/project.rs:16-47`). The allocator unit test calls the empty-name path but does not compile
that output (`:49-82`). Consequently the predecessor's full hostile matrix was
not established even before the named-type counterexample.

### B6 — `TypeSolution::identity` is revision identity, not semantic identity

**CONTRADICTED for cross-build solution determinism.** Repeated solves of the
same store and constraint set are deterministic; the checked ordinary and row
permutations support that fact. Artifacts are also byte-identical for identical
fully supplied inputs.

Independently constructed isomorphic stores are not content-stable. Store scope
comes from the process-global `NEXT_SCOPE` counter (`store.rs:9`, `:354-365`),
and `stable_identity` hashes that revision into the material
(`solve.rs:377-390`, `:406-417`). Two separately built, solved `I64` roots in
one clean process produced:

```text
identity-1=fnv1a64:e940afd009341b30
identity-2=fnv1a64:d06ce805e4ebf5f6
identities-equal=false
```

Revision is appropriate for cross-store handle validation, but it must be
separate from a semantic solution digest. The current identity cannot serve as
the predecessor's stable solution identity or a content-addressed cache key.

## Supported and unestablished areas

### Rows and zonking

**SUPPORTED for the exercised record systems; NOT ESTABLISHED for all of gate
2.** I reproduced field-recursive row occurs, ordinary type occurs, open/open
residual joining, lacks conflict attribution, sibling zonking, repeated solve
identity within one store, six ordinary equality orders, forward/reverse row
orders, and 1,024 distinct lexicographic permutations of the seven-constraint
row system. Public row IDs are allocated during final reification and internal
solver residual indices do not appear in `SolvedType`.

The 1,024 loop really advances to a distinct next permutation each time
(`tests/type_kernel/solve/row.rs:149-167`), unlike v2's repeated 24-order loop.
The checked algebra test covers identity, commutativity, associativity, and
idempotence for records (`:170-219`). There is no variant-row test and no named
monotonicity property. The predecessor gate required closed/open/variant laws
and monotonicity where defined. I found no additional ordinary row equation
that falsified the exercised solver, but the missing properties prevent a full
GREEN ruling.

### Apple arm64 layout

**SUPPORTED only at the explicitly stated carrier scope; gate 6 GREEN at that
scope.** On the actual `aarch64-apple-darwin` host, the tests reproduce size,
alignment, offsets, padding, an explicit one-byte option tag with 16/8 total
layout for `u64`, and the 8/8 null-pointer niche. `String`, `Vec<u8>`, and
`Option<String>` measure 24/8 and remain labeled observations;
`OpaqueRustContainer("String")` is rejected. The model records total size,
alignment, field layouts, padding, tag representation, target, representation,
and modeled/measured claim (`layout.rs:97-180`, `:255-433`).

This is not a lowering plan: `StorageType` is a separate manually supplied
vocabulary and no function maps `SolvedType` or a real production carrier into
it. That limitation is accurately stated, so it does not contradict narrow
gate 6; it does keep gates 8 and 9 RED.

### Proof slices

**SUPPORTED for the checked failures.** Zero-budget ingestion returns a typed
error with a named proof node. The mismatch witness contains both relevant
origins and excludes the unrelated tautology. Type-occurs, row-occurs, and lacks
failures retain non-empty attributable slices. `OperationContext::reachable`
walks only parent edges (`type_kernel.rs:184-254`), rather than cloning every
input origin as v2 did. Totality is still contradicted by B2.

## Structure, public API, and external-test isomorphism

The Goldilocks topology is materially better than v2. Excluding `src/lib.rs`,
the ten authority files have ten path-isomorphic external test files. There are
zero source-unit tests and zero ignored tests. The largest source files are
`project.rs` (709 lines), `scheme.rs` (593), `store.rs` (578), and `infer.rs`
(554); row, unification, and zonking are separate 371/351/213-line invariant
owners. I do not find a v2-style solver god-module.

The public surface is not yet Goldilocks. A textual audit finds 183 written
public declaration/accessor lines across `src` (the scoped-ID macro expands its
one written public struct declaration into four handle types). The surface
exports proof-graph internals, builder internals, the identifier allocator and
style, native measurement helpers, separate phase-specific error wrappers, and
opaque `RigidType`/`RigidRow` types. Some exposure is necessary for honest
external tests, but production consumers should not inherit this proof-harness
surface. A sealed kernel should center on a small set of closed handles,
`infer/solve`, the immutable semantic solution, target lowering, and typed
errors; test-only allocator and measurement hooks need not be production API.

Structural isomorphism did not produce behavioral completeness. The exact
misses correspond to the counterexamples:

- the store tests never retain a non-root free handle across `finish`;
- the inference vocabulary test asserts only that each descriptor yields some
  solved root, not that every result agrees with production;
- the level test exercises a direct meta, not a young meta captured through an
  old row;
- the deep test starts after store construction and does not traverse a deep
  real host descriptor;
- the projection compiler consumer contains no named type.

## Predecessor gates 1–9

| Gate | Ruling | Independent result |
| --- | --- | --- |
| 1. Store/binder | **CONTRADICTED / RED** | The four named binder witnesses pass, but `finish([closed])` leaves a retained free-recursive `TypeId` publicly usable. Closure is not sealed. |
| 2. Rows/zonk | **NOT ESTABLISHED in full / AMBER** | The v2 row-occurs, residual, and sibling-zonk failures are repaired on records; distinct permutations and four algebraic laws pass. Variant and monotonicity laws are absent. |
| 3. Schemes | **CONTRADICTED / RED** | Lacks/freshness/rigids/value restriction pass, but binding a level-0 row to a level-2 field leaves that field generalizable. |
| 4. Budget/proof | **CONTRADICTED / RED** | Checked preprocessing and solve exhaustion/proofs pass; a 20,000-deep live `TypeDesc` aborts with stack overflow under limit 256. Scheme work is uncharged. |
| 5. Real IR/host boundary | **CONTRADICTED / RED** | Exhaustive vocabulary matching is supported. Repeat-of-Span, EnumWrap, and compound Ref results disagree with production, and the carrier cannot infer a whole grammar or publish stable rule/node results. |
| 6. Apple arm64 physical layout | **SUPPORTED at explicit carrier scope / GREEN** | Native `repr(C)` sizes, alignment, offsets, padding, explicit tag, and pointer niche match on the recorded host; opaque Rust containers reject. No semantic-to-layout connection is claimed or proved. |
| 7. Rust/TypeScript projection | **CONTRADICTED / RED** | The primitive hostile fixture and same-input artifact determinism pass. Reusing one `Named("External")` twice generates unresolved `External_2` in both targets. The full predecessor hostile matrix is absent. |
| 8. Atomic production cut | **RED as documented** | No production reference to V3 exists; old sidecars, ignored solver errors, Span masking, and `project_types` remain. |
| 9. Consumers/performance/audits | **RED as documented** | No corpus consumer, shared lowering plan, runtime value execution, benchmark, production audit, or second clean adoption audit exists. |

## Production connectivity

Repository search outside the prototype finds no
`bbnf-type-kernel-v3`, `type-kernel-v3`, or production `type_kernel` reference.
`cargo metadata --no-deps` confirms the dependency arrow is only V3 to the real
`/Users/mkbabb/Programming/bbnf-lang/crates/ir` package.

Production still exposes `pub fn project_types(ir: &mut GrammarIR)` returning
no error (`crates/ir/src/passes/types/mod.rs:51`), ignores both CSP propagation
results (`:67`, `:127`), and uses `unwrap_or(TypeDesc::Span)` during extraction
(`:184-232`). `GrammarIR` still owns `types`, `type_map`, `payload_layouts`,
`struct_registry`, and `type_obligations`
(`crates/ir/src/types/grammar.rs:40-74`, `:257-275`, `:344-362`, `:396-428`).
The README/EVIDENCE statements that gates 8 and 9 are wholly RED are therefore
accurate.

## Commands and exact results

All repository commands ran from
`/Users/mkbabb/Programming/bbnf-lang`. `CARGO_TARGET_DIR` was the disposable
review directory for the documented crate gates. Cargo created the prototype's
intentionally untracked `Cargo.lock` while resolving; I removed that generated
file after the runs. The repository root `Cargo.lock` remained byte-identical at
SHA-256
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.

```sh
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v3-review-b.h24sD6/target \
  cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --check
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v3-review-b.h24sD6/target \
  cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --all-targets -- -D warnings
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v3-review-b.h24sD6/target \
  cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v3-review-b.h24sD6/target \
  cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
```

Results: format exit 0; clippy exit 0 with warnings denied and only the three
documented unused workspace-patch warnings; both test runs exit 0 with
`32 passed; 0 failed; 0 ignored`; doc tests report 0 tests. The real generated
value consumer ran in both test executions.

The disposable probe itself is clean under formatting and clippy:

```sh
cargo fmt --manifest-path /tmp/bbnf-type-kernel-v3-review-b.h24sD6/probe/Cargo.toml --check
cargo clippy --manifest-path /tmp/bbnf-type-kernel-v3-review-b.h24sD6/probe/Cargo.toml --all-targets -- -D warnings
cargo run --quiet --manifest-path /tmp/bbnf-type-kernel-v3-review-b.h24sD6/probe/Cargo.toml \
  --bin bbnf-type-kernel-v3-review-b-probe
```

Results: all exit 0. Salient output is reproduced in B1, B3–B6.

Named-type target consumers:

```sh
rustc --edition=2024 -D warnings --crate-type=lib named.rs -o libnamed.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom named.ts
```

Results: Rust exit 1/E0425 and TypeScript exit 2/TS2552, both at invented
`External_2`.

Deep live-descriptor process:

```sh
/tmp/bbnf-type-kernel-v3-review-b.h24sD6/probe/target/debug/deep_type_desc
```

Result: exit 134 with fatal stack overflow on the explicit 512-KiB thread.

Environment independently matches the manifest: arm64 macOS 26.4.1 (25E253),
rustc 1.96.0-nightly `02c7f9bec` with host `aarch64-apple-darwin`, Cargo
1.96.0-nightly, Node 26.0.0, and TypeScript 5.9.3.

## Smallest clean-break next step

Do not transpose V3 into production and do not add a compatibility path. Freeze
this directory as evidence and build one new quarantined **V4 closure carrier**
with the same useful module topology. Its smallest coherent scope is the six
counterexamples above, not gate 8:

1. Separate builder-internal IDs from opaque closed root handles. Public
   fingerprint/solve/scheme APIs must accept only handles certified closed by
   `finish`; retaining an internal free-recursive ID must not create a public
   route into `TypeStore`.
2. When binding a row, lower levels through every field type and its reachable
   rows before installing the substitution. Add the old-row/young-field scheme
   witness as an external test.
3. Make the entire live `TypeDesc` bridge iterative and charge each descriptor;
   make scheme traversal iterative and share the same operation context. The
   20,000-deep host witness must return typed exhaustion on 512 KiB.
4. Define one explicit semantic agreement table for Repeat, Ref, Seq,
   EnumWrap, vec context, transparency, and span policy. Exercise the real
   production constraint projector as the oracle until the atomic cut; do not
   label mere enum construction as agreement.
5. Split symbol interning from collision allocation in both emitters: repeated
   semantic names reuse one target name, while distinct source names that
   normalize together receive suffixes. Compile the full `SolvedType` matrix,
   including Named, nested records/variants, functions, empty/singleton tuples,
   and hostile quoted/control labels.
6. Keep store revision solely for handle validation. Hash a canonical reified
   solution without process scope for semantic identity. Contract the public
   surface before it becomes production compatibility debt.

Only after those witnesses and the missing variant/monotonic row laws pass
under two fresh hostile reviews should gate 8 be attempted as the predecessor's
single atomic deletion/transposition. Gates 8 and 9 must not be broadened into
this repair carrier.
