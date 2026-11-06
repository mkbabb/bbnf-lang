# Type-kernel V4 — Gestalt 1 hostile critic B

Status: **DIRTY / INCOMPLETE_ASSAY / no acceptance**  
Clean-pass count after this review: **0**  
Production authority: **none**  
Portfolio-candidate credit: **none**

## Independent scope and standard

This was a fresh hostile review of
`restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4`. I assumed the
tranche, wave choice, feature design, implementation, tests, evidence, KISS fit,
and product fit were wrong. Author receipts were treated only as commands to
replay.

I read completely:

- `LOCK-CONSTITUTION.md`;
- `TRANCHE-PORTFOLIO.md`;
- `waves/BB-VALUE-TAPE-P0.md`;
- the V3 `ADJUDICATION.md`, including the bound V4 public contract, row-law
  matrix, semantic-agreement matrix, target-compilation matrix, total-accounting
  contract, exclusions, and exact disposition;
- every V4 source file, external test file, package file, README, evidence
  receipt, handoff, and source ledger.

The other V4 critic report was not used as evidence or authority. No production,
prototype implementation, test, governing specification, lockfile, or author
receipt was edited. Fresh probes lived only under
`/tmp/bbnf-type-kernel-v4-critic-b-probe`; build products used external temporary
targets. This report is the only durable write from this review.

Ruling terms:

- **CORRECT** means reproduced by a fresh command/probe or compelled by source.
- **UNPROVEN** means the implementation may be useful, but the required domain
  was not executed or established.
- **RED** is binary: one counterexample or one unproved required matrix cell keeps
  the gate open.

## Verdict

**Do not accept, repair toward production, transpose, or count V4 as a portfolio
candidate. Freeze it as a falsified horizontal assay, bank only the explicitly
surviving organs, and route their corrected forms into independently complete
value/tape candidates.**

The ordinary author gates are reproducibly green: formatting and strict clippy
pass, two serial replays each pass 22 external tests, the crate graph has no
dependency, source/test topology is mirrored, and the recorded source/lock hashes
match. Those facts are real but narrow.

Fresh public-API examples reproduce seven ordinary correctness failures:

1. a free recursive type reachable only through a `Lacks` row endpoint survives
   `close`, and `solve` publishes an unrelated export;
2. a `RootId` obtained from one solution is silently accepted against another
   solution and selects the same-numbered local root;
3. equivalent expansive schemes built in independent stores have different public
   digests;
4. reusing `RigidContext` with a second builder returns a foreign draft handle and
   later fails `CrossStore`;
5. a directly self-recursive grammar solves to `Named("SelfRule")`, not an
   explicit recursive solution;
6. different input/effect digests collapse to the same `SemanticDigest`;
7. a legal source label `gen` emits invalid Rust 2024, while exact `I64` projects
   to TypeScript `number` with no declared numeric policy.

The layout/lowering route is also non-evidence: changing semantic and physical
profiles leaves Rust output byte-identical, and caller-supplied fabricated
toolchain strings receive a `Measured` layout claim.

The governing portfolio has independently revoked horizontal type-kernel
selection: a candidate must independently implement every PT/BB lock, the full
value/tape API, JSON, exhaustive CSS L4, BBNF-only type derivation, backends,
packages, consumers, ARM, resources, profiles, and two hostile passes. V4 admits
that it implements none of that complete vertical. It is therefore not merely an
incomplete candidate; it is the wrong unit of competition.

## Numbered findings

### 1. [BLOCKER] The active portfolio disqualifies this horizontal tranche

`TRANCHE-PORTFOLIO.md` says the type-kernel V1–V3 sequence disproved selecting a
horizontal stage before a complete product vertical. `BB-VALUE-TAPE-P0` forbids
credit for narrow graph, type, JSON, tape, regex, or assembly carriers and admits
only independently complete VT candidates. V4's README and handoff truthfully
admit no BBNF-only exact product API, CSS L4, tape, values, backend, consumer,
profile, or portfolio implementation.

This means V4 cannot be repaired into tranche acceptance by closing its local
tests. Its only legitimate disposition is evidence-bank status. Any useful organ
must be independently re-earned inside each complete candidate; importing a
shared V4 solver would violate the non-sharing law.

**Disposition:** RETIRE as an active tranche, wave, selection candidate, or
production precursor. BANK only named counterexamples and proved implementation
organs.

### 2. [BLOCKER] `close` does not certify every constraint endpoint

`ProblemBuilder::close` adds equality endpoints to `closure_roots` and calls
`validate_closed_root` on them, but a `Constraint::Lacks` contributes no type
root. The later loop only calls `validate_row` on the row handle
(`store.rs:490-507`); it never walks the row's fields for free recursion.

Fresh public probe:

```text
lacks-only-escaping-recursion=published:true
```

The example constructs a free `DraftRecVar`, places its `DraftTypeId` in a closed
record row, adds a compatible `lacks(row, "other")`, exports an unrelated `Bool`,
then calls `close` and `solve`. Both succeed and the unrelated export publishes.
The defining law says all exports **and every constraint endpoint** are exactly
closed. The compile-fail crater only proves `solve(DraftTypeId)` is ill-typed; it
does not prove the content of `ClosedProblem` is closed.

**Disposition:** Gate 1 RED. Repair by one iterative closure walk over every type
and row endpoint, with lexical binder state and boundary provenance, before any
`ClosedProblem` exists.

### 3. [HIGH] Solved handles are slot numbers, not solution-owned capabilities

`RootId`, `SolvedTypeId`, and `SolvedRowId` contain only a private `u32` slot
(`solve.rs:15-23`). `TypeSolution::root_type`, `view`, and `row` index the receiving
solution without checking origin (`solve.rs:161-210`). `generalize_roots` therefore
accepts a foreign `RootId` whenever that slot exists locally.

Fresh result:

```text
foreign-root-accepted=true foreign-root-scheme-is-local=true
```

The root came from an `I64` solution, was passed to `generalize` with a `Bool`
solution, and silently generalized the local `Bool` at slot zero. The same alias
class exists for public solved type and row IDs. This defeats revision ownership,
foreign-solution rejection, and the claimed keyed immutable solution boundary.

**Disposition:** Gate 1 and ownership/provenance RED. Repair with a solution
capability/revision in every public handle or with borrow-scoped views that cannot
cross solutions; reject foreign handles as a typed error.

### 4. [HIGH] Scheme identity leaks store scope, and reusable rigids are builder-unsafe

Rigid poly variables carry `key = revision.rotate_left(...) ^ source`
(`scheme.rs:487-527`). The public scheme digest hashes `Debug` text containing that
key (`scheme.rs:247-271`). Two semantically equivalent expansive schemes therefore
have different digests:

```text
scheme-a=fnv1a64:29eb35cc241c7863
scheme-b=fnv1a64:05575603a6d5a1f5
equivalent-rigid-schemes-equal=false
```

That directly contradicts the handoff's claim that private source identity is
excluded from public canonical bytes and the ledger's exclusion of store scope.

`RigidContext` also stores raw `DraftTypeId`/`DraftRowId` values without recording
the builder scope (`scheme.rs:101-105`). Reusing the context with another builder
returns the first builder's handle; the later close fails:

```text
rigid-context-cross-builder=CrossStore
```

The external test named `expansive_rigids_are_reused` never instantiates the
expansive scheme; it only checks that its quantifier count is zero.

**Disposition:** Gate 3 RED. Separate canonical rigid identity from target-builder
memoization; scope or reset `RigidContext` per builder; exclude all validation
identity from scheme bytes/digests.

### 5. [BLOCKER] The inference layer is not whole-grammar inference and does not represent grammar recursion

For `RefMode::Scalar`, inference returns the referenced rule's declared host type
or a nominal `Named(rule.name)` (`infer.rs:433-448`). It never unifies the reference
with the referenced rule root. Host types must point only to prior flat-arena nodes
(`infer.rs:578-587`), so that manual host vocabulary cannot express a recursive
binder either.

Fresh result for a one-rule direct self-reference:

```text
self-recursive-root=Named("SelfRule")
```

The author `recursive_json` test checks only that its outer result is a `Variant`.
Its array and JSON references are nominal strings, so the test does not prove a
recursive JSON semantic type, recursive binder, or reference constraint. More
generally, there is no `rule_root(RuleId)` or `node_root(NodeId)` API required by
the V3 contract; string-form `ExportKey`s substitute for stable typed lookup.

**Disposition:** Gate 5 RED. RETIRE the local inference claim. A future complete
candidate must infer from canonical BBNF module/program values, allocate rule
metas first, equate every reference with its rule root under the specified context,
and close legal SCCs with explicit recursion.

### 6. [BLOCKER] Required semantic policies and effects are decorative, and semantic identity omits input semantics

A source scan finds `RuleSpec.transparent` and `Repeat.one_or_more` only at their
declarations. `HostEffect` appears only in its declaration/type; the inference
match explicitly discards `effect`. They affect the snapshot's debug bytes but no
constraint, solved type, effect/capability row, lowering decision, or projection.
The implementation also lacks ownership and backend-availability declarations.

The local `TokenDispatch` becomes a token-labelled variant, while the bound V4
agreement table requires a result join over mapped continuations plus fallback and
states that the token does not determine the result. Compound reference and
heterogeneous-alternative obligations are not represented in a lowering plan.

Fresh result from otherwise equal host-return grammars with different source names
and `Pure` versus `Stateful` effects:

```text
input-digests-equal=false
semantic-digests-equal=true
```

`solve` hashes only export keys and root bytes (`solve.rs:376-390`), despite the
source ledger saying immutable input semantic bytes are included in semantic
identity. `InputDigest` is merely stored beside the solution.

**Disposition:** Gate 5 and determinism/provenance RED. RETIRE the shadow
`GrammarSpec`/`HostTypeNode` authority; derive the required facts from BBNF, model
effects/ownership/backend availability in the solved contract, and include the
canonical input identity in the keyed solution identity.

### 7. [BLOCKER] The total operation budget is post-hoc accounting, not a resource bound

The bound contract requires charging before work that cannot fit and requires
every comparison, moved item, node/edge, canonical byte, proof edge, and emitted
byte to be accounted. Several large operations happen before their charge:

- snapshot sorts rules/nodes before any ordering charge and constructs the full
  debug-format canonical input before charging its bytes (`infer.rs:177-178,
  707-724`);
- `solve` clones all nodes/rows in `State::new`, clones and sorts constraints, and
  builds complete digest material before the associated charges
  (`solve.rs:331-390`);
- zonk appends the complete canonical byte vector before the one bulk charge
  (`solve/zonk.rs:347-448`);
- scheme generalization formats the complete debug digest string before charging
  it (`scheme.rs:247-260`);
- projection emits the complete source and manifest before their bulk charges
  (`project.rs:174-219`);
- exhaustion constructs and clones the reachable proof slice without charging it
  (`type_kernel.rs:299-335`).

Large public values also derive `Clone`, making unbounded arena/source/proof copies
without an `OperationContext`. Iterator inputs such as `close`, `tuple`, and
`function` are collected before proportional capacity/work can be refused.

The 20,000-depth tests prove iterative stack behavior for selected paths. They do
not prove bounded memory, fail-before-work behavior, ordering charges, clone
behavior, or total accounting.

**Disposition:** Gate 4 RED. Replace post-hoc bulk ticks with checked reservations
and incremental charges before allocation/comparison/emission; remove or bound
public deep `Clone`; charge proof construction even on failure.

### 8. [HIGH] The required row-law and diagnostic matrix is largely unexecuted

The shared record/variant implementation and ordered maps are useful, and the
checked subset passes. But the bound matrix is binary and the external suite does
not execute all required cells. Missing or materially incomplete cells include:

- closed reflexivity;
- open/open equal-prefix canonical tail union;
- lacks identity, idempotence, and compatible union;
- row occurs through prior substitutions as an independent case;
- type occurs through row-tail substitution;
- equality endpoint symmetry and the two transitive bases;
- empty extension identity and three-set extension associativity;
- monotonic compatible and conflicting constraint addition;
- repeated zonk over the same solution;
- canonical proof-origin equality under endpoint/order permutations;
- hash-seed/root-order dimensions.

The 1,024 permutations reorder seven already-built constraints; the solver then
sorts them. That is useful constraint-insertion evidence, not the full independent
construction/root/hash matrix. Row construction origins are not retained in row
nodes, so a later row error cannot cite the field boundary unless another
constraint happens to carry it; the checked lacks test asserts predicate and
equation origins but deliberately does not assert the `field` origin it supplied.

**Disposition:** Gate 2 RED/UNPROVEN. BANK the symmetric record/variant core and
current counterexamples. Do not label the current test `matrix` as the bound
matrix.

### 9. [HIGH] Rust projection rejects a required legal hostile label, and target semantics are not exact/isomorphic

The Rust keyword table omits Rust 2024's reserved `gen`. A record field labelled
`gen` is emitted verbatim. Fresh real compiler result:

```text
rust-2024-gen-exit=1
rust-2024-gen-first-diagnostic=error: expected identifier, found reserved keyword `gen`
```

The target matrix requires hostile keywords to compile. The author crater checks
`type` and slash normalization, not the current edition's full reserved set.

The projection also maps semantic `I64` and `U64` to TypeScript `number` with no
numeric-width policy in `TargetProfile`:

```text
typescript-i64=export type Value = number;
```

That cannot represent every 64-bit integer exactly. Other primitive mappings are
not isomorphic either: Rust `ParseResult` is `Result<(), String>` while TypeScript
is only `{ readonly ok: boolean }`; Rust `BoxedEnum` and the TypeScript object do
not share an explicit wire/result contract. The full target matrix—every
primitive/value, all function positions, all hostile labels, rejected open forms,
and exact parity—is not present.

**Disposition:** Gate 7 RED. Use edition-complete/raw-identifier policy, a declared
and checked numeric policy, and shared semantic fixtures proving Rust/TS values and
boundaries rather than syntax alone.

### 10. [HIGH] `LoweringPlan` and physical measurement are ornamental

`lower` charges a walk, clones the entire `TypeSolution`, stores `TargetProfile`,
and makes no layout/capability decision (`layout.rs:128-153`). `project` never
reads `plan.profile()`. Fresh result:

```text
different-profiles-same-rust=true
```

`measure_carrier` does not measure the active toolchain. It accepts caller strings,
returns hard-coded dimensions, and labels `NonNullPointerOption` as `Measured`.
Fresh result:

```text
fabricated-toolchain-claim=Measured rustc=critic-b-fabricated-rustc
```

Changing caller text changes the receipt hash, which proves only text binding—not
measurement truth. There is no semantic-to-carrier mapping, no exact compiler
invocation in returned evidence, and no plan-controlled projection.

**Disposition:** Gate 6 RED despite plausible Apple-arm64 constants. RETIRE the
`Measured` claim and inert profiles. Preserve constants only as modeled hypotheses;
future candidates must obtain toolchain-qualified measurements and make checked
plan decisions that consumers actually use.

### 11. [MEDIUM] The topology is mirrored, but the public and artifact surfaces are not KISS

Positive structural facts:

- ten named authority modules have ten path-isomorphic external test modules;
- zero source test attributes, ignored tests, or unsafe constructs were found;
- `cargo tree` contains only `bbnf-type-kernel-v4`;
- no production reference exists under `crates` or root `Cargo.toml`.

The implementation nevertheless has 5,328 source lines against 1,897 test lines,
153 externally public declarations/methods, and 17 additional public fields
(plus 12 `pub(crate)` declarations). `pub mod` plus facade re-exports expose
duplicate navigation paths. The local grammar/host vocabulary, inert profile
types, caller-asserted measurement API, proof internals, and projection policy
substantially exceed the small equality/row closure question while still failing
the product boundary.

The standalone graph is dependency-free, but every Cargo command emits eleven
unused root patch warnings and the private lockfile records those unused patches.
The graph is isolated; the artifact/receipt is not minimal or noise-free.

**Disposition:** Retain the source/test isomorphism and solver submodule cohesion.
In any successor, collapse duplicate exports, keep internal IDs/proof machinery
private, remove shadow input/layout authorities, and generate the standalone lock
under an isolated Cargo configuration.

### 12. [HIGH] Test names and evidence claims exceed their assertions

The suite is clean but not semantically sufficient:

| Author claim/test | What it actually proves |
| --- | --- |
| `recursive_json_is_one_assay_not_a_hardcoded_schema` | Only that the outer root view is `Variant`; references are nominal `Named` values. |
| `whole_grammar_vocabulary_is_total` | Every local enum arm compiles and exports exist; no expected rule/node semantic table or live production differential is checked. |
| CSS L4 shapes | A hand-authored `HostTypeNode::Variant` returned by one host expression can pass through; no CSS syntax, recovery, BBNF derivation, or L4 row is proved. |
| `expansive_rigids_are_reused` | Only `row_quantifiers() == 0`; the expansive scheme is never instantiated. |
| `projection_rejects_open_and_recursive_semantics` | Only an open meta is projected; no recursive value is built in that test. |
| layout provenance tests | Hard-coded return values and caller-text hash changes; no independent layout measurement. |
| row `matrix` | A useful subset listed in finding 8, not the bound matrix. |

The README and handoff mostly disclose the greater-product incompleteness, which is
good evidence hygiene. `EVIDENCE.md` still overstates recursive JSON, batch rigids,
row-matrix completeness, measurement, and total hostile accounting relative to
the executed assertions.

**Disposition:** Author receipt remains a receipt. No clean-pass credit.

## Gate matrix

### Constitutional four-plane review

| Plane | Result | Binding reason |
| --- | --- | --- |
| Tranche fit | **RED** | Horizontal carrier is expressly ineligible under the active complete VT portfolio. |
| Wave fidelity | **RED** | No complete VT candidate, BBNF-only types, JSON/CSS totality, value/tape, backend/package/consumer, profile, or two-pass closure. |
| Feature truth | **RED** | Closure, handle ownership, schemes, inference, budgets, projection, and layout have reproduced defects. |
| Greater-product effect | **RED** | No consumer or production edge; local grammar/host/layout authorities would duplicate the required BBNF/plan authorities if adopted. |

### V3-bound V4 gates 1–9

| Gate | Result | Fresh critic-B ruling |
| --- | --- | --- |
| 1. Store/binder/closure | **RED** | `Lacks` row endpoint can carry escaping recursion; foreign solved handles alias local slots. |
| 2. Rows/zonk | **RED / UNPROVEN** | Useful checked subset, but required law/monotonicity/proof/permutation matrix is incomplete. |
| 3. Schemes | **RED** | Store scope enters public digest; `RigidContext` is unsafe across builders; named expansive reuse is untested. |
| 4. Budget/proof | **RED** | Multiple whole allocations/sorts/formats/clones occur before charge; failure proof construction is uncharged. |
| 5. Real IR/host semantics | **RED** | Local shadow input, nominal recursion, ignored transparency/cardinality/effects, no live oracle or BBNF-only derivation. |
| 6. Apple-arm64 layout | **RED** | Constants are plausible modeled data, but `Measured` is caller-asserted and no semantic-to-layout plan exists. |
| 7. Rust/TypeScript projection | **RED** | Rust 2024 `gen` fails; numeric/result parity and full target matrix are absent. |
| 8. Atomic production cut | **RED / out of scope** | No production consumer or deletion of incumbent masking/sidecar routes. |
| 9. Consumers/performance/audits | **RED / out of scope** | No runtime/value/backend/package/corpus/performance adoption and no clean hostile sequence. |

### Author/structure gates

| Gate | Result |
| --- | --- |
| `cargo fmt --check` | **PASS**, exit 0 |
| strict `cargo clippy --all-targets -- -D warnings` | **PASS**, exit 0; eleven unused root-patch warnings outside the crate graph |
| first serial test replay | **PASS**, 22 passed, 0 failed, 0 ignored; 0 unit, 0 doc |
| second serial locked replay | **PASS**, same counts |
| real author Rust/TypeScript crater | **PASS** inside both test replays |
| dependency graph | **PASS narrow**, only this package, zero dependencies |
| source/test isomorphism | **PASS**, ten authority files and ten mirrored external files |
| source tests / ignored tests / unsafe constructs | **PASS**, zero |
| production connectivity | **DISCONNECTED**, zero references under `crates` and root manifest |
| source/test receipt | **MATCH**, `0df94e3316df5b262b4e24dfb96f7af828c67542169a3b41ba52bb77474408e1` |
| V4 `Cargo.lock` | **MATCH**, `6de0490fde327c06b8ebac5ae797cb3570aa33841fa80b8e8e12211debd355bc` |
| repository `Cargo.lock` | **MATCH**, `a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908` |

## Exact survivor, repair, retirement, and routing disposition

| Organ | Disposition | Exact route |
| --- | --- | --- |
| Consumed draft builder and distinct draft/closed public types | **SURVIVE AS BANKED DESIGN, REPAIR** | Extend certification to every type/row constraint endpoint; do not share implementation between VT candidates. |
| Explicit scoped `DraftRecVar`, non-vacuous `mu`, root-local binder reification | **SURVIVE AS BANKED ORGAN** | Re-earn with full closure, foreign-handle, nesting, panic/failure, and grammar-SCC fixtures in each candidate. |
| Flat draft/solved/poly arenas and iterative deep traversals/drop | **SURVIVE AS BANKED ORGAN** | Add pre-allocation budgets, memoized canonical traversal where needed, and remove unbounded public clones. |
| Iterative equality worklist and single record/variant row mechanism | **SURVIVE AS UNPROVEN CORE** | Execute the complete two-kind law, proof, monotonicity, construction, root, and seed matrix. |
| Ordered labels/exports and root-local canonical bytes | **SURVIVE AS PARTIAL EVIDENCE** | Key identity by canonical BBNF input and use collision-resistant/full canonical receipts; qualify all public handles. |
| Parent-reachable proof idea | **SURVIVE AS DESIGN** | Connect construction/boundary origins, charge proof nodes/edges and failure slicing, and prove deterministic minimality. |
| External source-isomorphic tests, real compiler craters, no unsafe/dependencies | **SURVIVE** | Keep as per-candidate non-shared gates and expand to the full matrices. |
| Current `ClosedProblem::close` implementation | **REPAIR BEFORE ANY REUSE** | Walk `Lacks` rows and all reachable field types for exact free recursion. |
| Current solved IDs and `RigidContext` | **RETIRE/REPLACE** | Solution/builder-qualified capabilities; no raw cross-owner slots. |
| Current scheme digest/key encoding | **RETIRE/REPLACE** | Canonical rigid tokens exclude store revision; builder memo identity stays private. |
| Local `GrammarSpec`/`GrammarNode`/`HostTypeNode` authority | **RETIRE** | BBNF modules and canonical program/HIR are the only source; live differential remains an oracle during transposition. |
| Nominal rule-reference inference and author recursive-JSON claim | **RETIRE** | Infer rule metas/recursion and compare exact node/rule results. |
| Post-hoc bulk operation accounting and deep `Clone` surfaces | **RETIRE/REPLACE** | Reserve/charge before work and expose bounded immutable views. |
| `SemanticDigest` over exports alone and FNV-64 as authority identity | **RETIRE/REPLACE** | Include canonical input semantics and retain full reproducible canonical bytes with an appropriate digest. |
| Manual primitive Rust/TS mappings without declared policy | **RETIRE/REPLACE** | Derive target types/wire policy from BBNF and target capabilities; prove isomorphic values. |
| Inert `TargetProfile`/`LoweringPlan` and caller-asserted `Measured` evidence | **RETIRE** | A real checked plan owns layout decisions; measurement comes from the named toolchain command and raw evidence. |
| V4 crate as an active tranche/candidate/shared library | **RETIRE** | Freeze as falsified evidence. Do not transpose or make it a shared organ for VT-1…VT-12. |
| Production gates 8–9 | **DEFER, REMAIN RED** | No production edit until a complete candidate wins the portfolio and survives the required audits. |

## Exact commands and results

All repository commands ran from
`/Users/mkbabb/Programming/bbnf-lang`. No in-tree target was used.

### Author gates

```sh
audit_target=$(mktemp -d /tmp/bbnf-type-kernel-v4-critic-b-target.XXXXXX)
shasum -a 256 \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.lock \
  Cargo.lock
CARGO_TARGET_DIR="$audit_target" cargo fmt \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml \
  --check
CARGO_TARGET_DIR="$audit_target" cargo clippy \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml \
  --all-targets -- -D warnings
CARGO_TARGET_DIR="$audit_target" cargo test \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml \
  -- --test-threads=1
CARGO_TARGET_DIR="$audit_target" cargo test --locked \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml \
  -- --test-threads=1
```

Results: target `/tmp/bbnf-type-kernel-v4-critic-b-target.8k93cr`; format exit 0;
clippy exit 0 with warnings denied; both test exits 0 with `22 passed; 0 failed;
0 ignored`, zero source-unit tests, and zero doc tests. Cargo printed eleven
unused-patch warnings. The external tests invoked real `rustc` and `tsc` craters.

### Metadata, structure, and hashes

```sh
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v4-critic-b-target.8k93cr \
  cargo metadata --locked \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml \
  --no-deps --format-version 1
CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v4-critic-b-target.8k93cr \
  cargo tree --locked \
  --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml
rg -n '\bunsafe\s*(fn|\{|impl|trait)\b|unsafe\s+extern' \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests
rg -n '#\[(test|cfg\(test\)|ignore)' \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src
rg -n '#\[ignore' \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests
rg -n 'type-kernel-v4|bbnf_type_kernel_v4' crates Cargo.toml
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src \
     restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests \
     -type f -print0 | sort -z | xargs -0 shasum -a 256 | shasum -a 256
```

Results: metadata `dependencies=[]`, private workspace root is V4, external target
is used; tree printed only `bbnf-type-kernel-v4 v0.0.0`; all three structural `rg`
searches were empty; source/test receipt matched the ledger. Counts were 11 source
files including `lib.rs`, 10 external test files, 5,328 source lines, 1,897 test
lines, 153 externally public declarations/methods, 17 additional public fields,
and 12 `pub(crate)` declarations.

### Fresh focused correctness probe

Probe crate:
`/tmp/bbnf-type-kernel-v4-critic-b-probe`.

```sh
probe_target=$(mktemp -d /tmp/bbnf-type-kernel-v4-critic-b-probe-target.XXXXXX)
CARGO_TARGET_DIR="$probe_target" cargo fmt \
  --manifest-path /tmp/bbnf-type-kernel-v4-critic-b-probe/Cargo.toml --check
CARGO_TARGET_DIR="$probe_target" cargo clippy \
  --manifest-path /tmp/bbnf-type-kernel-v4-critic-b-probe/Cargo.toml \
  --all-targets -- -D warnings
CARGO_TARGET_DIR="$probe_target" cargo run --quiet \
  --manifest-path /tmp/bbnf-type-kernel-v4-critic-b-probe/Cargo.toml
```

Results: target `/tmp/bbnf-type-kernel-v4-critic-b-probe-target.McYFJo`; format,
clippy, build, and run exit 0. Load-bearing output:

```text
lacks-only-escaping-recursion=published:true
foreign-root-accepted=true foreign-root-scheme-is-local=true
scheme-a=fnv1a64:29eb35cc241c7863
scheme-b=fnv1a64:05575603a6d5a1f5
equivalent-rigid-schemes-equal=false
rigid-context-cross-builder=CrossStore
input-digests-equal=false
semantic-digests-equal=true
self-recursive-root=Named("SelfRule")
different-profiles-same-rust=true
fabricated-toolchain-claim=Measured rustc=critic-b-fabricated-rustc
rust-2024-gen-exit=1
rust-2024-gen-first-diagnostic=error: expected identifier, found reserved keyword `gen`
typescript-i64=export type Value = number;
```

### Environment

```text
arm64
macOS 26.4.1 (25E253)
rustc 1.96.0-nightly (02c7f9bec 2026-04-10)
host aarch64-apple-darwin; LLVM 22.1.2
cargo 1.96.0-nightly (eb94155a9 2026-04-09)
Node v26.0.0
TypeScript 5.9.3
```

## Clean-count and terminal ruling

This review contains correct findings, so the implemented-wave acceptance law
resets the consecutive-clean count to **zero**. The assay stays
**INCOMPLETE_ASSAY**. No author receipt, local green suite, or narrow survivor
earns acceptance.

The exact terminal disposition for this pass is:

- **BANK** the consumed closure-capability idea, explicit recursion core, flat
  arenas, iterative worklists/traversals, symmetric row core, external mirrored
  tests, and real-compiler crater method;
- **REPAIR only inside complete non-sharing candidates** the closure endpoint
  walk, owner-qualified handles, schemes, budgets/proofs, full row laws, BBNF
  inference, target policies, and real layout evidence;
- **RETIRE** V4 as a tranche/candidate/shared authority, its shadow grammar/host
  model, nominal recursion claim, scope-bearing scheme digest, unqualified solved
  IDs, post-hoc budget, inert lowering profile, caller-asserted measurement, and
  manual unproved target mappings;
- **DO NOT TOUCH PRODUCTION** under gates 8–9.

## Report digest

Digest scope: every byte before the `## Report digest` heading. Reproduce with:

```sh
sed '/^## Report digest$/,$d' \
  restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/GESTALT-1-CRITIC-B.md \
  | shasum -a 256
```

SHA-256: `10cc23cbd7a02b72e16e7c2049b98921ce162e87bfd3a5764332a6074b404fc2`
