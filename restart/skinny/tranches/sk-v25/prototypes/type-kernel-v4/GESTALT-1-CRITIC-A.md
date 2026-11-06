# Type-kernel V4 hostile Gestalt 1 — critic A

Status: **RED. V4 remains a falsified `INCOMPLETE_ASSAY`; it earns no clean
count, production authority, portfolio rank, or successor dependency.**

Reviewer declaration: **gpt-5.6-sol, xhigh reasoning effort**. I assumed the
tranche choice, implementation, laws, API, tests, receipts, structure, and KISS
fit were wrong. This is an independent compiler-library review, not an author
receipt. I did not modify implementation, tests, manifests, locks, or production.
The disposable counterexample is outside the repository at
`/tmp/bbnf-type-kernel-v4-critic-a-probe`; this report is the only durable write.

## Verdict

V4's narrow status label is honest, but that does not make the assay clean. The
checked-in 22-test suite passes twice and the standalone locked crate really has
no dependency. Several useful mechanisms are present: draft and closed handle
types are distinct, the successful deep paths are flat, ordinary solution
digests are scope-free on the checked closed roots, substantial record/variant
row examples pass, Rust and TypeScript printers are separate, and source/test
topology is exact.

The defining closure claim is nevertheless false. A `Lacks` constraint is a
closed-problem endpoint, yet `ProblemBuilder::close` validates only its row slot,
not the types reachable through the row. A public witness puts a free recursive
type in that row, closes with no exports, and successfully publishes a
`TypeSolution`. Eight other public/source-compelled defects independently keep
the assay RED: scheme identity includes private store revision; reusable rigids
leak a draft handle across builders; solved IDs alias silently across solutions;
same-identity binder shadowing admits a vacuous outer recursive binder; local
"whole grammar" inference disagrees with its own inferred rule root; equivalent
contradictions have construction-order-dependent diagnostics; operation
accounting performs large clones/sorts/formatting/emission before charging; and
the physical layout API labels hard-coded constants as `Measured` based solely on
caller-supplied strings.

The current tranche constitution also supersedes the premise that a horizontal
type kernel can select the product architecture. V4 is 5,328 source lines plus
1,897 external-test lines, yet it consumes no BBNF module, parser result, value,
tape, JSON oracle, CSS L4 row, backend, or consumer. Its local `GrammarSpec`,
`HostTypeNode`, semantic switches, primitive table, target mappings, and carrier
table are handwritten authorities forbidden as inputs to a complete candidate.
Even a perfectly repaired V4 would remain only a banked assay organ, never one of
VT-1…VT-12 and never a portfolio prototype.

One correct finding resets the clean count. This review has multiple executable
correct findings. **Gestalt 1 critic A is RED; consecutive-clean count is zero.**

## Immutable reviewed ledger

Repository HEAD was
`af15f63e0d2d3d719938c13b906a50acbb92ea3b`. The reviewed V4 directory is
untracked at that commit, so the hashes below, not the commit alone, identify the
subject. Build output under `type-kernel-v4/target` was explicitly excluded as a
target artifact. Every other V4 file present before this report was read
completely.

### Binding law and predecessor record

| SHA-256 | File |
| --- | --- |
| `006066f64915f02020102776093c4a519e3466fe6c31a3bbcfcfd8fef4a611e5` | `LOCK-CONSTITUTION.md` |
| `f9d3a1fcedc4cd7c504c11e1d991cb7bf294d292c14d065c3549194f6ba77c52` | `CHALLENGE-LEDGER.md` |
| `ffea9974b0ecf8723e7b1b83ff1394167d0419cc973c0f6fc0e867a28bf03b39` | `TRANCHE-PORTFOLIO.md` |
| `0564548b2e7149e9ec8f9651c23f66ba277fa21db5ed8406c52f0be66aa1dfb0` | `waves/BB-VALUE-TAPE-P0.md` |
| `b7331cf1217a0bf21177db2f7daf730e01618fcdbda6c88a81b957cefa5766cf` | V3 `ADJUDICATION.md` |
| `78112ea22bfbc25731cede9234dd485cb6eb93196826e1c4ebb371093cab8234` | V3 `REVIEW-A.md` |
| `1867129c19169c93e0ac4c20c4ce062a6876a8ebe6adf6b999f5c18489da7d92` | V3 `REVIEW-B.md` |

### V4 subject files

| SHA-256 | File |
| --- | --- |
| `6de0490fde327c06b8ebac5ae797cb3570aa33841fa80b8e8e12211debd355bc` | `Cargo.lock` |
| `4b9aa76c1c39ddfd6425977101b6d012d5fdac3a763acaebd61e1eec05ac9d39` | `Cargo.toml` |
| `4575b93e4b4549a362795adb8a05d68e6acc1a4c81e0a0e0270969a3dd6b7775` | `EVIDENCE.md` |
| `e41d98f10dccc7a3d8e4490cc7ec8f4afab4cd0b18692ac3d6fc31ff7dd6e115` | `GESTALT-HANDOFF.md` |
| `1840c27038e4359b2bfb7e99165202f5265328c924f54a4becf4e4e649531bc7` | `README.md` |
| `75bbf9dc0a0af3eb949ed4cd810f865a2486070d59f2cc47e2ab97c2e3420d5c` | `SOURCE-RECEIPT-LEDGER.md` |
| `9c71a43b066f1af1ed648457dbc27bd73bcf9e0eb256a95745a6ad172cdc1484` | `src/lib.rs` |
| `62c84b1782d0adf85bdf11b35e5fbf49c321b8ea49b0473c8fdc324fc1a37725` | `src/type_kernel.rs` |
| `07d2d0f0b3ecf624c05c6e4fb247d34e152e8191750d9b65b8683f00fa3d02c1` | `src/type_kernel/infer.rs` |
| `15f9eb0b39ea0e63e3ae0b8d27a149a0d390e67fb7edb7ac5671a47213b1b821` | `src/type_kernel/layout.rs` |
| `48010ae1686b4df3e0b0c33e47278012617bdbd7b0257f72f1b50357e95e244a` | `src/type_kernel/project.rs` |
| `f740fceea39d9b9f1a3549d9236ba7a5a4249d73743030713e117e8f2adc570f` | `src/type_kernel/scheme.rs` |
| `c8cb28f0aa70a955ece159dc301eb1eb60f29b9f9dbd768b21d94e4aad584b65` | `src/type_kernel/solve.rs` |
| `8820f2a56a060c2bed53849577bd2923c624e6dd197a8e496b742368a4f0cf40` | `src/type_kernel/solve/row.rs` |
| `e0e79d9a25048c52a213dc8c4e4fbc79471b54daf1b3fcaa0a775e07eb4737e1` | `src/type_kernel/solve/unify.rs` |
| `b4d02211c3113a70867034d0da322cca036191202ac4a8483474743efd0b29f7` | `src/type_kernel/solve/zonk.rs` |
| `a44b12e2739018cb8dca771b7a59d771993880faad9b8d9410706df3e7a34f2f` | `src/type_kernel/store.rs` |
| `9a77f290cb5f8834618167c7e25d318032ac93533c74e3aa49afd655546e768a` | `tests/type_kernel.rs` |
| `bcd6dd666af6f3a4b76043006eb282bf81e5c8b06cbd36b060109e713bedfc2b` | `tests/type_kernel/infer.rs` |
| `1e5fb03b68febe841a4bb509ef9469a2d26721b5f40301f0805a0c075ac8c40d` | `tests/type_kernel/layout.rs` |
| `e5d4f46cf959aec95029ce404f7992e97dc26369ee079ec0ab362da454b5ef2b` | `tests/type_kernel/project.rs` |
| `b4db51be27d2e3ad39a9d2b4a3a49828ae756c30d88fe0cea8ff6828f6c8f6bb` | `tests/type_kernel/scheme.rs` |
| `4359bb7a4ada2173c85afa7bb02cad371eb4eb7040244ef4cc61c7dd6950f40a` | `tests/type_kernel/solve.rs` |
| `f531b40600f342d4d18c14a7a14dd43e14071a8e92d42b7d99d93e76a2c17afc` | `tests/type_kernel/solve/row.rs` |
| `6d356aff677b271d94844a6b2a54cad2ef82d56fbda831d9953e6a355d5ea2ad` | `tests/type_kernel/solve/unify.rs` |
| `993703ea9ca1258ab02a2900ed5066f544fdd9820524d2a4f246668e364a4cac` | `tests/type_kernel/solve/zonk.rs` |
| `acc2f863ad2eac52f5a4b094665b30931990f46fd70cf4d5864760012a3d58bc` | `tests/type_kernel/store.rs` |

The independently reproduced source/test tree receipt was
`0df94e3316df5b262b4e24dfb96f7af828c67542169a3b41ba52bb77474408e1`.
The repository `Cargo.lock` remained
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.

## Load-bearing findings

### F01 — closure certification skips free recursion reachable from a `Lacks` endpoint

**Correct, defining-capability defect, gate 1 RED.**

`ProblemBuilder::close` adds both endpoints of `Equal` constraints to
`closure_roots` (`store.rs:490-503`). For `Lacks`, it only validates that the row
slot exists (`:504-507`). It never traverses the row's fields through
`validate_closed_root`. This violates the adjudicated requirement that closure
certify every export and every constraint endpoint.

The public probe constructs a free `DraftRecVar`, puts its `DraftTypeId` in a
closed record row, adds `lacks(row, "other")`, closes with no exports, and solves:

```text
lacks-row-with-free-rec-close=Ok("accepted")
lacks-row-with-free-rec-solve=Ok("published")
```

No forged ID, unsafe code, private field, or panic is involved. The compile-fail
draft/closed crater still passes, but it covers only passing a `DraftTypeId`
directly to `solve`; it does not prove exact closure of all accepted endpoints.

### F02 — same-identity binder shadowing admits a semantically vacuous outer `mu`

**Correct, recursive-binder law defect, gate 1 RED.**

`contains_rec` searches by raw `DraftRecVar` identity without treating a nested
`Mu` of the same identity as a shadowing boundary (`store.rs:631-671`). Zonking,
however, does treat it as shadowing by saving and restoring the prior binder
mapping (`solve/zonk.rs:162-177,255-275`). The two phases therefore disagree.

The probe builds `mu r. (mu r. r)`. The only `r` occurrence belongs to the inner
binder, so the outer binder is vacuous. V4 accepts, closes, and solves it:

```text
shadowed-vacuous-outer-mu=Ok("accepted")
shadowed-vacuous-root-bytes=Some([84, 75, 86, 52, ...])
```

The checked vacuity test uses a primitive body, not same-identity shadowing.
Either reusing one binder identity as a nested binder must be rejected, or the
vacuity walk must respect lexical shadowing; accepting one interpretation in the
builder and another in zonk is not coherent.

### F03 — scheme semantic identity contains private validation revision

**Correct, identity/polymorphism defect, gate 3 RED.**

For a non-generalized free variable, `classify_type_var` puts
`revision.rotate_left(17) ^ source` in `PolyVar::Rigid`
(`scheme.rs:487-507`). The scheme digest is then FNV over `Debug` text containing
that key (`:247-271`). This directly violates the adjudicated rule that private
validation revision is excluded from semantic identity.

Two independently built, semantically identical open roots generalized with
`BindingForm::Expansive` produced:

```text
scheme-digest-1=fnv1a64:a69719188d28d0da
scheme-digest-2=fnv1a64:2626e885a5014c3c
equivalent-expansive-scheme-digests-equal=false
```

The checked independent-store test covers only `TypeSolution::semantic_digest`.
It does not cover a scheme with a rigid free variable.

### F04 — `RigidContext` returns a foreign builder handle after successful instantiation

**Correct, arena/capability defect, gate 3 RED.**

`RigidContext` caches raw `DraftTypeId` and `DraftRowId` values by scheme key but
has no owning `ProblemBuilder` scope (`scheme.rs:101-112,551-617`). Reusing the
same public context with a second builder returns the first builder's handle from
an `Ok(InstantiatedRoot)`; only the later close notices `CrossStore`.

```text
first-rigid-draft=DraftTypeId { scope: 5, slot: 0 }
second-rigid-draft=DraftTypeId { scope: 5, slot: 0 }
second-builder-close-after-successful-instantiate=Err(CrossStore)
```

The API promises reusable rigids but does not state or enforce single-builder
affinity. An operation must not report successful instantiation while publishing
a handle invalid for the builder it was given.

### F05 — solved handles alias silently across independent solutions

**Correct, arena/identity/API defect, gates 1/3 RED.**

`RootId`, `SolvedTypeId`, and `SolvedRowId` carry only a `u32` slot
(`solve.rs:15-23`). `TypeSolution::{root_type,root_bytes,view,row}` index that slot
without solution identity (`:161-210`). A type ID obtained from an I64 solution
is accepted by a Bool solution and denotes Bool; a root ID likewise resolves:

```text
first-view=Some(Primitive(I64))
foreign-type-view-in-second=Some(Primitive(Bool))
foreign-root-type-in-second=Some(SolvedTypeId(0))
```

This is precisely the foreign-schema/foreign-revision class the greater value API
must reject. Opaque fields prevent forging, not accidental cross-solution reuse.
The revision exists privately but is absent from every public solved handle.

### F06 — the local "whole grammar" inference is not a whole-grammar solution

**Correct, adjudicated semantic-agreement defect, gate 5 RED.**

For a scalar reference without a handwritten declared type, `infer_node` returns
`Named(target.name)` instead of the inferred type of the target rule
(`infer.rs:433-448`). There is no equality edge from a `Ref` to the target rule's
root. The public counterexample has rule `Scalar = "x"` and rule `Referrer =
Scalar`:

```text
rule:Scalar=Some(Primitive(Span))
rule:Referrer=Some(Named("Scalar"))
```

The types should agree for the adjudicated scalar-ref case. This also explains
why the checked `recursive_json_is_one_assay_not_a_hardcoded_schema` test is not a
recursive type proof: JSON and Array references become unrelated `Named("Json")`
and `Named("Array")` leaves. No `Mu` is inferred and no rule-reference type-flow
cycle is solved.

Other binding deviations are source-compelled:

- the V3 handoff required `KernelInput::from_ir(&GrammarIR)` and stable
  `rule_root(RuleId)`/`node_root(NodeId)` lookups; V4 instead accepts a local
  `GrammarSpec` and exposes stringly `ExportKey("rule:...")` values;
- `RuleSpec::transparent` and `Repeat::one_or_more` affect the input digest but are
  never read by inference;
- `FnDescriptor::Expr.effect` is explicitly ignored (`infer.rs:461-476`);
- `TokenDispatch` creates a token-labelled variant (`:493-507`) rather than the
  adjudicated mapped-continuation/fallback join;
- `Seq` trusts caller booleans and collapses every multi-child type to `Span` when
  they are set, without proving its children are simple spans (`:379-391`);
- heterogeneous alternatives use synthetic branch-index labels and publish no
  exact obligation/lowering evidence.

The handoff admits lack of a live oracle. That admission is honest, but it is a
binding deviation and cannot close V3 gate 5.

### F07 — handwritten host/type facts are the actual semantic source

**Correct, BBNF-only provenance defect and complete-candidate disqualifier.**

No `.bbnf` module exists in or is consumed by V4. Public callers supply:

- `HostTypeNode` graphs containing named types, records, variants, functions, and
  primitive identities;
- `RuleSpec.declared_type`, `FnDescriptor::Expr.return_type`, and host effect;
- `Alt.homogeneous`, `Optional/Repeat.collapse_span`, `RefMode`, and `SeqPolicy`;
- the complete `Primitive` semantic vocabulary;
- `SemanticProfile::GrammarDerivedSchema`, which is merely a caller-selected enum
  label;
- Rust and TypeScript primitive mappings in `project.rs:699-739`;
- physical carrier choices and dimensions in `layout.rs:193-227`.

The CSS test makes the defect especially clear: its "CSS L4 shapes" are a
handwritten six-node host type table, and the grammar's only map points directly
at `HostTypeId(5)` (`tests/type_kernel/infer.rs:420-464`). This proves that the
kernel can copy a supplied schema, not that BBNF derives units, dimensions,
functions, variants, effects, ownership, recovery, serialization, or exact target
types. The README correctly calls this RED. No downstream candidate may import or
adapt these tables.

### F08 — contradictory diagnostics depend on arena construction order

**Correct, determinism-law defect, gates 2/4 RED.**

`solve` sorts equalities by draft endpoint slots (`solve.rs:338-359,403-412`).
Slots reflect arena construction order, not canonical semantic content. Two
equivalent systems—one meta equated with I64 and Bool—produce different mismatch
diagnostics when the primitive construction order is reversed:

```text
diagnostic-reverse-false=Mismatch { left: "I64", right: "Bool" }
diagnostic-reverse-true=Mismatch { left: "Bool", right: "I64" }
```

The solution correctly fails both times, but the constitution requires constraint
order and construction/hash permutations not to change the solution **or
diagnostic**. The 1,024-order row test holds draft construction fixed and then
`solve` re-sorts every submitted constraint, so it does not exercise this case.

### F09 — total operation accounting is retrospective, bypassable, and incomplete

**Correct, budget/proof defect, gate 4 RED.**

The successful 20,000-depth host, solve, and scheme paths are iterative, and the
small-budget checked witnesses return typed `Exhausted`. Those facts do not meet
the V3 accounting contract, which says charge proportional work before starting
work that cannot fit. Examples:

- `State::new` clones all problem nodes and rows before `solve`'s first charge;
  constraints are then cloned and sorted before `solve.order.constraints`
  (`solve.rs:245-285,331-345`);
- `KernelInput::snapshot` sorts collections, builds sets, and creates the entire
  `Debug`-formatted canonical input string before charging digest bytes
  (`infer.rs:166-238,707-725`);
- `validate_host_type` clones tuple/function ID vectors and validates every edge
  without a per-edge charge (`infer.rs:531-590`);
- zonk appends canonical bytes and only charges the completed byte vector at the
  end (`solve/zonk.rs:347-448`);
- scheme generalization builds the entire formatted digest material before its
  charge (`scheme.rs:247-260`);
- projection emits the complete source before charging `source.len()`, and builds
  its manifest before charging it (`project.rs:174-219`);
- `OperationContext::failure` clones the parent-reachable proof graph without any
  charge (`type_kernel.rs:299-335`);
- `OperationContext`, `KernelInput`, `ClosedProblem`, `TypeSolution`, `Scheme`,
  `RigidContext`, `LoweringPlan`, and artifacts expose public derived `Clone`
  traversals with no `OperationContext` at all.

Thus zero-budget solve can do large allocation/sort/clone work before returning
the checked error. No stack abort was reproduced, but total bounded work—the
actual claim—is false.

Proof attribution is also incomplete for row boundaries. `DraftRowNode` does not
store its construction origin (`store.rs:77-88`), so closed-row errors cannot
contain both row-definition causes required by the V4 row table. The checked
closed-difference test asserts only the error fields, not its proof origins.

### F10 — the row-law gate is a useful subset, not the bound matrix

**Correct, test/spec completeness defect, gate 2 RED.**

The same direct solver is genuinely exercised for Record and Variant, and the
implemented cases pass. The test named `record_and_variant_matrix...` invokes
only closed permutation, open/closed symmetry, one open/open disjoint case,
closed difference, one lacks conflict, direct/field occurs, one level capture,
extension order, and duplicate extension
(`tests/type_kernel/solve/row.rs:7-18`). The binding V3 matrix additionally
requires, as named cells:

- open/open equal-prefix tail union and same-tail unequal-prefix rejection;
- lacks identity, idempotence, compatible union, and exact causal attribution;
- row occurs through prior substitutions;
- type occurs through row tail substitution with order permutation;
- equality endpoint symmetry including canonical proof-origin sets;
- explicit transitivity, empty-extension identity, and true three-set extension
  associativity;
- monotonic compatible addition and monotonic conflicting addition;
- root-order and hash-seed dimensions, and repeated canonical row bytes.

The 1,024 test varies only submission order for one fixed seven-operation system.
No random hash or worker claim is needed for this ordered single-threaded carrier,
but construction order and root order remain binding dimensions and are not
covered. Missing mandatory cells alone keep the binary gate RED; F08 supplies an
actual construction-order failure.

### F11 — `Measured` Apple-arm64 layout evidence is a hard-coded caller assertion

**Correct, layout/provenance defect, gate 6 RED for V4.**

`measure_carrier` checks that the caller's string equals
`"aarch64-apple-darwin"`, then returns literal sizes/alignments/offsets. It uses no
`std::mem`, compiler probe, generated layout, host check, disassembly, or native
measurement (`layout.rs:156-258`). `NonNullPointerOption` is marked
`LayoutClaim::Measured` solely by selecting that enum arm.

The probe supplied fabricated compiler identities and still received:

```text
caller-asserted-layout=claim:Measured,size:8,align:8
```

The measurement digest proves only that caller text changed. It does not prove
the text names the executing compiler or that any measurement occurred. In
addition, `lower` only charges and clones `TypeSolution` into `LoweringPlan`
(`layout.rs:128-153`); it derives no carrier, representation, obligation, cost, or
target decision. This is a wrapper, not the adjudicated semantic-to-layout
boundary. V3's separately reproduced native carrier facts remain banked evidence;
V4 does not re-establish them.

### F12 — projection is partially compiler-valid but neither exact nor reproducibly pinned

**Correct, projection/provenance defect, gate 7 RED.**

The checked crater is real and passed under current `rustc` and `tsc`. It repairs
V3's repeated-Named and option-of-function bugs, covers nested records/variants,
and emits an empty variant. It is still not the required matrix or an exact
module-derived API:

- `Primitive::I64` and `U64` unconditionally become TypeScript `number`; no
  `bigint`, checked range, wire policy, or typed rejection exists. Under
  `SemanticProfile::ExactValues`, the generated source was
  `export type root = number;`. Node demonstrated ordinary loss:

  ```text
  requested-source=9007199254740993 runtime=9007199254740992 exact=false
  ```

- `Enum`, `BoxedEnum`, `Diagnostic`, and `ParseResult` map to handwritten target
  stand-ins (`u32`, `Box<u32>`, `String`, `Result<(), String>` and unrelated TS
  shapes), not grammar-derived exact public types;
- the checked target test does not exhaust every primitive, nested list/option
  combination, function returning union, list of function, or all hostile
  type/label rows from the V3 matrix;
- projection receives arbitrary `ToolchainIdentity` strings and never checks the
  invoked compiler;
- there is no `rust-toolchain.toml` or `rust-version`, and the TypeScript command is
  floating `npx --yes tsc` with no `package.json`/lockfile in this crate;
- the manifest records a hard-coded generator string but no generator source or
  binary digest and no versioned semantic-manifest digest. `input_digest=none` is
  accepted for manually constructed solutions.

The current environment happened to match the author text. That is current-host
evidence, not a deterministic toolchain selection mechanism.

### F13 — the facade and proof machinery are not KISS-proportionate

**Correct structural/KISS finding.**

The exact ten-source/ten-test isomorphism is good and no production SCC or package
edge exists. However, the crate contains 5,328 source lines, 1,897 test lines, and
154 textually written public declarations/re-exports. `src/lib.rs` exposes both
`type_kernel::*` and the entire public module hierarchy, creating two access paths
to the same broad assay API. Public cloneable proof graph nodes, raw solved child
IDs, local grammar enums, local host tables, carrier tables, and phase wrappers
become compatibility surface despite having no consumer.

Concrete deletion/collapse opportunities:

- delete the local `GrammarSpec`/`HostTypeNode` inference authority and its CSS/
  pseudo-recursive fixtures rather than adapt it; replace it only when BBNF module
  bytes are executable input;
- delete the hard-coded `measure_carrier` table and caller-asserted
  `GrammarDerivedSchema` label;
- collapse the current `LoweringPlan` wrapper until a real plan owns an independent
  invariant;
- remove or consume `ExportMap`; every call currently discards it;
- choose one public access path and keep draft/solver internals private;
- replace one proof node per accounting tick with the smallest causal structure
  that proves diagnostics, then measure it. V4 has no cost evidence for its proof
  graph;
- do not retain optional provenance (`input_digest=None`) on a production-intended
  artifact route.

The larger 725-line inference, 756-line store, and 809-line projector files are
not automatically god modules, but much of their size exists to model authorities
the binding portfolio forbids. File splitting cannot redeem wrong ownership.

## V3 adjudication and V4 handoff adherence matrix

| Bound V4 obligation | Result | Ruling |
| --- | --- | --- |
| New sibling, no production edit/shim/compatibility path | Standalone crate; no production references found | **PASS at quarantine scope** |
| Exact ten-file source/test topology, external tests, no unsafe/ignored/source tests | Exact mirror; 22 external tests; crate forbids unsafe | **PASS** |
| Draft-to-closed compile-time capability | Direct draft-to-`solve` crater fails as required | **PASS but insufficient**; F01 escapes through a constraint endpoint |
| Exact closure of every export and constraint endpoint | `Lacks` row field graph is not checked | **FAIL, F01** |
| Explicit recursive construction/vacuity/alpha locality | ordinary nested and multi-root examples pass | **FAIL overall, F02** |
| Flat semantic, solved, poly arenas; 20k small-stack success/exhaustion | checked examples pass | **PASS for checked traversals**; total accounting still fails |
| Whole live grammar snapshot and exact production semantic table | local shadow vocabulary, no `bbnf-ir`, scalar Ref mismatch | **FAIL, F06/F07** |
| Stable rule/node results | string export keys only | **FAIL** |
| Complete Record and Variant law matrix | substantial direct subset | **FAIL, F08/F10** |
| Batch schemes, levels, lacks, value restriction, reusable rigids | checked fresh rows pass | **FAIL, F03/F04/F05** |
| Scope-free semantic identities | closed solution example passes | **FAIL overall**; scheme digest includes revision |
| Total operation-bearing public arbitrary-input traversal | deep selected paths pass | **FAIL, F09** |
| Minimal attributable proofs | checked contradiction/lacks slices useful | **FAIL overall**; row construction causes are discarded |
| Semantic-to-Apple-arm64 explicit carrier boundary with exact toolchain evidence | hard-coded/caller-attested table | **FAIL, F11** |
| Full precedence-aware Rust/TS matrix and byte-identical artifacts | checked hostile crater passes | **PARTIAL/FAIL, F12** |
| No convergence/benchmark/production claim | author docs consistently say incomplete | **PASS as an admission** |

Therefore predecessor gates 1–7 are not closed: gates 1, 2, 3, 4, 5, 6, and 7
are all RED on this subject ledger. Gates 8–9 remain outside the assay and wholly
RED exactly as documented.

## Complete feature and law disposition

| Domain | Positive evidence retained | Falsification or missing closure |
| --- | --- | --- |
| Draft arena | scoped draft IDs; direct cross-store checks | closure omits `Lacks` endpoint graph; public values clone without budget |
| Recursive identity | explicit identities; root-local zonk numbering; two-root alpha bytes | same-ID shadow/vacuity disagreement |
| Solved arena | flat nodes/rows; iterative 20k paths | foreign solution IDs silently alias |
| Equality unification | iterative worklist; ordinary occurs checks | construction-order diagnostic drift; no production obligations |
| Record rows | core open/closed/disjoint/lacks/occurs examples pass | mandatory law/proof dimensions missing |
| Variant rows | same direct code and selected matrix pass | same missing dimensions; no exhaustive semantic consumer |
| Level lowering | old-row/young-field checked case passes | no complete prior-substitution/monotonic matrix |
| Zonk/canonical bytes | root-local recursive bytes and repeated checked builds pass | IDs are unscoped; canonical work is charged after construction |
| Polymorphism | flat poly arena, fresh quantified instances, checked lacks/value restriction | revision-tainted digest; cross-builder rigid leak; foreign root accepted by slot |
| Proof/transaction | checked failure publishes no solution; parent reachability excludes one tautology | row causes discarded; proof clone uncharged; no complete causal matrix |
| Input identity | rule/node vector order normalized in checked fixture | handwritten `Debug` encoding; sort/format before charge; no BBNF bytes |
| Grammar inference | exhaustive local enum match | scalar Ref wrong; effects/metadata ignored; recursion is names, not binders |
| Rust projection | checked real compiler succeeds; repeated names/collisions fixed | handwritten semantic mappings; incomplete matrix; toolchain unattested |
| TypeScript projection | checked real compiler succeeds; function-option grouping fixed | I64/U64 exactness lost; handwritten mappings; floating compiler |
| Layout | stable hard-coded carrier receipts | no measurement or semantic lowering |
| Artifact provenance | source/semantic/input text fields and stable checked output | no pinned toolchain/generator digest/semantic manifest; caller strings accepted |
| Generality | generic equality constructors can represent supplied shapes | all types and policies are supplied host facts; no unseen frozen grammar |
| Performance/resources | none claimed as selection evidence | no profiles, allocations, bytes comparison, ARM kernel, or cost problem |

## Tranche fit and greater-product effect

The active portfolio law answers the optimality question even if every local bug
were repaired. The supersession boundary says the V1–V3 horizontal sequence
disproved selection of one horizontal stage before a complete product vertical.
The only binding product experiment is BB-VALUE-TAPE-P0: at least ten independent
complete candidates, each implementing every PT/BB lock, the entire value/tape
surface, exact JSON, exhaustive mechanically frozen CSS L4, exact module-derived
Rust/TypeScript types, Rust/TS/WASM/ARM, packages/consumers, deterministic
artifacts, profiles, holdout, and two clean hostile passes.

V4 implements none of those complete-candidate surfaces. Its only product graph
edge is an isolated crate with zero dependencies; repository search found no
production consumer. That containment is good, but it also means the claimed
mechanisms have not crossed a parser/value/consumer boundary. The greater-product
effect is therefore:

1. no production coupling or legacy path was added;
2. a large second semantic authority was created in quarantine;
3. no BBNF-only provenance, consumer fit, cost, backend, JSON truth, or CSS truth
   was learned from that authority;
4. several useful born-RED tests and implementation ideas were learned, but the
   implementation cannot be a shared compiler/runtime/helper for VT candidates,
   because candidate sharing is forbidden and its input tables violate the
   BBNF-only law.

The tranche was defensible only as a bounded attempt to kill specific V3 claims.
It did kill some and repair some, but its local grammar/layout/projection expansion
is not the smallest experiment and cannot influence architecture ranking. A
smaller control would have retained only the draft/closed, flat solver/row/scheme,
and symbol-printer counterexamples, while deferring grammar, layout, and artifact
labels until a BBNF-source vertical existed.

## Receipts, dependency, and clean replay

Supported receipt claims:

- author source/test receipt, V4 lock hash, and root lock hash reproduced exactly;
- format and strict clippy exited 0;
- both in-repository test replays exited 0 with `22 passed; 0 failed; 0 ignored`;
- a copied clean directory passed `cargo test --locked --offline` with the lock
  unchanged and `cargo tree --locked --offline` showed only the root crate;
- metadata reports zero dependencies and one library plus one integration-test
  target;
- no `#[test]`, `#[cfg(test)]`, or `#[ignore]` occurred under `src`; no actual
  unsafe code occurred; no production reference occurred under `crates` or root
  `Cargo.toml`.

Unsupported or overstated receipt claims:

- exact compiler identities are recorded but not selected or verified;
- `npx --yes tsc` is a network/latest-capable command, not a locked TypeScript
  toolchain;
- `LayoutClaim::Measured` is not a measurement receipt;
- the ordinary test suite's cleanliness does not cover the public counterexamples;
- a zero-dependency graph proves isolation, not semantic agreement;
- build output already existed under the ignored V4 `target` directory; it was
  excluded from the subject as instructed and provides no clean-output proof.

The recorded current environment was arm64 macOS 26.4.1 (25E253), rustc
1.96.0-nightly `02c7f9bec`/LLVM 22.1.2, Cargo 1.96.0-nightly `eb94155a`, Node
v26.0.0, and TypeScript 5.9.3.

## Exact execution ledger

All repository commands ran from
`/Users/mkbabb/Programming/bbnf-lang`. Complete reads used `nl -ba` plus
non-overlapping `sed -n` ranges through EOF for every immutable-ledger file; file
inventory used `rg --files ... | sort`, `wc -l`, and SHA-256 replay. Load-bearing
execution commands were:

```sh
review_tmp=$(mktemp -d /tmp/bbnf-type-kernel-v4-critic-a.XXXXXX)
export CARGO_TARGET_DIR="$review_tmp/target"
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml -- --test-threads=1
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml -- --test-threads=1
```

Results: all four exit 0. Each test run reported 22 integration tests passed and
zero failed/ignored; source unit and doc tests reported zero. Running from the
repository root printed the root's eleven unused-patch warnings, as the author
receipt permits.

The clean-copy replay was:

```sh
clean_copy=$(mktemp -d /tmp/bbnf-type-kernel-v4-clean.XXXXXX)
cp -R restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/. "$clean_copy/"
export CARGO_TARGET_DIR="$clean_copy/target"
(cd "$clean_copy" && cargo test --locked --offline -- --test-threads=1)
(cd "$clean_copy" && cargo tree --locked --offline)
```

Both exit 0. The copied lock was
`6de0490fde327c06b8ebac5ae797cb3570aa33841fa80b8e8e12211debd355bc`
before and after. `cargo tree` printed only `bbnf-type-kernel-v4 v0.0.0`.

Structure, identity, and environment commands were:

```sh
cargo metadata --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --no-deps --format-version 1
rg -n '#\[(test|cfg\(test\)|ignore)' restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src
rg -n '\bunsafe\b' restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests
rg -n 'bbnf-type-kernel-v4|type-kernel-v4|type_kernel_v4' crates Cargo.toml
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src/type_kernel -type f | sort
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests/type_kernel -type f | sort
rg -n '^\s*pub (fn|struct|enum|mod|type)|^\s*pub use|^\s*pub fn' restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src | wc -l
wc -l $(rg --files restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests | sort)
rustc -vV
cargo -vV
node --version
npx --yes tsc --version
uname -m
sw_vers
```

The only `unsafe` match was the word in the Rust keyword table, not code. The
public-line count was 154 and source/test total was 7,225.

The disposable probe had these immutable files:

```text
184c45c8d4e05e99011b49a071319e92179aae17cd51b6241df427babc5b7510  Cargo.toml
e896701d834fc48c1d41eeabb1345b25081c6593ebc365324d322c4f5cd84e23  Cargo.lock
0f541cf4ac805f106bf7eb74a926b74dece6e1605e1330dc5f439a0792c78335  src/main.rs
```

It was run with:

```sh
export CARGO_TARGET_DIR=/tmp/bbnf-type-kernel-v4-critic-a-probe/target
cargo fmt --manifest-path /tmp/bbnf-type-kernel-v4-critic-a-probe/Cargo.toml --check
cargo clippy --manifest-path /tmp/bbnf-type-kernel-v4-critic-a-probe/Cargo.toml --all-targets -- -D warnings
cargo run --quiet --manifest-path /tmp/bbnf-type-kernel-v4-critic-a-probe/Cargo.toml
```

Final results: format 0, strict clippy 0, run 0. An initial format check on the
freshly written disposable probe exited 1 with formatting diffs; `cargo fmt` was
applied only to the disposable directory, after which the exact hashed probe above
passed. The semantic output is reproduced in F01–F08 and F11–F12.

The numeric projection falsifier was:

```sh
node -e 'const requested = 9007199254740993; console.log(`requested-source=9007199254740993 runtime=${requested} exact=${requested === 9007199254740993n}`)'
```

It exited 0 and printed the loss recorded in F12.

Receipt hashes were reproduced with:

```sh
shasum -a 256 Cargo.lock restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.lock
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src \
     restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests \
     -type f -print0 | sort -z | xargs -0 shasum -a 256 | shasum -a 256
```

## Precise disposition and successor routing

1. A fresh adjudicator must reproduce and classify F01–F13 against the immutable
   hashes above. On any correct classification, update the challenge ledger's V4
   Gestalt 1 column to RED and retain clean count zero.
2. Do not run Gestalt 2 as a consecutive-clean pass on this source, do not attempt
   gate 8, and do not present the passing author suite as a closure result.
3. Freeze this V4 ledger and report as falsified assay evidence. No production,
   parse-that, value, tape, backend, playground, or portfolio candidate may depend
   on this crate.
4. Do not answer these findings with another architecture-selecting horizontal
   V5. Carry the public counterexamples—`Lacks` endpoint escape, shadowed vacuity,
   foreign solved IDs, scope-free scheme identity, builder-scoped rigids,
   construction-order diagnostics, total pre-charge, scalar-ref agreement, and
   exact numeric projection—into every complete VT candidate's external born-RED
   suite.
5. Retain only ideas that re-earn themselves from BBNF modules: consumed
   draft/closed capabilities, iterative flat arenas, direct row worklists,
   root-local recursive numbering, semantic-symbol interning, and source/test
   isomorphism. They are donor evidence, not shared code.
6. Retire as successor inputs the local `GrammarSpec`, `HostTypeNode`, `RefMode`,
   collapse/homogeneity switches, handwritten primitive/projector tables,
   caller-asserted semantic/layout profiles, hard-coded carrier measurements,
   stringly export scheme, and unscoped solved IDs.
7. A complete successor begins with BBNF module bytes as the sole semantic/type/
   host source, including versioned BBNF prelude modules for host signatures,
   effects, ownership, and backend availability. It must independently close every
   PT/BB lock, full value/tape API, exact JSON, the exhaustive frozen CSS L4 union,
   exact generated Rust/TypeScript/WASM types and packages, scalar/real Apple ARM
   kernels with differential/profile evidence, unseen grammar holdout, resource
   and cost receipts, and two consecutive hostile gestalts.

Until that route exists, V4's terminal label is **BANK AS FALSIFIED
`INCOMPLETE_ASSAY`; RED; no movement**.
