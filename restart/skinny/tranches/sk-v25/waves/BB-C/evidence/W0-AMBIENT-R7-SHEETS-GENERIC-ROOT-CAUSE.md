MODEL/RUNTIME IDENTITY DECLARATION: gpt-5.6-sol; reasoning effort: ultra; role: R7 bounded hostile root-cause assay.

This identity is metadata exposed to this agent by its runtime. It is **UNVERIFIED**: this assay has no independent model/runtime attestation.

# BB-C.W0 R7 Sheets generic root-cause assay

**State: BORN RED.**

**Assay time:** 2026-07-23T00:36:37Z inspection boundary.

## Executive verdict

The retained `regen-google-sheets` failure and the `compare_op` registry
cluster are real, but they are **not one causal defect**.

1. The exact arena failure has a bounded, source- and binary-backed cause:
   candidate `xtask/src/regen_simple_runtime.rs` at SHA-256 `82a0e360…`
   changed the arena template from `format!(r#"...{{...}}"#)` to a plain raw
   string while leaving the doubled format escapes. The generator therefore
   handed `syn::parse_file` Rust containing `pub enum SheetsCompoundKind {{`.
   The retained binary itself contains `pub enum __P__CompoundKind {{`.
   `syn` reported `expected identifier` before any runtime file was written.
2. `StringId` numeric leakage is independently proven in
   `passes/types/registry.rs`: it serializes an interned string ID with
   `.to_string()` instead of resolving it through the grammar string table.
   This produces the sealed public names `"96"` and `"97"`.
3. Duplicate semantic field names are independently proven in
   `passes/types/subvariants.rs`: equal `TypeDesc` branches deliberately share
   one codegen coercion-variant name. `registry.rs` incorrectly reuses that
   codegen key as public semantic branch identity. Correct StringId resolution
   alone would turn four `"97"` fields into four `compare_op_1` fields; it
   would remain wrong.
4. Mapped-literal prefix factoring is independently proven to change the
   semantic/event surface. It collapses the grammar's six mapped operators to
   five structural branches, creates `Tuple([Span, U8])` beside `U8`, makes
   the direct keyword emitter discard the two mapped tags under the shared
   `<` prefix and emit a `Span`, and makes Pratt mining assign three single
   operators transformed-order tags rather than their grammar map constants.

The three current worktree candidates are the correct minimal source
directions but do not close this report:

- raw single braces in `emit_typed_formula_arena` repair the exact arena
  syntax regression relative to `82a0e360…`;
- excluding `Map(Literal, fn)` from byte-prefix factoring is the correct
  minimal correctness repair for defect 4;
- making every registry Alt field a logical `branch_<idx>` and removing
  `SubVariant` names from registry construction correctly repairs defects 2
  and 3 at their public boundary.

The root reports external ARM suites totaling **118 + 13 GREEN** against the
prefix/registry candidates. That is positive bounded source-test evidence,
but no content-addressed command logs or fresh generated Sheets products were
provided to this assay. The prefix test covers `MapExpr::Input`, not the
constant-mapped overlapping operator case that created the retained Sheets
drift. No production arena regeneration, exact Sheets/error-literal registry
assertion, direct/Pratt event trace, path-plan check, or fresh accepted fleet
exists at this boundary. R7 therefore remains RED.

## Authority and non-actions

This was a read-only hostile assay except for this report. It did not build,
generate, test, profile, mutate, install, stage, commit, or edit source or
generated/runtime products. The worktree was already broadly dirty and other
actors changed relevant files while this assay was running. Those bytes were
preserved.

The analysis preferentially binds the immutable R7 candidate and custody
copies rather than attributing volatile working-tree state to this assay.

## Bound evidence

| Artifact | SHA-256 | Relevance |
|---|---|---|
| R7 candidate author receipt | `024f0dbdf04b72f0c23519436c0b8e470929518cf013cd78ab9f3d01fd008c40` | one-shot order and terminal failure |
| `03-regen-google-sheets.json` | `db75f4f91d4baaf5377508b1715908e3a1618a3aa8e35b6f14f1f5d7cfe5af03` | status 1, exact command, cwd, timing |
| `03-regen-google-sheets.stderr` | `84c1ca19c580f26bcf9a6e930d9008def43586809c5453ef68486ecb8678f4c0` | `parse generated .../arena.rs`; `expected identifier` |
| R7 native binary | `aba131bd616ff0a70c7d288af2b0b204b44a4d4606140b12ab0b64bc7ce03dc2` | exact failing executable, arm64 |
| native build metadata | `0df5d57bf334240f63c236fa3a78cab9c6a7371acbb33d684fcd793a7257b29a` | no-overlay build against corrected POST ledger |
| candidate `regen_simple_runtime.rs` | `82a0e360b64bcd762dd90e584f32f95911bb6fa05305168ee104f257a3621a8f` | raw string plus doubled braces |
| Sheets projection TOML | `4facfd43428f74d4809e00c84a8bf71b3755c5bf5724f97b5fdee109f8aea3d6` | valid identifiers; no `compare_op` route |
| Sheets grammar | `78372003a65bade3baab67ea60e070f9bbbdfe112ed143cb5fc6b0d9ae8704a0` | six mapped comparison operators |
| sealed Sheets parser | `e200eb9867fadd1ced8148144de5af45cb0357a2e61c2b0829c7ac0a35732b84` | exact direct/Pratt event outputs |
| sealed Sheets sidecar | `f916a1ee9fe5a34493b6ac1c6ed7ed093ac013ce5c39a1079979d703d9927451` | five malformed `compare_op` fields |
| checked Sheets parser | `345397a562d102fefcf85908dadb9784737db0c14904c7e98e97fa1d6c9aac79` | pre-sealed comparison operand |
| checked Sheets sidecar | `37a2ee49d2c41b952be8d671baa33b5cc612fbe6c5db25845041bc2bd1f1cf1c` | pre-sealed comparison operand |
| candidate `prefix.rs` | `9241b8cd960452c959cf9652d969f9074a379e0585600fd84ecdde4d72b8de08` | mapped-literal factoring preimage |
| `types/registry.rs` | `baf448dbfe941c584343080f391b0d9cb2a6708db3725313624b183d34e32312` | numeric leakage/public-name misuse |
| `types/subvariants.rs` | `70994e5291aff9dbf61fada3f1e924bebdaf9770dc68340478ae1b391b0639bd` | equal-TypeDesc codegen-name reuse |
| `types/mod.rs` | `48bdab1ac30f15320214038318eeaf1800a6d4eb0c10008397643d2b3b325912` | interning and phase order |
| keyword `struct_direct.rs` | `4cfd2e8c4d02cd4df1a044a605c4e1e772e1dd9640cbe44f7a83fad2225eea66` | Seq rollback-to-Span event owner |
| operator `grammar_facts.rs` | `5bbdc2ac1030a47f90ac9a1646fda623d4d20e8c09e25a5d2e8fa6a6331ad4a1` | post-transform ordinal discriminants |
| Pratt `struct_direct.rs` | `bfc2dff4727d11dcf62bbb3f423cd0c0f3f52e7c888c9e15112cdf6d43f288d2` | emits mined discriminant as branch tag |
| precedence emitter | `6622e66f836315983137288abebf1eb39f9b0e49c3d229069bc96e2272eb9df6` | emits mined entries verbatim |

Current candidate identities at the seal boundary are:

| Candidate | SHA-256 | Adjudication |
|---|---|---|
| raw/single-brace `regen_simple_runtime.rs` | `06f8da21f333693357bc0cd323dc22a0d011ec3dcc85c5f70e09830fb777744e` | **CORRECT source repair** for the arena syntax cause; production regeneration still unproven |
| mapped-literal-excluding `prefix.rs` | `ca4603be2d71abab3f31b566fcb56ea72be37c3cbd47dbda1317ad1326bb151a` | **CORRECT minimal generic repair** for Map relocation, including aligned documentation; exact constant-map/Sheets products still unproven |
| logical-name `types/registry.rs` | `5cefa20606c50ea86d2b42d4588b10716b5a628bbb9533daa2a2358c7729db5d` | **CORRECT minimal public-identity repair**; full generated/path/fleet closure still unproven |
| prefix regression test | `6ab6691e877c545049e6ec91ebbc4bd70f489ad0b2ca67cb1f5beca677e21973` | correct `MapExpr::Input` gate, incomplete for distinct constant maps |
| registry regression test | `c5cbde12635ee3edeb03f0874b4d20c3f79aeb89bd942541d338a5389ff4805d` | correct privacy gate for internal sub-variant names, incomplete as a Sheets product gate |

The sealed parser is 13,800 lines / 674,254 bytes. The sealed sidecar is
1,260 lines / 27,312 bytes. The focused canonical JSON stream consisting of
sealed layouts 3 (`error_literal`) and 7 (`compare_op`) hashes to
`11cbf9602a2b00b22bccad93c9b7912ba15570100c4c1dd0606eb85a0dbaad58`.

## 1. Exact arena failure: template escaping, not the registry cluster

### Retained failure

The atomic candidate ran `regen-json` and `regen-css-pretty` successfully,
then ran `regen-google-sheets` once and stopped with status 1:

```text
Error: parse generated `/private/tmp/bbcw0-r7-programming-candidate.dyTgSs/bbnf-lang/crates/core/src/runtime/google_sheets/arena.rs`

Caused by:
    expected identifier
```

The existing candidate Sheets arena remained unchanged and valid; Sheets had
zero runtime-output changes. The failure happened in the in-memory render /
`syn::parse_file` phase before the write.

### Source transition

Against repository `HEAD`, candidate source `82a0e360…` made only this
semantic template transition:

```diff
- runtime_template(&format!(r#"
+ runtime_template(&r#"
  ...
  pub enum __P__CompoundKind {{
  ...
- "#,).replace(...)
+ "#.replace(...)
```

The doubled braces were format-string escapes. Once `format!` was removed,
they became literal output. Candidate lines 472-592 call `runtime_template`
on that already-invalid raw source; `runtime_template` only performs marker
replacement. Candidate lines 359-362 then pass the result to
`syn::parse_file`.

This is not merely inferred from the checkout. `strings -a` over binary
`aba131bd…` contains the exact literal:

```text
pub enum __P__CompoundKind {{
```

The build metadata records a successful no-overlay build from the 843-member
POST ledger whose `regen_simple_runtime.rs` row is exactly `82a0e360…`. The
binary behavior, source bytes, embedded string, and parser diagnostic agree.

### Why `compare_op` cannot cause this arena syntax failure

The arena renderer's inputs are narrowly visible in candidate source:

- enum variant identifiers come from `projection.kind.variants`;
- route arms contain only `layout.rule_id` and TOML `route.kind`;
- default and transparent variants come from the TOML;
- field names, `FieldSource`, `TypeDesc`, and sub-variants are never read by
  `emit_typed_formula_arena`.

All variant/default/transparent identifiers are validated as Rust-like
identifiers before rendering. The TOML declares a `CompareOp` enum variant,
but its route array contains no `{ rule = "compare_op", ... }` entry. Routes
cover `error_literal`, selected compound rules, and `formula`; the operator
rule itself is absent. Thus even malformed `compare_op` fields `"96"` and
`"97"` do not enter the arena source.

### Exact minimal arena repair and status

Either of these is generically correct:

1. retain the raw string and make every Rust brace single; or
2. restore `format!` and retain doubled format escapes.

The current worktree candidate takes option 1. Relative to the exact failing
`82a0e360…` source, that is the correct local repair. Relative to repository
`HEAD`'s `format!` form, the two valid forms are output-equivalent. The repair
must not be credited with any registry correction.

Required proof is an arena-render test using the production Sheets TOML plus
a production-shaped registry, followed by `syn::parse_file`; a synthetic
marker-only string is weaker. A clean, source-ledger-bound rebuild and
one-shot atomic runtime regeneration remain required for R7 acceptance.

## 2. The compare surface across grammar, registry, parser, and Pratt

The grammar declares this stable semantic mapping:

| Literal | Required `u8` |
|---|---:|
| `<>` | 0 |
| `<=` | 1 |
| `>=` | 2 |
| `=` | 3 |
| `<` | 4 |
| `>` | 5 |

The sealed outputs disagree in three different ways.

### Sealed registry

`compare_op` is rule 7, `TaggedEnum`, rule type `U8`, but has only five
fields:

| Structural index | Name | Field type |
|---:|---|---|
| 0 | `"96"` | `Tuple([Span, U8])` |
| 1 | `"97"` | `U8` |
| 2 | `"97"` | `U8` |
| 3 | `"97"` | `U8` |
| 4 | `"97"` | `U8` |

Its path plan consequently contains five `VariantName` rows for indices 0-4.
Variant lookup matches by field name and explicitly diagnoses multiple
matches as `AmbiguousVariant`; `@97` therefore cannot identify one branch.

### Sealed direct keyword parser

For `<>` and `<=`, the nested suffix map first pushes `u64` 0 or 1. The
Seq branch then rolls the builder back to its checkpoint and pushes the whole
matched operator as a string span. Those two semantic tags are discarded.

The remaining literal maps emit their grammar constants directly:

| Literal | Direct event |
|---|---|
| `<>` | `Span("<>")`, not tag 0 |
| `<=` | `Span("<=")`, not tag 1 |
| `>=` | tag 2 |
| `=` | tag 3 |
| `<` | tag 4 |
| `>` | tag 5 |

The generic owner is keyword `struct_direct.rs`: its Seq branch records a
checkpoint, emits the inner sequence, then on success rolls back every inner
record and pushes one `__seq_text` span. That behavior is valid only when the
factored Seq is semantically a span. It is incompatible with the registry's
claimed `Tuple([Span, U8])` and with a mapped `U8` semantic result.

### Sealed Pratt parser

The comparison Pratt table emits:

| Literal | Pratt tag | Required tag |
|---|---:|---:|
| `<>` | 0 | 0 |
| `<=` | 1 | 1 |
| `>=` | 2 | 2 |
| `<` | 3 | 4 |
| `>` | 4 | 5 |
| `=` | 5 | 3 |

`collect_inlined_alt_operators` and `collect_operator_alternatives` in
`grammar_facts.rs` enumerate literals after transformations and assign an
incrementing ordinal. They strip Map wrappers rather than reading the mapped
constant. The Pratt emitter then pushes that mined ordinal as the branch tag.
Prefix factoring changed traversal order, so three single-byte operators are
silently remapped. Round-trip tests can mask this because generated parsing
and generated serialization may share the same wrong table.

## 3. Defect A: `StringId` numeric leakage

### Mechanism

`SubVariant.variant_name` is a `StringId` (`u32`), not a string. Type
projection phase 5 does the right first half:

1. `collect_sub_variants_raw` creates textual names such as
   `compare_op_0` and `compare_op_1`;
2. `types/mod.rs` lines 311-330 interns those names into `ir.strings`;
3. `SubVariant.variant_name` stores the resulting numeric ID.

Registry population then does the wrong second half. Its helper finds a
branch and calls:

```rust
.map(|sv| sv.variant_name.to_string())
```

That formats the numeric ID. Other consumers establish the intended contract:
for example schema construction and Rust IR-type construction resolve a
sub-variant with `ir.get_string(sv.variant_name)`.

### Minimal A-only candidate

Resolve the StringId while the grammar string table is available. A
borrow-safe implementation can snapshot, per rule, a map from branch index to
`ir.get_string(variant_name).to_owned()` before mutably publishing the
registry, then pass those resolved strings into layout construction. An
out-of-range ID must fail the production projection rather than fall back to
its decimal representation.

This candidate is necessary if any consumer continues to expose sub-variant
names. It is **not sufficient** for the registry, because defect B remains.
The implemented registry candidate takes the stronger and cleaner option:
sub-variant coercion names are internal and are not consumed by registry
construction at all. That eliminates the leaking boundary rather than merely
formatting its ID correctly. This is adjudicated **CORRECT**.

## 4. Defect B: codegen coercion identity reused as semantic identity

### Mechanism

For a heterogeneous Alt, `subvariants.rs` builds
`seen_types: Vec<(TypeDesc, String)>`. When a later branch has the same
`TypeDesc`, it clones the existing variant name. That is deliberate and
reasonable for codegen coercion: one enum payload variant can represent one
structural type.

It is not a semantic branch name. The registry documents field names as
public branch identity and path lookup matches them textually. `registry.rs`
nevertheless substitutes the type-coercion name for `branch_<index>`.

For the transformed compare shape, the first Tuple gets
`compare_op_0`; all four `U8` branches get `compare_op_1`. Defect A changes
those strings to IDs 96/97; it did not create the reuse.

### Exact minimal generic registry repair

Stop using `RuleMeta::sub_variants` as the public field-name source. With the
current IR, the smallest sound rule is:

```text
Alt field semantic name = explicit source branch label, if one exists;
                          otherwise "branch_<logical_source_index>".
```

There is currently no separate explicit source-label field, so
`branch_<logical_source_index>` is the minimal safe production surface.
`SubVariant.variant_name` may remain a codegen-only coercion key.

The implemented `types/registry.rs` candidate does exactly this for the
current IR: it removes the `SubVariant` parameter/helper and unconditionally
names Alt fields `branch_<branch_idx>`. Because the name is constructed once
per enumerated branch index, names are unique within each emitted layout and
cannot be decimal StringIds or equal-TypeDesc aliases. It is adjudicated
**CORRECT for defects A/B at the public registry boundary**.

The durable model should make the split explicit:

- `semantic_branch_id` / `semantic_branch_name`: unique per source
  alternative and stable through transforms;
- `coercion_variant_key`: allowed to be shared by branches of equal
  `TypeDesc`.

Do not infer the first from the second.

The deterministic `branch_<idx>` construction makes duplicate production
names impossible for the current enumerated Alt. A separate validation gate
remains useful defense in depth, especially if explicit labels later enter
the schema. Keep raw `StructRegistry::insert` permissive if malformed-layout
diagnostic tests need to construct duplicates; validate production
registry/sidecar emission before writing ambiguous JSON.

### Candidate proof and remaining RED boundary

The current test
`tagged_enum_layout_keeps_internal_sub_variant_names_private` gives two
branches the same internal sub-variant ID and requires public names
`branch_0`, `branch_1`. That is the right ownership assertion, and the root's
reported ARM suite is GREEN. The source construction is type-independent, so
equal `TypeDesc` cannot reintroduce aliasing. A larger equal-`U8` test is still
required as the exact historical regression and a generated Sheets sidecar
must show the six expected fields before fleet acceptance.

The candidate uses the current post-transform branch index. It does not by
itself add durable source-alternative provenance for arbitrary future Alt
transforms. The mapped-literal exclusion prevents the known transform from
collapsing Sheets alternatives; generalized provenance remains **UNPROVEN**,
not a reason to reject this minimal repair.

## 5. Defect C: mapped literal/prefix factoring changes semantic topology

### Mechanism in the bound preimage

Candidate `prefix.rs` `9241b8cd…` classifies both bare literals and
`Map(Literal, fn)`. For a same-first-byte group, it strips the first byte and
moves each Map onto the suffix:

```text
Map(Literal("<>"), tag 0) | Map(Literal("<="), tag 1)

becomes

Seq(Literal("<"),
    Alt(Map(Literal(">"), tag 0),
        Map(Literal("="), tag 1)))
```

That has four generic consequences:

1. `MapExpr::Input` sees only the suffix instead of the full literal.
2. A semantic branch-level effect is nested below a structural Seq.
3. Two source alternatives become one outer structural branch; Sheets drops
   from six logical alternatives to five structural fields.
4. Type projection sees the Seq as `Tuple([Span, U8])`, while the other
   branches remain `U8`.

Type projection and registry classification then faithfully expose the
transformed structure; they cannot reconstruct lost source arity. The keyword
Seq emitter further collapses the nested events to a Span. The Pratt miner
re-enumerates transformed literals rather than preserving mapped values.

### Exact minimal generic prefix repair

Do not byte-factor any `Map(Literal, fn)` branch. Factor only bare literals.
This is the smallest correctness-first rule and is the current worktree
candidate. It preserves whole-match input, mapped effects, source Alt arity,
and operator declaration order. It may sacrifice a local trie optimization;
that is an acceptable correctness trade.

A future optimized representation may factor recognition while carrying
original semantic branch provenance and applying the Map to the complete
match after selection. Such a representation must prove event-trace
equivalence and retain the source branch index. Moving Map to a suffix is not
that representation.

### Exact objections to the implemented prefix candidate

The source implementation is correct and the root reports its ARM tests
GREEN, but product-level proof is incomplete:

- the modified test uses `MapExpr::Input`; add a constant-mapped overlap
  fixture matching `<>`, `<=`, and `<` so the exact Sheets topology and tags
  are covered;
- assert six logical branches and six `U8` fields after the full type pass,
  not only IR equality for two mapped literals;
- assert the Pratt table reads the mapped semantic constants, particularly
  `= -> 3`, `< -> 4`, `> -> 5`;
- assert direct builder events for `<>` and `<=` are tags, not spans;
- retain the now-aligned function contract documentation so future changes do
  not silently re-admit mapped literals.

The prefix repair **by itself** does not resolve interned registry names or
separate semantic identity from coercion identity. The independent logical
`branch_<idx>` registry candidate repairs that boundary.

## 6. Required tests and admission gates

These are the minimum generic gates, not optional regression polish.

### Arena/template

1. Render `emit_typed_formula_arena` using the real Sheets projection and a
   production-shaped registry; require `syn::parse_file` success.
2. Exercise both all declared variants and all production routes. A test that
   never expands `__VARIANTS__`/`__ARMS__` is insufficient.
3. From a clean source-ledger-bound native binary, run the atomic fleet once;
   retain source, binary, command, and every output hash.

### String IDs and semantic names

4. Registry-privacy unit test: even a nontrivial or shared internal StringId
   never enters public fields; fields are logical `branch_<idx>` names.
5. Equal-type test: at least three semantic branches with the same
   `TypeDesc::U8` may share a coercion key but must have distinct production
   registry names.
6. Production duplicate gate or construction invariant: duplicate semantic
   names cannot reach a written sidecar.
7. Path lookup: every emitted semantic name resolves to exactly one logical
   branch; no production `AmbiguousVariant` is possible.

### Prefix, type, events, and Pratt

8. Prefix unit tests for both `MapExpr::Input` and distinct constant maps over
   overlapping literals; mapped branches remain semantically whole.
9. Full Sheets projection test: `compare_op` has logical arity six, rule type
   `U8`, six unique branch identities, and no `Tuple([Span, U8])` field.
10. `error_literal` projection test: its mapped `U8` result is not exposed as
    a phantom Seq of leading `Span` plus `U8`.
11. Direct event trace for all six comparison literals: exactly tags 0-5 in
    grammar order, with no extra/Span event for `<>` or `<=`.
12. Pratt metadata test: exact literal-to-discriminant mapping
    `<>:0, <=:1, >=:2, =:3, <:4, >:5`. Prefer deriving the discriminant from
    `MapExpr::IntLit`/source provenance rather than trusting post-transform
    enumeration.
13. Public runtime tests must inspect tag/value semantics, not merely parse
    success. Existing `operator_branches_parse` only proves recognition.
14. Cross-surface parity: direct rule, Pratt expression path, registry, and
    serializer agree on the six mappings. Do not use generated round-trip
    alone as the oracle.
15. Fresh generated parser, sidecar, path plan, and runtime files reach a
    content-identical second generation from the same accepted binary.

With mapped factoring removed, all six branch field types should be `U8`.
Under the current registry classifier that naturally yields
`LayoutKind::UntaggedEnum`, as with other homogeneous `U8` operator rules, so
the path plan may correctly collapse to a wildcard rather than six
`VariantName` rows. If product requirements instead demand named variant path
selection for scalar tags, that is a separate schema decision; do not obtain
it accidentally from the malformed Tuple/U8 heterogeneity.

## 7. Rejected symptom-only changes

The following are adjudicated **INCORRECT** as closure claims; they do not
close this cluster:

- renaming `"96"`/`"97"` to `compare_op_0`/`compare_op_1` without separating
  semantic branch identity from the equal-TypeDesc coercion key;
- suffixing the four duplicate current fields after registry construction;
  that blesses five transformed branches instead of restoring six logical
  alternatives;
- hand-renaming fields in the JSON sidecar, generated Rust, path plan, or
  runtime;
- changing `Tuple([Span, U8])` to `U8` in serialization while leaving the
  prefix transform and Seq rollback behavior intact;
- teaching the keyword Seq emitter to keep only the nested U8 without
  restoring logical branch arity/provenance;
- changing grammar declarations or tag constants to match the transformed
  Pratt order;
- adding a `compare_op` runtime projection route or removing the unused
  `CompareOp` enum variant as an arena fix; neither affects doubled braces;
- weakening/skipping `syn::parse_file`, accepting doubled braces, or writing
  an invalid arena for later compilation to diagnose;
- accepting parse-success or self-round-trip tests as proof of discriminant
  semantics;
- accepting the raw/single-brace arena patch as evidence that registry names,
  types, or events are repaired;
- accepting the mapped-literal exclusion alone as evidence that StringIds and
  semantic names are repaired (the separate logical-name registry candidate
  is what repairs that boundary).

## Final disposition

The immediate arena syntax failure has a precise local cause and a correct
source repair. Mapped-prefix relocation has a correct minimal source repair.
Registry semantic-name ownership has a correct minimal source repair:
codegen sub-variant names stay private and public fields use logical
`branch_<idx>`. The reported external ARM `118 + 13` suites are GREEN. None of
those bounded judgments converts stale generated products into accepted
evidence.

The remaining exact gates are RED: regenerate and inspect `compare_op` as six
homogeneous `U8` branches with grammar tags 0-5; inspect `error_literal` for
the absence of a phantom prefix Span; verify the resulting path plan has the
intended homogeneous-enum behavior with no ambiguous names; run direct and
Pratt semantic tag tests; parse/render the production arena; and complete a
source/binary-bound, second-generation-identical atomic fleet.

Until those conditions hold under a source- and binary-bound atomic fleet,
the only defensible state is **BORN RED**.
