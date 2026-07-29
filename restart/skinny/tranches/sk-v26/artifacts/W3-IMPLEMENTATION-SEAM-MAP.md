# W3 clean-break implementation seam map

Status: `BORN_RED_ACCEPTED_IMPLEMENTATION_AUTHORIZED_AFTER_GIT_DURABILITY`.

This is an implementation routing artifact, not evidence that implementation
has begun. No production W3 edit is authorized until the complete born-RED
packet is hostile-review clean and git-durable.

Authority:

- `W3-DESIGN-ADJUDICATION.md`;
- `W3-GENERATED-V6-CANONICAL-SHAPE.md`;
- current W2 compiler source at BBNF `28461bf7c`;
- parse-that ABI 3 at `ed1ffa24e862b797b01dc4f9215d6be8f7fa0955`.

## Clean-break public boundary

W3 exposes only:

```rust
pub struct GrammarInstanceInput<'a> {
    manifest_bytes: &'a [u8],
    source_bytes: &'a [u8],
}

impl<'a> GrammarInstanceInput<'a> {
    pub const fn new(manifest_bytes: &'a [u8], source_bytes: &'a [u8]) -> Self {
        Self {
            manifest_bytes,
            source_bytes,
        }
    }
}

pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule>;
```

W3 deletes rather than adapts:

- `ModuleInput`;
- `compile_module`;
- `CompilationSession`;
- `EditError`;
- `compiler/edit.rs`.

No compatibility module, deprecated forwarding function, manifest-less edit
session, or path-derived logical identity is admitted.

## Two justified new modules

### `compiler/instance.rs`

Sole authority for:

- borrowed raw `GrammarInstanceInput`;
- strict duplicate-aware JSON decoding;
- the exact five-field typed `GrammarInstance`;
- manifest-local logical-path/name validation;
- trivia, Pratt-row shape/fixity/power, and compile-budget validation;
- canonical W3 Revision input serialization;
- the one name-bearing typed manifest model;
- a private validated instance/source carrier shared by ordinary compile and
  replay.

Manifest validation must not be copied into the frontend, replay, CLI, or
emitter. Source-dependent entry/site/atom/matcher existence, site-root
identity, and attachment are deliberately not decided before source parsing.

### `compiler/pratt.rs`

Sole authority for:

- private compact Pratt table/operator indices;
- revision-branded public IDs and checked lookups;
- fixity and binding-power representation;
- resolved table/operator rows and public read-only views.

Name-bearing manifest rows remain in `instance.rs`. Resolved compiler/runtime
rows are ordinal-only. Do not create one file per operator or fixity.

No generic `semantic_ops.rs` is planned: dependency, type, analysis, lowering,
event, and emission laws differ materially and remain colocated with their
owning stage.

## Root-owned high-collision spine

The primary implementation owner changes these sequentially after the central
types freeze:

1. `compiler/budget.rs`
2. `compiler/diagnostic.rs`
3. `compiler/instance.rs` (new)
4. `compiler/pratt.rs` (new)
5. `compiler/source.rs`
6. `compiler/syntax.rs`
7. `compiler/mod.rs`
8. `lib.rs`
9. root `Cargo.toml`, `crates/core/Cargo.toml`, and `Cargo.lock`
10. `compiler/frontend.rs`
11. `compiler/dependency.rs`
12. `compiler/typecheck.rs`
13. `compiler/analysis.rs`
14. `compiler/lower.rs`
15. `compiler/event.rs`
16. `compiler/emit.rs`
17. `compiler/replay.rs`
18. migrate every consumer and external control to the frozen instance API
19. delete edit-session callers and edit-only tests
20. atomically delete `compiler/edit.rs`, its module/export, the legacy
    ingress, raw-path/source replay authority, and implicit entry fallback
21. run the terminal active-source, target, external-control, and W3 gates

Parallel agents must not edit this shared spine before the foundational public
and internal carriers are frozen. Later delegation is restricted to coherent,
low-collision control packages, migration tests, or extracted emitter writers
with an already-fixed parent interface.

## Foundational transition

`CompileOptions` adds exact limits for:

- instance bytes;
- Pratt tables;
- Pratt operators;
- presentation hints.

Diagnostics add:

- `InvalidGrammarInstance`;
- `UnknownEntry`;
- `InvalidPrattTable`.

`source.rs` loses `ModuleInput` and raw-path Revision authority. The canonical
revision moves into the typed instance boundary and uses domain
`bbnf.w3.revision.v1\0`.

`syntax.rs` adds:

- `ProjectionKeep`;
- `Projection`;
- `Difference`;
- `Trim`;
- `Pratt`;
- explicit instance entry resolution;
- deterministic manifest-attached Pratt site/matcher resolution.

After parsing, `syntax.rs` is the sole owner of source-dependent resolution:
it consumes the typed instance model without copying or reparsing it, resolves
entry/site/atom/matcher names, validates the exact site-root identity, routes
`UnknownEntry`/`InvalidPrattTable` diagnostics, and attaches the deterministic
Pratt wrapper. It neither decodes manifest JSON nor duplicates manifest-local
shape, budget, trivia, fixity, or power checks.

The preferred attachment seam is after source parsing but before SourceMap is
sealed: validate that the site root is exactly its atom reference, append the
Pratt wrapper deterministically, and redirect the site rule. Attaching after
Program construction would invalidate the assumption that `NodeRemap` remains
identity and is not preferred.

`compiler/mod.rs` becomes `compile_instance` plus one private
`compile_validated_instance` route used by trusted replay. `CompiledModule`
retains the typed instance and exact top-level source authority required by
complete replay.

## Frontend

Keep `Parser` and draft nodes together in `frontend.rs`; splitting a one-use
parser into many tiny modules is rejected.

Required lexical/syntax admission:

- transactional `//` and non-nested block comments outside literals/regexes;
- structured `@pretty`;
- group, brace, and bracket primary forms;
- postfix recognition order `?w`, then `?`, `*`, `+`;
- one left-associative binary tier for `<<`, `>>`, and `-`;
- comma concatenation;
- choice;
- structured Projection, Difference, and Trim drafts.

Only a cohesive `frontend/lexical.rs` is justified if comment/trivia handling
would otherwise obscure the parser.

## Stage laws

### Dependency

- Projection: both children.
- Difference: include and exclude.
- Trim: child.
- Pratt: atom plus every operator matcher rule.

### Type

- Projection: derive both; result is kept side.
- Difference: derive both; result is include.
- Trim: child.
- Pratt: atom.

All discarded/excluded children retain public derivations.

### Analysis

- Projection: nullable conjunction; minimum checked sum.
- Difference: nullable only `include && !exclude`; nullable exclusion makes
  the language empty; otherwise minimum conservatively inherits include.
- Trim: child facts.
- Pratt: atom facts followed by nonempty atom/matcher validation.

Every child participates in postorder, work, and overflow. Difference never
sums include and exclude minima.

### Lowering

Adds the four exact PlanOps, one optional separately owned trivia DFA, compact
Pratt arrays, revision-branded checked views, and no runtime grammar graph.

Ordinary grammar regex plans and the singleton trivia plan remain visibly
distinct in storage, replay, and emission.

## Events, emission, and replay

`event.rs` advances to scalar-events v5 and adds only:

- `Trivia`;
- `PrattReduce`.

Generated scalar events gain optional operator identity, absent from every
other event kind.

`emit.rs` advances to generated Rust v6 and follows
`W3-GENERATED-V6-CANONICAL-SHAPE.md`. If extraction is required after the
parent interface freezes, only cohesive `compiler/emit/pratt.rs` and possibly
`compiler/emit/trivia.rs` writers are justified.

`replay.rs` advances the exact version matrix, retains typed instance plus one
top-level source authority, removes duplicated logical path/source bytes from
SourceMap modules, reconstructs through the private validated compile route,
and compares every carrier and generated byte. It never reconstructs by
calling a legacy ingress or by treating serialized stage carriers as trusted.

## Active consumer migration

Production:

- `compiler/replay.rs`;
- `compiler/frontend.rs` internal helpers;
- `compiler/edit.rs` (delete);
- `src/bin/bbnf-gen.rs`.

Active lib-unit consumers compiled with the library target:

- `compiler/analysis.rs`;
- `compiler/typecheck.rs`;
- `compiler/syntax.rs`.

Their fixture construction must cross the same private validated-instance seam
as production without restoring a public manifest-less ingress.

The CLI becomes:

```text
bbnf-gen MANIFEST SOURCE OUTPUT
```

It reads only user-supplied paths, passes the exact two byte sequences to
`compile_instance`, and writes the selected output. It derives no logical path
and contains no candidate or format dispatch.

Active integration targets:

- `frontend_contract`;
- `dependency_scc_laws`;
- `type_analysis_laws`;
- `emitter_contract`;
- `replay_edit_laws`;
- `record_semantic_laws`;
- `carrier_lookup_laws`;
- `identity_opacity` and its UI cases;
- `w3_instance_frontend_contract`;
- `w3_semantic_pratt_contract`;
- `w3_replay_public_contract`;

Non-edit laws migrate to explicit generic instance fixtures. Edit-session-only
laws are deleted; replay, identity, lookup, and canonicality laws remain.

Representation and public-crossing consumers:

- `contracts/w3-grammar-instance.schema.json`, retained beside
  `instance.rs` as a decoder-equivalence contract rather than a second
  production decoder;
- `W0-PUBLIC-WITNESS-ADDENDUM.tsv`;
- all three public instance manifests and case ledgers;
- `W3-PUBLIC-WITNESS-CASES.tsv`;
- the one generic dual-root witness harness;
- the clean registry/package-crossing gate;
- the forbidden-surface and candidate-preseal authorities.

The five held-out candidate manifests remain hash/read-only and unexecuted
until the post-freeze selection boundary.

External build consumers:

- `contracts/w2-generated-control/build.rs`;
- `contracts/w2-recursive-control/build.rs`;
- `contracts/w2-record-control/build.rs`.

Historical W2 tranche artifacts are immutable evidence, not active consumers.

Before deletion, rerun a full active-source search for `ModuleInput`,
`compile_module`, `CompilationSession`, `EditError`,
`bbnf.w2.source.v4`, `Revision::for_module`, SourceModule logical-path/source
fields and accessors, replay copies of those fields, and implicit
`name == "root"` or first-rule entry selection. Every executable hit must have
a terminal migration or deletion disposition.

The deletion phase is atomic:

1. migrate replay, CLI, all active unit/integration targets, and all three
   external W2 build controls;
2. delete edit callers and edit-session-only assertions;
3. delete `compiler/edit.rs`, `mod edit`, its exports, all legacy ingress
   declarations/calls, raw-path/source duplication, and implicit entry
   fallback;
4. verify no public or private adapter remains.

Terminal evidence requires:

- `cargo test --workspace --all-targets --locked`;
- all three W3 contract targets;
- all three migrated external W2 generated-control builds;
- the representation schema/decoder equivalence gate;
- every public witness case through the one generic dual-root harness;
- clean registry/package crossing;
- forbidden-surface absence and candidate non-execution.

## Anti-contrivance constraints

Do not add:

- runtime grammar graphs;
- format or plugin registries;
- backend traits for one Rust emitter;
- host callbacks or native code;
- compatibility adapters;
- empty feature/dispatch surfaces;
- duplicated manifest models;
- tiny one-type modules for Projection, Difference, or Trim;
- private naming data in ordinal generated descriptors.

The clean break is complete only when the old ingress and edit surface have no
active declaration, export, call site, test fixture, or package consumer.
