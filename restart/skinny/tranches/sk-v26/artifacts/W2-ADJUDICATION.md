# W2 terminal adjudication

Task: `/root/skv26_w2_final_adjudicator`.

Final disposition: `ACCEPT`.

No P0, P1, or P2 finding remains in the frozen W2 implementation, paired
parse-that kernel, six-artifact evidence packet, or hostile reviews.

## Exact accepted subjects

- BBNF carrier commit
  `28a91db51177495efb61cd1c664a85b37fdd9529`, tree
  `df6f1b063acfc49b3e0382fbd4a46892be7e5328`;
- parse-that commit `282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f`,
  tree `4e813a353251f86906baee44a9c4d28e743fe27b`;
- W2 evidence commit `60ee1a51d694b5cf3fd76779336ef165e593129f`,
  tree `02688383ebb21aeac1904377bf1598eb412fa218`;
- hostile-review commit
  `f46eb9497522ea97e164e8a1e4160784aa53eb1e`, tree
  `fa991843932b0b3f19332a8208e4fa287cf6519d`.

The evidence commit is an exact descendant of the carrier and adds only the
six mandated W2 artifacts. The review commit is an exact descendant of the
evidence commit and adds only Reviews A and B.

Review digests reproduce:

- Review A:
  `f10b3cd260d33437676e9ad389a889762341980a5a552940060360a06bcae350`;
- Review B:
  `1825f2995cd7109ff51fabc36b54a23612fe85eec9fa0a39c7f40818b9ba7761`.

All adjudication used fresh, clean, detached, tree-exact clones.

## Identity and carrier structure

The rejected full-capability carrier is repaired correctly:

- persistent arenas use private four-byte local ordinals;
- public IDs carry the complete 32-byte `Revision` plus a private ordinal;
- same-index foreign-revision capabilities reject before every public lookup;
- identical deterministic rebuilds interoperate;
- edits invalidate old capabilities, while restoring identical path and bytes
  restores their authority;
- `NodeRemap` accepts only its `from_revision` and brands results with its
  `to_revision`;
- local ordinal types, constructors, accessors, capability fields, naked
  capability serialization, and sealed-carrier serialization remain
  inaccessible externally;
- crate-level and independent `-D unnameable-types` gates prove the public
  view API is nameable;
- representation laws retain the pre-branding carrier-size ceilings.

The rejected `3fbfc1b3` full-capability epoch is not reused as authority.
`bbnf.analysis-facts.v2` is retired with that epoch. The final carrier uses v3
facts and v5 complete replay. Earlier `3faf768e` and `00967fb5` subjects occur
only in qualified wire-lineage evidence, not as admitted implementation
identities.

## Analysis and type semantics

The final implementation provides:

- dependency-first deterministic SCC execution with stable ordinal
  tie-breaking;
- one pass for acyclic components and bounded monotone fixed points for
  recursive components;
- deterministic work charging before terminal/regex initialization,
  traversal, component scheduling, and component evaluation;
- a three-state minimum-consumption lattice distinguishing finite values,
  empty language, and overflow;
- typed `MinimumConsumptionOverflow` rejection at a deterministic direct
  origin, including masked non-entry descendants;
- exact first-order structural types for products, sums, option, repetition,
  nominal records, transforms, and finite named recursive binders;
- structural occurs-check rejection outside `Named(RuleId)` boundaries;
- an executable certificate for every node derivation and an independent
  verifier for syntax correspondence, bounds, field ownership/order,
  transform identity, canonical rule binders, and arena reachability;
- deterministic constraint/discovery permutation laws and full-invalidation
  edit equality.

An independent 4,002-rule acyclic-chain probe reproduced 4,002 iterations,
28,014 work units, exact-cap acceptance, and one-unit-under typed rejection.

## Replay and evidence

All ten stage/aggregate packets are deterministic and revision-bound. The
complete `bbnf.compiled-module.v5` packet is a trusted inverse by source
recompilation and exact comparison of options, every stage carrier, generated
source, generated manifest, revision, and canonical bytes. Individual stage
packets remain deterministic receipts and are not represented as standalone
trusted deserializers.

Independent validation reproduced:

- Draft 2020-12 meta-schema acceptance;
- a valid real recursive nominal-record packet;
- packet size 39,934 bytes;
- packet SHA-256
  `2521dc79b97d094f1ebd5ab086acccab737895a516ab2f5d5e247d20e68b4d56`;
- analysis-facts SHA-256
  `1c833e8325d2ed036c0a19af5cfcb81426696ce625df6135353b55277fff6eca`;
- 12 node facts, 4 rule facts, 4 component passes, and 78 work units.

All 59 structured path/digest bindings, 33 replay-table file digests, three
grammar digests, and all six artifact hashes resolve against the exact frozen
trees.

## Generated control and paired kernel

The selected generated backend is direct static Rust control over literal,
regex, sequence, choice, repetition, records, transforms, and EOF. It contains
no runtime grammar graph or host callback path.

The paired parse-that kernel supplies consuming `ModuleResult`, borrowed input
provenance, transactional cursor and sink rollback, allocation-free but
transactionally counted `NoSink`, typed `InputBytes`, `Nesting`, `Events`, and
`Work` limits, exact `parse_that = 0.4.1`, and compile-time
`KERNEL_ABI == 2`.

Generated, recursive, and nominal-record controls establish exact event order,
complete-consumption behavior, source identity, field identities, rollback,
mode invariance, logical-event accounting, and typed faults.

## Independently replayed gates

BBNF passed formatting; 57 locked workspace/all-target/all-feature tests; five
identity-opacity compile-fail cases; strict Clippy; warning-denying rustdoc;
scoped wasm32 library checking; public nameability; 8 generated-control, 5
recursive-control, 4 record-control, and 3 scalar-profile-control tests; strict
Clippy for every control; and `git diff --check`.

parse-that passed its locked workspace/all-feature/all-target suite, 32 regex
laws, 48 kernel laws, the serialized allocation law, benchmark compilation,
strict Clippy, warning-denying rustdoc, scoped wasm32 for `parse_that` and
`bbnf-regex`, and 7 runtime-bridge laws with strict Clippy.

Both packages verify reproducibly:

- `parse_that-0.4.1.crate`: 15,270 bytes, SHA-256
  `76f1c1f107ea2125cb787e99c1f69c41a4acfb1588c55771f8218899c5846f94`;
- `bbnf-regex-0.1.0.crate`: 82,832 bytes, SHA-256
  `2e5e85ffb8f198665414b5d5744baa47cb3d1b625438684ab4ec1e077294a50b`.

All detached clones remained clean.

## Profile disposition and residual limits

The scalar classifier is `REJECT`, `STANDALONE_NONPRODUCTION_PROTOTYPE`, and
`PROHIBITED` from promotion. Its long-scalar improvements do not override the
predeclared rejection caused by whitespace/punctuation and compact-input
regressions.

Explicit W2 scope boundaries are:

- no HM/generalized inference, row types, effects, or host-transform typing;
- no incremental compiler or edit-preserving cache;
- no standalone trusted inverse for isolated stage packets;
- no tape, DOM, path, direct-to-struct, or materialized typed sink;
- no universal grammar-coverage or totality-cutover claim;
- no finalized Rust/ASM backend or SOTA-performance claim;
- no promotion of the rejected scalar classifier;
- no full-workspace wasm claim because host-only mimalloc benchmark
  dependencies are not wasm-compatible; selected libraries and the runtime
  bridge pass their scoped gates;
- `cargo package -p bbnf` remains `BLOCKED_EXTERNAL_REGISTRY` because
  `bbnf-regex 0.1.0` is unpublished, while the sibling package verifies.

## Authorization

W2 may receive a durable admission receipt selecting the exact identities
above. That admission must atomically replace the stale pre-admission
`IN_PROGRESS_BORN_RED` state, update the obligation and evidence ledgers and
authority epoch, and freeze the W2 receipt set.

Only after that durable admission commit may W3 pre-freeze work be authorized.
The hostile-review commit alone does not bypass the state/authority update.
