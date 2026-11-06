# Graph P2 independent review A

Status: **RED; 43% estimated engineering coverage and 0/12 P1 certification
clauses closed**.

Reviewer: `/root/graph_p2_reviewer_a`, session
`019f76ff-f6af-7a12-9076-1e135324b62f`. Root verified the recorded
`turn_context` as `gpt-5.6-sol`, `xhigh`. The reviewer did not read any other P2
review, changed no repository file, and placed its disposable probes under `/tmp`.

This is one skeptical input, not the final verdict. Review B and a fresh
adjudicator must reproduce or overturn every numbered finding.

## Supported kernel facts

- P2 has closed Rust enums for node, edge, relation, projection, confidence, and
  obligation kinds. Canonicalization rejects conflicting IDs, dangling endpoints,
  inconsistent relation classes, and inventory mappings to absent nodes.
- The supplied evidence contains 15,669 unique nodes, 28,314 edges, and 8,924
  inventory records: 8,852 mapped and 72 excluded. No edge endpoint or mapped
  inventory node is absent from the graph. `graph.json` contains no absolute
  `/Users` or temporary evidence path.
- All ten projections retain all 15,669 nodes, including isolated nodes. An
  independent Kosaraju implementation reproduced every emitted SCC partition and
  cyclic flag, and every emitted witness step names a selected edge:

  | Projection | Components | Cyclic |
  |---|---:|---:|
  | containment | 15,669 | 0 |
  | rust_module | 15,540 | 251 |
  | cargo_declared | 15,669 | 0 |
  | cargo_resolved | 15,669 | 0 |
  | frontend_build | 15,619 | 6 |
  | runtime_delivery | 15,631 | 3 |
  | test_overlay | 15,669 | 1 |
  | generated_provenance | 15,669 | 0 |
  | ownership | 15,669 | 0 |
  | call | 15,669 | 0 |

- For every projection, internal plus outgoing edge indices are an exact,
  nonduplicated partition of selected indices; incoming cut indices also select
  real edges.
- The latest architecture test binary passes all 15 tests. Two same-root exports
  of the moving current tree were byte-identical. `cargo metadata --locked` did not
  change any of the four lockfiles during the review.
- A same-name Cargo fixture retained versions and aliases. Successful root and
  skinny declaration counts matched raw Cargo metadata (`128 + 61 = 189`). Git and
  exporter worktree census counts matched (`78` bbnf plus `1` parse-that).

These are structural facts about the produced graph. They do not prove that its
edges represent the source semantics.

## Numbered defects for adjudication

### A1 — Rust SCCs are computed over false edges

Of 251 reported Rust cyclic components, 214 are singleton self-cycles. Inline
module bodies are visited through the parent AST and again as their own module;
bare `self`, `super`, or an existing prefix is then admitted as a module edge. A
rustc-valid fixture attributed `use super::Root` from `inner` to both `crate` and
`crate::inner`. The sole TestOverlay cycle is the same false-parent attribution.

### A2 — cfg-disabled modules emit false missing-module RED

Discovery visits `Item::Mod` before cfg evaluation. The valid fixture
`#[cfg(any())] mod absent;` compiles, yet P2 emits
`RUST_MODULE_FILE_MISSING`. Cfg is merely attached as a bounded edge rather than
governing discovery.

### A3 — Rust edge provenance is not auditable

All 6,758 selected Rust-projection edges have synthesized rather than real source
spans. External crate identities are global name-only nodes, disconnected from an
exact Cargo package/version/source/context identity.

### A4 — Cargo resolution contexts collapse

External IDs and resolved edges omit metadata-invocation/lock/target/feature
context. Raw metadata contained 1,281 root plus 392 skinny dep-kind edges, while
the graph exposed 1,494 after cross-scope deduplication; activated feature counts
similarly collapsed from `812 + 253` to `907`. Package nodes omit lock checksums,
and declaration IDs contain metadata-vector indices, leaving reorder stability
unproved.

### A5 — locked failure evidence loses the error

The wasm and parse-that Rust metadata failures are real stale-lock failures, but
their obligations preserve only the command context because the diagnostic chain
is discarded. A consumer cannot distinguish the causal Cargo error from other
metadata failure.

### A6 — inventory and semantic scopes disagree

Inventory has two physical Git repositories, but Cargo emits four Repository
nodes. Skinny and wasm are workspace scopes in the bbnf repository, not
repositories. There are 195 semantic node paths without matching inventory paths:
190 skinny, one wasm, one parse-that, and three bbnf. Directory resolution also
fabricates `docs`, `grammar`, and `playground/src` as file nodes. Skinny physical
inventory and semantic faces coexist as disconnected duplicates.

### A7 — frontend scope is incomplete

The collector hardcodes playground, three wasm packages, and parse-that/typescript,
omitting the root JavaScript workspace and VS Code extension. Their manifests,
source, build, and config files remain generic leaves. Tsconfigs are read, but Vite
and Playwright configuration semantics are not; `Configures` edges and frontend
tests in TestOverlay are absent.

### A8 — Vite, glob, worker, and CSS semantics are unsound

A valid fixture demonstrated four errors: a configured alias became an npm
package; `@docs/**/*.md` omitted a root-level match; `?worker` was labeled exact;
and `@import url(...) layer(...)` became a generic asset with layer semantics
lost. Umbrella bounded obligations cannot make emitted false exact edges honest.

### A9 — Vue locations use block-relative offsets as file offsets

Script locations are calculated from the block contents but applied to the whole
Vue file. `App.vue` physical line 2 is reported as line 1; later blocks can drift
much further.

### A10 — projection and shard hashes omit semantics

Projection hashes omit the projection spec and edge kind/class/source/resolver/
confidence/attributes. Shard hashes omit edge kinds, metadata, and cut edges. A
probe changing `RustUse` to `RustReexport` retained both projection and shard
hashes. Component IDs are also not projection-qualified.

### A11 — stable-path/privacy validation is partial

Only `Node.path` is checked. Absolute data in IDs, labels, attributes, edge sources,
inventory paths, or obligation text is accepted. A macOS `/tmp` versus
`/private/tmp` fixture also showed that the analyzed file is canonicalized while
the supplied root is not.

### A12 — fail-closed evidence is incomplete and non-atomic

Required obligations exit nonzero after writing artifacts, and canonicalization
can write a raw failure artifact. Collector failures can propagate before that
handler and leave no evidence. Normal artifacts are written sequentially rather
than atomically, so a failure can expose a mixed generation.

## Cluster and totality findings

- The largest Rust SCC has 23 members and the next 18, but the dominant count is
  false singleton self-cycles. A ten-member SCC's selected witness is only a
  self-loop and does not demonstrate its multi-member cycle.
- The largest FrontendBuild and RuntimeDelivery SCC is the same 29-member generated
  `playground/dist/assets` cluster. The same unqualified component ID occurs in
  both projections. Source and generated output are mixed while the dedicated
  GeneratedProvenance projection is empty.
- The supplied artifact reports 15,669/28,314/8,924 while two later deterministic
  current-tree runs reported 15,673/28,318/8,927. With no aggregate source-state
  manifest, neither artifact can be tied to one complete moving-worktree snapshot.
- Full two-repository relocation, 1,000-order permutation, toolchain/config binding,
  mutation locality, and all-worktree content binding remain explicitly unproved.

## Missing required obligations

At minimum P2 omits or misleadingly subsumes these families:

- `RUST_INLINE_MODULE_ATTRIBUTION_UNSOUND`
- `RUST_SELF_PATH_FALSE_CYCLES`
- `CARGO_RESOLUTION_CONTEXT_COLLAPSED`
- `CARGO_LOCK_CHECKSUM_UNBOUND`
- `INVENTORY_SCOPE_FACE_MISMATCH`
- `FRONTEND_APPLICATION_SCOPE_OMITTED`
- `VITE_ALIAS_RESOLUTION_UNSOUND`
- `GLOB_SEMANTICS_UNSOUND`
- `CSS_IMPORT_CLASSIFICATION_UNSOUND`
- `FRONTEND_TEST_OVERLAY_INCOMPLETE`
- `SOURCE_SPAN_INCOMPLETE`
- `PROJECTION_HASH_SEMANTICS_UNBOUND`
- `PRIVACY_VALIDATOR_INCOMPLETE`
- `COLLECTOR_FAILURE_EVIDENCE_INCOMPLETE`

## Goldilocks ruling

Plane-level separation is directionally coherent, but `graph.rs` (958 lines),
`rust.rs` (678), `cargo.rs` (513), `frontend.mjs` (473), and the 381-line integration
test are too coarse for their distinct responsibilities. Existing tests emphasize
positive existence and sorting, not exact edge sets and absence of false edges.

## Reviewer A's smallest P3 repair contract

1. Unify physical inventory and semantic faces under the two real repositories;
   model Cargo subworkspaces as workspaces. Validate one inventory record per path,
   file/directory type, semantic linkage, and every canonical string field.
2. Put Cargo invocation/lock/target/feature, Rust target/cfg, and worktree context in
   identity. Bind lock checksum and exact Rust external crate-to-package linkage.
3. Stop parent traversal into inline bodies; resolve `self`/`super`/`crate` from the
   real module context without manufacturing self-edges; evaluate cfg before
   discovery; emit real spans and exact-negative fixtures.
4. Discover frontend packages from workspaces; cover root and extension; interpret
   tsconfig membership, Vite aliases/plugins, Playwright/E2E, package maps,
   standards-correct globs, worker queries, and parsed CSS imports/layers.
5. Cryptographically hash full projection specs and complete selected node/edge
   records; hash complete shards and cut edges; bind sources, locks, configs,
   toolchains, resolvers, and outputs in one source-state manifest.
6. Recompute SCCs only after semantic repair; qualify component identities by
   projection and use edge-indexed witnesses. Keep cycle-basis RED until complete.
7. Atomically replace success or failure evidence for every collector outcome. Gate
   the counterexamples above, all four locked scopes, current-root twins, full
   two-repo relocation, mutation locality, and the 1,000-order permutation test.

P3 may close only with zero unexplained false edges in these probes, named RED for
every still-bounded plane, and byte-reproducible evidence bound to one exact source
state.
