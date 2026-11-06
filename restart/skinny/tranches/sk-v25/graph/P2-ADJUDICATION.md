# Architecture exporter P2 — fresh adjudication and supersession

Status: **RED; 0 of the 12 P2 contract clauses close. No P2 artifact may
authorize graph-directed source movement.**

This is the third, independent ruling required by the SK-V25 apotheosis
protocol. It supersedes `P2-REVIEW-A.md` and `P2-REVIEW-B.md` as verdict
authority without editing or erasing either review. The reviews remain claim
banks. `P1-ADJUDICATION.md` remains the authority for what P2 had to prove.

## Model-truth record

- Declared model: `gpt-5.6-sol`; declared reasoning effort: `xhigh`.
- This adjudicator did not delegate.
- The agent runtime exposed no readable recorded-model metadata endpoint. The
  parent must compare this declaration with the recorded `turn_context` before
  accepting the adjudication. Until then, recorded-model verification is
  **UNPROVEN**, not silently GREEN.

## Artifact discipline and independent snapshot

The repository was dirty and moving during both reviews and this adjudication.
Review A's `15,669/28,314/8,924`, Review B's `15,675/28,320/8,929`, and the
fresh counts below are therefore different observations, not contradictory
facts. An exact count is retained only with its artifact hash.

The exporter implementation and architecture-test files inspected for this
ruling had aggregate SHA-256
`8a16eb863f01c9832bdf408d9206fd0b7a18116d14ec51a41f71205506b074ca`.
That external fingerprint is not the source-state manifest required from the
exporter: it omits the analyzed constellation inputs, toolchains, configs,
locks, and resolver versions.

Two fresh same-root exports of the pre-adjudication-file tree both exited 1
with the same 16 required obligation instances and were byte-identical:

```text
graph.json        2781eb3e8551d4404d7fee1a16b116a5f4e5f873e97104bbb3f484d50538691a
projections.json  82a309d01e962301c3b8796a0205661bc3dbbe23637b3a6cf07f515e0d18d53f
inventory.json    e8208d25677353cf74422313261395075b1c3e1a4b75bdecb8525728c418de68
obligations.json  bb6ffe051bf9acc8cfebaacd10962884c0c3b03ab7106b8fb49d8c21b3bdb31f
graph.dot         df5fdb9bc9e35c8e89e2dc46a3a6608be5fda6a4d0d171f5f27e5bfc2176f602
REPORT.md         9b8cb4f3b7834468ec25876b69ccf9e6fe35360fc59420d4729d668563984206

nodes=15,699 edges=28,344 inventory=8,954
mapped=8,882 excluded=72 obligations=16
```

Those are artifact-bound machine facts only. The P2 schema has no aggregate
content manifest, so the artifact does not prove which complete moving source
snapshot it describes. Adding this adjudication file itself changes the
inventory on the next run without making the earlier artifact false.

### The load-bearing distinction

The SCC implementation is structurally correct for the edge set it is given.
That does **not** make the selected edges semantically correct. An independent
Kosaraju replay matched every emitted SCC partition and cyclic flag, retained
every node, replayed every emitted witness step against a selected edge, and
reconciled the internal-plus-outgoing shard partition. The same artifact has
251 Rust cyclic SCCs, of which 214 are singleton self-cycles manufactured in
large part by false Rust attribution. Structural SCC correctness survives;
semantic Rust/frontend cycle claims do not.

## Review A — numbered claim rulings

Each row was reproduced independently. Composite claims are narrowed where
the review's wording reached beyond the proof.

Bare anchors in the two ruling tables resolve exactly as follows:
`graph.rs`, `cargo.rs`, `rust.rs`, `inventory.rs`, `frontend.rs`, `mod.rs`, and
`output.rs` are under `xtask/src/architecture/`; `frontend.mjs` is
`xtask/architecture/frontend.mjs`.

| Claim | Ruling | Independent proof and exact anchor |
|---|---|---|
| **A1 — Rust SCCs are computed over false edges** | **CORRECT, with target-wording correction.** | `rust.rs:147-153`, `203-217`, and `254-263` recursively visit inline bodies once through every ancestor and again as their own modules. A rustc-valid disposable fixture emitted false `crate -> dependency` and `inline_outer -> dependency` edges in addition to the real `inline_inner -> dependency`, plus a false `crate -> crate` self-edge for an inner `use super::Root`. Thus the semantic origin was attributed to ancestors; it did not prove two different targets for one correctly attributed inner edge. Independent replay found 214 singleton self-cycles among 251 Rust cycles. The sole TestOverlay cycle is a root self-edge for `super::RefExpr`, also false attribution. |
| **A2 — cfg-disabled modules emit false missing-module RED** | **CORRECT.** | Module discovery happens before any cfg evaluation (`rust.rs:147-153`, `168-243`); cfg is only emitted as a bounded edge (`568-583`). `#[cfg(any())] mod absent;` compiled under rustc but produced `RUST_MODULE_FILE_MISSING`. |
| **A3 — Rust provenance is not auditable** | **CORRECT.** | All 6,758 Rust-projection edges in the fresh artifact have `source.kind=synthesized`; `Edge::new` supplies that default (`graph.rs:218-231`) and Rust collection never calls `Edge::at`. There are 56 global `cargo-crate:<name>` nodes (`rust.rs:319-331`) and zero exact Cargo-resolution edges joining them to package/version/source/context identities. |
| **A4 — Cargo resolution contexts collapse** | **CORRECT.** | Root and skinny `cargo metadata --locked` returned 1,281 + 392 dep-kind records and 812 + 253 feature activations; the unioned graph contains 1,494 resolved dependency edges and 907 feature edges. `collect` unions four invocations into one graph (`cargo.rs:37-88`), identities omit invocation/lock/target/feature/profile context (`399-419`), and canonicalization deduplicates equal edges (`graph.rs:421-422`). All 189 declarations match the successful root + skinny raw count of 128 + 61, but their IDs embed vector indices (`cargo.rs:271-305`). No Cargo package node contains a lock checksum. |
| **A5 — locked failure evidence loses the Cargo error** | **CORRECT.** | Both wasm and parse-that metadata calls fail because `--locked` would require lockfile changes. Manual commands report that causal error. The graph obligations retain only `cargo metadata <manifest>` because `collect_scope` adds context and `collect` serializes only `error.to_string()` (`cargo.rs:71-85`, `120-121`), discarding the diagnostic chain. |
| **A6 — inventory and semantic scopes disagree** | **CORRECT.** | Cargo declares four Repository nodes (`bbnf`, `bbnf-skinny`, `bbnf-wasm`, `parse-that`) at `cargo.rs:42-67,103-117`; physical inventory declares two repositories at `inventory.rs:12-23`. The fresh artifact has 195 unique semantic paths absent from physical inventory: 190 `bbnf-skinny`, one `bbnf-wasm`, one `parse-that`, and three `bbnf`. Across repeated semantic faces those are 356 node records. `bbnf:docs`, `bbnf:grammar`, and `bbnf:playground/src` are directory paths fabricated as `source_artifact` nodes by `allowDirectory` plus `addFile` (`frontend.mjs:147-150`, `321-335`). |
| **A7 — frontend scope is incomplete** | **CORRECT.** | The helper hardcodes only playground, three wasm output packages, and parse-that/typescript (`frontend.mjs:9-16`). Root `package.json` and the real `extension/package.json`, TS source, configs, and built output are generic inventory leaves. Tsconfigs are parsed (`94-105`), but there is no Vite/Playwright semantic loader and the graph contains zero `Configures` edges. E2E, Playwright, and Vite imports are classified as ordinary runtime edges, not TestOverlay/configuration facts. |
| **A8 — Vite, glob, worker, and CSS semantics are unsound** | **CORRECT, with exact CSS correction.** | A disposable configured fixture emitted zero UNKNOWNs while `~/target` became bounded `npm:~`; `@docs/**/*.md` matched the nested file but omitted the root-level file; `?worker` was marked exact; and `@import url("./base.css") layer(theme)` emitted both an exact `CssImport` and an exact `CssAsset` while losing layer semantics. The causes are hardcoded aliases (`frontend.mjs:338-349`), a nonstandard glob regex (`304-318,461-463`), forced worker relabeling (`263-268`), and regex CSS parsing (`226-242`). It is more precise to say the CSS statement is double-classified than merely “a generic asset.” |
| **A9 — Vue locations use block-relative offsets as file offsets** | **CORRECT.** | `analyzeVue` passes `block.content` to a new AST but `source()` applies its offsets to the full file (`frontend.mjs:47-52,174-186,213-223`). A physical line-4 import in a Vue script block was emitted at line 1. Containment locations that use SFC block offsets are not implicated by this specific defect. |
| **A10 — projection and shard hashes omit semantics** | **CORRECT.** | Projection hashes include only component members and selected endpoints (`graph.rs:676-690`); shard hashes include members and internal endpoints only (`627-642`). Changing `RustUse` to `RustReexport` retained projection hash `012f7ed11be11845` and shard hash `089bc907b544c769`. Removing the only outgoing cut changed the cut array from one edge to zero while retaining shard hash `eec85fdb716d6486`. SCC IDs hash only members and are not projection-qualified (`542-545`). |
| **A11 — stable-path/privacy validation is partial** | **CORRECT.** | Validation examines only `Node.path` (`graph.rs:452-460`). A graph with absolute node ID, label, attribute, and edge source canonicalized successfully. An external path package copied to two roots produced different opaque IDs because `package_identity` hashes an absolute Cargo package ID (`cargo.rs:409-419`). An analyzer call through `/tmp` failed because the file was canonicalized to `/private/tmp` while the supplied root was not (`rust.rs:110-117`). The current artifact's absence of private literals is a useful narrow fact, not proof of the validator. |
| **A12 — fail-closed evidence is incomplete and non-atomic** | **CORRECT.** | The normal command writes evidence then exits nonzero for required obligations (`mod.rs:15-32`). Canonicalization failure has a raw-failure writer, but Cargo/Rust/frontend/inventory errors propagate before that handler (`mod.rs:13-19`). Running the built binary with `node` absent failed in the frontend collector and left zero evidence files. Success files are overwritten sequentially with `std::fs::write` (`output.rs:9-38`), so neither success nor failure publication is atomic. |

Review A's engineering-coverage percentage is **rejected**: it has no defined
denominator or reproducible weighting. Its statement that no contract clause
closed is confirmed below.

## Review B — numbered claim rulings

| Claim | Ruling | Independent proof and exact anchor |
|---|---|---|
| **B1 — Rust has false exact edges and silent supported omissions** | **CORRECT.** | The A1 fixture reproduced ancestor duplication. An invalid `crate::dependency::Missing` path was truncated to the deepest existing `dependency` module and labeled exact because `resolve_use` searches candidates in reverse (`rust.rs:393-405`). A valid `extern crate std as renamed_std; use renamed_std::...` produced neither edge nor obligation; external qualified paths fail `starts_internal` and are ignored (`264-283,408-412`). An external child module inside an inline module compiled at `outer/child.rs` but P2 searched the wrong base (`607-629`) and emitted missing-module RED. The cfg-disabled module failed as A2. A standalone `#[cfg(test)] use` emitted shipping `RustUse` because edge kind depends on module-wide `testing`, not item cfg (`290-309`). External-module cfg is attached to the parent at `221-237`. Every observed Rust semantic location was synthesized. |
| **B2 — frontend collection mixes false positives with silence** | **CORRECT.** | One 22-node/18-edge disposable fixture emitted zero UNKNOWNs while reproducing all listed failures: Vite alias as npm; type-only reexport as runtime; computed import and `require` absent; Vue `script src` absent; non-router object emitted as route; WASM name in a JS comment emitted as glue load; commented CSS emitted exact; `@import url` double-emitted; `?url` emitted ordinary runtime; and no frontend TestOverlay edge. Anchors are `frontend.mjs:131-171`, `174-223`, `226-242`, `263-279`, and `351-366`. Current E2E/Playwright/Vite imports are runtime facts, and Vite `new URL` aliases manufacture the three directory nodes named in A6. |
| **B3 — inventory and identity disagree on physical scope** | **CORRECT.** | A6 proves the split faces. `NodeKind` has no Product or documentation-authority identity (`graph.rs:7-41`); no declarative constellation manifest exists beyond hardcoded arrays. `validate_inventory` checks each row independently but not uniqueness (`graph.rs:483-503`): two rows for one path mapped to different nodes returned `Ok(())`. |
| **B4 — relocation and privacy are unsound** | **CORRECT.** | The two relocated external path-package fixture IDs were `cargo-external:outside@0.1.0#d879693fe7191769` and `...#5afdf45af8673b76`. The absolute source is hidden behind FNV rather than removed. The absolute-field validation fixture returned `Ok(())`; see A11 anchors. |
| **B5 — Cargo is exact only inside one collapsed context** | **CORRECT, narrowly.** | Successful metadata does retain same-name versions, aliases, dep kind, target expression, and activated feature names; the same-name fixture remains valid. It does not retain the identity of the metadata invocation, lock, target/feature/profile context, feature definitions as edges, checksum/lock record, or manifest span. Root/skinny union counts reproduce A4; wasm/parse-that failure and causal-message loss reproduce A5. “Exact” must be limited to fields copied from one successful metadata result before context-destroying union. |
| **B6 — projection and shard hashes omit semantic content** | **CORRECT.** | The A10 mutation probes reproduce the unchanged hashes. Empty Call and GeneratedProvenance projections both hash to `aa90c3c501f3fdd9`. Source, resolver, confidence, attributes, projection spec, edge kind, and typed cut roles are absent from the hash inputs. |
| **B7 — cycles and shards do not meet W1/W5 dynamic partitioning** | **CORRECT, with one correction.** | Every projection retains every node and independent SCC replay matched. Witnesses are node paths, not typed edge identities, and only one cycle is returned (`graph.rs:906-946`), never a basis. Shards are one raw SCC each (`608-653`), with no articulation/fan/ownership/feature partition or boundary quotient. Fresh maxima are 23 Rust nodes/160 internal edges and 29 frontend nodes/152 internal edges. The ten-member Rust SCC's sole witness is a self-loop and does not evidence its multi-member cycle. Incoming cut references repeating the outgoing owner's edge is an expected incidence view, not itself a partition violation; the defect is that incoming/cut semantics are not hashed or validated. |
| **B8 — fail-closed behavior stops at top-level RED** | **CORRECT.** | Conflicting nodes, dangling endpoints, and absent inventory targets fail. But the independent graph probes show `Unknown` edge with zero required failures, ambiguous inventory `Ok(())`, and absolute non-path fields `Ok(())`. `ProjectionSpec.required_total` is serialized but never enforced (`graph.rs:331-338,505-700`). Early helper failure left no evidence; sequential output is non-atomic. Missing visitors remain silent. |
| **B9 — generated/WASM/call/docs/ownership planes are absent or skeletal** | **CORRECT.** | The fresh graph has zero `GeneratedFrom`, `Generates`, or `Calls` edges. Five bounded `WasmGlueLoads` edges lack producer/config/input/hash/stale/copy lineage. Documentation has no typed node or authority/supersession edge. Worktree census is 79 and matches 78 bbnf + one parse-that, but status ignores untracked files and maps status-command failure to `dirty=false` (`inventory.rs:213-226`); IDs use signature hash plus occurrence (`204-212`), and `source_state` hashes only signature plus the dirty boolean (`227-235`). There is no nominated-product projection. |

## P2 contract disposition

| # | Contract clause | Ruling |
|---:|---|---|
| 1 | Closed typed schema | **NOT CLOSED.** Closed Rust enums for current node/edge/class/projection/confidence/obligation kinds are real, and inconsistent edge class is rejected. Product, consumer/item, phase, condition, Cargo/Rust/frontend context, documentation authority, and typed attribute identities are absent; arbitrary string maps remain load-bearing. |
| 2 | Stable identity | **NOT CLOSED.** Many repository-relative IDs and current-artifact privacy are real. External path identity is absolute-derived, physical scopes split, worktrees are occurrence identities, and all serialized string fields are not validated. |
| 3 | Complete inventory and closure | **NOT CLOSED.** The fresh artifact has 15,699 unique node IDs, zero dangling endpoints, 8,882 mapped rows, 72 exclusions, zero absent mappings, and no duplicate inventory path in that artifact. There is no declared input manifest, file/directory type check, unique-path invariant, face linkage, or semantic/inventory scope agreement. |
| 4 | Named typed projections | **NOT CLOSED.** Ten named specs retain singleton nodes, but membership is hardcoded, `required_total` is inert, and type/config/dynamic-load/docs applicability is incomplete. |
| 5 | Exact Cargo resolution | **NOT CLOSED.** Same-name packages and successful metadata fields survive; invocation/lock/target/feature/profile identity, checksums, definitions, spans, external Rust linkage, and two locked scopes do not. |
| 6 | Bounded Rust semantics | **NOT CLOSED.** Repeated `super` positive tests pass, but ancestor attribution, cfg, external nested modules, renamed externs, qualified omissions, deepest-prefix guessing, item-test classification, and spans contradict the clause. |
| 7 | Frontend/build plane | **NOT CLOSED.** Package exports and several syntax forms exist, but application discovery, configured Vite/Playwright semantics, Vue `src`, type-only export, require/dynamic boundaries, tests, aliases, globs, queries, and routes are unsound or silent. |
| 8 | CSS/WASM/generated planes | **NOT CLOSED.** CSS and WASM filename edges exist; parsed layer semantics and complete producer-to-consumer provenance do not. GeneratedProvenance is empty. |
| 9 | Worktree plane | **NOT CLOSED.** Count/HEAD/branch/detached/dirty fields exist; nominated products, content hashes, untracked changes, command-error truth, and stable per-worktree identity do not. |
| 10 | Cycles and audit shards | **NOT CLOSED.** SCC partitioning is structurally correct, but semantic edges are false, witness evidence is not edge-complete, no cycle basis exists, and raw SCC shards do not implement dynamic W1/W5 clustering or semantic hashes. |
| 11 | Fail-closed truth | **NOT CLOSED.** Required obligations make the full command nonzero, but Unknown, silent unsupported cases, ambiguous inventory, incomplete privacy, and pre-validation collector failures evade complete evidence. Publication is non-atomic. |
| 12 | Executable proof | **NOT CLOSED.** Same-root twins, a small relocation fixture, and a 1,000-order kernel permutation probe pass. Full two-repository relocation, locale, full-constellation permutations, source/toolchain/config binding, mutation locality, and comprehensive negative/full-repository gates are absent. |

Passing positive tests never upgrades a contradicted global clause. The result is
**0 of 12 clauses closed**.

## Existing obligation families

The fresh graph contains 16 required instances in 14 code families. They are
truthful reasons for the command to remain RED, but broad boundary prose does
not excuse a false exact edge or a silent omission.

| Existing family | Ruling |
|---|---|
| `ARTIFACT_BINDING_BOUNDARY` | **Retain.** Correctly admits missing aggregate source/toolchain/resolver binding. |
| `CALL_PLANE_BOUNDARY` | **Retain.** Correct; the plane has zero edges. It must become language/context-specific in P3. |
| `CARGO_METADATA_LOCKED_FAILED` (wasm, parse-that) | **Retain failure, repair evidence.** The failures are real; actionable Cargo causes and lock identity were dropped. |
| `CYCLE_BASIS_BOUNDARY` | **Retain and strengthen.** It admits no basis but omits typed edge identity, semantic-edge validity, and dynamic clustering. |
| `FRONTEND_SEMANTIC_BOUNDARY` | **Retain and split.** Its broad wording does not name the reproduced false exact edges and silent forms. |
| `GENERATED_PROVENANCE_BOUNDARY` | **Retain.** Correct; the projection is empty. |
| `GENERATED_PROVENANCE_INCOMPLETE` | **Retain.** The observed OUT_DIR include lacks generator/input/config/hash evidence. |
| `PERMUTATION_TOTALITY_BOUNDARY` | **Retain.** Minimal 1,000-order kernel replay is not a full-constellation permutation gate. |
| `RELOCATED_CONSTELLATION_BOUNDARY` | **Retain.** Full two-repository relocation is unproved and external path identity already fails a small relocation. |
| `RUST_CFG_MATRIX_BOUNDARY` | **Retain and split.** Matrix absence is real; false discovery and test/shipping attribution need site-specific obligations. |
| `RUST_MACRO_TRAIT_DYNAMIC_BOUNDARY` | **Retain and split.** It does not cover supported renamed externs, qualified paths, inline attribution, or deepest-prefix guessing. |
| `WASM_PRODUCER_MANIFEST_BOUNDARY` | **Retain.** Correct but must include package-copy/declaration/runtime/stale lineage. |
| `WORKER_TARGET_DYNAMIC` (two subjects) | **Retain.** The two dynamic workers are real; they do not cover worker-query misclassification elsewhere. |
| `WORKTREE_NOMINATION_BOUNDARY` | **Retain and strengthen.** Add content-complete dirty state, untracked files, stable worktree identity, and command-failure evidence. |

## Missing obligation families and born-RED P3 gates

Every family below must exist as an attributed, source-bound obligation and a
negative fixture. An UNKNOWN automatically creates required RED for every
projection that needs that fact. No shim, deepest-prefix guess, regex
approximation, or heuristic fallback may emit an exact edge.

1. **Constellation manifest and typed identities/contexts**

   - Missing families: `CONSTELLATION_MANIFEST_MISSING`,
     `PRODUCT_IDENTITY_MISSING`, `SEMANTIC_CONTEXT_UNTYPED`,
     `INVENTORY_SCOPE_FACE_MISMATCH`, `DOCUMENTATION_AUTHORITY_MISSING`.
   - Born RED: a machine-readable manifest must name the two physical repos,
     bbnf root/skinny/wasm workspaces, parse-that Rust workspace, frontend
     packages/products, package faces, docs authorities, exclusions, and
     nominated worktrees. Deleting, duplicating, or renaming any face must fail
     before collection.

2. **Unique physical inventory**

   - Missing families: `INVENTORY_PATH_AMBIGUOUS`,
     `INVENTORY_NODE_KIND_MISMATCH`, `INVENTORY_SEMANTIC_FACE_UNLINKED`.
   - Born RED: one `(repo, relative path, file kind)` record per physical item;
     duplicate mappings, directory-as-file nodes, missing semantic links, orphan
     faces, and split `bbnf`/`bbnf-skinny` identities fail exactly. Inventory
     count/hash must reconcile with graph and every applicable projection.

3. **Rust attribution, cfg, spans, and external ownership**

   - Missing families include `RUST_INLINE_MODULE_ATTRIBUTION_UNSOUND`,
     `RUST_SELF_PATH_FALSE_CYCLE`, `RUST_PATH_PREFIX_GUESS_REJECTED`,
     `RUST_EXTERN_RENAME_UNRESOLVED`, `RUST_CFG_DISCOVERY_UNEVALUATED`,
     `RUST_TEST_ATTRIBUTION_UNSOUND`, `SOURCE_SPAN_INCOMPLETE`, and
     `RUST_EXTERNAL_PACKAGE_UNLINKED`.
   - Born RED fixtures: nested inline uses, `self`, repeated `super`, invalid
     deep suffix, renamed extern, qualified path, external child in inline mod,
     cfg-disabled missing file, cfg-disjoint twins, item-level test use,
     `#[path]`, include/OUT_DIR, macro unknown, and real line/column spans.
     Exact-negative assertions must prove false extras are absent.

4. **Cargo contexts and locks**

   - Missing families: `CARGO_RESOLUTION_CONTEXT_COLLAPSED`,
     `CARGO_LOCK_CHECKSUM_UNBOUND`, `CARGO_PROFILE_TARGET_FEATURE_UNBOUND`,
     `CARGO_DECLARATION_ID_ORDER_DEPENDENT`, and
     `CARGO_FAILURE_CAUSE_DROPPED`.
   - Born RED: exact equality with each `cargo metadata.resolve` invocation for
     the root, skinny, wasm, and parse-that contexts; lock digest/checksum,
     source, rename, kind, target, activated features, feature definitions,
     profile, manifest span, and external Rust-crate join. Reorder declarations
     without changing identity; stale lock failure must preserve the Cargo
     diagnostic and leave every lock byte-identical.

5. **Configured frontend, Vue, Vite, CSS, and tests**

   - Missing families include `FRONTEND_APPLICATION_SCOPE_OMITTED`,
     `FRONTEND_TEST_OVERLAY_INCOMPLETE`, `VITE_ALIAS_RESOLUTION_UNSOUND`,
     `GLOB_SEMANTICS_UNSOUND`, `WORKER_QUERY_SEMANTICS_UNSOUND`,
     `TYPE_ONLY_REEXPORT_MISCLASSIFIED`, `DYNAMIC_IMPORT_BOUNDARY_SILENT`,
     `REQUIRE_BOUNDARY_SILENT`, `VUE_SCRIPT_SOURCE_OMITTED`,
     `VUE_BLOCK_SPAN_MISATTRIBUTED`, `ROUTE_CONTEXT_UNPROVEN`, and
     `CSS_IMPORT_CLASSIFICATION_UNSOUND`.
   - Born RED: discover root, extension, playground, wasm faces, and
     parse-that/typescript from manifests; build configured TS programs; load
     Vite aliases/plugins/export conditions and Playwright/E2E contexts; parse
     Vue script/template/style/src with file-correct spans; use standards-correct
     globs and parsed CSS imports/url/layers. Promote every B2 disposable
     counterexample and require exact-positive plus exact-negative edge sets.

6. **Semantic projection, component, and shard hashes**

   - Missing families: `PROJECTION_HASH_SEMANTICS_UNBOUND`,
     `SHARD_HASH_CUT_UNBOUND`, `COMPONENT_ID_PROJECTION_UNBOUND`, and
     `HASH_ALGORITHM_NONCRYPTOGRAPHIC`.
   - Born RED: SHA-256 over the versioned projection spec plus complete selected
     node and edge records; projection-qualified component identity; shard hash
     over owner/reference roles, internal and incoming/outgoing cut records,
     kind/class/source/resolver/confidence/attributes. Every one-field mutation
     must change exactly the owning hashes.

7. **Edge-complete SCC evidence and dynamic clusters**

   - Missing families: `CYCLE_EDGE_IDENTITY_INCOMPLETE`,
     `CYCLE_BASIS_INCOMPLETE`, `DYNAMIC_SHARD_PARTITION_ABSENT`, and
     `BOUNDARY_QUOTIENT_ABSENT`.
   - Born RED: each cycle step names a typed edge identity and replays from the
     artifact; a deterministic independent cycle basis is complete; isolated
     nodes remain; dynamic shards use SCC, articulation, fan-in/out, ownership,
     feature, generated lineage, package face, test reachability, and frontend
     feature. Shard union equals the projection, ownership intersections are
     empty, boundary references reconcile, and large clusters recursively emit
     a quotient rather than remaining raw SCCs.

8. **Generated, WASM, call, documentation, and worktree truth**

   - Missing families include `GENERATED_LINEAGE_ABSENT`,
     `WASM_PACKAGE_LINEAGE_INCOMPLETE`, `CALL_CONTEXT_BOUNDARY_UNATTRIBUTED`,
     `DOCUMENTATION_SUPERSESSION_UNMODELED`,
     `WORKTREE_CONTENT_HASH_INCOMPLETE`, `WORKTREE_STATUS_ERROR_MASKED`, and
     `PRODUCT_WORKTREE_UNNOMINATED`.
   - Born RED: producer -> input -> generator/config/command -> output hash ->
     declaration/runtime/package-copy/load edges; stale/missing artifacts invert;
     language-bounded call planes emit attributed UNKNOWN; docs authority and
     supersession are typed; every worktree has stable repo-local identity,
     HEAD/branch/detached state, tracked and untracked content hash, dirty facts,
     command diagnostics, and explicit product nomination.

9. **Atomic failure evidence**

   - Missing families: `COLLECTOR_FAILURE_EVIDENCE_INCOMPLETE`,
     `VALIDATION_FAILURE_EVIDENCE_INCOMPLETE`, and
     `EVIDENCE_PUBLICATION_NONATOMIC`.
   - Born RED: inject Cargo, Rust parse/walk, Node/helper/decode, inventory,
     validation, projection, and final-write failures. Every run atomically
     replaces one complete success or failure generation with run ID, source
     binding, typed stage, diagnostic chain, and no mixed old/new files.

10. **Relocation, mutation, permutation, and source-state binding**

    - Missing families: `PRIVACY_VALIDATOR_INCOMPLETE`,
      `EXTERNAL_PATH_ID_RELOCATION_UNSTABLE`, `SOURCE_STATE_MANIFEST_ABSENT`,
      `MUTATION_LOCALITY_UNPROVEN`, `LOCALE_STABILITY_UNPROVEN`, and the two
      existing relocation/permutation boundaries.
    - Born RED: validate every canonical string field; bind both repos' file
      content, worktrees, four Cargo contexts/locks, TS/Vite/Playwright configs,
      toolchains, resolver/helper versions, graph, projections, components, and
      shards. Run two full relocated constellations, locale variation, 1,000
      full-input orderings, and deletion/rename/cfg/alias/glob/CSS/WASM/worktree
      mutations. Expected hashes must change locally and all unaffected hashes
      must remain equal.

## Smallest coherent P3 truth kernel

P3 must be a source-bound truth kernel, not another breadth pass over guessed
edges.

1. **Manifest first.** Parse one versioned constellation manifest into typed
   Repository, Product, Workspace, PackageFace, Context, WorktreeNomination,
   DocumentationAuthority, and Exclusion identities. The two physical
   repositories own all semantic faces.
2. **Inventory once.** Build a unique typed physical inventory per repo. Semantic
   collectors refer to inventory identities or create an explicit virtual
   artifact with provenance; they never manufacture a directory-shaped file.
3. **Facts before graph.** Each collector emits `Exact(fact)` or
   `Unknown(obligation)` with typed context and real source span. No heuristic
   fallback edge exists. Cargo and configured frontend contexts are explicit;
   Rust exactness is compiler-backed or narrowly bounded with fixtures.
4. **Pure structural kernel.** Canonicalization, projection selection, SCC,
   condensation, cycle basis, dynamic clustering, and hashes consume immutable
   semantic facts. They cannot repair or promote collector confidence.
5. **One evidence transaction.** A complete source-state manifest and all output
   hashes are assembled in a generation directory, fsynced where required, and
   atomically published. Failure publishes a complete typed failure generation.

### Goldilocks source and test topology

The current 958-line `graph.rs`, 678-line `rust.rs`, 513-line `cargo.rs`,
473-line frontend helper, and 381-line positive-heavy integration test are too
coarse. P3 should split by invariant, not by individual enum or visitor:

```text
xtask/src/architecture/
  mod.rs                         orchestration only
  manifest.rs                    constellation schema + nomination
  schema/{identity,fact,obligation}.rs
  inventory/{filesystem,worktree}.rs
  collect/{cargo,rust,frontend,artifact}.rs
  project/{spec,scc,cluster}.rs
  evidence/{binding,validate,write}.rs

xtask/architecture/frontend/
  program.mjs                    package/config discovery + TS program
  vue.mjs                        SFC/template/style attribution
  css.mjs                        parsed CSS dependencies

xtask/tests/architecture/
  {schema,inventory,cargo,rust,frontend,artifact,project,evidence}.rs
xtask/tests/fixtures/architecture/
  {schema,inventory,cargo,rust,frontend,artifact,project,evidence}/...
```

This is enough separation to keep identity, semantic collection, structural
algorithms, and publication independently testable without file-per-function
sand. Tests remain outside production trees and source-isomorphic at the
responsibility level.

## Exact P2 evidence retained and rejected

Retain only these machine facts, with their scope attached:

- the current closed enum primitives and deterministic sort/dedup kernel;
- rejection of conflicting node records, dangling endpoints, inconsistent edge
  classes, and inventory mappings to absent nodes;
- singleton-preserving SCC computation and internal-plus-outgoing shard
  partitioning for the selected edges;
- the exact artifact hashes/counts above, zero endpoint/mapped-inventory gaps,
  current-artifact absence of absolute checkout/temp literals, and 79-worktree
  count, all bound only to those artifacts;
- successful root + skinny Cargo declaration counts of 128 + 61, exact
  same-name package/alias fixture behavior, and locked metadata not rewriting
  the stale fixture;
- `cargo test -p xtask`: 15 architecture tests plus 3 metadata tests passed;
- two same-root full exports were byte-identical and honestly exited 1 for 16
  required obligation instances;
- minimal 1,000-order graph-kernel permutations had zero mismatches;
- the reproduced negative probes and source anchors in this adjudication.

Reject as architecture truth:

- every current Rust/frontend SCC, reachability, cycle, cluster, fan metric, and
  movement recommendation derived from semantically unproved edges;
- review-era and fresh exact counts when detached from their graph hashes;
- current Cargo unions as target/feature/profile/lock-specific resolution;
- inventory completeness, worktree content truth, relocation stability,
  projection/shard semantic hashes, generated/WASM/call/docs completeness, and
  source-state binding;
- any inference from the passing positive tests that a plane or global clause is
  GREEN.

## Authorization and exact precondition for P3

No P2 graph, projection, SCC, shard, report, or review may authorize relocation,
deletion, colocation, module splitting, or ownership movement. It may only seed
P3 fixtures and identify collector sites to replace.

P3 production implementation may begin only after the parent verifies this
adjudicator's recorded model metadata and adopts this adjudication as the
superseding P2 contract. The first permissible P3 slice is gate-only: freeze the
versioned constellation manifest/source-state capture contract and land every
born-RED counterexample family above with exact-negative assertions against P2.
Only after those gates are demonstrably RED may collectors be replaced. No shim,
legacy alias, dual path, deepest-prefix edge, or heuristic fallback is allowed.

Graph-directed source movement remains forbidden until the P3 artifact is
atomically source-bound, all applicable required planes contain no UNKNOWN or
unsupported fact, every semantic and structural gate above is GREEN, every cycle
and shard is edge-complete and hash-bound, the full relocation/mutation/locale/
permutation bank passes, and two fresh hostile audits plus a fresh adjudicator
close with no unresolved finding.

## Verification record

```text
cargo test -p xtask
  architecture: 15 passed
  metadata_fail_closed: 3 passed

two cargo run -q -p xtask -- architecture --output <mktemp> runs
  status: 1 / 1
  six canonical outputs: byte-identical

independent SCC replay of all ten projections
  SCC partitions/cyclic flags: exact match
  bad witness steps: 0
  shard internal+outgoing partition errors: 0
  Rust singleton self-cycles: 214 of 251 cyclic SCCs

rustc-valid disposable Rust fixture + analyze_rust_fixture
  false ancestor edges and root self-cycle reproduced
  cfg-disabled and inline-external missing-module RED reproduced
  cfg(test) shipping edge reproduced
  invalid deep path exact-prefix edge reproduced
  renamed extern omission reproduced

disposable configured frontend fixture
  22 nodes / 18 edges / 0 UNKNOWN
  all B2 false-positive and silent-omission cases reproduced

Cargo metadata reconciliation
  declarations: root 128 + skinny 61 = graph 189
  dep kinds: root 1281 + skinny 392 -> graph 1494 after context collapse
  activations: root 812 + skinny 253 -> graph 907 after context collapse
  wasm and parse-that locked failures reproduced with causal Cargo diagnostics

hash/privacy/failure probes
  RustUse -> RustReexport: projection/shard hashes unchanged
  outgoing cut deletion: shard hash unchanged
  ambiguous inventory / absolute non-path fields / Unknown edge: accepted
  relocated external path package: identity changed
  /tmp -> /private/tmp alias: root mismatch failure
  node-helper launch failure: status 1, zero evidence files
  minimal kernel permutations: 1,000 runs, zero mismatches
```

Full two-repository relocation, locale variation, full-input 1,000-order
permutation, mutation-locality, and source-state/toolchain/config binding were
not established by P2 and remain explicitly RED/UNPROVEN. Stop here; P3 is not
implemented by this adjudication.
