# Graph P2 independent review B

Status: **RED; 0/12 P2 clauses close**. Eleven clauses are contradicted by source
or disposable fixtures; executable proof is not established.

Reviewer: `/root/graph_p2_reviewer_b`, session
`019f7713-7e57-7e71-86f6-70615c246253`. Root verified recorded metadata as
`gpt-5.6-sol`, `xhigh`. The reviewer did not read review A, changed no repository
file, and placed probes under `/tmp/bbnf-p2-review-b-probes-019f7685`.

This is the second skeptical input, not final authority. A fresh adjudicator must
reproduce and rule every numbered finding from reviews A and B.

## Evidence snapshot

Two same-root exports each exited 1 with the same sixteen required obligations and
were byte-identical:

- 15,675 nodes, 28,320 edges;
- 8,858 mapped inventory entries plus 71 exclusions;
- 79 worktrees, matching 78 bbnf plus one parse-that;
- `graph.json` SHA-256
  `21c94c7091a00cdc01ce72dbbc1cdd65b6b94ef3efcd02fcf40be62b95b63df7`;
- `projections.json` SHA-256
  `be43ec19413e9d8574875b7c7174125de5b1be0aa415f5af2d6a64771cf5cc12`;
- internal report graph hash `cd89ecd374c11436`.

The architecture integration test passes 15/15. Successful canonical artifacts
contain no literal checkout or temporary path. These narrow facts do not establish
semantic soundness or source binding.

## Numbered findings for adjudication

### B1 — Rust has false exact edges and silent supported omissions

Inline bodies are recursively visited through ancestors and then analyzed again as
their own modules. A rustc-valid fixture emitted false `crate -> dependency` and
`crate::outer -> dependency` edges in addition to the expected
`crate::outer::inner -> dependency`. Renamed extern imports and qualified uses
emitted neither an edge nor an obligation. An invalid deep path was truncated to
its deepest existing prefix and labeled exact.

An external module nested in an inline module compiled under rustc but produced
`RUST_MODULE_FILE_MISSING`. All observed Rust semantic locations are synthesized;
external-module cfg is attached to the parent, and item-level `#[cfg(test)] use`
remains a shipping use.

### B2 — frontend collection mixes false positives with silence

A disposable TypeScript/Vue/Vite/CSS/WASM fixture emitted no UNKNOWNs while:

- a Vite alias became an npm package;
- a type-only reexport became a runtime reexport;
- computed dynamic import and `require()` disappeared;
- Vue `script src` disappeared;
- a route-shaped non-router object became a route;
- a WASM filename in a comment became a glue load;
- a commented CSS import became exact;
- `@import url(...)` became both import and asset;
- `?url` became an ordinary runtime import;
- no frontend-test classification edge appeared.

Actual E2E, Playwright, and Vite-config imports enter RuntimeDelivery as runtime
edges. Vite URL aliases also manufacture directory-shaped asset nodes. The
collector uses syntax-only TypeScript, regex CSS/WASM, and global route-shaped
object matching rather than configured program/build semantics.

### B3 — inventory and identity disagree on physical scope

Cargo models skinny and wasm as separate repository prefixes while inventory
models their files under the bbnf repository. The semantic prefixes own no
inventory/worktree records and at least 190 skinny paths have disconnected dual
identities. The same mismatch reaches manifests and ownership.

No declarative constellation/input manifest, Product identity, or documentation
authority/supersession relation exists. Validation also accepts two inventory rows
for one path mapped to different nodes (`ambiguous_inventory=Ok(())`).

### B4 — relocation and privacy are not sound

External path-package IDs hash Cargo package IDs containing absolute file URLs.
The visible node hides the absolute path behind FNV, so relocation changes identity
without showing the cause. Validation checks only `Node.path`; absolute node IDs,
labels, attributes, and edge sources canonicalize successfully.

### B5 — Cargo is exact only inside one collapsed context

Successful metadata retains exact same-name package versions, aliases, dependency
kind, target expression, and activated feature names. It does not preserve
invocation/lock/target/feature/profile context, feature definitions, checksums,
exact lock records, manifest spans, or stable declaration identity. Root and skinny
results are unioned and deduplicated. External Rust name-only crate nodes do not
join exact Cargo packages. Parse-that and wasm fail locked metadata, and their
obligations omit Cargo's actionable lockfile cause.

### B6 — projection and shard hashes omit semantic content

Shard hashes contain members plus internal endpoints only; projection hashes omit
edge kind and projection specification. They omit source, resolver, confidence,
attributes, and typed cut edges. A `RustUse` to `RustReexport` mutation kept both
hashes at `012f7ed11be11845`; deleting the only outgoing cut edge changed the cut
array without changing its shard hash. Empty Call and GeneratedProvenance
projections share a hash.

### B7 — cycles and shards do not meet dynamic W1/W5 partitioning

Every projection retains every node and SCC identity is deterministic. But cycle
witnesses are node paths rather than typed edge identities, only one witness is
emitted instead of a basis, incoming cut arrays duplicate outgoing ownership, and
validation ignores them. There is no articulation/fan/ownership/feature/boundary-
quotient partitioning; shards are raw SCCs, including oversized 160/114-edge Rust
and 29-node/152-edge frontend components plus tens of thousands of ungrouped
singletons.

### B8 — fail-closed behavior stops at the top-level RED

The full command writes normal evidence before its obligation exit, and basic node/
endpoint conflicts fail. Yet an explicit Unknown edge, ambiguous inventory, and
absolute identities/sources canonicalize with no required failure;
`required_total` is informational. Collector, helper, walk, and projection errors
can escape before evidence writing, and missing visitor cases remain silent.

### B9 — generated, WASM, call, documentation, and ownership planes are absent or skeletal

The fresh graph has zero `GeneratedFrom`, `Generates`, or `Calls` edges. Five
bounded WASM glue edges lack Rust producer, generator/config, content hash, stale
status, declaration/runtime linkage, or package-copy lineage. Documentation is
generic inventory only. Worktree count/HEAD/branch/dirty exist, but IDs depend on
hash-plus-occurrence order, untracked changes are excluded, command failure becomes
clean, and the source-state hash contains a signature plus boolean rather than
content. No nominated product projection exists.

## P2 clause disposition proposed by reviewer B

| Clause | Proposed ruling |
|---|---|
| closed typed schema | CONTRADICTED: key product/item/consumer/phase/condition and documentation identities absent; attributes remain open strings |
| stable identity | CONTRADICTED: scope duplication, absolute-derived hashes, worktree ordinals, partial privacy checks |
| inventory and closure | CONTRADICTED: endpoint closure is real, but no manifest, ambiguous rows pass, physical faces split |
| typed projections | CONTRADICTED: type/config/dynamic-load/docs applicability absent and membership hard-coded |
| exact Cargo | CONTRADICTED: contexts, profiles, definitions, checksums, spans, and two workspaces missing |
| bounded Rust | CONTRADICTED by valid and invalid rustc fixtures |
| frontend/build | CONTRADICTED by TS/Vue/Vite/test fixture |
| CSS/WASM/generated | CONTRADICTED |
| worktrees | CONTRADICTED beyond count |
| cycles/audit shards | CONTRADICTED |
| fail-closed truth | CONTRADICTED |
| executable proof | NOT ESTABLISHED: full relocation, locale, 1,000 permutations, comprehensive mutations, and full-repository gates absent |

Several P2.W0 primitives are real, but this is a deterministic RED extractor with
partial collectors rather than architecture authority.

## Obligation incompleteness

The sixteen obligations usefully admit call, cfg-matrix, generated provenance,
cycle basis, some frontend semantics, WASM producer manifests, worktree nomination/
content, aggregate binding, relocation, permutations, metadata failures, OUT_DIR,
and dynamic-worker limitations. They do not name the concrete schema/projection
omissions, split scope, absolute-derived identity, inventory ambiguity, Cargo
profile/checksum/feature loss, Rust false/omitted edges, frontend test/config
pollution, Vite/require/type-only/Vue/CSS/WASM failures, documentation authority,
semantic hashes, shard boundaries, Unknown acceptance, or early evidence loss.

## Goldilocks ruling

`graph.rs` is 958 lines and owns schema, validation, SCC, witnesses, projections,
shards, and hashing. `rust.rs` is 678 lines, `cargo.rs` 513,
`frontend.mjs` 473, and the 381-line integration test is a cross-plane monolith.
Positive fixture assertions ask only whether edge kinds exist and therefore admit
false extras. The required context/cfg/module/alias/provenance/Vite/test/privacy/
semantic-hash/locale/relocation mutation bank is largely absent.

## Reviewer B's smallest coherent revision

1. Reopen P2.W0 around a declarative constellation manifest for repository,
   product, workspace, package faces, exclusions, and nominated worktrees; unify
   skinny/wasm physical identity and explicitly scope external path packages.
2. Replace stringly identity/conditions with typed contexts. Enforce unique
   inventory paths and canonicality across every serialized field; every Unknown
   automatically creates a required attributed obligation.
3. Emit exact edges only with proved attribution and spans. Quarantine current
   Rust ancestor fallback and frontend regex guesses as site-specific RED rather
   than edges.
4. Use SHA-256 over projection spec plus complete selected node/edge records; hash
   node/internal/cut records with explicit shard owner/reference roles.
5. Split graph, Rust, frontend, and tests into source-isomorphic semantic modules.
6. Promote every disposable counterexample above as a permanent negative fixture
   before resuming Cargo W1, Rust W2, and frontend W3 breadth.

The next revision may remain RED, but its RED must be attributable, source-bound,
and safe to use for the next graph-directed move.
