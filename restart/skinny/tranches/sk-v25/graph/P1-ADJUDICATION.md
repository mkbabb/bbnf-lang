# Architecture exporter P1 — supersession adjudication

Status: **RED; P1 is a useful deterministic inventory prototype, not a total or
sound architecture graph.** This ruling supersedes both hostile reports. It does
not modify the exporter.

## Model-truth record

- Declared model: `gpt-5.6-sol`; declared reasoning effort: `xhigh`.
- The agent runtime exposed no readable `turn_context.model` field or equivalent
  metadata endpoint. Recorded-model verification is therefore **UNPROVEN**, not
  silently GREEN. The parent session must compare the recorded session metadata
  with this declaration before accepting the judgment.

## Independent snapshot

Two exports of the same working tree were byte-identical:

```text
cargo run -q -p xtask -- architecture --output /tmp/bbnf-arch-a.sOBYRE
cargo run -q -p xtask -- architecture --output /tmp/bbnf-arch-b.5DdcyW
cmp <a>/graph.json <b>/graph.json
cmp <a>/condensation.json <b>/condensation.json
```

```text
graph.json         873acc40aee008935c1523a1ec66f9a3b354ffa1ae34c59ea8b3046a0066905d
condensation.json  3a0ba60caf319a925eb6db93332d5e79f7241a623069e725dd9d86e111f40ec6
graph.dot          61db6ee631ac8f464f680f8cd4fe2b383c70b2140b230df2217e261be222b6a0
REPORT.md          b8f1ae1addde426e2bf33bbc9d450d2dccc72071f1eda7a1bfc7c7671fa88de6
nodes=3017 edges=10106 unresolved=0 SCCs=1880 cyclic-SCCs=66
```

The hostile reports' `3020/10108` counts are stale against this independently
observed tree, which was changing concurrently. Their determinism conclusion is
correct; those exact counts are not current facts.

`cargo test -p xtask --test architecture` passes 3/3. Those tests cover
sort/dedup, one excluded test edge, and one simple Rust module fixture only.

## Claim rulings

| Claim | Ruling | Proof |
|---|---|---|
| Same-root byte determinism | **CORRECT, narrowly** | All four artifacts had identical SHA-256 hashes across two clean output directories. Relocation determinism is false below. |
| Exact `3020/10108` inventory | **INCORRECT for the adjudicated snapshot** | Independent rerun produced `3017/10106`; count-only evidence must always bind a graph hash and source-state hash. |
| Every edge endpoint names a node | **CORRECT for this snapshot** | 3,014 distinct endpoints; zero absent from the 3,017-node set. This is endpoint closure, not inventory completeness. |
| Condensation covers every graph node | **INCORRECT** | SCC vertices are derived only from non-excluded edge endpoints (`graph.rs:139-159`). Only 2,312 of 3,017 nodes appear; **705 are dropped**: 295 Cargo targets, 281 Rust test modules, 49 inline test modules, 40 cfg nodes, 21 source-test witnesses, 6 Rust modules, 5 external crates, 4 repos, 3 TS modules, and 1 generated output. |
| Multi-`super` Rust imports receive false targets | **CORRECT** | A rustc-valid fixture with `crate::a::b::c -> use super::super::x::Thing` was exported as `c -> b`; an inline `c::d -> use super::super::super::x::Thing` was exported as `d -> c`. `resolve_use` consumes only the first `super` (`rust.rs:318-344`) and then accepts the deepest existing prefix. |
| Cargo emits 20 false edges | **CORRECT for non-development declarations** | A `semver::VersionReq` check over the artifact found exactly **20** non-dev `cargo_dependency` edges whose target version violates the edge's own requirement: wrong `rustc-hash` (3), `syn` (5), `toml` (2), skinny `thiserror` (9), and skinny `cssparser` (1). The cause is name-wide lockfile fan-out (`cargo.rs:112-166`) instead of Cargo's resolve graph. Development edges add more false positives. |
| Containment inflates cycle count `66 -> 27` | **CORRECT** | Recomputing SCCs with the current exclusions gives 66 cyclic components; additionally excluding `rust_mod` containment gives 27. The largest SCC shrinks from 158 to 10. Structural containment must not be silently mixed into dependency projections. |
| 705 nodes are lost from the SCC product | **CORRECT** | Direct set difference between `graph.nodes[].id` and all `condensation.components[].members`; breakdown above. |
| IDs are checkout-relative and relocation-stable | **INCORRECT** | 88 node IDs contain `/Users/...`, principally `included:` and `rust_source:` IDs (`rust.rs:354-373`); 1,445 node `path` fields are absolute. Same-machine determinism does not survive checkout relocation. |
| Rust use resolution is complete | **INCORRECT** | The valid fixture's function-local `use crate::a::x::Thing` and qualified type path emitted no edge. Only module-level `Item::Use` is visited (`rust.rs:229-240`); macros, qualified paths, block imports, trait/method resolution, and repeated `super` are not soundly resolved. Unsupported internal forms can be ignored instead of becoming RED. |
| cfg/feature/target projections are represented | **INCORRECT** | The graph has 40 `rust_cfg` nodes and 213 raw `cfg` edges, but zero feature nodes and no evaluated target/feature projection. Cargo uses `.no_deps()` and flattens declared normal/dev/build dependencies without activation, target, or feature resolution. |
| Frontend E2E/config/package coverage exists | **INCORRECT** | Zero graph nodes for `playground/e2e/playground.spec.ts`, `playwright.config.ts`, `vite.config.ts`, or any `package.json`. The analyzer seeds one tsconfig plus `.vue` files under `src` (`frontend.mjs:25-48`). |
| Vite/glob/worker semantics are represented | **INCORRECT** | `useDocs.ts` uses `import.meta.glob("@docs/**/*.md")`, yet the graph has zero `docs/` nodes. Vite aliases/plugins/assets are unvisited. `?worker` imports are reduced to ordinary external-package edges, losing worker and asset semantics. |
| CSS dependency/asset coverage exists | **INCORRECT** | `main.css` is a leaf node, but its `@import "./preset-bbnf.css"` produces neither the preset node nor an edge; CSS parsing is explicitly listed unsupported (`graph.rs:103-111`). |
| WASM delivery is represented | **INCORRECT** | There are zero `.wasm` nodes. The real generated package contains JS, declarations, package metadata, and a 3.6 MB binary, but resolution fabricates `frontend:playground/src/wasm/bbnf_wasm.ts`, a path that does not exist, by rewriting `.d.ts` to `.ts` (`frontend.mjs:167-172`). |
| Package export maps are represented | **INCORRECT** | Zero `package.json` nodes and no export-map edge kind. |
| Call graphs are represented | **INCORRECT** | Explicitly listed unsupported; no call-site/function plane exists. |
| Generated provenance is represented | **INCORRECT as a plane** | One `generated_by` placeholder edge exists for an `OUT_DIR` include, but generator, inputs, outputs, command/configuration, and artifact hashes are absent; the plane is explicitly unsupported. |
| Worktree topology is represented | **INCORRECT** | `git worktree list --porcelain` reports 78 worktrees; the graph contains none and explicitly lists the plane unsupported. |
| `unresolved=0` makes P1 GREEN | **INCORRECT** | Missing visitors do not emit unresolved records. Four omissions are declared only as passive strings, and E2E, Vite, glob, package, feature, and WASM omissions are not all represented there. The gate fails open. |
| Critic B's `24% convergence` | **UNPROVEN** | No reproducible denominator, weighted rubric, or artifact-bound calculation establishes this percentage. Its concrete examples reproduce, but the scalar score must not enter the fact ledger. |
| P2 is required before graph-driven restructuring | **CORRECT** | False edges, omitted nodes, mixed relations, unstable IDs, and absent cycle witnesses can all direct destructive moves incorrectly. P1 may inform work only with its limitations attached. |

## Minimum P2 contract

P2 is GREEN only if all clauses hold; partial support is an explicit RED/UNKNOWN,
never an omitted record.

1. **Closed typed schema.** Node and edge kinds are closed enums with relation
   classes (`containment`, `declared-dependency`, `resolved-build`, `runtime`,
   `type-only`, `test`, `configuration`, `asset`, `generated-provenance`,
   `ownership`, `call`). Conflicting duplicate IDs are errors, not lexicographic
   deduplication. Every edge records source location and resolver/provenance.
2. **Stable identity.** IDs are `<repo-scope>:<normalized-repo-relative-path>` plus
   a logical sub-identity where needed. No home, checkout, temp, or worktree
   absolute path enters an ID or canonical artifact. Copying the constellation to
   another root must produce byte-identical canonical output.
3. **Complete inventory and closure.** A declared input manifest inventories every
   in-scope source, test, manifest, config, asset, generated artifact, and worktree.
   Every inventory item maps to a node or a reasoned exclusion. Every edge endpoint
   exists. Every graph node appears in every applicable projection's SCC set,
   including isolated singleton nodes. Coverage sums and hashes must reconcile.
4. **Named typed projections.** At minimum emit separately versioned containment,
   Rust-module, Cargo-declared, Cargo-resolved-per-target/feature, frontend-build,
   runtime/delivery, test-overlay, generated-provenance, ownership, and call
   projections. Projection membership rules are artifacts; there is no anonymous
   hard-coded `excluded_edge_kinds` list.
5. **Exact Cargo resolution.** Consume `cargo metadata.resolve`, Cargo package IDs,
   dependency rename/kind, target cfg, activated features, and exact lock source.
   Distinguish declared from activated edges. Never join lock identities by name.
6. **Bounded Rust semantics.** Correct file/inline/`#[path]` modules and arbitrary
   `crate`/`self`/repeated-`super`; visit imports in all lexical scopes and qualified
   module paths. Represent reexports separately. Evaluate cfg per declared target
   and feature set. Macro/trait/method/dynamic resolution outside the chosen engine
   becomes an attributed UNKNOWN and RED for projections that require it—never a
   guessed ancestor edge.
7. **Frontend/build plane.** Load application, node, E2E, Playwright, and Vite
   configs; package manifests/export maps; Vue scripts/templates/styles; TS
   imports/reexports/dynamic imports; `import.meta.glob`; aliases; URL/worker/query
   imports; and generated declaration-to-runtime mappings. Resolver identity and
   config are recorded.
8. **CSS/WASM/generated planes.** Parse CSS `@import`, `url()`, layers, and Vue
   style sources. Model WASM glue JS, declarations, package exports, binary, Rust
   producer, wasm-bindgen command/config, and consumers. Generated artifacts carry
   generator, inputs, configuration, output hash, and stale/missing status.
9. **Worktree plane.** Inventory all worktrees with repo-relative stable scope,
   HEAD, branch/detached state, dirty status, and source-state hash. Never merge
   same-path nodes from different HEADs. Default product projections nominate one
   worktree per repo; census projections retain all 78.
10. **Cycles and audit shards.** Every cyclic SCC includes edge-complete witnesses
    and a deterministic cycle basis; containment cycles are reported only in the
    containment projection. Emit deterministic cluster shards with node/edge
    membership, incoming/outgoing cut edges, counts, and hashes. Shards must be a
    lossless, non-overlapping partition for the chosen projection.
11. **Fail-closed truth.** Unknown resolver outcomes, unsupported required planes,
    dangling endpoints, conflicting IDs, missing inventory items, stale generated
    outputs, and coverage mismatches make the command non-zero after writing the
    evidence artifacts. `unsupported_planes` cannot be informational-only.
12. **Executable proof.** Same-root rerun, relocated-root rerun, permutation,
    mutation, negative-fixture, and full-repository gates are byte- and
    meaning-stable. The report binds source-state hash, toolchain/config hashes,
    graph hash, projection hashes, and shard hashes.

Call-graph totality may be staged by language, but a plane cannot be declared
GREEN until its static/dynamic boundary and UNKNOWN policy are explicit. Consumer
count is never used as a vacuity proof.

## Ordered executable repair waves

### P2.W0 — schema, identity, inventory, and projection kernel

- Replace free-form kinds and silent ID dedup with closed types and conflict errors.
- Introduce stable scoped relative IDs, input manifests, named projection specs,
  singleton-preserving SCCs, endpoint/coverage reconciliation, and source spans.
- Fixtures: conflicting IDs, isolated nodes, relocation, separator normalization,
  projection membership, unknown-required-plane failure.
- Exit: two roots and 1,000 insertion permutations serialize identically; all
  negative fixtures fail closed with byte-stable diagnostics.

### P2.W1 — Cargo truth

- Emit declared and exact resolved graphs from Cargo package IDs; preserve
  normal/dev/build, rename, target cfg, source, checksum, and activated features.
- Mutation bank: two versions with the same name, renamed dependencies, optional
  features, target-only dependencies, patches, git sources, and path dependencies.
- Exit: zero requirement-violating resolved edges; graph edges equal
  `cargo metadata.resolve` for every declared projection.

### P2.W2 — Rust module and configuration truth

- Repair arbitrary repeated-`super`, lexical imports, qualified paths, reexports,
  includes, cfg/cfg_attr evaluation, and test overlays.
- Emit UNKNOWN for macro/trait/dynamic semantics beyond the chosen resolver.
- Exit: rustc-valid mutation fixtures resolve to exact modules; every injected
  invalid/unsupported form is an attributed RED, never a false edge or silence.

### P2.W3 — TS, Vue, Vite, package, and E2E truth

- Build from all relevant configs and package manifests; expand glob/alias/query,
  worker, URL, route, template-component, and style-source semantics.
- Exit: mutating any existing E2E, Vite alias, docs glob member, worker, export-map,
  or route edge changes exactly the expected shard and projection.

### P2.W4 — CSS, WASM, and generated provenance

- Parse CSS dependencies/assets; connect Rust producer -> generator/config/input ->
  JS/declaration/WASM/package outputs -> frontend consumers; hash all artifacts.
- Exit: stale/missing/mismatched WASM or generated output is RED; no phantom file
  node can be emitted.

### P2.W5 — worktree constellation

- Add all-worktree census and nominated-product projections without cross-HEAD
  identity collapse.
- Exit: observed worktree count reconciles with `git worktree list`; dirty/HEAD
  mutations affect only their scoped census shards.

### P2.W6 — call planes, cycle basis, and graph-cluster audit shards

- Add bounded Rust and frontend call planes with explicit UNKNOWN boundaries.
- Emit deterministic cycle bases and edge-complete, lossless cluster shards for
  each named projection; separate containment from dependency cycles.
- Exit: shard union equals the projection exactly, shard intersections are empty,
  all cycle witnesses replay, and every node/edge is routed to the three-view audit.

### P2.W7 — adversarial close

- Run same-root and relocated-root determinism; target/feature matrix; 1,000-order
  permutations; deletion/rename/alias/glob/CSS/WASM/worktree mutation bank; full
  workspace tests; and two consecutive clean totality audits.
- Exit: no required UNKNOWN, no unsupported required plane, no false/dangling edge,
  no omitted/duplicate inventory node, no unexplained artifact drift, and identical
  canonical hashes on the second clean pass. Only then may P2 drive destructive
  modularization.

## Reproduction notes

The adjudication used disposable probes under `/tmp`; no exporter source was
edited. Load-bearing checks were:

```text
cargo test -p xtask --test architecture
# 3 passed

# graph/condensation set reconciliation
nodes=3017 endpoints=3014 absent-endpoints=0 condensation-members=2312 dropped=705

# SCC projection replay
current exclusions: cyclic=66 max-SCC=158
plus rust_mod exclusion: cyclic=27 max-SCC=10

# semver check over non-dev Cargo edges
invalid_non_dev=20

# frontend and constellation census
docs=0 preset-css=0 vite=0 e2e=0 package-json=0 wasm-binary=0
generated_by=1 cargo-feature-nodes=0 worktrees=78

# valid Rust mutation fixture
rustc --crate-type lib /tmp/bbnf-rust-fixture/lib.rs --edition 2024
cargo run -q --manifest-path /tmp/bbnf-graph-probe/Cargo.toml
c -> b       for super::super::x::Thing          # false; expected a::x
c::d -> c    for super::super::super::x::Thing   # false; expected a::x
# function-local use and qualified type path emitted no edge
```

Canonical judgment: preserve P1 as a falsified prototype and test corpus; do not
certify its counts, SCCs, or reachability as architecture truth. P2 must supersede
it rather than patching its report around the omitted planes.
