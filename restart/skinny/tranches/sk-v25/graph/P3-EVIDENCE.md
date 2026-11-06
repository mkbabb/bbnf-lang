# Architecture exporter P3 — truth-kernel foundation evidence

Status: **foundation gates GREEN; complete architecture truth remains RED.**

The P3 exporter now has a typed, source-bound, fail-closed foundation. Its
complete run deliberately exits nonzero because 415 projection-qualified
required facts remain `Unknown`. This artifact does not authorize relocation,
deletion, colocation, module splitting, ownership movement, or any other
graph-directed source change.

Declared implementation model: `gpt-5.6-sol`; reasoning effort: `xhigh`.
The implementing agent did not delegate.

## Supersession and clean break

`P2-ADJUDICATION.md` remains the verdict authority. The pre-replacement P2
snapshot and exact-negative bank are frozen in `P3-BASELINE.md`:

```text
P2 source checksum-ledger SHA-256
c349aefa215cf9c9a7a79e46e0ccfe62458592b9d346ceb0a9fd456ee400d602

P2 bank combined-output SHA-256
bbfc985a84a04d2b92a4890753e8df1d51b4e415a19cf37affd1255cfa83ca30

P2 bank result
0 passed; 10 failed
```

P3 removes the P2 collector/graph/output modules and the single heuristic
frontend helper. There is no legacy alias, dual path, deepest-prefix exact
edge, or heuristic compatibility collector. The replacement topology is:

```text
xtask/architecture/constellation-v3.toml
xtask/architecture/frontend/{program,vue,css}.mjs
xtask/src/architecture/
  manifest.rs
  schema/{identity,fact,obligation}.rs
  inventory/{filesystem,worktree}.rs
  collect/{cargo,rust,frontend,artifact}.rs
  project/{spec,scc,cluster}.rs
  evidence/{binding,validate,write}.rs
xtask/tests/architecture.rs
xtask/tests/architecture/{schema,inventory,cargo,rust,frontend,artifact,project,evidence}.rs
xtask/tests/architecture_p3_counterexamples.rs
```

The implementation ledger immediately before this receipt was added covers 56
files and 5,548 lines, including the baseline record, entrypoints, manifest,
helpers, source-isomorphic tests, and P3 frontend fixtures:

```text
eb7c8829a240025585418a566af062b3a8b699faab1c7d0881afa86a490f334b
```

The receipt itself is necessarily an observer write after the bound twin
snapshot. The twin generation binds the exact implementation and complete
two-repository census that it analyzed; this later prose file is not claimed
to be part of that earlier source binding.

## Foundation that is now exact

- One versioned manifest declares two physical repositories, two products,
  four Cargo workspaces and locks, frontend contexts, semantic package faces,
  documentation authority/supersession, exclusions, and nominated worktrees.
- Closed typed identities represent repository-relative physical paths,
  products, workspaces, package faces, contexts, worktrees, documents, nodes,
  edges, conditions, sources, resolvers, obligations, and projections.
- Each collector result is `Exact(fact)` or `Unknown(obligation)`. An Unknown
  must name at least one applicable projection and automatically expands to one
  required failure per applicable projection.
- Inventory emits one typed file/directory/symlink record per physical item,
  validates unique physical identity and kind, and links semantic faces without
  inventing directory-shaped files.
- Worktree evidence includes stable typed identity, HEAD/branch/detached state,
  tracked and untracked status/content, and fails on command errors.
- Projection hashes use SHA-256 over the versioned spec and complete selected
  node/edge records. Edge IDs bind complete edge records; component IDs are
  projection-qualified; SCC evidence uses typed edge identities; clusters bind
  typed owner/reference/internal/cut records and emit a boundary quotient.
- Source binding covers repository inventories, contexts and lock digests,
  frontend configs, helper/resolver sources, tools, and worktrees without
  serializing checkout or temporary absolute paths.
- Publication writes one generation directory, fsyncs it, renames it into
  place, and atomically replaces `CURRENT`. Failure publication has the same
  transaction shape and types unavailable artifacts explicitly.

Rust, frontend, Cargo resolution, generated/WASM lineage, and call semantics
are not promoted beyond the proof available. Unsupported or unimplemented
semantic facts remain named Unknowns; no P2 heuristic edge survives as exact.

## Exact executable gates

### Promoted counterexample bank

The final source state ran the exact bank twice:

```text
cargo test -p xtask --locked --test architecture_p3_counterexamples -- --test-threads=1

run 1: 10 passed; 0 failed
run 2: 10 passed; 0 failed
normalized semantic output SHA-256, both runs:
265553a9ecbb78d4747979d91e1ee1bbcf39a8017554bb0f8ac57e6814ec7e18
```

The bank proves the typed constellation, unique inventory rejection,
per-projection Unknown expansion, complete projection/shard hash sensitivity,
projection-qualified SCC identity and typed witnesses, serialized-string
privacy rejection, exact-negative Rust quarantine, site-specific frontend
quarantine, and complete early failure publication.

### Full xtask suite and formatting

```text
cargo test -p xtask --locked
  architecture SHA-256 vectors: 1 passed
  architecture foundation:      12 passed
  P3 counterexamples:            10 passed
  metadata fail-closed:           3 passed
  total:                         26 passed; 0 failed

combined-output SHA-256
1baf9ae007bfadd974626a1d8e1476b3a73c03511a5ee11a331df251bf2be4dc

cargo fmt -p xtask -- --check
  GREEN

cargo check -p xtask --locked
  GREEN
```

The final release exporter binary used by the twins was:

```text
a83502242adbc8336d4e21229a17c3d931a587fa67a542547904ff284e078cb8  target/release/xtask
```

### Typed failure injection

Each requested stage exited 1, published one `CURRENT`, and exposed exactly one
complete three-file failure generation containing `binding.json`,
`diagnostics.json`, and `generation.json`. Every generation reported
`status=failure`, the requested typed stage, and zero checkout/temp absolute
paths.

```text
manifest    p3-37d025cca3ef913e9617d344add8376a1849e8fc19726a2f3d14d0b2bd389119
inventory   p3-6baec750a4980579ab018b482fd7ed07c03ba34fb644c398acf1aab6363df39d
worktree    p3-70715824678680f24b6ac6d3a643a1c578943ffb38b6854b57d8d70a81a54835
cargo       p3-fba86007f0ff1e660648bf9a01e6b607c48ee64271cb2ea41c4452d199ba078e
rust        p3-d4692c52957d34c257f0dbeb0a037f10df257b3a6696996a18dd8f48e08017b7
node        p3-90af126ef9a795a3f5af09b5632d1f4ec716558a154092e2f22b2cf35ab80c71
frontend    p3-4115c9a85a52cdf72570068d75518dabfff930575613ff061620773eea3b513d
artifact    p3-9ae7cdfd9c262a41110dd0fde3184b9d936271135b214292b7fd4e2db75cfbf3
projection  p3-919c773418cc86208245c06e9857a89f8b3dada6e449109138a0ceee200b908d
validation  p3-cde8c7bbe3183dc4ab564e6974503802c9272f5160a3784a30f8c6e2906201af
write       p3-14692d552d0b623f2a9175cd9d1962283b4084768cfc4c2c37055a9d73bc8bfc

injection receipt SHA-256
6707f3e9206712c6f05f38f761f36ad55fd473f89f05a472fc29094b5bfb1d3e
```

## Stable full-constellation twins

After all implementation lanes reported quiescent, the exporter ran twice with
`LC_ALL=C` and `TZ=UTC`. Both commands honestly exited 1 for required Unknowns,
both `CURRENT` pointers named the same run, and recursive byte comparison of
the generation directories found no difference.

```text
run id          p3-d1d9d3b1656f81a424bddbad49e4e9135f98aa692ee015bb415d2e08f13b2ae5
source binding  8eaedcc606ff066230d8b241112addac171034dc67e4d92c72470ebb315a0337
nodes           10,682
edges           10,709
inventory       10,572
projections     11
required        415
absolute paths  0
generation KiB  138,560
```

Artifact SHA-256 values, identical in both generations:

```text
19f4efddc6e184e7568f6a83d6e2cb958cf21a4be9148768ef7dd0e0ac112424  binding.json
37517e5f3dc66819f61f5a7bb8ace1921282415f10551d2defa5c3eb0985b570  diagnostics.json
43379693f99772f8d247ca2241245722b711dec09ad6de5e23d6496c6f2f64fc  generation.json
616ab935c015e4eaffce902f99dfb1a8766e48552af6de7bbba8920c6815bb65  manifest.json
617493020e1acbcd5dcc925db5e480884fa0ced5ff4212c8bc782465374aa246  graph.json
844a9bd160f4a7edb3be4da2e927f3dcc89ea9fc830ba1dd98ea3c09a9d759d3  projections.json
b7ec7aa68692bc719ed65b44861ea159e4be3af7e9ed7eb9ec0844af246ea13e  obligations.json
d2c6d5cb011e261158c9e73d67c6973f2e709e297acac3f15e1fcb4100bd72b0  inventory.json

artifact checksum-ledger SHA-256
735c955e21af4511b9022153e93311f6baf45eaed989491a69a5573ad5cc161e
```

### Mutation-locality observation

Earlier attempted twins did not match while other authorized lanes were
writing inside the declared constellation. Inventory comparison localized one
drift set to the parse-that V4 `.test-dist` build tree and a changing V4
`EVIDENCE.md`; another pair detected a final type-kernel adjudication rewrite.
The type rewrite retained counts but changed the source binding and run ID:

```text
pre-final run   p3-de1530d6eca201634a0a17cc4b6d055711197282727bc57afd7a89d41a3cb8e3
post-write run  p3-2bcb67cc0cfcbff40aa2fb3fe82379331572b0a362924f0055dae0f0b168f8ea
final source    restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/ADJUDICATION.md
final SHA-256   b7331cf1217a0bf21177db2f7daf730e01618fcdbda6c88a81b957cefa5766cf
```

This is evidence of source-binding sensitivity, not exporter nondeterminism.
Ephemeral `.test-dist` trees are now explicit manifest exclusions for both
repositories; changing source evidence remains visible. The final twins above
were taken only after the writers stopped.

## Exact remaining RED

The 415 required failures are deliberate and projection-qualified:

| Plane | Required instances | Truth retained |
|---|---:|---|
| Cargo resolution | 22 | four contexts stay Unknown for resolution equality, lock-package checksums, profile/target/features, stable declaration spans, and two preserved metadata failure causes |
| Rust semantics | 60 | inline/self/prefix/cfg/test/span/external ownership and broader macro/trait/dynamic facts stay Unknown across four Rust faces |
| Frontend semantics | 325 | application/test scope, aliases, globs, worker queries, type-only/dynamic/require, Vue source/spans, routes, and CSS classification stay site-specific Unknowns |
| Generated/WASM/call | 4 | producer and delivery lineage plus attributed call contexts stay Unknown |
| Global hostile proof | 4 | relocation, 1,000 full-input permutations, the full mutation bank, and locale variation remain pending |
| **Total** | **415** | **complete command remains RED** |

The exact obligation-code/count ledger has SHA-256:

```text
4d60d8dd38520da18aaa7f9c1558c771aaf94681c3e294132a59371b4e52d3ff
```

Passing structural and publication gates do not upgrade these semantic planes.
No SCC, cluster, or projection derived from an Unknown is movement authority.

## Routed repository gates

Strict clippy was invoked but is RED only at pre-existing files outside this
implementation slice:

```text
cargo clippy -p xtask --all-targets --locked -- -D warnings
  exit 101
  crates/egraph-derive/src/lib.rs:282:5
  clippy::collapsible_if
  log SHA-256 6f5e1f7834c25d8e6916ae70fae6c2e7826aa44bcb0fccc9b8d22b3f3f9cc107

cargo clippy -p xtask --all-targets --no-deps --locked -- -D warnings
  exit 101
  xtask/src/regen_simple_runtime.rs:473:10
  clippy::useless_format
  log SHA-256 6a0c7df8fda9ed01254b9b8358434da7f7d371de7b4e3eff1bce56e8f339c0d0
```

Those files were not modified. The workspace lock was byte-identical before
the P2 baseline, after all P3 tests and exports, and at handoff:

```text
a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908  Cargo.lock
```

Stop here. The truth-kernel foundation is executable and deterministic; the
architecture remains globally RED until every applicable required semantic and
hostile-proof obligation is discharged and the fresh hostile reviews and
adjudication required by `P2-ADJUDICATION.md` close.
