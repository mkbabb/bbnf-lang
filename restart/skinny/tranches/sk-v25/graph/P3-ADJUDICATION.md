# Architecture exporter P3 — fresh adjudication, supersession, and P3.1 repair contract

Status: **P3 is superseded and RED. The aggregate claim that its foundation is
GREEN is rejected; 0 of the 12 P2 clauses close. No P3 artifact may authorize
production or source movement. P3.1 is the only authorized next exporter slice.**

This is the independent adjudication required after `P3-REVIEW-A.md` and
`P3-REVIEW-B.md`. It supersedes `P3-EVIDENCE.md` and both reviews as verdict
authority without editing or erasing them. The reviews remain claim banks,
`P3-BASELINE.md` remains the frozen born-RED P2 receipt, and
`P2-ADJUDICATION.md` remains the governing 12-clause contract.

## Model and scope record

- Declared adjudicator model: `gpt-5.6-sol`; reasoning effort: `xhigh`.
- This adjudicator did not delegate.
- No recorded-model metadata endpoint was exposed inside the task. The parent
  must compare this declaration with the recorded task metadata. Until that
  comparison, recorded-model verification is **UNPROVEN**.
- I read all of `P2-ADJUDICATION.md`, `P3-BASELINE.md`, `P3-EVIDENCE.md`, both
  P3 reviews, BB-A.W0-W5, the manifest, all frontend helpers, all files under
  `xtask/src/architecture/`, both architecture test entrypoints, all
  source-isomorphic tests, every architecture fixture, and the xtask Cargo and
  CLI entrypoint surface.
- Reproduction artifacts and compiled probes were confined to `/tmp`. This
  adjudication is the only repository file written. `Cargo.lock`, exporter
  source, helpers, fixtures, tests, evidence, reviews, and production files were
  not edited.

## Fact record: adjudicated snapshot

The checkout was dirty and changing outside this task. Counts in
`P3-EVIDENCE.md`, Review A, Review B, and this section are observations of
different bound snapshots, not interchangeable constants. The 90-file P3
implementation/config/helper/test ledger read for this ruling had 5,612 lines
and aggregate SHA-256:

```text
8b45907e44c3b0dbdc2950e184fed6371b42656227e0aa4217b9c958d3a9e7ae
```

The rebuilt release binary and unchanged workspace lock were:

```text
a83502242adbc8336d4e21229a17c3d931a587fa67a542547904ff284e078cb8  target/release/xtask
a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908  Cargo.lock
```

One fresh `LC_ALL=C TZ=UTC` release export honestly exited 1:

```text
run id          p3-79c7e77dffb9da619db58f280e568ebc7a876b71e6c3870bc08b0b56cba4b402
source binding  58174ff70d363cbd0b276665a76a5bf04b16c3123494b5af3452929352c06caa
nodes           10,686
edges           10,713
inventory       10,576
worktrees       79 measured + 2 synthetic nomination nodes
projections     11
required        415
wall            68.67 s
maximum RSS     581,238,784 bytes
generation      141,920,691 bytes
projections     128,739,068 bytes
```

The exact projection facts were:

| Projection | Edges | Required | Serialized movement trust |
|---|---:|---:|---|
| containment | 54 | 0 | `true` |
| rust_module | 0 | 36 | `false` |
| cargo_declared | 0 | 8 | `false` |
| cargo_resolved | 0 | 22 | `false` |
| frontend_build | 0 | 159 | `false` |
| runtime_delivery | 0 | 160 | `false` |
| test_overlay | 0 | 19 | `false` |
| generated_provenance | 0 | 2 | `false` |
| ownership | 10,657 | 4 | `false` |
| call | 0 | 5 | `false` |
| documentation | 2 | 0 | `true` |

The inventory locally contained 8,987 files, 1,502 directories, and 87
symlinks. All 10,576 emitted `(repository,path)` identities were unique; 29
rows were exclusion roots and only 21 rows carried a semantic-face link.
Sixteen tracked `.profiles/**` files were absent because the walker stopped at
the excluded directory. The graph had 10,576 physical ownership edges, 79 real
worktree ownership edges, and two product nomination edges. Neither
`bbnf-primary` nor `parse-that-primary` matched any measured worktree; neither
nomination target had a repository-ownership edge.

The current generation contained no searched literal `/Users/`,
`/private/tmp/`, `/private/var/`, `/tmp/`, `file://`, or `/home/` path. All six
current context manifest and lock hashes happened to be present. Those are
artifact-local facts; they do not prove the validators or schemas are total.

## Independent counterexample record

These probes were written or emitted only under `/tmp` and were run against the
adjudicated source/binary.

1. A graph with one required Rust failure still projected containment with
   `trustworthy_for_movement=true`.
2. A manifest-failure generation had `diagnostics.json` removed. The identical
   rerun selected the same run ID, repointed `CURRENT`, left the file absent,
   and retained a `generation.json` claim that it was published.
3. All eleven advertised stage injections exited 1 with the requested label,
   but every generation had only three files, `binding.complete=false`, and the
   same bootstrap-unavailable domains. No requested late stage was reached.
4. Six independent manifest mutations returned `Ok(())`: cross-repository
   product workspace, cross-product face, absent document path, omitted
   reciprocal face, cross-repository nomination, and two repository IDs
   resolving to the same canonical root.
5. `file:///etc/private-source`, an encoded file URI, an embedded
   `/home/reviewer/secret.rs`, and an embedded `C:/reviewer/secret.rs` all
   constructed as valid `CanonicalText`.
6. A complete bidirected three-node graph had 6 typed edges and cycle-space
   dimension 4. P3 emitted 3 witnesses of GF(2) rank 3.
7. A 25-node directed ring split into the same two member sets before and after
   adding a feature signal. Only the post-hoc signal label changed.
8. Legal branches `feature/a` and `feature-a` collided as
   `fixture-branch-feature-a`. An untracked mutation changed a detached
   worktree's identity as well as its content hash. Adding an ignored file left
   identity, dirty state, and content hash unchanged.
9. A CSS symlink escaped its disposable repository to `../../outside.css`.
   Mutating only the external target retained the link-text hash but changed
   helper JSON and changed the CSS Unknown count from one to zero.

These reproduce the load-bearing findings without relying on either review's
stored output.

## Review A rulings

Severity below is this adjudication's severity. `BLOCKER` means the defect can
create machine-readable authority or a false exact ownership fact. `HIGH`
means a foundation truth or evidence invariant is contradicted. `MEDIUM` means
the current artifact remains safely RED but its proof or operational claim is
materially overstated.

| Item | Ruling | Independent disposition |
|---|---|---|
| **A1 — affirmative movement authorization** | **CORRECT; BLOCKER.** | Fresh output and the one-Unknown probe both reproduced it. `project/spec.rs:56-75` is projection-local. The two `true` values contradict the global RED state and the prose prohibition. Incomplete context/document identities make the risk concrete. |
| **A2 — corrupt generation reuse** | **CORRECT; HIGH.** | The missing-file replay reproduced exactly. `evidence/write.rs:154-175` verifies neither file set nor hashes when the run directory exists. New-name staging/rename is retained; idempotent self-verifying publication is false. |
| **A3 — incomplete source binding** | **CORRECT; HIGH.** | The escaped-symlink mutation independently changed collector output without changing link identity. `binding.rs:83-94,150-176` allows absent manifest/lock hashes while setting `complete=true`; capture precedes live collectors and has no recensus. The current hashes being present does not repair the invariant. Registry/network influence is an additional unbound Cargo input while metadata is executed. |
| **A4 — worktree truth and nomination** | **CORRECT, with ignored-content narrowing; BLOCKER for nomination, HIGH otherwise.** | The artifact has 79 measured records and two unmatched phantom nominations; collision and detached-instability probes reproduced. Ignored bytes are demonstrably omitted. That omission is a defect only relative to the unqualified “tracked and untracked content” claim; P3.1 may explicitly exclude ignored derived bytes if no collector can read them and all relevant ignored inputs are separately bound. |
| **A5 — manifest is not relational authority** | **CORRECT; HIGH.** | All six manifest mutations passed. Typed TOML syntax and unique IDs are retained; reciprocal repository/product/workspace/face/context/document/nomination authority is not. |
| **A6 — inventory incomplete by construction** | **CORRECT; HIGH.** | Fresh local counts reproduced uniqueness, the 29 stopped exclusion roots, 21 face-linked rows, and 16 tracked files erased by `.profiles/**`. Local row validity is retained; complete authored/input closure is not. |
| **A7 — SCC useful, basis/clusters false** | **CORRECT; HIGH.** | The K3 rank and 25-node signal probes reproduced both defects. SCC partition membership and typed replayable witnesses survive; the `cycle_basis` completeness name and W5 partition claim do not. |
| **A8 — relabeled bootstrap injections** | **CORRECT; HIGH.** | All eleven labels produced the same three-file minimal-binding shape. `requested_injection` returns at `mod.rs:149-156`; `inject` is a no-op at `503-505`. The receipt proves typed labeling of an early failure, not stage-local failure handling. |
| **A9 — privacy is artifact-clean but validator-partial** | **CORRECT; MEDIUM.** | The fresh artifact scan was clean, while four generic/URI probes passed. The recursive string walk is retained; its path predicate is not universal. |
| **A10 — semantic bank overclaims** | **CORRECT; MEDIUM.** | Rust tests call a collector that never reads Rust; frontend tests mostly assert unconditional package-level codes. Legacy Cargo/Rust/frontend fixtures are not routed through owning P3 collectors. Zero heuristic exact edges is real; site-specific counterexample execution is not. |
| **A11 — determinism, resources, and topology** | **CORRECT; MEDIUM.** | The fresh run used 68.67 s, 554 MiB RSS, and 135.3 MiB of generation bytes; `projections.json` was 122.8 MiB. Every projection repeats the node universe and singleton structures. `mod.rs` remains 548 lines, and the CLI promises DOT/report outputs the writer does not emit. Prior same-source twin determinism is retained; practical audit output and orchestration-only topology are not. |

## Review B rulings

Review B's nine findings collapse onto the same eleven independent defect
families, but each item is ruled explicitly.

| Item | Ruling | Independent disposition |
|---|---|---|
| **B1 — globally RED movement trust** | **CORRECT; BLOCKER.** | Same as A1; reproduced from both full output and a minimal graph. |
| **B2 — incomplete repeated-output publication** | **CORRECT; HIGH.** | Same as A2; missing-file reuse was independently reproduced. Exact-file-set, size, and digest verification are absent. |
| **B3 — false cycle basis and non-dynamic cluster** | **CORRECT; HIGH.** | Same as A7. The fresh K3 result was `dimension=4,witnesses=3,rank=3`; a feature mutation did not move membership. Missing required `CYCLE_BASIS_INCOMPLETE` and `DYNAMIC_SHARD_PARTITION_ABSENT` instances are themselves fail-closed gaps. |
| **B4 — source binding omits consumed inputs** | **CORRECT; HIGH.** | Same as A3. Current frontend context bindings also name the Rust workspace manifests (`Cargo.toml` and `rust/Cargo.toml`) rather than their package-face manifests. |
| **B5 — manifest/inventory relationships unchecked** | **CORRECT; HIGH.** | Same as A5/A6. Current root/manifest kinds happen to be correct; the validator does not require them, validate document paths, or enforce reciprocal ownership. |
| **B6 — synthetic worktree nominations** | **CORRECT; BLOCKER/HIGH.** | Same as A4. Product edges land on two unmeasured, unowned nodes. A repository-local administrative worktree key is required; branch/content state must remain attributes. |
| **B7 — prefix-blacklist privacy** | **CORRECT; MEDIUM.** | Same as A9. Review A's URI cases and Review B's embedded-host cases all pass the current predicate. |
| **B8 — injections never reach their stage** | **CORRECT; HIGH.** | Same as A8. A stage label is not proof that the collector, validator, fsync, rename, or `CURRENT` swap failed there. |
| **B9 — blanket quarantine is not site proof** | **CORRECT; MEDIUM.** | Same as A10. The safe quarantine posture is retained; the receipt's stronger site-specific coverage claim is superseded. |

No numbered review finding is left unruled. No review engineering percentage or
unbound count is adopted as a contract fact.

## The 12 P2 clauses after P3

| # | Clause | Adjudicated P3 disposition |
|---:|---|---|
| 1 | Closed typed schema | **NOT CLOSED.** Closed enums/newtypes are useful, but `Attribute::Detail` remains an arbitrary pair; manifest relationships, context-to-face/input identity, documentation physical authority, nomination identity, and several cross-kind invariants are not closed. |
| 2 | Stable identity | **NOT CLOSED.** Repository-relative physical and complete edge identities are retained. Worktree identity collides or changes with content, phantom nomination identity splits ownership, file URIs/embedded host paths pass, symlink-consumed bytes escape, and full relocation is required RED. |
| 3 | Complete inventory and closure | **NOT CLOSED.** Emitted rows are unique and kind-tagged. Tracked authored files can disappear below broad exclusions, semantic attachment is skeletal, required physical kinds/documents are unchecked, symlink inputs escape, roots can alias, and capture is not a coherent end-to-end snapshot. |
| 4 | Named typed projections | **NOT CLOSED.** Eleven typed specs, singleton-preserving membership, and SHA-256 hashes exist. Nine semantic views have zero edges, and two incomplete views serialize movement trust during a globally RED generation. |
| 5 | Exact Cargo resolution | **NOT CLOSED by design.** Four contexts and current lock bytes are named; two failure causes are retained. No declaration/resolution/feature fact is exact, successful metadata is discarded, context inputs are misbound/unbound, network state is unbound, and 22 failures remain. |
| 6 | Bounded Rust semantics | **NOT CLOSED by design.** No P2 guessed edge survives, but the collector reads no Rust source, emits no exact Rust edge, and the tests do not execute the named Rust fixtures. Sixty projection-qualified failures remain. |
| 7 | Frontend/build plane | **NOT CLOSED.** No guessed exact edge survives and a few Vue/CSS sites are text-anchored. Configured programs, tests, imports, aliases, globs, workers, routes, and CSS remain 325 failures; unconditional codes are not site proof and helper reads can escape source binding. |
| 8 | CSS/WASM/generated planes | **NOT CLOSED.** CSS sites are quarantined rather than parsed; generated/WASM/call lineage has zero exact edge and four required instances. |
| 9 | Worktree plane | **NOT CLOSED.** HEAD/branch/detached flags, tracked diffs, ordinary untracked bytes, and command-error propagation are retained. Identity is not stable/total, ignored policy is unstated, and product nomination is false. |
| 10 | Cycles and audit shards | **NOT CLOSED.** SCC membership, typed witnesses, typed cuts, and quotient plumbing are useful. The emitted set is rank-incomplete and threshold ID bisection is not signal-driven W1/W5 partitioning. |
| 11 | Fail-closed truth | **NOT CLOSED.** Unknowns expand per projection and make the command exit 1. Positive movement flags, false exact nominations, corrupt generation reuse, unbound consumed inputs, incomplete privacy, missing foundation obligations, and bootstrap-only injections contradict total fail-closed truth. |
| 12 | Executable proof | **NOT CLOSED.** Format/check/26 tests and prior same-source twins pass. Manifest, worktree, symlink, privacy, K3, signal, corruption, and real-stage probes were missing; relocation, full mutation/permutation/locale, output budgets, DOT/report, and a clean strict gate remain open. |

**Result: 0 of 12 clauses close.** Passing a foundation organ cannot close a
global clause contradicted by one exact-negative fixture or applicable Unknown.

## Exact organs retained

Retain only with the stated scope:

- the P2-to-P3 clean break: no P2 collector, output module, heuristic frontend
  helper, deepest-prefix edge, legacy alias, or dual collector path remains;
- the versioned declarative manifest as the sole topology input, pending a
  relational and physical validator;
- canonical repository-relative `RepoPath`, current closed enums/newtypes, and
  the distinction between `Exact(fact)` and projection-qualified
  `Unknown(obligation)`;
- deterministic Unknown expansion, sort/dedup, conflicting-node rejection,
  dangling-endpoint rejection, relation-class checking, and complete
  edge-record SHA-256 identity;
- the SHA-256 implementation and known vectors;
- the current artifact's unique physical rows, file/directory/symlink tags,
  ordinary file hashes, explicit exclusion-root rows, and zero searched private
  literals;
- Git command-error propagation plus tracked-diff and non-ignored-untracked
  content sensitivity, but not a total worktree identity/content policy;
- zero exact Cargo, Rust, frontend, generated, WASM, or call edge and the 415
  current failures as honest reasons to remain RED;
- projection SHA-256 over the implemented spec and selected complete records,
  projection-qualified component IDs, SCC membership for selected edges, and
  typed replayable cycle witnesses;
- typed internal/outgoing/incoming cut incidence, deterministic cluster-record
  hashes for the records actually included, and deterministic quotient plumbing;
- fsync/staging/rename publication for a previously absent generation name and
  atomic same-filesystem `CURRENT` replacement when its preconditions hold;
- artifact-bound same-source determinism from the P3 receipt/reviews and the
  fresh run facts above;
- `cargo fmt -p xtask -- --check`, `cargo check -p xtask --locked`, and all 26
  xtask tests, which passed again during this adjudication.

Reject as truth, proof, or authorization:

- `foundation gates GREEN` as an aggregate P3 status;
- `SourceBinding.complete=true` as proof of a complete immutable/hermetic input;
- manifest completeness, reciprocal authority, semantic/inventory closure, and
  universal privacy;
- current product nomination edges and both synthetic nomination nodes;
- the emitted witness set's `cycle_basis` name/completeness and sorted-halves
  clustering as W5 dynamic partitioning;
- all eleven injection receipts as evidence of eleven real failure sites;
- pre-existing generation reuse as atomic self-verifying publication;
- the promoted P3 bank as execution of site-specific semantic fixtures;
- any current `trustworthy_for_movement=true` value;
- any Cargo/Rust/frontend/runtime/test/generated/WASM/call topology, cycle,
  reachability, cluster, or movement inference while its plane is empty or has
  an applicable Unknown;
- any exact count/hash detached from its run ID, source binding, and snapshot;
- any production/source move, deletion, colocation, split, or ownership choice.

## Supersession ruling and P3.1 scope

P3 is superseded as a foundation verdict and movement-capable artifact. Its
retained source organs are a repair base, not a legacy compatibility path.
`P3-EVIDENCE.md` may be cited only as a historical receipt for its bound runs;
its aggregate GREEN and coverage interpretations are no longer authoritative.

**P3.1 is the smallest coherent next repair.** It is not P4, not a semantic
breadth wave, and not source-layout work. It must remove false authority,
false exact nomination, incomplete evidence reuse, unbound foundation inputs,
false structural completeness, and proof overclaim before any exact Cargo,
Rust, frontend, generated, WASM, or call promotion. P3.1 must replace P3 in
place with a `v3.1` schema; it may not keep a v3 writer/reader, resurrect a P2
module/helper, add a compatibility adapter, or run dual paths.

P3.1 completion may state only **“foundation repaired; global architecture
RED.”** It cannot state P2, P3, BB-A.W1, or movement GREEN while any semantic or
hostile-proof obligation remains.

## Born-RED P3.1 counterexample bank

The following bank must land before implementation changes, fail against this
P3 for the named reason, and remain source-isomorphic outside production code.
After repair, each foundation case must invert exactly: remove the mutation and
the named error disappears; no unrelated hash, obligation, or projection may
change. Merely checking that an enum code exists is not execution.

| ID | Disposable fixture/mutation | Required P3.1 assertion |
|---|---|---|
| F01 | A graph with one required failure outside containment/docs | Generation authorization and every projection/cluster movement field are false. |
| F02 | A fully GREEN synthetic graph without a separately bound adjudication receipt | Movement remains false; structural validity cannot self-authorize movement. |
| F03 | Delete one file from an existing content-addressed generation | Rerun never points `CURRENT` at it; exact typed write failure or verified reconstruction occurs. |
| F04 | Modify one byte and add one extra file to an existing generation | Exact file-set, size, and digest verification catches both; corrupt data is quarantined or atomically replaced. |
| F05 | Alias two repository declarations to one canonical root | Manifest stage fails before inventory with `REPOSITORY_ROOT_ALIAS`. |
| F06 | Cross repository/product/workspace/face/context links; omit reciprocal entries | Each mutation fails with one relational code; no cross-owner exact edge is built. |
| F07 | Missing/wrong-kind Cargo manifest, frontend manifest/config, lock, face root, document path, and supersession target | Manifest/inventory validation rejects the exact role and expected physical kind. |
| F08 | Put a tracked authored file below an exclusion; use unsupported/overbroad exclusion syntax | Census fails; a tracked input cannot be erased by an “ephemeral” exclusion. |
| F09 | Nomination absent, ambiguous, cross-repository, or selecting a phantom ID | No node is synthesized; selector resolves to exactly one measured worktree or fails. |
| F10 | Branches `feature/a`/`feature-a`; two detached worktrees at one HEAD; dirty detached mutation | IDs remain collision-free and stable across content/HEAD/branch changes; mutable state changes only attributes/hash. |
| F11 | Ordinary untracked and ignored files | The declared ignored policy is executable. Non-ignored untracked bytes change state; ignored bytes either are explicitly out-of-scope and unreadable to collectors or are separately source-bound. |
| F12 | Symlink inside a semantic face escaping the repository, plus dangling and in-repo symlinks | Escape/dangling fails before helper read. An allowed in-repo target has typed identity and bound target bytes, or all semantic symlink following is rejected. |
| F13 | Missing context manifest/lock hash and a frontend context bound to a Rust manifest | `complete=false`; every context binds its own typed manifest/config/lock roles. |
| F14 | Mutate a bound in-repository input after capture but before/during collection | Final recensus or immutable-read verification fails with `SOURCE_SNAPSHOT_DRIFT`; mixed-snapshot graph is never published. |
| F15 | K3 bidirected graph, self-loops, parallel typed edges, and shuffled edge order | Basis count and GF(2) rank equal `m-n+c`; every edge witness replays/closes and output is order invariant. |
| F16 | One-field ownership, feature, lineage, test, package-face, frontend-feature, articulation, and fan mutations | Hard signal changes move/prevent membership as specified; articulation/fan affect cut choice; no post-hoc-only label passes. |
| F17 | Whole and embedded POSIX, Windows-drive, UNC, `file:` URI, percent-encoded URI, checkout, and temp paths in every serializable field and diagnostic | Universal privacy validator rejects/sanitizes each; ordinary `https:` URLs and repository-relative paths remain valid. |
| F18 | Real failpoints at manifest read, inventory walk/read, Git, Cargo spawn/read/decode, Rust, Node spawn, helper decode, frontend, artifact, projection, basis, cluster, validation, file create/write/fsync, generation rename, directory fsync, and `CURRENT` replace | The requested operation is reached; prior artifacts/binding have the stage-appropriate state; no mixed generation becomes current. |
| F19 | Empty-edge full constellation and repeated singleton partitions | Output uses an explicit total-node-universe rule without P-fold singleton record expansion and stays within the budgets below. |
| F20 | Fresh and repeated full output | Deterministic JSON, DOT, concise report, condensation/basis, quotient, shards, binding, obligations, and generation receipt are all present and hash-bound. |

The inherited semantic bank is a separate **required-RED ledger** during P3.1:
both Cargo fixtures, both Rust fixture families, both frontend fixture families,
and every P2 disposable Rust/frontend/CSS/WASM case must be enumerated by
fixture ID and owning collector. P3.1 must retract “site-specific proof” until a
fixture is actually parsed/resolved. Later plane work may flip one case only
with exact source/context/projection attribution, exact-positive and
exact-negative edge sets, mutation inversion, and no false extras. Blanket
package-level obligation emission may remain a capability boundary but cannot
be counted as a site test.

## Binding laws for the P3.1 implementation

### Global movement latch

1. One generation-level `MovementAuthorization` controls every view. While the
   generation is RED/failure, the binding is incomplete, any required Unknown
   or hostile proof remains, validation is incomplete, or any review finding is
   unresolved, the value is `denied` and every projection/cluster serializes
   false.
2. A GREEN graph is necessary but not sufficient. Movement also requires a
   separate versioned authorization receipt named by the manifest and binding
   the exact run ID, source-binding hash, all 12 clause verdicts, two hostile
   review hashes, adjudication hash, and verified model metadata. P3.1 will not
   possess such a receipt, so its movement value is unconditionally denied.
3. Remove `trustworthy_for_movement`. A useful local field may be named
   `structurally_valid_for_selected_edges`; its schema and documentation must say
   it is not authorization.

### Manifest, inventory, and source snapshot

1. Canonical repository roots must be distinct. Products, workspaces, faces,
   contexts, documents, exclusions, and nominations form a reciprocal relational
   schema: every forward list equals the inverse declarations and all linked
   repositories/products/workspaces agree.
2. Name exactly the four Cargo and two frontend contexts by typed ID/kind, not
   `len()`. A context directly names its repository, workspace, applicable
   package faces, manifest, lock, target/profile, and typed config inputs.
3. Face roots are directories; manifests, locks, configs, and documents are
   files unless an explicitly typed symlink policy says otherwise. Documentation
   role/supersession is validated and linked to the physical file in the graph.
4. Exclusion matching has one documented grammar. A directory exclusion may
   stop descent only after reconciliation with `git ls-files`; no tracked
   authored/config/evidence input may disappear. Derived ignored trees are
   allowed only if collectors are forbidden to read them.
5. Collectors read through typed captured physical identities. The simplest
   accepted symlink policy is to reject semantic symlink following. If following
   is retained, resolve inside the same repository, bind link text, target
   identity, and target bytes, and reject escape/dangling/cycles.
6. `SourceBinding.complete` is computed, never assigned optimistically. It is
   true only when every required context/helper/tool/config/worktree/input hash
   exists and validates. Remove the live Cargo metadata call while its successful
   result is discarded; later Cargo collection must be locked, offline/hermetic,
   and bind every consumed resolver input.
7. Collect immutable captured bytes or perform a final complete recensus and
   worktree/context/helper comparison immediately before publication. Drift is a
   typed failure, never a retry that hides the first observation.

### Worktree policy

1. Use a repository-local Git administrative identity: `primary`, `bare`, or
   the normalized relative `git-dir` key beneath the repository common dir.
   Never derive identity from absolute path, branch spelling, HEAD, dirty state,
   or content hash.
2. HEAD, branch, detached/bare state, tracked diff, non-ignored-untracked bytes,
   and content digest are mutable attributes. Preserve branch spelling; no lossy
   `portable_key` identity.
3. State explicitly that ignored derived content is outside the worktree digest.
   Any ignored content a collector can consume must instead be declared and
   bound by the source inventory; otherwise collector access fails.
4. A nomination declaration has its own typed identity and selector. It resolves
   to exactly one measured worktree in the same repository/product. The product
   edge targets that measured identity; absence/ambiguity fails and no phantom
   Worktree node is created.

### Cycle-basis and clustering law

Treat each projection as a directed labeled multigraph whose distinct typed edge
IDs are distinct basis columns. For each connected SCC subgraph, including
self-loops and parallel edges, the GF(2) cycle-space dimension is
`rank = m - n + c` (`c` is the number of undirected connected components of the
subgraph; normally one per nonempty SCC). Emit exactly `rank` independent
ordered directed cycles. Every witness must use present edge IDs, remain inside
the component, replay head-to-tail, and close. A validator independently rebuilds
the incidence vectors and proves count, rank, and determinism. Failure emits
`CYCLE_BASIS_INCOMPLETE` and denies movement; nonempty replayable witnesses are
not enough.

Clustering order is also normative:

1. SCC is the first structural boundary. Ownership, package face, feature,
   generated lineage, test reachability, and frontend feature form hard
   compatibility signatures; incompatible signatures never merge.
2. Actual articulation points and measured fan-in/out choose cuts before size
   enforcement. A signal mutation must affect membership/cut eligibility, not
   merely label a preselected half.
3. Smaller adjacent acyclic/SCC seeds may merge only across a quotient edge when
   their full signature and reviewer route agree. No merge is based only on
   lexical identity adjacency.
4. Target 24 nodes/80 internal edges. Hard maximum is 32 nodes/120 internal
   edges; oversize clusters recursively cut on graph signals. Lexical ordering
   is only a deterministic tie-breaker after graph criteria, never the partition
   algorithm.
5. Emit total disjoint ownership, typed internal/outgoing/incoming incidence,
   exact quotient reconciliation, the component basis, every shard-local
   internal basis, and quotient cycle evidence needed to reconstruct cross-shard
   cycles. All node/edge/cut roles participate in SHA-256 hashes.

### Evidence transaction and real stage order

The only permitted order is:

```text
bootstrap -> manifest relational validation -> repository resolution
-> inventory/exclusion/kind validation -> worktree census/nomination resolution
-> complete source binding -> foundation facts
-> cargo -> rust -> node/frontend -> artifact boundaries
-> graph canonicalization -> projection/SCC -> cycle basis
-> dynamic clusters/quotient -> global validation -> final recensus
-> generation assembly -> staged file writes/fsync -> generation commit
-> CURRENT replacement -> complete
```

Failpoints are checked at their named operation, not once at bootstrap. A failure
generation publishes every already-completed safe artifact and marks every
future/unvalidated artifact typed-unavailable. Candidate unvalidated graphs use
a distinct name and can never be mistaken for `graph.json`.

Publication must:

1. build a uniquely named staging directory on the output filesystem;
2. serialize the exact expected file set; record size and SHA-256 for each;
3. fsync every file and the staging directory;
4. under an output-root publication lock, verify any existing run directory's
   exact file set, sizes, hashes, schema, run ID, and binding;
5. reuse only a byte-verified directory; otherwise quarantine it and atomically
   install the verified staging directory, then fsync `generations/`;
6. write/fsync a temporary `CURRENT`, atomically rename it, and fsync the root.

Any injected or real failure before `CURRENT` replacement leaves the previous
valid pointer intact and no mixed directory current. If the medium cannot accept
even a failure generation, report that honestly on stderr and do not claim one
was published. Logical write failpoints must exercise the clean fallback
transaction; disk-full/fsync/rename/CURRENT failpoints must prove old-current
preservation.

### Privacy law

The final recursive string walk remains, but the predicate must tokenize and
percent-decode path-like substrings independent of the host OS. Reject whole or
embedded POSIX absolute paths, tilde paths, Windows drives, UNC paths, and
case-insensitive `file:` URIs, including encoded forms. Sanitize diagnostics
from structured paths before string assembly. Validate JSON object keys as well
as values. Permit repository-relative canonical paths and non-file URLs such as
`https:`. Full relocation remains required RED until two equivalent relocated
two-repository generations are byte-identical and contain no checkout/temp
literal.

## KISS-forward topology

Keep the successful P3 split and change only where an invariant is currently
mixed:

```text
xtask/src/architecture/
  mod.rs                         stage orchestration only (target <= 220 lines)
  manifest.rs                    schema + reciprocal relational validation
  schema/{identity,fact,obligation}.rs
  inventory/{filesystem,worktree}.rs
  collect/{foundation,cargo,rust,frontend,artifact}.rs
  project/{spec,scc,cycle,cluster,quotient}.rs
  evidence/{binding,validate,write}.rs

xtask/architecture/frontend/{program,vue,css}.mjs
xtask/tests/architecture/{schema,manifest,inventory,worktree,cargo,rust,
                          frontend,artifact,project,evidence,privacy}.rs
xtask/tests/fixtures/architecture/<owning-responsibility>/...
```

`foundation.rs` owns current manifest/inventory/worktree fact construction now
embedded in `mod.rs`. `cycle.rs` owns basis construction/rank; `scc.rs` owns only
partition. `cluster.rs` owns membership policy/signals; `quotient.rs` owns typed
incidence, quotient reconciliation, and their hashes. Do not split individual
enums or one-line helpers into sand modules. Do not add a second manifest,
collector, graph, writer, or v3 compatibility route.

For compact output, keep the canonical node/edge records only in `graph.json`.
Each projection binds a node-universe hash and selected edge IDs. A versioned
implicit-singleton rule or shared content-addressed partition table may represent
total isolated-node membership; it must expand deterministically and validate
to the complete node set without repeating full singleton records eleven times.

## Resource and output budgets

Budgets are measured on the same-host release full-constellation gate with
`LC_ALL=C TZ=UTC`, warm build artifacts, no network, and canonical uncompressed
published bytes. They prevent compression from hiding an unauditable model.

| Budget | P3.1 limit |
|---|---:|
| full exporter wall time, each of two twins | <= 45 s |
| maximum resident set | <= 402,653,184 bytes (384 MiB) |
| complete generation | <= 50,331,648 bytes (48 MiB) |
| `projections.json` or successor projection payload | <= 25,165,824 bytes (24 MiB) |
| synthetic bootstrap/manifest failure generation | <= 131,072 bytes |
| architecture unit/integration/counterexample suite | <= 30 s, zero network |
| shard target / hard maximum | 24/80; 32/120 nodes/internal edges |

A budget miss is a required `RESOURCE_BUDGET_EXCEEDED` failure and denies
movement. It may be raised only by a new adjudication with raw measurements and
a format/complexity explanation, never by silently weakening the gate.

## Clean gates and stopping rule

P3.1 does not close until all of the following are recorded against one
immutable source ledger:

1. the F01-F20 bank is captured born-RED against P3, then passes twice with
   byte-identical normalized semantic receipts against P3.1;
2. `cargo fmt -p xtask -- --check`, `cargo check -p xtask --locked`,
   `cargo test -p xtask --locked -- --test-threads=1`, and
   `cargo clippy -p xtask --all-targets --no-deps --locked -- -D warnings` all
   exit 0; the current strict clippy failure at
   `xtask/src/regen_simple_runtime.rs:473` is routed outside P3 source but still
   prevents calling the repository gate clean;
3. `Cargo.lock` is byte-identical before/after every bank, build, and export;
4. every real stage/I/O failpoint proves the stage-appropriate artifact set,
   binding completeness, old-current preservation, and no orphan current temp;
5. two quiescent full exports meet the budgets and are byte-identical, including
   JSON, DOT, concise report, bases, quotient, shards, and receipts;
6. corruption, concurrency, source-drift, symlink, exclusion, worktree,
   nomination, privacy, K3/rank, signal-movement, and output-compaction fixtures
   pass exact-positive and exact-negative assertions;
7. in-memory full captured facts survive 1,000 input orderings with identical
   outputs; foundation mutation-locality and available locale/relocation fixtures
   invert exactly;
8. every inherited semantic fixture is enumerated honestly as executed or
   required RED; no unconditional code is credited as site coverage;
9. two fresh hostile critics and a fresh adjudicator verify model metadata and
   close the P3.1 foundation findings with immutable hashes.

Even after those gates, Cargo, Rust, frontend, CSS/WASM/generated, call,
full-relocation, full-mutation, locale, and hostile-proof Unknowns remain global
RED until discharged by later exact plane work. BB-A.W2-W5 remain blocked.

No P3 or P3.1 graph, SCC, basis, cluster, quotient, report, or review authorizes
relocation, deletion, colocation, splitting, ownership change, or any production
source movement. Stop after this adjudication.
