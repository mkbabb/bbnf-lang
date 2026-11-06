# Architecture exporter P3 — hostile review A

Status: **RED. The claimed truth-kernel foundation is not GREEN, complete
architecture truth remains RED, and 0 of the 12 P2 clauses close. Production
or source movement remains forbidden.**

This review assumed P3 was wrong. It did not delegate, did not inspect any P3
review, did not edit implementation, fixtures, P2/P3 evidence, `Cargo.lock`, or
either production repository, and used only fresh `/tmp` outputs and disposable
fixtures. Recorded-model verification is **UNPROVEN** inside this reviewer; the
parent must compare this task's recorded model metadata with the protocol before
using the review as a hostile-audit receipt.

## Reviewed snapshot and authority

I read completely:

- `P2-ADJUDICATION.md`, `P3-BASELINE.md`, and `P3-EVIDENCE.md`;
- `waves/BB-A/W0.md` through `W5.md`;
- `xtask/architecture/constellation-v3.toml` and all three frontend helpers;
- every file under `xtask/src/architecture/`;
- every architecture test, fixture, and helper, including the promoted P3 bank;
- the P3 `xtask` Cargo/entrypoint surface and its diffs.

Before this review file existed, the 88-file implementation/test/config ledger
had 5,610 lines and aggregate SHA-256:

```text
a9b390c73fe40b06de0bbfe6f5a6bb52722e6f2c6be15a7c1e0eabc919221133
```

The nine-file authority/wave checksum ledger had aggregate SHA-256:

```text
e6c8a0bb8853a1fe55251c46a6837881e24bcd37e9326a585f45849f0e53df7d
```

`Cargo.lock` remained:

```text
a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908
```

## Numbered findings

### 1. [P0] The RED artifact contains an affirmative movement authorization

`project/spec.rs:56-75` sets `trustworthy_for_movement` from only the Unknowns
attributed to that one projection. It does not require the generation to be
GREEN and does not apply a global hostile-proof latch.

The fresh generation was globally RED with 415 required failures, but emitted:

```text
projection      edges  required  trustworthy_for_movement
containment        54         0  true
documentation       2         0  true
```

The containment face is not complete even on its own terms. The six context
nodes created at `mod.rs:316-328` have no typed link to workspace, lock, target,
kind, or package face. The documentation nodes at `mod.rs:383-407` omit their
physical path and authority role. `DOCUMENTATION_SUPERSESSION_UNMODELED` exists
in the enum but is never emitted. Therefore the two `true` values are not merely
poor wording: a machine consumer can accept exact SCCs/clusters from an
incomplete projection while the command itself is RED.

This directly contradicts the prose prohibition on using any P3 projection,
SCC, or cluster for movement. It also answers the review's movement question:
**yes, exact output can accidentally authorize movement.** Generation RED must
be an unbypassable prerequisite, not a projection-local convention.

### 2. [P0] Content-addressed publication silently reuses corrupt generations

`evidence/write.rs:154-175` checks only whether the final run-ID directory
exists. If it exists, P3 neither verifies its file set nor checks bytes against
the hashes in `generation.json`; it still atomically replaces `CURRENT`.

Adversarial replay against an initially fresh output:

```text
first run:  exit 1
run ID:     p3-48604d8cfa060d64739c53bd64f4e7fe5430c7fdd5bd269ccdf80dbea8116b59
action:     move generations/$RUN/graph.json aside
second run: exit 1, same run ID
after:      CURRENT names the same run; graph.json does not exist
summary:    generation.json still declares graph.json published
```

Exact command shape:

```sh
OUT=$(mktemp -d /tmp/bbnf-p3-review-a-export.XXXXXX)
LC_ALL=C TZ=UTC target/release/xtask architecture --output "$OUT"
RUN=$(tr -d '\n' < "$OUT/CURRENT")
mv "$OUT/generations/$RUN/graph.json" "$OUT/graph.json.removed"
LC_ALL=C TZ=UTC target/release/xtask architecture --output "$OUT"
test -f "$OUT/generations/$RUN/graph.json"
```

The final `test` failed. The second exporter nevertheless printed that an
atomic evidence generation had been published. Atomic creation for a new name
is a retained organ; atomic, self-verifying publication is falsified.

### 3. [P0] The `complete=true` source binding has unbound semantic inputs

There are two independent counterexamples.

First, filesystem inventory hashes a symlink's target text, not target content
(`inventory/filesystem.rs:163-192`), while the frontend helper treats a symlink
to `App.vue` as a file (`program.mjs:41-59`) and `vue.mjs:4-14` follows it with
`readFileSync`. A disposable package face used a constant absolute symlink
target outside the face. Mutating only the external target produced:

```text
readlink-text SHA-256 before/after
fa676e8b2a0a20dd24256e3abf72213270d2686d651a802eed03de90128cb8bf
fa676e8b2a0a20dd24256e3abf72213270d2686d651a802eed03de90128cb8bf

helper JSON SHA-256 before
a609ad7915bf41e13e1aceec55f567a2bbc0e7fe2a1a9d3fd4e523d09ac60e2b
helper JSON SHA-256 after
c7905fbce0cc32cdac7a93d6db38b10cb813ed66ec78b1ac6be1eef22c4484b2

new facts
VUE_BLOCK_SPAN_MISATTRIBUTED  App.vue  1:1
VUE_SCRIPT_SOURCE_OMITTED     App.vue  1:1
```

Thus graph output can change while the repository inventory, symlink worktree
content, helpers, tools, and declared configuration remain identical.

Second, `SourceBinding::capture` permits `manifest_hash` and `lock_hash` to be
`None` (`evidence/binding.rs:82-95`) and then unconditionally sets
`complete=true` (`binding.rs:150-176`). A declared context can therefore lack
its lock digest without making the binding incomplete. Cargo's live registry
index/network state can also alter the preserved Cargo diagnostic but is not a
bound input.

The current artifact's binding is useful and its two repository inventory
hashes are real, but the claim that it completely binds all inputs consumed by
the exporter is rejected.

### 4. [P0] Worktree identity, content, and nomination are not truthful

The fresh binding contained 79 real worktree records. The manifest contained
two product nominations. **Zero** nomination IDs matched a bound record:

```text
bbnf-primary        -> no bound worktree
parse-that-primary  -> no bound worktree
```

`mod.rs:451-498` first creates nodes for real records and then creates two new
phantom `Worktree` nodes named after nomination IDs, with exact
`ProductNominatesWorktree` edges to the phantoms. The product therefore does not
nominate a worktree whose HEAD, branch, dirty state, or content was measured.

Three disposable Git probes independently falsified the remaining claims:

1. `status --untracked-files=all` and `ls-files --others --exclude-standard`
   (`worktree.rs:133-165`) omit ignored content. Adding `!! ignored.txt` to an
   auxiliary worktree left both records byte-identical and `dirty=false`:

   ```text
   fixture-branch-aux  false  false  bdcd149ffbc03b4323324d79f79d1c7bb3d8f0a63625c571b54ee9c5e4b16e99
   ```

   Adding an ordinary untracked file correctly changed that record to dirty and
   changed its hash; untracked support is retained, ignored support is not.

2. Legal branches `feature/a` and `feature-a` both become
   `fixture-branch-feature-a` through `portable_key`; collection failed with
   `WORKTREE_IDENTITY_COLLISION`.

3. A detached worktree's identity includes the first 12 content-hash digits
   (`worktree.rs:104-125`). Adding one untracked file changed identity from
   `fixture-detached-5e145d7e86cb-bdcd149ffbc0` to
   `fixture-detached-5e145d7e86cb-324aa068003f`. Content mutation is therefore
   represented as deletion plus creation, not a stable identity with changed
   state.

These are current exact false ownership facts, not deferred semantic edges.

### 5. [P1] The manifest is typed syntax but not constellation authority

`manifest.rs:157-234` checks uniqueness and existence of several references,
but does not enforce reciprocal membership or repository consistency across
products, faces, contexts, and nominations. `resolve_repositories` also does not
require two distinct canonical roots (`manifest.rs:237-262`). A public-API
probe against the reviewed manifest returned:

```text
aliased_repositories_validate=Ok(())
aliased_roots_equal=true
cross_repo_product_validate=Ok(())
omitted_product_face_validate=Ok(())
cross_repo_nomination_validate=Ok(())
```

The mutations were: declare `parse-that` as `workspace_root`; move the
`bbnf-language` product identity to the parse-that repository without moving
its faces; remove `bbnf-playground` only from the product's face list; and point
the bbnf nomination at the parse-that repository. All should fail before
inventory. `contexts.len() >= 6` is likewise not proof of the four named Cargo
and two named frontend contexts.

The declarative TOML and typed deserialization are retained. Its authority and
completeness claims are rejected.

### 6. [P1] Inventory is unique for the emitted rows but incomplete by construction

The current artifact has good local invariants:

```text
records=10,574
unique physical triples=10,574
unique (repo,path)=10,574
files=8,985 directories=1,502 symlinks=87
exclusion-root records=29
records carrying any semantic-face link=21
```

However `walk_repository` records an excluded directory and returns without
visiting its children (`filesystem.rs:181-200`). The current repository has 16
tracked files beneath `.profiles/`; `git ls-files .profiles` lists profile
symbols, failure stderr, root-cause Markdown, and profile evidence. None has a
physical inventory row because `.profiles/**` is declared ephemeral. The
collector never reconciles exclusions with tracked/authored status, so a broad
pattern can erase authored evidence from the supposedly complete census.

Only package-face root and manifest records receive semantic-face links. The
remaining physical files have repository ownership but no package/context face.
Unique-row validation is GREEN; complete physical and semantic closure is RED.

### 7. [P1] SCC partitioning is sound for selected edges; the cycle basis and dynamic clusters are not

The Kosaraju-style SCC partition retains isolated nodes and is structurally
reasonable for the edge set supplied. Typed edge identities and replayable
witness chains are also real.

The function named `deterministic_cycle_basis` (`project/scc.rs:132-184`)
instead finds one return path per edge and deduplicates cycles. It does not test
linear independence or cycle-space rank. On a three-node complete directed
graph:

```text
edges=6 vertices=3 cycle_rank=4 emitted_basis=3
cycle_0_edges=2
cycle_1_edges=2
cycle_2_edges=2
```

The emitted three two-cycles cannot be a complete basis of rank four.
`validate.rs:99-126` verifies only that whatever witnesses exist replay and
close; it never verifies completeness or independence.

Dynamic clustering is also not W1/W5 clustering. `split_component`
(`cluster.rs:171-199`) repeatedly halves the sorted identity list until size
thresholds fit. Articulation, fan, ownership, feature, generated lineage, test
reachability, package face, and frontend feature do not choose boundaries;
`cluster_signals` labels already-chosen halves afterward, and its
`Articulation` signal means only “more than two nodes and any cut edge.” The
boundary quotient and typed cut records are deterministic retained organs, but
the grouping semantics are rejected.

### 8. [P1] Typed stage injection is a relabeled bootstrap failure, not stage proof

`requested_injection` is consumed at `mod.rs:149-156`, before manifest loading.
Every later `inject(stage)` is a no-op (`mod.rs:503-505`). Re-running all eleven
advertised injections produced the receipt's run IDs and three-file
generations, but **every binding had `complete=false` and the same six
unavailable domains**:

```text
stages: manifest inventory worktree cargo rust node frontend artifact projection validation write
each:   exit=1, files=3, binding.complete=false
missing: repositories, contexts, frontend-configs, tools, helpers, worktrees
```

An actual missing-`node` execution, with Cargo/Rust/Git still available,
reported:

```text
exit=1
generation stage=bootstrap
binding.complete=false
diagnostic="execute required tool `node`"
```

The promoted `node` test at
`architecture_p3_counterexamples.rs:297-315` only sets the relabeling variable;
the evidence test does the same for `manifest`. No test injects during a Cargo
subprocess, frontend decode, validation, generation write, directory fsync,
rename, existing-generation verification, or `CURRENT` replacement. The eleven
run IDs in `P3-EVIDENCE.md` are reproducible but do not prove the eleven stages
they name.

### 9. [P1] Privacy is clean for this artifact, but the validator is not universal

A literal scan of the fresh generation found zero `/Users/`, `/private/tmp/`,
`/private/var/`, `/tmp/`, or `file://` strings. Repository-relative paths and
the current artifact's privacy are retained.

`validate_portable_string` (`schema/identity.rs:223-241`) recognizes a small set
of textual absolute-path forms. It accepts absolute file URIs and encoded file
URIs outside those prefixes:

```text
CanonicalText::new("file:///etc/private-source")       = Ok
RepositoryId::new("file:///etc/private-source")        = Ok
CanonicalText::new("file:%2F%2F%2Fetc%2Fprivate-source") = Ok
```

Because recursive serialized-string validation delegates to this predicate,
the bank's `/Users` and `/private/tmp` fixture does not prove every serialized
string field is portable. Universal privacy validation remains RED.

### 10. [P1] Semantic quarantine is honest, but the “promoted counterexample bank” overclaims coverage

The current graph contains no exact Cargo, Rust, frontend, generated, WASM, or
call edge. This is the safest and strongest P3 choice: no P2 heuristic semantic
edge survived. The fresh required ledger remains 415 projection-qualified
failures, exactly distributed as documented:

```text
Cargo resolution       22
Rust semantics         60
Frontend semantics    325
Generated/WASM/call     4
Global hostile proof    4
total                  415
```

That quarantine does not execute most promoted counterexamples. The Rust bank
test at `architecture_p3_counterexamples.rs:148-173` never opens or compiles a
Rust fixture; it asks a manifest-face loop to emit fixed obligation codes and
checks that the collector always emits zero edges. `rg` found no P3 test
reference to `cargo-locked`, `cargo-same-name`, `rust-multi-super`, the older
Rust fixture, or `frontend-planes`. The frontend test verifies fixed/global
codes plus simple Vue/CSS text scans, not configured resolver behavior.

Therefore “no guessed exact semantic edge” is GREEN, while exact site
attribution, real failure-cause mutation, and the born-RED negative fixture bank
required by P2 are still unproved. Passing the static bank cannot close any
semantic clause.

### 11. [P2] Determinism is real, but performance, output size, and Goldilocks structure are not closed

Two full release computations over the same source produced the same run ID and
artifact hashes. This is retained determinism. The cost was:

```text
run 1: 69.73 s, maximum RSS 584,744,960 bytes
run 2: 63.78 s, maximum RSS 583,942,144 bytes
generation: 138,588 KiB
projections.json: 123 MiB
nodes=10,684 edges=10,711 inventory=10,574 projections=11 required=415
```

Every projection repeats all 10,684 identities, 10,684 SCC records, and 10,684
cluster records even when it has zero edges. Eight projections in this artifact
have zero semantic edges. This also makes unrelated node mutations change every
projection hash because `spec.rs:39-66` hashes all node records into every
projection. The current format is deterministic but not a local or practical
audit-shard artifact.

The source split is materially better than P2 and most modules are reasonable.
It does not meet the proposed topology exactly: `architecture/mod.rs` is 548
lines and contains roughly 290 lines of foundation fact construction rather
than orchestration only. `cluster.rs` combines partition policy, false semantic
signals, cut ownership, quotient construction, and hashing. The CLI still
promises DOT and a human-readable report (`main.rs:33-39`), but the P3 writer
emits neither. Goldilocks topology is improved, not closed.

## Exact executable record

### Clean gates

```text
cargo fmt -p xtask -- --check
  exit 0; real 0.13 s

cargo check -p xtask --locked
  exit 0; real 0.42 s

cargo test -p xtask --locked -- --test-threads=1
  exit 0; 1 hash unit + 12 architecture + 10 P3 bank + 3 metadata = 26 passed
  real 2.84 s

cargo test -p xtask --locked --test architecture_p3_counterexamples -- --test-threads=1
  run 1: exit 0, 10 passed
  run 2: exit 0, 10 passed
  raw logs differed only in Cargo/test elapsed-time lines

cargo clippy -p xtask --all-targets --no-deps --locked -- -D warnings
  exit 101
  pre-existing route: xtask/src/regen_simple_runtime.rs:473:10, useless_format
  log SHA-256 e7540041f7ec0e4bad9c9c5692886eeea6c21950294a6a4a88cb9e9cbfd43ee6
```

### Fresh full artifact before the corruption probe

```text
release binary
a83502242adbc8336d4e21229a17c3d931a587fa67a542547904ff284e078cb8  target/release/xtask

run ID
p3-48604d8cfa060d64739c53bd64f4e7fe5430c7fdd5bd269ccdf80dbea8116b59

source binding
2a60fe02e1d9579d1932bec6ee5d2633638c59e55a625d47daf1dc8682a561c0

db71d0ad93c946e7527028aaa46b3f19f905fad60370ce5e8d39a71a807eb2a5  binding.json
37517e5f3dc66819f61f5a7bb8ace1921282415f10551d2defa5c3eb0985b570  diagnostics.json
b785deba8bf7b35263fe6e0aae6984d78b0a9125973c0d4bb8f84e6624b3abb1  generation.json
17ce9bfa4056829b76f2a9005c6f70aa139b6e7280d1569dff761afa1692796c  graph.json
9ee26204a475b5cdbb7baac23591d947a407c1bfff12e18d31a65c141fd63c61  inventory.json
616ab935c015e4eaffce902f99dfb1a8766e48552af6de7bbba8920c6815bb65  manifest.json
b7ec7aa68692bc719ed65b44861ea159e4be3af7e9ed7eb9ec0844af246ea13e  obligations.json
320fe6bd807622e0609302f69b489b1209e20759432a7318a9c6617edbeea408  projections.json

exit=1; status=red; stage=complete
nodes=10,684; edges=10,711; inventory=10,574; required=415
forbidden literal path matches=0
```

The two-record drift from `P3-EVIDENCE.md` is bound by this generation's own
inventory/graph hashes. Detached receipt counts are not reused as truth.

## P2 clause disposition after P3

| # | P2 contract clause | P3 review-A disposition |
|---:|---|---|
| 1 | Closed typed schema | **NOT CLOSED.** Typed enums/newtypes are real; manifest cross-links, context graph identity, exclusion identity, documentation roles, and real worktree nomination are incomplete. |
| 2 | Stable identity | **NOT CLOSED.** Many repository-relative identities are stable; legal branch names collide, detached identity changes with content, phantom nominations split identity, and absolute file-URI identities pass. |
| 3 | Complete inventory and closure | **NOT CLOSED.** Current emitted rows are unique/kind-correct; tracked files under broad exclusions are absent, semantic faces link only roots/manifests, symlink-consumed content is unbound, and aliased repository roots pass. |
| 4 | Named typed projections | **NOT CLOSED.** Eleven named specs and SHA-256 hashes exist; two incomplete projections affirm movement trust during a RED generation. |
| 5 | Exact Cargo resolution | **NOT CLOSED by design.** Zero exact Cargo semantic edges and 22 required failures are honest. Successful metadata output is discarded, live diagnostic inputs are unbound, and context equality/checksums/spans remain Unknown. |
| 6 | Bounded Rust semantics | **NOT CLOSED by design.** Zero exact Rust edges prevents P2 false positives, but static face-level Unknowns do not execute or attribute the Rust counterexamples. |
| 7 | Frontend/build plane | **NOT CLOSED by design and source binding is contradicted.** Zero exact frontend edges is safe; 325 failures remain, and helper semantics can follow an unbound external symlink target. |
| 8 | CSS/WASM/generated planes | **NOT CLOSED by design.** They remain required Unknown with zero exact lineage; the conditional text scan is not parsed CSS truth. |
| 9 | Worktree plane | **NOT CLOSED.** HEAD/diff and ordinary untracked hashing are retained; ignored content, stable detached identity, legal branch uniqueness, and actual nomination fail. |
| 10 | Cycles and audit shards | **NOT CLOSED.** SCC partitioning and typed cut incidence are retained; the cycle basis is incomplete and clusters are sorted-size halves, not W1/W5 dynamic partitions. |
| 11 | Fail-closed truth | **NOT CLOSED.** Required Unknowns make the command nonzero, but corrupt generations are reused, movement trust can be true during RED, and advertised stage injections never reach their stage. |
| 12 | Executable proof | **NOT CLOSED.** Clean gates and same-source determinism pass; real hostile stage failures, complete fixture promotion, relocation/permutation/locale/mutation banks, ignored/symlink tests, publication corruption, and practical output bounds do not. |

Global disposition: **0 of 12 clauses closed.** Passing foundation organs do not
upgrade the global clause that contains them.

## Retained organs

Retain, with their narrow scope attached:

- the declarative versioned manifest format and typed deserialization;
- repository-relative `RepoPath`, closed current enums, `Exact` versus
  projection-qualified `Unknown`, and automatic Unknown expansion;
- SHA-256 known vectors, complete edge-record identity, deterministic sort and
  dedup, conflicting-node and dangling-endpoint rejection;
- the current artifact's 10,574 unique physical identities, kind checks, file
  content hashes, and zero forbidden literal checkout/temp paths;
- ordinary tracked diff and non-ignored untracked worktree hashing;
- zero P2 heuristic Cargo/Rust/frontend/artifact edges and the honest 415
  remaining required failures;
- singleton-preserving SCC partitioning for the selected edge set, typed
  replayable witnesses, typed internal/outgoing/incoming cut records, and a
  deterministic boundary quotient;
- projection/shard SHA-256 sensitivity to the complete records included in the
  implemented hashes;
- fsync-plus-rename creation for a previously absent generation and atomic
  `CURRENT` replacement when all preconditions hold;
- same-source deterministic run ID and bytes, and all passing clean gates above.

## Rejected claims

Reject as architecture truth or foundation proof:

- “foundation gates GREEN” as an aggregate status;
- “complete typed constellation” and manifest authority;
- complete, stable worktree identity/content and real product nomination;
- complete source binding, including symlink-consumed, ignored, missing-lock,
  registry/environment, and excluded inputs;
- universal serialized-string privacy;
- complete cycle basis or W1/W5 dynamic cluster semantics;
- atomic publication in the presence of a pre-existing run-ID directory;
- the eleven typed injection receipts as proof of eleven actual failure stages;
- the promoted bank as execution of the full born-RED counterexample families;
- `trustworthy_for_movement=true` in any current projection;
- every current exact nomination/ownership inference involving
  `bbnf-primary` or `parse-that-primary`;
- any source move, deletion, colocation, split, ownership decision, or
  architectural recommendation derived from P3.

## Smallest coherent P3.1 next step

This is a **P3.1 foundation repair**, not P4 and not collector expansion.

First land born-RED tests, using only disposable repositories/outputs, for the
exact counterexamples in findings 1–9: global RED movement latch; existing-run
corruption; aliased roots and reciprocal manifest links; real nomination
resolution; ignored, colliding, and detached worktrees; symlink target escape;
missing context locks; complete independent cycle rank; dynamic split signals;
real stage-local and mid-write failures; and file-URI privacy.

Then make the smallest implementation slice that turns only those foundation
gates GREEN:

1. make every movement flag false unless the **whole generation** is GREEN and
   a separate authorization gate is satisfied;
2. validate or reject every pre-existing content-addressed generation before
   replacing `CURRENT`, and place failpoints inside the real stages and write
   transaction;
3. require distinct canonical repository roots, reciprocal repository/product/
   workspace/face/context/nomination links, present file-kind-correct locks, and
   nominations that resolve to measured worktree records;
4. use stable collision-free worktree identity, declare and test ignored-content
   policy, and reject or content-bind symlink traversal;
5. replace the witness set with a rank-checked independent basis and keep
   clusters RED until boundaries are actually driven by the W1/W5 signals.

Do not begin Cargo, Rust, frontend, generated, WASM, call, or source-layout P4
work in that slice. After P3.1, rerun two fresh hostile critics and a fresh
adjudicator. Production/source movement remains forbidden regardless of the
foundation repairs until all semantic planes and hostile global banks are
GREEN.
