# Architecture exporter P3 — hostile review B

Status: **RED. The P3 foundation does not close any of the 12 P2 contract
clauses, and several claims presented as foundation-GREEN are contradicted by
the implementation and disposable counterexamples. No P3 artifact may
authorize graph-directed source movement.**

## Reviewer identity, scope, and independence

- Declared reviewer model: `gpt-5.6-sol`; reasoning effort: `xhigh`.
- This reviewer did not delegate.
- The review covered `P2-ADJUDICATION.md`, `P3-BASELINE.md`, `P3-EVIDENCE.md`,
  BB-A waves W0-W5, the complete P3 manifest/helper/source/test topology, and
  every architecture fixture under `xtask/tests/fixtures/architecture/`.
- `P3-REVIEW-A.md` and any other P3 reviewer session were not read.
- No implementation, fixture, ledger, evidence, lock, or production file was
  changed. Disposable probes and outputs were confined to `/tmp`.

## Snapshot discipline and measured facts

The checkout was already dirty and actively owned by other work. Exact counts
below are bound only to the named fresh run. The release binary used by the run
was byte-identical to the one named in `P3-EVIDENCE.md`:

```text
a83502242adbc8336d4e21229a17c3d931a587fa67a542547904ff284e078cb8  target/release/xtask
```

One fresh `LC_ALL=C TZ=UTC` release export produced:

```text
run id          p3-f984420ba4d77868c38449e3ef24de7ea32077ef248b8da661939dfa7af39ca4
source binding  224ddadd2820592fad447d92c4bd9c9eab4284aa8962f3419f6dac9ccf3d2a7c
exit             1, honestly RED
nodes            10,685
edges            10,712
inventory        10,575
projections      11
required         415 projection-qualified failures
generation size  138,600 KiB (135 MiB by `du -sh`)
wall time         67.95 s
user/system       49.46 s / 50.14 s
maximum RSS       585,351,168 bytes
```

The exact edge distribution was 54 containment edges, 10,656 ownership edges,
two documentation edges, and zero edges in Rust, Cargo-declared,
Cargo-resolved, frontend-build, runtime-delivery, test-overlay,
generated-provenance, and call projections. The required ledger retained the
same 22 Cargo, 60 Rust, 325 frontend, four generated/WASM/call, and four hostile
proof instances stated by `P3-EVIDENCE.md`.

Fresh artifact hashes were:

```text
1376d69aa6b19c3fb67c3ca7e53a932a18b9439bf0f552ff01719f1d1ad1af58  binding.json
37517e5f3dc66819f61f5a7bb8ace1921282415f10551d2defa5c3eb0985b570  diagnostics.json
666fd6c89a54f7dcf20675f17269a470fcb20a62b63a5c4addbc0e55029e3664  generation.json
b0af43ec5235c262b3474fe80f6ed4bdb553015ec8bd7faddbc8bfbe6c347e9c  graph.json
c86c67690548a873922168882e998cbd95712a329a7438e02f1746bcc33b612c  inventory.json
616ab935c015e4eaffce902f99dfb1a8766e48552af6de7bbba8920c6815bb65  manifest.json
b7ec7aa68692bc719ed65b44861ea159e4be3af7e9ed7eb9ec0844af246ea13e  obligations.json
74198117b9ea3d58fe10f0d890613379e603ecf7719b3b35231975ff29ea4f02  projections.json
```

The fresh generation contained no `/Users/`, `/private/tmp/`, `/tmp/`,
`file:///`, or `/home/` literal. This is a narrow fact about that artifact, not
proof of the privacy validator.

No fresh twin is claimed. Live coordination reported source-side drift during
the audit, including changing Value-side untracked/vnext observations; that
tree is not named by the P3 two-repository manifest. Neither that external
drift nor differences between this run and the earlier receipt are attributed
to exporter nondeterminism. A same-source twin is meaningful only for an
explicitly quiescent, bound snapshot.

Local gates measured before this review file was written:

```text
cargo test -p xtask --locked
  26 passed; 0 failed

cargo fmt -p xtask -- --check
  GREEN

cargo check -p xtask --locked
  GREEN

cargo clippy -p xtask --all-targets --locked -- -D warnings
  RED at pre-existing crates/egraph-derive/src/lib.rs:282

cargo clippy -p xtask --all-targets --no-deps --locked -- -D warnings
  RED at pre-existing xtask/src/regen_simple_runtime.rs:473

Cargo.lock before and after
  a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908
```

The test/check/format facts are retained. The strict clippy failures are routed
outside the P3 implementation slice, but the repository's strict local gate is
not clean.

## Numbered severity findings

### 1. **BLOCKER — a globally RED generation serializes positive movement authority**

**Measured fact.** `project/spec.rs:56-75` computes
`trustworthy_for_movement` from only the failures assigned to that individual
projection. The fresh generation is globally RED with 415 required failures,
yet serialized:

```text
containment    required=0  trustworthy_for_movement=true
documentation required=0  trustworthy_for_movement=true
all other nine projections                           =false
```

There is no global movement-authority field that overrides those two positive
flags. The documentation plane is additionally built from manifest declarations
whose physical document paths are not validated, as finding 5 shows.

**Judgment.** This contradicts both the P2 prohibition and the P3 receipt's
statement that no P3 projection is movement authority while the architecture is
RED. A consumer following the explicit field can treat two incomplete views as
movement-safe. Clauses 4, 11, and 12 remain open.

**Small correction.** Make movement authority a generation-level fail-closed
decision. While any required obligation, failed hostile proof, incomplete
binding, or unresolved reviewer finding exists, every projection and cluster
must serialize movement authority as false. If a per-projection structural
status is useful, rename it so it cannot be confused with authorization.

### 2. **HIGH — repeated-output reuse can publish an incomplete generation as current**

**Measured fact.** `evidence/write.rs:154-175` skips all file verification when
`generations/<run-id>` already exists and still atomically replaces `CURRENT`.
A disposable manifest-stage failure run was published, its
`diagnostics.json` was moved aside, and the identical command was rerun against
the same output root. Both commands selected
`p3-37d025cca3ef913e9617d344add8376a1849e8fc19726a2f3d14d0b2bd389119`;
after the second command `CURRENT` pointed at a directory containing only
`binding.json` and `generation.json`. `diagnostics.json` remained absent.

**Judgment.** The staging-and-rename path is a useful atomic skeleton, but
publication is not fail-closed under repeated output use, preexisting/corrupt
generations, or recovery. The command can report that a complete generation was
published when the current generation is incomplete. Clauses 11 and 12 remain
open.

**Small correction.** Before reusing a run ID, byte-verify the exact expected
file set and every hash in `generation.json`. Treat any missing, extra, or
mismatched file as a typed write-stage failure; rebuild through a fresh staging
directory and atomic replacement, or quarantine the corrupt directory. Add a
repeated-output corruption fixture.

### 3. **HIGH — emitted cycle evidence is not a cycle basis, and clustering is not the BB-A dynamic partition**

**Measured fact.** `project/scc.rs:132-184` finds one return path for each edge,
deduplicates the resulting cycles, and labels the set `cycle_basis` without an
independence or rank test. `evidence/validate.rs:99-126` checks only that each
witness replays and closes. A disposable complete bidirected three-node graph
had six typed Rust edges and cycle-space dimension `6 - 3 + 1 = 4`; P3 emitted
three two-edge witnesses whose GF(2) rank was three:

```text
nodes=3 edges=6 cycle_space_dimension=4 witnesses=3 witness_rank=3
```

The two-node production test at `tests/architecture/project.rs:91-106` asserts
only non-emptiness and typed IDs, so it cannot detect this defect.

Separately, `project/cluster.rs:171-198` recursively bisects sorted member IDs at
the 24-node/80-edge thresholds. Ownership, feature, generated-lineage, test,
frontend, fan, and alleged articulation signals are computed only after the
membership split (`cluster.rs:201-237`); they label a shard but do not determine
the partition. No articulation algorithm exists, smaller compatible clusters
are never merged, and split shards carry no shard-local basis.

**Judgment.** SCC membership for the supplied edge set is structurally useful,
and typed replayable witness edges are an improvement over P2. Completeness and
BB-A.W5 dynamic clustering are false, however. Clause 10 remains open, and the
absence of `CYCLE_BASIS_INCOMPLETE` and `DYNAMIC_SHARD_PARTITION_ABSENT` from
the fresh RED ledger is itself incorrect.

**Small correction.** Build a deterministic independent fundamental cycle basis
and verify rank/completeness from emitted edges. Drive shard membership from the
required articulation, fan, ownership, feature, lineage, package-face, test, and
frontend signals; then enforce size recursively and emit a replayable quotient
and shard-local cycle evidence. Promote the three-node counterexample and
one-field signal movement fixtures.

### 4. **HIGH — the claimed complete source binding does not bind every input collectors read**

**Measured fact.** Inventory hashes a symlink's target text only and does not
follow it (`inventory/filesystem.rs:165-192`). The frontend walk treats a
symlink to a CSS file as a file (`frontend/program.mjs:41-59`), and
`css.mjs:4-6` follows it with `readFileSync`. In a disposable package face,
`external.css -> ../../outside.css` escaped the declared repository. Changing
only the external target content left the symlink text and symlink hash equal,
but changed the helper's `CSS_IMPORT_CLASSIFICATION_UNSOUND` count from one to
zero. That collector output is therefore not bound by the repository inventory.

`SourceBinding::capture` also sets `complete=true` unconditionally even though
context manifest/lock hashes are optional (`evidence/binding.rs:83-94,150-176`).
The fresh binding identified `frontend-bbnf-root` with `Cargo.toml` and
`frontend-parse-that-typescript` with `rust/Cargo.toml`, because frontend
contexts inherit the Rust workspace manifest rather than their package-face
manifest. Binding is captured before Cargo and frontend read live files
(`architecture/mod.rs:175-204`), and there is no end-of-run recensus. An
in-repository write between capture and collection can likewise produce output
from bytes different from those named by the binding.

The fresh Cargo failure text also reported `Updating crates.io index`; registry
and network state are not bound. Successful metadata output is discarded, so
this mutable input currently adds cost and failure variability without an exact
fact.

**Judgment.** The current artifact is content-sensitive for ordinary inventoried
files and honestly keeps full relocation/mutation proofs RED, but it is not a
complete single source snapshot. Clauses 2, 3, 5, 7, 11, and 12 remain open.

**Small correction.** Reject symlinks that escape a repository/package face, or
type and bind the resolved target identity and bytes. Give each context its real
manifest/config/lock identities, require every claimed hash to be present before
setting `complete`, and make collectors consume an immutable captured input set
or perform a final recensus and fail/retry on drift. Run Cargo hermetically or
bind the registry/network state it is allowed to consume.

### 5. **HIGH — manifest and inventory validation check existence, not the required relationships**

**Measured fact.** `manifest.rs:157-234` verifies that referenced IDs exist, but
does not verify that a product's listed workspace and faces belong to the same
repository/product, that the product and face links are reciprocal, or that a
documentation path exists with file kind. Three in-memory mutations of the
loaded current manifest all returned `Ok(())`:

```text
bbnf-language lists parse-that-rust workspace       Ok(())
bbnf-root-rust names parse-that-language product    Ok(())
p2-verdict-authority path = definitely/missing.md   Ok(())
```

Documentation nodes are emitted without any edge to the declared physical path
(`architecture/mod.rs:383-407`). `Inventory::attach_faces` checks only path
presence and `Inventory::validate` checks generic content-hash/kind consistency
(`inventory/filesystem.rs:58-135`); it does not require a package root to be a
directory, a package manifest to be a file, or every attached semantic face ID
to be declared and repository-compatible. The current manifest happens to use
directory roots and file manifests, which is a retained artifact-local fact.

**Judgment.** The declarative manifest is a valuable replacement for P2
hardcoding, but deleting/misrouting declared authorities and crossing product
ownership can pass its advertised first gate. Clauses 1 and 3 remain open.

**Small correction.** Validate the manifest as a reciprocal relational schema:
repository -> product -> workspace/face, face -> product/workspace/repository,
context -> exact face/config/lock, document role/supersession -> existing
physical file, exclusion -> supported matcher, and nomination -> resolved
worktree. Enforce expected physical kinds and reject undeclared semantic-face
attachments before collection.

### 6. **HIGH — product worktree nominations point to synthetic, unowned nodes**

**Measured fact.** The fresh binding observed 79 real worktrees. The manifest
nominates `bbnf-primary` and `parse-that-primary`; neither identity occurs in
those 79 records. `architecture/mod.rs:451-498` first creates nodes for observed
records, then creates two unrelated Worktree nodes from nomination IDs and
points `ProductNominatesWorktree` at them. Both nomination targets lack a
`RepositoryOwnsWorktree` edge and carry no observed HEAD, branch, dirty, or
content state.

The underlying identity scheme is also not stable/total for all valid Git
worktree sets. Branch identities use a lossy punctuation normalizer, while a
detached identity includes HEAD plus the first 12 characters of mutable content
state (`inventory/worktree.rs:104-125`). Two clean detached worktrees at the same
HEAD collide; changing dirty content changes a detached worktree's identity.
The collector fails closed on such a collision, but it does not satisfy stable
enumeration.

**Judgment.** Tracked and untracked content hashing and command-error propagation
are useful retained organs. Product nomination and stable worktree identity are
not established. Clause 9 remains open.

**Small correction.** Resolve each nomination selector to exactly one observed
worktree record and place the product edge on that identity; absence or
ambiguity must fail. Derive a stable portable identity from a repository-local
Git worktree administrative identity, keeping HEAD/branch/content as mutable
attributes rather than identity material.

### 7. **MEDIUM — the all-string privacy walk uses a host-prefix blacklist, not path detection**

**Measured fact.** `schema/identity.rs:223-264` does visit every serialized
string, which repairs the P2 field-coverage omission. Its detector rejects a
whole absolute string and a few macOS prefixes, but accepts embedded absolute
paths on other hosts. Fresh probes returned `Ok` for:

```text
failed at /home/reviewer/secret.rs
source file:///home/reviewer/secret.rs
failed at /var/folders/reviewer/secret.rs
```

The counterexample bank at
`tests/architecture_p3_counterexamples.rs:238-255` checks only whole
`/Users/...` and `/private/tmp/...` strings. The fresh full artifact had none of
the searched private literals, which remains a narrow positive observation.

**Judgment.** Serialized path privacy and full relocation are not proved.
Clauses 2, 11, and 12 remain open.

**Small correction.** Parse/tokenize path-like substrings and file URLs using a
platform-independent rule, then validate every field and diagnostic against the
resolved repository/temp roots plus generic POSIX, Windows, UNC, and URI forms.
Add embedded-path and Linux/Windows relocation fixtures.

### 8. **MEDIUM — typed stage injection never reaches the requested stage**

**Measured fact.** `architecture/mod.rs:149-156` reads
`BBNF_ARCHITECTURE_FAIL_STAGE` once and returns before manifest loading for
every requested stage. All later `inject(stage)` calls are no-ops
(`mod.rs:503-505`). A `cargo` injection published `stage=cargo`, but its binding
was minimal (`complete=false`), and manifest, inventory, graph, obligations, and
projections were all unavailable. It did not inject a Cargo collector failure.
Likewise, the advertised `write` case does not fail inside staging, file fsync,
generation rename, or `CURRENT` replacement.

The tests at `tests/architecture/evidence.rs:6-26` and
`tests/architecture_p3_counterexamples.rs:297-315` check only the requested
stage label and presence of three files. They therefore pass for the bootstrap
short circuit.

**Judgment.** Typed failure generations for genuine early bootstrap failures are
useful, but the stage-injection receipt does not prove collector, projection,
validation, or write failure reporting. Clauses 11 and 12 remain open.

**Small correction.** Make `inject(stage)` compare against the requested stage
at each real boundary, remove the bootstrap short circuit, and add injectable
I/O operations inside publication. Assert which prior artifacts and complete or
minimal binding must exist for every stage, including corrupt-output reuse and
actual write failures.

### 9. **MEDIUM — the promoted semantic bank proves blanket quarantine codes, not site-specific counterexamples**

**Measured fact.** The Rust collector never opens a Rust source file; it emits
the same nine manifest-anchored obligations for each Rust face
(`collect/rust.rs:10-78`). Its tests do not pass the Rust fixtures to a resolver;
they only assert zero exact edges and presence of enum codes. The frontend
helper unconditionally adds nine package-anchored obligation codes for every
frontend face (`frontend/program.mjs:11-21`), whether or not the corresponding
syntax exists. The frontend tests likewise assert only empty `edges` and code
presence, not exact site, applicable context, positive resolution, or absence of
false extras. The Cargo same-name/locked, Rust multi-super, and older frontend
plane fixtures are not exercised by the P3 source-isomorphic tests.

The approach does safely remove P2's false exact Rust/frontend edges, and the
415 required failures honestly prevent overall success. It does not establish
the receipt's stronger claim that the promoted bank proves site-specific
quarantine or semantic counterexample handling.

**Judgment.** This is a correct fail-closed foundation posture, not closure of
Cargo, Rust, frontend, CSS/WASM/generated, or call semantics. Clauses 5-8 and 12
remain open.

**Small correction.** Preserve blanket capability boundaries if useful, but
distinguish them from site obligations. Execute every named fixture through the
owning collector, assert exact source/context/projection attribution, exact
positive and exact-negative sets, mutation inversion, and absence when the
construct is removed. Do not count an unconditional code as semantic coverage.

## P2 contract disposition

Passing foundation tests cannot close a clause contradicted by a negative
fixture or kept RED by an applicable Unknown.

| # | P2 contract clause | Independent P3 ruling |
|---:|---|---|
| 1 | Closed typed schema | **NOT CLOSED.** Closed enums/newtypes are real, but manifest relationships are not closed, `Attribute::Detail` remains an arbitrary string pair, identity-kind/source-span/context relationships are not globally validated, and semantic identities remain absent from exact facts. |
| 2 | Stable identity | **NOT CLOSED.** Repository-relative physical identities and complete edge digests are useful. Detached worktree identity is content-derived/colliding, privacy detection is prefix-based, symlink targets escape the binding, and full relocation remains required RED. |
| 3 | Complete inventory and closure | **NOT CLOSED.** The fresh artifact has 10,575 unique observed records with file/directory/symlink kinds and explicit exclusions. Cross-owner manifest links, missing documents, semantic attachment validity, required physical kinds, escaped symlinks, and a coherent concurrent snapshot are not closed. |
| 4 | Named typed projections | **NOT CLOSED.** Eleven typed specs retain all singleton nodes and semantic hashes bind selected node/edge records. Two planes nevertheless advertise movement trust in a globally RED run, and nine semantic planes have zero edges. |
| 5 | Exact Cargo resolution | **NOT CLOSED.** Four typed contexts and lock bytes are bound, and two Cargo causes are retained. No Cargo declaration/resolution/feature fact is emitted, successful metadata is discarded, frontend context manifests are wrong, registry state is unbound, and 22 required instances remain. |
| 6 | Bounded Rust semantics | **NOT CLOSED.** P2 guesses are gone, but the collector reads no Rust source, emits no exact Rust fact, and leaves 60 required instances. The fixture bank proves code presence rather than bounded resolver behavior. |
| 7 | Frontend/build plane | **NOT CLOSED.** No guessed exact edge survives and Vue/CSS file scans produce some site anchors. Configured programs, applications/tests, aliases/globs/queries/routes/imports remain 325 required instances; blanket codes are not site-specific proof, and symlink reads escape binding. |
| 8 | CSS/WASM/generated planes | **NOT CLOSED.** CSS sites are quarantined, but parsing and exact provenance are absent. Generated/WASM/call projections have zero exact edges and four required instances. |
| 9 | Worktree plane | **NOT CLOSED.** Seventy-nine observed records carry HEAD/branch/detached/dirty/content state and command errors fail. The two product nominations point to synthetic unowned nodes, and identity is not stable/total. |
| 10 | Cycles and audit shards | **NOT CLOSED.** SCC membership and typed replayable edges are useful, but the K3 witness set is rank-incomplete and threshold bisection is not the required dynamic partition. No corresponding required RED is emitted. |
| 11 | Fail-closed truth | **NOT CLOSED.** Unknowns expand per applicable projection and the command exits 1. Positive movement flags, corrupt generation reuse, unbound collector inputs, incomplete privacy, and bootstrap-only stage injection contradict global fail-closed truth. |
| 12 | Executable proof | **NOT CLOSED.** Twenty-six tests, format, check, hashes, and an honest RED export pass, but tests miss the reproduced manifest, cycle, cluster, worktree, symlink, privacy, repeated-output, and real stage-failure cases. Relocation, full permutation/mutation, locale, and strict repository gates remain RED/unproved. |

**Result: 0 of 12 P2 clauses close.**

## Organs retained and rejected

### Retain, with their scope attached

- The versioned manifest as the single declarative topology input, after its
  relationship and physical-closure validator is repaired.
- Closed enum/newtype scaffolding; canonical relative physical identities; the
  pure SHA-256 implementation and known vectors.
- `Exact(fact)` versus `Unknown(obligation)` and deterministic expansion of one
  Unknown into every named applicable projection failure.
- Sorted inventory walking, explicit file/directory/symlink kinds, ordinary file
  content hashing, explicit exclusion records, tracked/untracked worktree content
  hashing, and command-error propagation.
- Complete edge-record identities, projection SHA-256 over versioned spec plus
  selected node/edge records, projection-qualified component IDs, typed cut
  roles, complete cluster-record hashes, and quotient plumbing.
- SCC partition membership and typed replayable cycle edges, but not the emitted
  set's `cycle_basis` name or completeness claim.
- Generation staging, fsync, directory rename, typed unavailable artifacts, and
  failure diagnostics as an atomic-publication skeleton, but not preexisting
  generation reuse.
- The clean removal of P2 heuristic exact Rust/frontend edges and all 415
  projection-qualified required failures as honest reasons to stay RED.
- The fresh run/hash/count/runtime facts and passing local tests, bound only to
  the named observations above.

### Reject as architecture truth or authorization

- Every `trustworthy_for_movement=true` value from a globally RED generation.
- `SourceBinding.complete=true` as proof of an immutable, hermetic input
  snapshot; full relocation/determinism claims outside an explicitly quiescent
  bound twin.
- Manifest completeness, reciprocal ownership, documentation authority, and
  semantic/inventory closure as global invariants.
- Synthetic nomination Worktree nodes as product worktree truth, and current
  worktree IDs as a stable total identity scheme.
- The K3-incomplete witness set as a cycle basis and sorted threshold bisection
  as BB-A.W5 dynamic clustering.
- The eleven-stage injection receipt as proof of failures at those real stages,
  and the current counterexample bank as site-specific semantic coverage.
- Cargo, Rust, frontend, runtime, test, generated, WASM, call, cycle, cluster,
  reachability, or movement judgments from projections containing no exact
  semantic edges or unresolved applicable Unknowns.
- Any exact count or hash detached from its run ID and source-binding receipt.

## Smallest coherent correction sequence

1. **Freeze authorization immediately.** Force global and per-projection
   movement authority false; add required RED instances for
   `CYCLE_BASIS_INCOMPLETE`, `DYNAMIC_SHARD_PARTITION_ABSENT`,
   `PRODUCT_WORKTREE_UNNOMINATED`, `PRIVACY_VALIDATOR_INCOMPLETE`,
   `SOURCE_STATE_MANIFEST_ABSENT`, `COLLECTOR_FAILURE_EVIDENCE_INCOMPLETE`, and
   `EVIDENCE_PUBLICATION_NONATOMIC` until their new negative gates pass.
2. **Repair the exact foundation before semantic breadth.** Enforce reciprocal
   manifest/context/document/physical relationships, resolve nominations onto
   observed stable worktree identities, reject or fully bind symlinks, detect
   generic embedded paths, and recensus or snapshot collector inputs.
3. **Repair structural proof.** Replace path-per-edge witnesses with a verified
   deterministic independent cycle basis; make the required dynamic signals
   determine shard membership and verify quotient/cut/basis reconciliation.
4. **Repair evidence execution.** Verify or atomically reconstruct an existing
   generation, inject at actual collector/kernel/write boundaries, and promote
   corruption, concurrency, and write-failure fixtures.
5. **Then promote semantics one plane at a time.** Turn the existing broad
   Unknown boundaries into exact site/context facts only when exact-positive,
   exact-negative, mutation, relocation, and full-repository gates pass. Keep all
   other obligations RED; do not erase them by unconditional code emission.

This is the smallest coherent correction because it preserves the useful P3
clean break and typed substrate while removing false authority first. It does
not authorize source movement. BB-A.W2-W5 remain blocked until all 12 clauses,
two fresh hostile audits, and the required fresh adjudication close on an
immutable, source-bound generation.
