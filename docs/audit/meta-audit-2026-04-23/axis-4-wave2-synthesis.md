# Axis 4 — Wave 2 Synthesis Quality

## Verdict

Wave 2 synthesis is directionally strong but not execution-ready as written.

The good news is real: the synthesis converges on the right broad shape.
`B1` is framed as infra-first, `repo-modernization/INDEX.md` gets the
pin-before-port sequencing mostly right, `BA` is properly struct-tree-first,
and `BB` is correctly trying to keep the VM in a bounded oracle role rather
than resurrecting the deleted DTA/PSI runtime.

The bad news is also real: the synthesis still contains enough stale or
internally contradictory detail that it should not be dispatched blindly.
The main failures are:

1. `B1`'s concrete rewrite did not fully absorb the earlier 12-step migration
   draft; old wave names and artefact names remain live.
2. The fleet-modernization plan still smears appurtenant work into "during
   B1" even though `B1`'s own wave specs explicitly narrowed that scope.
3. `BA` is architecturally sound but not dispatchable as written because its
   file bounds target a workspace root that is not a package.
4. `BB` still carries tape-era language, the known `ir-rewrites` module/crate
   drift, and stale BC wave files in the `BB` wave directory.

Method: audited `docs/tranches/B1/{B1.md,TOOLCHAIN-MIGRATION.md,waves/W0-W3.md}`,
`docs/tranches/next-tranche-research/repo-modernization/{INDEX.md,parse-that.md,pprint.md,crates-core.md,crates-csp-solver.md,csp-solver-sibling.md,crates-gorgeous.md,gorgeous-sibling.md}`,
`docs/tranches/BA/{BA.md,waves/W0-W3.md}`,
`docs/tranches/BB/{BB.md,waves/W0-W6.md}`, and spot-checked live repo state
plus sibling filesystem state on 2026-04-23.

## Verified

### 1. B1 does put the ICE-clean gate before the bench-harness port

This is one of the most important synthesis decisions, and it checks out.

- `B1.md` invariant 14 requires pin + clean `iter-check-full` before any divan
  port begins, and invariant 15 requires one-shot removal of `bencher` with no
  dual-harness window (`docs/tranches/B1/B1.md:92-100`).
- The wave summary maps Steps 1-4 to `W0`, Steps 5-8 to `W1`, Steps 9-11 to
  `W2`, and Step 12 to `W3` (`docs/tranches/B1/B1.md:116-121`).
- `B1.W0` makes the close-gate explicit: pinned nightly, alias surface,
  nextest surface, and a clean `iter-check-full` run with zero
  `target/rustc-ice-*.txt` before `W1` opens (`docs/tranches/B1/waves/W0.md:156-216`).
- `B1.W1` then opens only after `W0` is green, including the ICE-clean gate
  and the `iter-check-full-cold-pinned` proof row
  (`docs/tranches/B1/waves/W1.md:22-31`).

That sequencing is sound. It reflects the right lesson from the audit:
stabilize the substrate and command truth before touching secondary perf
instrumentation.

### 2. The modernization program mostly respects the right macro-order

At the 10,000-foot level, the repo-modernization index gets the order right:
pin and execution-surface normalization first, bench-framework migrations
second, structural refactors last.

- `INDEX.md` puts pinning and basic config coherence in Phase A, bench ports
  in Phase B, and structural items like bench-architecture split / XDG cache
  lift / watt in Phase C (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:119-200`).
- The critical gate is correctly identified: no divan migration before the
  B1 pin lands (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:109-115`).
- This order is echoed in the per-repo docs for `parse-that`, `pprint`,
  `crates/csp-solver`, and `crates/gorgeous`, even where their boundaries need
  tightening (see below).

This is the right macro-architecture for modernization. The issue is not the
phase order; the issue is scope leakage across those phases.

### 3. The csp-solver "workspace authoritative for benches" decision is the right one

The recommendation in `INDEX.md §7` is technically sound and is supported by
live repo state.

- `INDEX.md` recommends that the workspace copy own the fleet bench gate while
  the sibling remains the binding-oriented host (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:240-275`).
- `crates-csp-solver.md` correctly records that the workspace copy currently
  has no benches and therefore no local performance gate
  (`docs/tranches/next-tranche-research/repo-modernization/crates-csp-solver.md:11-27`).
- The live filesystem confirms that absence: `crates/csp-solver/` has no
  `benches/*.rs` today.
- The live sibling confirms the missing coverage source: the csc411 sibling
  currently carries 6 `csp-solver` benches and 2 `morph-core` benches.
  The audited paths on 2026-04-23 were:
  `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/benches/{assignment,cost_finite_domain,lattice,map_coloring,queens,sudoku}.rs`
  and
  `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/morph-core/benches/{align,primitives}.rs`.

The fundamental argument holds: because bbnf-lang consumes the vendored
workspace copy, that copy needs the authoritative bench surface.

### 4. BA’s core architectural direction is correct

The thesis in `BA.md` is materially aligned with the tape-abrogated substrate.

- `BA` is explicitly struct-tree-native: no tape cursor, no `TapeOffset`, no
  paired open/close walk (`docs/tranches/BA/BA.md:23-32`).
- Path typing is grammar-derived from `StructRegistry`, not hand-authored per
  grammar (`docs/tranches/BA/BA.md:15-20`, `90-103`).
- The path engine is intentionally shared across bindings (`docs/tranches/BA/BA.md:43-48`, `96-99`, `296-316`).
- The parent-pointer decision is deferred to measurement rather than asserted
  by taste (`docs/tranches/BA/BA.md:214-251`).

That is the right substrate story. BA is not trying to smuggle tape back in.

### 5. BB’s high-level e-graph-first / VM-residue idea is the correct salvage of the VM

The core idea behind `BB` is sound.

- `BB` correctly treats rule inference as an `IrNode` enumeration problem and
  positions the e-graph as the fast-path proof substrate
  (`docs/tranches/BB/BB.md:22-37`).
- It also correctly scopes the VM to a narrow oracle role rather than a
  runtime architecture (`docs/tranches/BB/BB.md:50-59`, `98-117`, `184-204`).
- Rule storage is correctly placed outside `crates/core`, with fleet-wide
  rules in IR-local storage and grammar-specific rules colocated with the
  grammar (`docs/tranches/BB/BB.md:119-182`).

Conceptually, this is the right answer to the "what survives from Era V?"
question: not the runtime walker, but the oracle-capable VM as a bounded
validation tool.

## Refined

### 1. The 4-wave B1 rewrite is conceptually correct, but the migration draft is only partially normalized

The wave mapping itself is good. The supporting migration document is not fully
updated to match it.

- After enumerating all 12 steps, `TOOLCHAIN-MIGRATION.md` still says
  `Total B1.W0 budget` even though the listed steps now span `W0` through `W3`
  (`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:263-273`).
- The validation section still names divan artefacts as `post-B1-W0-divan-*`
  and nextest close output as `post-B1-W0-nextest-close.json`
  (`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:295-323`), while the tranche plan
  and wave docs place divan in `W1` and the aggregate matrix in `W3`
  (`docs/tranches/B1/B1.md:118-121`, `docs/tranches/B1/waves/W1.md:220-229`,
  `docs/tranches/B1/waves/W3.md:92-149`).

This is not a thesis error. It is a normalization error. But it matters,
because B1 is supposed to be the execution-truth tranche, not a pile of
half-renamed migration notes.

### 2. The parent-pointer decision space is identified correctly, but option 1 is phrased too loosely

`BA.md` has the right three-way choice set:

1. embedded parent data,
2. root traversal,
3. hybrid sidecar (`docs/tranches/BA/BA.md:220-243`).

That said, the "in-struct parent pointer" option currently conflates raw
references and IDs (`parent: Option<&Parent>` or `parent_id: NodeId`).
Those are not equivalent implementation classes under Rust ownership.

The real comparison set should be stated as:

- embedded `NodeId` / arena index,
- root traversal,
- sidecar index.

That refinement does not change the likely outcome. It makes the design space
technically honest.

### 3. The gorgeous retirement decision is correct, but the execution story is stale

The recommendation to retire the sibling mirror is well-supported by the
documented drift (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:204-236`,
`docs/tranches/next-tranche-research/repo-modernization/gorgeous-sibling.md:39-62`).

However, the live filesystem check on 2026-04-23 found no
`/Users/mkbabb/Programming/gorgeous` directory at all. That means the
modernization text should stop describing retirement as a pending B1-era
action item and instead record that the sibling is already absent or already
effectively retired.

The decision is good. The operational description is behind reality.

### 4. The csp-solver reconciliation logic is sound, but the per-repo doc still hedges the decision

`INDEX.md` makes a real decision: workspace-authoritative benches
(`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:240-275`).

`crates-csp-solver.md` then weakens that decision by re-opening the question as
"optionally: make the workspace copy the authoritative source"
(`docs/tranches/next-tranche-research/repo-modernization/crates-csp-solver.md:28-39`).

That hedge should be deleted. This is exactly the kind of "decision as
optionality" softening that caused drift elsewhere.

## Flawed

### 1. B1 and TOOLCHAIN-MIGRATION still disagree about what B1 actually is

This is the clearest synthesis defect in the B1 rewrite.

- `B1.md` says B1 is a 4-wave tranche with `W1` owning the divan port and
  `W3` owning the aggregate close matrix
  (`docs/tranches/B1/B1.md:104-121`).
- `TOOLCHAIN-MIGRATION.md` still labels the 12-step total as `B1.W0`,
  still names divan outputs under `post-B1-W0-divan-*`, and still treats
  nextest-close as a `W0` artefact (`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:273`,
  `295-323`).

This is not a cosmetic mismatch. It means the concrete rewrite did not fully
absorb the migration draft it claims to supersede.

### 2. The fleet-modernization plan is inconsistent about what lands "during B1"

The synthesis currently tells two incompatible stories.

- `B1.W2.c` explicitly narrows B1's cross-repo scope to `../parse-that` and
  `../pprint`, and explicitly says gorgeous sibling / csp-solver / wider fleet
  work is out of scope for B1 (`docs/tranches/B1/waves/W2.md:173-205`).
- `INDEX.md` still says Phase A "during bbnf-lang B1" includes parse-that,
  pprint, gorgeous retirement, and csp-solver sibling setup
  (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:119-145`).
- `parse-that.md` and `pprint.md` each further treat CI rewrites and
  `.config/nextest.toml` installs as Phase A / B1-coupled work
  (`docs/tranches/next-tranche-research/repo-modernization/parse-that.md:67-84`,
  `docs/tranches/next-tranche-research/repo-modernization/pprint.md:63-77`).

Either B1 owns only the narrow triad pin/config mirroring, or it owns
appurtenant CI / nextest / sibling retirement work. The docs currently claim
both.

### 3. The repo-modernization bench count-up is internally wrong

`INDEX.md §8` does not survive arithmetic.

- It says 39 sites are ported in Phase B from existing benches, with origins
  `32 bencher, 4 test::Bencher, 8 criterion`
  (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:301-305`).
  That origin sum is 44, not 39.
- It then says pre-migration total is 63 existing sites, "minus 2
  gorgeous-sibling benches that relocate, plus 3 new = 61 final"
  (`docs/tranches/next-tranche-research/repo-modernization/INDEX.md:307-309`).
  That arithmetic is also wrong: `63 - 2 + 3 = 64`, not 61.

This section is not reliable in its current form. It needs to be rebuilt from
actual per-repo bench counts.

### 4. BA is not dispatchable as written because its file bounds target a non-package workspace root

This is the biggest execution flaw in BA.

- `BA.W0` places the micro-bench and tests at `benches/parent_pointer_strategies.rs`
  and `tests/path_type_errors.rs`
  (`docs/tranches/BA/waves/W0.md:31-40`).
- `BA.W1` places its bench and parity test at `benches/path_extract.rs` and
  `tests/path_parity.rs` (`docs/tranches/BA/waves/W1.md:27-34`).
- `BA.W2` again places integration tests at `tests/path_isomorphic.rs`
  (`docs/tranches/BA/waves/W2.md:28-37`).
- `BA.W3` continues to extend root-level `benches/path_extract.rs` and
  `tests/path_parity.rs` (`docs/tranches/BA/waves/W3.md:24-31`).

But the live root `Cargo.toml` is a workspace-only manifest, not a package,
and the repo currently has neither a root `benches/` nor a root `tests/`
directory. Those files would not be runnable integration/bench targets on the
current workspace structure.

BA therefore needs a concrete home before dispatch:

- either `crates/core/{benches,tests}/...`,
- or a new dedicated path crate,
- or another package with an actual `Cargo.toml`.

Until that is fixed, BA is not executable as a tranche spec.

### 5. BA still carries benchmark-surface drift from the pre-B1 world

Two conflicting harness stories remain live.

- `BA.W1` says the bench harness is "`vitest` / `criterion` as appropriate per
  the existing workspace convention"
  (`docs/tranches/BA/waves/W1.md:64-69`).
- But `B1`'s entire toolchain rewrite makes `divan` the bench surface and
  explicitly rejects criterion for the primary workflow
  (`docs/tranches/B1/B1.md:43-46`,
  `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:27-37`).
- Separately, `BA.md`'s W3 summary still says the final matrix is
  `cold / warm` (`docs/tranches/BA/BA.md:188-191`), while `BA.W1` and `BA.W3`
  both say cold-only per `feedback_no-warm-benches`
  (`docs/tranches/BA/waves/W1.md:19-23`, `docs/tranches/BA/waves/W3.md:40-43`).

This is exactly the sort of planning drift the meta-audit was supposed to
eliminate.

### 6. The cross-language `path!` claim is overstated and partially infeasible as written

The API-unification goal is good. The compile-time claim is not.

- `BA` repeatedly describes `path!` as compile-time typed and isomorphic across
  Rust, TS, and Python (`docs/tranches/BA/BA.md:15-22`, `96-99`, `138-141`,
  `264-316`).
- The TS design actually described is a template-literal tag invoking a WASM
  validator at load time / bundler time, not at TypeScript type-check time
  (`docs/tranches/BA/BA.md:280-288`,
  `docs/tranches/BA/waves/W2.md:44-55`).
- The Python design is necessarily runtime-typed and runtime-validated
  (`docs/tranches/BA/BA.md:289-294`,
  `docs/tranches/BA/waves/W2.md:57-66`).

So the current text conflates:

- Rust compile-time path typing,
- TS build-time or load-time validation,
- Python runtime validation.

Those can still be a coherent product surface, but they are not one compile-time
mechanism.

### 7. BB is still semantically anchored to tape output on a tape-abrogated substrate

This is the biggest architectural flaw in BB.

- The flow diagram still says the VM oracle compares tape output
  (`docs/tranches/BB/BB.md:74-77`).
- The architecture section says the VM compiles candidates and byte-compares
  tape output (`docs/tranches/BB/BB.md:102-107`).
- `VM-as-oracle` says execution outputs are tape sequences and equivalence is
  byte-equal tape output across the corpus (`docs/tranches/BB/BB.md:194-199`).
- The dependency section explicitly says "tape output produced by the
  struct-aware emitter" and even mixes "materialised struct state where tape is
  absent, and tape output where tape remains"
  (`docs/tranches/BB/BB.md:269-279`).
- `BB.W0` inherits the same model: the oracle wrapper "compares tape output"
  (`docs/tranches/BB/waves/W0.md:64-67`).

That is not a coherent AZ-II-era oracle contract. If AZ-II really closes tape
abrogation, then BB must pick a post-tape semantic equality surface:

- struct-tree equality,
- projection equality,
- normalized semantic object equality,
- or some other explicitly chosen canonical form.

Right now it says "no tape" and "compare tape output" in the same tranche.

### 8. BB still contains the known `ir-rewrites` module/crate drift

This defect was already known elsewhere, and it is still present in the Wave 2
synthesis docs.

- `BB.md` correctly describes `crates/ir/src/rewrites/` as a module location
  inside the existing IR crate (`docs/tranches/BB/BB.md:14-18`, `119-137`).
- But the critical-files table then says `crates/ir/src/rewrites/` is
  `create (new crate)` (`docs/tranches/BB/BB.md:375-383`).
- The defensible floor repeats "`crates/ir/src/rewrites/` crate landed"
  (`docs/tranches/BB/BB.md:389-393`).
- `BB.W0` makes it worse by listing `crates/ir/Cargo.toml | create`
  even though `crates/ir/Cargo.toml` already exists in the live workspace
  (`docs/tranches/BB/waves/W0.md:32-45`).
- `BB.W0` also points to nonexistent `crates/bbnf_derive/src/rewrites.rs`
  (`docs/tranches/BB/waves/W0.md:44`); the live derive crate path is
  `crates/derive/`, not `crates/bbnf_derive/`.

This is a hard dispatch blocker. The user already rejected the standalone-crate
framing. The docs have not fully internalized that correction.

### 9. BB still carries stale BC wave files in the BB wave directory

`docs/tranches/BB/waves/` contains `W5.md` and `W6.md`, but those files are not
BB waves at all.

- `W5.md` begins `# BC.W5 — Debug, Inspect, and Minimise Tooling`
  (`docs/tranches/BB/waves/W5.md:1-6`).
- `W6.md` begins `# BC.W6 — FINAL + Bounded-Cost Closure` and points at
  nonexistent `docs/tranches/BC/FINAL.md` / `PROGRESS.md`
  (`docs/tranches/BB/waves/W6.md:1-23`, `57-64`).
- The live repo has no `docs/tranches/BC/` directory.

This is plain legacy cruft inside the very tranche family that was supposed to
be cleaned up by the audit. It undermines confidence in the BB surface as a
canonical plan.

## Open

### 1. Whether divan belongs inside B1 or immediately after B1 is still not proven here

The synthesis commits to "B1 includes divan." That may be right, but this axis
review did not rerun build/test/bench timings. The claim remains architectural,
not freshly measured.

If the actual blocker is cache invalidation + alias truth + test honesty, then
the 19-bench harness port may still be oversized for a pre-AY-II annex. That
decision needs one explicit owner and one fresh timing pass.

### 2. The modernization docs need a live filesystem rebase before execution

This pass confirmed:

- `../parse-that` exists,
- `../pprint` exists,
- the csc411 `csp-solver` sibling exists,
- `/Users/mkbabb/Programming/gorgeous` does not exist.

That alone is enough to require a quick rebase pass over the appurtenant docs
before any agent is dispatched from them.

### 3. BA needs a real implementation brief for TS/Python validation mechanics

If the goal is true semantic isomorphism, the tranche needs to choose one of:

- generated TS type-level path unions,
- a build plugin / codegen step,
- load-time WASM validation plus best-effort editor tooling,
- or a narrower claim that only Rust gets compile-time guarantees.

Without that choice, `BA.W2` remains more manifesto than plan.

### 4. BB needs an explicit post-tape oracle equality contract before W0

`BB` cannot stay ambiguous about the oracle output surface.

If the equality check is not tape, say what it is. The tranche currently lacks
that one sentence, and without it the W0 oracle wrapper is under-specified at
its semantic core.

### 5. The Wave 2 synthesis surface still needs one explicit legacy-prune pass

This should include at minimum:

- B1 migration doc artefact renames,
- BA root-file relocation into a real package,
- BA harness naming cleanup,
- BB module-not-crate correction,
- BB tape-language purge,
- removal or relocation of `BB/waves/W5.md` and `W6.md`.

Until that pass lands, the plan is directionally good but documentarily noisy.

