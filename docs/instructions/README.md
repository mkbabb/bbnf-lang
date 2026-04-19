# Operational Directives — Agents and Orchestrators

These directives govern **all** work in this repository: implementation,
auditing, benchmarking, profiling, testing, documentation. They are
non-negotiable. An agent that violates them produces debt, not progress.
Internalise them before beginning any task.

Companion documents:

- `PROFILING.md` — samply workflow, profile artefact discipline.
- `tranche/` — tranche authoring + execution:
  - `tranche/SPEC.md` — tranche creation specification.
  - `tranche/START.md` — orchestrator invocation prompt.
  - `tranche/RESEARCH.md` — research wave protocol.
  - `tranche/WAVE_SPEC.md` — per-wave sub-document format.
  - `tranche/README.md` — subdir index + invocation order.

## Tranche structure

Every tranche lives in `docs/tranches/{LETTER}/` as a directory:

```
docs/tranches/{LETTER}/
├── {LETTER}.md        Plan document — phases, hard gates, critical files, invariants
├── PROGRESS.md        Dated execution log — ground truth of what landed, what didn't, what blocked
├── FINAL.md           Completion document — required at end of tranche (see below)
├── research/          Verbatim agent research artefacts that informed the plan
└── *.md               Audits, critiques, supplementary analysis
```

- `{LETTER}.md` is written **before execution**.
- `PROGRESS.md` is updated **during execution**. Every entry is dated.
  Every entry records: what was done, what was committed, what blocked,
  what shifted: particularly note frictional impasses, or work that landed seamlessly. This is the canonical record — the diff between
  `{LETTER}.md` and `PROGRESS.md` tells you what changed under contact.
- `FINAL.md` is written **at tranche completion**. See tranche
  completion requirements below.
- All tranches reference this file for operational protocol rather
  than restating directives inline.

## Crate ownership

All crates in the dependency graph are owned and modifiable, including
external path dependencies:

- **`../parse-that/`** — parser combinators, bbnf-regex, HIR, NFA/DFA,
  scanners. Patched via `.cargo/config.toml`. Full write access.
- **`../pprint/`** — pretty printing. Patched similarly.
- **`crates/csp-solver/`** — CSP/COP substrate. Workspace member.
- **`crates/bbnf-tape/`, `crates/bbnf-ir/`, `crates/egraph/`,
  `crates/bbnf-regex/`, `crates/analysis/`, `crates/gorgeous/`** —
  workspace members.

These are NOT read-only external dependencies. Fixes, features, and
architectural changes to any of them are first-class work items, not
"upstream" deferrals.

## Code discipline

- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues.**
  Idiomatic approaches only. If something does not work, find and
  fix the root cause. Temporary fixes become permanent debt.
- **NO legacy code.** Architectural transpositions for elegance,
  simplicity, and performance are mandatory. Delete dead code; do
  not comment it out, gate it behind feature flags, or rename it to
  `_unused`.
- **NO backward-compatibility shims.** Always migrate fully. No
  adaptor layers, no re-exports for removed items, no `// removed`
  comments where code used to live.
- **NO deferrals.** If the current tranche declares a piece of work
  within its scope, it ships in that tranche. Deferring to a future
  tranche is accepted only when the plan document declares the
  deferral explicitly, with rationale, at plan time — not silently
  during execution. Absorb-mode re-planning and new-letter response
  per `tranche/SPEC.md` §Scope-reveal protocol are not deferrals.
- **Execute the plan, not around it.** The plan declares intent;
  contact reveals scope. The default response to scope-reveal-
  under-contact is re-plan-with-more-agents: dispatch more
  sub-agents, split into sub-waves, carry plan-declared
  intentional unworkability across waves. Placeholder arms,
  single-probe stubs, `Unsupported` branches, "substrate only"
  landings that leave the consumer unwired, and additive
  shadow-surfaces that keep a legacy path alive beside a partial
  replacement are architectural debt dressed as pragmatism.
  Conservative engineering is high-quality execution of what the
  plan stipulates; architecture-aversion is its opposite.
  Escalate only for hard environmental blockers — compiler bug,
  authorization boundary, irrecoverable state. Scope-reveal is
  not an escalation condition.
- **Substrate-with-consumer is one unit of work.** A wave that
  lands an emitter pass, an IR field, a const slot, or a runtime
  variant *without verifying its output is consumed in the hot
  path* has not closed — it has staged a deferral. The completion
  criterion is not "the pass exists" but "the pass's output drives
  runtime behaviour and that fact is verified by samply
  attribution, symbol presence, or a wire-contract test that
  asserts the data flows from mining through emit to runtime use."
  An empty `pub const` slot with a functioning emitter is a
  deferral. An emitted dispatch table that no consumer reads is a
  deferral. A consumer call replaced by a fallback path is a
  deferral. Substrate without consumer is the pattern that
  masquerades as scope-revelation; it is rejected on the same
  footing as a placeholder arm.
- **NO god modules.** At every level — crate, module, file —
  separate concerns into proper sub-units. A file that grows past
  a natural responsibility boundary factors into a directory
  module (`foo/` with `foo/lib.rs`, `foo/kind.rs`, `foo/pass.rs`)
  over flat siblings (`foo.rs`, `foo_kind.rs`, `foo_pass.rs`).
  "Utils" / "helpers" / "common" modules that accumulate
  orthogonal concerns are god modules in gestation — name the
  responsibility, split accordingly. Crates mirror the same rule:
  general-purpose infrastructure (e-graph, cost model, CSP
  substrate, regex engine) lives in its own crate, not stuffed
  into a domain crate.
- **Generated files are output of fresh regen; never hand-patch.**
  `generated.rs` is produced by `scripts/bootstrap-bbnf.sh`. The
  only legitimate edits are via that script.
- **Tests live in `tests/` directories only.** Never inline
  `#[cfg(test)]` modules under `src/`.
- **One codegen path, no fallbacks, no hybrids.** A new optimisation
  integrates with the existing pass pipeline, cost model, and
  rewrite framework; it does not create a parallel one.
- **Commit frequently with `/commit`.** Each natural milestone
  (phase sub-item, artefact landing, invariant restoration) gets
  its own commit. Do not batch unrelated changes. The orchestrator
  and every sub-agent commit at milestones — not at tranche end.

## Parallel agent orchestration

The orchestrator runs the tranche. Sub-agents execute isolated,
bounded work in parallel. The discipline below is non-negotiable;
prior sessions have lost work when it was violated.

**Wave structure.**

- A tranche decomposes into **waves**. A wave is a set of up to
  **six parallel agents** with no overlapping file bounds.
- The orchestrator defines waves explicitly at tranche start and
  updates them in `PROGRESS.md` as execution unfolds.
- Wave boundaries are sequencing boundaries. Work in wave N+1
  depends on wave N completing and being cherry-picked onto master.

**Worktree isolation.**

- **Every sub-agent runs in its own isolated worktree** — no
  exceptions. Research agents, audit agents, benchmark agents,
  profile agents, codegen agents: all worktree-isolated.
- Worktrees are siblings of the main repo. Seed immediately with
  `scripts/seed-worktree.sh` so gitignored corpora (`data/`) and
  any other required-but-ignored resources are visible:

  ```bash
  ROOT=$(git rev-parse --show-toplevel)
  PARENT=$(dirname "$ROOT")
  git worktree add --detach "$PARENT/bbnf-wt-<agent-tag>" HEAD
  "$ROOT/scripts/seed-worktree.sh" "$PARENT/bbnf-wt-<agent-tag>"
  ```

  Skipping the seed step produces environmental test failures that
  look like regressions — W2.1's agent reported 24 "failures" that
  were all missing `data/{bbnf,css,json}`. Do not repeat.

- Worktrees are **never** placed under `/tmp`, `/private/tmp`, or
  any ephemeral path. Loss of work there has happened; do not
  repeat it.
- A sub-agent **never** runs `git checkout`, `git stash`,
  `git reset`, `git rebase`, or any branch-switching command on
  the main worktree. Branch operations happen inside the agent's
  own worktree.
- If an agent needs to compare against an older commit, it does
  so inside its worktree. The main worktree's HEAD is owned by
  the orchestrator.
- Sub-agents inherit the orchestrator's build cache via symlink —
  `ln -s <main-repo>/target <worktree>/target` before the agent's
  first build. Worktree git isolation does not imply build
  isolation; rebuilding the entire workspace per sub-agent burns
  cycles and RSS. W0a.2 PROGRESS entry documents the mitigation.

**File bounds.**

- Every sub-agent prompt declares explicit, fastidious **file
  bounds**: the files it may read, the files it may modify, the
  files it must not touch.
- No two agents in the same wave share write access to the same
  file. Exclusive write per file per wave.
- Cross-wave conflicts are resolved by sequencing (moving work to
  a later wave), never by merging concurrent writes to the same
  file from different agents.

**Commit and cherry-pick.**

- Master must be clean before spawning a wave.
- Each sub-agent commits inside its worktree with `/commit` at
  milestones.
- At wave completion, the orchestrator reviews each worktree's
  commits, cherry-picks accepted commits onto master, and deletes
  the worktree.
- Un-accepted commits are discussed with the user before discard.

**Agent-claim hardening.**

- Agent results are **not trusted at face value.** The orchestrator
  independently verifies key findings before acting on them —
  benchmark numbers, feature-wiring claims, regression
  attributions, file diffs, test passes.
- An agent may parrot doc claims without verifying them against
  code or data. Cross-check against saved artefacts (`expand.rs`,
  `profile.json.syms.json`, `bench.txt`) before folding a claim
  into master.
- When an agent's report and a saved artefact disagree, trust the
  artefact.

**Wave verification ledger.** Every wave the orchestrator closes
records, in `PROGRESS.md`, the verification artefacts that
established the close — not the agent's claim of the close. The
ledger entries are mandatory for any wave whose hard gate involves
emitted code, runtime behaviour, performance, or substrate
activation:

- **Symbol verification.** `nm target/release/deps/<bench>` over
  the bench binary establishes which symbols are present and
  absent in the compiled hot path. A wave that claims symbol X is
  removed verifies the absence; a wave that claims helpers are
  inlined verifies *those* helpers are also absent (eliminating one
  dispatcher does not remove dispatch when the per-state arms call
  cross-crate helpers — every such call is a real function-call
  boundary if the callee does not inline).
- **Wire-contract end-to-end test.** Any pipeline of the shape
  *IR mining → emitter pass → `pub const` literal → runtime
  consumer* carries one test that exercises the full path. A
  fixture grammar with known mineable data is processed through
  the full pipeline; the resulting `pub const` literal is asserted
  to contain the mined values; a runtime invocation is asserted to
  consume the const non-trivially (samply or counter). A test at
  the mining-pass level OR at the emitter level alone is
  *insufficient* — the projection silently drops data when only
  one boundary is asserted.
- **Samply attribution per lever.** A wave that claims a lever
  fires cites the samply self-time line item showing the consumer
  symbol present (or the previously-dominant symbol absent). The
  citation is a `.profiles/samply/<wave>/<bench>/` path the
  orchestrator can re-load, not a paraphrase.
- **Substrate-without-consumer is rejected at wave close.** If the
  ledger cannot point at a runtime consumer for every emitted
  substrate the wave introduced, the wave has not closed; the
  orchestrator re-plans with additional agents per the
  scope-reveal contract. Agent-reported "consumer wiring deferred
  to follow-on" is the deferral pattern; it does not close a wave.

**Agent briefing.**

- Sub-agent prompts are **self-contained**. The agent starts with
  no conversation context. Brief like a colleague who just walked
  in: what to do, why, which files, what the hard gate is.
- Prompts name the specific tranche and the specific phase the
  agent is executing. The agent reads the current tranche document
  before beginning.

**No trammelling.**

- Sub-agents must not trammel each other (concurrent writes, racing
  on shared files) and the orchestrator must not trammel sub-agents
  (editing files a sub-agent owns).
- If a collision is detected (e.g. two agents modify the same file
  before wave completion), the orchestrator halts, resolves the
  conflict, and re-plans the wave.

## Tranche completion requirements

A tranche is **not complete** until:

1. **`FINAL.md` exists** under `docs/tranches/{LETTER}/`. It
   contains:
   - Full recapitulation of every phase, sub-phase, and hard gate.
     What landed, with commit hashes. What did not land, with
     rationale.
   - Verification that every invariant declared in `{LETTER}.md`
     holds in the final state, with artefact citations.
   - Summary of cross-tranche debt addressed or deferred, with
     forward references.
   - Future work and refinement areas, framed as seeds for the next
     tranche's planning.
2. **`docs/benchmarks/post-{LETTER}.json` exists** covering the
   full parse-bench matrix:
   - `json_monolithic` × {data, twitter, citm, canada, data_xl}
   - `css_l4` × {normalize, bootstrap, tailwind}
   - `google_sheets_monolithic` × {parse_simple, parse_nested, parse_stress}
   - `bbnf_monolithic` × {json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar}

   The VM, WASM, TS, and competitors benches are **not** included.
   Numbers come from a fresh cold bench run on master after all
   tranche commits have landed.
3. **All tests pass** — `cargo test --workspace` exits zero, with
   no `#[ignore]` added in the tranche, no workarounds, no
   temporary skips.

**Escape clause + scope-reveal response modes** live in
`tranche/SPEC.md` §Scope-reveal protocol. Plan-time-declared
intentional unworkability, Absorb-mode re-plans under contact,
and new-letter response on scope shifts are all normed there; do
not re-litigate from README. A tranche silently declaring itself
"incomplete" at execution time to dodge the completion requirements
is a violation of the no-workarounds invariant.

## Expensive commands — always file-first

ALWAYS write expensive command output to a file, then grep / sed /
awk over the file. NEVER re-run an expensive command to slice
output differently.

```bash
cargo test --workspace > /tmp/test-out.txt 2>&1
grep 'test result' /tmp/test-out.txt
grep FAILED /tmp/test-out.txt
tail -40 /tmp/test-out.txt
```

Applies to `cargo test`, `cargo bench`, `cargo expand`, `cargo
build`, `cargo check --workspace`, `samply record`, and any
command taking > 30 seconds.

**Never read large output files line-by-line.** `cargo expand`
output routinely exceeds 100K lines. Use targeted `grep -n`,
`awk`, `sed` to extract the slice you need. Know the file size
before reading.

```bash
grep -n 'fn __declaration' /tmp/expand-css.txt
awk 'NR>=5000 && NR<=5100' /tmp/expand-css.txt
awk '/fn __declaration/,/^        fn __/' /tmp/expand-css.txt > /tmp/decl.txt
wc -l /tmp/decl.txt
```

## Memory discipline for aggregate test binaries

Aggregate test binaries linking ≥ 4 derive-Parser sites hit LLVM
codegen super-linearity — AX.W0a.2.d observed a single rustc peaking
at 26 GB RSS on a 5-derive-site `tape_parity` binary. Split into
per-grammar test binaries (AX commit `61053374` template) and export
`CARGO_BUILD_JOBS=4` in sub-agent briefings to cap parallel rustc
processes. Per-grammar binaries compile in ~11-14 s at ~3 GB RSS.

## Cache clearing

Clear **all** `.bbnf-cache` directories before any bench, regen,
or proc-macro expansion test. The derive macro caches expansions;
`cargo clean` does NOT clear them.

```bash
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null
```

Also clear `crates/target/.bbnf-cache/` explicitly if present.

If `bbnf-analysis` ICEs (recurring nightly issue):

```bash
cargo clean -p bbnf-analysis
```

## Testing

```bash
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

cargo test -p bbnf-tape -p bbnf-ir -p egraph > /tmp/leaf-tests.txt 2>&1
grep 'test result' /tmp/leaf-tests.txt

cargo test -p bbnf --test grammar_roundtrip > /tmp/roundtrip.txt 2>&1
grep '^test\|test result' /tmp/roundtrip.txt

cargo test -p bbnf --test payload_layouts > /tmp/payload.txt 2>&1
grep 'test result' /tmp/payload.txt

cargo test --workspace > /tmp/workspace-tests.txt 2>&1
grep 'test result' /tmp/workspace-tests.txt
```

Grammar roundtrip is the primary correctness gate.

## Benchmarking

Benchmarks run sequentially — never in parallel, never in separate
commands that race. One invocation per bench, output to a file.

```bash
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

cargo bench -p bbnf --bench compile_pipeline > /tmp/bench-compile.txt 2>&1
cargo bench -p bbnf --bench json_monolithic > /tmp/bench-json.txt 2>&1
cargo bench -p bbnf --bench css_l4 > /tmp/bench-css.txt 2>&1
cargo bench -p bbnf --bench google_sheets_monolithic > /tmp/bench-sheets.txt 2>&1
cargo bench -p bbnf --bench bbnf_monolithic > /tmp/bench-bbnf.txt 2>&1

grep -n 'bench:' /tmp/bench-*.txt
```

Cold per-parse only. Warm/cached benchmarks are disingenuous. The
bench binaries use `#[global_allocator] mimalloc`.

Primary datasets:

- `data/json/`
- `data/css/`
- `data/sheets/`
- `grammar/`

## Profiling

Samply profiling, shared target directories, wave preparation, and
the headless orchestration contract are documented separately in
**`PROFILING.md`** alongside this file. The rules above (file-first
expensive commands, cache clearing, no trammelling, isolated
worktrees, hardened agent claims) all apply to profiling work; the
profiling document layers the samply-specific workflow on top.

## Bootstrap regen

```bash
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

bash scripts/bootstrap-bbnf.sh

find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null
cargo test -p bbnf --test grammar_roundtrip > /tmp/roundtrip.txt 2>&1
grep '^test\|test result' /tmp/roundtrip.txt

cp crates/core/src/grammar/generated.rs /tmp/gen1.rs
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
bash scripts/bootstrap-bbnf.sh
diff /tmp/gen1.rs crates/core/src/grammar/generated.rs  # must be empty
```

### Self-host circular-dependency escape

The bootstrap script uses the *currently-compiled* `bbnf` library's
`BbnfBootstrap::parse` to parse `bbnf.bbnf` itself. When a tranche
rewrites the parser in a way that makes the post-rewrite
`BbnfBootstrap::parse` fail on `bbnf.bbnf` before the emitter
regenerates the correct table, the script cannot close its own loop:
running it recompiles `bbnf` against the stale `generated.rs`, tries
to parse `bbnf.bbnf` with the broken parser, fails, emits an
essentially-empty output. AW-I.W3 opened this state; AW-I.W4ζ
escaped it via a one-shot recipe:

```bash
# 1. Check out a known-good fn-per-rule generated.rs. Pick the
#    commit before the parser rewrite landed. If the DtaTable
#    struct has since grown fields, hand-patch the missing ones
#    with inert defaults (`entry: DtaRuleId(0)`, etc.) to restore
#    compile.
git checkout <pre-rewrite-HEAD>^ -- crates/core/src/grammar/generated.rs
# (hand-patch DtaTable literal here if the struct grew)

# 2. Confirm the lib compiles against the restored generated.rs.
cargo check -p bbnf --lib

# 3. Run bootstrap. The bbnf lib now carries the OLD working parser;
#    the proc macro's `BbnfBootstrap::parse` succeeds on bbnf.bbnf
#    and hands the AST to the CURRENT emitter + walker + lifter,
#    emitting a fresh DTA-based generated.rs.
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
bash scripts/bootstrap-bbnf.sh

# 4. Commit the new generated.rs. Idempotency check follows
#    (gen1 == gen2). If the second bootstrap emits a truncated
#    stub, the parser-consumer contract still has gaps — follow
#    the tranche/SPEC.md §Root-cause discipline walk-through.
```

Commits `87f65214` (transient entry patch) and `49656fd4` (one-shot
regen) are the template. The recipe is a legitimate orchestrator
move, not a workaround — it breaks an architecturally-inherent
circular dependency that only arises during parser-rewrite tranches.

## Performance claims

- **Every claimed perf win has a samply profile.** No speculative
  throughput numbers.
- **`cargo expand` evidence for every codegen activation claim.**
  Visual inspection of the expanded code, not just "test passes".
- **Run the actual profiler; never guess from static analysis.**
  samply / Instruments / perf on the actual bench binary.
- **Performance narratives reconstruct the actual timeline from
  commits.** No fabrication, no embellishment. A claim without an
  artefact citation is a vibe, not evidence.
- **Separate emitted-code facts from runtime hotspot facts.** Both
  are required; neither alone is sufficient.
- **Cross-crate inlining is verified with `nm`, not assumed.**
  Removing one dispatcher does not eliminate dispatch when the
  emitted code calls cross-crate helpers — every such call is a
  real function-call boundary if the callee does not inline, and
  the function-call boundary IS a dispatcher in that case. Every
  perf claim that invokes "specialised" / "inlined" / "compiled"
  code paths cites `nm target/release/deps/<bench>` showing both:
  (a) the dispatcher symbol absent, and (b) the cross-crate helper
  symbols absent. If the helpers are present in the bench binary,
  they are not inlined; whatever dispatch the wave claimed to
  eliminate has been moved to the helper-call boundary, not
  removed. Workspace LTO + `#[inline(always)]` on every hot helper,
  OR per-grammar inline emission of the helper bodies, are the only
  two answers; verify whichever ships.

## Architecture invariants

- **One codegen path.** No fallbacks. One regex system (HIR). KISS.
- **Clean tier boundaries.** Each crate owns a responsibility;
  analysis lives in the library crate, codegen decisions in the
  IR/backend crates. Data crosses boundaries through well-defined
  structs, not re-derivation. If two crates compute the same
  thing, one is wrong.
- **General-purpose constructs in their own crates** — not stuffed
  into domain crates. The egraph substrate, cost model, and CSP
  solver are general-purpose.
- **No overfitting.** Don't canonise a specific struct, function,
  or pattern as "the one true way" in design docs. Architecture
  evolves; principles endure. The principle is separation of
  concerns; the implementation is whatever serves it best today.
- **Fixed-point loops use LLVM-style `Changed` bool**, not
  structural hash. Content hash stays as `debug_assert!` only.
- **Decision points are pluggable** (cost model, pattern registry,
  rewrite rules) — not hardcoded branches.
- **Fold new work into existing systems.** No orthogonal
  subsystems. A new optimisation integrates with the existing
  pass pipeline, cost model, and rewrite framework; it does not
  create a parallel one.
- **Typed materialisation is total.** Every `->` annotation in a
  grammar reaches the tape emitter; inference composes types and
  never loses them. No scalar-only type coverage — primitives,
  Span, owned strings, tuples, tagged-union enums with recursion,
  optional types, and variable-length lists all have codegen
  routes.
- **Grammar-specialised codegen comes from the grammar.** Schemas,
  dispatch tables, payload layouts, scanner alphabets, capacity
  closures, column selectors, keyword tables — all emitted,
  never hand-written.
- **Hoist emitter-known data into emitted code.** When the emitter
  knows a value at codegen time, the emitted code carries the
  literal — not a runtime indirection through the source array
  the emitter populated. A `match SOURCE_ARRAY[N] { Variant {
  field } => field, _ => unreachable_unchecked() }` inside a body
  that the emitter knows IS index N is a runtime memory load of
  data the emitter possesses; the literal `let field =
  <known_value>;` is the codegen form. LLVM's per-site
  specialisation depends on the literal binding; the
  runtime-`match`-against-`unreachable_unchecked` pattern preserves
  the source-array indirection and defeats const-folding in
  practice (whether or not it is theoretically recoverable through
  ThinLTO). Mechanism, not optimism.
- **Wire-contract pipelines have end-to-end tests.** Any pipeline
  of the shape *IR mining → IR pass → emitter pass → `pub const`
  literal → runtime consumer* carries one test that exercises the
  full path. Not "the IR pass produces the right value" + "the
  emitter pass formats the right syntax" separately — those leave
  the projection silently dropping data when the two contracts
  drift. The test loads the runtime const literal (via the same
  symbol the consumer reads) and asserts it contains the mined
  values for a fixture grammar with known mineable data. Mining-
  side tests + emitter-side tests alone are *insufficient*; the
  projection between them silently drops data when only one
  boundary is asserted.

## Indefatigability

The orchestrator does **not** relinquish control until the tranche
is complete per the completion requirements above. Progress stalls
only for user input, hard blockers (environment, irrecoverable
conflicts), or genuine scope changes that require re-planning with
the user. A stall that is actually a worked-around problem is a
violation. The orchestrator's job is to finish.
