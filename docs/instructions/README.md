# Operational Directives — Agents and Orchestrators

These directives bind **all** work in this repository — implementation,
auditing, benchmarking, profiling, testing, documentation. An agent
that violates them produces debt, not progress. Internalise them
before beginning any task.

`PROFILING.md` layers samply workflow on top. `tranche/` holds tranche
authoring and execution (`SPEC.md`, `START.md`, `RESEARCH.md`,
`WAVE_SPEC.md`, `README.md`).

## Tranche structure

Every tranche lives in `docs/tranches/{LETTER}/`:

```
{LETTER}.md     Plan — phases, hard gates, critical files, invariants (written before execution)
PROGRESS.md     Dated execution log — what landed, what blocked, what shifted (updated during)
FINAL.md        Completion document (written at close; see completion requirements)
research/       Verbatim agent research artefacts
*.md            Audits, critiques, supplementary analysis
```

The diff between `{LETTER}.md` and `PROGRESS.md` is the canonical
record of what changed under contact. Tranches reference this file
for operational protocol rather than restating directives inline.

**Mid-tranche scope pivots open a new letter.** A pivot mid-execution
does not continue the existing letter's numbering — close the current
`PROGRESS.md` with the pivot rationale, write a fresh
`{NEXT_LETTER}.md`, and carry forward only the explicit scope the new
plan declares.

## Crate ownership

All crates in the dependency graph are owned and modifiable, including
external path dependencies patched via `.cargo/config.toml`:

- **`../parse-that/`** — parser combinators, bbnf-regex, HIR, NFA/DFA, scanners.
- **`../pprint/`** — pretty printing.
- **`crates/csp-solver/`, `crates/tape/`, `crates/ir/`,
  `crates/egraph/`, `crates/simd-scan/`, `crates/analysis/`,
  `crates/gorgeous/`, `crates/core/`** — workspace members.

Fixes, features, and architectural changes to any of them are
first-class work items, not "upstream" deferrals.

## Code discipline

- **No workarounds, hacks, or `#[allow(...)]` to mask issues.** If
  something does not work, find and fix the root cause. Temporary
  fixes become permanent debt.
- **No legacy code.** Architectural transpositions for elegance,
  simplicity, and performance are mandatory. Delete dead code; do
  not comment it out, gate it behind feature flags, or rename it
  `_unused`.
- **No backward-compatibility shims.** Migrate fully. No adaptor
  layers, no re-exports for removed items, no `// removed` markers.
- **No deferrals.** Work within the tranche's declared scope ships
  in that tranche. Explicit plan-time deferral with rationale is
  acceptable; silent deferral during execution is not. Absorb-mode
  re-planning and new-letter response per `tranche/SPEC.md`
  §Scope-reveal protocol are not deferrals.
- **Execute the plan, not around it.** The plan declares intent;
  contact reveals scope. The default response to scope-reveal-
  under-contact is re-plan-with-more-agents — dispatch more
  sub-agents, split into sub-waves, carry plan-declared intentional
  unworkability across waves. Placeholder arms, single-probe stubs,
  `Unsupported` branches, "substrate only" landings that leave the
  consumer unwired, and additive shadow-surfaces beside a partial
  replacement are architectural debt dressed as pragmatism. Escalate
  only for hard environmental blockers — compiler bug, authorization
  boundary, irrecoverable state. Scope-reveal is not an escalation
  condition.
- **Relinquish when stuck.** A sub-agent three-plus iterations
  without a commit, or over ~30 min without forward motion, stops,
  reports current state with every diagnostic artefact (probe tests,
  symptom notes, draft-fix diff), and relinquishes to the
  orchestrator. Indefatigability binds the orchestrator, not the
  individual sub-agent. Commit diagnostic probes separately from
  attempted fixes so the orchestrator can cherry-pick or discard
  independently. The orchestrator then dispatches the triumvirate
  (see §Orchestration).
- **Audits analyse expand-begotten code, not just sources.** Any
  audit of emitter behaviour — correctness, regression diagnosis,
  feature activation — inspects `cargo expand` output as its primary
  artefact. Emitter intentions and expanded reality routinely diverge
  (dead branches, conditional silence, macro-hygiene drift); only
  expanded code reaches the compiler and bench binary. Audit reports
  cite specific line ranges in the saved expand artefact
  (`target/expand/<bench>.rs`) as primary evidence; source citations
  are secondary.
- **Substrate-with-consumer is one unit of work.** A wave landing an
  emitter pass, an IR field, a const slot, or a runtime variant
  *without verifying its output is consumed in the hot path* has not
  closed — it has staged a deferral. The completion criterion is not
  "the pass exists" but "the pass's output drives runtime behaviour
  and that fact is verified by samply attribution, symbol presence,
  or a wire-contract test asserting the data flows from mining
  through emit to runtime use." An empty `pub const` slot, an
  emitted dispatch table no consumer reads, a consumer replaced by a
  fallback — all are deferrals, rejected on the same footing as
  placeholder arms.
- **No god modules.** At every level — crate, module, file —
  separate concerns. A file past a natural responsibility boundary
  factors into a directory module (`foo/` with `foo/lib.rs`,
  `foo/kind.rs`, `foo/pass.rs`) over flat siblings (`foo.rs`,
  `foo_kind.rs`). `utils` / `helpers` / `common` modules that
  accumulate orthogonal concerns are god modules in gestation.
  General-purpose infrastructure (e-graph, cost model, CSP
  substrate, regex engine) lives in its own crate.
- **Generated files are output of fresh regen; never hand-patch.**
  Generated parser files are produced by `cargo xtask regen` under
  `crates/core/src/grammar/generated/<ident>.rs`. Do not hand-edit
  generated grammar output. Fix the grammar, IR facts, projection
  inference, or emitter root cause, then regenerate.
- **Tests live in `tests/` directories only.** Never inline
  `#[cfg(test)]` modules under `src/`. (Convention-enforced via
  code review; B7.W1.A5 hoisted all 69 inline tests from
  `parse-that/rust/parse_that/src/` to bring that repo into
  alignment with bbnf-lang and pprint, which already conformed.)
- **One codegen path, no fallbacks, no hybrids.** A new optimisation
  integrates with the existing pass pipeline, cost model, and
  rewrite framework; it does not create a parallel one.
- **Commit frequently with `/commit`.** Each natural milestone
  (phase sub-item, artefact landing, invariant restoration) gets its
  own commit. Do not batch unrelated changes; every agent commits at
  milestones, not at tranche end.

## Orchestration

The orchestrator runs the tranche. Sub-agents execute isolated,
bounded work in parallel.

**Wave structure.** A tranche decomposes into **waves** — sets of up
to 10 parallel agents with no overlapping file bounds. The
orchestrator defines waves at tranche start and updates them in
`PROGRESS.md`. Wave N+1 depends on wave N completing and being
cherry-picked onto master.

**Worktree isolation.** Every sub-agent — research, audit, benchmark,
profile, codegen — runs in its own worktree. No exceptions. Worktrees
are siblings of the main repo; never `/tmp` or `/private/tmp` (work
has been lost there). Seed immediately so gitignored corpora (`data/`)
are visible:

```bash
ROOT=$(git rev-parse --show-toplevel)
PARENT=$(dirname "$ROOT")
git worktree add --detach "$PARENT/bbnf-wt-<agent-tag>" HEAD
"$ROOT/scripts/seed-worktree.sh" "$PARENT/bbnf-wt-<agent-tag>"
```

Sub-agents inherit the orchestrator's build cache via symlink —
`ln -s <main-repo>/target <worktree>/target` before first build.
Git isolation does not imply build isolation.

Sub-agents **never** run `git checkout`, `git stash`, `git reset`,
`git rebase`, or any branch operation on the main worktree; those
happen inside the agent's own worktree. Comparisons against older
commits run inside the worktree. The main worktree's HEAD is owned
by the orchestrator.

**File bounds.** Every sub-agent prompt declares explicit, fastidious
file bounds — may-read, may-modify, must-not-touch. No two agents in
the same wave share write access to the same file. Cross-wave
conflicts resolve by sequencing, never by merging concurrent writes.

**Commit and cherry-pick.** Master must be clean before spawning a
wave. Each sub-agent commits inside its worktree with `/commit` at
milestones. At wave completion the orchestrator reviews each
worktree's commits, cherry-picks accepted ones onto master, and
deletes the worktree. Un-accepted commits are discussed with the
user before discard.

**Dispatch hard-cap template.** Every sub-agent dispatch carries
`HARD CAP: N min. At 0.9N commit, at N halt`. Defaults: **research
20, plan 15, redress 30, implementation 45**. At 0.9N the agent
commits whatever state it has; at N it halts unconditionally and
reports.

**Status tick cadence.** The orchestrator emits a one-line status
tick every ~5 min of orchestrator-silent wait. Never make the user
ask status twice.

**No bash-tail on JSONL.** The harness delivers background-process
completion events automatically. Explicit polling of sub-agent or
background-command progress is forbidden: no `ps aux | grep rustc`,
no repeated `tail -f` on JSONL, no `sleep`-then-check loops. Commands
dispatched with `run_in_background:true` deliver completion
notifications as tool-call results; the Monitor tool streams
stdout-line events when intermediate signal is needed. For a one-shot
worktree inspection, use `scripts/worktree-status.sh <worktree>`.
Polling burns context and produces no actionable state.

**Empty-return redispatch.** An empty or null sub-agent return is
not scope-revelation. Redispatch the prompt verbatim with a pointer
to the prior worktree's state (`worktree path, branch, last commit
SHA, last JSONL mtime`). Silence is harness friction, not absence of
scope.

**Triumvirate auto-trigger.** JSONL quiet > 15 min OR a first pass
returning no commit automatically triggers a
**research + plan + redress** triumvirate — one agent to research the
blocker (all relevant sources, saved artefacts, prior tranche lineage,
`cargo expand` output), one to author a concrete fix plan (file-level
diffs, ordered change set, root-cause attribution), one to execute.
The halted agent's probe tests are the triumvirate's starting point.
No user prompt is required.

**Agent-claim hardening.** Agent results are not trusted at face
value. Key findings — benchmark numbers, feature-wiring claims,
regression attributions, file diffs, test passes — are cross-checked
against saved artefacts (`expand.rs`, `profile.json.syms.json`,
`bench.txt`). When a report and artefact disagree, trust the
artefact.

**Wave verification ledger.** Every wave close records, in
`PROGRESS.md`, the verification artefacts — not the agent's claim of
the close. Mandatory for any wave whose hard gate involves emitted
code, runtime behaviour, performance, or substrate activation:

- **Symbol verification** — `nm target/release/deps/<bench>`
  establishes which symbols are present and absent. Claims of symbol
  removal verify absence; claims of helper inlining verify *those*
  helpers are also absent. Eliminating one dispatcher does not
  remove dispatch when per-state arms call cross-crate helpers that
  do not inline.
- **Wire-contract end-to-end test.** Any pipeline shaped *IR mining
  → emitter pass → `pub const` literal → runtime consumer* carries
  one test exercising the full path. Tests at mining-pass OR emitter
  level alone are insufficient — the projection silently drops data
  when only one boundary is asserted.
- **Samply attribution per lever.** A wave claiming a lever fires
  cites the samply self-time line item showing the consumer symbol
  present (or previously-dominant symbol absent), as a
  `.profiles/samply/<wave>/<bench>/` path the orchestrator can
  re-load — not a paraphrase.
- **Substrate-without-consumer is rejected.** If the ledger cannot
  point at a runtime consumer for every emitted substrate, the wave
  has not closed. "Consumer wiring deferred to follow-on" is the
  deferral pattern.

**Agent briefing.** Sub-agent prompts are self-contained. The agent
starts with no conversation context — brief like a colleague who
just walked in: what to do, why, which files, what the hard gate is.
Prompts name the specific tranche and phase; the agent reads the
current tranche document before beginning.

**No trammelling.** Sub-agents must not trammel each other
(concurrent writes, racing on shared files); the orchestrator must
not trammel sub-agents (editing files a sub-agent owns). On
collision, the orchestrator halts, resolves, and re-plans the wave.

## Tranche completion

A tranche is **not complete** until:

1. **`FINAL.md` exists** under `docs/tranches/{LETTER}/`, containing:
   - Full recapitulation of every phase, sub-phase, and hard gate —
     what landed (commit hashes), what did not (rationale).
   - Verification that every invariant declared in `{LETTER}.md`
     holds in the final state, with artefact citations.
   - Cross-tranche debt addressed or deferred, with forward
     references.
   - Future work and refinement areas, framed as seeds for the next
     tranche's planning.
2. **`docs/benchmarks/post-{LETTER}.json` exists** covering the close
   matrix (`make ay-bench-close WAVE=close`, fat LTO):
   - `json_monolithic` × {data, twitter, citm, canada, data_xl}
   - `css_l4` × {normalize, bootstrap, tailwind}
   - `google_sheets_monolithic` × {parse_simple, parse_nested, parse_stress}
   - `bbnf_monolithic` × {json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar}
   - `compile_pipeline` × tranche-selected entries

   VM, WASM, TS, and competitors benches are not included.
   `json_value` is included when the tranche touches the Value API.
   Numbers come from a fresh cold bench run on master after all
   tranche commits have landed.
3. **All tests pass** — `cargo test --workspace` exits zero, no
   `#[ignore]` added in the tranche, no workarounds, no skips.

Escape clause and scope-reveal response modes live in
`tranche/SPEC.md` §Scope-reveal protocol. Plan-time-declared
intentional unworkability, Absorb-mode re-plans, and new-letter
response are all normed there. A tranche silently declaring itself
"incomplete" to dodge the completion requirements violates the
no-workarounds invariant.

## Expensive commands — always file-first

Write expensive command output to a file, then grep / sed / awk over
the file. Never re-run an expensive command to slice output
differently.

```bash
cargo test --workspace > /tmp/test-out.txt 2>&1
grep 'test result' /tmp/test-out.txt
grep FAILED /tmp/test-out.txt
tail -40 /tmp/test-out.txt
```

Applies to `cargo test`, `cargo bench`, `cargo expand`, `cargo
build`, `cargo check --workspace`, `samply record`, and any command
taking > 30 seconds.

**Never read large files line-by-line.** Any file over ~2 K lines —
`cargo expand` output, generated grammar files, monolithic audit docs, session
transcripts (`.jsonl`), long PROGRESS files, bench logs — is read via
`grep -n`, `awk`, `sed` with explicit line ranges, not whole-file
`Read`. `wc -l` first; if > 2 K lines, every subsequent access uses
targeted extraction. Files under
`crates/core/src/grammar/generated/*.rs` are file-first like any other
large artefact.

```bash
wc -l crates/core/src/grammar/generated/bbnf.rs
grep -n 'fn __declaration' /tmp/expand-css.txt
awk 'NR>=5000 && NR<=5100' /tmp/expand-css.txt
awk '/fn __declaration/,/^        fn __/' /tmp/expand-css.txt > /tmp/decl.txt
```

## Concurrent cargo — one per target

At most one `cargo` invocation in flight per `CARGO_TARGET_DIR` at
any instant. `target/.cargo-lock` silently serialises concurrent
invocations with indeterminate ordering; benches, samply prepare, and
build-cache-sensitive workflows observe non-deterministic artefacts
under contention.

- Parallel sub-agents sharing one target via symlink do not run cargo
  concurrently against it — the orchestrator sequences them.
- Parallel sub-agents each needing a concurrent cargo invocation use
  distinct `CARGO_TARGET_DIR`s (per-worktree target, no symlink) and
  accept the rebuild cost.
- The orchestrator never spawns its own concurrent cargo invocations
  against the main target while a wave's agents are active.

## Memory discipline for aggregate test binaries

Aggregate test binaries linking ≥ 4 derive-Parser sites hit LLVM
codegen super-linearity — AX.W0a.2.d observed one rustc peaking at
26 GB RSS on a 5-derive-site `tape_parity` binary. Split into
per-grammar test binaries (AX commit `61053374` template) and export
`CARGO_BUILD_JOBS=4` in sub-agent dispatches to cap parallel rustc
processes. Per-grammar binaries compile in ~11-14 s at ~3 GB RSS.

## Testing

`cargo iter-check` / `cargo iter-test-{leaf,grammar,ws}` are the
routine-iteration surface (see `.cargo/config.toml`). For one-shot
diagnostic runs, redirect output to file then grep — never re-run an
expensive command to slice output differently.

```bash
cargo nextest run -p bbnf-ir -p egraph -p csp-solver --profile ax-iter > /tmp/leaf-tests.txt 2>&1
grep 'Summary' /tmp/leaf-tests.txt

cargo nextest run -p bbnf --test grammar_roundtrip --profile ax-iter > /tmp/roundtrip.txt 2>&1
grep -E '^test|Summary' /tmp/roundtrip.txt

cargo nextest run --workspace --profile ax-iter > /tmp/workspace-tests.txt 2>&1
grep 'Summary' /tmp/workspace-tests.txt
```

Grammar roundtrip is the primary correctness gate.

## Benchmarking

Benchmarks run sequentially — never in parallel, never in separate
commands that race. One invocation per bench, output to a file. Cold
per-parse only; warm/cached benchmarks are disingenuous. Bench
binaries use `#[global_allocator] mimalloc`.

```bash
cargo bench -p bbnf --bench compile_pipeline > /tmp/bench-compile.txt 2>&1
cargo bench -p bbnf --bench json_monolithic > /tmp/bench-json.txt 2>&1
cargo bench -p bbnf --bench css_l4 > /tmp/bench-css.txt 2>&1
cargo bench -p bbnf --bench google_sheets_monolithic > /tmp/bench-sheets.txt 2>&1
cargo bench -p bbnf --bench bbnf_monolithic > /tmp/bench-bbnf.txt 2>&1

grep -n 'bench:' /tmp/bench-*.txt
```

Primary datasets: `data/json/`, `data/css/`, `data/sheets/`,
`grammar/`.

## Bootstrap regen

`cargo xtask regen` is the canonical regen entrypoint (B2 retired the
proc-macro IR-pipeline; per-grammar source lives at
`crates/core/src/grammar/generated/<ident>.rs`).

```bash
cargo xtask regen
cargo nextest run -p bbnf --test grammar_roundtrip --profile ax-iter > /tmp/roundtrip.txt 2>&1
grep -E '^test|Summary' /tmp/roundtrip.txt

cp crates/core/src/grammar/generated/json.rs /tmp/gen1.rs
cargo xtask regen --grammar json
diff /tmp/gen1.rs crates/core/src/grammar/generated/json.rs  # must be empty
```

### Self-host circular-dependency escape

Bootstrap uses the currently-compiled `bbnf` library's
`BbnfBootstrap::parse` to parse `bbnf.bbnf`. When a parser rewrite
makes post-rewrite `BbnfBootstrap::parse` fail on `bbnf.bbnf` before
the emitter regenerates the table, the script cannot close its loop.
AW-I.W4ζ escape recipe:

```bash
# 1. Restore a known-good generated grammar file from before the
#    rewrite. Hand-patch any new DtaTable fields with inert defaults
#    (entry: DtaRuleId(0)) to restore compile.
git checkout <pre-rewrite-HEAD>^ -- crates/core/src/grammar/generated/bbnf.rs

# 2. Confirm the lib compiles.
cargo check -p bbnf --lib

# 3. Re-run bootstrap. The bbnf lib now carries the OLD working
#    parser; BbnfBootstrap::parse succeeds on bbnf.bbnf and hands
#    the AST to the CURRENT emitter, producing fresh generated source.
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
cargo xtask regen --grammar bbnf

# 4. Commit. Idempotency check (gen1 == gen2) follows. Truncated
#    second output means the parser-consumer contract still has gaps.
```

Commits `87f65214` (transient entry patch) and `49656fd4` (one-shot
regen) are the template. The recipe breaks an architecturally-
inherent circular dependency that arises only during parser-rewrite
tranches — a legitimate orchestrator move, not a workaround.

## Performance claims

- **Every claimed perf win has a samply profile.** No speculative
  throughput numbers.
- **`cargo expand` evidence for every codegen activation claim.**
  Visual inspection of expanded code, not just "test passes".
- **Run the actual profiler; never guess from static analysis.**
  samply / Instruments / perf on the actual bench binary.
- **Performance narratives reconstruct the actual timeline from
  commits.** No fabrication, no embellishment. A claim without an
  artefact citation is a vibe.
- **Separate emitted-code facts from runtime hotspot facts.** Both
  are required; neither alone is sufficient.
- **Cross-crate inlining is verified with `nm`, not assumed.** Every
  perf claim invoking "specialised" / "inlined" / "compiled" code
  paths cites `nm target/release/deps/<bench>` showing both (a) the
  dispatcher symbol absent, and (b) the cross-crate helper symbols
  absent. Helpers present in the bench binary are not inlined; the
  dispatch claimed eliminated has moved to the helper-call boundary.
  Workspace LTO + `#[inline(always)]` on every hot helper, OR
  per-grammar inline emission of helper bodies, are the only two
  answers.

## Architecture invariants

- **One codegen path.** No fallbacks. One regex system (HIR). KISS.
- **Clean tier boundaries.** Each crate owns a responsibility;
  analysis in the library crate, codegen decisions in IR/backend
  crates. Data crosses boundaries through well-defined structs, not
  re-derivation. If two crates compute the same thing, one is wrong.
- **General-purpose constructs in their own crates** — egraph
  substrate, cost model, CSP solver. Not stuffed into domain crates.
- **No overfitting.** Don't canonise a specific struct, function, or
  pattern as "the one true way" in design docs. Architecture evolves;
  principles endure.
- **Fixed-point loops use LLVM-style `Changed` bool**, not structural
  hash. Content hash stays as `debug_assert!` only.
- **Decision points are pluggable** (cost model, pattern registry,
  rewrite rules) — not hardcoded branches.
- **Fold new work into existing systems.** No orthogonal subsystems.
  A new optimisation integrates with the existing pass pipeline,
  cost model, and rewrite framework.
- **Typed materialisation is total.** Every `->` annotation in a
  grammar reaches the tape emitter; inference composes types and
  never loses them. Primitives, Span, owned strings, tuples,
  tagged-union enums with recursion, optional types, and
  variable-length lists all have codegen routes.
- **Grammar-specialised codegen comes from the grammar.** Schemas,
  dispatch tables, payload layouts, scanner alphabets, capacity
  closures, column selectors, keyword tables — all emitted, never
  hand-written.
- **Hoist emitter-known data into emitted code.** When the emitter
  knows a value at codegen time, the emitted code carries the literal
  — `let field = <known_value>;`, not a runtime `match SOURCE_ARRAY[N]
  { ... _ => unreachable_unchecked() }` that LLVM may or may not
  const-fold. The runtime-match pattern preserves source-array
  indirection and blocks const-folding in practice regardless of
  ThinLTO theory. Mechanism, not optimism.
- **Wire-contract pipelines have end-to-end tests.** Not "mining-pass
  test" + "emitter-pass test" separately — those leave the projection
  silently dropping data when the two contracts drift. The test loads
  the runtime const literal via the same symbol the consumer reads
  and asserts it contains the mined values for a fixture grammar with
  known mineable data.

## Indefatigability

The orchestrator does not relinquish control until the tranche is
complete per the completion requirements. Progress stalls only for
user input, hard blockers (environment, irrecoverable conflicts), or
genuine scope changes requiring re-planning. A stall that is actually
a worked-around problem is a violation. The orchestrator's job is to
finish.
