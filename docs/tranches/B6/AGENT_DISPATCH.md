# B6 — Per-Wave Agent Dispatch Templates

This document carries one concrete dispatch template per wave —
W0, W1, W2 — built from `docs/instructions/tranche/AGENT_DISPATCH_
TEMPLATE.md`. Each template is self-contained: an agent dispatched
with the template plus the wave spec at `waves/W<N>.md` plus
`docs/instructions/README.md` plus `docs/instructions/tranche/
SPEC.md` has every input it needs.

The orchestrator substitutes the worktree path field per
dispatch and reviews the wave's hard-gate items before dispatch
to confirm the agent has runtime evidence for each.

---

## W0 — Bootstrap two-stage fast-path (1 agent)

```
You are sub-agent W0.a for tranche B6. W0 replaces the
`cargo xtask regen --grammar bbnf` entry with a two-stage fast-path
(phase-1 CSP-only parser + hand-written direct-descent CSP facets
parser). Your job: land the fast-path; preserve the current path under
`--no-fast`; close the cold-wall gate at ≤ 3 min.

## Worktree (ABSOLUTE ROOT — all work here)

`/Users/mkbabb/Programming/bbnf-wt-b6-w0-a`

Never leave that directory. Never touch
`/Users/mkbabb/Programming/bbnf-lang` — that is the orchestrator's
main checkout. `target/` symlinks to main; `data/` is seeded.

## Memory discipline (non-negotiable)

Before every cargo invocation:

    export CARGO_BUILD_JOBS=4

Prefer `cargo {test,check} --profile ax-iter` during iteration.
Use `dev` only when samply attribution is needed.

During iteration use `make iter-test-leaf` (fastest correctness
tier) and `cargo iter-check` (warm ≤ 0.5 s); reserve workspace
links for the W0 hard-gate runs.

Never run two cargo invocations concurrently. Memory budget per
README.md §Memory discipline for aggregate test binaries.

On session resume use:

    scripts/worktree-status.sh
    scripts/kill-all-rust.sh --dry-run

## Read first (required, in order)

1. `docs/instructions/README.md` — operational directives.
2. `docs/instructions/tranche/SPEC.md` — §Hard gates, §Runtime-
   evidence, §Activation-gate, §Scope-reveal, §Prelude annexes.
3. `docs/instructions/PROFILING.md` — bench/profile workflow
   (cold wall methodology).
4. `docs/tranches/B6/B6.md` — tranche plan; invariants 1-8,
   wave summary, critical-files table.
5. `docs/tranches/B6/waves/W0.md` — your wave spec.
6. `xtask/src/regen.rs` — the current entry; phase-1 split point
   lives at `regen_grammar`'s `compile_paths_request` call
   (line 222).
7. `crates/bootstrap/src/lib.rs` — phase-1 entry through
   `BbnfBootstrap` route.

## Scope — W0.a only

1. Create `xtask/src/csp_descent.rs` (~250 LOC) — hand-written
   direct-descent CSP parser for the BBNF self-host grammar.
2. Modify `xtask/src/regen.rs` to two-stage entry: phase-1 routes
   to the existing `BbnfBootstrap::parse` only when CSP-only
   pre-parse cannot reach a structural emit; otherwise the fast-
   path runs `csp_descent` and skips the IR pipeline.
3. Modify `xtask/src/main.rs` to add `--fast` / `--no-fast`
   flags; default fast-path ON.
4. Verify post-regen `crates/core/src/grammar/generated/bbnf.rs`
   is byte- or `prettyplease::unparse`-format-equivalent to its
   pre-W0 state.
5. Capture cold-wall artefact at `docs/benchmarks/post-B6-W0-
   walls.txt` (3 runs, median; cold per `make clean-incr` plus
   `find . -name .bbnf-cache -exec rm -rf {} +`).

Do NOT touch items outside your sub-phase. The egraph scheduler,
alias surface, JSON benches, IR audit tests are W1/W2 scope.

## File bounds

Allow-list:
- `xtask/src/regen.rs` — modify; replace `BbnfBootstrap::parse`
  call at line 222 with two-stage entry.
- `xtask/src/csp_descent.rs` — create; ~250 LOC direct-descent
  CSP parser.
- `xtask/src/main.rs` — modify; `--fast` / `--no-fast` flag
  plumbing.
- `xtask/Cargo.toml` — modify only if a crate dep needs to land
  for the CSP descent (declare in commit message).
- `crates/bootstrap/src/lib.rs` — modify; document the phase-1
  split point and route comment.
- `crates/core/src/grammar/generated/bbnf.rs` — regen via
  `cargo xtask regen --grammar bbnf` only; commit the post-W0
  output as part of the gate.
- `docs/benchmarks/post-B6-W0-walls.txt` — create; cold-wall
  artefact.

Forbidden:
- No edits under `crates/core/src/lower/`,
  `crates/core/src/backend/`, `crates/tape/src/`, `crates/ir/`,
  or any `.bbnf` grammar source. Substrate is W0-frozen.
- No `#[allow(...)]` introductions outside macro contexts.
- No legacy code, no backward-compat shim outside the named
  `--no-fast` flag (preserved for one tranche cycle, retired in
  B7's named restoration wave per SPEC §Transitional fallback).
- No edits to other grammars' generated files; W0 emits only
  for `bbnf.bbnf`.

## Hard gate

1. **Cold wall**: `time -p cargo xtask regen --grammar bbnf`
   3-run median ≤ 3 min, with `make clean-incr` + cache clear
   between runs. Artefact: `docs/benchmarks/post-B6-W0-walls.txt`.
2. **Output equivalence**: `crates/core/src/grammar/generated/
   bbnf.rs` byte- or `prettyplease`-format-equivalent to the
   pre-W0 file at master HEAD `f34f2e80`. Diff via `git diff
   --stat` plus a structural `cargo iter-check-full` exit-0
   verification. Artefact: diff captured to
   `docs/benchmarks/post-B6-W0-walls.txt`.
3. **Workspace nextest 1477/1477** post-W0; the fast-path's
   correctness verifies via the existing test surface
   (`make iter-test`).
4. **`cargo xtask regen --check` exit 0** across all 9 grammars
   post-W0; the fast-path emits the same byte sequence on
   second invocation (idempotent).
5. **`cargo bench-bbnf` median within 5 % of post-B5 baseline
   (2.806 ms)**. Hot-path bench non-regression; the fast-path
   is xtask-time, not parse-time, so the gate confirms no
   accidental tape-side regression.
6. **No `#[allow(...)]` introduced** anywhere in the diff
   outside macro contexts.

## Verification artefacts

- `docs/benchmarks/post-B6-W0-walls.txt` — cold wall medians,
  output equivalence diff, idempotent regen verification.
- Commit hashes for: csp_descent.rs creation, regen.rs two-
  stage edit, main.rs flag plumbing, generated/bbnf.rs regen
  output, walls.txt artefact.

## Commit discipline

- Use `git commit` with messages citing `B6.W0.a`.
- Commit at every natural milestone: csp_descent.rs land,
  regen.rs two-stage entry, main.rs flag, generated regen,
  walls capture.
- Never commit `crates/core/src/grammar/generated/bbnf.rs`
  with hand edits; only as the output of `cargo xtask regen
  --grammar bbnf`.

Commit message template:

    {type}(path): {one-line summary} (B6.W0.a)

    {3-5 lines: rationale, file + function changed,
    runtime-verifiable outcome.}

    {Evidence: walls.txt path, regen --check artefact,
    bench delta path.}

## Return format

≤ 500 words. Dense technical reporting only. Include:

1. Commit SHAs in order with one-line descriptions.
2. Cold-wall median + 3 raw timings.
3. Hard-gate status table — exit status + artefact path per
   gate item.
4. Output equivalence diff summary (byte-equivalent vs
   `prettyplease`-format-equivalent vs structurally divergent).
5. `git status --short` (must be empty or `target/` symlink).

No narrative filler. No "I then ran …" prose.

## Non-negotiables

- No stubs, no fallbacks, no feature flags, no `#[ignore]` or
  `#[allow(dead_code)]` introduced to hide incomplete work.
- One xtask path; legacy preserves under `--no-fast` for one
  tranche cycle, retired in B7. The flag is the only permitted
  shadow surface; introducing any other forks the architecture.
- Runtime evidence for every claim — grep alone is insufficient
  when the emitted code might be dead.
- Idiomatic, gestalt approach. If the right answer involves a
  structural split inside csp_descent.rs (e.g., a directory
  module), split per `feedback_no_god_modules` and
  `feedback_directory_module_structure`.
- If scope-reveal surfaces under contact, halt and report per
  SPEC §Scope-reveal — do not silently ship a partial fix or
  retreat to additive shadow-surface (`parse_dta()` style).

HARD CAP: 45 min. At 40 min commit whatever state you have; at
45 min halt unconditionally and report.

Begin.
```

---

## W1 — Egraph lazy + iter-check-az + gorgeous audit (2 parallel agents)

The two W1 agents share no file write access; both may run in
parallel. Agent W1.a owns the egraph scheduler deferral; W1.b
owns the alias surface plus the gorgeous audit.

### W1.a dispatch — egraph lazy passes

```
You are sub-agent W1.a for tranche B6. W0 closed the bootstrap
fast-path; W1 carves the iteration surface to AY-III's working
set. Your job: defer egraph scheduler passes whose `--check`
consumer count is zero; capture the deferral artefact under
`audit/W1-egraph-lazy-trace.md`.

## Worktree

`/Users/mkbabb/Programming/bbnf-wt-b6-w1-a`

Never touch `/Users/mkbabb/Programming/bbnf-lang`.

## Memory discipline + read first

Same as W0.a. Read `docs/tranches/B6/B6.md`, `waves/W1.md`, and:
- `crates/egraph/src/scheduler.rs` — `BackoffScheduler::run`.
- `crates/egraph/src/csp_scheduler.rs` — CSP scheduler mirror.
- `docs/tranches/B6/waves/W1.md` — your wave spec.
- `docs/instructions/PROFILING.md` — cold-wall methodology.

## Scope — W1.a only

1. Modify `crates/egraph/src/scheduler.rs`:
   - In `BackoffScheduler::run`, classify each rule by its
     `--check` consumer-count metadata. Rules with zero
     consumers downstream of the regen-check entry defer to a
     `lazy_run` invocation; only `regen --check` triggers them.
2. Modify `crates/egraph/src/csp_scheduler.rs` to mirror the
   classification.
3. Author `docs/tranches/B6/audit/W1-egraph-lazy-trace.md`
   citing the `cargo expand` slice showing the deferred-pass
   call sites and a samply trace confirming zero self-time
   for those passes during routine `iter-check`.

## File bounds

Allow-list:
- `crates/egraph/src/scheduler.rs` — modify
- `crates/egraph/src/csp_scheduler.rs` — modify
- `docs/tranches/B6/audit/W1-egraph-lazy-trace.md` — create

Forbidden:
- No edits to `.cargo/config.toml`, `Makefile`, `crates/gorgeous/`
  (W1.b scope).
- No edits to `crates/core/`, `crates/tape/`, `crates/ir/`.
- No edits to `xtask/` (W0 scope, frozen).

## Hard gate

1. **Cold `iter-check-full` wall ≥ 30 % reduction**: pre-W1
   > 660 s, post-W1 ≤ 460 s; 3-run median per the cold-wall
   methodology. Artefact: `docs/benchmarks/post-B6-W1-walls.txt`.
2. **Workspace nextest 1477/1477 non-regressing** post-W1.
3. **`cargo xtask regen --check` exit 0** across all 9 grammars
   — the deferred passes still fire under `--check`, verifying
   the lazy pattern preserves correctness.
4. **`audit/W1-egraph-lazy-trace.md` lands** with `cargo expand`
   slice citation and samply trace; the deferred-pass call
   sites are runtime-verifiable.
5. **No `#[allow(...)]` introduced** outside macro contexts.

## Return format

≤ 500 words. Same shape as W0.a.

HARD CAP: 45 min. At 40 min commit; at 45 min halt.

Begin.
```

### W1.b dispatch — iter-check-az alias + gorgeous audit

```
You are sub-agent W1.b for tranche B6. Your job: add the
`iter-check-az` carve-out alias targeting AY-III's workspace; verify
gorgeous default features; capture the alias scope at
`audit/W1-iter-az-scope.md`.

## Worktree

`/Users/mkbabb/Programming/bbnf-wt-b6-w1-b`

Never touch `/Users/mkbabb/Programming/bbnf-lang`.

## Memory discipline + read first

Same as W0.a. Read `docs/tranches/B6/B6.md`, `waves/W1.md`, and:
- `.cargo/config.toml` — existing alias surface (`iter-check`,
  `iter-check-full`, `iter-check-{lsp,prettify,bootstrap}`).
- `crates/gorgeous/Cargo.toml` — default-features check
  (already `default = []`).

## Scope — W1.b only

1. Add `iter-check-az` alias to `.cargo/config.toml` `[alias]`
   block. Scope: the AY-III working set —
   `--workspace --exclude bbnf-bootstrap --exclude gorgeous
   --exclude bbnf-analysis --exclude bbnf-lsp` plus any
   downstream-of-egraph crates AY-III's plan declares non-load-
   bearing for routine iteration.
2. Verify `crates/gorgeous/Cargo.toml` carries `default = []`
   already; capture the no-op proof in
   `audit/W1-iter-az-scope.md`.
3. Author `docs/tranches/B6/audit/W1-iter-az-scope.md` listing
   the alias's bin set, with a one-line rationale per
   inclusion.

## File bounds

Allow-list:
- `.cargo/config.toml` — modify; add one alias line.
- `docs/tranches/B6/audit/W1-iter-az-scope.md` — create.

Forbidden:
- No edits to `crates/egraph/`, `crates/gorgeous/Cargo.toml`
  (gorgeous needs no edit; only the verification artefact).
- No edits under `xtask/`, `crates/core/`, `crates/tape/`,
  `crates/ir/`.

## Hard gate

1. **Cold `iter-check-az` wall ≤ 30 s** with the cold-wall
   methodology; 3-run median. Artefact: `post-B6-W1-walls.txt`.
2. **Workspace nextest non-regressing** — `iter-check-az`
   coverage's bin set still compiles cleanly under the alias.
3. **`gorgeous/Cargo.toml` `default = []` proof** captured in
   `audit/W1-iter-az-scope.md`.
4. **`cargo bench-bbnf` median within 5 % of B5 baseline
   2.806 ms** — the alias surface is read-only with respect to
   the hot path; non-regression confirms no accidental side
   effect.

## Return format

≤ 400 words. Same shape as W0.a.

HARD CAP: 45 min. At 40 min commit; at 45 min halt.

Begin.
```

---

## W2 — JSON test partitioning + IR audit feature-gate (2 parallel agents)

W2.a partitions the JSON benches; W2.b feature-gates the IR audits
plus the close-ceremony Makefile wiring. The two agents share no
file write access.

### W2.a dispatch — JSON bench partition

```
You are sub-agent W2.a for tranche B6. W1 closed the iteration
carve-out. Your job: partition the JSON monolithic bench into per-
fixture modules; confirm the bench surface stays bit-equivalent.

## Worktree

`/Users/mkbabb/Programming/bbnf-wt-b6-w2-a`

Never touch `/Users/mkbabb/Programming/bbnf-lang`.

## Memory discipline + read first

Same as W0.a. Read:
- `docs/tranches/B6/B6.md`, `waves/W2.md`.
- `crates/core/benches/json/monolithic.rs` — the unified bench
  to partition.
- `docs/instructions/PROFILING.md` — bench surface contract.

## Scope — W2.a only

1. Split `crates/core/benches/json/monolithic.rs` into a
   directory module: create `crates/core/benches/json/
   monolithic/{twitter,canada,citm,data,data_xl}.rs`; convert
   the original `monolithic.rs` into the `mod.rs` declaration
   that aggregates the five.
2. Add `iter-test-json-partitioned` alias to `.cargo/config.toml`
   for the partitioned surface (per-fixture filter by
   substring).
3. Capture warm-wall artefact at `docs/benchmarks/post-B6-W2-
   walls.txt`.

## File bounds

Allow-list:
- `crates/core/benches/json/monolithic.rs` — modify-carve;
  becomes a thin `mod.rs` aggregator.
- `crates/core/benches/json/monolithic/twitter.rs` — create.
- `crates/core/benches/json/monolithic/canada.rs` — create.
- `crates/core/benches/json/monolithic/citm.rs` — create.
- `crates/core/benches/json/monolithic/data.rs` — create.
- `crates/core/benches/json/monolithic/data_xl.rs` — create.
- `.cargo/config.toml` — modify; add one alias line.

Forbidden:
- No edits under `crates/core/tests/`, `crates/core/Cargo.toml`,
  `Makefile` (W2.b scope).
- No semantic changes to bench logic; the per-fixture splits
  are mechanical extractions of the existing macro-expanded
  body.

## Hard gate

1. **Warm `iter-test --profile ax-iter` wall ≥ 20 % reduction**:
   pre-W2 ~ 20 s, post-W2 ≤ 16 s; 3-run median. Artefact:
   `post-B6-W2-walls.txt`.
2. **`cargo bench-json` median within 1 % per fixture** vs
   pre-W2 (the partition is structural; per-fixture timings
   stay bit-equivalent).
3. **Workspace nextest non-regressing** post-W2.
4. **`cargo xtask regen --check` exit 0** across all 9
   grammars.

## Return format

≤ 400 words. Same shape as W0.a.

HARD CAP: 45 min. At 40 min commit; at 45 min halt.

Begin.
```

### W2.b dispatch — IR audit feature-gate + close ceremony

```
You are sub-agent W2.b for tranche B6. Your job: feature-gate the
IR audit tests behind `ir-audit`; wire the close-ceremony Makefile
target to enforce the feature; preserve close-gate coverage.

## Worktree

`/Users/mkbabb/Programming/bbnf-wt-b6-w2-b`

Never touch `/Users/mkbabb/Programming/bbnf-lang`.

## Memory discipline + read first

Same as W0.a. Read:
- `docs/tranches/B6/B6.md`, `waves/W2.md`.
- `crates/core/tests/payload_layouts.rs` — IR audit (the
  `compute_payload_layouts` validation harness).
- `crates/core/tests/projection_totality.rs` — IR audit (the
  AY-II.W0'.b wire-contract).
- `crates/core/Cargo.toml` — features section.
- `Makefile` — `ay-bench-close` target.

## Scope — W2.b only

1. Add `ir-audit` feature flag to `crates/core/Cargo.toml`
   `[features]` section.
2. Gate `crates/core/tests/payload_layouts.rs` behind
   `#![cfg(feature = "ir-audit")]` (file-level cfg).
3. Gate `crates/core/tests/projection_totality.rs` behind
   `#![cfg(feature = "ir-audit")]`.
4. Modify `Makefile`: `ay-bench-close WAVE=close` adds
   `--features ir-audit` so the close ceremony runs the audits;
   routine `iter-test` does not.

## File bounds

Allow-list:
- `crates/core/tests/payload_layouts.rs` — modify; add
  file-level cfg.
- `crates/core/tests/projection_totality.rs` — modify; add
  file-level cfg.
- `crates/core/Cargo.toml` — modify; add `ir-audit` feature.
- `Makefile` — modify; `--features ir-audit` on close target.

Forbidden:
- No edits under `crates/core/benches/`, `.cargo/config.toml`
  (W2.a scope).
- No semantic changes to test bodies; only the file-level cfg
  attribute.

## Hard gate

1. **Routine `cargo iter-test` warm wall ≥ 20 % reduction** —
   the gated tests do not compile under routine. Artefact:
   `post-B6-W2-walls.txt`.
2. **`make ay-bench-close WAVE=close` exit 0** with
   `--features ir-audit`; the gated tests fire at close.
3. **Workspace nextest non-regressing** at routine
   (1477/1477) and at close (audit tests included).
4. **`cargo iter-test --features ir-audit` exit 0** with the
   gated tests visible.

## Return format

≤ 400 words. Same shape as W0.a.

HARD CAP: 45 min. At 40 min commit; at 45 min halt.

Begin.
```

---

## Cross-wave invariants (orchestrator-enforced)

Per the parent index `B6.md` invariants 1-8: every wave's hard
gate keeps workspace nextest at 1477/1477, `cargo xtask regen
--check` at exit 0 across all 9 grammars, `cargo bench-bbnf`
median within 5 % of B5 baseline 2.806 ms, no `#[allow(...)]`
introductions outside macro contexts. The orchestrator verifies
these invariants at every wave boundary before cherry-picking
onto master per SPEC §Sequencing.
