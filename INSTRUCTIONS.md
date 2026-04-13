# INSTRUCTIONS.md — Operational Directives for Agents and Orchestrators

These directives govern ALL work in this repository: implementation,
auditing, benchmarking, profiling, and testing. They are non-negotiable.
Agents must internalize them before beginning work.

## Code discipline

- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues.**
  If something doesn't work, find and fix the root cause. Temporary
  fixes become permanent debt.
- **NO legacy code.** Architectural transpositions for elegance,
  simplicity, and performance are mandatory. Delete dead code; don't
  comment it out or gate it behind feature flags.
- **NO backward compatibility shims.** Always migrate fully. No
  adaptor layers, re-exports for removed items, or `_unused` renames.
- **Commit frequently with `/commit`.** Each natural milestone gets
  its own commit. Don't batch unrelated changes.
- **Use `rule_kind()` dispatch, not string matching.** The lowering
  layer identifies nodes by their tape record identity, not by their
  span text content. String matching is acceptable ONLY as a recovery
  path when the tape shape masks the rule_kind (document why).
- **Generated files are output of fresh regen; never hand-patch.**
  `generated.rs` is produced by `scripts/bootstrap-bbnf.sh`. The
  only legitimate edits to it are via that script.

## Parallel agent orchestration

- **Up to 6 parallel agents per wave**, isolated worktrees
  (`isolation: "worktree"`), cherry-pick onto master.
- **No file collisions across agents in the same wave.** Exclusive
  write per file per wave. Cross-wave conflicts resolved by
  sequencing.
- **Commit before parallelizing.** Never let sub-agents race on
  shared files. Master must be clean before spawning worktree agents.
- **Each agent gets explicit file bounds** in its prompt: which files
  it may modify, which it must not touch.
- **Agent prompts are self-contained.** The agent starts with no
  context from the conversation. Brief it like a colleague who just
  walked in: what to do, why, which files, what the hard gate is.

## Running expensive commands

**ALWAYS write output to a file, then grep/search over it.** Never
re-run an expensive command for a different slice of output.

```bash
# CORRECT: run once, search many times
cargo test --workspace 2>&1 > /tmp/test-out.txt
grep "test result" /tmp/test-out.txt
grep "FAILED" /tmp/test-out.txt
tail -20 /tmp/test-out.txt

# WRONG: re-running the entire build for each query
cargo test --workspace 2>&1 | grep "test result"   # 3 minutes
cargo test --workspace 2>&1 | grep "FAILED"         # 3 more minutes
```

This applies to: `cargo test`, `cargo bench`, `cargo expand`,
`cargo build`, `cargo check --workspace`, `samply record`, and
any command taking > 30 seconds.

## Cache clearing

**Clear ALL `.bbnf-cache` directories before any bench, regen,
or proc-macro expansion test.** The derive macro caches expansions
and `cargo clean` does NOT clear them.

```bash
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
```

Also clear the `crates/target/.bbnf-cache/` directory specifically
— `cargo clean` from the workspace root does not reach it.

When the bbnf-analysis crate ICEs (recurring nightly issue), clean
it specifically:

```bash
cargo clean -p bbnf-analysis
```

## Testing

```bash
# Full leaf-crate test suite
cargo test -p bbnf-tape -p bbnf-ir -p egraph 2>&1 > /tmp/leaf-tests.txt
grep "test result" /tmp/leaf-tests.txt

# Grammar roundtrip (the primary correctness gate)
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
cargo test -p bbnf --test grammar_roundtrip 2>&1 > /tmp/roundtrip.txt
grep "^test\|test result" /tmp/roundtrip.txt

# Payload layout validation
cargo test -p bbnf --test payload_layouts 2>&1 > /tmp/payload.txt
grep "test result" /tmp/payload.txt

# All bbnf-specific tests
cargo test -p bbnf --test grammar_roundtrip --test payload_layouts \
  --test regex_classify --test optimize --test runtime_root \
  2>&1 > /tmp/bbnf-tests.txt
grep "test result" /tmp/bbnf-tests.txt
```

Tests live in `tests/` directories only — never inline `#[cfg(test)]`
in `src/` files.

## Benchmarking

Benchmarks must run sequentially to avoid interference. Single
invocation only — never run bench suites in separate commands.

```bash
# Clear caches first
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null

# Compile pipeline (always works)
cargo bench -p bbnf --bench compile_pipeline 2>&1 > /tmp/bench-compile.txt
grep "bench:" /tmp/bench-compile.txt

# JSON monolithic parse
cargo bench -p bbnf --bench json_monolithic 2>&1 > /tmp/bench-json.txt
grep "bench:" /tmp/bench-json.txt

# CSS L4 parse
cargo bench -p bbnf --bench css_l4 2>&1 > /tmp/bench-css.txt
grep "bench:" /tmp/bench-css.txt

# Google Sheets
cargo bench -p bbnf --bench google_sheets_monolithic 2>&1 > /tmp/bench-sheets.txt
grep "bench:" /tmp/bench-sheets.txt

# CSS competitors baseline
cargo bench -p bbnf --bench css_competitors 2>&1 > /tmp/bench-competitors.txt
grep "bench:" /tmp/bench-competitors.txt
```

Cold per-parse only — warm/cached benchmarks are disingenuous.
The bench binaries use `#[global_allocator] mimalloc`.

## Profiling with samply

samply needs `debug=true` in the bench profile and INTERACTIVE
`samply record` (not `--save-only`).

```bash
# 1. Ensure debug info is enabled for the bench profile
# In Cargo.toml [profile.bench]: debug = true

# 2. Build the bench binary
cargo bench -p bbnf --bench json_monolithic --no-run

# 3. Profile interactively (opens Firefox Profiler)
samply record target/release/deps/json_monolithic-* --bench canada

# 4. Wait 5-8 seconds after profiler page loads for symbolication
# 5. Save the profile from Firefox Profiler UI
```

**DO NOT use `--save-only` + `samply load`** — symbols show as hex
addresses. The interactive path uses a local symbol server that
Firefox Profiler queries on-demand.

After profiling, remove `debug = true` from the bench profile to
avoid bloating release builds.

## Bootstrap regen

```bash
# Clear ALL caches
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null

# Regen
bash scripts/bootstrap-bbnf.sh

# Verify roundtrip
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
cargo test -p bbnf --test grammar_roundtrip 2>&1 > /tmp/roundtrip.txt
grep "^test\|test result" /tmp/roundtrip.txt

# Verify idempotency
cp crates/core/src/grammar/generated.rs /tmp/gen1.rs
rm -rf target/.bbnf-cache/ crates/target/.bbnf-cache/
bash scripts/bootstrap-bbnf.sh
diff /tmp/gen1.rs crates/core/src/grammar/generated.rs
# Must be empty (zero diff)
```

## Cargo expand for codegen verification

```bash
# Expand a specific bench target
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
cargo expand -p bbnf --bench json_monolithic 2>/dev/null > /tmp/expand-json.txt

# Search the expansion
grep "push_leaf_with\|push_compound\|KvPair\|meta_idx" /tmp/expand-json.txt | head -20
grep "fn __value\|fn __object\|fn __array" /tmp/expand-json.txt | head -10

# Expand bbnf-bootstrap for bootstrap parser analysis
cargo expand -p bbnf-bootstrap --lib 2>/dev/null > /tmp/expand-bootstrap.txt
grep "fn __grammar\|fn __rule\|fn __factor" /tmp/expand-bootstrap.txt | head -10
```

Always redirect `cargo expand` to a file — output is typically
10,000+ lines.

## Performance claims

- **Every claimed perf win has a samply diff.** No speculative
  throughput numbers.
- **`cargo expand` evidence for every codegen activation claim.**
  Visual inspection of the expanded code, not just test-pass.
- **Run actual profiler, don't guess from static analysis.**
  samply/Instruments/perf on the actual bench binary.
- **Performance docs must reconstruct actual timeline from commits;
  don't fabricate or embellish.**

## Architecture invariants

- **One codegen path.** No combinator fallback. One regex system
  (HIR). KISS.
- **One propagate() method**, not suffixed variants. The solver
  determines optimal strategy internally.
- **General-purpose constructs in own crate(s)**, not stuffed into
  domain crates. The egraph substrate, cost model, and CSP solver
  are general-purpose.
- **Regex analysis in bbnf-regex**, not bbnf-lang. Only IR-specific
  items (kernel coverage, recognizer mining, codegen routing) in
  bbnf-lang.
- **`RegexInfo` is the sole bridge** between the regex library tier
  and the codegen tier. All classification, HIR, FIRST sets, and
  engine feasibility flow through it.
- **Fixed-point loops use LLVM-style Changed bool**, not structural
  hash. Content hash as debug_assert only.
- **Decision points are pluggable** (cost model, pattern registry,
  rewrite rules), not hardcoded branches.
