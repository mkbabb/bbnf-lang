---
title: Build & Iteration
order: 10
section: Development
---

# Build & Iteration

How the workspace is laid out for fast inner loops, where the cache lives,
and the discipline that keeps cargo from being re-run unnecessarily.

## Cache-bump policy

`bbnf-derive` keeps a content-addressed disk cache of generated parser
TokenStreams under `target/.bbnf-cache/`. The cache key folds in every grammar
file (entry + transitive `@import` deps), the parser attributes, the consuming
struct ident, the bbnf crate version, and `BBNF_SCHEMA_VERSION`.

`BBNF_SCHEMA_VERSION` lives in `crates/derive/src/lib.rs` as a `const u64`. Bump
it in the **same commit** as any breaking change to:

- `bbnf_ir::GrammarIR` field shape, semantics, or invariants
- `bbnf_ir::TypeDesc` projection rules
- The Rust codegen output shape — enum naming, method signatures, monolithic
  emitter contracts, attribute handling

If the change is non-breaking — adding an optional field, refining a comment,
deleting dead code that does not surface in generated output — leave the
version alone. The `build.rs` `rerun-if-changed` signals will still cause
cargo to re-run the proc-macro on edits to the source trees, which is enough
for in-tree development.

The two tiers exist because unrelated edits to `derive/`, `core/`, or `ir/`
should not bust the on-disk cache for downstream consumers. Forgetting to bump
on a real break is the more common failure mode — when in doubt, bump.

## Output-to-file discipline

Every long cargo invocation captures its combined stdout and stderr to a
single log file under `/tmp/`. Subsequent inspection (variant filters, line
counts, error grepping) reads from that file. Re-invoking cargo to look at a
different slice of the same output is wasted compile time.

```
cargo test -p bbnf-ir 2>&1 | tee /tmp/bbnf-ir-test.log
cargo bench -p bbnf 2>&1 | tee /tmp/bbnf-bench.log
cargo build -p gorgeous --lib 2>&1 | tee /tmp/gorgeous-build.log
```

After the first run, drive every follow-up off the file: `grep -n 'error\['
/tmp/bbnf-ir-test.log`, `tail -n 200 /tmp/gorgeous-build.log`, and so on.
Re-run cargo only when you have new source on disk worth measuring, never to
re-render output.

The same rule applies to `make test`, `make bench`, and any wrapper script
that shells out to cargo. Tee at the outermost layer.

## Bisect script

`scripts/bisect-fastpath.sh` wraps `git bisect run` with per-step log capture
so a regression can be located and re-inspected without manual replay.

```
scripts/bisect-fastpath.sh <good-commit> <bad-commit> [command]
```

The default command is `cargo check -p gorgeous --lib`. Each bisect step's
combined output is written to `/tmp/bisect-<short-hash>.log`, so once the
offending commit is identified, the failure mode at that commit (and at every
midpoint cargo touched on the way) is already on disk for re-inspection. The
script aborts cleanly if the working tree is dirty or a bisect is already in
progress; pass `--help` for full usage.

## Dev profile overrides

`Cargo.toml` carries a small set of `[profile.dev.package.*]` overrides that
lift `opt-level` to `1` for the three crates that dominate downstream debug
build time:

- `bbnf-ir` — IR pipeline runs on every proc-macro expansion
- `csp-solver` — fixed-point solving over FIRST/FOLLOW lattices
- `parse_that` — combinator core every generated parser links against

`opt-level = 1` enables LLVM's mid-tier passes without losing line numbers or
local variable inspection — debuggers still single-step cleanly. The cost is a
slightly longer first build of those three crates; the win is that every
subsequent cargo invocation in the workspace pays a much smaller cost on the
hot critical path through them. The `[profile.dev]` block also bumps
`codegen-units` to `256` so the rest of the workspace parallelises across all
available cores.

## Linker override

Apple's default `ld64` is acceptable for this tree. A faster linker can be
installed optionally for a debug-link speedup, and the uncommented examples
live in `.cargo/config.toml` under `[target.aarch64-apple-darwin]`.

```
brew install mold            # then uncomment the mold block
brew install llvm            # then uncomment the ld.lld block
```

On Debian/Ubuntu: `apt install mold` or `apt install lld`. On Arch:
`pacman -S mold lld`.

Both options require the linker binary to resolve on `PATH` (or at the
absolute path the config specifies). The workspace intentionally ships
without an active override so a fresh checkout always builds — opt in
after confirming the linker is installed.

## Test binary consolidation

`crates/ir/tests/` was collapsed from 33 individual `.rs` files into four
binary roots. Each integration test file in cargo compiles to its own binary
that links the entire crate, so 33 roots meant 33 link steps on every clean
test run. Folding related tests into module trees under four roots cuts that
to four link steps without losing any test isolation.

| Group     | Sub-modules                                                                                                                                        |
|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| lattices  | emission_tier_lattice, csp_components, csp_materialization, csp_types, materialization, materialization_eclass_gate, types                         |
| passes    | recognizer, dag_invariant, charset, regex_first, context_facts, dag_roundtrip, passes_alias, passes_dispatch, passes_follow, passes_inline, passes_lr, passes_optimize, passes_prefix, passes_prune, passes_span |
| egraph    | egraph_analysis, egraph_grammar, egraph_interner, egraph_roundtrip, egraph_suffix, type_desc_interner                                              |
| vm        | compiler, cost_weights_unified, cross_rule_csp, debug, interpreter                                                                                 |

Add new tests to the relevant group module rather than spawning a new binary
root. Cross-group tests belong with the group whose subject matter they
exercise most.

## Typical iteration target

The numbers below describe the inner loop on a clean checkout with the
overrides above active. They are budgets, not guarantees — regressions past
them warrant a profile.

| Build                                            | Budget |
|--------------------------------------------------|--------|
| Cold workspace `cargo build`                     | ≤ 4 min |
| Warm `cargo build -p gorgeous --lib` (no edits)  | ≤ 30 s |
| `cargo test -p bbnf-ir`                          | ≤ 60 s |

If any of these regress without an obvious cause, bisect with
`scripts/bisect-fastpath.sh` against the most recent known-good revision.
