# Q7 — derive-cache invalidation key

**Status**: resolved, implementation scoped into AZ.W0
**Owner tranche**: AZ.W0
**Decision date**: 2026-04-23
**Affects**: `crates/derive`, cache directory layout, every incremental build

## Context

`crates/derive` caches expanded output of its proc-macros keyed by
input. A cache miss is a full re-expansion; a stale hit is a silent
correctness bug that surfaces only when downstream compilation or
runtime behavior diverges. Historically this cache has been a source
of sporadic debugging pain: contributors find that a grammar change
produces no visible effect until the cache is nuked by hand, or
conversely that a derive-crate update produces an effect without the
grammar changing because the old key collided.

The invalidation key is the mechanism that distinguishes these cases.
A too-narrow key (grammar text only) produces stale hits across
derive-crate upgrades. A too-broad key (whole-project hash) produces
cache-miss storms on every unrelated edit. The question was what
factors to combine and in what shape.

A secondary concern: historical cache paths live inside `target/`,
which CI and cleanup scripts routinely wipe. This conflates "clean
build" with "cold cache" and hides hit-rate measurement.

## Decision

**Composite key**: `(grammar-sha256, bbnf-derive-crate-version,
rustc-version-sha)`. Flagged as a chronic pain point and scoped with
explicit discipline requirements on robustness, performance,
observability, and migration. Implementation lands in AZ.W0 (moved
from BA.W0 under the Shape C re-sequence — see
`00-tape-abrogation-shape-c.md`).

## Reasoning

The three factors correspond to the three known staleness modes. The
grammar SHA catches grammar edits. The derive-crate version catches
macro-behavior changes. The rustc version SHA catches compiler
behavior that the derive crate's output depends on (hygiene edges,
macro-expansion order). Any one of the three changing invalidates;
all three unchanged is a safe hit.

A composite key is stable against false-shared edits — a change to an
unrelated crate does not invalidate. It is also stable against compiler
bumps: a rustc point release bumps the version SHA and sweeps the
cache, which is the correct behavior, not a bug.

The alternatives were rejected: a pure content hash of the expansion
input misses rustc and derive-crate drift; a per-contributor cache
bypass is a workaround; a "nuke on suspicion" script is the status quo
and the reason this is chronic.

Moving the cache directory from `target/.bbnf-cache/` to
`$XDG_CACHE_HOME/bbnf-derive/` decouples cache lifetime from build
output. Cleanups that wipe `target/` no longer nuke the cache. CI
opts into wiping the XDG path explicitly if it wants a cold run.

## Resolution mechanism

Discipline requirements that ship alongside the key change:

- **Robustness**: test suite under
  `crates/derive/tests/cache_invalidation/` exercises each factor
  independently (grammar-only change, derive-crate-only bump,
  rustc-only bump) and all combinations.
- **Performance**: cache hit resolves in under 50 ms. Key derivation
  is memoized per process. LRU eviction at 2 GB cache size.
- **Observability**: every miss logs the reason (which factor
  differed). No silent misses. Logging routes through the existing
  tracing subscriber at debug level.
- **Migration**: one-time move from `target/.bbnf-cache/` to
  `$XDG_CACHE_HOME/bbnf-derive/`. Old path detection emits a single
  warning and removes the legacy directory after migration.

## Follow-up gate

The invalidation test suite is the standing gate — any factor that
regresses breaks CI. Cache-hit latency is a tracked metric; sustained
regression past 50 ms opens a perf bug. Silent-miss reports (debug log
review) run quarterly.

## References

- `crates/derive/src/cache.rs` (key composition — modified in AZ.W0)
- `crates/derive/tests/cache_invalidation/` (new, AZ.W0)
- Q0 re-sequence: `00-tape-abrogation-shape-c.md`
