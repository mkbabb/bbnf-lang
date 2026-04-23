# bbnf-derive cache lift to $XDG_CACHE_HOME

**Status**: DEFERRED — routes to tranche BA per Agent 2's scope
recommendation in `TOOLCHAIN-SOTA.md §Proc-macro cost model P.1`. This
document captures the concrete design for BA.W0 to execute; B1 does not
land the lift itself.

B1 retains the existing `target/.bbnf-cache/` layout and leans on the
bootstrap-script fix (see `patches/scripts/bootstrap-bbnf.sh.action`) to
recover the ≥130s → ≤10s savings via content-hash guard rather than cache
relocation.

## Current layout

`crates/derive/src/lib.rs:144-169` resolves the cache directory:

```rust
fn cache_dir() -> Option<PathBuf> {
    if let Ok(target) = std::env::var("CARGO_TARGET_DIR") {
        return Some(PathBuf::from(target).join(".bbnf-cache"));
    }
    // Walk up from CARGO_MANIFEST_DIR for target/
    if let Ok(manifest) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut dir = PathBuf::from(manifest);
        loop {
            let candidate = dir.join("target");
            if candidate.is_dir() {
                return Some(candidate.join(".bbnf-cache"));
            }
            if !dir.pop() { break; }
        }
    }
    None
}
```

Two problems:

1. **Per-target isolation.** Every worktree has its own `target/`, so every
   new worktree (8 parallel agents) pays the cold-expansion cost on first
   access (≥130s × 8 worktrees = ~17 minutes wasted).

2. **Bootstrap nuke.** `scripts/bootstrap-bbnf.sh:30` unconditionally
   `rm -rf target/.bbnf-cache/` before `cargo expand`. This throws away the
   cache even when the grammar is unchanged. **B1 fixes this in isolation
   (script-level)**; the cache-lift defers the structural move.

## Target layout (BA)

### Resolution order

1. `$BBNF_DERIVE_CACHE` (explicit override for CI / sandboxes).
2. `$XDG_CACHE_HOME/bbnf-derive/`.
3. `$HOME/.cache/bbnf-derive/` (Linux default).
4. `$HOME/Library/Caches/bbnf-derive/` (macOS default).
5. Fall back to `target/.bbnf-cache/` (preserves current behaviour if none
   of the above resolve).

Rationale: XDG paths are shared across worktrees and across `cargo clean`
(`cargo clean` only touches `target/`), so the cache survives both. The
fallback preserves existing behaviour for sandboxed CI runners that may not
expose `$HOME`.

### Content-keyed filename (unchanged)

Existing key: hash of `(grammar source content + parser attributes + struct
ident + bbnf version + BBNF_SCHEMA_VERSION)`. Well-factored — preserve
verbatim. Rename input only: include the host `rustc` version too, so a
nightly bump invalidates stale entries (prevents the AttrId-class ICE from
propagating into an XDG cache shared across toolchain bumps).

New key: `hash(grammar_content, parser_attrs, ident, bbnf_version,
BBNF_SCHEMA_VERSION, rustc_version_commit_hash)`.

### Eviction

Per-cache-invocation:
1. On write, check total cache size via `std::fs::read_dir + metadata`.
2. If size > 2 GB, scan entries by `atime` (access time via `fs::metadata`),
   evict oldest until size < 1.5 GB (hysteresis to avoid thrash).
3. Walk is O(entries) — typical cache has ≤100 entries (one per `#[derive(
   Parser)]` call-site variant), so scan is <1ms.

Hard cap 2 GB is arbitrary; tune after one month of observed steady-state.

### Concurrency

Two parallel proc-macro invocations may write the same key. Current code
uses `write_tmp + rename` for atomicity (`crates/derive/src/lib.rs:185-190`).
The XDG move adds **one new risk**: two worktrees writing the same key
simultaneously. Resolution:

- Keep `write_tmp + rename` — `rename(2)` is atomic within a filesystem.
- Add a `flock(2)` on the cache directory during eviction only (not during
  per-key write — unnecessary since keys are content-addressed).

### Code delta (crates/derive/src/lib.rs:144-169)

```rust
fn cache_dir() -> Option<PathBuf> {
    // 1. Explicit override
    if let Ok(p) = std::env::var("BBNF_DERIVE_CACHE") {
        return Some(PathBuf::from(p));
    }
    // 2. XDG_CACHE_HOME
    if let Ok(xdg) = std::env::var("XDG_CACHE_HOME") {
        return Some(PathBuf::from(xdg).join("bbnf-derive"));
    }
    // 3. macOS default
    #[cfg(target_os = "macos")]
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home).join("Library/Caches/bbnf-derive"));
    }
    // 4. Linux default
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home).join(".cache/bbnf-derive"));
    }
    // 5. Fallback: legacy target-local
    if let Ok(target) = std::env::var("CARGO_TARGET_DIR") {
        return Some(PathBuf::from(target).join(".bbnf-cache"));
    }
    if let Ok(manifest) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut dir = PathBuf::from(manifest);
        loop {
            let candidate = dir.join("target");
            if candidate.is_dir() {
                return Some(candidate.join(".bbnf-cache"));
            }
            if !dir.pop() { break; }
        }
    }
    None
}
```

Net LOC change: +15 lines in cache_dir; +40 LOC for `evict_if_oversize()`;
key-hash changes include one additional input (`rustc_version`). Total ~60
LOC delta in `crates/derive/src/lib.rs`.

## Migration

One-time move of existing cache contents (optional; safe to skip, as
content-keying makes cold regen correct). If adopted:

```bash
# $XDG_CACHE_HOME/bbnf-derive/ inherits any previously-cached entries
# from target/.bbnf-cache/ across all worktrees.
for wt in .claude/worktrees/*/; do
    if [ -d "$wt/target/.bbnf-cache" ]; then
        mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/bbnf-derive"
        cp -n "$wt/target/.bbnf-cache/"*.rs \
              "${XDG_CACHE_HOME:-$HOME/.cache}/bbnf-derive/"
    fi
done
```

`cp -n` (no-clobber) preserves whichever worktree's entry lands first; all
are content-equivalent per the content-keyed invariant.

## Why deferred to BA (not B1)

1. **B1 is bounded-infra.** The 20-file divan migration + rust-toolchain pin
   + config.toml rewrite already consumes the B1 budget.
2. **The bootstrap-script fix captures the primary win.** `rm -rf
   target/.bbnf-cache/` deletion (W0.b scope) resolves the ≥130s wall in
   fresh-worktree bootstrap without changing the cache location.
3. **The cache-lift is a BA/BB-class structural change.** It touches
   concurrency (flock), eviction (LRU scan), and cross-platform path
   resolution (macOS/Linux/CI). Agent-day estimate: 1-2 days.
4. **BA has a natural home for it.** If BA's scope is proc-macro cost
   infrastructure (cache layout + watt preparation + per-site measurement),
   the cache-lift is the first move.
