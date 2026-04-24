# Cross-repo toolchain propagation

**Scope**: bbnf-lang's B1 toolchain migration extends to sibling repos that
participate in the path-patched crate graph. Workspace-member crates inherit
bbnf-lang's `.cargo/config.toml` and `rust-toolchain.toml`; sibling repos
(separate git roots, standalone workspaces) need their own drafts.

Wave 1-B is independently cataloguing 16 repos; this document's enumeration
is the B1-specific subset (the repos B1 toolchain decisions directly touch).

## Propagation matrix

| Repo | `rust-toolchain.toml` | `.cargo/config.toml` | divan | nextest |
|---|---|---|---|---|
| bbnf-lang (this repo) | ADD (authoritative pin) | REWRITE (§patches/config.toml.draft) | MIGRATE | REQUIRE |
| `../parse-that` | ADD (same pin) | ADD (path patches + ax-iter profile) | MIGRATE if bench'd | REQUIRE |
| `../pprint` | ADD (same pin) | ADD (minimal) | MIGRATE if bench'd | REQUIRE |
| workspace-member crates | INHERIT | INHERIT | INHERIT | INHERIT |
| `../csc411` (csp-solver source) | ADD (same pin) | ADD if missing | MIGRATE | REQUIRE |

## Sibling repo drafts (abbreviated)

### `../parse-that/rust-toolchain.toml`

Identical content to bbnf-lang's `rust-toolchain.toml.draft`. parse-that is
the lowest-level path-patched dependency (bbnf-regex, parse_that); it MUST
pin the same nightly to guarantee proc-macro compatibility with
bbnf-derive's cached TokenStreams.

### `../parse-that/.cargo/config.toml` (if absent)

```toml
[build]
rustflags = ["-Zthreads=8", "-Zshare-generics=y"]

# macOS arm64: default to system ld64 via clang. lld is opt-in;
# developers who want it must `brew install lld` (separate from llvm)
# and uncomment the block below, adjusting the path from
# `brew --prefix lld` on their host.
[target.aarch64-apple-darwin]
rustflags = ["-Zthreads=8", "-Zshare-generics=y"]

# [target.aarch64-apple-darwin]
# linker    = "clang"
# rustflags = [
#     "-C", "link-arg=-fuse-ld=/opt/homebrew/opt/lld/bin/ld.lld",
#     "-Zthreads=8",
#     "-Zshare-generics=y",
# ]

[profile.ax-iter]
inherits      = "dev"
opt-level     = 0
debug         = "line-tables-only"
incremental   = true
codegen-units = 256

[alias]
iter-check = "check --profile ax-iter"
iter-test  = "nextest run --cargo-profile ax-iter"
```

### `../pprint/rust-toolchain.toml`

Same pin as bbnf-lang. pprint is lightweight (no proc-macros beyond
`pprint_derive`), so the pin is mostly for tooling parity.

### `../pprint/.cargo/config.toml`

Minimal — matches parse-that's structure, but without the `-Zshare-generics`
flag (pprint has one generic struct; the flag is a no-op for it).

## Workspace-member crates (inherit)

Every crate under `bbnf-lang/crates/*` inherits bbnf-lang's `.cargo/config.toml`
and `rust-toolchain.toml` automatically via cargo's config-file resolution
(walks up from manifest dir). No per-crate files required. Forbid any
`.cargo/config.toml` inside `crates/*/` — workspace-local config overrides
confuse the resolution order.

## Divan adoption (per-repo)

- **bbnf-lang**: §divan-migration.md — 19 bench binaries.
- **parse-that**: if `rust/parse_that/benches/*.rs` or `rust/regex/benches/*.rs`
  exist, mechanically port. Otherwise no change.
- **pprint**: bench targets are not assumed; inspect before migration.
- **csc411**: if the crate has benchmarks (for CSP solver hot paths), port.

## Nextest adoption (per-repo)

All Rust repos: `cargo install cargo-nextest --locked` documented in each
repo's README §Dev setup. Mirror bbnf-lang's `.config/nextest.toml.draft`
with repo-specific store paths and profile tuning.

## Policy rationale

Workspace-member crates that path-depend on bbnf-lang (via `crates/*`)
inherit because cargo resolves `.cargo/config.toml` at invocation-cwd and
walks up. Sibling repos (`../parse-that`, `../pprint`) have their own
invocation cwd; their `.cargo/config.toml` is independent.

The consequence: a developer running `cd ../parse-that && cargo check`
WITHOUT the sibling-repo files above would fall back to ambient nightly
(no pin) and default LLVM backend (no cranelift). The propagation ensures
consistent cross-repo toolchain behaviour.

## Ordering

1. bbnf-lang lands B1.W0 (rust-toolchain.toml + .cargo/config.toml + Makefile
   + nextest.toml).
2. parse-that lands its mirror commit (same pin + `.cargo/config.toml` if
   absent). Validate with `cargo check` in the sibling repo.
3. pprint lands its mirror commit.
4. Cross-repo CI validates that `cd bbnf-lang && cargo iter-check` succeeds
   with all three siblings pinned.

Total: 3 repos × ~30 min each = ~1.5 agent-hours for the cross-repo pass,
after bbnf-lang's B1.W0 lands.
