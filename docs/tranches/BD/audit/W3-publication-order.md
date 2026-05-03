# W3 — Publication Order Specification

Date: 2026-05-03
Scope: Sister-crate publication order, semver-checks integration, docs.rs preparation, npm publication for path-ts + runtime packages. Documents the cargo-release auto-computed dependency DAG, per-crate publication readiness, post-publication smoke verification.

## §1 Path-Dep Dependency DAG

Per `docs/tranches/BD/audit/research-anchors.md:§3`, cargo-release walks the workspace path-dep graph and computes the topological publication order. The BBNF sister-crate DAG:

```
                                  ┌─────────────────────────────────┐
                                  │  External dependencies          │
                                  │  (proc-macro2, syn, quote, ...)  │
                                  └─────────────────────────────────┘
                                            │
                                            ▼
                                  ┌──────────────────┐
                                  │  egraph-derive    │  (depth 0 — no internal deps)
                                  └──────────────────┘
                                            │
                                            ▼
                                  ┌──────────────────┐
                                  │  egraph           │  (depth 1 — depends on egraph-derive)
                                  └──────────────────┘

  ┌──────────────────┐
  │  csp-solver      │  (depth 0 — no internal deps; uses bbnf-lang in-tree as algorithm-evolution sibling)
  └──────────────────┘

  ┌──────────────────┐
  │  bbnf-regex      │  (depth 0 — no internal deps)
  └──────────────────┘

                                  ┌──────────────────┐
                                  │  path-core        │  (depth 0 — no internal deps)
                                  └──────────────────┘
                                            │
                                            ▼
                                  ┌──────────────────┐
                                  │  path             │  (depth 1 — depends on path-core)
                                  └──────────────────┘

  ┌──────────────────┐
  │  path-ts          │  (depth 1 — depends on path-core; npm publish, not crates.io)
  └──────────────────┘
```

## §2 Publication Order

cargo-release walks the DAG and produces this order:

| Step | Crate / Package | Registry | Command |
|---|---|---|---|
| 1 | egraph-derive | crates.io | `cargo release publish -p egraph-derive --execute` |
| 2 | egraph | crates.io | `cargo release publish -p egraph --execute` |
| 3 | csp-solver | crates.io | `cargo release publish -p csp-solver --execute` |
| 4 | bbnf-regex | crates.io | `cargo release publish -p bbnf-regex --execute` |
| 5 | path-core | crates.io | `cargo release publish -p path-core --execute` |
| 6 | path | crates.io | `cargo release publish -p path --execute` |
| 7 | path-ts (Rust binary) | npm | `cd crates/path-ts && napi prepublish && npm publish --access public` |
| 8 | @bbnf-lang/runtime | npm | `cd npm/runtime && napi prepublish && npm publish --access public` |
| 9 | @bbnf-lang/runtime-wasm | npm | `cd npm/runtime-wasm && npm publish --access public` |

Steps 1-6 are automated via `cargo release --workspace --execute`. Steps 7-9 are manual npm pushes (no cargo-release equivalent for npm yet).

## §3 cargo-release Workspace Configuration

`release.toml` (workspace root):

```toml
[workspace]
shared-version = false  # each sister crate has its own version
publish = false  # workspace default is no-publish; per-crate opt-in
push = false  # require manual git push after release tag
tag-name = "{{prefix}}{{version}}"  # per-crate prefix (egraph-v0.1.0)

# enable workspace-wide pre-release hooks
pre-release-hook = ["cargo", "test", "--all-features", "-p", "{{crate_name}}"]
pre-release-replacements = []

# ---

[package."egraph-derive"]
version = "0.1.0"
publish = true
tag-prefix = "egraph-derive-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "egraph-derive"]

[package."egraph"]
version = "0.1.0"
publish = true
tag-prefix = "egraph-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "egraph"]

[package."csp-solver"]
version = "0.1.0"
publish = true
tag-prefix = "csp-solver-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "csp-solver"]

[package."bbnf-regex"]
version = "0.1.0"
publish = true
tag-prefix = "bbnf-regex-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "bbnf-regex"]

[package."path-core"]
version = "0.1.0"
publish = true
tag-prefix = "path-core-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "path-core"]

[package."path"]
version = "0.1.0"
publish = true
tag-prefix = "path-v"
pre-release-hook = ["cargo", "semver-checks", "check-release", "-p", "path"]

# path-ts uses npm publish, not cargo
[package."path-ts"]
publish = false  # explicit no-publish on crates.io
```

## §4 cargo-semver-checks Validation

Per `docs/tranches/BD/audit/research-anchors.md:§3`, cargo-semver-checks detects the following semver violations:

- Removed pub items (functions, types, traits)
- Added `#[must_use]` on existing functions
- Function signature changes
- Trait def/impl modifications
- Visibility reductions

The pre-release hook in `release.toml` (§3) runs cargo-semver-checks per crate against the BC.W5 freeze baseline.

### Baseline rev tagging

The BC.W5 freeze baseline is captured as a git tag:

```bash
git tag bc-w5-freeze
git push origin bc-w5-freeze
```

cargo-semver-checks consumes this tag:

```bash
cargo semver-checks check-release \
  -p egraph \
  --baseline-rev bc-w5-freeze
```

### Per-crate validation report

The W3-G2 gate at BD.W3 produces `docs/tranches/BD/audit/W3-semver-checks-report.md`:

| Crate | Baseline | Candidate | Result |
|---|---|---|---|
| egraph-derive | bc-w5-freeze | bd-w3 | clean |
| egraph | bc-w5-freeze | bd-w3 | clean |
| csp-solver | bc-w5-freeze | bd-w3 | clean |
| bbnf-regex | bc-w5-freeze | bd-w3 | clean |
| path-core | bc-w5-freeze | bd-w3 | clean |
| path | bc-w5-freeze | bd-w3 | clean |

If any crate reports a break, the wave amends the freeze docs (with a major version bump) and re-runs validation.

## §5 docs.rs Metadata Enrichment

Per `docs/tranches/BD/audit/research-anchors.md:§3`, every crate's `Cargo.toml` requires:

| Field | Required? | Notes |
|---|---|---|
| `description` | yes | crates.io + docs.rs require |
| `license` | yes | "MIT OR Apache-2.0" workspace standard |
| `repository` | strong recommend | "https://github.com/mkbabb/bbnf-lang" |
| `homepage` | recommend | per-crate sub-path |
| `documentation` | recommend | "https://docs.rs/<crate>" |
| `keywords` | yes (≤ 5) | crates.io requires for discovery |
| `categories` | yes | crates.io categories list |
| `readme` | strong recommend | path to README.md |
| `[package.metadata.docs.rs] all-features` | yes | enables all-features doc build |
| `package.exclude` | recommend | excludes git directories, build artefacts |

Per-crate metadata table (excerpt):

### egraph

```toml
[package]
name = "egraph"
version = "0.1.0"
edition = "2021"
license = "MIT OR Apache-2.0"
description = "Generic e-graph data structure with cost-extraction and rewrite saturation"
repository = "https://github.com/mkbabb/bbnf-lang"
homepage = "https://github.com/mkbabb/bbnf-lang/tree/master/crates/egraph"
documentation = "https://docs.rs/egraph"
keywords = ["egg", "rewriting", "compiler", "optimization", "e-graph"]
categories = ["compilers", "data-structures"]
readme = "README.md"

[package.metadata.docs.rs]
all-features = true
rustdoc-args = ["--cfg", "docsrs"]
```

### egraph-derive

```toml
[package]
name = "egraph-derive"
version = "0.1.0"
description = "Procedural macro for deriving Language impls for the egraph crate"
keywords = ["egg", "rewriting", "derive", "procedural-macro"]
categories = ["development-tools::procedural-macro-helpers"]
# ... etc.
```

### csp-solver

```toml
[package]
name = "csp-solver"
version = "0.1.0"
description = "Generic constraint satisfaction problem solver with AC-3 + branch-and-bound"
keywords = ["csp", "constraint", "ac3", "satisfaction", "solver"]
categories = ["algorithms", "compilers"]
# ... etc.
```

### bbnf-regex

```toml
[package]
name = "bbnf-regex"
version = "0.1.0"
description = "Bespoke NFA→DFA regex engine for the BBNF parser fleet"
keywords = ["regex", "nfa", "dfa", "bbnf", "parser"]
categories = ["parser-implementations", "text-processing"]
# ... etc.
```

### path-core

```toml
[package]
name = "path-core"
version = "0.1.0"
description = "Path-AST and compile logic shared between the BBNF Rust + TS proc-macro shells"
keywords = ["bbnf", "path", "pointer", "json-pointer"]
categories = ["parser-implementations"]
# ... etc.
```

### path

```toml
[package]
name = "path"
version = "0.1.0"
description = "Rust proc-macro shell for typed path queries against BBNF grammars"
keywords = ["bbnf", "path", "macro", "proc-macro"]
categories = ["development-tools::procedural-macro-helpers"]
# ... etc.
```

## §6 npm Publication

### path-ts (NAPI native binary)

Per BD.W0 §2.7 + BD.W1 §2.6, the npm package layout for NAPI binaries uses per-platform sub-packages.

Steps:

1. CI matrix builds binaries for 6 platforms (BD.W1 §2.6)
2. CI uploads each binary as a GitHub Actions artefact
3. BD.W3 publication workflow downloads all 6 artefacts
4. `napi prepublish --skip-gh-release` packages each platform sub-package
5. `npm publish --access public` for each sub-package + main package

```bash
# in BD.W3 publication workflow
cd crates/path-ts
napi prepublish --skip-gh-release  # creates per-platform sub-package layouts in npm/
for pkg in npm/path-ts-darwin-arm64 npm/path-ts-darwin-x64 npm/path-ts-linux-x64-gnu \
           npm/path-ts-linux-arm64-gnu npm/path-ts-linux-x64-musl npm/path-ts-win32-x64-msvc; do
  cd "$pkg" && npm publish --access public && cd -
done
npm publish --access public  # main package last
```

### @bbnf-lang/runtime + @bbnf-lang/runtime-wasm

Same pattern for `@bbnf-lang/runtime` (NAPI per-platform) + `@bbnf-lang/runtime-wasm` (single package; WASM binary is platform-independent).

```bash
# @bbnf-lang/runtime
cd npm/runtime
napi prepublish --skip-gh-release
for pkg in npm/runtime-darwin-arm64 ... ; do
  cd "$pkg" && npm publish --access public && cd -
done
npm publish --access public

# @bbnf-lang/runtime-wasm (single package)
cd npm/runtime-wasm
npm publish --access public
```

## §7 Post-Publication Smoke Verification

Per BD.W3 §2.9, post-publication smoke tests verify each artefact resolves end-to-end. The smoke tests run in a clean environment (not the dev workspace):

### crates.io smoke

```bash
mkdir -p /tmp/smoke-egraph && cd /tmp/smoke-egraph
cargo init --bin
cargo add egraph
echo 'fn main() { use egraph::EGraph; let _ = EGraph::<i32, ()>::default(); }' > src/main.rs
cargo build && cargo run
```

Expected: cargo build succeeds; runtime exits with code 0.

Repeat for each published crate (egraph, egraph-derive, csp-solver, bbnf-regex, path-core, path).

### npm smoke

```bash
mkdir -p /tmp/smoke-runtime && cd /tmp/smoke-runtime
npm init -y
npm install @bbnf-lang/runtime
cat > index.mjs << 'EOF'
import { parseJson } from '@bbnf-lang/runtime/parsers/json';
const out = parseJson(Buffer.from('{"key": "value"}'));
console.log(JSON.stringify(out));
EOF
node index.mjs
```

Expected: npm install resolves; node prints parsed object.

Repeat for `@bbnf-lang/runtime-wasm`, `@bbnf-lang/path-ts`.

## §8 Failure Modes + Mitigations

| Failure | Mitigation |
|---|---|
| crates.io publication fails: name squatted | Alternative names checked at BC.W5; record published name in audit doc |
| npm publication fails: scope squatted | Alternative scopes checked at BD.W0; record published scope in audit doc |
| cargo-semver-checks reports unexpected break | Amend BC.W5 freeze docs with major version bump; re-run validation |
| Per-platform binary missing from CI cache | Re-run BD.W1 §2.6 CI matrix; cache rebuild |
| Smoke test fails post-publication | Root-cause: registry caching; wait + retry; if persistent, root-cause via `cargo install --debug` |
| docs.rs build fails (all-features compile error) | Pre-check at BD.W3 §2.3; `cargo doc -p <crate> --all-features` locally before publication |

## §9 parse-that Disposition

Per BC.W5c gap-I and synthesis surgery 33, `parse-that`'s disposition is decided in-plan. Default per surgery 33: option (i) permanent path-dep. BD.W3 records the decision at `docs/tranches/BD/audit/W3-parse-that-disposition.md`.

**parse-that is NOT published at BD.W3** (option (i) default). The crate remains a workspace path-dep; its `Cargo.toml` has `publish = false`. If BC.W5c chose option (ii) or (iii), the order at §2 expands; the audit doc records the actual choice.

## §10 Closing Posture

The publication order is auto-computed by cargo-release from the path-dep DAG. Sister crates publish in topological order (egraph-derive → egraph; csp-solver, bbnf-regex, path-core at depth 0; path at depth 1). cargo-semver-checks validates each against the BC.W5 freeze baseline. docs.rs metadata is enriched per-crate. npm packages publish via @napi-rs/cli. Post-publication smoke tests verify each artefact resolves end-to-end. parse-that's disposition is permanent path-dep (default per surgery 33).
