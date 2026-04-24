# ─────────────────────────────────────────────────────────────────────────────
# Makefile — bbnf-lang (B1.W0 successor)
#
# Redesign invariants:
#   - High-level convenience targets only. Iteration surfaces (check / test /
#     clippy per bounded working-set) live as cargo aliases in .cargo/config.toml
#     (feedback: single-cargo-per-target; one canonical surface per operation).
#   - Every heavy target delegates to a cargo alias or a single shell script.
#   - No nextest-detection / GNU-timeout fallback ladder — nextest is required
#     (enforced via the pinned toolchain install in PROFILING.md §Dev-host setup).
#   - No wave-specific targets. Wave orchestration lives in plan docs, not Make.
#
# Delta:
#   - BEFORE: 470 lines (master Makefile + scripts/* shims)
#   - AFTER: ~150 lines (this file). All iter-* targets moved to cargo aliases;
#     test runner detection removed; wave-specific targets deleted.
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: all build build-lsp build-lsp-debug build-ext build-wasm dev \
        test test-ci test-close \
        bench bench-json bench-css bench-bbnf bench-sheets bench-compile \
        profile profile-json profile-css \
        expand expand-bootstrap expand-derive asm \
        install package \
        bump-patch bump-minor bump-major release \
        clean clean-vsix clean-incr clean-cache ay-prime watch deploy

# ─── Default ─────────────────────────────────────────────────────────────────
all: build

# ─── Build ───────────────────────────────────────────────────────────────────

## Build LSP (release) + VS Code extension bundle
build: build-lsp build-ext

## Build LSP binary (release) and copy to server/
build-lsp:
	cargo build --profile ay-final -p bbnf-lsp
	mkdir -p server
	cp target/ay-final/bbnf-lsp server/bbnf-lsp

## Build LSP binary (ax-iter profile) — fast iteration
build-lsp-debug:
	cargo build --profile ax-iter -p bbnf-lsp

## Quick dev build: debug LSP + extension
dev: build-lsp-debug build-ext
	mkdir -p server
	cp target/ax-iter/bbnf-lsp server/bbnf-lsp

## Build VS Code extension bundle
build-ext:
	cd extension && npm run build

## Build WASM module into playground/src/wasm/
build-wasm:
	cd wasm && wasm-pack build --target web --out-dir ../playground/src/wasm

# ─── Test ────────────────────────────────────────────────────────────────────
#
# nextest is REQUIRED. Install via `cargo install cargo-nextest --locked`.
# The `cargo iter-test*` aliases in .cargo/config.toml cover the dev-loop
# surfaces; the Makefile targets below cover full-workspace and CI variants.

## Full workspace test (default profile — local interactive)
test:
	cargo nextest run --workspace

## CI profile — retries, junit, fail-fast=false
test-ci:
	cargo nextest run --workspace --profile ci

## Close-ceremony run — full suite with slow-timeout=120s; emits per-test timing
test-close:
	cargo nextest run --workspace --profile close

# ─── Bench ───────────────────────────────────────────────────────────────────
#
# Divan harness. Each target is ONE cargo invocation (feedback: bench-single-run).
# Output captured by callers via redirection; this Makefile does not shell-
# redirect (feedback: clean-instrumentation — no `eprintln` / tee inside Make).

bench:
	cargo bench-all

bench-json:
	cargo bench-json

bench-css:
	cargo bench-css

bench-bbnf:
	cargo bench-bbnf

bench-sheets:
	cargo bench-sheets

bench-compile:
	cargo bench-compile

# ─── Profile (samply) ────────────────────────────────────────────────────────
#
# `scripts/profile.sh` was ABROGATED in B1.W2.b per meta-audit/08 catalog.
# Successor: `scripts/prepare-profile-wave.sh` (compiles benches once + writes
# wave.tsv contract) + `scripts/profile-bench-headless.sh` (per-entry samply
# record + symbol-resolved profile). The canonical profiling workflow lives
# in docs/instructions/PROFILING.md.

profile:
	./scripts/prepare-profile-wave.sh

profile-json:
	./scripts/profile-bench-headless.sh --bench json_monolithic

profile-css:
	./scripts/profile-bench-headless.sh --bench css_l4

# ─── Expand / inspect ────────────────────────────────────────────────────────
#
# Cargo aliases `expand-bootstrap` / `expand-derive` / `asm-bbnf` carry the
# right profile flags; Makefile delegates.

expand: expand-bootstrap

expand-bootstrap:
	cargo expand-bootstrap > target/expanded-bootstrap.rs
	@echo "Expanded to target/expanded-bootstrap.rs"

expand-derive:
	cargo expand-derive > target/expanded-derive.rs

asm:
	cargo asm-bbnf

# ─── Install / Package ───────────────────────────────────────────────────────

install: build
	cd extension && npx vsce package -o ../bbnf-lang.vsix
	code --install-extension bbnf-lang.vsix
	@echo "Extension installed. Reload VS Code to activate."

package: build
	cd extension && npx vsce package -o ../bbnf-lang.vsix
	@echo "Packaged: bbnf-lang.vsix"

# ─── Release ─────────────────────────────────────────────────────────────────

bump-patch:
	cd extension && npm version patch --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"

bump-minor:
	cd extension && npm version minor --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"

bump-major:
	cd extension && npm version major --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"

release:
	git push --follow-tags

# ─── Clean ───────────────────────────────────────────────────────────────────

clean:
	rm -f *.vsix
	rm -rf extension/dist
	cargo clean

clean-vsix:
	rm -f *.vsix

## ICE recovery: nuke incremental cache; preserve proc-macro .bbnf-cache.
## Invoked when an rustc-ice-*.txt appears at repo root. Documented in
## docs/instructions/PROFILING.md §ICE recovery.
clean-incr:
	rm -rf target/*/incremental
	@echo "Incremental cache cleared. Proc-macro .bbnf-cache preserved."

## Nuke proc-macro cache (last resort; only if content-keyed cache desyncs).
## Under the B1 derive-cache design (patches/derive-cache-design.md) this
## targets $XDG_CACHE_HOME/bbnf-derive/ rather than target/.bbnf-cache/.
clean-cache:
	rm -rf target/.bbnf-cache "$${XDG_CACHE_HOME:-$$HOME/.cache}/bbnf-derive"

# ─── Prime / cache setup ─────────────────────────────────────────────────────
#
# Seed the bbnf-derive proc-macro cache via a single cold run of the two
# derive-Parser-heavy crates. B1.W0.d sub-gate requires this target; cache
# entries land under target/.bbnf-cache/ today (AZ-I.W0 lifts the location
# to $XDG_CACHE_HOME/bbnf-derive/). Reports the cache-entry count on exit.
ay-prime:
	cargo check --profile ax-iter -p bbnf-bootstrap --lib
	cargo check --profile ax-iter -p gorgeous --lib
	@count=$$(find target -name .bbnf-cache -type d 2>/dev/null | xargs -I{} find {} -type f 2>/dev/null | wc -l | tr -d ' '); \
	echo "ay-prime complete; .bbnf-cache entries: $$count"

# ─── Deploy / Watch ──────────────────────────────────────────────────────────

deploy: build-wasm
	./scripts/deploy.sh

watch:
	cargo watch -p bbnf-lsp -x "check --profile ax-iter"
