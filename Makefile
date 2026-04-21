.PHONY: all build build-lsp build-lsp-debug build-ext build-wasm dev test test-rust test-ts \
       install package publish bump-patch bump-minor bump-major release clean clean-vsix watch \
       deploy \
       iter-check iter-test-leaf iter-test-grammar iter-test-ws \
       expand-json expand-css expand-bbnf expand-sheets \
       asm-parse bench-compile bench-run profile-wave

# ─── Build ──────────────────────────────────────────────────────────────

## Build everything (release LSP + extension)
all: build

build: build-lsp build-ext

## Build the LSP binary (release mode) and copy to server/
build-lsp:
	cargo build --release -p bbnf-lsp
	mkdir -p server
	cp target/release/bbnf-lsp server/bbnf-lsp

## Build the LSP binary (debug mode) — faster iteration
build-lsp-debug:
	cargo build -p bbnf-lsp

## Build the VS Code extension bundle
build-ext:
	cd extension && npm run build

## Quick dev build: debug LSP + extension (fast iteration)
dev: build-lsp-debug build-ext
	mkdir -p server
	cp target/debug/bbnf-lsp server/bbnf-lsp

## Build the WASM module (bbnf-wasm → playground/src/wasm/)
build-wasm:
	cd wasm && wasm-pack build --target web --out-dir ../playground/src/wasm

# ─── Test ───────────────────────────────────────────────────────────────
#
# Tranche Y.-1.c — freezing guards.
#
# Prefer cargo-nextest (`.config/nextest.toml` configures slow-timeout
# / terminate-after / leak-timeout). Install with:
#   cargo install cargo-nextest --locked
# If nextest is absent, fall back to `cargo test` under GNU `timeout`
# (macOS: `gtimeout` via coreutils). As a last resort run plain
# `cargo test` with a warning — at least CI should have one of
# nextest or timeout installed.

HAS_NEXTEST := $(shell command -v cargo-nextest 2>/dev/null)
HAS_TIMEOUT := $(shell command -v timeout 2>/dev/null)
HAS_GTIMEOUT := $(shell command -v gtimeout 2>/dev/null)

# Per-test-binary wall clock, in seconds. 300s = 5 minutes is the
# outer cap on an entire cargo test invocation; if your test run
# legitimately needs more than that, split the target.
TEST_TIMEOUT_SECS ?= 300

ifdef HAS_NEXTEST
  TEST_RUNNER := cargo nextest run --workspace
  TEST_RUNNER_CI := cargo nextest run --workspace --profile ci
else ifdef HAS_TIMEOUT
  TEST_RUNNER := timeout $(TEST_TIMEOUT_SECS) cargo test --workspace
  TEST_RUNNER_CI := timeout $(TEST_TIMEOUT_SECS) cargo test --workspace
else ifdef HAS_GTIMEOUT
  TEST_RUNNER := gtimeout $(TEST_TIMEOUT_SECS) cargo test --workspace
  TEST_RUNNER_CI := gtimeout $(TEST_TIMEOUT_SECS) cargo test --workspace
else
  TEST_RUNNER := cargo test --workspace
  TEST_RUNNER_CI := cargo test --workspace
endif

## Run all tests (full workspace; heavy by design — for routine
## iteration call `iter-test-leaf` or `iter-test-grammar` instead,
## which route through the `ax-iter` profile and per-grammar split).
test: test-rust

## Rust workspace tests (bbnf + lsp)
test-rust:
ifndef HAS_NEXTEST
  ifndef HAS_TIMEOUT
    ifndef HAS_GTIMEOUT
	@echo "warning: no test timeout available (install cargo-nextest or GNU timeout / gtimeout)"
    endif
  endif
endif
	$(TEST_RUNNER)

## CI target — uses nextest's ci profile or falls back to the same
## timeout wrapper as test-rust
test-ci:
	$(TEST_RUNNER_CI)

## Run LSP benchmarks
bench:
	cargo test -p bbnf-lsp --test bench_lsp -- --nocapture

# ─── AY Iteration Surface ──────────────────────────────────────────────
#
# B0.W0 public fast-path commands for tranche AY.W5-W7 executors.
# Aliases live in `.cargo/config.toml`; these Makefile targets route
# through them or through `scripts/test-tier.sh` where shell plumbing
# is required (cargo aliases cannot pipe or redirect).
#
# Routine (ax-iter profile, fast): iter-check, iter-test-leaf,
#   iter-test-grammar, iter-test-ws.
# Structural (AY hard-gate evidence): expand-*, asm-parse, bench-compile.
# Heavy (explicitly not default): bench-run, profile-wave.
#
# Canonical catalog: `docs/benchmarks/post-B0-W0-commands.txt`.

## AY routine compile-gate — `cargo check --profile ax-iter --workspace`.
iter-check:
	cargo iter-check

## Leaf-crate test tier under ax-iter (tape, bbnf-ir, egraph,
## csp-solver, bbnf-ser). Fastest correctness surface — no
## derive-Parser sites. Routes through the `iter-test-leaf` cargo
## alias rather than `scripts/test-tier.sh leaf` because the script
## still passes `-p bbnf-tape` (stale name; B0.W1 owns that fix).
iter-test-leaf:
	cargo iter-test-leaf

## Per-grammar test tier under ax-iter. One derive-Parser site per
## test binary (tape_parity_*, *_parity, grammar_roundtrip, etc.).
iter-test-grammar:
	scripts/test-tier.sh grammar --profile ax-iter

## Full workspace test tier under ax-iter. Heavier than leaf/grammar
## but still the fast profile — heavy final-proof runs use `test-rust`.
iter-test-ws:
	scripts/test-tier.sh workspace --profile ax-iter

## `cargo expand` of the JSON monolithic bench → target/expand/json_monolithic.rs.
## Evidence for AY.W5 hard gate 1, AY.W7 hard gate 2.
expand-json:
	mkdir -p target/expand
	cargo expand-json > target/expand/json_monolithic.rs

## `cargo expand` of the CSS L4 bench → target/expand/css_l4.rs.
expand-css:
	mkdir -p target/expand
	cargo expand-css > target/expand/css_l4.rs

## `cargo expand` of the BBNF self-parse bench → target/expand/bbnf_monolithic.rs.
expand-bbnf:
	mkdir -p target/expand
	cargo expand-bbnf > target/expand/bbnf_monolithic.rs

## `cargo expand` of the Google Sheets bench → target/expand/google_sheets_monolithic.rs.
expand-sheets:
	mkdir -p target/expand
	cargo expand-sheets > target/expand/google_sheets_monolithic.rs

## `cargo asm` of a bench function. Evidence for AY.W5 hard gate 3
## (close-stamp verification). Usage:
##   make asm-parse BENCH=json_monolithic FN=json::close_compound
asm-parse:
	@if [ -z "$(BENCH)" ] || [ -z "$(FN)" ]; then \
		echo "usage: make asm-parse BENCH=<bench-name> FN=<symbol>" >&2; \
		exit 2; \
	fi
	mkdir -p target/asm
	cargo asm -p bbnf --profile release --bench $(BENCH) $(FN) > target/asm/$(BENCH)-$(subst ::,_,$(subst /,_,$(FN))).s

## Compile-gate a bench binary without running it. Usage:
##   make bench-compile BENCH=json_monolithic
## Heavy: uses the bench profile (fat LTO, codegen-units=1).
bench-compile:
	@if [ -z "$(BENCH)" ]; then \
		echo "usage: make bench-compile BENCH=<bench-name>" >&2; \
		exit 2; \
	fi
	cargo bench -p bbnf --bench $(BENCH) --no-run --profile bench

## Run a bench. HEAVY — not default. Usage:
##   make bench-run BENCH=json_monolithic
bench-run:
	@if [ -z "$(BENCH)" ]; then \
		echo "usage: make bench-run BENCH=<bench-name>" >&2; \
		exit 2; \
	fi
	cargo bench -p bbnf --bench $(BENCH)

## Samply profile-wave preparation. HEAVY — requires CARGO_TARGET_DIR
## exported. Invokes `scripts/prepare-profile-wave.sh` when present.
profile-wave:
	@if [ -z "$(CARGO_TARGET_DIR)" ]; then \
		echo "profile-wave requires CARGO_TARGET_DIR to be exported" >&2; \
		exit 2; \
	fi
	@if [ ! -x scripts/prepare-profile-wave.sh ]; then \
		echo "scripts/prepare-profile-wave.sh not present (B0.W1 lands the script)" >&2; \
		exit 2; \
	fi
	scripts/prepare-profile-wave.sh

# ─── Install / Package ─────────────────────────────────────────────────

## Install the extension locally into VS Code (builds first)
install: build
	cd extension && npx vsce package -o ../bbnf-lang.vsix
	code --install-extension bbnf-lang.vsix
	@echo ""
	@echo "Extension installed. Reload VS Code to activate."

## Package a .vsix without installing
package: build
	cd extension && npx vsce package -o ../bbnf-lang.vsix
	@echo ""
	@echo "Packaged: bbnf-lang.vsix"

# ─── Release ────────────────────────────────────────────────────────────

## Bump patch version (0.0.x), commit, and tag
bump-patch:
	cd extension && npm version patch --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"; \
	echo ""; \
	echo "Tagged v$$VERSION. Run 'make release' or 'git push --follow-tags' to publish."

## Bump minor version (0.x.0), commit, and tag
bump-minor:
	cd extension && npm version minor --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"; \
	echo ""; \
	echo "Tagged v$$VERSION. Run 'make release' or 'git push --follow-tags' to publish."

## Bump major version (x.0.0), commit, and tag
bump-major:
	cd extension && npm version major --no-git-tag-version
	@VERSION=$$(cd extension && node -p "require('./package.json').version"); \
	git add extension/package.json; \
	git commit -m "chore: bump version to v$$VERSION"; \
	git tag "v$$VERSION"; \
	echo ""; \
	echo "Tagged v$$VERSION. Run 'make release' or 'git push --follow-tags' to publish."

## Push the tag to trigger the GitHub Actions release pipeline
release:
	git push --follow-tags
	@echo ""
	@echo "Pushed. GitHub Actions will build platform binaries and publish to the VS Code Marketplace."
	@echo "Monitor: https://github.com/mkbabb/bbnf-lang/actions"

# ─── Clean ──────────────────────────────────────────────────────────────

clean:
	rm -f *.vsix
	rm -rf extension/dist
	cargo clean

## Remove old .vsix files
clean-vsix:
	rm -f *.vsix

## Deploy playground (rebuild WASM + Vite build + rsync)
deploy: build-wasm
	./scripts/deploy.sh

## Continuous rebuild on save (requires cargo-watch: cargo install cargo-watch)
watch:
	cargo watch -p bbnf-lsp -x build
