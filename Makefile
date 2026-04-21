.PHONY: all build build-lsp build-lsp-debug build-ext build-wasm dev test test-rust test-ts \
       install package publish bump-patch bump-minor bump-major release clean clean-vsix watch \
       deploy \
       iter-check iter-test-leaf iter-test-grammar iter-test-ws \
       expand-json expand-css expand-bbnf expand-sheets \
       asm-parse bench-compile bench-run profile-wave \
       ay-expand-json ay-expand-named-type ay-asm-close-compound \
       ay-test-value-api ay-test-wire-contract ay-test-named-type \
       ay-samply-json-twitter ay-samply-json-twitter-lookup \
       ay-bench-close ay-prepare-profile-wave

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

# ─── AY W5-W7 Gate Commands ────────────────────────────────────────────
#
# Targets here implement the EXACT public commands cited by AY.W5-W7
# hard gates. See docs/instructions/PROFILING.md §AY W5-W7 gate commands
# for the gate→target→artefact manifest. AY executors call `make ay-*`
# without re-deriving the cargo invocation from the wave spec.
#
# Categories:
#   Expand gates   — ay-expand-json (W5.1, W7.2), ay-expand-named-type (W6.2).
#   Asm gate       — ay-asm-close-compound (W5.3).
#   Test gates     — ay-test-value-api (W5.1), ay-test-wire-contract (W7.1),
#                    ay-test-named-type (W6.1).
#   Samply gates   — ay-samply-json-twitter (W5.2), ay-samply-json-twitter-lookup (W6.3).
#   Bench gate     — ay-bench-close (W5.5 / W6.4 / W7.4).
#   Profile prep   — ay-prepare-profile-wave (shared prerequisite).
#
# Artefact convention: expand → target/expand/ay-*.rs, asm → target/asm/ay-*.s,
# samply → .profiles/samply/AY-<WAVE>/<scenario>/, bench → docs/benchmarks/post-AY-<WAVE>-mid.json.

## AY.W5.1 + AY.W7.2 hard gate — expand of JSON monolithic bench.
## Writes target/expand/ay-json.rs; reports line count on stdout.
ay-expand-json:
	@mkdir -p target/expand
	cargo expand -p bbnf --bench json_monolithic > target/expand/ay-json.rs
	@wc -l target/expand/ay-json.rs

## AY.W6.2 hard gate — expand of named-type preservation test.
## Writes target/expand/ay-named-type.rs; reports line count on stdout.
ay-expand-named-type:
	@mkdir -p target/expand
	cargo expand -p bbnf --test named_type_preservation > target/expand/ay-named-type.rs
	@wc -l target/expand/ay-named-type.rs

## AY.W5.3 hard gate — close-stamp asm inspection. FN defaults to the
## AY.W5.1 target (tape::builder::close_compound); override per gate:
##   make ay-asm-close-compound FN=<module>::<fn>
## cargo asm exiting non-zero on an unresolved symbol is acceptable
## evidence for AY executors (the stderr names the real candidate).
FN ?= tape::builder::close_compound
ay-asm-close-compound:
	@mkdir -p target/asm
	cargo asm -p bbnf $(FN) > target/asm/ay-close-$(subst ::,-,$(FN)).s

## AY.W5.1 hard gate — value API apples-to-apples under ax-iter.
ay-test-value-api:
	cargo test -p bbnf --test value_api_apples_to_apples --profile ax-iter

## AY.W7.1 hard gate — shared-plan wire-contract assertions under ax-iter.
## The test binary is authored in AY.W7.3; pre-AY this target surfaces
## the missing-target error cleanly so executors see the AY-pre-W7 state.
ay-test-wire-contract:
	cargo test -p bbnf --test gate_predicate_wire_contract --profile ax-iter

## AY.W6.1 hard gate — named-type preservation under ax-iter.
ay-test-named-type:
	cargo test -p bbnf --test named_type_preservation --profile ax-iter

## AY.W5.2 hard gate — samply on eager JSON twitter. Requires
## CARGO_TARGET_DIR exported and a prebuilt bench binary under
## $(CARGO_TARGET_DIR)/profiling-prep/deps/json_monolithic-*. WAVE
## defaults to W5; override per gate:
##   make ay-samply-json-twitter WAVE=W5
## Artefact dir: .profiles/samply/AY-<WAVE>/json_twitter_eager/.
## Ports 3130/3131 match the wave.tsv contract in PROFILING.md.
WAVE ?= W5
ay-samply-json-twitter:
	@if [ -z "$(CARGO_TARGET_DIR)" ]; then \
		echo "ay-samply-json-twitter requires CARGO_TARGET_DIR to be exported" >&2; \
		exit 2; \
	fi
	@if [ ! -x scripts/profile-bench-headless.sh ]; then \
		echo "scripts/profile-bench-headless.sh not present (B0.W1.b lands the script)" >&2; \
		exit 2; \
	fi
	@BIN="$$(ls $(CARGO_TARGET_DIR)/profiling-prep/deps/json_monolithic-* 2>/dev/null | grep -v '\.d$$' | head -1)"; \
	if [ -z "$$BIN" ]; then \
		echo "no prebuilt json_monolithic binary under $(CARGO_TARGET_DIR)/profiling-prep/deps/ — run 'make ay-prepare-profile-wave' first" >&2; \
		exit 2; \
	fi; \
	scripts/profile-bench-headless.sh \
		--bench json_monolithic \
		--entry twitter \
		--record-port 3130 \
		--load-port 3131 \
		--artifact-dir .profiles/samply/AY-$(WAVE)/json_twitter_eager \
		--bench-cwd "$(CURDIR)/crates/core" \
		--bin "$$BIN"

## AY.W6.3 hard gate — samply on eager JSON twitter path lookup. Same
## prebuilt binary as ay-samply-json-twitter, different artefact dir and
## port pair. WAVE defaults to W6; override per gate:
##   make ay-samply-json-twitter-lookup WAVE=W6
## Artefact dir: .profiles/samply/AY-<WAVE>/json_twitter_lookup/.
## Ports 3132/3133 are reserved for this gate.
ay-samply-json-twitter-lookup:
	@if [ -z "$(CARGO_TARGET_DIR)" ]; then \
		echo "ay-samply-json-twitter-lookup requires CARGO_TARGET_DIR to be exported" >&2; \
		exit 2; \
	fi
	@if [ ! -x scripts/profile-bench-headless.sh ]; then \
		echo "scripts/profile-bench-headless.sh not present (B0.W1.b lands the script)" >&2; \
		exit 2; \
	fi
	@BIN="$$(ls $(CARGO_TARGET_DIR)/profiling-prep/deps/json_monolithic-* 2>/dev/null | grep -v '\.d$$' | head -1)"; \
	if [ -z "$$BIN" ]; then \
		echo "no prebuilt json_monolithic binary under $(CARGO_TARGET_DIR)/profiling-prep/deps/ — run 'make ay-prepare-profile-wave' first" >&2; \
		exit 2; \
	fi; \
	scripts/profile-bench-headless.sh \
		--bench json_monolithic \
		--entry twitter \
		--record-port 3132 \
		--load-port 3133 \
		--artifact-dir .profiles/samply/AY-$(WAVE)/json_twitter_lookup \
		--bench-cwd "$(CURDIR)/crates/core" \
		--bin "$$BIN"

## AY.W5.5 / AY.W6.4 / AY.W7.4 close-matrix bench. Runs the 19-entry
## parse-bench sweep sequentially — never parallel, never backgrounded —
## across the five monolithic benches (json, css, sheets, bbnf, compile)
## and writes each bench's log under docs/benchmarks/post-AY-<WAVE>-*.
## The aggregate JSON is composed post-run from the per-bench logs.
##   make ay-bench-close WAVE=W5           # profiling-prep profile (mid)
##   make ay-bench-close WAVE=close        # bench profile (fat LTO, publish)
## WAVE=close selects --profile bench for publish-grade numbers; any
## other value selects --profile profiling-prep for mid-wave checks.
BENCH_PROFILE = $(if $(filter close,$(WAVE)),bench,profiling-prep)
ay-bench-close:
	@mkdir -p docs/benchmarks
	@echo "AY bench-close WAVE=$(WAVE) profile=$(BENCH_PROFILE)" >&2
	find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null || true
	cargo bench --profile $(BENCH_PROFILE) -p bbnf --bench json_monolithic \
		> docs/benchmarks/post-AY-$(WAVE)-json.txt 2>&1
	cargo bench --profile $(BENCH_PROFILE) -p bbnf --bench css_l4 \
		> docs/benchmarks/post-AY-$(WAVE)-css.txt 2>&1
	cargo bench --profile $(BENCH_PROFILE) -p bbnf --bench google_sheets_monolithic \
		> docs/benchmarks/post-AY-$(WAVE)-sheets.txt 2>&1
	cargo bench --profile $(BENCH_PROFILE) -p bbnf --bench bbnf_monolithic \
		> docs/benchmarks/post-AY-$(WAVE)-bbnf.txt 2>&1
	cargo bench --profile $(BENCH_PROFILE) -p bbnf --bench compile_pipeline \
		> docs/benchmarks/post-AY-$(WAVE)-compile.txt 2>&1
	@echo "per-bench logs written under docs/benchmarks/post-AY-$(WAVE)-*.txt" >&2
	@echo "aggregate docs/benchmarks/post-AY-$(WAVE)-mid.json is composed post-run" >&2

## Shared prerequisite for ay-samply-*. Wraps
## scripts/prepare-profile-wave.sh (idempotent per B0.W1.b). Requires
## CARGO_TARGET_DIR exported. Emits the prebuilt-binary artefact-dir
## hint downstream samply targets consume.
ay-prepare-profile-wave:
	@if [ -z "$(CARGO_TARGET_DIR)" ]; then \
		echo "ay-prepare-profile-wave requires CARGO_TARGET_DIR to be exported" >&2; \
		exit 2; \
	fi
	@if [ ! -x scripts/prepare-profile-wave.sh ]; then \
		echo "scripts/prepare-profile-wave.sh not present (B0.W1.b lands the script)" >&2; \
		exit 2; \
	fi
	scripts/prepare-profile-wave.sh
	@echo "prebuilt bench binaries under $(CARGO_TARGET_DIR)/profiling-prep/deps/" >&2

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
