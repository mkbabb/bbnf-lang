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
        regen regen-check \
        ay-expand-json ay-expand-named-type ay-asm-close-compound \
        ay-test-value-api ay-test-wire-contract ay-test-named-type \
        ay-samply-json-twitter ay-samply-json-twitter-lookup \
        ay-bench-close ay-prepare-profile-wave \
        install package \
        bump-patch bump-minor bump-major release \
        clean clean-vsix clean-incr watch deploy

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

## ICE recovery: nuke incremental cache.
## Invoked when an rustc-ice-*.txt appears at repo root. Documented in
## docs/instructions/PROFILING.md §ICE recovery.
clean-incr:
	rm -rf target/*/incremental
	@echo "Incremental cache cleared."

# ─── Regen ───────────────────────────────────────────────────────────────────
#
# `cargo xtask regen` is the canonical regen entrypoint post-B2 — runs the
# 17-pass IR pipeline + emission once per invocation, writing per-grammar
# source to crates/core/src/grammar/generated/<ident>.rs. The pre-B2
# `scripts/bootstrap-bbnf.sh` (cargo-expand wrapper + Python post-process)
# retired with `crates/derive/`; the wall fell from 80+ min cold to seconds.

## Regenerate every grammar enumerated in [workspace.metadata.bbnf.grammars].
regen:
	cargo xtask regen

## CI / pre-commit gate: regenerate to a tempdir + diff against the
## checked-in tree; exit non-zero on drift. Replaces the pre-B2
## `scripts/check-bootstrap-clean.sh`.
regen-check:
	cargo xtask regen --check

# ─── Deploy / Watch ──────────────────────────────────────────────────────────

deploy: build-wasm
	./scripts/deploy.sh

watch:
	cargo watch -p bbnf-lsp -x "check --profile ax-iter"
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
# Fat-LTO `bench` profile triggers when WAVE is `close` or ends in `-close`
# (e.g. `W0p-close`, `W1-close`); any other value uses `profiling-prep` for
# faster mid-wave verification. AY-II's W0p.md ceremony uses `WAVE=W0p-mid`
# for the substrate-verification pass; W1-W5 close gates use `WAVE=close`
# or `WAVE=<wave>-close` for publish-grade peer-parity numbers.
BENCH_PROFILE = $(if $(or $(filter close,$(WAVE)),$(filter %-close,$(WAVE))),bench,profiling-prep)
ay-bench-close:
	@mkdir -p docs/benchmarks
	@echo "AY bench-close WAVE=$(WAVE) profile=$(BENCH_PROFILE)" >&2
	@# Cold-per-parse is a divan-harness property (sample_size = 1,
	@# skip_ext_time = true), not a filesystem-wipe property.
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

