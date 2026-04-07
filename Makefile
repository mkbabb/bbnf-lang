.PHONY: all build build-lsp build-lsp-debug build-ext build-wasm dev test test-rust test-ts \
       install package publish bump-patch bump-minor bump-major release clean clean-vsix watch \
       deploy

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

## Run all tests
test: test-rust

## Rust workspace tests (bbnf + lsp)
test-rust:
	cargo test --workspace

## Run LSP benchmarks
bench:
	cargo test -p bbnf-lsp --test bench_lsp -- --nocapture

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
