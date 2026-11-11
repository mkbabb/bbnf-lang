#!/usr/bin/env bash
# LW2.TOPO-7 falsifier (A2): a credential-free clean resolve of one git+rev
# edge on bbnf-lang. Fresh CARGO_HOME, no global/system git config, no
# credential helper, no ssh (GIT_SSH_COMMAND=false, no agent), no prompt.
# usage: resolve.sh <bbnf-lang rev>
set -u
rev=$1; w=$(mktemp -d "${TMPDIR:-/tmp}/topo7-resolve.XXXXXX"); trap 'rm -rf "$w"' EXIT
mkdir -p "$w/home" "$w/crate/src"; : > "$w/crate/src/lib.rs"
cat > "$w/crate/Cargo.toml" <<TOML
[package]
name = "topo7-resolve"
version = "0.0.0"
edition = "2021"
publish = false
[dependencies]
bbnf-egraph = { package = "egraph", git = "https://github.com/mkbabb/bbnf-lang", rev = "$rev" }
TOML
cd "$w/crate"
env -i PATH="$PATH" HOME="$w/home" CARGO_HOME="$w/home/.cargo" RUSTUP_HOME="${RUSTUP_HOME:-$HOME/.rustup}" \
  GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND=false \
  GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=credential.helper GIT_CONFIG_VALUE_0= \
  CARGO_NET_GIT_FETCH_WITH_CLI=true \
  cargo +stable metadata --format-version 1 > "$w/meta.json" 2> "$w/err.txt"
rc=$?
grep -E "error|failed|submodule" "$w/err.txt" | grep -v "spurious network" | head -6
[ $rc -eq 0 ] && echo "packages: $(python3 -c 'import json,sys;print(len(json.load(open(sys.argv[1]))["packages"]))' "$w/meta.json")"
echo "rev=$rev rc=$rc"; exit $rc
