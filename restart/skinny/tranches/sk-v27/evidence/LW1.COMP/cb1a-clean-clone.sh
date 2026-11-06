#!/bin/sh
# LW1.COMP-4 (CB-1a) falsifier: a clean, credential-free clone of bbnf-lang at
# <rev> passes scripts/check-hermetic-build.sh. The check script is taken from
# <check-script> so the same check can be read against a commit that predates it.
#
# Clean: a fresh (blobless) clone over https into an empty directory (no submodules, no
# sibling checkouts beside it) and a fresh CARGO_HOME (no cached git sources).
# Credential-free: no global or system git config, no ssh agent, ssh disabled,
# no terminal prompt; cargo fetches git sources through the git CLI under that
# environment. The toolchain still comes from this host's rustup.
#
# Usage: cb1a-clean-clone.sh <rev> <check-script> <scratch-dir>
set -u
rev=$1; check=$2; scratch=$3
rm -rf "$scratch"; mkdir -p "$scratch"
cd "$scratch" || exit 2
env -u SSH_AUTH_SOCK GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
    GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND=false \
    git clone --quiet --no-checkout --filter=blob:none https://github.com/mkbabb/bbnf-lang.git bbnf-lang || exit 3
env -u SSH_AUTH_SOCK GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND=false git -C bbnf-lang -c advice.detachedHead=false checkout --quiet "$rev" || exit 3
echo "clone: $(git -C bbnf-lang rev-parse HEAD); siblings: $(ls "$scratch" | tr '\n' ' ')"
cd bbnf-lang || exit 2
exec env -u SSH_AUTH_SOCK GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
    GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND=false \
    CARGO_HOME="$scratch/cargo-home" CARGO_NET_GIT_FETCH_WITH_CLI=true \
    RUSTUP_HOME="${RUSTUP_HOME:-$HOME/.rustup}" \
    PATH="$HOME/.cargo/bin:$PATH" \
    sh "$check"
