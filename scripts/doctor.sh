#!/usr/bin/env bash
# Probe host readiness for the bbnf-lang dev loop.
#
# Each probe runs `command -v <tool>` and prints either
#   [ok] <tool>
# or
#   [missing] <tool>: install via <hint>
#
# Exits 0 when every probe passes; exits 1 when any probe fails (with a
# trailing summary listing the missing tools). Total wall is sub-second on
# a fully-equipped host because every check is a single shell builtin.
#
# Tools probed:
#   sccache         rustc compile cache (.cargo/config.toml:42 mandates it)
#   cargo-nextest   workspace test runner (Makefile + iter-test surfaces)
#   samply          profiler (scripts/profile-bench-headless.sh consumer)
#   lld             fast linker (.cargo/config.toml lld block, opt-in on macOS)
#   cargo-expand    macro expansion (Makefile expand-* surfaces)
#
# Usage: scripts/doctor.sh   (also reachable via `make doctor`)

set -u

# ─── Probe table ─────────────────────────────────────────────────────────────
# Each entry is "<tool>|<install-hint>". The hint is the literal command the
# operator runs to install the missing tool; macOS is the dispatch host so
# `brew` is the default for non-cargo binaries.
PROBES=(
    "sccache|cargo install sccache --locked"
    "cargo-nextest|cargo install cargo-nextest --locked"
    "samply|cargo install samply --locked"
    "lld|brew install lld   (linux: distro package, e.g. apt install lld)"
    "cargo-expand|cargo install cargo-expand --locked"
)

missing=()

for entry in "${PROBES[@]}"; do
    tool="${entry%%|*}"
    hint="${entry#*|}"
    if command -v "$tool" >/dev/null 2>&1; then
        printf '[ok] %s\n' "$tool"
    else
        printf '[missing] %s: install via %s\n' "$tool" "$hint"
        missing+=("$tool")
    fi
done

if [ "${#missing[@]}" -eq 0 ]; then
    echo "doctor: green"
    exit 0
fi

echo "doctor: missing ${#missing[@]} tool(s): ${missing[*]}" >&2
exit 1
