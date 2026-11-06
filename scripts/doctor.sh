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
#   xcrun           exact Xcode clang/ld/ar resolver
#   cargo-expand    macro expansion (Makefile expand-* surfaces)
#
# Usage: scripts/doctor.sh   (also reachable via `make doctor`)

set -u

if [[ "$(/usr/bin/uname -s)" != "Darwin" || "$(/usr/bin/uname -m)" != "arm64" ]]; then
    printf '[unsupported] native host must be macOS arm64; got %s %s\n' \
        "$(/usr/bin/uname -s)" "$(/usr/bin/uname -m)" >&2
    exit 1
fi

# ─── Probe table ─────────────────────────────────────────────────────────────
# Each entry is "<tool>|<install-hint>". The hint is the literal command the
# operator runs to install the missing tool; macOS is the dispatch host so
# `brew` is the default for non-cargo binaries.
PROBES=(
    "sccache|cargo install sccache --locked"
    "cargo-nextest|cargo install cargo-nextest --locked"
    "samply|cargo install samply --locked"
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

if [ ! -x /usr/bin/arch ]; then
    printf '[missing] /usr/bin/arch: required canonical Apple ARM launcher\n'
    missing+=("arch")
fi

if [ ! -x /usr/bin/xcrun ]; then
    printf '[missing] /usr/bin/xcrun: install via xcode-select --install\n'
    missing+=("xcrun")
fi

if [ -x /usr/bin/arch ] && [ -x /usr/bin/xcrun ]; then
    for tool in clang ld ar; do
        resolved="$(/usr/bin/arch -arm64 /usr/bin/xcrun --find "$tool" 2>/dev/null)"
        resolver_status=$?
        if [ "$resolver_status" -ne 0 ]; then
            printf '[missing] xcrun %s: resolver exited %s\n' "$tool" "$resolver_status" >&2
            exit "$resolver_status"
        elif [ -n "$resolved" ] && [ -x "$resolved" ]; then
            printf '[ok] xcrun %s: %s\n' "$tool" "$resolved"
        else
            printf '[missing] xcrun %s: install via xcode-select --install\n' "$tool"
            missing+=("xcrun-$tool")
        fi
    done
fi

if [ "${#missing[@]}" -eq 0 ]; then
    echo "doctor: green"
    exit 0
fi

echo "doctor: missing ${#missing[@]} tool(s): ${missing[*]}" >&2
exit 1
