#!/usr/bin/env bash
set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
OUT_ROOT="$ROOT/.profiles/samply/prebuild"
TARGET_DIR="${CARGO_TARGET_DIR:-}"

# Canonical profile for prepared bench binaries (B0.W1.a).
# `profiling-prep` inherits from release and carries DWARF for samply
# while keeping build cost well below `bench`'s fat-LTO pipeline.
# `release/deps/` remains searched as a legacy fallback until older wave
# artefacts age out.
PROFILE="profiling-prep"
PROFILE_DIRS=(profiling-prep release bench)

BENCHES=(
  json_monolithic
  css_l4
  google_sheets_monolithic
  bbnf_monolithic
  json_value
)

# Maps each bench target name to its source path (relative to
# crates/core/). Bench paths are declared in crates/core/Cargo.toml;
# duplicating the mapping here keeps freshness checks O(1) without a
# TOML parser dependency. If a bench is added to Cargo.toml, add it here.
bench_source() {
  case "$1" in
    json_monolithic)          echo "benches/json/monolithic.rs" ;;
    css_l4)                   echo "benches/css/l4.rs" ;;
    google_sheets_monolithic) echo "benches/google_sheets/monolithic.rs" ;;
    bbnf_monolithic)          echo "benches/bbnf/monolithic.rs" ;;
    json_value)               echo "benches/json/value.rs" ;;
    *) return 1 ;;
  esac
}

bench_features() {
  case "$1" in
    json_value) echo "competitor" ;;
    *) echo "" ;;
  esac
}

if [[ -z "$TARGET_DIR" ]]; then
  echo "CARGO_TARGET_DIR must be set to one absolute shared path before prebuild." >&2
  exit 1
fi

case "$TARGET_DIR" in
  /*) ;;
  *)
    echo "CARGO_TARGET_DIR must be absolute: $TARGET_DIR" >&2
    exit 1
    ;;
esac

# Locate an existing bench binary under any known profile's deps dir.
# Prints the path on stdout; returns 0 when exactly one candidate found,
# 1 otherwise. Candidates must be executable regular files named
# `<bench>-*` with no `.d` or `.dSYM` suffix.
find_bench_binary() {
  local bench="$1"
  local bins=()
  local profile_dir

  for profile_dir in "${PROFILE_DIRS[@]}"; do
    local deps="$TARGET_DIR/$profile_dir/deps"
    [[ -d "$deps" ]] || continue
    while IFS= read -r bin; do
      bins+=("$bin")
    done < <(
      find "$deps" -maxdepth 1 -type f -perm -111 \
        -name "$bench-*" ! -name '*.d' ! -name '*.dSYM' 2>/dev/null | sort
    )
    if [[ "${#bins[@]}" -gt 0 ]]; then
      break
    fi
  done

  if [[ "${#bins[@]}" -eq 1 ]]; then
    printf '%s\n' "${bins[0]}"
    return 0
  fi
  return 1
}

# A prepared binary is fresh when it exists AND is newer than its bench
# source file. Any edit to `crates/core/benches/<bench>.rs` invalidates
# the cache; other Rust inputs are detected by cargo dependency tracking
# on the next `cargo bench --no-run` invocation.
bench_binary_fresh() {
  local bench="$1"
  local bin="$2"
  local src_rel
  src_rel="$(bench_source "$bench")" || return 1
  local src="$ROOT/crates/core/$src_rel"

  [[ -f "$bin" && -x "$bin" ]] || return 1
  [[ -f "$src" ]] || return 1
  # find -newer is true when $bin mtime > $src mtime.
  [[ -n "$(find "$bin" -newer "$src" 2>/dev/null)" ]] || return 1
  return 0
}

mkdir -p "$OUT_ROOT"

cd "$ROOT"
find . -name '.bbnf-cache' -type d -exec rm -rf {} + 2>/dev/null

: > "$OUT_ROOT/binaries.txt"
printf 'bench\tbinary\n' > "$OUT_ROOT/binaries.tsv"
printf 'root\t%s\ntarget_dir\t%s\nprofile\t%s\n' \
  "$ROOT" "$TARGET_DIR" "$PROFILE" > "$OUT_ROOT/meta.txt"

for bench in "${BENCHES[@]}"; do
  bin=""
  if cached_bin="$(find_bench_binary "$bench")" && bench_binary_fresh "$bench" "$cached_bin"; then
    bin="$cached_bin"
    printf 'reused: %s\n' "$bin"
  else
    features="$(bench_features "$bench")"
    if [[ -n "$features" ]]; then
      cargo bench -p bbnf --features "$features" --bench "$bench" --profile "$PROFILE" --no-run \
        > "$OUT_ROOT/$bench-build.txt" 2>&1
    else
      cargo bench -p bbnf --bench "$bench" --profile "$PROFILE" --no-run \
        > "$OUT_ROOT/$bench-build.txt" 2>&1
    fi
    if ! bin="$(find_bench_binary "$bench")"; then
      {
        printf 'Expected exactly one binary for %s under %s/{%s}/deps\n' \
          "$bench" "$TARGET_DIR" "$(IFS=,; echo "${PROFILE_DIRS[*]}")"
      } > "$OUT_ROOT/$bench-binary-error.txt"
      echo "Could not resolve a unique binary for $bench; see $OUT_ROOT/$bench-binary-error.txt" >&2
      exit 1
    fi
    printf 'rebuilt: %s\n' "$bin"
  fi

  printf '%s\n' "$bin" >> "$OUT_ROOT/binaries.txt"
  printf '%s\t%s\n' "$bench" "$bin" >> "$OUT_ROOT/binaries.tsv"
done

printf 'Prebuild artifacts: %s\n' "$OUT_ROOT"
sed -n '1,40p' "$OUT_ROOT/binaries.txt"
