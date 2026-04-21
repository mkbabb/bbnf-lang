#!/usr/bin/env bash
set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
OUT_ROOT="$ROOT/.profiles/samply/prebuild"
EXPAND_ROOT="$OUT_ROOT/expand"
WAVE_TSV="$OUT_ROOT/wave.tsv"
TARGET_DIR="${CARGO_TARGET_DIR:-}"

# One port pair per bench. A single subagent handles one bench and iterates
# through its entries sequentially, reusing the port pair across entries.
BENCHES=(
  json_monolithic
  css_l4
  google_sheets_monolithic
  bbnf_monolithic
  json_monolithic_value
)
RECORD_PORTS=(
  3130
  3140
  3150
  3160
  3170
)
LOAD_PORTS=(
  3131
  3141
  3151
  3161
  3171
)

# Bench source paths (relative to crates/core/). Duplicated from
# crates/core/Cargo.toml [[bench]] entries; kept in sync manually.
bench_source() {
  case "$1" in
    json_monolithic)          echo "benches/json/monolithic.rs" ;;
    css_l4)                   echo "benches/css/l4.rs" ;;
    google_sheets_monolithic) echo "benches/google_sheets/monolithic.rs" ;;
    bbnf_monolithic)          echo "benches/bbnf/monolithic.rs" ;;
    json_monolithic_value)    echo "benches/json/value.rs" ;;
    *) return 1 ;;
  esac
}

entries_for() {
  case "$1" in
    json_monolithic)
      echo "data twitter citm canada data_xl"
      ;;
    css_l4)
      echo "normalize bootstrap tailwind"
      ;;
    google_sheets_monolithic)
      echo "parse_simple parse_nested parse_stress"
      ;;
    bbnf_monolithic)
      echo "json ebnf css_pretty google_sheets bbnf_self css_l4_grammar"
      ;;
    json_monolithic_value)
      echo "bbnf_data bbnf_twitter bbnf_citm bbnf_canada bbnf_data_xl sonic_data sonic_twitter sonic_citm sonic_canada sonic_data_xl"
      ;;
    *)
      return 1
      ;;
  esac
}

if [[ -z "$TARGET_DIR" ]]; then
  echo "CARGO_TARGET_DIR must be set to one absolute shared path before prepare-profile-wave." >&2
  exit 1
fi

case "$TARGET_DIR" in
  /*) ;;
  *)
    echo "CARGO_TARGET_DIR must be absolute: $TARGET_DIR" >&2
    exit 1
    ;;
esac

check_port_free() {
  local port="$1"
  local out="$OUT_ROOT/port-$port.txt"
  lsof -nP -iTCP:"$port" -sTCP:LISTEN > "$out" 2>&1 || true
  if [[ -s "$out" ]]; then
    echo "Port $port is already in use; see $out" >&2
    exit 1
  fi
}

# `expand.rs` is fresh when it exists AND is newer than every input that
# could alter expansion: the bench source, all shape emitter files, and
# the regenerated `generated.rs`. `find -newer` returns truthy when
# `$expand_rs` mtime > `$candidate` mtime; any stale input invalidates
# the cache. Callers that edit emitter internals re-run expand the next
# time; unrelated benches stay cached.
expand_fresh() {
  local bench="$1"
  local expand_rs="$EXPAND_ROOT/$bench/expand.rs"
  local src_rel
  src_rel="$(bench_source "$bench")" || return 1
  local bench_src="$ROOT/crates/core/$src_rel"
  local shapes_dir="$ROOT/crates/core/src/backend/rust/emitter/shapes"
  local generated="$ROOT/crates/core/src/grammar/generated.rs"

  [[ -s "$expand_rs" ]] || return 1
  [[ -f "$bench_src" ]] || return 1
  [[ -n "$(find "$expand_rs" -newer "$bench_src" 2>/dev/null)" ]] || return 1
  [[ -f "$generated" ]] || return 1
  [[ -n "$(find "$expand_rs" -newer "$generated" 2>/dev/null)" ]] || return 1

  if [[ -d "$shapes_dir" ]]; then
    # Any shape emitter newer than expand.rs invalidates the cache.
    local newer_shape
    newer_shape="$(find "$shapes_dir" -type f -name '*.rs' -newer "$expand_rs" -print -quit 2>/dev/null)"
    [[ -z "$newer_shape" ]] || return 1
  fi
  return 0
}

mkdir -p "$OUT_ROOT" "$EXPAND_ROOT"

for port in "${RECORD_PORTS[@]}" "${LOAD_PORTS[@]}"; do
  check_port_free "$port"
done

"$ROOT/scripts/prebuild-benches.sh" \
  > "$OUT_ROOT/prebuild.stdout" \
  2> "$OUT_ROOT/prebuild.stderr"

find "$ROOT" -name '.bbnf-cache' -type d -exec rm -rf {} + 2>/dev/null

for bench in "${BENCHES[@]}"; do
  mkdir -p "$EXPAND_ROOT/$bench"
  if expand_fresh "$bench"; then
    printf 'expand: reused %s\n' "$EXPAND_ROOT/$bench/expand.rs"
  else
    cargo expand -p bbnf --bench "$bench" \
      > "$EXPAND_ROOT/$bench/expand.rs" \
      2> "$EXPAND_ROOT/$bench/expand.err"
    printf 'expand: regen %s\n' "$EXPAND_ROOT/$bench/expand.rs"
  fi
done

printf 'bench\tentry\trecord_port\tload_port\tartifact_dir\tbinary\texpand_rs\texpand_err\tbench_cwd\ttarget_dir\n' > "$WAVE_TSV"

for i in "${!BENCHES[@]}"; do
  bench="${BENCHES[$i]}"
  record_port="${RECORD_PORTS[$i]}"
  load_port="${LOAD_PORTS[$i]}"
  bench_cwd="$ROOT/crates/core"
  binary="$(awk -F '\t' -v bench="$bench" '$1 == bench { print $2 }' "$OUT_ROOT/binaries.tsv")"

  if [[ -z "$binary" ]]; then
    echo "Missing binary entry for $bench in $OUT_ROOT/binaries.tsv" >&2
    exit 1
  fi

  entries="$(entries_for "$bench")"
  for entry in $entries; do
    artifact_dir="$ROOT/.profiles/samply/$bench/$entry"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$bench" \
      "$entry" \
      "$record_port" \
      "$load_port" \
      "$artifact_dir" \
      "$binary" \
      "$EXPAND_ROOT/$bench/expand.rs" \
      "$EXPAND_ROOT/$bench/expand.err" \
      "$bench_cwd" \
      "$TARGET_DIR" \
      >> "$WAVE_TSV"
  done
done

printf 'Prepared profile wave: %s\n' "$WAVE_TSV"
sed -n '1,40p' "$WAVE_TSV"
