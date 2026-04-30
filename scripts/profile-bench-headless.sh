#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  profile-bench-headless.sh <bench> [entry] [port]
  profile-bench-headless.sh --bench <bench> [--entry <entry>] [--record-port <port>] \
    [--load-port <port>] [--artifact-dir <dir>] [--bin <path>] [--bench-cwd <dir>] \
    [--syms-pattern <regex>]

Examples:
  profile-bench-headless.sh bbnf_monolithic bbnf_self 3130
  profile-bench-headless.sh google_sheets_monolithic parse_simple 3132
  profile-bench-headless.sh --bench json_monolithic --entry canada \
    --record-port 3130 --load-port 3131 \
    --artifact-dir "$ROOT/.profiles/samply/json_monolithic/canada" \
    --bin /abs/shared-target/release/deps/json_monolithic-abc123

Outputs are written under:
  .profiles/samply/<bench>/<entry>/
EOF
}

ROOT=$(git rev-parse --show-toplevel)
OUT_ROOT="$ROOT/.profiles/samply"
BENCH=""
ENTRY=""
PORT=""
LOAD_PORT=""
ARTIFACT_DIR=""
BIN=""
BENCH_CWD="$ROOT/crates/core"
SYMS_PATTERN=""

default_entry() {
  case "$1" in
    json_monolithic) echo "canada" ;;
    css_l4) echo "tailwind" ;;
    google_sheets_monolithic) echo "parse_simple" ;;
    bbnf_monolithic) echo "bbnf_self" ;;
    json_value) echo "sonic_canada" ;;
    *) return 1 ;;
  esac
}

check_port_free() {
  local port="$1"
  local out="$ARTIFACT_DIR/port-$port.txt"
  lsof -nP -iTCP:"$port" -sTCP:LISTEN > "$out" 2>&1 || true
  if [[ -s "$out" ]]; then
    echo "Port $port is already in use; see $out" >&2
    exit 1
  fi
}

resolve_bin() {
  bins=()
  for profile_dir in profiling-prep release bench; do
    local deps="${CARGO_TARGET_DIR:-$ROOT/target}/$profile_dir/deps"
    [[ -d "$deps" ]] || continue
    while IFS= read -r bin; do
      bins+=("$bin")
    done < <(
      find "$deps" -maxdepth 1 -type f -perm -111 \
        -name "$BENCH-*" ! -name '*.d' ! -name '*.dSYM' | sort
    )
    if [[ "${#bins[@]}" -gt 0 ]]; then
      break
    fi
  done
  if [[ "${#bins[@]}" -ne 1 ]]; then
    printf 'Could not resolve a unique bench binary for %s\n' "$BENCH" >&2
    printf 'Candidates:\n%s\n' "${bins[*]}" >&2
    exit 1
  fi
  BIN="${bins[0]}"
}

if [[ $# -ge 1 && "$1" != --* ]]; then
  if [[ $# -lt 1 || $# -gt 3 ]]; then
    usage >&2
    exit 1
  fi
  BENCH="$1"
  ENTRY="${2:-}"
  PORT="${3:-3130}"
else
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --bench) BENCH="$2"; shift 2 ;;
      --entry) ENTRY="$2"; shift 2 ;;
      --record-port) PORT="$2"; shift 2 ;;
      --load-port) LOAD_PORT="$2"; shift 2 ;;
      --artifact-dir) ARTIFACT_DIR="$2"; shift 2 ;;
      --bin) BIN="$2"; shift 2 ;;
      --bench-cwd) BENCH_CWD="$2"; shift 2 ;;
      --syms-pattern) SYMS_PATTERN="$2"; shift 2 ;;
      -h|--help) usage; exit 0 ;;
      *)
        echo "Unknown argument: $1" >&2
        usage >&2
        exit 1
        ;;
    esac
  done
fi

if [[ -z "$BENCH" ]]; then
  usage >&2
  exit 1
fi

if [[ -z "$ENTRY" ]]; then
  ENTRY=$(default_entry "$BENCH") || {
    echo "No default entry for bench '$BENCH'" >&2
    exit 1
  }
fi

if [[ -z "$PORT" ]]; then
  PORT=3130
fi
if [[ -z "$LOAD_PORT" ]]; then
  LOAD_PORT=$((PORT + 1))
fi
if [[ -z "$ARTIFACT_DIR" ]]; then
  ARTIFACT_DIR="$OUT_ROOT/$BENCH/$ENTRY"
fi
if [[ -z "$SYMS_PATTERN" ]]; then
  SYMS_PATTERN="$BENCH|BbnfBootstrap|GoogleSheetsParser|JsonParser|CssL4Parser"
fi

mkdir -p "$ARTIFACT_DIR"

printf 'bench\t%s\nentry\t%s\nrecord_port\t%s\nload_port\t%s\nartifact_dir\t%s\nbench_cwd\t%s\n' \
  "$BENCH" "$ENTRY" "$PORT" "$LOAD_PORT" "$ARTIFACT_DIR" "$BENCH_CWD" \
  > "$ARTIFACT_DIR/meta.txt"

if [[ -z "$BIN" ]]; then
  cd "$ROOT"
  find . -name '.bbnf-cache' -type d -exec rm -rf {} + 2>/dev/null
  cargo bench -p bbnf --bench "$BENCH" \
    > "$ARTIFACT_DIR/bench.txt" 2>&1
  cargo bench -p bbnf --bench "$BENCH" --no-run \
    > "$ARTIFACT_DIR/build.txt" 2>&1
  resolve_bin
else
  printf 'prebuilt_binary\t%s\nbuild_skipped\ttrue\n' "$BIN" > "$ARTIFACT_DIR/build.txt"
  cd "$BENCH_CWD"
  "$BIN" --bench "$ENTRY" \
    > "$ARTIFACT_DIR/bench.txt" 2>&1
fi

if [[ ! -x "$BIN" ]]; then
  echo "Could not resolve bench binary for $BENCH" >&2
  exit 1
fi

printf 'binary\t%s\n' "$BIN" >> "$ARTIFACT_DIR/meta.txt"

wait_for_record_artifacts() {
  local tries=60
  while (( tries > 0 )); do
    if [[ -f "$ARTIFACT_DIR/profile.json.gz" && \
          -f "$ARTIFACT_DIR/profile.json.syms.json" ]] && \
       grep -Eq 'Press Ctrl\+C to stop\.|Local server listening at' "$ARTIFACT_DIR/record.txt" 2>/dev/null; then
      return 0
    fi
    sleep 1
    tries=$((tries - 1))
  done
  return 1
}

wait_for_load_ready() {
  local tries=30
  while (( tries > 0 )); do
    if grep -Eq 'Press Ctrl\+C to stop\.|Local server listening at' "$ARTIFACT_DIR/load.txt" 2>/dev/null; then
      return 0
    fi
    sleep 1
    tries=$((tries - 1))
  done
  return 1
}

cleanup_pid() {
  local pid="$1"
  kill -INT "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
}

check_port_free "$PORT"
check_port_free "$LOAD_PORT"

cd "$BENCH_CWD"

samply record --no-open --unstable-presymbolicate --port "$PORT" \
  -o "$ARTIFACT_DIR/profile.json.gz" \
  "$BIN" --bench "$ENTRY" \
  > "$ARTIFACT_DIR/record.txt" 2>&1 &
RECORD_PID=$!

if ! wait_for_record_artifacts; then
  cleanup_pid "$RECORD_PID"
  echo "Timed out waiting for record artifacts in $ARTIFACT_DIR" >&2
  exit 1
fi

cleanup_pid "$RECORD_PID"

samply load --no-open --port "$LOAD_PORT" "$ARTIFACT_DIR/profile.json.gz" \
  > "$ARTIFACT_DIR/load.txt" 2>&1 &
LOAD_PID=$!

if ! wait_for_load_ready; then
  cleanup_pid "$LOAD_PID"
  echo "Timed out waiting for load server in $ARTIFACT_DIR" >&2
  exit 1
fi

cleanup_pid "$LOAD_PID"

grep -En "$SYMS_PATTERN" "$ARTIFACT_DIR/profile.json.syms.json" \
  > "$ARTIFACT_DIR/syms-proof.txt" || true

if [[ ! -s "$ARTIFACT_DIR/syms-proof.txt" ]]; then
  echo "No named bench/parser frames found in $ARTIFACT_DIR/profile.json.syms.json" >&2
  exit 1
fi

printf 'Artifact directory: %s\n' "$ARTIFACT_DIR"
ls -l "$ARTIFACT_DIR"
printf '\nNamed-frame proof:\n'
sed -n '1,10p' "$ARTIFACT_DIR/syms-proof.txt"
