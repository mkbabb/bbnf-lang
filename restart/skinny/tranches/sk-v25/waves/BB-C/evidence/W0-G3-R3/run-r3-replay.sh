#!/bin/bash
set -euo pipefail

export LC_ALL=C
export TZ=UTC

if [[ $# -ne 1 ]]; then
    echo "usage: $0 REPLAY_ROOT" >&2
    exit 64
fi

replay_root=$(cd "$1" && pwd -P)
historical_dir="$replay_root/historical"
current_dir="$replay_root/current"
evidence_dir=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-G3-R3
runs_dir="$evidence_dir/runs-sealed-v2"

if [[ -e "$runs_dir" ]]; then
    echo "refusing to replace existing runs: $runs_dir" >&2
    exit 73
fi

mkdir -p "$runs_dir/identity" "$runs_dir/locks" "$runs_dir/metadata" \
    "$runs_dir/raw" "$runs_dir/exits" "$runs_dir/normalized" \
    "$runs_dir/source-after"

common_env=(
    env
    CARGO_BUILD_JOBS=1
    CARGO_INCREMENTAL=0
    CARGO_NET_OFFLINE=true
    CARGO_PROFILE_DEV_CODEGEN_UNITS=16
    CARGO_PROFILE_DEV_DEBUG=0
    CARGO_PROFILE_TEST_CODEGEN_UNITS=16
    CARGO_PROFILE_TEST_DEBUG=0
    CARGO_TERM_COLOR=never
    NO_COLOR=1
    RUST_BACKTRACE=0
    RUST_TEST_THREADS=1
    SOURCE_DATE_EPOCH=0
    TERM=dumb
    RUSTC_WRAPPER=
)

{
    printf 'replay_root=%s\n' "$replay_root"
    printf 'historical_dir=%s\n' "$historical_dir"
    printf 'current_dir=%s\n' "$current_dir"
    printf 'PATH=%s\n' "$PATH"
    printf 'HOME=%s\n' "$HOME"
    printf 'USER=%s\n' "${USER:-}"
    printf 'SHELL=%s\n' "${SHELL:-}"
    printf 'TMPDIR=%s\n' "${TMPDIR:-/tmp}"
    printf '%s\n' \
        'CARGO_BUILD_JOBS=1' \
        'CARGO_INCREMENTAL=0' \
        'CARGO_NET_OFFLINE=true' \
        'CARGO_PROFILE_DEV_CODEGEN_UNITS=16' \
        'CARGO_PROFILE_DEV_DEBUG=0' \
        'CARGO_PROFILE_TEST_CODEGEN_UNITS=16' \
        'CARGO_PROFILE_TEST_DEBUG=0' \
        'CARGO_TERM_COLOR=never' \
        'NO_COLOR=1' \
        'RUST_BACKTRACE=0' \
        'RUST_TEST_THREADS=1' \
        'SOURCE_DATE_EPOCH=0' \
        'TERM=dumb' \
        'RUSTC_WRAPPER='
} > "$runs_dir/identity/environment.txt"

{
    uname -a
    uname -m
    sw_vers
    sysctl -n machdep.cpu.brand_string
    cargo -vV
    rustc -vV
    rustup show active-toolchain
    command -v jq
    jq --version
    shasum -a 256 "$(command -v jq)"
    xcrun --show-sdk-path || printf 'sdk_path=unavailable\n'
} > "$runs_dir/identity/toolchain-and-host.txt" 2>&1

[[ "$(jq --version)" == "jq-1.8.1" ]]
[[ "$(shasum -a 256 "$(command -v jq)" | awk '{print $1}')" == \
    "ef35d26cab5fffd780c5272142dc2d5ae37ae02cae7885204030e48020f74cd1" ]]

{
    shasum -a 256 /Users/mkbabb/Programming/bbnf-lang/Cargo.lock
    shasum -a 256 "$historical_dir/Cargo.lock"
    shasum -a 256 "$current_dir/Cargo.lock"
} > "$runs_dir/locks/before.sha256"

run_metadata() {
    local label=$1
    local source_dir=$2
    local target_dir="$replay_root/targets/$label"
    local output="$runs_dir/metadata/$label.json"
    local status
    if (
        cd "$source_dir"
        "${common_env[@]}" CARGO_TARGET_DIR="$target_dir" \
            cargo metadata --offline --locked --format-version 1
    ) > "$output" 2> "$runs_dir/metadata/$label.stderr.log"; then
        status=0
    else
        status=$?
    fi
    printf '%s\n' "$status" > "$runs_dir/exits/metadata-$label.exit"
    [[ "$status" -eq 0 ]]
}

run_metadata historical "$historical_dir"
run_metadata current "$current_dir"

for label in historical current; do
    [[ "$(jq -r '.version' "$runs_dir/metadata/$label.json")" == "1" ]]
    jq -r '.packages[] | select(.name == "parse_that" or .name == "bbnf-regex" or .name == "pprint" or .name == "pprint_derive") | [.name, .version, .manifest_path, (.source // "path")] | @tsv' \
        "$runs_dir/metadata/$label.json" | sort > "$runs_dir/metadata/$label.path-packages.tsv"
    [[ "$(wc -l < "$runs_dir/metadata/$label.path-packages.tsv" | tr -d ' ')" == "4" ]]
done

expected_paths=(
    "$replay_root/parse-that/rust/parse_that/Cargo.toml"
    "$replay_root/parse-that/rust/regex/Cargo.toml"
    "$replay_root/pprint/rust/Cargo.toml"
    "$replay_root/pprint/rust/derive/Cargo.toml"
)
for label in historical current; do
    for expected_path in "${expected_paths[@]}"; do
        grep -Fq "$expected_path" "$runs_dir/metadata/$label.path-packages.tsv"
    done
    if grep -Eq '/Users/mkbabb/Programming/(parse-that|pprint)/' \
        "$runs_dir/metadata/$label.path-packages.tsv"; then
        echo "active sibling path leaked into $label metadata" >&2
        exit 1
    fi
done
printf 'historical=frozen-only\ncurrent=frozen-only\n' > "$runs_dir/metadata/path-resolution.assertions"

run_cargo_test() {
    local log_file=$1
    local exit_file=$2
    local label=$3
    local source_dir=$4
    local target_dir=$5
    shift 5

    {
        printf '=== COMMAND %s ===\n' "$label"
        printf 'cd %q && ' "$source_dir"
        printf '%q ' "${common_env[@]}" "CARGO_TARGET_DIR=$target_dir" cargo "$@"
        printf '\n'
    } >> "$log_file"

    local status
    if (
        cd "$source_dir"
        "${common_env[@]}" CARGO_TARGET_DIR="$target_dir" cargo "$@"
    ) >> "$log_file" 2>&1; then
        status=0
    else
        status=$?
    fi
    printf '=== EXIT %s %s ===\n' "$label" "$status" >> "$log_file"
    printf '%s\n' "$status" > "$exit_file"
    return "$status"
}

reject_resource_failure() {
    local log_file=$1
    ! grep -Eiq 'could not compile|signal:|SIGKILL|Killed:|out of memory|(^|[^[:alnum:]])OOM([^[:alnum:]]|$)' \
        "$log_file"
}

historical_target="$replay_root/targets-v2/historical"
historical_diag_log="$runs_dir/raw/historical-r3-diagnostic.raw.log"
set +e
run_cargo_test "$historical_diag_log" "$runs_dir/exits/historical-r3-diagnostic.exit" \
    historical-r3-diagnostic "$historical_dir" "$historical_target" \
    test --offline --locked -j 1 -p bbnf --test egraph_semantic \
    egraph_semantic_nullable_alt_required_context_diagnostic -- --exact --nocapture --test-threads=1
historical_diag_status=$?
set -e
[[ "$historical_diag_status" -eq 101 ]]
reject_resource_failure "$historical_diag_log"
grep -Fq 'test egraph_semantic_nullable_alt_required_context_diagnostic ... FAILED' \
    "$historical_diag_log"
grep -Fq 'left: [120]' "$historical_diag_log"
grep -Fq 'right: [120, 122]' "$historical_diag_log"
grep -Fq 'test result: FAILED.' "$historical_diag_log"

declare -a historical_labels=(historical-r2-allowlist historical-r2-graph historical-r2-empty)
declare -a historical_packages=(bbnf-ir bbnf-ir bbnf)
declare -a historical_tests=(egraph egraph egraph_semantic)
declare -a historical_filters=(
    egraph_universal::default_rules_match_exact_allowlist
    egraph_universal::default_saturation_keeps_nullable_alt_distinct_from_literal
    egraph_semantic_nullable_alt_preserves_empty_branch
)

for index in 0 1 2; do
    label=${historical_labels[$index]}
    log_file="$runs_dir/raw/$label.raw.log"
    set +e
    run_cargo_test "$log_file" "$runs_dir/exits/$label.exit" \
        "$label" "$historical_dir" "$historical_target" \
        test --offline --locked -j 1 -p "${historical_packages[$index]}" \
        --test "${historical_tests[$index]}" "${historical_filters[$index]}" \
        -- --exact --nocapture --test-threads=1
    status=$?
    set -e
    [[ "$status" -eq 101 ]]
    reject_resource_failure "$log_file"
    grep -Fq "test ${historical_filters[$index]} ... FAILED" "$log_file"
    grep -Fq 'test result: FAILED.' "$log_file"
done

grep -Fq 'wrap-of-epsilon-scalar' \
    "$runs_dir/raw/historical-r2-allowlist.raw.log"
grep -Fq 'left: e2' "$runs_dir/raw/historical-r2-graph.raw.log"
grep -Fq 'right: e2' "$runs_dir/raw/historical-r2-graph.raw.log"
grep -Fq 'nullable alternation rejected empty input' \
    "$runs_dir/raw/historical-r2-empty.raw.log"

cat "$historical_diag_log" \
    "$runs_dir/raw/historical-r2-allowlist.raw.log" \
    "$runs_dir/raw/historical-r2-graph.raw.log" \
    "$runs_dir/raw/historical-r2-empty.raw.log" \
    > "$runs_dir/raw/historical-all-reds.raw.log"

run_current_group() {
    local run_label=$1
    local log_file="$runs_dir/raw/current-$run_label.raw.log"
    # A/B intentionally share only this frozen-current target directory.
    local target_dir="$replay_root/targets-v2/current"
    : > "$log_file"

    run_cargo_test "$log_file" "$runs_dir/exits/current-$run_label-ir.exit" \
        current-ir "$current_dir" "$target_dir" \
        test --offline --locked -j 1 -p bbnf-ir --test egraph \
        egraph_universal:: -- --test-threads=1
    run_cargo_test "$log_file" "$runs_dir/exits/current-$run_label-core.exit" \
        current-core "$current_dir" "$target_dir" \
        test --offline --locked -j 1 -p bbnf --test egraph_semantic \
        egraph_semantic_nullable_alt_ -- --test-threads=1

    reject_resource_failure "$log_file"
    ! grep -Fq 'FAILED' "$log_file"
    grep -Fq 'test result: ok. 9 passed; 0 failed' "$log_file"
    grep -Fq 'test result: ok. 2 passed; 0 failed' "$log_file"
}

run_current_group a
run_current_group b

{
    shasum -a 256 /Users/mkbabb/Programming/bbnf-lang/Cargo.lock
    shasum -a 256 "$historical_dir/Cargo.lock"
    shasum -a 256 "$current_dir/Cargo.lock"
} > "$runs_dir/locks/after.sha256"
cmp "$runs_dir/locks/before.sha256" "$runs_dir/locks/after.sha256"

(
    cd "$runs_dir"
    find identity locks metadata raw exits -type f -print | sort | while IFS= read -r file; do
        shasum -a 256 "$file"
    done
) > "$runs_dir/RAW-MANIFEST.sha256"

"$evidence_dir/normalize-logs.sh" "$runs_dir/raw/current-a.raw.log" \
    > "$runs_dir/normalized/current-a.txt"
"$evidence_dir/normalize-logs.sh" "$runs_dir/raw/current-b.raw.log" \
    > "$runs_dir/normalized/current-b.txt"
"$evidence_dir/normalize-logs.sh" "$runs_dir/raw/historical-all-reds.raw.log" \
    > "$runs_dir/normalized/historical-all-reds.txt"

current_a_hash=$(shasum -a 256 "$runs_dir/normalized/current-a.txt" | awk '{print $1}')
current_b_hash=$(shasum -a 256 "$runs_dir/normalized/current-b.txt" | awk '{print $1}')
cmp "$runs_dir/normalized/current-a.txt" "$runs_dir/normalized/current-b.txt"
{
    printf 'current_a_sha256=%s\n' "$current_a_hash"
    printf 'current_b_sha256=%s\n' "$current_b_hash"
    printf 'equal=yes\n'
    shasum -a 256 "$runs_dir/normalized/historical-all-reds.txt"
} > "$runs_dir/normalized/hashes-and-equality.txt"

"$evidence_dir/write-source-ledger.sh" \
    /Users/mkbabb/Programming/parse-that/rust/parse_that \
    > "$runs_dir/source-after/parse-that-parse-that.after.tsv"
"$evidence_dir/write-source-ledger.sh" \
    /Users/mkbabb/Programming/parse-that/rust/regex \
    > "$runs_dir/source-after/parse-that-regex.after.tsv"
"$evidence_dir/write-source-ledger.sh" \
    /Users/mkbabb/Programming/pprint/rust \
    > "$runs_dir/source-after/pprint-rust.after.tsv"
"$evidence_dir/write-source-ledger.sh" \
    /Users/mkbabb/Programming/pprint/rust/derive \
    > "$runs_dir/source-after/pprint-derive.after.tsv"

for label in parse-that-parse-that parse-that-regex pprint-rust pprint-derive; do
    cmp "$evidence_dir/closure/ledgers/$label.before.tsv" \
        "$runs_dir/source-after/$label.after.tsv"
done

"$evidence_dir/census-source-symlinks.sh" > "$runs_dir/source-after/symlinks.after.txt"
grep -Fqx 'total_symlinks=0' "$runs_dir/source-after/symlinks.after.txt"

(
    cd "$runs_dir"
    find . -type f ! -name COMPLETION-MANIFEST.sha256 ! -name COMPLETE -print \
        | sort | while IFS= read -r file; do
            shasum -a 256 "$file"
        done
) > "$runs_dir/COMPLETION-MANIFEST.sha256"
(
    cd "$runs_dir"
    shasum -a 256 -c COMPLETION-MANIFEST.sha256
) > /dev/null
completion_manifest_hash=$(shasum -a 256 "$runs_dir/COMPLETION-MANIFEST.sha256" | awk '{print $1}')
printf 'status=complete\nmanifest_sha256=%s\n' "$completion_manifest_hash" \
    > "$runs_dir/COMPLETE"
